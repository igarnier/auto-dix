module Make(G : GraphSig.S) =
  struct
    
    module VLabelSet     = Set.Make(G.V.L)
    module ELabelSet     = Set.Make(G.E.L)
    module ELabelSetMset = Aux.Multiset(ELabelSet)
    module Perm          = Perm.CycleBased(G.V.L)
    module SchreierSims  = SchreierSims.Make(Perm)
                                  
    type cell = { list : G.vertex list;
                  size : int }

    type partition = cell list

    let _ = assert (not G.is_directed)

    let fold_cell ~f ~cell ~acc = List.fold_left f acc cell.list

    let initial_partition : G.t -> partition =
      fun graph ->
      (* Split vertices by their colors *)
      let tbl    = Hashtbl.create 51 in
      let vertlist, len = G.fold_vertex (fun v (acc, len) -> (v :: acc, len + 1)) graph ([], 0) in
      if len = 0 then
        failwith (Printf.sprintf "%s, %d: empty graph\n" __MODULE__ __LINE__)
      else
        let labels =
          List.fold_left
            (fun set v ->
             let label = G.V.label v in
             Hashtbl.add tbl label v;
             let set = VLabelSet.add label set in
             set
            ) VLabelSet.empty vertlist
        in
        let cells =
          VLabelSet.fold
            (fun label acc ->
             (* NB: label is given by fold in increasing order *)
             let vertices = Hashtbl.find_all tbl label in
             { list = vertices; size = (List.length vertices) } :: acc
            ) labels []
        in
        List.rev cells

    let vertex_vertex_sig : G.t -> G.vertex -> G.vertex -> ELabelSet.t =
      fun graph v v' ->
      let edges = G.find_all_edges graph v v' in
      List.fold_left
        (fun set e ->
         ELabelSet.add (G.E.label e) set
        ) ELabelSet.empty edges

    let vertex_cell_sig : G.t -> G.vertex -> cell -> ELabelSetMset.t =
      fun graph v cell ->
      fold_cell
        ~cell
        ~acc:ELabelSetMset.empty
        ~f:(fun setmset v' ->
            ELabelSetMset.add (vertex_vertex_sig graph v v') setmset
           )
          
    let cell_shatters_cell ~graph ~target_cell ~splitter_cell =
      (* TODO optim: exception to exit *)
      let outcome =
        fold_cell
          ~cell:target_cell
          ~acc:`Empty
          ~f:(fun acc v ->
              match acc with
              | `Shatters -> acc
              | `Empty    ->
                 let signature = vertex_cell_sig graph v splitter_cell in
                 `Witness signature
              | `Witness sig' ->
                 let signature = vertex_cell_sig graph v splitter_cell in
                 let c = ELabelSetMset.compare signature sig' in
                 if c = 0 then
                   acc
                 else
                   `Shatters
             )
      in
      match outcome with
      | `Empty ->
         failwith (Printf.sprintf "Error at %s %d\n%!" __MODULE__ __LINE__)
      | `Shatters  -> true
      | `Witness _ -> false
                     
    let exists_cell_shattering_cell  ~graph ~partition ~target_cell =
      try
        let outcome =
          List.find
            (fun splitter_cell ->
             cell_shatters_cell ~graph ~target_cell ~splitter_cell
            ) partition
        in
        Some outcome
      with Not_found -> None

    let rec insert_into_bucket elt signature bucket_list =
      match bucket_list with
      | []           -> (signature, 1, [elt]) :: []
      | (si, size, bucket as hd) :: tail ->
         let c = ELabelSetMset.compare signature si in
         if c < 0 then
           (signature, 1, [elt]) :: bucket_list
         else if c = 0 then
           (si, size + 1, elt :: bucket) :: tail
         else
           hd :: (insert_into_bucket elt signature tail)
           
    let perform_cell_split ~graph ~target_cell ~splitter_cell ~context =
      let _ =
        Printf.printf
          "performing cell split with target cell = %s and splitter cell = %s\n"
          (Aux.to_sseq G.V.to_string "," target_cell.list)
          (Aux.to_sseq G.V.to_string "," splitter_cell.list)
      in
      let buckets =
        fold_cell
          ~cell:target_cell
          ~acc:[]
          ~f:(fun acc v ->
              let signature = vertex_cell_sig graph v splitter_cell in
              insert_into_bucket v signature acc
             )
      in
      let cells = List.map (fun (_, size, list) -> { list; size }) buckets in
      let (prefix, suffix) = context in
      (* TODO: ugly concats. *)
      prefix @ cells @ suffix
                                             
    let refine_partition : G.t -> partition -> partition option =
      (* TODO optim: exception to exit *)
      fun graph partition ->
      Aux.zip_fold
        (fun prefix cell suffix acc ->
         (* TODO optim: if cell is singleton, skip all computations. *)
         let prefix = List.rev prefix in (* zip_fold gives prefix in reverse order. *)
         match acc with
         | Some result -> acc
         | None ->
            let outcome = exists_cell_shattering_cell graph prefix cell in
            let outcome =
              match outcome with
              | Some _ -> outcome
              | None   -> exists_cell_shattering_cell graph (cell :: suffix) cell in
            match outcome with
            | None ->
               let _ =
                 Printf.printf
                   "cell = %s is not splittable\n"
                   (Aux.to_sseq G.V.to_string "," cell.list)
               in
               None
            | Some cell' ->
               Some (perform_cell_split ~graph ~target_cell:cell ~splitter_cell:cell' ~context:(prefix, suffix))
        ) None partition
        
     let rec refine_partition_until_equitable graph partition =
       match refine_partition graph partition with
       | None           -> partition
       | Some partition -> refine_partition_until_equitable graph partition

     let rec is_partition_discrete = function
       | [] -> true
       | [_] :: tail ->
          is_partition_discrete tail
       | _ -> false

     let permutation_of_discrete_partition graph part =
       let vertices = G.fold_vertex (fun v acc -> v :: acc) graph [] in
       let part     = List.map
                        (function
                          | [x] -> x
                          | _ -> failwith "permutation_of_discrete_partition: partition is not discrete"
                        ) part
       in
       Perm.of_mapping (List.combine vertices part)

     let apply_perm graph perm =
       G.map_vertex (Perm.action perm) graph

     let find_first_smallest =
       let rec find_first_smallest_aux partition smallest =
         match partition with
         | [] -> smallest
         | cell :: tail ->
            if cell.size < smallest.size then
              find_first_smallest_aux tail cell
            else
              find_first_smallest_aux tail smallest
       in function
       | [] -> failwith (Printf.sprintf "%s, %d: empty partition\n" __MODULE__ __LINE__)
       | cell :: tail -> find_first_smallest_aux tail cell

                    
     let rec explore graph explored aut minimizer partition =
       if is_partition_discrete partition then
         (* leaf of the search tree *)
         let perm = permutation_of_discrete_partition partition in
         let digest = apply_perm graph perm in
         begin match digest_find_opt digest explored with
               | None -> ()
               | Some perm' ->
                  add_automorphism aut  (Perm.prod (Perm.inv perm) perm')
         end;
         digest_add digest perm explored
       else
         let 
         find_first_smallest_cell graph explored aut minimizer partition

     and find_first_smallest_cell graph explored aut minimizer partition =
       match partition with
       | [] -> failwith ""
       
         
     (* let rec explore graph explored aut minimizer partition = *)
     (*   if is_partition_discrete partition then *)
     (*     let digest = apply_perm graph partition in *)
     (*     begin match digest_find_opt digest explored with *)
     (*           | None      -> () *)
     (*           | Some perm -> *)
     (*              add_automorphism aut (Perm.product (Perm.inverse partition) perm) *)
     (*     end; *)
     (*     digest_add digest partition explored; *)
     (*     digest_min minimizer digest *)
     (*   else *)
     (*     find_first_smallest_cell graph explored aut minimizer partition *)

     (* and find_first_smallest_cell graph explored aut minimizer partition smallest = *)
     (*   match partition with *)
     (*   | [] -> *)
     (*      None *)
     (*   | cell :: tail -> *)
     (*      match smallest with *)
     (*      |  ->  *)
                          
     (* and enumerate_splits graph explored aut minimizer partition = *)
     (*   match partition with *)
     (*   | [] -> *)
     (*      failwith (Printf.sprintf "Error at %s %d\n%!" __MODULE__ __LINE__) *)
     (*   | cell :: tail -> *)

  end


(* module MakePurelyFunctional(G : GraphSig.S) = *)
(*   struct *)

(*     module VertexSet    = Set.Make(G.V) *)
(*     module VertexMap    = Map.Make(G.V) *)
(*     module VLabelSet    = Set.Make(G.V.L) *)
(*     module VLabelMap    = Map.Make(G.V.L)                                                                     *)
(*     module ELabelSet    = Set.Make(G.E.L) *)
(*     module ELabelSetMset = Set.Make(ELabelSet) *)

(*     (\* Let part = [V1; V2; V3; \ldots; Vn]. *)
(*      * Invariants: for all v \in Vi, w \in Vj with i \le j then *)
(*      * (G.V.label v) \le (G.V.label w). *)
(*      *\) *)
(*     type partition = VertexSet.t list *)

(*     let _ = assert (not G.is_directed) *)

(*     let vertex_vertex_sig : G.t -> G.vertex -> G.vertex -> ELabelSet.t = *)
(*       fun graph v v' -> *)
(*       let edges = G.find_all_edges graph v v' in *)
(*       List.fold_left *)
(*         (fun set e -> *)
(*          ELabelSet.add (G.E.label e) set *)
(*         ) ELabelSet.empty edges *)

(*     let vertex_cell_sig : G.t -> G.vertex -> VertexSet.t -> ELabelSetMset.t = *)
(*       fun graph v cell -> *)
(*       VertexSet.fold *)
(*         (fun v' setset -> *)
(*          ELabelSetMset.add (vertex_vertex_sig graph v v') setset *)
(*         ) cell ELabelSetMset.empty *)

(*     let initial_partition : G.t -> partition = *)
(*       fun graph -> *)
(*       (\* Split vertices by their colors *\) *)
(*       let tbl    = Hashtbl.create 51 in *)
(*       let labels = *)
(*         G.fold_vertex *)
(*           (fun v acc -> *)
(*            let label = G.V.label v in *)
(*            Hashtbl.add tbl label v; *)
(*            VLabelSet.add label acc *)
(*           ) graph VLabelSet.empty *)
(*       in *)
(*       let partitions = *)
(*         VLabelSet.fold *)
(*           (fun label acc -> *)
(*            (\* NB: label is given by fold in increasing order *\) *)
(*            let vertices = *)
(*              List.fold_left *)
(*                (fun set elt -> *)
(*                 VertexSet.add elt set *)
(*                ) VertexSet.empty (Hashtbl.find_all tbl label) *)
(*            in *)
(*            vertices :: acc *)
(*           ) labels [] *)
(*       in *)
(*       List.rev partitions *)
               

(*     let cell_shatters_cell : G.t -> VertexSet.t -> VertexSet.t -> bool  = *)
(*       fun graph cell cell' -> *)
(*       match cell with *)
(*       | [] -> *)
(*          failwith (Printf.sprintf "Error at %s %d\n%!" __MODULE__ __LINE__) *)
(*       | vertex :: tail -> *)
(*          let signature = vertex_cell_sig graph vertex cell' in *)
(*          List.exists (fun v -> *)
(*                       ELabelSetMset.compare (vertex_cell_sig graph v cell') signature <> 0 *)
(*                      ) tail *)
                     
(*     let exists_cell_shattering_cell : G.t -> partition -> VertecSet.t -> VertexSet.t option = *)
(*       fun graph cells cell_to_shatter -> *)
(*       try Some (List.find (fun cell -> cell_shatters_cell graph cell cell_to_shatter) cells) *)
(*       with Not_found -> None *)
                  

(*     (\* let refine_partition : partition -> partition option = *\) *)
(*     (\*   fun cells -> *\) *)
(*     (\*   zip_fold *\) *)
(*     (\*     (fun prev cell tail acc -> *\) *)
(*     (\*      match exists_cell_shattering_cell prev cell with *\) *)
(*     (\*      | None -> *\) *)
(*     (\*         match exists_cell_shattering_cell tail cell with *\) *)
(*     (\*         | None ->  *\) *)
(*     (\*     ) *\) *)
      

(*     (\* let rec zip_fold f acc prev list = *\) *)
(*     (\*   match list with *\) *)
(*     (\*   | [] -> acc *\) *)
(*     (\*   | x :: l -> *\) *)
(*     (\*      zip_fold f (f prev x l acc) (x :: prev) l *\) *)
                  
(*     (\* let zip_fold f acc l = zip_fold f acc [] l *\) *)

(*     (\* let find_splitting_pair (cells : partition) = *\) *)
(*     (\*   Aux.zip_fold *\) *)
(*     (\*     (fun prev elt tail acc -> *\) *)
         
(*     (\*     ) acc cells *\) *)
                                

(*   end *)
