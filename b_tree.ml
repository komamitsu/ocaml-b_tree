module BTreeMake (Record : sig
                             type k
                             type v
                             val compare : k -> k -> int
                             val debug : unit -> unit
                           end) :
  sig 
    type page
    val create_page : int -> page
    val insert : page -> Record.k -> Record.v -> page
    val find : page -> Record.k -> Record.v option
  end
=
  struct
    type page = { 
      recs: (Record.k * Record.v) option array;
      ptrs: page option array
    }

    let create_page size =
      { recs = (Array.make (size + 1) None);
        ptrs = (Array.make (size + 1 + 1) None) }

    let get_page_size page = (Array.length page.recs) - 1

    let is_leaf page =
      let page_size = get_page_size page in
      let rec _is_leaf i =
        if i >= page_size then true
        else 
          match page.ptrs.(i) with
          | Some _ -> false
          | None -> _is_leaf (i + 1)
      in
      _is_leaf 0

    let find_ins_idx page record =
      let page_size = get_page_size page in
      let rec _find i =
        if i >= page_size then i
        else 
          match page.recs.(i) with
          | Some (k, v) when Record.compare k (fst record) >= 0 -> i
          | Some (k, v) -> _find (i + 1)
          | None -> i
      in
      _find 0

    let insert_rec_and_ptr page record ptr =
      let ins_idx = find_ins_idx page record in
      let valid_recs_size = (Array.length page.recs) - 1 in
      let valid_ptrs_size = (Array.length page.ptrs) - 1 in
      let left_recs = Array.sub page.recs 0 ins_idx in
      let right_recs = 
        Array.sub page.recs ins_idx (valid_recs_size - ins_idx) in
      let left_ptrs = Array.sub page.ptrs 0 (ins_idx + 1) in
      let right_ptrs = 
        Array.sub page.ptrs (ins_idx + 1) (valid_ptrs_size - ins_idx - 1) in
      { recs = (Array.append left_recs (Array.append [|Some record|] right_recs));
        ptrs = (Array.append left_ptrs (Array.append [|ptr|] right_ptrs)) }
      
    let split_page page =
      let page_size = get_page_size page in
      let mid_pos = page_size / 2 in
      let rec _split i l r =
        match () with
        | _ when i > page_size ->
            r.ptrs.(i - mid_pos - 1) <- page.ptrs.(i);
            l, r
        | _ when i < mid_pos ->
            l.recs.(i) <- page.recs.(i);
            l.ptrs.(i) <- page.ptrs.(i);
            _split (i + 1) l r
        | _ when i = mid_pos ->
            l.ptrs.(i) <- page.ptrs.(i);
            _split (i + 1) l r
        | _ when i > mid_pos ->
              r.recs.(i - mid_pos - 1) <- page.recs.(i);
              r.ptrs.(i - mid_pos - 1) <- page.ptrs.(i);
              _split (i + 1) l r
        | _ -> _split (i + 1) l r
      in
      let l, r = _split 0 (create_page page_size) (create_page page_size) in
      (page.recs.(mid_pos), l, r)

    let insert page key value =
      let page_size = get_page_size page in
      let rec _insert page record ptr splited =
        if splited || is_leaf page then
          (* insert into the page *)
          let new_page = insert_rec_and_ptr page record ptr in
          match new_page.recs.(page_size) with
          | Some _ ->
            let center_key, left_pages, right_pages = split_page new_page in
            let node_page = create_page page_size in
            node_page.recs.(0) <- center_key;
            node_page.ptrs.(0) <- Some left_pages;
            node_page.ptrs.(1) <- Some right_pages;
            (true, node_page)
          | None -> (false, new_page)
        else  
          (* insert into the child page *)
          let ptr_idx = find_ins_idx page record in
            match page.ptrs.(ptr_idx) with
            | Some child_page -> 
                let splited, child_page =
                  _insert child_page record ptr false in
                if splited then 
                  match child_page.recs.(0) with
                  | Some r -> 
                      page.ptrs.(ptr_idx) <- child_page.ptrs.(0);
                      let splited, new_page = 
                        _insert page r child_page.ptrs.(1) true in
                      (splited, new_page)
                  | _ -> failwith "insert: invalid key"
                else (
                  page.ptrs.(ptr_idx) <- Some child_page;
                  (false, page)
                )
            | None -> 
                let new_page = create_page page_size in
                new_page.recs.(0) <- Some record;
                (false, new_page)
      in
      let _, updated_page = _insert page (key, value) None false in
      updated_page

    let find page key =
      let page_size = get_page_size page in
      let rec _find page =
        Record.debug ();
        let rec _search_keys i =
          if i >= page_size then 
            match page.ptrs.(page_size) with
            | Some p -> _find p
            | None -> None
          else 
            match page.recs.(i) with
            | Some (k, v) when Record.compare k key = 0 -> Some v
            | Some (k, v) when Record.compare k key < 0 -> _search_keys (i + 1)
            | _ ->
                match page.ptrs.(i) with
                | Some p -> _find p
                | None -> None
        in
        _search_keys 0
      in
      _find page
  end

(*
module IntBTree = BTreeMake(struct
                              type k = int
                              type v = string
                              let compare a b = a - b
                              let debug = fun () -> ()
                            end)
open IntBTree

let _ =
  (* insert_rec_and_ptr *)
  let page = 
    { recs = [|Some (4, "four"); Some (8, "eight"); None|];
      ptrs = [|
        Some { recs = [|Some (2, "two"); None; None|]; ptrs = [|None; None; None; None|] };
        Some { recs = [|Some (6, "six"); None; None|]; ptrs = [|None; None; None; None|] };
        Some { recs = [|Some (10, "ten"); None; None|]; ptrs = [|None; None; None; None|] };
        None
      |] } in

  let tmp_page = 
    insert_rec_and_ptr page (2, "two")
      (Some { recs = [|Some (3, "three"); None; None|]; ptrs = [|None; None; None; None|] })
  in
  assert (
    { recs = [|Some (2, "two"); Some (4, "four"); Some (8, "eight")|];
      ptrs = [|
        Some { recs = [|Some (2, "two"); None; None|]; ptrs = [|None; None; None; None|] };
        Some { recs = [|Some (3, "three"); None; None|]; ptrs = [|None; None; None; None|] };
        Some { recs = [|Some (6, "six"); None; None|]; ptrs = [|None; None; None; None|] };
        Some { recs = [|Some (10, "ten"); None; None|]; ptrs = [|None; None; None; None|] }
      |] } = tmp_page);

  let tmp_page = 
    insert_rec_and_ptr page (6, "six") 
      (Some { recs = [|Some (7, "seven"); None; None|]; ptrs = [|None; None; None; None|] })
  in
  assert (
    { recs = [|Some (4, "four"); Some (6, "six"); Some (8, "eight")|];
      ptrs = [|
        Some { recs = [|Some (2, "two"); None; None|]; ptrs = [|None; None; None; None|] };
        Some { recs = [|Some (6, "six"); None; None|]; ptrs = [|None; None; None; None|] };
        Some { recs = [|Some (7, "seven"); None; None|]; ptrs = [|None; None; None; None|] };
        Some { recs = [|Some (10, "ten"); None; None|]; ptrs = [|None; None; None; None|] }
      |] } = tmp_page);

  let tmp_page = 
    insert_rec_and_ptr page (10, "ten")
      (Some { recs = [|Some (11, "eleven"); None; None|]; ptrs = [|None; None; None; None|] })
  in
  assert (
    { recs = [|Some (4, "four"); Some (8, "eight"); Some (10, "ten")|];
      ptrs = [|
        Some { recs = [|Some (2, "two"); None; None|]; ptrs = [|None; None; None; None|] };
        Some { recs = [|Some (6, "six"); None; None|]; ptrs = [|None; None; None; None|] };
        Some { recs = [|Some (10, "ten"); None; None|]; ptrs = [|None; None; None; None|] };
        Some { recs = [|Some (11, "eleven"); None; None|]; ptrs = [|None; None; None; None|] }
      |] } = tmp_page);

  let page = create_page 2 in
  assert ({ recs = [|None; None; None|];
            ptrs = [|None; None; None; None|] } = page);
  let page = insert_rec_and_ptr page (10, "ten") None in
  assert ({ recs = [|Some (10, "ten"); None; None|];
            ptrs = [|None; None; None; None|] } = page);
  let page = insert_rec_and_ptr page (4, "four") None in
  assert ({ recs = [|Some (4, "four"); Some (10, "ten"); None|];
            ptrs = [|None; None; None; None|] } = page);
  let page = insert_rec_and_ptr page (7, "seven") 
              (Some { recs = [|Some (8, "eight"); None; None|];
                      ptrs = [|None; None; None; None|] })
  in
  assert ({ recs = [|Some (4, "four"); Some (7, "seven"); Some (10, "ten")|];
            ptrs = [|None;
                     None; 
                     Some { recs = [|Some (8, "eight"); None; None|];
                            ptrs = [|None; None; None; None|] };
                     None|] } = page);
  (* split_page *)
  let center_key, left_pages, right_pages = split_page page in
  assert (Some (7, "seven") = center_key);
  assert ({ recs = [|Some (4, "four"); None; None|];
            ptrs = [|None; None; None; None|] } = left_pages);
  assert ({ recs = [|Some (10, "ten"); None; None|];
            ptrs = [|Some { recs = [|Some (8, "eight"); None; None|];
                            ptrs = [|None; None; None; None|] };
                     None; None; None|] } = right_pages);
  let page = 
    { recs = [|Some (4, "four"); Some (6, "six"); Some (8, "eight")|];
      ptrs = [|
        Some { recs = [|Some (2, "two"); None; None|]; ptrs = [|None; None; None; None|] };
        Some { recs = [|Some (5, "five"); None; None|]; ptrs = [|None; None; None; None|] };
        Some { recs = [|Some (7, "seven"); None; None|]; ptrs = [|None; None; None; None|] };
        Some { recs = [|Some (10, "ten"); None; None|]; ptrs = [|None; None; None; None|] }
      |] } in
  let center_key, left_pages, right_pages = split_page page in
  assert (Some (6, "six") = center_key);
  assert ({ recs = [|Some (4, "four"); None; None|];
            ptrs = [|
              Some { recs = [|Some (2, "two"); None; None|]; ptrs = [|None; None; None; None|] };
              Some { recs = [|Some (5, "five"); None; None|]; ptrs = [|None; None; None; None|] };
              None;
              None|] } = left_pages);
  assert ({ recs = [|Some (8, "eight"); None; None|];
            ptrs = [|
              Some { recs = [|Some (7, "seven"); None; None|]; ptrs = [|None; None; None; None|] };
              Some { recs = [|Some (10, "ten"); None; None|]; ptrs = [|None; None; None; None|] };
              None;
              None|] } = right_pages);

  (* insert *)
  let page = create_page 2 in
  let page = insert page 10 "ten" in
  assert ({ recs = [|Some (10, "ten"); None; None|];
            ptrs = [|None; None; None; None|] } = page);
  let page = insert page 4 "four" in
  assert ({ recs = [|Some (4, "four"); Some (10, "ten"); None|];
            ptrs = [|None; None; None; None|] } = page);
  let page = insert page 7 "seven" in
  assert ({ recs = [|Some (7, "seven"); None; None|];
            ptrs = [|
              Some { recs = [|Some (4, "four"); None; None|];
                     ptrs = [|None; None; None; None|] }; 
              Some { recs = [|Some (10, "ten"); None; None|];
                     ptrs = [|None; None; None; None|] }; 
              None;
              None
            |] } = page);
  let page = insert page 2 "two" in
  assert ({ recs = [|Some (7, "seven"); None; None|];
            ptrs = [|
              Some { recs = [|Some (2, "two"); Some (4, "four"); None|];
                     ptrs = [|None; None; None; None|] }; 
              Some { recs = [|Some (10, "ten"); None; None|];
                     ptrs = [|None; None; None; None|] }; 
              None;
              None
            |] } = page);
  let page = insert page 3 "three" in
  assert ({ recs = [|Some (3, "three"); Some (7, "seven"); None|];
            ptrs = [|
              Some { recs = [|Some (2, "two"); None; None|];
                     ptrs = [|None; None; None; None|] }; 
              Some { recs = [|Some (4, "four"); None; None|];
                     ptrs = [|None; None; None; None|] }; 
              Some { recs = [|Some (10, "ten"); None; None|];
                     ptrs = [|None; None; None; None|] }; 
              None
            |] } = page);
  (* find *)
  assert (None = find page 1);
  assert (Some "two" = find page 2);
  assert (Some "three" = find page 3);
  assert (Some "four" = find page 4);
  assert (Some "seven" = find page 7);
  assert (Some "ten" = find page 10);
  assert (None = find page 11)
*)

