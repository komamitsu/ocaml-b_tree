#use "topfind"
#require "extlib"
#require "unix"

module BTreeMake (Record : sig
                             type k
                             type v
                             val compare : k -> k -> int
                           end) =
  struct
    type page = { 
      recs: (Record.k * Record.v) option array;
      ptrs: page option array
    }

    let empty_recs size = Array.make (size + 1) None

    let empty_ptrs size = Array.make (size + 1 + 1) None

    let create_page size = { recs = empty_recs size; ptrs = empty_ptrs size }

    let get_page_size page = (Array.length page.recs) - 1

    let is_leaf page =
      let page_size = get_page_size page in
      let rec loop i =
        if i >= page_size then true
        else 
          match page.ptrs.(i) with
          | Some _ -> false
          | None -> loop (i + 1)
      in
      loop 0

    let insert_key_and_ptr page record ptr =
      let page_size = get_page_size page in
      let rec loop idx_src idx_dst new_page inserted =
        if idx_src > page_size then (
          new_page.ptrs.(page_size + 1) <- page.ptrs.(page_size);
          new_page
        )
        else (
          match page.recs.(idx_src) with
          | Some (k, v) -> 
              if not inserted && Record.compare k (fst record) >= 0 then (
                new_page.recs.(idx_dst) <- Some record;
                new_page.recs.(idx_dst + 1) <- Some (k, v);
                new_page.ptrs.(idx_dst) <- page.ptrs.(idx_src);
                new_page.ptrs.(idx_dst + 1) <- ptr;
                loop (idx_src + 1) (idx_dst + 1 + 1) new_page true
              )
              else (
                new_page.recs.(idx_dst) <- Some (k, v);
                new_page.ptrs.(idx_dst) <- page.ptrs.(idx_src);
                new_page.ptrs.(idx_dst) <- page.ptrs.(idx_src);
                loop (idx_src + 1) (idx_dst + 1) new_page inserted
              )
          | None -> 
              if not inserted then (
                new_page.recs.(idx_dst) <- Some record;
                new_page.ptrs.(idx_dst + 1) <- ptr
              );
              new_page.ptrs.(idx_dst) <- page.ptrs.(idx_src);
              new_page
        )
      in
      loop 0 0 (create_page (get_page_size page)) false

    let split_page page =
      let page_size = get_page_size page in
      let mid_pos = page_size / 2 in
      let rec loop i l r =
        if i > page_size then l, r
        else (
          if i < mid_pos then (
            l.recs.(i) <- page.recs.(i);
            l.ptrs.(i) <- page.ptrs.(i);
            loop (i + 1) l r
          )
          else (
            if i > mid_pos then (
              r.recs.(i - mid_pos - 1) <- page.recs.(i);
              r.ptrs.(i - mid_pos - 1) <- page.ptrs.(i);
              loop (i + 1) l r
            )
            else loop (i + 1) l r
          )
        )
      in
      let l, r = loop 0 (create_page page_size) (create_page page_size) in
      (page.recs.(page_size / 2), l, r)

    let find_ins_idx page record =
      let page_size = get_page_size page in
      let rec loop i =
        if i >= page_size then i
        else 
          match page.recs.(i) with
          | Some (k, v) when Record.compare k (fst record) >= 0 -> i
          | _ -> loop (i + 1)
      in
      loop 0

    let insert page key value =
      let rec _insert page record ptr splited =
        if splited || is_leaf page then (
          let new_page = insert_key_and_ptr page record ptr in
          match new_page.recs.(get_page_size new_page) with
          | Some _ ->
            let center_key, left_pages, right_pages =
              split_page new_page in
            let node_page = create_page (get_page_size page) in
            node_page.recs.(0) <- center_key;
            node_page.ptrs.(0) <- Some left_pages;
            node_page.ptrs.(1) <- Some right_pages;
            (true, node_page)
          | None -> 
              (false, new_page)
        )
        else (
          let i = find_ins_idx page record in
            match page.ptrs.(i) with
            | Some p -> 
                let splited, child_page = _insert p record ptr false in
                if splited then (
                  match child_page.recs.(0) with
                  | Some r -> 
                      page.ptrs.(i) <- child_page.ptrs.(0);
                      let splited, new_page = 
                        _insert page r child_page.ptrs.(1) true in
                      (splited, new_page)
                  | _ -> failwith "insert: invalid key"
                )
                else (
                  page.ptrs.(i) <- Some child_page;
                  (false, page)
                )
            | None -> 
                let new_page = create_page (get_page_size page) in
                new_page.recs.(0) <- Some record;
                (false, new_page)
        )
      in
      let _, updated_page = _insert page (key, value) None false in
      updated_page
  end

module IntBTree = BTreeMake(struct
                              type k = int
                              type v = string
                              let compare a b = a - b
                            end)
open IntBTree

let _ =
  (* insert_key_and_ptr *)
  let page = 
    { recs = [|Some (4, "four"); Some (8, "eight"); None|];
      ptrs = [|
        Some { recs = [|Some (2, "two"); None; None|]; ptrs = [|None; None; None; None|] };
        Some { recs = [|Some (6, "six"); None; None|]; ptrs = [|None; None; None; None|] };
        Some { recs = [|Some (10, "ten"); None; None|]; ptrs = [|None; None; None; None|] };
        None
      |] } in

  let tmp_page = 
    insert_key_and_ptr page (2, "two")
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
    insert_key_and_ptr page (6, "six") 
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
    insert_key_and_ptr page (10, "ten")
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
  let page = insert_key_and_ptr page (10, "ten") None in
  assert ({ recs = [|Some (10, "ten"); None; None|];
            ptrs = [|None; None; None; None|] } = page);
  let page = insert_key_and_ptr page (4, "four") None in
  assert ({ recs = [|Some (4, "four"); Some (10, "ten"); None|];
            ptrs = [|None; None; None; None|] } = page);
  let page = insert_key_and_ptr page (7, "seven") 
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
            |] } = page)

