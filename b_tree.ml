#use "topfind"
#require "extlib"
#require "unix"

module BTreeMake (Record : sig
                             type t
                             val compare : t -> t -> int
                           end) =
  struct
    type page = { 
      keys: Record.t option array;
      ptrs: page option array
    }

    let empty_keys size = Array.make (size + 1) None

    let empty_ptrs size = Array.make (size + 1 + 1) None

    let create_page size = { keys = empty_keys size; ptrs = empty_ptrs size }

    let get_page_size page = (Array.length page.keys) - 1

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

    let insert_key_and_ptr page key ptr =
      let page_size = get_page_size page in
      let rec loop idx_src idx_dst new_page inserted =
        if idx_src > page_size then (
          new_page.ptrs.(page_size + 1) <- page.ptrs.(page_size);
          new_page
        )
        else (
          match page.keys.(idx_src) with
          | Some k -> 
              if not inserted && Record.compare k key >= 0 then (
                new_page.keys.(idx_dst) <- Some key;
                new_page.keys.(idx_dst + 1) <- Some k;
                new_page.ptrs.(idx_dst) <- page.ptrs.(idx_src);
                new_page.ptrs.(idx_dst + 1) <- ptr;
                loop (idx_src + 1) (idx_dst + 1 + 1) new_page true
              )
              else (
                new_page.keys.(idx_dst) <- Some k;
                new_page.ptrs.(idx_dst) <- page.ptrs.(idx_src);
                new_page.ptrs.(idx_dst) <- page.ptrs.(idx_src);
                loop (idx_src + 1) (idx_dst + 1) new_page inserted
              )
          | None -> 
              if not inserted then (
                new_page.keys.(idx_dst) <- Some key;
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
            l.keys.(i) <- page.keys.(i);
            l.ptrs.(i) <- page.ptrs.(i);
            loop (i + 1) l r
          )
          else (
            if i > mid_pos then (
              r.keys.(i - mid_pos - 1) <- page.keys.(i);
              r.ptrs.(i - mid_pos - 1) <- page.ptrs.(i);
              loop (i + 1) l r
            )
            else loop (i + 1) l r
          )
        )
      in
      let l, r = loop 0 (create_page page_size) (create_page page_size) in
      (page.keys.(page_size / 2), l, r)

    let find_ins_idx page key =
      let page_size = get_page_size page in
      let rec loop i =
        if i >= page_size then i
        else 
          match page.keys.(i) with
          | Some k when Record.compare k key >= 0 -> i
          | _ -> loop (i + 1)
      in
      loop 0

    let insert page key =
      let rec _insert page key ptr splited =
        if splited || is_leaf page then (
          let new_page = insert_key_and_ptr page key ptr in
          match new_page.keys.(get_page_size new_page) with
          | Some _ ->
            let center_key, left_pages, right_pages =
              split_page new_page in
            let node_page = create_page (get_page_size page) in
            node_page.keys.(0) <- center_key;
            node_page.ptrs.(0) <- Some left_pages;
            node_page.ptrs.(1) <- Some right_pages;
            (true, node_page)
          | None -> 
              (false, new_page)
        )
        else (
          let i = find_ins_idx page key in
            match page.ptrs.(i) with
            | Some p -> 
                let splited, child_page = _insert p key ptr false in
                if splited then (
                  match child_page.keys.(0) with
                  | Some k -> 
                      page.ptrs.(i) <- child_page.ptrs.(0);
                      let splited, new_page = 
                        _insert page k child_page.ptrs.(1) true in
                      (splited, new_page)
                  | _ -> failwith "insert: invalid key"
                )
                else (
                  page.ptrs.(i) <- Some child_page;
                  (false, page)
                )
            | None -> 
                let new_page = create_page (get_page_size page) in
                new_page.keys.(0) <- Some key;
                (false, new_page)
        )
      in
      let _, updated_page = _insert page key None false in
      updated_page
  end

module IntBTree = BTreeMake(struct
                              type t = int
                              let compare a b = a - b
                            end)
open IntBTree

let _ =
  (* insert_key_and_ptr *)
  let page = 
    { keys = [|Some 4; Some 8; None|];
      ptrs = [|
        Some { keys = [|Some 2; None; None|]; ptrs = [|None; None; None; None|] };
        Some { keys = [|Some 6; None; None|]; ptrs = [|None; None; None; None|] };
        Some { keys = [|Some 10; None; None|]; ptrs = [|None; None; None; None|] };
        None
      |] } in

  let tmp_page = 
    insert_key_and_ptr page 2 
      (Some { keys = [|Some 3; None; None|]; ptrs = [|None; None; None; None|] })
  in
  assert (
    { keys = [|Some 2; Some 4; Some 8|];
      ptrs = [|
        Some { keys = [|Some 2; None; None|]; ptrs = [|None; None; None; None|] };
        Some { keys = [|Some 3; None; None|]; ptrs = [|None; None; None; None|] };
        Some { keys = [|Some 6; None; None|]; ptrs = [|None; None; None; None|] };
        Some { keys = [|Some 10; None; None|]; ptrs = [|None; None; None; None|] }
      |] } = tmp_page);

  let tmp_page = 
    insert_key_and_ptr page 6 
      (Some { keys = [|Some 7; None; None|]; ptrs = [|None; None; None; None|] })
  in
  assert (
    { keys = [|Some 4; Some 6; Some 8|];
      ptrs = [|
        Some { keys = [|Some 2; None; None|]; ptrs = [|None; None; None; None|] };
        Some { keys = [|Some 6; None; None|]; ptrs = [|None; None; None; None|] };
        Some { keys = [|Some 7; None; None|]; ptrs = [|None; None; None; None|] };
        Some { keys = [|Some 10; None; None|]; ptrs = [|None; None; None; None|] }
      |] } = tmp_page);

  let tmp_page = 
    insert_key_and_ptr page 10
      (Some { keys = [|Some 11; None; None|]; ptrs = [|None; None; None; None|] })
  in
  assert (
    { keys = [|Some 4; Some 8; Some 10|];
      ptrs = [|
        Some { keys = [|Some 2; None; None|]; ptrs = [|None; None; None; None|] };
        Some { keys = [|Some 6; None; None|]; ptrs = [|None; None; None; None|] };
        Some { keys = [|Some 10; None; None|]; ptrs = [|None; None; None; None|] };
        Some { keys = [|Some 11; None; None|]; ptrs = [|None; None; None; None|] }
      |] } = tmp_page);

  let page = create_page 2 in
  assert ({ keys = [|None; None; None|];
            ptrs = [|None; None; None; None|] } = page);
  let page = insert_key_and_ptr page 10 None in
  assert ({ keys = [|Some 10; None; None|];
            ptrs = [|None; None; None; None|] } = page);
  let page = insert_key_and_ptr page 4 None in
  assert ({ keys = [|Some 4; Some 10; None|];
            ptrs = [|None; None; None; None|] } = page);
  let page = insert_key_and_ptr page 7 
              (Some { keys = [|Some 8; None; None|];
                     ptrs = [|None; None; None; None|] })
  in
  assert ({ keys = [|Some 4; Some 7; Some 10|];
            ptrs = [|None;
                     None; 
                     Some { keys = [|Some 8; None; None|];
                            ptrs = [|None; None; None; None|] };
                     None|] } = page);
  (* split_page *)
  let center_key, left_pages, right_pages = split_page page in
  assert (Some 7 = center_key);
  assert ({ keys = [|Some 4; None; None|];
            ptrs = [|None; None; None; None|] } = left_pages);
  assert ({ keys = [|Some 10; None; None|];
            ptrs = [|Some { keys = [|Some 8; None; None|];
                            ptrs = [|None; None; None; None|] };
                     None; None; None|] } = right_pages);
  (* insert *)
  let page = create_page 2 in
  let page = insert page 10 in
  assert ({ keys = [|Some 10; None; None|];
            ptrs = [|None; None; None; None|] } = page);
  let page = insert page 4 in
  assert ({ keys = [|Some 4; Some 10; None|];
            ptrs = [|None; None; None; None|] } = page);
  let page = insert page 7 in
  assert ({ keys = [|Some 7; None; None|];
            ptrs = [|
              Some { keys = [|Some 4; None; None|];
                     ptrs = [|None; None; None; None|] }; 
              Some { keys = [|Some 10; None; None|];
                     ptrs = [|None; None; None; None|] }; 
              None;
              None
            |] } = page);
  let page = insert page 2 in
  assert ({ keys = [|Some 7; None; None|];
            ptrs = [|
              Some { keys = [|Some 2; Some 4; None|];
                     ptrs = [|None; None; None; None|] }; 
              Some { keys = [|Some 10; None; None|];
                     ptrs = [|None; None; None; None|] }; 
              None;
              None
            |] } = page);
  let page = insert page 3 in
  assert ({ keys = [|Some 3; Some 7; None|];
            ptrs = [|
              Some { keys = [|Some 2; None; None|];
                     ptrs = [|None; None; None; None|] }; 
              Some { keys = [|Some 4; None; None|];
                     ptrs = [|None; None; None; None|] }; 
              Some { keys = [|Some 10; None; None|];
                     ptrs = [|None; None; None; None|] }; 
              None
            |] } = page)

