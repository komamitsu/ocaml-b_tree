#use "b_tree.ml"

module IntBTree = BTreeMake(struct
                              type k = int
                              type v = string
                              let compare a b = a - b
                              let debug = fun () -> ()
                            end)
open IntBTree

let _ =
  let _test page_size rec_num =
    Random.self_init ();
    let rec _create_page n page cap_keys =
      if n <= 0 then (page, cap_keys)
      else 
        let key = Random.int max_int in
        let value = Printf.sprintf "_%d_" key in
        (*
        let value =
          if n = 500 then Printf.sprintf "_%d_" (key + 1)
          else Printf.sprintf "_%d_" (key + 1) in
        *)
        let new_page = IntBTree.insert page key value in
        _create_page (n - 1) new_page (key::cap_keys)
    in
    let page, cap_keys = 
      _create_page rec_num (IntBTree.create_page page_size) [] in
    let rec _assert = function
      | [] -> ()
      | hd::tl ->
          let expected_value = Printf.sprintf "_%d_" hd in
          match IntBTree.find page hd with
          | Some v -> 
              assert (expected_value = v);
              _assert tl
          | None -> failwith "not found"
    in
    _assert cap_keys
  in
  _test    2   1000;
  _test   20   1000;
  _test  100  10000

