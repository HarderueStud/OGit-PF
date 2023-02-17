type commit = {
    parents : Digest.t list;
    date : float;
    message : string;
    content : Digest.t
}
    

let fullStringOfInt _i = 
    let s = string_of_int _i in
    if String.length s = 1 then "0" ^ s else s


let date_fm (_d:float) : string = 
    let tm = Unix.localtime _d in 
    let s = fullStringOfInt(tm.tm_hour) ^":"^ fullStringOfInt(tm.tm_min) ^":"^ fullStringOfInt(tm.tm_sec) ^"-"^ fullStringOfInt(tm.tm_mday) ^"/"^ 
    fullStringOfInt(tm.tm_mon + 1) ^"/"^ fullStringOfInt(tm.tm_year + 1900) in
    (* Il y a possiblement quelque chose de plus optimisé mais je manque de temps pour terminer le projet *)
    s


let get_1_2 (a,_) = a


let date_fm_toFloat (_s:string) : float = 
    let _l = String.split_on_char '-' _s in
    let _t = String.split_on_char ':' (List.nth _l 0) in
    let _d = String.split_on_char '/' (List.nth _l 1) in
    let tm = {
        Unix.tm_sec = int_of_string (List.nth _t 2);
        Unix.tm_min = int_of_string (List.nth _t 1);
        Unix.tm_hour = int_of_string (List.nth _t 0);
        Unix.tm_mday = int_of_string (List.nth _d 0);
        Unix.tm_mon = int_of_string (List.nth _d 1) - 1;
        Unix.tm_year = int_of_string (List.nth _d 2) - 1900;
        Unix.tm_wday = 0;
        Unix.tm_yday = 0;
        Unix.tm_isdst = false
    } in
    let _d = Unix.mktime tm in
    get_1_2 _d


let rec concatStringList _l _sep = 
    match _l with
    | [] -> ""
    | h::t -> h ^ if t = [] then "" else _sep ^ concatStringList t _sep


let set_head (_l:Digest.t list) : unit = 
    let _f = Filename.concat ".ogit" "HEAD" in
    let _ch = open_out _f in
    let _c = List.map (fun x -> Digest.to_hex x) _l in
    let () = output_string _ch (concatStringList _c "\n") in
    let () = close_out _ch in
    ()


let get_head () : Digest.t list = 
    let _f = Filename.concat ".ogit" "HEAD" in
    if not (Sys.file_exists _f) then [] else
    let _ch = open_in _f in
    let _c = really_input_string _ch (in_channel_length _ch) in
    let () = close_in _ch in
    let _l = String.split_on_char '\n' _c in
    let _l = List.filter (fun x -> x <> "") _l in
    let _l = List.map (fun x -> Digest.from_hex x) _l in
    (*let() = Format.printf "HEAD : %s" (concatStringList (List.map (fun x -> x) _l) "\n") in*)
    _l


let make_commit _s  _h =
    let _d = Unix.time () in
    let _p = get_head () in
    let _c = {parents = if _p = [] then [] else _p; date = _d; message = _s; content = _h} in
    _c


let init_commit () : commit = 
    let _s = "Initial commit" in
    let _h = Objects.store_work_directory() in
    make_commit _s _h


let store_commit _c = 
    let _p = List.map (fun x -> Digest.to_hex x) _c.parents in
    let _p = concatStringList _p ";" in
    let _d = date_fm _c.date in
    let _m = _c.message in
    let _h = Digest.to_hex (_c.content) in
    let _s = _p ^ "\n" ^ _d ^ "\n" ^ _m ^ "\n" ^ _h in
    (*let() = Format.printf "STORE : %s" _s in*)
    let hash = Digest.string _s in
    let _f = Filename.concat ".ogit/logs" (Digest.to_hex hash) in
    let _ch = open_out _f in
    let () = output_string _ch _s in
    let () = close_out _ch in
    let () = set_head [hash] in
    hash

let rec _print_list = function 
    [] -> ()
    | e::l -> print_string (e) ; print_string "\n" ; _print_list l

let read_commit (_h:Digest.t):commit = 
    let _f = Filename.concat ".ogit/logs" (Digest.to_hex _h) in
    if not (Sys.file_exists _f) then raise (Failure "Commit not found") else
    let _ch = open_in _f in
    let _c = really_input_string _ch (in_channel_length _ch) in
    let () = close_in _ch in
    let _l = String.split_on_char '\n' _c in
    let _p = List.nth _l 0 in
    let _d = date_fm_toFloat (List.nth _l 1) in
    let _m = List.nth _l 2 in
    let _i = Digest.from_hex (List.nth _l 3) in
    let _p = String.split_on_char ';' _p in
    let _p = List.filter (fun x -> x <> "") _p in
    
    (*let () = Format.printf "PARENTS : %d" (List.length _p) in
    _print_list _p;*)

    let _p = if List.length _p = 0 then [] else List.map (fun x -> Digest.from_hex x) _p in
    let _c = {parents = _p; date = _d; message = _m; content = _i} in
    _c