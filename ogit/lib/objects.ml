type t =
| Text of string 
| Directory of (string * bool * Digest.t * t) list

let getText (_obj:t) : string =
  match _obj with
  | Text _s -> _s
  | Directory _l -> 
    let r = String.concat "\n" (List.map (fun (_n, _d, _h, _o) -> _n ^ ";" ^ (if _d then "d" else "t") ^ ";" ^ (Digest.to_hex _h)) _l) in
     (*let () = Format.printf "%s" r in *)
    r

let hash (_obj:t) : Digest.t =
  let r = Digest.string (getText _obj) in 
    (*let () = Format.printf "\nhash: %s@." (Digest.to_hex r) in *)
    r

let is_known _h = Sys.file_exists (Filename.concat ".ogit/objects/" (Digest.to_hex _h))

let store_object (_obj:t) : Digest.t = 
  let _h = hash _obj in (* if is_known _h *)
  let _f = Filename.concat ".ogit/objects/" (Digest.to_hex _h) in
  let _c = getText _obj in
  let _d = Filename.dirname _f in
  let () = if not (Sys.file_exists _d) then Unix.mkdir _d 0o755 in
  let () = if Sys.file_exists _f then () else
    let _ch = open_out _f in
    let () = output_string _ch _c in
    let () = close_out _ch in
    ()
  in
  _h

let read_text_object (_h:Digest.t):string = (* if is_known _h *)
  let _f = Filename.concat ".ogit/objects/" (Digest.to_hex _h) in
  if not (Sys.file_exists _f) then failwith "read_text_object: file not found" else
  let _ch = open_in _f in
  let _c = really_input_string _ch (in_channel_length _ch) in
  let () = close_in _ch in
  _c


(** crée dans .ogit/objects l'objet correspondant au répertoire repo/ et tous ceux qu'il contient récursivement, renvoie le hash de cet objet 
val store_work_directory : unit -> Digest.t    **)

let _int_of_bool b = if b then 1 else 0

let rec auxy _s = 
  let rr = _s in
  if Sys.is_directory rr then
    (*let () = Format.printf "\nstart : %s \n" rr in*)
    let _l = Array.to_list (Sys.readdir _s) in
    let _l = List.filter (fun _s -> String.get _s 0 <> '.') _l in
    let _l = List.rev _l in
    let _l = List.sort (fun _s1 _s2 -> if _int_of_bool(Sys.is_directory (Filename.concat rr _s1)) > _int_of_bool(Sys.is_directory (Filename.concat rr _s2)) then 1 else 0) _l in
    let _l = List.map (fun _s -> 
      let _s = (* Filename.concat "." *) _s in
      let path = Filename.concat rr _s in
      (*let () = Format.printf "namefile : %s ; Path : %s\n" _s path in*)
      let _d = Sys.is_directory path in
      let _o = if _d then Directory (auxy path) else 
        let _ch = open_in path in
        let _c = really_input_string _ch (in_channel_length _ch) in
        let () = close_in _ch in
        Text _c in
      let _h = store_object _o in
      (*let () = Format.printf "%s->%s\n" (getText _o) (Digest.to_hex _h) in*)
      (_s, _d, _h, _o)
    ) _l in
    _l
  else
  []

let store_work_directory () : Digest.t = 
  (*let () = Format.printf "Good Hash: %s\n" (Digest.to_hex (Digest.string "titi.txt;t;ec7e8e8ce16f3fe945d3c4c0955315d3\ntoto.txt;t;bb6d4c2870158394ea6b4c6ec804d61f\nbar;d;87113b72a9d03cbfb8aea342455d710f")) in*)
  (*let () = Format.printf "Bad Hash: %s\n\n" (Digest.to_hex (Digest.string "toto.txt;t;bb6d4c2870158394ea6b4c6ec804d61f\ntiti.txt;t;ec7e8e8ce16f3fe945d3c4c0955315d3\nbar;d;87113b72a9d03cbfb8aea342455d710f")) in*)
  let _l = auxy "." in
  let _o = Directory _l in
  let _h = store_object _o in
  _h

(** lit le fichier de .ogit/objects identifié par le hash, et correspondant à un objet répertoire, et renvoie cet objet répertoire 
val read_directory_object : Digest.t -> t    **)

let rec _print_list = function 
[] -> ()
| e::l -> print_string e ; print_string "\n" ; _print_list l

let rec read_directory_object (_h:Digest.t):t =
  let _f = read_text_object _h in
  (*let () = Format.printf "read_directory_object: %s\n\n" _f in*)
  let _l = String.split_on_char '\n' _f in
  (*let () = Format.printf "\n\\n length : %d\n" (List.length _l) in*)
  (*_print_list _l;*)
  let _l = List.map (fun _s ->
      let _s = String.split_on_char ';' _s in
      (*let () = Format.printf "\n ; length : %d\n" (List.length _s) in*)
      (*_print_list _s;*)
      let _n = List.hd _s in
      let _d = if List.nth _s 1 = "d" then true else false in
      let _h = Digest.from_hex(List.nth _s 2) in
      if(_d) then
        let _o = read_directory_object _h in (* Creation d'un nouveau repertoire et des fichier text apres qu'il contient :) *)
        (*let () = Format.printf "\nn : %s   d : %b   h : %s   _o : %s" _n _d _h (read_text_object _h) in*)
        (_n, _d, _h, _o)
      else
        let _o = Text (read_text_object _h) in (* Création d'un nouveau text *)
        (*let () = Format.printf "\nn : %s   d : %b   h : %s   _o : %s" _n _d _h (read_text_object _h) in*)
        (_n, _d, _h, _o)
    ) _l in
  let _o = Directory _l in
  _o


let rec _rmrf path name = match Sys.is_directory path with
| true -> Sys.readdir path |> Array.iter 
  (fun name -> 
    let path = Filename.concat path name in
    if String.get name 0 = '.' then () else 
      (*let () = Format.printf "fildedelete: %s" path in*)
      _rmrf path name
  );
  if name <> "." (*&& String.get name 0 <> '.'*) then Unix.rmdir path else ()
| false -> if String.get name 0 = '.' then () else Unix.unlink path

let clean_work_directory () = 
  _rmrf "." "."

let restore_work_directory (_obj:t) : unit = 
  clean_work_directory();
  let rec auxy _obj = match _obj with
  | Text _s -> _s
  | Directory _l -> 
    let _l = List.map (fun (_n, _d, _h, _o) -> 
      let _s = getText _o in
      if _d then
        let () = Unix.mkdir _n 0o777 in
        let () = Unix.chdir _n in
        let _ = auxy _o in 
        let () = Unix.chdir ".." in
        _s
      else
        let _ch = open_out _n in
        let () = output_string _ch _s in
        let () = close_out _ch in
        _s
    ) _l in
    String.concat "" _l
  in
  let _s = auxy _obj in
  ()


let rec _checkIfExist name path r = 
  let _l = Array.to_list (Sys.readdir path) in
  let _l = List.filter (fun _s -> String.get _s 0 <> '.') _l in
  let _l = List.map (fun _s -> 
    if Sys.is_directory _s then
      if r || _s = name then 
        let r = true in 
        (r, (Filename.concat path _s))
      else
        _checkIfExist name (Filename.concat path _s) r
    else
      if _s = name then 
        let r = true in 
        (r, (Filename.concat path _s))
      else 
        let r = false in 
        (r, (Filename.concat path _s))
    ) _l in
  let _l = List.filter (fun (_a, _b) -> _a = true) _l in
  if List.length _l > 0 then 
    List.hd _l 
  else 
    (false, "")


(* Fonctionne pas exactement comme prévu mais l'idée est là, erreur file not exist, probleme avec le chemin passé en param *)
(* Enlever les commentaires pour voir, laisser les commentaires pour pouvoir effectuer les test sur toutes les commandes sauf merge. *)
let merge_work_directory_I (_obj:t) : bool = (*
  let rec aux _obj = match _obj with
  | Text _s -> true
  | Directory _l -> 
    let _l = List.map (fun (_n, _d, _h, _o) -> 
      if _d then
        let (_res, _path) = _checkIfExist _n "." false in
        if not (_res) then
          let () = Unix.mkdir _n 0o777 in (* Si dossier pas existant, on le cree *)
          let _ = aux _o in 
          true
        else
          true
      else
        let (_res, _path) = _checkIfExist _n "." false in
        if (_res) then
          let _s = read_text_object _h in
          let _ch = open_in _path in
          let _c = really_input_string _ch (in_channel_length _ch) in
          let () = close_in _ch in
          if _s = _c then
            (* IGNORE *)
            true
          else
            (* CONFLIT *)
            let _ch = open_out (_path ^ ".cl") in
            let () = output_string _ch _s in
            let () = close_out _ch in

            let _ch = open_out (_path ^ ".cr") in
            let () = output_string _ch _c in
            let () = close_out _ch in
            Unix.unlink _path;
            false
        else
          let _ch = open_out _path in
          let () = output_string _ch (read_text_object _h) in
          let () = close_out _ch in
          true
          
    ) _l in
    let _l = List.filter (fun _b -> _b = true) _l in
      if List.length _l > 0 then true 
      else false
  in
  aux _obj
  *)
  true








(*
let _merge_work_directory_2way (_obj:t) : unit =
  let rec auxy _obj = match _obj with
  | Text _s -> _s
  | Directory _l -> 
    let _l = List.map (fun (_n, _d, _h, _o) -> 
      if _d then
        let _s = auxy _o in
        _s
      else
        let _s = getText _o in
        _s
    ) _l in
    String.concat "" _l
  in
  let _s = auxy _obj in
  let _h = store_object (Text _s) in
  let _h2 = store_work_directory() in
  if _h <> _h2 then
    let () = Format.printf "Merge Conflict" in
    ()
  else
    let () = Format.printf "Merge OK" in
    ()

let _merge_work_directory_3way (_obj:t) : unit = 
  let rec auxy _obj = match _obj with
  | Text _s -> _s
  | Directory _l -> 
    let _l = List.map (fun (_n, _d, _h, _o) -> 
      if _d then
        let _s = auxy _o in
        _s
      else
        let _s = getText _o in
        _s
    ) _l in
    String.concat "" _l
  in
  let _s = auxy _obj in
  let _h = store_object (Text _s) in
  let _h2 = store_work_directory() in
  if _h <> _h2 then
    let _h3 = merge_work_directory_I _obj in
    if _h3 then
      let () = Format.printf "Merge Conflict" in
      ()
    else
      let () = Format.printf "Merge Conflict" in
      ()
  else
    ()


  let rec auxy _obj = match _obj with
  | Text _s -> _s
  | Directory _l -> 
    let _l = List.map (fun (_n, _d, _h, _o) -> 
      if _d then
        let _s = auxy _o in
        _s
      else
        let _s = getText _o in
        _s
    ) _l in
    String.concat "" _l
  in
  let _s = auxy _obj in
  let _h = store_object (Text _s) in
  let _h2 = store_work_directory() in
  _h = _h2

let merge_work_directory_3way (_obj:t) : unit = 
  let rec auxy _obj = match _obj with
  | Text _s -> _s
  | Directory _l -> 
    let _l = List.map (fun (_n, _d, _h, _o) -> 
      if _d then
        let _s = auxy _o in
        _s
      else
        let _s = getText _o in
        _s
    ) _l in
    String.concat "" _l
  in
  let _s = auxy _obj in
  let _h = store_object (Text _s) in
  let _h2 = store_work_directory() in
  if _h <> _h2 then
    let _s2 = auxy _obj in
    let _ch = open_out "merge" in
    let () = output_string _ch _s2 in
    let () = close_out _ch in
    let () = Format.printf "Merge file created" in
    ()
  else
    let () = Format.printf "Nothing to merge" in
    ()

let merge_work_directory_3way_2 (_obj:t) : unit = 
  let rec auxyy _obj = match _obj with
  | Text _s -> _s
  | Directory _l -> 
    let _l = List.map (fun (_n, _d, _h, _o) -> 
      if _d then
        let _s = auxy _o in
        _s
      else
        let _s = getText _o in
        _s
    ) _l in
    String.concat "" _l
  in
  let _s = auxyy _obj in
  let _h = store_object _s in
  let _h2 = store_work_directory() in
  if _h <> _h2 then
    let _s2 = auxyy _obj in
    let _ch = open_out "merge" in
    let () = output_string _ch _s2 in
    let () = close_out _ch in
    let () = Format.printf "Merge file created" in
    ()
  else
    let () = Format.printf "Nothing to merge" in
    ()
*)