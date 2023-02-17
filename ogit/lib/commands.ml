(** fichier commands.ml **)
(** fonctions représentant les commandes ogit **)

let ogit_init () =  
  let _f = Filename.concat "" ".ogit" in
  if Sys.file_exists _f then failwith ".ogit existe deja." else
    let _ = Unix.mkdir _f 0o777 in
    let _ = Unix.mkdir (Filename.concat _f "objects") 0o777 in
    let _ = Unix.mkdir (Filename.concat _f "logs") 0o777 in
    let _ = Logs.store_commit (Logs.init_commit ()) in
    ()

let srcs = [".cl"; ".cr"; ".CR"; ".CL"]
let is_conflict_file file = List.exists (Filename.check_suffix file) srcs

let check_conflict_in_tree () = 
  let _l = Sys.readdir "." in
  let _l = Array.to_list _l in
  let _l = List.filter (fun x -> String.length x = 32) _l in
  let _l = List.filter (fun x -> is_conflict_file x) _l in
  if List.length _l = 0 then () 
  else failwith "Conflit non résolu."


let ogit_commit _msg = 
  check_conflict_in_tree ();
  let _o = Objects.store_work_directory () in
  let _h = Logs.store_commit (Logs.make_commit _msg _o) in
  Logs.set_head [_h]

let rec _print_list = function 
  [] -> ()
  | e::l -> print_string (e) ; print_string "\n" ; _print_list l

let rec aux _h = 
  let _c = Logs.read_commit _h in
  if _c.parents = [] then _h::[] else _h::(aux (List.hd _c.parents))

let ogit_log () =
  let _h = List.hd(Logs.get_head ()) in
  let _l = aux _h in
  let _l = List.filter (fun x -> x <> "") _l in
  let _l = List.map (fun x -> Digest.to_hex x) _l in
  let _l = List.rev _l in
  let () = Format.printf "Commits : %d\n" (List.length _l) in 
  _print_list _l


let searchForCompleteHash (_hash:string):string list = 
  let _f = Filename.concat ".ogit" "logs" in
  let _l = Sys.readdir _f in
  let _l = Array.to_list _l in
  let _l = List.filter (fun x -> String.length x <> 32) _l in
  let _l = List.filter (fun x -> String.sub x 0 (String.length _hash) = _hash) _l in
  _l

let checkHash (_hash:string) : string = 
  if String.length _hash < 32 then 
  let _l = searchForCompleteHash _hash in
  if List.length _l = 0 then failwith "Hash non trouvé." else
  if List.length _l > 1 then failwith "Hash non unique." else
    List.hd _l
  else _hash

let ogit_checkout (_hash:string):unit = 
  (* Vestige des heures passé à essayer de comprendre pourquoi le Digest soulevé une erreur...
  let _c = String.to_bytes "test" in
  let _r = Digest.string "test" in
  let _r = Digest.to_hex _r in
  let _p = Digest.bytes _c in
  let _p = Digest.to_hex _p in
  let () = Format.printf "\n\n %s == %s : %b" _r _p (_r=_p) in
  
  let () = Format.printf "\n1. HASH : %s" _hash in
  let _h = Digest.to_hex _hash  in
  let () = Format.printf "\n2. HASH : %s" _h in
  let _h = Digest.from_hex _h  in
  let () = Format.printf "\n3. HASH : %s" _h in
  *)

  let _hash = checkHash _hash in
  let _c = Logs.read_commit (Digest.from_hex _hash) in
  let _r = Objects.read_directory_object _c.content in (* Exception dans read object si file not found. *)
  (*let () = Format.printf "c content : %s" (Digest.to_hex _c.content) in*)
  let _ = Objects.restore_work_directory _r in
  (*ogit_log*) ()

(* TODO Objects merge directory ~ fonctionne pas pour le moment *)
let ogit_merge (_hash:string) : unit =
  let _hash = checkHash _hash in
  let _c = Logs.read_commit (Digest.from_hex _hash) in
  let _r = Objects.read_directory_object _c.content in
  let _result = Objects.merge_work_directory_I _r in
  if _result then ogit_commit "Merge"
  else Format.printf "failwith Merge impossible, conflit." 
  (* failwith "Merge impossible, conflit."*)