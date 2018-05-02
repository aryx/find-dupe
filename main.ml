open Common

let signatures files = 
  files |> List.map (fun file ->
    match Common.cmd_to_list (spf "md5sum \"%s\"" file) with
    | [x] -> 
      if x =~ "^\\([0-9a-f]+\\) .*$"
      then Common.matched1 x, file
      else failwith (spf "not a md5sum signature %s for %s" x file)
    | _ -> failwith (spf "could not get signature for %s" file)
  )

let ask_delete dupe orig =
  pr2 (spf "file %s is the same than %s" dupe orig);
  ()

let main () =
  let dir1 = "/home/pad/Downloads/" in
  let other_dirs = "/home/pad/Documents/" in

  let files = 
    Common.cmd_to_list (spf "find %s -type f" dir1) in
  let hfiles = signatures files |> Common.hash_of_list in
  pr2 (spf "Done scanning %s" dir1);

  let other_files = 
    Common.cmd_to_list (spf "find %s -type f" other_dirs) in
  let hother = signatures other_files |> Common.hash_of_list in

  pr2 (spf "Done scanning %s" other_dirs);
  
  hfiles |> Hashtbl.iter (fun k file ->
    if Hashtbl.mem hother k
    then ask_delete file (Hashtbl.find hother k)
  )

let _ = main ()
