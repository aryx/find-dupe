open Common

let signature file = 
  match Common.cmd_to_list (spf "md5sum %s" (Filename.quote file)) with
  | [x] -> 
    if x =~ "^\\([0-9a-f]+\\) .*$"
    then Common.matched1 x
    else failwith (spf "not a md5sum signature %s for %s" x file)
  | _ -> failwith (spf "could not get signature for %s" file)
  

let ask_delete dupe orig =
  pr (spf "file %s\n =   %s" dupe orig);
  Common2.command2_y_or_no (spf "rm -f %s" (Filename.quote dupe)) |> ignore;
  ()

let main () =
  (* CONFIG ! *)
  let home = "/Users/luisa" in
  let dir1 = Filename.concat home "Downloads" in
  let other_dirs = Filename.concat home "Dropbox" in

  let files = Common.cmd_to_list (spf "find %s -type f" dir1) in
  let files_and_sig = files |> List.map (fun file -> file, signature file) in
  pr2 (spf "Done scanning %s" dir1);

  let other_files = Common.cmd_to_list (spf "find %s -type f" other_dirs) in
  (* use the Hashtbl.find_all property *)
  let hother_size = Hashtbl.create 1001 in

  other_files |> List.iter (fun file -> 
    Hashtbl.add hother_size (Common2.filesize file) file
  );
  let hdone = Hashtbl.create 1001 in
      
  pr2 (spf "Done scanning %s" other_dirs);
  
  files_and_sig |> List.iter (fun (file, k) ->
    let size = Common2.filesize file in
    if Hashtbl.mem hother_size size && size > 5000
    then begin 
      let candidates = Hashtbl.find_all hother_size size in
      match candidates |> Common.find_some_opt (fun candidate ->
        let md5 = 
          if Hashtbl.mem hdone candidate 
          then Hashtbl.find hdone candidate
          else begin
            let md5 = signature candidate in
            Hashtbl.add hdone candidate md5;
            md5
          end
        in
        if md5 = k
        then Some (candidate, md5)
        else None
      ) with
      | Some (orig, _) -> 
        ask_delete file orig  
      | None -> ()
    end
  )

let _ = main ()
