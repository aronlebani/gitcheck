let is_git (dir : string) : bool =
    Sys.readdir dir |> Array.exists @@ fun x -> String.equal x ".git"

let check_for_unstaged_changes (dir : string) =
    match Sys.command ("cd " ^ dir ^ " && git diff-index --quiet HEAD") with
    | 1 -> Printf.printf "%s has uncommitted changes\n" dir
    | _ -> ()

let rec crawl (file : string) =
  match Sys.file_exists file with
  | false -> ()
  | true -> (
      match Sys.is_directory file with
      | false -> ()
      | true -> (
          match is_git file with
          | false -> Array.iter crawl (Array.map (fun x -> Filename.concat file x) (Sys.readdir file))
          | true -> check_for_unstaged_changes file))

let () =
    let path = Array.get Sys.argv 1 in
    try path with
    | Invalid_argument _ -> Printf.printf "Usage: gitcheck <path>"
    | _ -> crawl path
