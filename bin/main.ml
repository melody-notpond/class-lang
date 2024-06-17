let print_usage () =
  Printf.eprintf
"usage:
    %s r [file] - run a file
    %s t [file] - typecheck a file
" Sys.argv.(0) Sys.argv.(0);;

let () =
  if Array.length Sys.argv = 1 || Array.length Sys.argv = 2 then begin
    print_usage ();
    exit 1
  end

let () =
  let name = Sys.argv.(2) in
  let contents = String.concat "\n" @@ Array.to_list @@ Arg.read_arg name in
  if Sys.argv.(1) = "r" then
    match Lang.run_string contents with
    | Ok v -> Printf.printf "%s:\n%s" name @@ String.concat "\n" v
    | Error e -> Printf.eprintf "error running %s: %s" name e; exit 1
  else if Sys.argv.(1) = "t" then
    match Lang.typecheck_string contents with
    | Ok v -> Printf.printf "%s:\n%s" name @@ String.concat "\n" v
    | Error e -> Printf.eprintf "error running %s: %s" name e; exit 1
  else begin
    print_usage();
    exit 1
  end
