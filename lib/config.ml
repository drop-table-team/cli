open Types

let action  = ref None
let address = ref None
let files = ref []
let query = ref None
let quiet = ref false
let tags = ref []

let file_configs =
    try In_channel.open_bin "/etc/drop-table-cli.conf"
        |> In_channel.input_lines
        |> List.map (fun line ->
            Str.(bounded_split (regexp "=") line 2) |> List.map String.trim
            |> (fun l -> List.(hd l, nth l 1)))
    with Sys_error _ -> []

let usage_msg = "hackathon_cli {chat|search|upload} [--address] [--tag] {<query>|<file> ...}"

let add_addr address_in =
    match !address with
    | None -> address := Some address_in
    | Some address -> 
        Printf.eprintf "Two definitions of --address: \"%s\" and \"%s\"\n" address address_in;
        exit 1

let speclist = [
    ("--address", Arg.String add_addr, "endpoint address");
    ("--tag", Arg.String (fun tag -> tags := tag :: !tags), "tag to narrow search (multiple allowed)");
    ("--quiet", Arg.Set quiet, "suppress response when uploading");
]

let anon_fun input =
    match !action with
    | None -> (
        match input with
        | "chat" -> action := Some ChatAction
        | "search" -> action := Some SearchAction 
        | "upload" -> action := Some UploadAction
        | _ ->
            prerr_endline "<action> must be `chat`, `search` or `upload`\n";
            exit 1
    )
    | Some ChatAction
    | Some SearchAction -> (
        match !query with
        | None -> query := Some input
        | Some query ->
            let () = Printf.eprintf "Two definitions of <query>: \"%s\" and \"%s\"\n" query input in
            exit 1
    )
    | Some UploadAction -> files := input :: !files

let get_address () = 
    match !address with
    | Some address -> address
    | None ->
        match List.find_opt (fun config -> fst config = "address") file_configs with
        | Some address_conf -> snd address_conf
        | None ->
            let () = prerr_endline "Address must be passed as argument or specified in config file" in
            exit 1

let get_module_server_address () =
    List.find (fun config -> fst config = "module_server_address") file_configs |> snd

let err_with_help () =
    Arg.usage speclist usage_msg;
    exit 1

let get () =
    Arg.parse speclist anon_fun usage_msg;
    let address = get_address () in
    match !action with
    | Some ChatAction -> (
        match !query with
        | Some query ->
            Chat {
                address;
                query;
            }
        | None -> err_with_help ()
    )
    | Some SearchAction -> (
        match !query with
        | Some query ->
            Search {
                address;
                query;
                tags = !tags;
            }
        | _ -> err_with_help ()
    )
    | Some UploadAction -> (
        match !files with
        | [] -> err_with_help ()
        | files -> 
            let modules = get_module_server_address () |> Server_comm.get_modules in
            Upload {
                address;
                files;
                modules;
                quiet = !quiet;
            }
    )
    | None -> err_with_help ()
