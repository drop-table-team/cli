open Option

type chat_conf = {
    address: string;
    query: string;
}

type search_conf = {
    address: string;
    query: string;
    tags: string list;
}

type upload_conf = {
    address: string;
    files: string list;
}

type config_type =
    | Chat of chat_conf
    | Search of search_conf
    | Upload of upload_conf

type action_type =
    | ChatAction
    | SearchAction
    | UploadAction

let action = ref None
let files = ref []
let query = ref None
let tags = ref []
let address = ref None

let anon_fun input =
    match !action with
    | None -> (
        match input with
        | "chat" -> action := Some ChatAction
        | "search" -> action := Some SearchAction 
        | "upload" -> action := Some UploadAction
        | _ ->
            let () = prerr_endline "<action> must be `chat`, `query` or `upload`\n" in
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

let add_tag tag = tags := tag :: !tags

let add_addr address_in =
    match !address with
    | None -> address := Some address_in
    | Some address -> 
        let () = Printf.eprintf "Two definitions of --address: \"%s\" and \"%s\"\n" address address_in in
        exit 1

let speclist = [
    ("--address", Arg.String add_addr, "endpoint address");
    ("--tag", Arg.String add_tag, "tag to narrow search (multiple allowed)");
]

let usage_msg = "hackathon_cli {search|upload} [--address] [--tag] {<query>|<file> ...}"

let err_with_help () =
    let () = Arg.usage speclist usage_msg in
    exit 1

let file_configs =
    try In_channel.open_bin "/etc/drop-table-cli.conf"
    |> In_channel.input_lines
    |> List.map (fun line -> Str.bounded_split (Str.regexp "=") line 2 |> List.map String.trim
        |> (fun l -> (List.hd l, List.nth l 1)))
    with Sys_error _ -> []

let get_address_from_conf_file () = List.find_opt (fun config -> fst config = "address") file_configs

let get_address () = 
    match !address with
    | Some address -> address
    | None ->
        match get_address_from_conf_file () with
        | Some address_conf -> snd address_conf
        | None ->
            let () = prerr_endline "Address must be passed as argument or specified in config file" in
            exit 1

let get () =
    Arg.parse speclist anon_fun usage_msg;
    match !action with
    | Some ChatAction -> (
        let address = get_address () in
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
            let address = get_address () in
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
            let address = get_address () in
            Upload {
                address;
                files;
            }
    )
    | None -> err_with_help ()
