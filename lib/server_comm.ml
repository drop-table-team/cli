open Lwt
open Cohttp
open Cohttp_lwt_unix

open Types

let get_modules address =
    let response = Client.get ("http://" ^ address ^ "/modules/input" |> Uri.of_string)
        >>= fun (_, body) -> Cohttp_lwt.Body.to_string body
    in Lwt_main.run response |> Json_parsers.parse_modules

let boundary = "LOLOLOL69696969696969420"

let rec tags_to_string tags =
    match tags with
    | head :: rest -> ", \"" ^ head ^ "\"" ^ tags_to_string rest
    | [] -> ""

let create_search_json query tags =
    {|{"query": "|}
    ^ query ^
    {|", "tags": [|}
    ^ (try
        ("\"" ^ List.hd tags ^ "\"") ^ (tags_to_string @@ List.tl tags)
        with Failure _ -> ""
    )^
    "]}"

let get_endpoint filename modules =
    let extension = Filename.extension filename |> (fun orig -> String.init (String.length orig - 1) (fun i -> String.get orig (i+1)))
    in try let input_module = List.find (fun input_module -> List.exists
        (fun mimetype -> String.split_on_char '/' mimetype |> Fun.flip List.nth 1 |> (=) extension)
        input_module.types
    ) modules
    in input_module.name
    with Not_found ->
        Printf.eprintf "File extension %s not supported by server\n" extension;
        exit 1

let get_content_type filename =
    match Filename.extension filename with
    | ".jpg" -> "image/jpg"
    | ".jpeg" -> "image/jpeg"
    | ".png" -> "image/png"
    | ".pdf" -> "application/pdf"
    | _ -> "text/plain"

let create_upload_body filename =
    "--" ^ boundary ^ "\r\n" ^
    {|Content-Disposition: form-data; name="file"; filename="|} ^ Filename.basename filename ^ "\"\r\n" ^
    "Content-Type: " ^ get_content_type filename ^ "\r\n\r\n" ^
    In_channel.(open_bin filename |> input_all) ^ "\r\n" ^
    "--" ^ boundary ^ "--"

let upload_header_list body_size = [
    ("Content-Type", "multipart/form-data; boundary=" ^ boundary);
    ("Content-Length", Int.to_string body_size);
]

let request meth ?headers address body =
    Client.call
        ~headers:(Header.init() |> match headers with
            | Some headers -> Fun.flip Header.add_list headers
            | None -> (fun x -> x)
        )
        ~body:(`String body)
        meth
        ("http://" ^ address |> Uri.of_string)
    >>= fun (_, body) -> Cohttp_lwt.Body.to_string body
    
let get = request `GET
let post = request `POST

let upload_file address filename modules =
    let endpoint = get_endpoint filename modules
    and body = create_upload_body filename in
    post 
        ~headers:(upload_header_list @@ String.length body)
        (address ^ "/modules/input/" ^ endpoint)
        body
