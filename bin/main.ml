open Lwt
open Cohttp
open Cohttp_lwt_unix
open Hackathon_cli
open Server_comm_utils
open Json_parsers

let get url body =
    Client.call ~body:(`String body) `GET ("http://" ^ url |> Uri.of_string)
    >>= fun (_, body) -> Cohttp_lwt.Body.to_string body

let upload_file address filename =
    let endpoint = get_endpoint filename
    and body = create_upload_body filename in
    Client.post
        ~body:(`String body)
        ~headers:(Header.add_list (Header.init ()) (header_list @@ String.length body))
        ("http://" ^ address ^ "/" ^ endpoint |> Uri.of_string)
    >>= fun (_, body) ->
    Cohttp_lwt.Body.to_string body

let print_chat_response response =
    let () = print_endline response.text
    in if not @@ List.is_empty response.sources then
        let () = print_endline "--- Sources ---" in
        List.iter (fun ref -> print_endline @@ "[" ^ (Int.to_string ref.id) ^ "] " ^ ref.title) response.sources

let print_document index document =
    let () = (index+1 |> Int.to_string) ^ ": " ^ document.title |> print_endline
    and () = if not @@ List.is_empty document.tags then
        let () = List.hd document.tags |> print_endline in
        List.tl document.tags |> List.iter (fun tag -> "; " ^ tag |> print_endline)
    in print_endline document.summary

let () =
    let args = Config.get () in
    match args with
    | Chat args -> let chat_response = get args.address args.query |> Lwt_main.run |> parse_json_chat in
        print_chat_response chat_response
    | Search args ->
        let documents = get args.address (create_search_json args.query args.tags) |> Lwt_main.run |> parse_json_documents in
        List.iteri print_document documents
    | Upload args -> List.iter (fun file -> upload_file args.address file |> Lwt_main.run |> print_endline) args.files
