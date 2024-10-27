open Hackathon_cli
open Json_parsers
open Server_comm
open Types

let print_chat_response response =
    let () = print_endline response.text
    in if not @@ List.is_empty response.sources then
        let () = print_endline "\n--- Sources ---" in
        List.iter (fun ref -> print_endline @@ "[" ^ (Int.to_string ref.id) ^ "] (" ^ ref.uuid ^ ") " ^ ref.title ^ ": " ^ ref.snippet) response.sources

let print_document index (document: document) =
    let () = (index |> Int.to_string) ^ " (" ^ document.uuid ^ "): " ^ document.title |> print_endline
    and () = if not @@ List.is_empty document.tags then
        let () = List.hd document.tags |> print_string in
        List.tl document.tags |> List.iter (fun tag -> "; \n" ^ tag |> print_string)
    in print_endline @@ "\n" ^ document.summary ^ "\n"

let maybe_print_upload_result quiet result =
    if not quiet then
        let response = parse_json_upload result 
        in let () = print_newline (); print_endline @@ "\t" ^ response.title
        and () = print_endline @@ "Summary: " ^ response.summary
        and () = print_endline @@ "Short summary: " ^ response.short_summary
        in print_endline @@ "Tags: " ^ List.(fold_left
            (fun acc next -> acc ^ ", \"" ^ next ^ "\"")
            ("\"" ^ hd response.tags ^ "\"")
            (tl response.tags))

let () =
    let args = Config.get () in
    match args with
    | Chat args ->
        post
            ~headers:[("Content-Type", "application/json")]
            (args.address ^ "/ask")
            @@ "{\"prompt\": \"" ^ args.query ^ "\"}"
        |> Lwt_main.run |> parse_json_chat |> print_chat_response
    | Search args ->
        post
            ~headers:[("Content-Type", "application/json")]
            (args.address ^ "/query")
            (create_search_json args.query args.tags)
        |> Lwt_main.run |> parse_json_documents
        |> List.iteri print_document
    | Upload args ->
        args.files |> List.iter (fun file ->
            upload_file args.address file args.modules |> Lwt_main.run |>
            maybe_print_upload_result args.quiet)
