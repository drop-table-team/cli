open Types

let invalid_res_err () = failwith "Invalid json received from server"

let get_json_entry obj key =
    List.find (fun entry -> fst entry = key) obj |> snd

let parse_json_documents response_text =
    let response_object = match Yojson.Basic.from_string response_text with
        | `Assoc response_object -> response_object
        | _ -> invalid_res_err ()
    in let documents = match List.hd response_object |> snd with
        | `List documents -> documents
        | _ -> invalid_res_err ()
    in List.map (fun document ->
        let document = match document with
            | `Assoc document -> document
            | _ -> invalid_res_err ()
        in let title = get_json_entry document "title"
        and summary = get_json_entry document "short"
        and uuid = get_json_entry document "uuid"
        and tags = get_json_entry document "tags"
        in match (title, uuid, summary, tags) with
        | (`String title, `String uuid, `String summary, `List tags) ->
            let tags = List.map (fun tag ->
                match tag with
                | `String tag -> tag
                | _ -> invalid_res_err ()
            ) tags in
            {title; uuid; summary; tags}
        | _ -> invalid_res_err ()
    ) documents

let parse_json_chat response_text =
    let response_object = match Yojson.Basic.from_string response_text with
        | `Assoc response_object -> response_object
        | _ -> invalid_res_err ()
    in let text = get_json_entry response_object "response"
    and sources = get_json_entry response_object "sources"
    in match (text, sources) with
    | (`String text, `List sources) ->
        let sources = List.map (fun source ->
            let source = match source with
            | `Assoc source -> source
            | _ -> invalid_res_err ()
            in let id = get_json_entry source "id"
            and uuid = get_json_entry source "uuid"
            and title = get_json_entry source "title"
            and snippet = get_json_entry source "snippet"
            in match (id, uuid, title, snippet) with
            | (`Int id, `String uuid, `String title, `String snippet) -> {id; uuid; title; snippet}
            | _ -> invalid_res_err ()
        ) sources in
        {text; sources}
    | _ -> invalid_res_err ()

let parse_modules modules_text =
    let modules = match Yojson.Basic.from_string modules_text with
    | `List modules -> modules
    | _ -> invalid_res_err ()
    in List.map (fun input_module ->
        let input_module = match input_module with
        | `Assoc input_module -> input_module
        | _ -> invalid_res_err ()
        in let name = get_json_entry input_module "name"
        and types = get_json_entry input_module "types"
        in match (name, types) with
        | (`String name, `List types) ->
            let types = List.map (fun mimetype ->
                match mimetype with
                | `String filetype -> filetype
                | _ -> invalid_res_err ()
            ) types
            in {name; types}
        | _ -> invalid_res_err ()
    ) modules

let parse_json_upload result_text =
    let result = match Yojson.Basic.from_string result_text with
    | `Assoc result -> result
    | _ -> invalid_res_err ()
    in let title = get_json_entry result "title"
    and summary = get_json_entry result "summary"
    and short_summary = get_json_entry result "short_summary"
    and tags = get_json_entry result "tags"
    in match (title, summary, short_summary, tags) with
    | (`String title, `String summary, `String short_summary, `List tags) ->
        let tags = List.map(fun tag -> match tag with
            | `String tag -> tag
            | _ -> invalid_res_err ()
        ) tags
        in {title; summary; short_summary; tags}
    | _ -> invalid_res_err ()
