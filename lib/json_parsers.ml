type source = {
    id: int;
    title: string;
}

type chat_response = {
    text: string;
    sources: source list;
}

type document = {
    title: string;
    summary: string;
    tags: string list;
}

let invalid_res_err () = failwith "Invalid response from server"

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
        and summary = get_json_entry document "summary"
        and tags = get_json_entry document "tags"
        in match (title, summary, tags) with
        | (`String title, `String summary, `List tags) ->
            let tags = List.map (fun tag ->
                match tag with
                | `String tag -> tag
                | _ -> invalid_res_err ()
            ) tags in
            {title; summary; tags}
        | _ -> invalid_res_err ()
    ) documents

let parse_json_chat response_text =
    let response_object = match Yojson.Basic.from_string response_text with
        | `Assoc response_object -> response_object
        | _ -> invalid_res_err ()
    in let answer = match List.hd response_object |> snd with
        | `Assoc answer -> answer
        | _ -> invalid_res_err ()
    in let text = get_json_entry answer "text"
    and sources = get_json_entry answer "sources"
    in match (text, sources) with
    | (`String text, `List sources) ->
        let sources = List.map (fun source ->
            let source = match source with
            | `Assoc source -> source
            | _ -> invalid_res_err ()
            in let id = get_json_entry source "id"
            and title = get_json_entry source "title"
            in match (id, title) with
            | (`Int id, `String title) -> {id; title}
            | _ -> invalid_res_err ()
        ) sources in
        { text; sources }
    | _ -> invalid_res_err ()
