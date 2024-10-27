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

val parse_json_chat : string -> chat_response

val parse_json_documents : string -> document list
