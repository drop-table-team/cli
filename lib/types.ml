type modules = {
    name: string;
    types: string list;
}

type source = {
    id: int;
    uuid: string;
    title: string;
    snippet: string;
}

type chat_response = {
    text: string;
    sources: source list;
}

type document = {
    title: string;
    summary: string;
    tags: string list;
    uuid: string;
}

type upload_result = {
    title: string;
    summary: string;
    short_summary: string;
    tags: string list;
}

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
    modules: modules list;
    quiet: bool;
}

type config =
    | Chat of chat_conf
    | Search of search_conf
    | Upload of upload_conf

type action_type =
    | ChatAction
    | SearchAction
    | UploadAction
