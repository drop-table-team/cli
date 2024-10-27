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

type config =
    | Chat of chat_conf
    | Search of search_conf
    | Upload of upload_conf

val get : unit -> config
