let rec tags_to_string tags =
    match tags with
    | head :: rest -> ", \"" ^ head ^ "\"" ^ tags_to_string rest
    | [] -> ""

let create_search_json query tags =
    {|{"query": "|}
    ^ query ^
    {|", "tags": [|}
    ^ List.hd tags ^ (tags_to_string @@ List.tl tags) ^
    {|]}|}

let get_endpoint filename =
    match Filename.extension filename with
    | "gif"
    | "jpg"
    | "jpeg"
    | "png"
    | "webp" -> "convertImage"
    | _ -> ""

let boundary = "---------------LOLOLOL69696969696969420"

let get_content_type filename =
    match Filename.extension filename with
    | "gif" -> "image/gif"
    | "jpg"
    | "jpeg" -> "image/jpeg"
    | "png" -> "image/png"
    | "webp" -> "image/webp"
    | _ -> "text/plain"

let create_upload_body filename =
    boundary ^ "\n" ^
    {|Content-Disposition: form-data; name="file"; filename="|} ^ Filename.basename filename ^ "\"\n" ^
    "Content-Type: " ^ get_content_type filename ^ "\n\n" ^
    (In_channel.open_bin filename |> In_channel.input_all) ^ "\n" ^
    boundary ^ "--"

let header_list body_size = [
    ("Content-Type", "multipart/form-data; boundary="^boundary);
    ("Content-Length", Int.to_string body_size);
]
