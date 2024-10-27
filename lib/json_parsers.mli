open Types

val parse_json_chat : string -> chat_response

val parse_json_documents : string -> document list

val parse_modules : string -> modules list

val parse_json_upload : string -> upload_result
