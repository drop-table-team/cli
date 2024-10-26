open H2

let usage_msg = "test [-verbose] <name>"
let verbose = ref false
let text = ref "Hi, "
let anon_fun input_text = text := !text ^ input_text
let speclist = [
    ("-verbose", Arg.Set verbose, "Output debug info")
]

let () = Arg.parse speclist anon_fun usage_msg;
    if !verbose then
        text := !text ^ " debug";
    print_endline !text
