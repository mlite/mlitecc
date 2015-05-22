let handle = ref (Parser_handle.scan_args [])

let init hdl =
    handle := hdl

let d s = Parser_handle.shift !handle s
let reduce s = Parser_handle.reduce !handle s
let get_lineno () = Parser_handle.get_lineno !handle
let get_filename () = Parser_handle.get_filename !handle
let syntax_error s = Parser_handle.syntax_error !handle s
let incr_lineno () = ()
