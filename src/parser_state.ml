let line_number = ref 1
let reset_line_number () = line_number := 1
let increment_line_number () = line_number := !line_number + 1
let get_line_number () = !line_number

exception Exit