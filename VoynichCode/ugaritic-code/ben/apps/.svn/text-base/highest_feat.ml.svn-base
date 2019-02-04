open Util
open Sys

let datas = Array.map open_in (Array.sub argv 1 (Array.length argv - 1))
let datas' = Array.map (fun d -> fst (parse_data_file d)) datas 
let data = List.flatten (Array.to_list datas');;
print_endline (string_of_int (get_highest_feature data))
