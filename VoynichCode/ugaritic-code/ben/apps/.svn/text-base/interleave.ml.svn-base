open Util

let main files =
	let files = A.sub Sys.argv 1 (A.length Sys.argv - 1) in
	let inputs = A.map array_of_file files in
	let lengths = A.map A.length inputs in
	assert (A.all_same lengths);
	let n = lengths.(0) in
	for i = 0 to n - 1 do
		for j = 0 to A.length inputs - 1 do
			print_endline inputs.(j).(i)
		done
	done
		

let files = A.sub Sys.argv 1 (A.length Sys.argv - 1);;
main files

