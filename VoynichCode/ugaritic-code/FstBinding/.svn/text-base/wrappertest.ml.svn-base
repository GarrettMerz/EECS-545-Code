let (fst, start) = OpenFst.new_fst ();;
let middle = OpenFst.add_state fst;;
let finish = OpenFst.add_state fst;;
let [one; two; three] = List.map char_of_int [1; 2; 3];;
OpenFst.add_arc fst start middle one one;;
OpenFst.add_arc fst start middle two two;;
OpenFst.add_arc fst middle finish three three;;
OpenFst.set_final fst finish;;
OpenFst.write_to_file fst "wrappertest.ocaml.out";;
