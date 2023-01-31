let context = ref false
let input1 = ref ""
let input2 = ref ""

let args = [("-context", Arg.Unit (fun () -> context := true), "Contain the context or not.");
            ("-t1", Arg.Set_string input1, "The first tree to compare.");
            ("-t2", Arg.Set_string input2, "The second tree to compare.")]
let () = Arg.parse args (fun x -> raise (Arg.Bad ("Bad argument : " ^ x))) "Tree23."

let _ = Tree_diff_lib.Tree23.diff !input1 !input2 !context