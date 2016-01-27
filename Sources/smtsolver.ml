class t =
object (self)

val mutable smtfilename = "default.smt"
val mutable smtfile = Unix.openfile "default.smt" [Unix.O_TRUNC;Unix.O_CREAT;Unix.O_WRONLY] 0o640

val smtout = Unix.pipe()
val sstdout = Unix.dup Unix.stdout

method sstdout = sstdout


method set_smtfilename filename = smtfilename <- Printf.sprintf "%s" filename


method smtout_r = fst smtout
method smtout_w = snd smtout

method open_smtwrite = ()

method smtwrite s =
  ignore (Unix.write smtfile s 0 (String.length s))

method close_smtwrite = 
  Unix.close smtfile;

method launch =
flush stderr; flush stdout;
match Unix.fork () with
    0 -> let ipid=Unix.getpid() in
         Unix.close self#smtout_r;
         (*Utils.print "SMT Solver processing ...\n"; flush stderr;*)
	 Unix.dup2 self#smtout_w Unix.stdout;
	 ignore (Unix.execvp "smt-solver/mathsat" [|"-input=smt";smtfilename|]);
(*	 ignore (Sys.command (Printf.sprintf "smt-solver/run < %s" smtfilename)); *)
	 Unix.dup2 self#sstdout Unix.stdout;
	 Utils.print "Error : launching SMT solver.\n"; flush stderr;
	 Unix.close self#smtout_w;
	 exit 0;
  | pid  -> Unix.close self#smtout_w;
            Unix.close self#sstdout;
	    ignore (Unix.waitpid [] pid);
	    let c = Unix.in_channel_of_descr self#smtout_r in
	      let result = input_line c in
		(*Utils.print "SMT solver result : %s\n" result;*)
		Unix.close self#smtout_r;
                (*Printf.printf "Child [%d] returns %s\n" pid result;*)
		if (String.compare result "sat") = 0
		then true
		else false



end