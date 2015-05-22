(* 
   MLite C Compiler -- Ning Wang <email@ningwang.org> 2006-2010
   
   The name `Mlite C Compiler' belongs to us, but the code is available free for
   any use in any field of endeavor.  You may redistribute Mlite C Compiler in
   whole or in part.  We ask that, as a matter of courtesy, you acknowledge its
   source and include this LICENSE file.  You may modify Mlite C Compiler and
   create derived works, with which you may do as you like, but the result may
   not be called Mlite C Compiler without written consent.
   
   The software is placed in the public domain.  It is not protected by copyright,
   and it is not protected by a ``copyleft'' agreement like the one used by the
   Free Software Foundation.
*)

open Format
let doit te_tbl ce_tbl basename = 
  (*if !Mlite_config.enable_type_table_print then*)
    begin
      let out_chan = open_out (basename ^ ".te_tbl")
      in
      let out_fm = formatter_of_out_channel out_chan
      in
      Tent_printer.pp_print_te_tbl out_fm te_tbl;
      close_out out_chan;
      if !Mlite_config.enable_ce_tbl_print then
	let out_chan = open_out (basename ^ ".ce_tbl")
	in
	let out_fm = formatter_of_out_channel out_chan
	in
	Cent_printer.pp_print_ce_tbl out_fm ce_tbl;
	close_out out_chan;
    end
      
