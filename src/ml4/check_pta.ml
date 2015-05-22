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

let check = function 
  | P4M_meta_zero_tok -> ()
  | P4M_meta_ident_tok -> ()
  | P4M_meta_random_tok ___6 -> ()
  | P4M_const_int_tok (___5, ___6) -> ()
  | P4M_const_float_tok (___5, ___6) -> ()
  | P4M_const_char_tok (___5, ___6) -> ()
  | P4M_const_wchar_tok (___5, ___6) -> ()

  | P4M_const_ch_str_sO_sL_tok (v,o,l,s) -> ()
  | P4M_const_ch_str_sO_L_tok (v,o,l,s) -> ()
  | P4M_const_ch_str_O_sL_tok (v,o,l,s) -> ()
  | P4M_const_ch_str_O_L_tok (v,o,l,s) -> ()
  | P4M_const_wch_str_sO_sL_tok (v,o,l,s) -> ()
  | P4M_const_wch_str_sO_L_tok (v,o,l,s) -> ()
  | P4M_const_wch_str_O_sL_tok (v,o,l,s) -> ()
  | P4M_const_wch_str_O_L_tok (v,o,l,s) -> ()

      (** global **)
  | P4M_g_sO_tok (___b, ___l, ___o, ___s, ___t) -> ()
  | P4M_g_dO_tok (___b, ___l,    _, ___s, ___t) -> ()
	(** auto **)
  | P4M_a_sO_tok (___c, ___d, ___b, ___l, ___o, ___s, ___t) -> ()
  | P4M_a_dO_tok (___c, ___d, ___b, ___l,    _, ___s, ___t) -> ()
	(** heap **)
  | P4M_h_sL_sO_tok (___c, ___d, ___b, ___l, ___o, ___s) -> ()
  | P4M_h_sL_dO_tok (___c, ___d, ___b, ___l,    _, ___s) -> ()
  | P4M_h_dL_sO_tok (___c, ___d, ___b,    _, ___o, ___s) -> ()
  | P4M_h_dL_dO_tok (___c, ___d, ___b,    _,    _, ___s) -> ()
  | P4M_pp_tok (___c, ___b, ___l, ___o, ___s, ___t) -> ()
  | P4M_clib_proc_tok (___a, ___s, ___t) -> ()
  | P4M_sta_proc_tok (___a, ___s, ___t) -> ()
  | P4M_dll_proc_tok (___a, ___s, ___t) -> ()
  | (___1, ___2, ___3, ___4, ___5, ___6, ___7, ___8, ___9)  -> ()

