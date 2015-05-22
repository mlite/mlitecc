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

type trans_phase = 
  | Phase_cabs
  | Phase_ast_aa_gram
  | Phase_ast_ba_stmt
  | Phase_ast_ca_expr
  | Phase_ast_da_type
  | Phase_ast_ea_expr
  | Phase_ast_eb_expr
  | Phase_ast_ec_expr
  | Phase_ast_fa_stmt
  | Phase_ast_ga_code
  | Phase_ast_ha_graf
  | Phase_ast_cmm
  | Phase_ast_ia_init
  | Phase_end

exception Phase_trans_done
    
let trans_phase = ref Phase_end
    
let cc_basic_phase_opts = 
  [
    ("--ast-cabs",
    Arg.Unit 
      (fun () -> trans_phase := Phase_cabs),
    "output frontc cabs C code");
    ("--ast-aa-gram", 
    Arg.Unit 
      (fun () -> trans_phase := Phase_ast_aa_gram),
    "output C99 gram C code");
    ("--ast-ba-stmt",
    Arg.Unit
      (fun () -> trans_phase := Phase_ast_ba_stmt),
    "output ba-stmt C code");
    ("--ast-ca-expr",
    Arg.Unit 
      (fun () -> trans_phase := Phase_ast_ca_expr),
    "output ca-expr C code");
    ("--ast-da-type",
    Arg.Unit 
      (fun () -> trans_phase := Phase_ast_da_type),
    "output da-type C code");
    ("--ast-ea-expr",
    Arg.Unit 
      (fun () -> trans_phase := Phase_ast_ea_expr),
    "output ea-expr C code");
    ("--ast-eb-expr",
    Arg.Unit 
      (fun () -> trans_phase := Phase_ast_eb_expr),
    "output eb-expr C code");
    ("--ast-ec-expr",
    Arg.Unit 
      (fun () -> trans_phase := Phase_ast_ec_expr),
    "output ec-expr C code");
    ("--ast-fa-stmt",
    Arg.Unit
      (fun () -> trans_phase := Phase_ast_fa_stmt),
    "output fa-stmt C code");
    ("--ast-ga-code",
    Arg.Unit
      (fun () -> trans_phase := Phase_ast_ga_code),
    "output ga-code C code");    
    ("--ast-ha-graf",
    Arg.Unit 
      (fun () -> trans_phase := (Phase_ast_ha_graf)),
    "output ha-graf C code");
    ("--ast-ia-init",
    Arg.Unit 
      (fun () -> trans_phase := (Phase_ast_ia_init)),
    "output ia-init C code");
    ("--ast-cmm",
    Arg.Unit 
      (fun () -> trans_phase := (Phase_ast_cmm)),
    "output cmm code");
  ]

let cc_intermediate_phase_opts = []
    
let cc_link_phase_opts = []
  
let trans_phase_opts = 
  cc_basic_phase_opts 
