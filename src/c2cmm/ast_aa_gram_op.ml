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

include Ast_aa_gram
include C_syntax_symbol

let c_integer_one = (Constant ("1", DEC_NONE))


let const_one_c_expression = 
  Expression_1
    (Assignment_expression_1
       (Conditional_expression_1
	  (Logical_or_expression_1
	     (Logical_and_expression_1
		(Inclusive_or_expression_1
		   (Exclusive_or_expression_1
		      (And_expression_1
			 (Equality_expression_1
			    (Relational_expression_1
			       (Shift_expression_1
				  (Additive_expression_1
				     (Multiplicative_expression_1
					(Cast_expression_1
					   (Unary_expression_1
					      (Postfix_expression_1
						 (Primary_expression_2
						    (Constant_integer 
						       c_integer_one
						    )
						 )
					      )
					   )
					)
				     )
				  )
			       )
			    )
			 )
		      )
		   )
		)
	     )
	  )
       )
    )

let crt_c_expression_by_c_primary_expression a = 
  Expression_1 
    (Assignment_expression_1 
      (Conditional_expression_1 
	  (Logical_or_expression_1 
	     (Logical_and_expression_1
		(Inclusive_or_expression_1
		   (Exclusive_or_expression_1
		      (And_expression_1
			 (Equality_expression_1
			    (Relational_expression_1
			       (Shift_expression_1
				  (Additive_expression_1
				     (Multiplicative_expression_1
					(Cast_expression_1
					   (Unary_expression_1 
					      (Postfix_expression_1 a
					      )))))))))))))))

let crt_c_expression_by_c_postfix_expression a = 
  Expression_1 
    (Assignment_expression_1 
       (Conditional_expression_1 
	  (Logical_or_expression_1 
	     (Logical_and_expression_1
		(Inclusive_or_expression_1
		   (Exclusive_or_expression_1
		      (And_expression_1
			 (Equality_expression_1
			    (Relational_expression_1
			       (Shift_expression_1
				  (Additive_expression_1
				     (Multiplicative_expression_1
					(Cast_expression_1
					   (Unary_expression_1 a
					   ))))))))))))))
    

let crt_c_expression_by_c_unary_expression a = 
   Expression_1 
    (Assignment_expression_1 
       (Conditional_expression_1 
	  (Logical_or_expression_1 
	     (Logical_and_expression_1
		(Inclusive_or_expression_1
		   (Exclusive_or_expression_1
		      (And_expression_1
			 (Equality_expression_1
			    (Relational_expression_1
			       (Shift_expression_1
				  (Additive_expression_1
				     (Multiplicative_expression_1
					(Cast_expression_1 a
					)))))))))))))
    

let crt_c_expression_by_c_cast_expression a = 
  Expression_1 
    (Assignment_expression_1 
       (Conditional_expression_1 
	  (Logical_or_expression_1 
	     (Logical_and_expression_1
		(Inclusive_or_expression_1
		   (Exclusive_or_expression_1
		      (And_expression_1
			 (Equality_expression_1
			    (Relational_expression_1
			       (Shift_expression_1
				  (Additive_expression_1
				     (Multiplicative_expression_1 a
				     ))))))))))))
    

let crt_c_expression_by_c_multiplicative_expression a = 
  Expression_1 
    (Assignment_expression_1 
       (Conditional_expression_1 
	  (Logical_or_expression_1 
	     (Logical_and_expression_1
		(Inclusive_or_expression_1
		   (Exclusive_or_expression_1
		      (And_expression_1
			 (Equality_expression_1
			    (Relational_expression_1
			       (Shift_expression_1
				  (Additive_expression_1 a
				  )))))))))))


let crt_c_expression_by_c_additive_expression a = 
  Expression_1 
    (Assignment_expression_1 
       (Conditional_expression_1 
	  (Logical_or_expression_1 
	     (Logical_and_expression_1
		(Inclusive_or_expression_1
		   (Exclusive_or_expression_1
		      (And_expression_1
			 (Equality_expression_1
			    (Relational_expression_1
			       (Shift_expression_1 a
			       ))))))))))
    

let crt_c_expression_by_c_shift_expression a = 
  Expression_1 
    (Assignment_expression_1 
       (Conditional_expression_1 
	  (Logical_or_expression_1 
	     (Logical_and_expression_1
		(Inclusive_or_expression_1
		   (Exclusive_or_expression_1
		      (And_expression_1
			 (Equality_expression_1
			    (Relational_expression_1 a
			    )))))))))


let crt_c_expression_by_c_relational_expression a = 
  Expression_1 
    (Assignment_expression_1 
       (Conditional_expression_1 
	  (Logical_or_expression_1 
	     (Logical_and_expression_1
		(Inclusive_or_expression_1
		   (Exclusive_or_expression_1
		      (And_expression_1
			 (Equality_expression_1 a
			 ))))))))


let crt_c_expression_by_c_equality_expression a = 
  Expression_1 
    (Assignment_expression_1 
       (Conditional_expression_1 
	  (Logical_or_expression_1 
	     (Logical_and_expression_1
		(Inclusive_or_expression_1
		   (Exclusive_or_expression_1
		      (And_expression_1 a
		      )))))))


let crt_c_expression_by_c_and_expression a = 
  Expression_1 
    (Assignment_expression_1 
       (Conditional_expression_1 
	  (Logical_or_expression_1 
	     (Logical_and_expression_1
		(Inclusive_or_expression_1
		   (Exclusive_or_expression_1 a
		   ))))))

let crt_c_expression_by_c_exclusive_or_expression a = 
  Expression_1 
    (Assignment_expression_1 
       (Conditional_expression_1 
	  (Logical_or_expression_1 
	     (Logical_and_expression_1
		(Inclusive_or_expression_1 a
		)))))

let crt_c_expression_by_c_inclusive_or_expression a = 
  Expression_1 
    (Assignment_expression_1 
       (Conditional_expression_1 
	  (Logical_or_expression_1 
	     (Logical_and_expression_1 a
	     ))))


let crt_c_expression_by_c_logical_and_expression a = 
  Expression_1 
    (Assignment_expression_1 
       (Conditional_expression_1 
	  (Logical_or_expression_1 a
	  )))


let crt_c_expression_by_c_logical_or_expression a = 
  Expression_1 
    (Assignment_expression_1 
       (Conditional_expression_1 a
       ))


let crt_c_expression_by_c_conditional_expression a = 
   Expression_1 
    (Assignment_expression_1 a
    )

let crt_c_expression_by_c_assignment_expression a = 
  Expression_1 a


let rec get_c_assignment_expression: c_expression -> c_assignment_expression = 
  fun expr ->
    match expr with
    | Expression_1 v -> v
    | _ -> 
	get_c_assignment_expression 
	  (crt_c_expression_by_c_primary_expression 
	     (Primary_expression_4 expr))


let rec get_c_conditional_expression: c_expression -> c_conditional_expression = 
  fun expr ->
    let expr' = get_c_assignment_expression expr
    in
    match expr' with
    | Assignment_expression_1 v -> v
    | _ -> 
	get_c_conditional_expression 
	  (crt_c_expression_by_c_primary_expression 
	     (Primary_expression_4 expr))


let rec get_c_logical_or_expression: c_expression -> c_logical_or_expression =
  fun expr ->
    let expr' = get_c_conditional_expression expr
    in
    match expr' with
    | Conditional_expression_1 v -> v
    | _ -> 
	get_c_logical_or_expression 
	  (crt_c_expression_by_c_primary_expression 
	     (Primary_expression_4 expr))


let rec get_c_logical_and_expression: c_expression -> c_logical_and_expression = 
  fun expr ->
    let expr' = get_c_logical_or_expression expr
    in
    match expr' with
    | Logical_or_expression_1 v -> v
    | _ -> 
	get_c_logical_and_expression 
	  (crt_c_expression_by_c_primary_expression 
	     (Primary_expression_4 expr))


let rec get_c_inclusive_or_expression: c_expression -> c_inclusive_or_expression = 
  fun expr ->
    let expr' = get_c_logical_and_expression expr
    in
    match expr' with
    | Logical_and_expression_1 v -> v
    | _ -> 
	get_c_inclusive_or_expression 
	  (crt_c_expression_by_c_primary_expression 
	     (Primary_expression_4 expr))


let rec get_c_exclusive_or_expression: c_expression -> c_exclusive_or_expression = 
  fun expr ->
    let expr' = get_c_inclusive_or_expression expr
    in
    match expr' with
    | Inclusive_or_expression_1 v -> v
    | _ -> 
	get_c_exclusive_or_expression 
	  (crt_c_expression_by_c_primary_expression 
	     (Primary_expression_4 expr))


let rec get_c_and_expression: c_expression -> c_and_expression = 
  fun expr ->
    let expr' = get_c_exclusive_or_expression expr
    in
    match expr' with
    | Exclusive_or_expression_1 v -> v
    | _ -> 
	get_c_and_expression 
	  (crt_c_expression_by_c_primary_expression 
	     (Primary_expression_4 expr))


let rec get_c_equality_expression: c_expression -> c_equality_expression = 
  fun expr ->
    let expr' = get_c_and_expression expr
    in
    match expr' with
    | And_expression_1 v -> v
    | _ -> 
	get_c_equality_expression 
	  (crt_c_expression_by_c_primary_expression 
	     (Primary_expression_4 expr))


let rec get_c_relational_expression: c_expression -> c_relational_expression = 
  fun expr ->
    let expr' = get_c_equality_expression expr
    in
    match expr' with
    | Equality_expression_1 v -> v
    | _ -> 
	get_c_relational_expression 
	  (crt_c_expression_by_c_primary_expression 
	     (Primary_expression_4 expr))


let rec get_c_shift_expression: c_expression -> c_shift_expression = 
  fun expr ->
    let expr' = get_c_relational_expression expr
    in
    match expr' with
    | Relational_expression_1 v -> v
    | _ -> 
	get_c_shift_expression 
	  (crt_c_expression_by_c_primary_expression 
	     (Primary_expression_4 expr))



let rec get_c_additive_expression: c_expression -> c_additive_expression = 
  fun expr ->
    let expr' = get_c_shift_expression expr
    in
    match expr' with
    | Shift_expression_1 v -> v
    | _ -> 
	get_c_additive_expression 
	  (crt_c_expression_by_c_primary_expression 
	     (Primary_expression_4 expr))


let rec get_c_multiplicative_expression: c_expression -> c_multiplicative_expression = 
  fun expr ->
    let expr' = get_c_additive_expression expr
    in
    match expr' with
    | Additive_expression_1 v -> v
    | _ -> 
	get_c_multiplicative_expression 
	  (crt_c_expression_by_c_primary_expression 
	     (Primary_expression_4 expr))


let rec get_c_cast_expression: c_expression -> c_cast_expression =
  fun expr ->
    let expr' = get_c_multiplicative_expression expr
    in
    match expr' with
    | Multiplicative_expression_1 v -> v
    | _ ->
	get_c_cast_expression 
	  (crt_c_expression_by_c_primary_expression 
	     (Primary_expression_4 expr))


let rec get_c_unary_expression: c_expression -> c_unary_expression = 
  fun expr ->
    let expr' = get_c_cast_expression expr
    in
    match expr' with
    | Cast_expression_1 v -> v 
    | _ -> 
	get_c_unary_expression 
	  (crt_c_expression_by_c_primary_expression 
	     (Primary_expression_4 expr))


let rec get_c_postfix_expression: c_expression -> c_postfix_expression = 
  fun expr ->
    let expr' = get_c_unary_expression expr
    in
    match expr' with
    | Unary_expression_1 v -> v
    | _ -> 
	get_c_postfix_expression 
	  (crt_c_expression_by_c_primary_expression 
	     (Primary_expression_4 expr))



let c_integer_suffix = ['u';'U';'l';'L']

let extract_c_integer_constant: Cabs.int_base -> string -> c_integer_constant =
  fun int_base str ->
    let cut_suffix: string -> char -> string * bool =
      fun c_int ch ->
	let len = String.length c_int
	in
	if c_int.[len - 1] = ch then
	  let str = String.sub c_int 0 (len - 1)
	  in
	  (str, true)
	else
	  (c_int, false)
    in
    let long_cnt = ref 0
    and is_unsigned = ref false
    and remain_str = ref str
    and changed = ref true
    in
    let _ = 
      while !changed do
	begin
	  changed := false;
	  List.iter
	    (fun suffix ->
	      let (str, has_suffix) = cut_suffix !remain_str suffix
	      in
	      if has_suffix then
		begin
		  changed := true;
		  remain_str := str;
		  if suffix = 'u' or suffix = 'U' then
		    is_unsigned := true
		  else if suffix = 'l' or suffix = 'L' then
		    incr long_cnt
		end
	    ) c_integer_suffix
	end
      done
    in
    let suffix = 
      if !is_unsigned then
	begin
	  match !long_cnt with
	    | 0 -> 
		begin
		  match int_base with
		    | Cabs.DEC -> DEC_U
		    | Cabs.HEX -> HEX_U
		    | Cabs.OCT -> OCT_U
		end
	    | 1 -> 
		begin
		  match int_base with
		    | Cabs.DEC -> DEC_UL
		    | Cabs.HEX -> HEX_UL
		    | Cabs.OCT -> OCT_UL
		end
	    | 2 -> 
		begin
		  match int_base with
		    | Cabs.DEC -> DEC_ULL
		    | Cabs.HEX -> HEX_ULL
		    | Cabs.OCT -> OCT_ULL
		end
	    | _ -> assert false
	end
      else
	begin
	  match !long_cnt with
	    | 0 -> 
		begin
		  match int_base with
		    | Cabs.DEC -> DEC_NONE
		    | Cabs.HEX -> HEX_NONE
		    | Cabs.OCT -> OCT_NONE
		end
	    | 1 -> 
		begin
		  match int_base with
		    | Cabs.DEC -> DEC_L
		    | Cabs.HEX -> HEX_L
		    | Cabs.OCT -> OCT_L
		end
	    | 2 -> 
		begin
		  match int_base with
		    | Cabs.DEC -> DEC_LL
		    | Cabs.HEX -> HEX_LL
		    | Cabs.OCT -> OCT_LL
		end
	    | _ -> assert false
	end
    in
    Constant (!remain_str, suffix)



let c_integer_constant_to_int = function
  | Constant (str, suffix) -> 
      int_of_string str

let c_integer_constant_to_int32 = function
  | Constant (str, suffix) -> 
      Int32.of_string str
	
let c_integer_constant_to_int64 = function
  | Constant (str, suffix) -> 
      Int64.of_string str


let is_constant_zero = function
  | Constant (str, _) ->
      (int_of_string str = 0)
