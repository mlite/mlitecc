m4_dnl
m4_dnl MLite C Compiler -- Ning Wang <email@ningwang.org> 2006-2010
m4_dnl   
m4_dnl The name `Mlite C Compiler' belongs to us, but the code is available free for
m4_dnl any use in any field of endeavor.  You may redistribute Mlite C Compiler in
m4_dnl whole or in part.  We ask that, as a matter of courtesy, you acknowledge its
m4_dnl source and include this LICENSE file.  You may modify Mlite C Compiler and
m4_dnl create derived works, with which you may do as you like, but the result may
m4_dnl not be called Mlite C Compiler without written consent.
   
m4_dnl The software is placed in the public domain.  It is not protected by copyright,
m4_dnl and it is not protected by a ``copyleft'' agreement like the one used by the
m4_dnl Free Software Foundation.

m4_dnl TYPEDEF_2(int, cint)
m4_define(TYPEDEF_2, `typedef $1 $2;')

TYPEDEF_2(_Bool, bint)
TYPEDEF_2(signed char, int8)
TYPEDEF_2(unsigned char, uint8)
TYPEDEF_2(short, int16)
TYPEDEF_2(unsigned short, uint16)
TYPEDEF_2(int, int32)
TYPEDEF_2(unsigned int, uint32)
TYPEDEF_2(long long, int64)
TYPEDEF_2(unsigned long long, uint64)
TYPEDEF_2(float, float32)
TYPEDEF_2(double, float64)
TYPEDEF_2(long double, float80)

m4_define(`foreach',`m4_ifelse(m4_eval($#>2),1,
`m4_pushdef(`$1',`$3')$2`'m4_popdef(`$1')
`'m4_ifelse(m4_eval($#>3),1,`$0(`$1',`$2',m4_shift(m4_shift(m4_shift($@))))')')')


m4_dnl bin_arithmatic (op_name, size, t,  op)
m4_dnl bin_arithmatic ($1,      $2,   $3, $4)
m4_define(bin_op,
`
#define op_$1_$2(v0,v1)              \
({                                   \
   $3 t0 = ($3) v0;                  \
   $3 t1 = ($3) v1;                  \
   t0 $4 t1;                         \
})                                   
')



m4_define(bin_int_op,
`
bin_op(add, $1, $2, +)
bin_op(sub, $1, $2, -)
bin_op(mul, $1, $2, *)
bin_op(div, $1, $2, /)
bin_op(mod, $1, $2, %)
bin_op(band,$1, $2, &)
bin_op(bor, $1, $2, |)
bin_op(bxor,$1, $2, ^)
bin_op(shl, $1, $2, <<)
bin_op(shr, $1, $2, >>)
')

m4_define(bin_uint_op,
`
bin_op(mulu, $1, $2, *)
bin_op(divu, $1, $2, /)
bin_op(modu, $1, $2, %)
bin_op(shru, $1, $2, >>)
')


m4_define(bin_real_op,
`
bin_op(fadd, $1, $2, +)
bin_op(fsub, $1, $2, -)
bin_op(fmul, $1, $2, *)
bin_op(fdiv, $1, $2, /)
')


bin_int_op(bits8, int8)
bin_int_op(bits16, int16)
bin_int_op(bits32, int32)
bin_int_op(bits64, int64)

bin_uint_op(bits8, uint8)
bin_uint_op(bits16, uint16)
bin_uint_op(bits32, uint32)
bin_uint_op(bits64, uint64)

bin_real_op(bits32, float)
bin_real_op(bits64, double)
bin_real_op(bits80, long double)


m4_dnl bin_arithmatic (op_name, size, t,  op)
m4_dnl bin_arithmatic ($1,      $2,   $3, $4)
m4_define(bin_rel,
`
#define op_$1_$2(v0,v1)              \
({                                   \
   $3 t0 = ($3) v0;                  \
   $3 t1 = ($3) v1;                  \
   t0 $4 t1;                         \
})                                   
')

m4_define(bin_irel,
`
bin_rel(eq, $1, $2, ==)
bin_rel(ne, $1, $2, !=)
bin_rel(gt, $1, $2, >)
bin_rel(ge, $1, $2, >=)
bin_rel(lt, $1, $2, <)
bin_rel(le, $1, $2, <=)
')

m4_define(bin_urel,
`
bin_rel(gtu, $1, $2, >)
bin_rel(geu, $1, $2, >=)
bin_rel(ltu, $1, $2, <)
bin_rel(leu, $1, $2, <=)
')

m4_define(bin_frel,
`
bin_rel(feq, $1, $2, ==)
bin_rel(fne, $1, $2, !=)
bin_rel(fgt, $1, $2, >)
bin_rel(fge, $1, $2, >=)
bin_rel(flt, $1, $2, <)
bin_rel(fle, $1, $2, <=)
')



bin_irel(bits32, int32)
bin_irel(bits64, int64)
bin_urel(bits32, uint32)
bin_urel(bits64, uint64)

bin_frel(bits32, float32)
bin_frel(bits64, float64)
bin_frel(bits80, float80)



m4_define(op_convertion, ``#'define op_$1_$2_to_$3(v0) (($4)v0)')
m4_define(signed_extend, ``#'define op_sx_$1_$2(v0) (($3)v0)')
m4_define(zero_extend, ``#'define op_zx_$1_$2(v0) (($3)v0)')
m4_define(lobits, ``#'define op_lobits_$1_$2(v0) (($3)v0)')

m4_dnl b2i
op_convertion(b2i, bits8, bits16, int16)
op_convertion(b2i, bits8, bits32, int32)
op_convertion(b2i, bits8, bits64, int64)

m4_dnl b2u
op_convertion(b2u, bits8, bits16, uint16)
op_convertion(b2u, bits8, bits32, uint32)
op_convertion(b2u, bits8, bits64, uint64)


m4_dnl f2i
op_convertion(f2i, bits32, bits8, int8)
op_convertion(f2i, bits32, bits16,int16)
op_convertion(f2i, bits32, bits32,int32)
op_convertion(f2i, bits32, bits64,int64)

op_convertion(f2i, bits64, bits8, int8)
op_convertion(f2i, bits64, bits16,int16)
op_convertion(f2i, bits64, bits32,int32)
op_convertion(f2i, bits64, bits64,int64)

op_convertion(f2i, bits80, bits8, int8)
op_convertion(f2i, bits80, bits16,int16)
op_convertion(f2i, bits80, bits32,int32)
op_convertion(f2i, bits80, bits64,int64)

m4_dnl f2u
op_convertion(f2u, bits32, bits8, uint8)
op_convertion(f2u, bits32, bits16,uint16)
op_convertion(f2u, bits32, bits32,uint32)
op_convertion(f2u, bits32, bits64,uint64)

op_convertion(f2u, bits64, bits8, uint8)
op_convertion(f2u, bits64, bits16,uint16)
op_convertion(f2u, bits64, bits32,uint32)
op_convertion(f2u, bits64, bits64,uint64)

op_convertion(f2u, bits80, bits8, uint8)
op_convertion(f2u, bits80, bits16,uint16)
op_convertion(f2u, bits80, bits32,uint32)
op_convertion(f2u, bits80, bits64,uint64)


m4_dnl f2f
op_convertion(f2f, bits32, bits64, float64)
op_convertion(f2f, bits32, bits80, float80)
op_convertion(f2f, bits64, bits32, float32)
op_convertion(f2f, bits64, bits80, float80)
op_convertion(f2f, bits80, bits32, float32)
op_convertion(f2f, bits80, bits64, float64)

m4_dnl i2f
op_convertion(i2f, bits8, bits32, float32)
op_convertion(i2f, bits8, bits64, float64)
op_convertion(i2f, bits8, bits80, float80)

op_convertion(i2f, bits16, bits32, float32)
op_convertion(i2f, bits16, bits64, float64)
op_convertion(i2f, bits16, bits80, float80)

op_convertion(i2f, bits32, bits32, float32)
op_convertion(i2f, bits32, bits64, float64)
op_convertion(i2f, bits32, bits80, float80)

op_convertion(i2f, bits64, bits32, float32)
op_convertion(i2f, bits64, bits64, float64)
op_convertion(i2f, bits64, bits80, float80)

m4_dnl u2f
op_convertion(u2f, bits8, bits32, float32)
op_convertion(u2f, bits8, bits64, float64)
op_convertion(u2f, bits8, bits80, float80)

op_convertion(u2f, bits16, bits32, float32)
op_convertion(u2f, bits16, bits64, float64)
op_convertion(u2f, bits16, bits80, float80)

op_convertion(u2f, bits32, bits32, float32)
op_convertion(u2f, bits32, bits64, float64)
op_convertion(u2f, bits32, bits80, float80)

op_convertion(u2f, bits64, bits32, float32)
op_convertion(u2f, bits64, bits64, float64)
op_convertion(u2f, bits64, bits80, float80)


m4_dnl i2i
signed_extend(bits8,  bits16, int16)
signed_extend(bits8,  bits32, int32)
signed_extend(bits8,  bits64, int64)
signed_extend(bits16, bits32, int32)
signed_extend(bits16, bits64, int64)
signed_extend(bits32, bits64, int64)

zero_extend(bits8, bits16, uint16)
zero_extend(bits8, bits32, uint32)
zero_extend(bits8, bits64, uint64)
zero_extend(bits16,bits32, uint32)
zero_extend(bits16,bits64, uint64)
zero_extend(bits32,bits64, uint64)


lobits(bits64, bits8, int8)
lobits(bits64, bits16,int16)
lobits(bits64, bits32,int32)
lobits(bits32, bits8, int8)
lobits(bits32, bits16,int16)
lobits(bits16, bits8, int8)



m4_define(neg_op, ``#'define op_neg_$1(v0)      (-v0)')
neg_op(bits8, int8)
neg_op(bits16, int16)
neg_op(bits32, int32)
neg_op(bits64, int64)

m4_define(fneg_op, ``#'define op_fneg_$1(v0)      (-v0)')
fneg_op(bits32, float32)
fneg_op(bits64, float64)
fneg_op(bits80, float80)


m4_define(bnot_op, ``#'define op_bnot_$1(v0)    (~v0)')
bnot_op(bits8, int8)
bnot_op(bits16, int16)
bnot_op(bits32, int32)
bnot_op(bits64, int64)


