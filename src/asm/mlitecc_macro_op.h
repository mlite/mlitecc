

typedef _Bool bint;
typedef signed char int8;
typedef unsigned char uint8;
typedef short int16;
typedef unsigned short uint16;
typedef int int32;
typedef unsigned int uint32;
typedef long long int64;
typedef unsigned long long uint64;
typedef float float32;
typedef double float64;
typedef long double float80;


















#define op_add_bits8(v0,v1)              \
({                                   \
   int8 t0 = (int8) v0;                  \
   int8 t1 = (int8) v1;                  \
   t0 + t1;                         \
})                                   


#define op_sub_bits8(v0,v1)              \
({                                   \
   int8 t0 = (int8) v0;                  \
   int8 t1 = (int8) v1;                  \
   t0 - t1;                         \
})                                   


#define op_mul_bits8(v0,v1)              \
({                                   \
   int8 t0 = (int8) v0;                  \
   int8 t1 = (int8) v1;                  \
   t0 * t1;                         \
})                                   


#define op_div_bits8(v0,v1)              \
({                                   \
   int8 t0 = (int8) v0;                  \
   int8 t1 = (int8) v1;                  \
   t0 / t1;                         \
})                                   


#define op_mod_bits8(v0,v1)              \
({                                   \
   int8 t0 = (int8) v0;                  \
   int8 t1 = (int8) v1;                  \
   t0 % t1;                         \
})                                   


#define op_band_bits8(v0,v1)              \
({                                   \
   int8 t0 = (int8) v0;                  \
   int8 t1 = (int8) v1;                  \
   t0 & t1;                         \
})                                   


#define op_bor_bits8(v0,v1)              \
({                                   \
   int8 t0 = (int8) v0;                  \
   int8 t1 = (int8) v1;                  \
   t0 | t1;                         \
})                                   


#define op_bxor_bits8(v0,v1)              \
({                                   \
   int8 t0 = (int8) v0;                  \
   int8 t1 = (int8) v1;                  \
   t0 ^ t1;                         \
})                                   


#define op_shl_bits8(v0,v1)              \
({                                   \
   int8 t0 = (int8) v0;                  \
   int8 t1 = (int8) v1;                  \
   t0 << t1;                         \
})                                   


#define op_shr_bits8(v0,v1)              \
({                                   \
   int8 t0 = (int8) v0;                  \
   int8 t1 = (int8) v1;                  \
   t0 >> t1;                         \
})                                   




#define op_add_bits16(v0,v1)              \
({                                   \
   int16 t0 = (int16) v0;                  \
   int16 t1 = (int16) v1;                  \
   t0 + t1;                         \
})                                   


#define op_sub_bits16(v0,v1)              \
({                                   \
   int16 t0 = (int16) v0;                  \
   int16 t1 = (int16) v1;                  \
   t0 - t1;                         \
})                                   


#define op_mul_bits16(v0,v1)              \
({                                   \
   int16 t0 = (int16) v0;                  \
   int16 t1 = (int16) v1;                  \
   t0 * t1;                         \
})                                   


#define op_div_bits16(v0,v1)              \
({                                   \
   int16 t0 = (int16) v0;                  \
   int16 t1 = (int16) v1;                  \
   t0 / t1;                         \
})                                   


#define op_mod_bits16(v0,v1)              \
({                                   \
   int16 t0 = (int16) v0;                  \
   int16 t1 = (int16) v1;                  \
   t0 % t1;                         \
})                                   


#define op_band_bits16(v0,v1)              \
({                                   \
   int16 t0 = (int16) v0;                  \
   int16 t1 = (int16) v1;                  \
   t0 & t1;                         \
})                                   


#define op_bor_bits16(v0,v1)              \
({                                   \
   int16 t0 = (int16) v0;                  \
   int16 t1 = (int16) v1;                  \
   t0 | t1;                         \
})                                   


#define op_bxor_bits16(v0,v1)              \
({                                   \
   int16 t0 = (int16) v0;                  \
   int16 t1 = (int16) v1;                  \
   t0 ^ t1;                         \
})                                   


#define op_shl_bits16(v0,v1)              \
({                                   \
   int16 t0 = (int16) v0;                  \
   int16 t1 = (int16) v1;                  \
   t0 << t1;                         \
})                                   


#define op_shr_bits16(v0,v1)              \
({                                   \
   int16 t0 = (int16) v0;                  \
   int16 t1 = (int16) v1;                  \
   t0 >> t1;                         \
})                                   




#define op_add_bits32(v0,v1)              \
({                                   \
   int32 t0 = (int32) v0;                  \
   int32 t1 = (int32) v1;                  \
   t0 + t1;                         \
})                                   


#define op_sub_bits32(v0,v1)              \
({                                   \
   int32 t0 = (int32) v0;                  \
   int32 t1 = (int32) v1;                  \
   t0 - t1;                         \
})                                   


#define op_mul_bits32(v0,v1)              \
({                                   \
   int32 t0 = (int32) v0;                  \
   int32 t1 = (int32) v1;                  \
   t0 * t1;                         \
})                                   


#define op_div_bits32(v0,v1)              \
({                                   \
   int32 t0 = (int32) v0;                  \
   int32 t1 = (int32) v1;                  \
   t0 / t1;                         \
})                                   


#define op_mod_bits32(v0,v1)              \
({                                   \
   int32 t0 = (int32) v0;                  \
   int32 t1 = (int32) v1;                  \
   t0 % t1;                         \
})                                   


#define op_band_bits32(v0,v1)              \
({                                   \
   int32 t0 = (int32) v0;                  \
   int32 t1 = (int32) v1;                  \
   t0 & t1;                         \
})                                   


#define op_bor_bits32(v0,v1)              \
({                                   \
   int32 t0 = (int32) v0;                  \
   int32 t1 = (int32) v1;                  \
   t0 | t1;                         \
})                                   


#define op_bxor_bits32(v0,v1)              \
({                                   \
   int32 t0 = (int32) v0;                  \
   int32 t1 = (int32) v1;                  \
   t0 ^ t1;                         \
})                                   


#define op_shl_bits32(v0,v1)              \
({                                   \
   int32 t0 = (int32) v0;                  \
   int32 t1 = (int32) v1;                  \
   t0 << t1;                         \
})                                   


#define op_shr_bits32(v0,v1)              \
({                                   \
   int32 t0 = (int32) v0;                  \
   int32 t1 = (int32) v1;                  \
   t0 >> t1;                         \
})                                   




#define op_add_bits64(v0,v1)              \
({                                   \
   int64 t0 = (int64) v0;                  \
   int64 t1 = (int64) v1;                  \
   t0 + t1;                         \
})                                   


#define op_sub_bits64(v0,v1)              \
({                                   \
   int64 t0 = (int64) v0;                  \
   int64 t1 = (int64) v1;                  \
   t0 - t1;                         \
})                                   


#define op_mul_bits64(v0,v1)              \
({                                   \
   int64 t0 = (int64) v0;                  \
   int64 t1 = (int64) v1;                  \
   t0 * t1;                         \
})                                   


#define op_div_bits64(v0,v1)              \
({                                   \
   int64 t0 = (int64) v0;                  \
   int64 t1 = (int64) v1;                  \
   t0 / t1;                         \
})                                   


#define op_mod_bits64(v0,v1)              \
({                                   \
   int64 t0 = (int64) v0;                  \
   int64 t1 = (int64) v1;                  \
   t0 % t1;                         \
})                                   


#define op_band_bits64(v0,v1)              \
({                                   \
   int64 t0 = (int64) v0;                  \
   int64 t1 = (int64) v1;                  \
   t0 & t1;                         \
})                                   


#define op_bor_bits64(v0,v1)              \
({                                   \
   int64 t0 = (int64) v0;                  \
   int64 t1 = (int64) v1;                  \
   t0 | t1;                         \
})                                   


#define op_bxor_bits64(v0,v1)              \
({                                   \
   int64 t0 = (int64) v0;                  \
   int64 t1 = (int64) v1;                  \
   t0 ^ t1;                         \
})                                   


#define op_shl_bits64(v0,v1)              \
({                                   \
   int64 t0 = (int64) v0;                  \
   int64 t1 = (int64) v1;                  \
   t0 << t1;                         \
})                                   


#define op_shr_bits64(v0,v1)              \
({                                   \
   int64 t0 = (int64) v0;                  \
   int64 t1 = (int64) v1;                  \
   t0 >> t1;                         \
})                                   





#define op_mulu_bits8(v0,v1)              \
({                                   \
   uint8 t0 = (uint8) v0;                  \
   uint8 t1 = (uint8) v1;                  \
   t0 * t1;                         \
})                                   


#define op_divu_bits8(v0,v1)              \
({                                   \
   uint8 t0 = (uint8) v0;                  \
   uint8 t1 = (uint8) v1;                  \
   t0 / t1;                         \
})                                   


#define op_modu_bits8(v0,v1)              \
({                                   \
   uint8 t0 = (uint8) v0;                  \
   uint8 t1 = (uint8) v1;                  \
   t0 % t1;                         \
})                                   


#define op_shru_bits8(v0,v1)              \
({                                   \
   uint8 t0 = (uint8) v0;                  \
   uint8 t1 = (uint8) v1;                  \
   t0 >> t1;                         \
})                                   




#define op_mulu_bits16(v0,v1)              \
({                                   \
   uint16 t0 = (uint16) v0;                  \
   uint16 t1 = (uint16) v1;                  \
   t0 * t1;                         \
})                                   


#define op_divu_bits16(v0,v1)              \
({                                   \
   uint16 t0 = (uint16) v0;                  \
   uint16 t1 = (uint16) v1;                  \
   t0 / t1;                         \
})                                   


#define op_modu_bits16(v0,v1)              \
({                                   \
   uint16 t0 = (uint16) v0;                  \
   uint16 t1 = (uint16) v1;                  \
   t0 % t1;                         \
})                                   


#define op_shru_bits16(v0,v1)              \
({                                   \
   uint16 t0 = (uint16) v0;                  \
   uint16 t1 = (uint16) v1;                  \
   t0 >> t1;                         \
})                                   




#define op_mulu_bits32(v0,v1)              \
({                                   \
   uint32 t0 = (uint32) v0;                  \
   uint32 t1 = (uint32) v1;                  \
   t0 * t1;                         \
})                                   


#define op_divu_bits32(v0,v1)              \
({                                   \
   uint32 t0 = (uint32) v0;                  \
   uint32 t1 = (uint32) v1;                  \
   t0 / t1;                         \
})                                   


#define op_modu_bits32(v0,v1)              \
({                                   \
   uint32 t0 = (uint32) v0;                  \
   uint32 t1 = (uint32) v1;                  \
   t0 % t1;                         \
})                                   


#define op_shru_bits32(v0,v1)              \
({                                   \
   uint32 t0 = (uint32) v0;                  \
   uint32 t1 = (uint32) v1;                  \
   t0 >> t1;                         \
})                                   




#define op_mulu_bits64(v0,v1)              \
({                                   \
   uint64 t0 = (uint64) v0;                  \
   uint64 t1 = (uint64) v1;                  \
   t0 * t1;                         \
})                                   


#define op_divu_bits64(v0,v1)              \
({                                   \
   uint64 t0 = (uint64) v0;                  \
   uint64 t1 = (uint64) v1;                  \
   t0 / t1;                         \
})                                   


#define op_modu_bits64(v0,v1)              \
({                                   \
   uint64 t0 = (uint64) v0;                  \
   uint64 t1 = (uint64) v1;                  \
   t0 % t1;                         \
})                                   


#define op_shru_bits64(v0,v1)              \
({                                   \
   uint64 t0 = (uint64) v0;                  \
   uint64 t1 = (uint64) v1;                  \
   t0 >> t1;                         \
})                                   





#define op_fadd_bits32(v0,v1)              \
({                                   \
   float t0 = (float) v0;                  \
   float t1 = (float) v1;                  \
   t0 + t1;                         \
})                                   


#define op_fsub_bits32(v0,v1)              \
({                                   \
   float t0 = (float) v0;                  \
   float t1 = (float) v1;                  \
   t0 - t1;                         \
})                                   


#define op_fmul_bits32(v0,v1)              \
({                                   \
   float t0 = (float) v0;                  \
   float t1 = (float) v1;                  \
   t0 * t1;                         \
})                                   


#define op_fdiv_bits32(v0,v1)              \
({                                   \
   float t0 = (float) v0;                  \
   float t1 = (float) v1;                  \
   t0 / t1;                         \
})                                   




#define op_fadd_bits64(v0,v1)              \
({                                   \
   double t0 = (double) v0;                  \
   double t1 = (double) v1;                  \
   t0 + t1;                         \
})                                   


#define op_fsub_bits64(v0,v1)              \
({                                   \
   double t0 = (double) v0;                  \
   double t1 = (double) v1;                  \
   t0 - t1;                         \
})                                   


#define op_fmul_bits64(v0,v1)              \
({                                   \
   double t0 = (double) v0;                  \
   double t1 = (double) v1;                  \
   t0 * t1;                         \
})                                   


#define op_fdiv_bits64(v0,v1)              \
({                                   \
   double t0 = (double) v0;                  \
   double t1 = (double) v1;                  \
   t0 / t1;                         \
})                                   




#define op_fadd_bits80(v0,v1)              \
({                                   \
   long double t0 = (long double) v0;                  \
   long double t1 = (long double) v1;                  \
   t0 + t1;                         \
})                                   


#define op_fsub_bits80(v0,v1)              \
({                                   \
   long double t0 = (long double) v0;                  \
   long double t1 = (long double) v1;                  \
   t0 - t1;                         \
})                                   


#define op_fmul_bits80(v0,v1)              \
({                                   \
   long double t0 = (long double) v0;                  \
   long double t1 = (long double) v1;                  \
   t0 * t1;                         \
})                                   


#define op_fdiv_bits80(v0,v1)              \
({                                   \
   long double t0 = (long double) v0;                  \
   long double t1 = (long double) v1;                  \
   t0 / t1;                         \
})                                   
















#define op_eq_bits32(v0,v1)              \
({                                   \
   int32 t0 = (int32) v0;                  \
   int32 t1 = (int32) v1;                  \
   t0 == t1;                         \
})                                   


#define op_ne_bits32(v0,v1)              \
({                                   \
   int32 t0 = (int32) v0;                  \
   int32 t1 = (int32) v1;                  \
   t0 != t1;                         \
})                                   


#define op_gt_bits32(v0,v1)              \
({                                   \
   int32 t0 = (int32) v0;                  \
   int32 t1 = (int32) v1;                  \
   t0 > t1;                         \
})                                   


#define op_ge_bits32(v0,v1)              \
({                                   \
   int32 t0 = (int32) v0;                  \
   int32 t1 = (int32) v1;                  \
   t0 >= t1;                         \
})                                   


#define op_lt_bits32(v0,v1)              \
({                                   \
   int32 t0 = (int32) v0;                  \
   int32 t1 = (int32) v1;                  \
   t0 < t1;                         \
})                                   


#define op_le_bits32(v0,v1)              \
({                                   \
   int32 t0 = (int32) v0;                  \
   int32 t1 = (int32) v1;                  \
   t0 <= t1;                         \
})                                   




#define op_eq_bits64(v0,v1)              \
({                                   \
   int64 t0 = (int64) v0;                  \
   int64 t1 = (int64) v1;                  \
   t0 == t1;                         \
})                                   


#define op_ne_bits64(v0,v1)              \
({                                   \
   int64 t0 = (int64) v0;                  \
   int64 t1 = (int64) v1;                  \
   t0 != t1;                         \
})                                   


#define op_gt_bits64(v0,v1)              \
({                                   \
   int64 t0 = (int64) v0;                  \
   int64 t1 = (int64) v1;                  \
   t0 > t1;                         \
})                                   


#define op_ge_bits64(v0,v1)              \
({                                   \
   int64 t0 = (int64) v0;                  \
   int64 t1 = (int64) v1;                  \
   t0 >= t1;                         \
})                                   


#define op_lt_bits64(v0,v1)              \
({                                   \
   int64 t0 = (int64) v0;                  \
   int64 t1 = (int64) v1;                  \
   t0 < t1;                         \
})                                   


#define op_le_bits64(v0,v1)              \
({                                   \
   int64 t0 = (int64) v0;                  \
   int64 t1 = (int64) v1;                  \
   t0 <= t1;                         \
})                                   




#define op_gtu_bits32(v0,v1)              \
({                                   \
   uint32 t0 = (uint32) v0;                  \
   uint32 t1 = (uint32) v1;                  \
   t0 > t1;                         \
})                                   


#define op_geu_bits32(v0,v1)              \
({                                   \
   uint32 t0 = (uint32) v0;                  \
   uint32 t1 = (uint32) v1;                  \
   t0 >= t1;                         \
})                                   


#define op_ltu_bits32(v0,v1)              \
({                                   \
   uint32 t0 = (uint32) v0;                  \
   uint32 t1 = (uint32) v1;                  \
   t0 < t1;                         \
})                                   


#define op_leu_bits32(v0,v1)              \
({                                   \
   uint32 t0 = (uint32) v0;                  \
   uint32 t1 = (uint32) v1;                  \
   t0 <= t1;                         \
})                                   




#define op_gtu_bits64(v0,v1)              \
({                                   \
   uint64 t0 = (uint64) v0;                  \
   uint64 t1 = (uint64) v1;                  \
   t0 > t1;                         \
})                                   


#define op_geu_bits64(v0,v1)              \
({                                   \
   uint64 t0 = (uint64) v0;                  \
   uint64 t1 = (uint64) v1;                  \
   t0 >= t1;                         \
})                                   


#define op_ltu_bits64(v0,v1)              \
({                                   \
   uint64 t0 = (uint64) v0;                  \
   uint64 t1 = (uint64) v1;                  \
   t0 < t1;                         \
})                                   


#define op_leu_bits64(v0,v1)              \
({                                   \
   uint64 t0 = (uint64) v0;                  \
   uint64 t1 = (uint64) v1;                  \
   t0 <= t1;                         \
})                                   





#define op_feq_bits32(v0,v1)              \
({                                   \
   float32 t0 = (float32) v0;                  \
   float32 t1 = (float32) v1;                  \
   t0 == t1;                         \
})                                   


#define op_fne_bits32(v0,v1)              \
({                                   \
   float32 t0 = (float32) v0;                  \
   float32 t1 = (float32) v1;                  \
   t0 != t1;                         \
})                                   


#define op_fgt_bits32(v0,v1)              \
({                                   \
   float32 t0 = (float32) v0;                  \
   float32 t1 = (float32) v1;                  \
   t0 > t1;                         \
})                                   


#define op_fge_bits32(v0,v1)              \
({                                   \
   float32 t0 = (float32) v0;                  \
   float32 t1 = (float32) v1;                  \
   t0 >= t1;                         \
})                                   


#define op_flt_bits32(v0,v1)              \
({                                   \
   float32 t0 = (float32) v0;                  \
   float32 t1 = (float32) v1;                  \
   t0 < t1;                         \
})                                   


#define op_fle_bits32(v0,v1)              \
({                                   \
   float32 t0 = (float32) v0;                  \
   float32 t1 = (float32) v1;                  \
   t0 <= t1;                         \
})                                   




#define op_feq_bits64(v0,v1)              \
({                                   \
   float64 t0 = (float64) v0;                  \
   float64 t1 = (float64) v1;                  \
   t0 == t1;                         \
})                                   


#define op_fne_bits64(v0,v1)              \
({                                   \
   float64 t0 = (float64) v0;                  \
   float64 t1 = (float64) v1;                  \
   t0 != t1;                         \
})                                   


#define op_fgt_bits64(v0,v1)              \
({                                   \
   float64 t0 = (float64) v0;                  \
   float64 t1 = (float64) v1;                  \
   t0 > t1;                         \
})                                   


#define op_fge_bits64(v0,v1)              \
({                                   \
   float64 t0 = (float64) v0;                  \
   float64 t1 = (float64) v1;                  \
   t0 >= t1;                         \
})                                   


#define op_flt_bits64(v0,v1)              \
({                                   \
   float64 t0 = (float64) v0;                  \
   float64 t1 = (float64) v1;                  \
   t0 < t1;                         \
})                                   


#define op_fle_bits64(v0,v1)              \
({                                   \
   float64 t0 = (float64) v0;                  \
   float64 t1 = (float64) v1;                  \
   t0 <= t1;                         \
})                                   




#define op_feq_bits80(v0,v1)              \
({                                   \
   float80 t0 = (float80) v0;                  \
   float80 t1 = (float80) v1;                  \
   t0 == t1;                         \
})                                   


#define op_fne_bits80(v0,v1)              \
({                                   \
   float80 t0 = (float80) v0;                  \
   float80 t1 = (float80) v1;                  \
   t0 != t1;                         \
})                                   


#define op_fgt_bits80(v0,v1)              \
({                                   \
   float80 t0 = (float80) v0;                  \
   float80 t1 = (float80) v1;                  \
   t0 > t1;                         \
})                                   


#define op_fge_bits80(v0,v1)              \
({                                   \
   float80 t0 = (float80) v0;                  \
   float80 t1 = (float80) v1;                  \
   t0 >= t1;                         \
})                                   


#define op_flt_bits80(v0,v1)              \
({                                   \
   float80 t0 = (float80) v0;                  \
   float80 t1 = (float80) v1;                  \
   t0 < t1;                         \
})                                   


#define op_fle_bits80(v0,v1)              \
({                                   \
   float80 t0 = (float80) v0;                  \
   float80 t1 = (float80) v1;                  \
   t0 <= t1;                         \
})                                   










#define op_b2i_bits8_to_bits16(v0) ((int16)v0)
#define op_b2i_bits8_to_bits32(v0) ((int32)v0)
#define op_b2i_bits8_to_bits64(v0) ((int64)v0)

#define op_b2u_bits8_to_bits16(v0) ((uint16)v0)
#define op_b2u_bits8_to_bits32(v0) ((uint32)v0)
#define op_b2u_bits8_to_bits64(v0) ((uint64)v0)


#define op_f2i_bits32_to_bits8(v0) ((int8)v0)
#define op_f2i_bits32_to_bits16(v0) ((int16)v0)
#define op_f2i_bits32_to_bits32(v0) ((int32)v0)
#define op_f2i_bits32_to_bits64(v0) ((int64)v0)

#define op_f2i_bits64_to_bits8(v0) ((int8)v0)
#define op_f2i_bits64_to_bits16(v0) ((int16)v0)
#define op_f2i_bits64_to_bits32(v0) ((int32)v0)
#define op_f2i_bits64_to_bits64(v0) ((int64)v0)

#define op_f2i_bits80_to_bits8(v0) ((int8)v0)
#define op_f2i_bits80_to_bits16(v0) ((int16)v0)
#define op_f2i_bits80_to_bits32(v0) ((int32)v0)
#define op_f2i_bits80_to_bits64(v0) ((int64)v0)

#define op_f2u_bits32_to_bits8(v0) ((uint8)v0)
#define op_f2u_bits32_to_bits16(v0) ((uint16)v0)
#define op_f2u_bits32_to_bits32(v0) ((uint32)v0)
#define op_f2u_bits32_to_bits64(v0) ((uint64)v0)

#define op_f2u_bits64_to_bits8(v0) ((uint8)v0)
#define op_f2u_bits64_to_bits16(v0) ((uint16)v0)
#define op_f2u_bits64_to_bits32(v0) ((uint32)v0)
#define op_f2u_bits64_to_bits64(v0) ((uint64)v0)

#define op_f2u_bits80_to_bits8(v0) ((uint8)v0)
#define op_f2u_bits80_to_bits16(v0) ((uint16)v0)
#define op_f2u_bits80_to_bits32(v0) ((uint32)v0)
#define op_f2u_bits80_to_bits64(v0) ((uint64)v0)


#define op_f2f_bits32_to_bits64(v0) ((float64)v0)
#define op_f2f_bits32_to_bits80(v0) ((float80)v0)
#define op_f2f_bits64_to_bits32(v0) ((float32)v0)
#define op_f2f_bits64_to_bits80(v0) ((float80)v0)
#define op_f2f_bits80_to_bits32(v0) ((float32)v0)
#define op_f2f_bits80_to_bits64(v0) ((float64)v0)

#define op_i2f_bits8_to_bits32(v0) ((float32)v0)
#define op_i2f_bits8_to_bits64(v0) ((float64)v0)
#define op_i2f_bits8_to_bits80(v0) ((float80)v0)

#define op_i2f_bits16_to_bits32(v0) ((float32)v0)
#define op_i2f_bits16_to_bits64(v0) ((float64)v0)
#define op_i2f_bits16_to_bits80(v0) ((float80)v0)

#define op_i2f_bits32_to_bits32(v0) ((float32)v0)
#define op_i2f_bits32_to_bits64(v0) ((float64)v0)
#define op_i2f_bits32_to_bits80(v0) ((float80)v0)

#define op_i2f_bits64_to_bits32(v0) ((float32)v0)
#define op_i2f_bits64_to_bits64(v0) ((float64)v0)
#define op_i2f_bits64_to_bits80(v0) ((float80)v0)

#define op_u2f_bits8_to_bits32(v0) ((float32)v0)
#define op_u2f_bits8_to_bits64(v0) ((float64)v0)
#define op_u2f_bits8_to_bits80(v0) ((float80)v0)

#define op_u2f_bits16_to_bits32(v0) ((float32)v0)
#define op_u2f_bits16_to_bits64(v0) ((float64)v0)
#define op_u2f_bits16_to_bits80(v0) ((float80)v0)

#define op_u2f_bits32_to_bits32(v0) ((float32)v0)
#define op_u2f_bits32_to_bits64(v0) ((float64)v0)
#define op_u2f_bits32_to_bits80(v0) ((float80)v0)

#define op_u2f_bits64_to_bits32(v0) ((float32)v0)
#define op_u2f_bits64_to_bits64(v0) ((float64)v0)
#define op_u2f_bits64_to_bits80(v0) ((float80)v0)


#define op_sx_bits8_bits16(v0) ((int16)v0)
#define op_sx_bits8_bits32(v0) ((int32)v0)
#define op_sx_bits8_bits64(v0) ((int64)v0)
#define op_sx_bits16_bits32(v0) ((int32)v0)
#define op_sx_bits16_bits64(v0) ((int64)v0)
#define op_sx_bits32_bits64(v0) ((int64)v0)

#define op_zx_bits8_bits16(v0) ((uint16)v0)
#define op_zx_bits8_bits32(v0) ((uint32)v0)
#define op_zx_bits8_bits64(v0) ((uint64)v0)
#define op_zx_bits16_bits32(v0) ((uint32)v0)
#define op_zx_bits16_bits64(v0) ((uint64)v0)
#define op_zx_bits32_bits64(v0) ((uint64)v0)


#define op_lobits_bits64_bits8(v0) ((int8)v0)
#define op_lobits_bits64_bits16(v0) ((int16)v0)
#define op_lobits_bits64_bits32(v0) ((int32)v0)
#define op_lobits_bits32_bits8(v0) ((int8)v0)
#define op_lobits_bits32_bits16(v0) ((int16)v0)
#define op_lobits_bits16_bits8(v0) ((int8)v0)




#define op_neg_bits8(v0)      (-v0)
#define op_neg_bits16(v0)      (-v0)
#define op_neg_bits32(v0)      (-v0)
#define op_neg_bits64(v0)      (-v0)


#define op_fneg_bits32(v0)      (-v0)
#define op_fneg_bits64(v0)      (-v0)
#define op_fneg_bits80(v0)      (-v0)



#define op_bnot_bits8(v0)    (~v0)
#define op_bnot_bits16(v0)    (~v0)
#define op_bnot_bits32(v0)    (~v0)
#define op_bnot_bits64(v0)    (~v0)


