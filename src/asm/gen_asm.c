/* 
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
*/

#ifndef GEN_ASM_H
#define GEN_ASM_H 1

extern void abort (void);
typedef char int8_t;
typedef short int16_t;
typedef int int32_t;
typedef long long int64_t;

typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
typedef unsigned long long uint64_t;

typedef float float32;
typedef double float64;
typedef long double float80;

typedef float float32_t;
typedef double float64_t;
typedef long double float80_t;
typedef int bool_t;


static int8_t int8_sx [][2] = 
  {
    {0x01, 0xff},
    {0x02, 0xfe},
    {0x04, 0xfc},
    {0x08, 0xf8},
    {0x10, 0xf0},
    {0x20, 0xe0},
    {0x40, 0xc0},
    {0x80, 0x80},
  };

static int16_t int16_sx [][2] = 
  {
    /* low 8 */
    {0x0001, 0xffff},
    {0x0002, 0xfffe},
    {0x0004, 0xfffc},
    {0x0008, 0xfff8},
    {0x0010, 0xfff0},
    {0x0020, 0xffe0},
    {0x0040, 0xffc0},
    {0x0080, 0xff80},
    
    /* high 8 */
    {0x0100, 0xff00},
    {0x0200, 0xfe00},
    {0x0400, 0xfc00},
    {0x0800, 0xf800},
    {0x1000, 0xf000},
    {0x2000, 0xe000},
    {0x4000, 0xc000},
    {0x8000, 0x8000},
  };

static int32_t int32_sx [][2] = 
  {
    /* low 16 */
    {0x00000001, 0xffffffff},
    {0x00000002, 0xfffffffe},
    {0x00000004, 0xfffffffc},
    {0x00000008, 0xfffffff8},
    {0x00000010, 0xfffffff0},
    {0x00000020, 0xffffffe0},
    {0x00000040, 0xffffffc0},
    {0x00000080, 0xffffff80},
    {0x00000100, 0xffffff00},
    {0x00000200, 0xfffffe00},
    {0x00000400, 0xfffffc00},
    {0x00000800, 0xfffff800},
    {0x00001000, 0xfffff000},
    {0x00002000, 0xffffe000},
    {0x00004000, 0xffffc000},
    {0x00008000, 0xffff8000},

    /* high 16 */
    {0x00010000, 0xffff0000},
    {0x00020000, 0xfffe0000},
    {0x00040000, 0xfffc0000},
    {0x00080000, 0xfff80000},
    {0x00100000, 0xfff00000},
    {0x00200000, 0xffe00000},
    {0x00400000, 0xffc00000},
    {0x00800000, 0xff800000},
    {0x01000000, 0xff000000},
    {0x02000000, 0xfe000000},
    {0x04000000, 0xfc000000},
    {0x08000000, 0xf8000000},
    {0x10000000, 0xf0000000},
    {0x20000000, 0xe0000000},
    {0x40000000, 0xc0000000},
    {0x80000000, 0x80000000},
  };


static int64_t int64_sx [][2] = 
  {
    /* low 32 */
    {0x0000000000000001ULL, 0xffffffffffffffffULL},
    {0x0000000000000002ULL, 0xfffffffffffffffeULL},
    {0x0000000000000004ULL, 0xfffffffffffffffcULL},
    {0x0000000000000008ULL, 0xfffffffffffffff8ULL},
    {0x0000000000000010ULL, 0xfffffffffffffff0ULL},
    {0x0000000000000020ULL, 0xffffffffffffffe0ULL},
    {0x0000000000000040ULL, 0xffffffffffffffc0ULL},
    {0x0000000000000080ULL, 0xffffffffffffff80ULL},
    {0x0000000000000100ULL, 0xffffffffffffff00ULL},
    {0x0000000000000200ULL, 0xfffffffffffffe00ULL},
    {0x0000000000000400ULL, 0xfffffffffffffc00ULL},
    {0x0000000000000800ULL, 0xfffffffffffff800ULL},
    {0x0000000000001000ULL, 0xfffffffffffff000ULL},
    {0x0000000000002000ULL, 0xffffffffffffe000ULL},
    {0x0000000000004000ULL, 0xffffffffffffc000ULL},
    {0x0000000000008000ULL, 0xffffffffffff8000ULL},
    {0x0000000000010000ULL, 0xffffffffffff0000ULL},
    {0x0000000000020000ULL, 0xfffffffffffe0000ULL},
    {0x0000000000040000ULL, 0xfffffffffffc0000ULL},
    {0x0000000000080000ULL, 0xfffffffffff80000ULL},
    {0x0000000000100000ULL, 0xfffffffffff00000ULL},
    {0x0000000000200000ULL, 0xffffffffffe00000ULL},
    {0x0000000000400000ULL, 0xffffffffffc00000ULL},
    {0x0000000000800000ULL, 0xffffffffff800000ULL},
    {0x0000000001000000ULL, 0xffffffffff000000ULL},
    {0x0000000002000000ULL, 0xfffffffffe000000ULL},
    {0x0000000004000000ULL, 0xfffffffffc000000ULL},
    {0x0000000008000000ULL, 0xfffffffff8000000ULL},
    {0x0000000010000000ULL, 0xfffffffff0000000ULL},
    {0x0000000020000000ULL, 0xffffffffe0000000ULL},
    {0x0000000040000000ULL, 0xffffffffc0000000ULL},
    {0x0000000080000000ULL, 0xffffffff80000000ULL},
    
    /* high 32 */
    {0x0000000100000000ULL, 0xffffffff00000000ULL},
    {0x0000000200000000ULL, 0xfffffffe00000000ULL},
    {0x0000000400000000ULL, 0xfffffffc00000000ULL},
    {0x0000000800000000ULL, 0xfffffff800000000ULL},
    {0x0000001000000000ULL, 0xfffffff000000000ULL},
    {0x0000002000000000ULL, 0xffffffe000000000ULL},
    {0x0000004000000000ULL, 0xffffffc000000000ULL},
    {0x0000008000000000ULL, 0xffffff8000000000ULL},
    {0x0000010000000000ULL, 0xffffff0000000000ULL},
    {0x0000020000000000ULL, 0xfffffe0000000000ULL},
    {0x0000040000000000ULL, 0xfffffc0000000000ULL},
    {0x0000080000000000ULL, 0xfffff80000000000ULL},
    {0x0000100000000000ULL, 0xfffff00000000000ULL},
    {0x0000200000000000ULL, 0xffffe00000000000ULL},
    {0x0000400000000000ULL, 0xffffc00000000000ULL},
    {0x0000800000000000ULL, 0xffff800000000000ULL},
    {0x0001000000000000ULL, 0xffff000000000000ULL},
    {0x0002000000000000ULL, 0xfffe000000000000ULL},
    {0x0004000000000000ULL, 0xfffc000000000000ULL},
    {0x0008000000000000ULL, 0xfff8000000000000ULL},
    {0x0010000000000000ULL, 0xfff0000000000000ULL},
    {0x0020000000000000ULL, 0xffe0000000000000ULL},
    {0x0040000000000000ULL, 0xffc0000000000000ULL},
    {0x0080000000000000ULL, 0xff80000000000000ULL},
    {0x0100000000000000ULL, 0xff00000000000000ULL},
    {0x0200000000000000ULL, 0xfe00000000000000ULL},
    {0x0400000000000000ULL, 0xfc00000000000000ULL},
    {0x0800000000000000ULL, 0xf800000000000000ULL},
    {0x1000000000000000ULL, 0xf000000000000000ULL},
    {0x2000000000000000ULL, 0xe000000000000000ULL},
    {0x4000000000000000ULL, 0xc000000000000000ULL},
    {0x8000000000000000ULL, 0x8000000000000000ULL},
  };

#define BINARYOP(NAME, EXP, T1, T2, RT) \
\
inline RT##_t op_##NAME##_##T1##_##T2##_##RT (T1##_t v1, T1##_t v2) {\
  return (EXP); \
}

#define UNARYOP(NAME, EXP, T1, RT)  \
\
inline RT##_t op_##NAME##_##T1##_##RT (T1##_t v1) { \
  return (EXP);                          \
}




#define UNARYOP(NAME, EXP, T1, RT)  \
\
inline RT##_t op_##NAME##_##T1##_##RT (T1##_t v1) { \
  return (EXP);                          \
}


/* float to int */
UNARYOP (f2i, (int8_t)v1, float32, int8)
UNARYOP (f2i, (int16_t)v1, float32, int16)
UNARYOP (f2i, (int32_t)v1, float32, int32)
UNARYOP (f2i, (uint8_t)v1, float32, uint8)
UNARYOP (f2i, (uint16_t)v1, float32, uint16)
UNARYOP (f2i, (uint32_t)v1, float32, uint32)

UNARYOP (f2i, (int8_t)v1, float64, int8)
UNARYOP (f2i, (int16_t)v1, float64, int16)
UNARYOP (f2i, (int32_t)v1, float64, int32)
UNARYOP (f2i, (uint8_t)v1, float64, uint8)
UNARYOP (f2i, (uint16_t)v1, float64, uint16)
UNARYOP (f2i, (uint32_t)v1, float64, uint32)

UNARYOP (f2i, (int8_t)v1, float80, int8)
UNARYOP (f2i, (int16_t)v1, float80, int16)
UNARYOP (f2i, (int32_t)v1, float80, int32)
UNARYOP (f2i, (uint8_t)v1, float80, uint8)
UNARYOP (f2i, (uint16_t)v1, float80, uint16)
UNARYOP (f2i, (uint32_t)v1, float80, uint32)

UNARYOP (f2f, (float64_t)v1, float32, float64)
UNARYOP (f2f, (float80_t)v1, float32, float80)
UNARYOP (f2f, (float32_t)v1, float64, float32)
UNARYOP (f2f, (float80_t)v1, float64, float80)
UNARYOP (f2f, (float32_t)v1, float80, float32)
UNARYOP (f2f, (float64_t)v1, float80, float64)


/* float arithmatics */
BINARYOP (fadd, v1 + v2, float32, float32, float32)
BINARYOP (fsub, v1 - v2, float32, float32, float32)
BINARYOP (fmul, v1 * v2, float32, float32, float32)
BINARYOP (fdiv, v1 / v2, float32, float32, float32)
UNARYOP (fneg, (-v1), float32, float32)
BINARYOP (feq, v1 == v2, float32, float32, bool)
BINARYOP (fge, v1 >= v2, float32, float32, bool)
BINARYOP (fgt, v1 >  v2, float32, float32, bool)
BINARYOP (fle, v1 <= v2, float32, float32, bool)
BINARYOP (flt, v1 <  v2, float32, float32, bool)
BINARYOP (fne, v1 != v2, float32, float32, bool)


BINARYOP (fadd, v1 + v2, float64, float64, float64)
BINARYOP (fsub, v1 - v2, float64, float64, float64)
BINARYOP (fmul, v1 * v2, float64, float64, float64)
BINARYOP (fdiv, v1 / v2, float64, float64, float64)
UNARYOP (fneg, (-v1), float64, float64)
BINARYOP (feq, v1 == v2, float64, float64, bool)
BINARYOP (fge, v1 >= v2, float64, float64, bool)
BINARYOP (fgt, v1 >  v2, float64, float64, bool)
BINARYOP (fle, v1 <= v2, float64, float64, bool)
BINARYOP (flt, v1 <  v2, float64, float64, bool)
BINARYOP (fne, v1 != v2, float64, float64, bool)


BINARYOP (fadd, v1 + v2, float80, float80, float80)
BINARYOP (fsub, v1 - v2, float80, float80, float80)
BINARYOP (fmul, v1 * v2, float80, float80, float80)
BINARYOP (fdiv, v1 / v2, float80, float80, float80)
UNARYOP (fneg, (-v1), float80, float80)
BINARYOP (feq, v1 == v2, float80, float80, bool)
BINARYOP (fge, v1 >= v2, float80, float80, bool)
BINARYOP (fgt, v1 >  v2, float80, float80, bool)
BINARYOP (fle, v1 <= v2, float80, float80, bool)
BINARYOP (flt, v1 <  v2, float80, float80, bool)
BINARYOP (fne, v1 != v2, float80, float80, bool)



/* int8_t arithmatics */
BINARYOP (add, v1 + v2, int8, int8, int8)
BINARYOP (sub, v1 - v2, int8, int8, int8)
BINARYOP (mul, v1 * v2, int8, int8, int8)
BINARYOP (mulu, (uint8_t)v1 * (uint8_t)v2, int8, int8, int8)
BINARYOP (div, v1 / v2, int8, int8, int8)
BINARYOP (divu, (uint8_t)v1 / (uint8_t)v2, int8, int8, uint8)
BINARYOP (mod, v1 % v2, int8, int8, int8)
BINARYOP (modu, (uint8_t)v1 % (uint8_t)v2, int8, int8, uint8)
BINARYOP (band, v1 & v2, int8, int8, int8)
BINARYOP (bor, v1 | v2, int8, int8, int8)
BINARYOP (bxor, v1 ^ v2, int8, int8, int8)
BINARYOP (shr, v1 >> v2, int8, int8, int8)
BINARYOP (shl, v1 << v2, int8, int8, int8)
UNARYOP (neg, (-v1), int8, int8)
UNARYOP (bcom, (~v1), int8, int8)
BINARYOP (eq, v1 =  v2, int8, int8, bool)
BINARYOP (ne, v1 != v2, int8, int8, bool)
BINARYOP (ge, v1 >= v2, int8, int8, bool)
BINARYOP (gt, v1 >  v2, int8, int8, bool)
BINARYOP (le, v1 <= v2, int8, int8, bool)
BINARYOP (lt, v1 <  v2, int8, int8, bool)
BINARYOP (geu, (uint8_t)v1 >= (uint8_t)v2, int8, int8, bool)
BINARYOP (gtu, (uint8_t)v1 >  (uint8_t)v2, int8, int8, bool)
BINARYOP (leu, (uint8_t)v1 <= (uint8_t)v2, int8, int8, bool)
BINARYOP (ltu, (uint8_t)v1 <  (uint8_t)v2, int8, int8, bool)

/* 16 bits */
BINARYOP (add, v1 + v2, int16, int16, int16)
BINARYOP (sub, v1 - v2, int16, int16, int16)
BINARYOP (mul, v1 * v2, int16, int16, int16)
BINARYOP (mulu, (uint16_t)v1 * (uint16_t)v2, int16, int16, int16)
BINARYOP (div, v1 / v2, int16, int16, int16)
BINARYOP (divu, (uint16_t)v1 / (uint16_t)v2, int16, int16, uint16)
BINARYOP (mod, v1 % v2, int16, int16, int16)
BINARYOP (modu, (uint16_t)v1 % (uint16_t)v2, int16, int16, uint16)
BINARYOP (band, v1 & v2, int16, int16, int16)
BINARYOP (bor, v1 | v2, int16, int16, int16)
BINARYOP (bxor, v1 ^ v2, int16, int16, int16)
BINARYOP (shr, v1 >> v2, int16, int16, int16)
BINARYOP (shl, v1 << v2, int16, int16, int16)
UNARYOP (neg, (-v1), int16, int16)
UNARYOP (bcom, (~v1), int16, int16)
BINARYOP (eq, v1 =  v2, int16, int16, bool)
BINARYOP (ne, v1 != v2, int16, int16, bool)
BINARYOP (ge, v1 >= v2, int16, int16, bool)
BINARYOP (gt, v1 >  v2, int16, int16, bool)
BINARYOP (le, v1 <= v2, int16, int16, bool)
BINARYOP (lt, v1 <  v2, int16, int16, bool)
BINARYOP (geu, (uint16_t)v1 >= (uint16_t)v2, int16, int16, bool)
BINARYOP (gtu, (uint16_t)v1 >  (uint16_t)v2, int16, int16, bool)
BINARYOP (leu, (uint16_t)v1 <= (uint16_t)v2, int16, int16, bool)
BINARYOP (ltu, (uint16_t)v1 <  (uint16_t)v2, int16, int16, bool)


/* 16 bits */
BINARYOP (add, v1 + v2, int32, int32, int32)
BINARYOP (sub, v1 - v2, int32, int32, int32)
BINARYOP (mul, v1 * v2, int32, int32, int32)
BINARYOP (mulu, (uint32_t)v1 * (uint32_t)v2, int32, int32, int32)
BINARYOP (div, v1 / v2, int32, int32, int32)
BINARYOP (divu, (uint32_t)v1 / (uint32_t)v2, int32, int32, uint32)
BINARYOP (mod, v1 % v2, int32, int32, int32)
BINARYOP (modu, (uint32_t)v1 % (uint32_t)v2, int32, int32, uint32)
BINARYOP (band, v1 & v2, int32, int32, int32)
BINARYOP (bor, v1 | v2, int32, int32, int32)
BINARYOP (bxor, v1 ^ v2, int32, int32, int32)
BINARYOP (shr, v1 >> v2, int32, int32, int32)
BINARYOP (shl, v1 << v2, int32, int32, int32)
UNARYOP (neg, (-v1), int32, int32)
UNARYOP (bcom, (~v1), int32, int32)
BINARYOP (eq, v1 =  v2, int32, int32, bool)
BINARYOP (ne, v1 != v2, int32, int32, bool)
BINARYOP (ge, v1 >= v2, int32, int32, bool)
BINARYOP (gt, v1 >  v2, int32, int32, bool)
BINARYOP (le, v1 <= v2, int32, int32, bool)
BINARYOP (lt, v1 <  v2, int32, int32, bool)
BINARYOP (geu, (uint32_t)v1 >= (uint32_t)v2, int32, int32, bool)
BINARYOP (gtu, (uint32_t)v1 >  (uint32_t)v2, int32, int32, bool)
BINARYOP (leu, (uint32_t)v1 <= (uint32_t)v2, int32, int32, bool)
BINARYOP (ltu, (uint32_t)v1 <  (uint32_t)v2, int32, int32, bool)


/* 16 bits */
BINARYOP (add, v1 + v2, int64, int64, int64)
BINARYOP (sub, v1 - v2, int64, int64, int64)
BINARYOP (mul, v1 * v2, int64, int64, int64)
BINARYOP (mulu, (uint64_t)v1 * (uint64_t)v2, int64, int64, int64)
BINARYOP (div, v1 / v2, int64, int64, int64)
BINARYOP (divu, (uint64_t)v1 / (uint64_t)v2, int64, int64, uint64)
BINARYOP (mod, v1 % v2, int64, int64, int64)
BINARYOP (modu, (uint64_t)v1 % (uint64_t)v2, int64, int64, uint64)
BINARYOP (band, v1 & v2, int64, int64, int64)
BINARYOP (bor, v1 | v2, int64, int64, int64)
BINARYOP (bxor, v1 ^ v2, int64, int64, int64)
BINARYOP (shr, v1 >> v2, int64, int64, int64)
BINARYOP (shl, v1 << v2, int64, int64, int64)
UNARYOP (neg, (-v1), int64, int64)
UNARYOP (bcom, (~v1), int64, int64)
BINARYOP (eq, v1 =  v2, int64, int64, bool)
BINARYOP (ne, v1 != v2, int64, int64, bool)
BINARYOP (ge, v1 >= v2, int64, int64, bool)
BINARYOP (gt, v1 >  v2, int64, int64, bool)
BINARYOP (le, v1 <= v2, int64, int64, bool)
BINARYOP (lt, v1 <  v2, int64, int64, bool)
BINARYOP (geu, (uint64_t)v1 >= (uint64_t)v2, int64, int64, bool)
BINARYOP (gtu, (uint64_t)v1 >  (uint64_t)v2, int64, int64, bool)
BINARYOP (leu, (uint64_t)v1 <= (uint64_t)v2, int64, int64, bool)
BINARYOP (ltu, (uint64_t)v1 <  (uint64_t)v2, int64, int64, bool)



UNARYOP (i2f, (float32_t)v1, int8, float32)
UNARYOP (i2f, (float64_t)v1, int8, float64)
UNARYOP (i2f, (float80_t)v1, int8, float80)

UNARYOP (i2f, (float32_t)v1, int16, float32)
UNARYOP (i2f, (float64_t)v1, int16, float64)
UNARYOP (i2f, (float80_t)v1, int16, float80)

UNARYOP (i2f, (float32_t)v1, int32, float32)
UNARYOP (i2f, (float64_t)v1, int32, float64)
UNARYOP (i2f, (float80_t)v1, int32, float80)

UNARYOP (i2f, (float32_t)v1, int64, float32)
UNARYOP (i2f, (float64_t)v1, int64, float64)
UNARYOP (i2f, (float80_t)v1, int64, float80)

UNARYOP (i2f, (float32_t)v1, uint8, float32)
UNARYOP (i2f, (float64_t)v1, uint8, float64)
UNARYOP (i2f, (float80_t)v1, uint8, float80)

UNARYOP (i2f, (float32_t)v1, uint16, float32)
UNARYOP (i2f, (float64_t)v1, uint16, float64)
UNARYOP (i2f, (float80_t)v1, uint16, float80)

UNARYOP (i2f, (float32_t)v1, uint32, float32)
UNARYOP (i2f, (float64_t)v1, uint32, float64)
UNARYOP (i2f, (float80_t)v1, uint32, float80)

UNARYOP (i2f, (float32_t)v1, uint64, float32)
UNARYOP (i2f, (float64_t)v1, uint64, float64)
UNARYOP (i2f, (float80_t)v1, uint64, float80)

UNARYOP   (lobits,  v1 & 0xffffffff ,uint64,uint32)
UNARYOP   (lobits,  v1 & 0xffff     ,uint64,uint16)
UNARYOP   (lobits,  v1 & 0xff       ,uint64,uint8)
UNARYOP   (lobits,  v1 & 0xffff     ,uint32,uint16)
UNARYOP   (lobits,  v1 & 0xff       ,uint32,uint8)
UNARYOP   (lobits,  v1 & 0xff       ,uint16,uint8)

UNARYOP (zx, (int16_t)v1, int8, int16)
UNARYOP (zx, (int32_t)v1, int8, int32)
UNARYOP (zx, (int64_t)v1, int8, int64)

UNARYOP (zx, (int32_t)v1, int16, int32)
UNARYOP (zx, (int64_t)v1, int16, int64)

UNARYOP (zx, (int64_t)v1, int32, int64)


/* i8to? */
int8_t op_sx_int8_int8 (int8_t a, int msb)
{
  if (msb == 0)
    return (int8_t) a;
  else
    {
      if (!(0 < msb && msb < 8))
         abort (); 
      int8_t i = a | int8_sx[msb][1];
      return i;
    }
}

int16_t op_sx_int8_int16 (int8_t a, int msb)
{
  if (msb == 0)
    return (int16_t) a;
  else
    {
      if (!(0 < msb && msb < 8))
         abort ();
      int16_t i = a | int16_sx[msb][1];
      return i;
    }
}

int32_t op_sx_int8_int32 (int8_t a, int msb)
{
  if (msb == 0)
    return (int32_t) a;
  else
    {
      if (!(0 < msb && msb < 8))
         abort ();
      int32_t i = a | int32_sx[msb][1];
      return i;
    }
}

int64_t op_sx_int8_int64 (int8_t a, int msb)
{
  if (msb == 0)
    return (int64_t) a;
  else
    {
      if (!(0 < msb && msb < 8))
         abort ();
      int64_t i = a | int64_sx[msb][1];
      return i;
    }
}

/* i16to? */
int16_t op_sx_int16_int16 (int16_t a, int msb)
{
  if (msb == 0)
    return (int16_t) a;
  else
    {
      if (!(0 < msb && msb < 16))
         abort ();
      int16_t i = a | int16_sx[msb][1];
      return i;
    }
}

int32_t op_sx_int16_int32 (int16_t a, int msb)
{
  if (msb == 0)
    return (int32_t) a;
  else
    {
      if (!(0 < msb && msb < 16))
         abort ();
      int32_t i = a | int32_sx[msb][1];
      return i;
    }
}

int64_t op_sx_int16_int64 (int16_t a, int msb)
{
  if (msb == 0)
    return (int64_t) a;
  else
    {
      if (!(0 < msb && msb < 16))
         abort ();
      int64_t i = a | int64_sx[msb][1];
      return i;
    }
}


/* i32to? */
int32_t op_sx_int32_int32 (int32_t a, int msb)
{
  if (msb == 0)
    return (int32_t) a;
  else
    {
      if (!(0 < msb && msb < 32))
         abort ();
      int32_t i = a | int32_sx[msb][1];
      return i;
    }
}

int64_t op_sx_int32_int64 (int32_t a, int msb)
{
  if (msb == 0)
    return (int64_t) a;
  else
    {
      if (!(0 < msb && msb < 32))
         abort ();
      int64_t i = a | int64_sx[msb][1];
      return i;
    }
}


/* i64to? */
int64_t op_sx_int64_int64 (int64_t a, int msb)
{
  if (msb == 0)
    return (int64_t) a;
  else
    {
      if (!(0 < msb && msb < 64))
         abort ();
      int64_t i = a | int64_sx[msb][1];
      return i;
    }
}





/* zero extension */
/* uint8_t zx_int8_int8 (uint8 a, uint msb) */
/* { */
/*   return (uint8_t) a; */
/* } */

/* uint16_t zx_int8_int16 (uint8_t a, int msb) */
/* { */
/*   return (uint16_t) a; */
/* } */

/* uint32_t zx_int8_int32 (uint8_t a, int msb) */
/* { */
/*   if (msb == 0) */
/*     return (uint32_t) a; */
/*   else */
/*     { */
/*       assert (0 < msb && msb < 8); */
/*       uint32_t i = a | int32_zx[msb][1]; */
/*       return i; */
/*     } */
/* } */

/* uint64_t zx_int8_int64 (uint8_t a, int msb) */
/* { */
/*   if (msb == 0) */
/*     return (uint64_t) a; */
/*   else */
/*     { */
/*       assert (0 < msb && msb < 8); */
/*       uint64_t i = a | int64_zx[msb][1]; */
/*       return i; */
/*     } */
/* } */

/* /\* i16to? *\/ */
/* uint16_t zx_int16_int16 (uint16_t a, int msb) */
/* { */
/*   if (msb == 0) */
/*     return (uint16_t) a; */
/*   else */
/*     { */
/*       assert (0 < msb && msb < 16); */
/*       uint16_t i = a | int16_zx[msb][1]; */
/*       return i; */
/*     } */
/* } */

/* uint32_t zx_int16_int32 (uint16_t a, int msb) */
/* { */
/*   if (msb == 0) */
/*     return (uint32_t) a; */
/*   else */
/*     { */
/*       assert (0 < msb && msb < 16); */
/*       uint32_t i = a | int32_zx[msb][1]; */
/*       return i; */
/*     } */
/* } */

/* uint64_t zx_int16_int64 (uint16_t a, int msb) */
/* { */
/*   if (msb == 0) */
/*     return (uint64_t) a; */
/*   else */
/*     { */
/*       assert (0 < msb && msb < 16); */
/*       uint64_t i = a | int64_zx[msb][1]; */
/*       return i; */
/*     } */
/* } */


/* /\* i32to? *\/ */
/* uint32_t zx_int32_int32 (uint32_t a, int msb) */
/* { */
/*   if (msb == 0) */
/*     return (uint32_t) a; */
/*   else */
/*     { */
/*       assert (0 < msb && msb < 32); */
/*       uint32_t i = a | int32_zx[msb][1]; */
/*       return i; */
/*     } */
/* } */

/* uint64_t zx_int32_int64 (uint32_t a, int msb) */
/* { */
/*   if (msb == 0) */
/*     return (uint64_t) a; */
/*   else */
/*     { */
/*       assert (0 < msb && msb < 32); */
/*       uint64_t i = a | int64_zx[msb][1]; */
/*       return i; */
/*     } */
/* } */


/* /\* i64to? *\/ */
/* uint64_t zx_int64_int64 (uint64_t a, int msb) */
/* { */
/*   if (msb == 0) */
/*     return (uint64_t) a; */
/*   else */
/*     { */
/*       assert (0 < msb && msb < 64); */
/*       uint64_t i = a | int64_zx[msb][1]; */
/*       return i; */
/*     } */
/* } */
#endif /* GEN_ASM_H */
