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

#include <caml/custom.h>
enum cval 
  {
    CHAR,
    SCHAR,
    UCHAR,
    SHORT,
    USHORT,
    INT,
    UINT,
    LONG,
    ULONG,
    LLONG,
    ULLONG,
    FLOAT32,
    FLOAT64,
    FLOAT80
  };

struct cval 
{
  int type;
  int data[7];
};


void serialize (value v, uintnat * wsize_32, uintnat * wsize_64)
{
  int w = Wosize_val (v);
  * wsize_32 = sizeof(struct cval)/(sizeof(int));
  * wsize_64 = sizeof(struct cval)/(sizeof(int));
  caml_serialize_block_1 (Data_custom_val(v), sizeof(struct cval));
}

uintnat * deserialize (void * dst)
{
  caml_deserialize_block_1 (Data_custom_val(v), sizeof(struct cval));
}



struct custom_operations cvalop = 
  {
    .identifier = "cval";
    .finalize = custom_finalize_default;
    .compare = custom_compare_default;
    .hash = custom_hash_default;
    .serialize = serialize;
    .deserialize = deserialize;
  };
