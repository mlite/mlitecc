#line 142 "uint64.nw"
#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/config.h>
#line 150 "uint64.nw"
value uint64_compare(value v1, value v2)
{
    uint64 i1 = Int64_val(v1);
    uint64 i2 = Int64_val(v2);
    return i1 == i2 ? Val_int(0) : i1 < i2 ? Val_int(-1) : Val_int(1);
}
#line 159 "uint64.nw"
value uint64_float64(value f)
{
    union { double d; int64 i; } buffer;
    buffer.d = Double_val(f);
    return copy_int64(buffer.i);
}

value uint64_float32(value f)
{
    union { float f; unsigned n; } buffer;
    int64 i;
    buffer.f = (float) Double_val(f);
    i = (int64) buffer.n;
    return copy_int64(i);
}

#line 178 "uint64.nw"
value uint64_i2f(value i)
{
    union { double d; int64 i; } buffer;
    buffer.i = Int64_val(i);
    return copy_double(buffer.d);
}
#line 189 "uint64.nw"
value uint64_i2i(value i)
{
    union { int32 i[2]; int64 j; } buffer;
    
    buffer.i[0] = Int_val(i);
    buffer.i[1] = 0;

    return copy_int64(buffer.j);
}
#line 201 "uint64.nw"
value uint64_add(value v1, value v2)  /* ML */
{ return copy_int64((uint64)Int64_val(v1) + (uint64)Int64_val(v2)); }

value uint64_sub(value v1, value v2)  /* ML */
{ return copy_int64((uint64)Int64_val(v1) - (uint64)Int64_val(v2)); }

value uint64_mul(value v1, value v2)  /* ML */
{ return copy_int64((uint64)Int64_val(v1) * (uint64)Int64_val(v2)); }
#line 211 "uint64.nw"
value uint64_div(value v1, value v2)  /* ML */
{
    int64 divisor = Int64_val(v2);
    if (divisor == 0) raise_zero_divide();
    return copy_int64((uint64)Int64_val(v1) / (uint64)divisor);
}

value uint64_mod(value v1, value v2)  /* ML */
{
    int64 divisor = Int64_val(v2);
    if (divisor == 0) raise_zero_divide();
    return copy_int64((uint64)Int64_val(v1) % (uint64)divisor);
}
#line 230 "uint64.nw"
static int parse_digit(char * p)
{
    int c = *p;
    if (c >= '0' && c <= '9')
        return c - '0';
    else if (c >= 'A' && c <= 'F')
        return c - 'A' + 10;
    else if (c >= 'a' && c <= 'f')
        return c - 'a' + 10;
    else
        return -1;
}
#line 247 "uint64.nw"
static char* parse_base (char *p, int *base)
{
    *base = 10;
    if (*p == '0') {
        switch (p[1]) {
        case 'x': case 'X':
            *base = 16; return p + 2;
        case 'o': case 'O':
            *base =  8; return p + 2; 
        case 'b': case 'B':
            *base =  2; return p + 2; 
        default:
            *base =  8; return p;
        }
    } else {
        return p;
    }
}
#line 269 "uint64.nw"
value uint64_of_string(value s)          /* ML */
{
    uint64 max_uint64 = ~(uint64)0;
    char * p;
    uint64 res, threshold;
    int base, d;

    p = parse_base (String_val(s), &base);
    threshold = max_uint64 / (uint64) base;
    for (res = 0; /*nothing*/; p++) {
        d = parse_digit(p);
        if (d < 0 || d >= base) break;
        /* Detect overflow in multiplication base * res */
        if (threshold < res) caml_failwith("overflow");
        res = res * (uint64)base + (uint64)d;
        /* Detect overflow in addition (base * res) + d */
        if (res < (uint64)d) caml_failwith("overflow");
    }
    if ((base == 10 || (base == 8 && res == 0)) && (*p == 'u' || *p == 'U')) p++;
    if (p != String_val(s) + caml_string_length(s)) failwith("syntax");
    return copy_int64(res);
}
