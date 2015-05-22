type t =
  | I2F of (int * mode)
  | F2F of (int * mode)
  | F2I of (int * mode)
  | FABS
  | FADD of mode
  | FDIV of mode
  | FEQ
  | FGT
  | FGE
  | FLT
  | FLE
  | FMUL of mode
  | FMULX 
  | FNE
  | FNEG
  | FORDERED
  | FSQRT
  | FSUB of mode
  | FUNORDERED
  | MINF of int
  | MZERO of int
  | NaN of int
  | PINF of int
  | PZERO of int

and mode = 
  | Round_down
  | Round_up
  | Round_nearest
  | Round_zero

and intop = 
  | ADD
  | ADDC
  | ADD_OVERFLOWS
  | AND
  | BIT
  | BOOL
  | BORROW
  | CARRY
  | COM
  | CONJOIN
  | DISJOIN
  | DIV
  | DIV_OVERFLOWS
  | DIVU
  | EQ
  | FALSE
  | GE
  | GEU
  | GT
  | GTU
  | LE
  | LEU
  | LOBITS of int
  | LT
  | LTU
  | MOD
  | MODU
  | MUL
  | MUL_OVERFLOWS 
  | MULU_OVERFLOWS
  | MULUX
  | MULX
  | NE
  | NEG
  | NOT
  | OR
  | PARITY
  | POPCNT
  | QUOT
  | QUOT_OVERFLOWS
  | REM
  | ROTL
  | ROTR
  | SHL
  | SHRA
  | SHRL
  | SUB
  | SUBB
  | SUB_OVERFLOWS
  | SX of int
  | TRUE
  | XOR
  | ZX of int
	
