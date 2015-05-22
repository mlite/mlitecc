exception Unsupported
type state = { mutable n : int; mutable explanations : (int list -> string list) list }
let state = { n = 0; explanations = [] }

let getn f =
  state.n <- state.n + 1;
  let n = state.n in
  state.explanations <- f :: state.explanations;
  n


let fail msg code args =
(*  let binname = Sys.argv.(0) in *)
  let binname = "qc--" in
  let args = String.concat ", " (List.map string_of_int (code :: args)) in
  Printf.eprintf "%s\n" msg;
  Printf.eprintf "For a longer explanation run\n  %s -e 'Unsupported.explain(%s)'\n"
        binname args;
  raise Unsupported


let explain n ns =
  let rec ex m l = match l with
  | [] -> Printf.printf "There is no explanation numbered %d\n" n
  | x :: xs ->
      if m = n then
        List.iter (Printf.printf "%s\n") (x ns)
      else
        ex (m-1) xs
  in ex state.n state.explanations


let exs ss = (fun _ -> ss)
let ex0 f = function
  |	[] -> f()
  |	_  ->
      ["For this explanation code, Unsupported.explain expects exactly one argument"]
let ex1 f = function
  | [n] -> f n
  | _ ->
      ["For this explanation code, Unsupported.explain expects exactly two arguments"]
let ex2 f = function
  | [n; m] -> f n m
  | _ ->
    ["For this explanation code, Unsupported.explain expects exactly three arguments"]


let s = Printf.sprintf
let nosupport s = "This back end does not support " ^ s


let widen_n = getn (ex1 (fun n ->
    [ s "On this target, floating-point operations can be at most %d bits wide." n;
      "Narrower operations can be widened using one of the stages in the Widen";
      s "module, but anything wider than %d bits is unsupported." n;
    ]))

let widen_float d =
  fail (nosupport (s "%d-bit floating-point computation" d)) widen_n [d]


let stack_width_n = getn (ex2 (fun h w ->
  [ s "Your code tried to pass a %d-bit argument to an operator that takes its" h;
    s "arguments on the machine stack, but on this target, only %d-bit values can" w;
    "go on the machine stack.";
    "(This message could be triggered if you tried to use software rounding modes";
    "instead of hardware rounding modes in a floating-point instruction on the";
    s "Pentium, or if you tried to use something other than %d-bit floats.)" w;
  ]))

let stack_width ~have ~want =
  fail (nosupport (s "%d-bit value on the machine stack" have))
       stack_width_n [have; want]


let calling_convention_n = getn (ex0 (fun _ ->
  [ "Your code tried to make a procedure call, return, or cut to using an"
  ; "unsupported calling convention.  Please verify the correct spelling of the"
  ; "calling convention or get a wizard to extend the compiler with this"
  ; "calling convention."
  ]))

let calling_convention name =
  fail (nosupport (s "the '%s' calling convention" name))
       calling_convention_n []


let automaton_widths_n = getn (ex1 (fun x ->
  [ s "Your code tried to pass a %d-bit value as a parameter or return" x
  ; "value to/from a procedure although it was not supported by the calling"
  ; "convention being used for the call/return/cut to."
  ]))

let automaton_widths w =
  fail (s "The current calling convention could not support a %d-bit value" w)
      automaton_widths_n [w]

let automaton_widen_n = getn (ex2 (fun have want ->
  [ s "Your code tried to pass a %d-bit value as a parameter or return" have
  ; "value to/from a procedure although the calling convention being"
  ; s "used expected a value %d-bits wide or potentially narrower." want
  ]))

let automaton_widen ~have ~want =
  fail (s "The current calling convention either could not widen a " ^
        s "%d-bit value to %d-bits or fit it in a %d-bit register"
          have want want)
      automaton_widen_n [ have ; want ]


let div_and_mod_n = getn (fun _ ->
    ["On this target, signed division and modulus round toward zero.";
     "You must use the operators %quot and %rem, not %div and %mod.";
    ])

let div_and_mod () = fail (nosupport "%div and %mod") div_and_mod_n []

let div_overflows_n = getn (fun _ ->
    ["This code generator cannot test for %div_overflows and %quot_overflows"])

let div_overflows () =
  fail (nosupport "overflow detection for division") div_overflows_n []

let mulx_and_mulux_n = getn (fun _ ->
    ["This target does not support extended integer-multiply operators."]
    )

let mulx_and_mulux () = fail (nosupport "%mulx and %mulux") mulx_and_mulux_n []

let floatlit = getn (ex1 (fun n ->
    [ "On this target, floating-point literals can be either 32 or 64 bits wide.";
      s "You asked for a literal of %d bits.  You could try %%f2f%d(<literal>)." n n;
    ]))

let floatlit d =
  fail (nosupport (s "%d-bit floating-point literal" d)) floatlit [d]

let singlebit_n = getn (fun _ ->
  ["This target does not support extended addition and subtraction using";
   "single-bit values with the %addc, %carry, %subb, and %borrow operators.";
  ])
let singlebit ~op =
  fail (nosupport (s "the %%%s operator" op)) singlebit_n []

let popcnt_n = getn (ex2 (fun good bad ->
  [ s "Your code tried to use the %%popcnt operator on a %d-bit argument," bad;
    s "but on this target, %%popcnt applies only to %d-bit values." good;
  ]))

let popcnt ~notok ~ok =
  fail (nosupport (s "%%popcnt(bits%d)" notok))
       popcnt_n [ok; notok]
