let codes = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_."
let () = assert (String.length codes = 64)

let encode s =
  let b = Buffer.create 32 in
  let n = String.length s in
  let rec walk i accum bits =
    if bits >= 5 then
      let idx = accum lsr (bits-5) in
      Buffer.add_char b codes.[idx land 0x1f];
      walk i accum (bits-5)
    else if i = n then
      if bits = 0 then ()
      else walk i (accum lsl 1) (bits+1)
    else
      walk (i+1) ((accum lsl 8) lor (Char.code s.[i])) (bits+8) in
  begin
    walk 0 0 0;
    Buffer.contents b
  end
