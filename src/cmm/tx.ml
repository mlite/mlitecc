type t = { mutable limit : int; mutable remaining : int; mutable last : string; }

let ts = { limit = max_int; remaining = max_int; last = "<none>"; }

let _ = Reinit.at 
  (fun () -> 
    begin 
      ts.limit <- max_int;
      ts.remaining <- ts.limit;
      ts.last <- "<none>";
    end)

let () = Cmm_debug.register "tx" "watch transaction counts"

let set_limit n = ts.limit <- n; ts.remaining <- n
let remaining () = ts.remaining
let decrement ~name ~from ~to' =
  assert (ts.remaining = from);
  if from <> to' then
    begin
      Cmm_debug.eprintf "tx" "'%s' decrementing tx's from %d to %d (diff is %d)\n"
        name from to' (from-to');
      ts.remaining <- to';
      ts.last <- name
    end

let used _ = ts.limit - ts.remaining
let last _ = ts.last

