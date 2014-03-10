type 'a command =
| Noop
| Delete
| Replace of 'a

module A1 = Bigarray.Array1

type ('a, 'elt) bigarray = ('a, 'elt, Bigarray.c_layout) A1.t

(* TODO: existentialize 'ke and 've? Needs GADT syntax? *)
type ('k, 'ke, 'v, 've) t = {
  mutable count: int;
  buckets: (int, Bigarray.int_elt) bigarray;

  (* These three encode an array of ('k, 'v, int) *)
  keys: ('k, 'ke) bigarray;
  values: ('v, 've) bigarray;
  nexts: (int, Bigarray.int_elt) bigarray;

  mutable first_free: int;
}

let subsequent_pos = -1
let end_of_list = -2

let create
      ?(initial_buckets=8)
      keykind
      valuekind
      =
  assert (initial_buckets > 0);
  let data_count = initial_buckets * 4 in
  let t = {
      count = 0;
      buckets = A1.create Bigarray.int Bigarray.c_layout initial_buckets;
      keys = A1.create keykind Bigarray.c_layout data_count;
      values = A1.create valuekind Bigarray.c_layout data_count;
      nexts = A1.create Bigarray.int Bigarray.c_layout data_count;
      first_free = 0;
    }
  in
  A1.fill t.buckets end_of_list;
  A1.fill t.nexts subsequent_pos;
  t.nexts.{data_count - 1} <- end_of_list;
  t

(* TODO: "clear" and "reset" *)

let copy t =
  let data_count = A1.dim t.keys in
  let t' = {
      count = t.count;
      buckets = A1.create Bigarray.int Bigarray.c_layout (A1.dim t.buckets);
      keys = A1.create (A1.kind t.keys) Bigarray.c_layout data_count;
      values = A1.create (A1.kind t.values) Bigarray.c_layout data_count;
      nexts = A1.create Bigarray.int Bigarray.c_layout data_count;
      first_free = t.first_free;
    }
  in
  A1.blit t.buckets t'.buckets;
  A1.blit t.keys t'.keys;
  A1.blit t.values t'.values;
  A1.blit t.nexts t'.nexts;
  t'

let next_pos t pos =
  let pos' = t.nexts.{pos} in
  if pos' = subsequent_pos
  then pos + 1
  else pos'


let fold f t =
  let rec traverse_chain pos acc =
    if pos = end_of_list
    then acc
    else
      let acc = f t.keys.{pos} t.values.{pos} acc in
      traverse_chain t.nexts.{pos} acc
  in
  let rec traverse_buckets bucket acc =
    if bucket < 0
    then acc
    else (traverse_chain t.buckets.{bucket} acc;
          traverse_buckets (bucket-1) acc)
  in
  traverse_buckets (A1.dim t.buckets - 1)


let iter f t =
  fold (fun k v () -> f k v) t ()

let access key f t =
  let bucket = Hashtbl.hash key in
  let rec find_in_bucket ow pos =
    if pos = end_of_list
    then ow, pos
    else
      if t.keys.{pos} = key
      then ow, pos
      else
        let ow pos' = t.nexts.{pos} <- pos' in
        find_in_bucket ow (next_pos t pos)
  in
  let ow_first pos' = t.buckets.{bucket} <- pos' in
  let overwrite_prev_pos, pos = find_in_bucket ow_first t.buckets.{bucket} in
  let value_opt =
    if pos = end_of_list
    then None
    else Some t.values.{pos}
  in
  let command, retval = f value_opt in
  begin match command with
  | Noop -> ()
  | Delete ->
      if pos <> end_of_list then
        let next_data_pos = next_pos t pos in
        t.nexts.{pos} <- t.first_free;
        t.first_free <- pos;
        overwrite_prev_pos next_data_pos;
        t.count <- t.count - 1
        (* We don't resize down. This is consistent with Hashtbl. *)
  | Replace value -> 
      if pos <> end_of_list
      then t.values.{pos} <- value
      else
        let freepos = t.first_free in
        if freepos = end_of_list
        then failwith "TODO: resize the table"
        else begin
          t.first_free <- next_pos t freepos;
          t.nexts.{freepos} <- t.buckets.{bucket};
          t.buckets.{bucket} <- freepos;
          t.keys.{freepos} <- key;
          t.values.{freepos} <- value;
          t.count <- t.count + 1
        end
  end;
  retval


let find_opt key =
  access key (fun value -> Noop, value)

let find_exn key =
  access key (function
                | Some value -> Noop, value
                | None -> raise Not_found)

let mem key =
  access key (function
                | Some _ -> Noop, true
                | None -> Noop, false)

let replace key value =
  access key (fun _ -> Replace value, ())

let remove key =
  access key (fun _ -> Delete, ())


(* Use unsafe_get and unsafe_set when code is mature *)
(* Could also try to implement open addressing instead of chaining *)
(* Support duplicate keys? *)
(* Instead of the "subsequent_pos" trick, we could remember an
 * "initialized_until" position. Then we wouldn't have to spend time and
 * (overcommitted) memory on init of "nexts". But we still have to init
 * "buckets", so does it matter much? *)
(* Why use bigarrays for this? We can store all the same values unboxed in
 * standard arrays (except complex64). Maybe use standard arrays just for the
 * int ones? *)
(* This library would benefit greatly if Bigarray could support tuples. *)