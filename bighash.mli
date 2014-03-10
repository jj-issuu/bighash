type 'a command =
| Noop
| Delete
| Replace of 'a

type ('k, 'ke, 'v, 've) t

val create: ?initial_buckets:int ->
            ('k, 'ke) Bigarray.kind ->
            ('v, 've) Bigarray.kind ->
            ('k, 'ke, 'v, 've) t

val copy: ('k, 'ke, 'v, 've) t ->
          ('k, 'ke, 'v, 've) t

val access: 'k ->
            ('v option -> 'v command * 'a) ->
            ('k,'ke,'v,'ve) t ->
            'a

val find_opt: 'k ->
               ('k,'ke,'v,'ve) t ->
               'v option

val find_exn: 'k ->
               ('k,'ke,'v,'ve) t ->
               'v

val mem: 'k ->
          ('k,'ke,'v,'ve) t ->
          bool

val replace: 'k ->
             'v ->
             ('k,'ke,'v,'ve) t ->
             unit

val remove: 'k ->
            ('k,'ke,'v,'ve) t ->
            unit

val fold: ('k -> 'v -> 'a -> 'a) ->
          ('k, 'ke, 'v, 've) t ->
          'a ->
          'a

val iter: ('k -> 'v -> unit) ->
          ('k, 'ke, 'v, 've) t ->
          unit
