type t =
  { h : Fe.t
  ; i : Fe.t
  ; j : Fe.t
  ; r : Fe.t
  ; s1 : Fe.t
  ; s1j : Fe.t
  ; s2 : Fe.t
  ; two_z1z2 : Fe.t
  ; u1 : Fe.t
  ; u2 : Fe.t
  ; v : Fe.t
  ; z1z1 : Fe.t
  ; z1z1z1 : Fe.t
  ; z2z2 : Fe.t }

let create () =
  { h = Fe.create ()
  ; i = Fe.create ()
  ; j = Fe.create ()
  ; r = Fe.create ()
  ; s1 = Fe.create ()
  ; s1j = Fe.create ()
  ; s2 = Fe.create ()
  ; two_z1z2 = Fe.create ()
  ; u1 = Fe.create ()
  ; u2 = Fe.create ()
  ; v = Fe.create ()
  ; z1z1 = Fe.create ()
  ; z1z1z1 = Fe.create ()
  ; z2z2 = Fe.create () }
