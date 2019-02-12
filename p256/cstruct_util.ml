let pp_hex_le fmt cs =
  let n = Cstruct.len cs in
  for i = n - 1 downto 0 do
    let byte = Cstruct.get_uint8 cs i in
    Format.fprintf fmt "%02x" byte
  done

let rev cs =
  let len = Cstruct.len cs in
  let out = Cstruct.create len in
  for i = 0 to len - 1 do
    Cstruct.set_uint8 out i (Cstruct.get_uint8 cs (len - 1 - i))
  done;
  out
