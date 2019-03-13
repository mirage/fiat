let pp_hex_le fmt cs =
  let n = Cstruct.len cs in
  for i = n - 1 downto 0 do
    let byte = Cstruct.get_uint8 cs i in
    Format.fprintf fmt "%02x" byte
  done
