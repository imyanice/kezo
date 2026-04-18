open Types

type packet = { id : packets_type; data : Buffer.t }

let empty p_id =
  let b = Buffer.create 1 in
  Buffer.add_uint8 b (Convert.packet_type_to_id p_id); (* packet ID is 1 byte *)
  { id = p_id; data = b }

let write_uint8 packet byte = Buffer.add_uint8 packet.data byte

(* {id = packet.id; data = packet.data } *)
let write_float packet f =
  f |> Int64.bits_of_float |> Buffer.add_int64_le packet.data

let write_uint16 packet = Buffer.add_uint16_le packet.data

let build p =
  let bytes = Buffer.to_bytes p.data in
  let length = Bytes.length bytes in
  let l_bytes = Bytes.create 2 in
  Bytes.set_uint16_be l_bytes 0 2;
  (Bytes.cat l_bytes bytes, length + 2)
