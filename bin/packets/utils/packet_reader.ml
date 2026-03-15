open Types

type packet = { id : packets_type; data : Bytes.t; offset : int }

let empty bytes =
  {
    id = Convert.packet_type_from_id (Bytes.get_uint16_be bytes 0);
    data = bytes;
    offset = 2;
  }

let read_uint8 packet =
  let r = Bytes.get_uint8 packet.data packet.offset in
  incr (ref packet.offset);
  r

let read_float packet =
  let r = Bytes.get_uint16_be packet.data packet.offset in
  ref packet.offset := packet.offset + 8;
  Float.of_int r
