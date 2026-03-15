open Utils

type packet = { x : float; y : float; eid : int (* user id / entity id *) }

let write x y eid =
  let packet_writer = PacketWriter.empty Location in
  PacketWriter.write_float packet_writer x;
  PacketWriter.write_float packet_writer y;
  PacketWriter.write_uint16 packet_writer eid;
  PacketWriter.build packet_writer
