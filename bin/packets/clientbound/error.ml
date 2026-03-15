open Utils

type packet = { error_type : Types.error_type }

let write error =
  let packet = PacketWriter.empty Error in
  PacketWriter.write_uint8 packet (Convert.error_to_id error);
  PacketWriter.build packet
