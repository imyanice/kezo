open Utils

type packet = { x : float; y : float }

let read packet_reader =
  {
    x = PacketReader.read_float packet_reader;
    y = PacketReader.read_float packet_reader;
  }
