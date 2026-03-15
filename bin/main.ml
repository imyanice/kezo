let ( let* ) = Lwt.bind
let max_packet_size = 65_535

module ClientsMap = Map.Make (Int)

let clients_map : Lwt_io.output Lwt_io.channel ClientsMap.t ref =
  ref ClientsMap.empty

type inventory_items = Default
type location = Spawn | IDF

module Users = struct
  type t = {
    username : string;
    uuid : string;
    inventory : (inventory_items * int) list;
    position : float * float;
    location : location;
  }

  let init =
    {
      username = "steve";
      uuid = "00000000-0000-0000-0000-000000000000";
      inventory = [];
      position = (0., 0.);
      location = Spawn;
    }

  (** input validation is expected to be done by the caller *)
  let set_username user new_username =
    {
      username = new_username;
      uuid = user.uuid;
      inventory = user.inventory;
      position = user.position;
      location = user.location;
    }

  let set_uuid user new_uuid =
    {
      username = user.username;
      uuid = new_uuid;
      inventory = user.inventory;
      position = user.position;
      location = user.location;
    }

  let set_position user new_pos =
    {
      username = user.username;
      uuid = user.uuid;
      inventory = user.inventory;
      position = new_pos;
      location = user.location;
    }
end

module UsersMap = Map.Make (Int)

let users_map : Users.t UsersMap.t ref = ref UsersMap.empty
let current_id = ref 0

type packets_type = Error | Login | Location | Invalid

type error_type =
  | Invalid_Length
  | Handshake_Fail
  | Ping_Uncomplete
  | Unexpected

exception Invalid_Length
exception Unexpected

(* 0x12FF *)
let bytes_from_list bytes =
  let rec aux = function
    | b :: tail, n ->
        let _ = Bytes.set bytes n b in
        aux (tail, n + 1)
    | [], _ -> ()
  in
  aux

let packet_type_to_id = function
  | Error -> 0x00
  | Login -> 0x01
  | Location -> 0x02
  | Invalid -> -1

let packet_type_from_id = function
  | 0x00 -> Error
  | 0x01 -> Login
  | _ -> Invalid

let error_to_id = function
  (* match __ with *)
  | Handshake_Fail -> 0x00
  | Invalid_Length -> 0x01
  | Ping_Uncomplete -> 0x02
  | Unexpected -> 0x03

module PacketWriter = struct
  type packet = { id : packets_type; data : Buffer.t }

  let empty p_id =
    let b = Buffer.create 1 in
    Buffer.add_uint8 b (packet_type_to_id p_id);
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
end

module PacketReader = struct
  type packet = { id : packets_type; data : Bytes.t; offset : int }

  let empty bytes =
    {
      id = packet_type_from_id (Bytes.get_uint16_be bytes 0);
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
end

module ClientBound = struct
  module Error = struct
    type packet = { error_type : error_type }

    let write error =
      let packet = PacketWriter.empty Error in
      PacketWriter.write_uint8 packet (error_to_id error);
      PacketWriter.build packet
  end

  module Location = struct
    type packet = { x : float; y : float; eid : int (* user id / entity id *) }

    let write x y eid =
      let packet_writer = PacketWriter.empty Location in
      PacketWriter.write_float packet_writer x;
      PacketWriter.write_float packet_writer y;
      PacketWriter.write_uint16 packet_writer eid;
      PacketWriter.build packet_writer
  end
end

module ServerBound = struct
  module Login = struct
    type packet = { username : string; uuid : string }

    let read bytes =
      {
        username = Bytes.sub_string bytes 0 16;
        uuid = Bytes.sub_string bytes 16 32;
      }
    (* need proper string encoding *)
  end

  module Location = struct
    type packet = { x : float; y : float }

    let read packet_reader =
      {
        x = PacketReader.read_float packet_reader;
        y = PacketReader.read_float packet_reader;
      }
  end
end

module Packets = struct
  module ServerBound = ServerBound
  module ClientBound = ClientBound
end

let login_player packet_data output =
  let login_packet = Packets.ServerBound.Login.read packet_data in
  print_string
    (login_packet.username ^ " is logging in with uuid: " ^ login_packet.uuid);
  clients_map := !clients_map |> ClientsMap.add !current_id output;
  let user = Users.init in
  let user = Users.set_username user login_packet.username in
  let user = Users.set_uuid user login_packet.uuid in
  users_map := !users_map |> UsersMap.add !current_id user;
  incr current_id

let broadcast_packet bytes length =
  ClientsMap.to_seq !clients_map
  |> Seq.map (fun (_k, stream) ->
         Lwt_io.write_from_exactly stream bytes 0 length)
  |> List.of_seq |> Lwt.join


let rec handle_play_packets (_input, _output) id
    (packet_reader : PacketReader.packet) =
  match packet_reader.id with
  | Location ->
      let location_packet = Packets.ServerBound.Location.read packet_reader in
      let user = UsersMap.find id !users_map in
      let old_x, old_y = user.position in
      if
        abs_float (old_y -. location_packet.y) <= 3.
        && abs_float (old_x -. location_packet.x) <= 3.
      then (
        let user =
          Users.set_position user (location_packet.x, location_packet.y)
        in
        users_map := UsersMap.add id user !users_map;
        let bytes, len =
          Packets.ClientBound.Location.write location_packet.x location_packet.y
            id
        in

        broadcast_packet bytes len)
      else Lwt.fail Unexpected (* sending weird packets *)
  | _ -> Lwt.fail Unexpected

let close_with_reason input output reason =
  let bytes, length = Packets.ClientBound.Error.write reason in
  Lwt.finalize
    (fun () -> Lwt_io.write_from_exactly output bytes 0 (length))
    (fun () -> Lwt.join [ Lwt_io.close input; Lwt_io.close output ])

let parse_packet input =
  let* size = Lwt_io.read_int16 input in
  if size > max_packet_size then Lwt.fail Invalid_Length
  else
    let bytes = Bytes.create size in
    let* () = Lwt_io.read_into_exactly input bytes 0 size in
    Lwt.return (PacketReader.empty bytes)

(* we should put the packets in a queue to be handled later
   why? because if packets arrive to fast as we can handle
   we may process packet 2 before packet 1
   no use of handling that now because i don't want to :D
*)
let handle_client (input, output) =
  let rec receive_data () =
    try%lwt
      let* packet = parse_packet input in

      Lwt.async (fun () ->
          Lwt.catch
            (fun () -> handle_play_packets (input, output) !current_id packet)
            (function _ -> close_with_reason input output Unexpected));
      if not ((Lwt_io.is_closed input) || (Lwt_io.is_closed output))  then receive_data ()
      else Lwt.return_unit
    with
    | Unexpected -> close_with_reason input output Unexpected
    | Invalid_Length -> close_with_reason input output Invalid_Length
  in
  let loop () =
    let* size = Lwt_io.read_int16 input in
    if size > max_packet_size (* max packet length in bytes *) then
      close_with_reason input output Invalid_Length
    else
      let bytes = Bytes.create size in
      let* () = Lwt_io.read_into_exactly input bytes 0 size in
      let packet_reader = PacketReader.empty bytes in
      match packet_reader.id with
      | Login ->
          login_player packet_reader.data output;
          receive_data ()
      | _ -> close_with_reason input output Handshake_Fail
  in
  loop ()

let rec accept_connections server_socket =
  let* client_socket, _addr = Lwt_unix.accept server_socket in
  let input = Lwt_io.of_fd ~mode:Lwt_io.input client_socket in
  let output = Lwt_io.of_fd ~mode:Lwt_io.output client_socket in

  Lwt.async (fun () -> handle_client (input, output));
  accept_connections server_socket

let start_server port =
  let sockaddr = Unix.(ADDR_INET (inet_addr_any, port)) in
  let server_socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in

  Lwt_unix.setsockopt server_socket Unix.SO_REUSEADDR true;
  let* () = Lwt_unix.bind server_socket sockaddr in
  Lwt_unix.listen server_socket 10;
  let* () = Lwt_io.printlf "kezo listenting on %d" port in
  accept_connections server_socket

let () =
  let port = 8080 in
  Lwt_main.run (start_server port)
