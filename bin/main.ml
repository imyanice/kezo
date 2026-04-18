let ( let* ) = Lwt.bind

open Packets.Types

let max_packet_size = 65_535

let println s =
  print_endline s;
  flush stdout

module ClientsMap = Map.Make (Int)

let clients_map : Lwt_io.output Lwt_io.channel ClientsMap.t ref =
  ref ClientsMap.empty

module UsersMap = Map.Make (Int)

let users_map : Users.t UsersMap.t ref = ref UsersMap.empty
let current_id = ref 0

let login_player packet_data output =
  let login_packet = Packets.ServerBound.Login.read packet_data in
  println (login_packet.username ^ " is logging in");
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

let handle_play_packets (_input, _output) id
    (packet_reader : Packets.Utils.PacketReader.packet) =
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
  (*TODO: remove the user in the map*)
  println "closing connection";
  let bytes, length = Packets.ClientBound.Error.write reason in
  try%lwt
    Lwt.finalize
      (fun () -> Lwt_io.write_from_exactly output bytes 0 length)
      (fun () -> Lwt.join [ Lwt_io.close input; Lwt_io.close output ])
  with _ -> Lwt.return_unit

let parse_packet input =
  let* size = Lwt_io.BE.read_int16 input in
  let size = size land 0xFFFF in
  if size > max_packet_size then Lwt.fail Invalid_Length
  else
    let bytes = Bytes.create size in
    let* () = Lwt_io.read_into_exactly input bytes 0 size in
    Lwt.return (Packets.Utils.PacketReader.empty bytes)

(* we should put the packets in a queue to be handled later
   why? because if packets arrive too fast as we can handle them
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
      if not (Lwt_io.is_closed input || Lwt_io.is_closed output) then
        receive_data ()
      else Lwt.return_unit
    with
    | Unexpected -> close_with_reason input output Unexpected
    | Invalid_Length -> close_with_reason input output Invalid_Length
    | _ ->
        println "connection closed by EOF";
        Lwt.return_unit
  in
  let handshake () =
    println "handshake started";
    let* size = Lwt_io.BE.read_int16 input in
    let size = (size land 0xFFFF) - 2 in
    println @@ Printf.sprintf "reading %i bytes" size;
    if size > max_packet_size (* max packet length in bytes *) then
      close_with_reason input output Invalid_Length
    else
      let bytes = Bytes.create size in
      let* () = Lwt_io.read_into_exactly input bytes 0 size in
      let packet_reader = Packets.Utils.PacketReader.empty bytes in
      println
      @@ Printf.sprintf "receiving packet with id: %i"
           (Packets.Utils.Convert.id_from_packet_type packet_reader.id);
      match packet_reader.id with
      | Login ->
          login_player packet_reader.data output;
          receive_data ()
      | _ -> close_with_reason input output Handshake_Fail
  in
  try%lwt handshake () with _ -> Lwt.return_unit

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
  Printexc.record_backtrace true;
  let port = 8080 in
  Lwt_main.run (start_server port)
