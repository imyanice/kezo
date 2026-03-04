let ( let* ) = Lwt.bind
module ClientsMap = Map.Make(Int)
let clients_map : (Lwt_io.output Lwt_io.channel) ClientsMap.t  ref = ref ClientsMap.empty

type inventory_items =
  | Default

type location =
  Spawn
  | IDF
module Users =
  struct
   type t = { username: string; uuid: string; inventory:  (inventory_items * int) list; position: float * float; location : location}
   let init = { username = "steve"; uuid = "00000000-0000-0000-0000-000000000000"; inventory = []; position = 0., 0.; location = Spawn }

   (** input validation is expected to be done by the caller *)
   let set_username user new_username = {username = new_username; uuid = user.uuid; inventory = user.inventory; position = user.position; location = user.location}
   let set_uuid user new_uuid= {username = user.username; uuid = new_uuid; inventory = user.inventory; position = user.position; location = user.location}
  end

module UsersMap = Map.Make(Int)
let users_map :  (Users.t UsersMap.t) ref = ref UsersMap.empty
let current_id = ref 0
type packets_type =
  | Error
  | Login
  | Invalid
type error_type =
  | Invalid_Length
  | Handshake_Fail
  | Ping_Uncomplete
(* 0x12FF *)
let bytes_from_list bytes = let rec aux = function
  | (b::tail, n) -> let _ =  Bytes.set bytes n b in aux (tail, (n + 1))
  | ([], _) -> () in aux
let packet_type_to_id = function
  | Error -> 0x00
  | Login -> 0x01
| Invalid -> -1
let packet_type_from_id = function
  | 0x00 -> Error
  | 0x01 -> Login
  | _ -> Invalid
let error_to_id = function
  | Handshake_Fail-> 0x00
  | Invalid_Length -> 0x01
  | Ping_Uncomplete -> 0x02

module PacketWriter =
  struct
    type packet = {id: packets_type; data : Buffer.t}
    let empty p_id = let b = Buffer.create 1 in Buffer.add_uint8 b (packet_type_to_id p_id); { id = p_id; data = b }
    let write_uint8 packet byte =
      Buffer.add_uint8 packet.data byte
      (* {id = packet.id; data = packet.data } *)
    let build p =
      let bytes = Buffer.to_bytes p.data in
      let length = Bytes.length bytes in
      let l_bytes = Bytes.create 2 in
      Bytes.set_uint16_be l_bytes 0 2;
      (Bytes.cat l_bytes bytes, length + 2)
  end

module PacketReader =
  struct
    type packet = {id: packets_type; data : Bytes.t; offset: int}
    let empty bytes = { id = packet_type_from_id (Bytes.get_uint16_be bytes 0); data = bytes; offset = 2 }
    let read_uint8 packet = let r = Bytes.get_uint8 packet.data packet.offset in incr (ref packet.offset); r
  end

module ClientBound = struct
  module Error = struct
    type packet = { error_type: error_type }
    let write error = let packet = PacketWriter.empty Error in PacketWriter.write_uint8 packet (error_to_id error.error_type); PacketWriter.build packet
  end
end

module ServerBound = struct
  module Login = struct
    type packet = { username: string; uuid: string}
    let read bytes = { username = Bytes.sub_string bytes 0 16; uuid = Bytes.sub_string bytes 16 32 } (* need proper string encoding *)
  end
end

module Packets = struct
  module ServerBound = ServerBound
  module ClientBound = ClientBound
end

let handle_packet bytes =
   print_string "called";
   Printf.printf " first=%02X" (Char.code (Bytes.get bytes 0));
   print_string " called2\n";
   Printf.printf "payload as string: %S\n" (Bytes.to_string bytes);
   Printf.printf "payload as hex: ";
   Bytes.iter (fun c -> Printf.printf "%02X " (Char.code c)) bytes;
   print_newline ();
   flush stdout

let login_player packet_data output =
  let login_packet = Packets.ServerBound.Login.read packet_data in
  print_string (login_packet.username ^ " is logging in with uuid: " ^ login_packet.uuid);
  clients_map := !clients_map |> ClientsMap.add !current_id output ;
  let user = Users.init in
  let user = Users.set_username user login_packet.username in
  let user = Users.set_uuid user login_packet.uuid in
  users_map := !users_map |> UsersMap.add !current_id user;
  incr current_id



let handle_client (input, output) =
  let rec loop () =

    let* size = Lwt_io.read_int16 input in
    print_string "Receiving packet of size: ";
    print_int size;
    print_newline ();
    if size > (65_531) (* max packet length in bytes *) then
      let data, length = Packets.ClientBound.Error.write { error_type = Invalid_Length } in
      let* () = Lwt_io.write_from_exactly output data 0 length in
      let* () = Lwt_io.close input in
      Lwt_io.close output;
    else
      let bytes = Bytes.create size in
      let* () = Lwt_io.read_into_exactly input bytes 0 size in
      let packet_reader = PacketReader.empty bytes in
      match packet_reader.id with
        | Login -> login_player packet_reader.data output; loop()
          | _ -> let data, length = Packets.ClientBound.Error.write { error_type = Invalid_Length } in
          let* () = Lwt_io.write_from_exactly output data 0 length in
          let* () = Lwt_io.close input in
          Lwt_io.close output;
  in
  loop ()

let rec accept_connections server_socket =

  let* (client_socket, _addr) =
  Lwt_unix.accept server_socket in
  let input =
    Lwt_io.of_fd ~mode:Lwt_io.input client_socket in
  let output =
    Lwt_io.of_fd ~mode:Lwt_io.output client_socket in


  Lwt.async (fun () -> handle_client (input, output));
  accept_connections server_socket

let start_server port =
  let sockaddr =
    Unix.(ADDR_INET (inet_addr_any, port)) in
  let server_socket =
    Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in

  Lwt_unix.setsockopt
    server_socket Unix.SO_REUSEADDR true;
  let* () = Lwt_unix.bind server_socket sockaddr in
  Lwt_unix.listen server_socket 10;
  let* () =
    Lwt_io.printlf "Server started on port %d" port
  in
  accept_connections server_socket

let () =
  let port = 8080 in
  Lwt_main.run (start_server port)
