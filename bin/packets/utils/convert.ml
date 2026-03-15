open Types

let error_to_id = function
  (* match __ with *)
  | Handshake_Fail -> 0x00
  | Invalid_Length -> 0x01
  | Ping_Uncomplete -> 0x02
  | Unexpected -> 0x03

let packet_type_to_id = function
  | Error -> 0x00
  | Login -> 0x01
  | Location -> 0x02
  | Invalid -> -1

let packet_type_from_id = function
  | 0x00 -> Error
  | 0x01 -> Login
  | _ -> Invalid
