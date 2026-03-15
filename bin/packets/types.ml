type packets_type = Error | Login | Location | Invalid

type error_type =
  | Invalid_Length
  | Handshake_Fail
  | Ping_Uncomplete
  | Unexpected

exception Invalid_Length
exception Unexpected
