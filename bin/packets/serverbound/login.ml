type packet = { username : string; uuid : string }

let read bytes =
  {
    username = Bytes.sub_string bytes 0 16;
    uuid = Bytes.sub_string bytes 16 32;
  }
(* need proper string encoding *)
