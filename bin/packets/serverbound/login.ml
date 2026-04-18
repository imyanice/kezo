type packet = { username : string; uuid : Bytes.t }

let read bytes = { username = Bytes.sub_string bytes 0 17; uuid = Bytes.empty }
(* need proper string encoding *)
