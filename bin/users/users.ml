module Location = Locations
module Inventory = Inventory

type t = {
  username : string;
  uuid : Bytes.t;
  inventory : (Inventory.inventory_items * int) list;
  position : float * float;
  location : Locations.location;
}

let init =
  {
    username = "steve";
    uuid = Bytes.empty;
    inventory = [];
    position = (0., 0.);
    location = Locations.Spawn;
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
