module OrderedType = {
  type t = string;
  let compare = String.compare;
};

include OrderedType;

module Map = Map.Make(OrderedType);
