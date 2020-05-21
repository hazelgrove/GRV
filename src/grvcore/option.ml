module Base_Option = Option
include Base_Option

module Let_syntax = struct
  module Let_syntax = struct
    let map ~(f : 'a -> 'b) (o : 'a option) : 'b option = Base_Option.map f o

    let bind (o : 'a option) ~(f : 'a -> 'b option) : 'b option =
      Base_Option.bind o f
  end
end
