include Stdlib.Result

module O = struct
  let ( let* ) t f = bind t f

  let ( let+ ) t f = map f t
end
