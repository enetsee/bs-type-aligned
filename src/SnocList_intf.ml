module type S = sig

  module F : sig type ('a,'b) t end

  type ('a,'b) t = 
    | SNil : ('a,'a) t
    | Snoc : (('a,'b) t * ('b,'c) F.t) -> ('a,'c) t
      
  include Seq.S with type ('a,'b) t := ('a,'b) t and module F := F

end
  
module type SnocList = sig 
  module type S = S
  module Make(F : sig type ('a,'b) t end) : S with module F := F
end