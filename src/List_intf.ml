module type S = sig   
  
  module F : sig type ('a,'b) t end

  type ('a,'b) t = 
    | CNil : ('a,'a) t
    | CCons : (('a,'b) F.t * ('b,'c) t) -> ('a,'c) t
    
  include Seq.S with type ('a,'b) t := ('a,'b) t and module F := F

end

module type List = sig 
  module type S = S
  module Make(F : sig type ('a,'b) t end) : S  with module F := F
end