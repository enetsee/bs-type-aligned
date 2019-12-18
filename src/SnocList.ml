include SnocList_intf

module Make(F : sig type ('a,'b) t end) : S 
  with module F := F = 
struct

  module T = struct
    type ('a,'b) t = 
      | SNil : ('a,'a) t
      | Snoc : (('a,'b) t * ('b,'c) F.t) -> ('a,'c) t
      
    module F = F

    type ('a,'b) l = 
      | EmptyL : ('a,'a) l
      | ConsL : (('a,'b) F.t * ('b,'c) t) -> ('a,'c) l
      
    type ('a,'b) r = 
      | EmptyR : ('a,'a) r
      | ConsR : (('a,'b) t * ('b,'c) F.t) ->  ('a,'c) r
    
    let empty = SNil

    let singleton x = Snoc(SNil,x)

    let snoc xs x  =  Snoc(xs,x)

    let consOrSnoc = `Snoc snoc
    
    let viewRight : type a b. (a,b) t -> (a,b) r = function 
      | SNil ->  EmptyR
      | Snoc(rest,next) -> ConsR(rest,next)

    let view = `Right viewRight
      
  end

  include T

  include Seq.Make(T)

end