include List_intf

module Make(F : sig type ('a,'b) t end) : S  
  with module F := F = 
struct

  module T = struct
    type ('a,'b) t = 
      | CNil : ('a,'a) t
      | CCons : (('a,'b) F.t * ('b,'c) t) -> ('a,'c) t
      
    module F = F

    type ('a,'b) l = 
      | EmptyL : ('a,'a) l
      | ConsL : (('a,'b) F.t * ('b,'c) t) -> ('a,'c) l
    
    
    type ('a,'b) r = 
      | EmptyR : ('a,'a) r
      | ConsR : (('a,'b) t * ('b,'c) F.t) ->  ('a,'c) r
      
    let empty = CNil

    let singleton x = CCons(x,CNil)

    let cons x xs =  CCons(x,xs)
    
    let viewLeft : type a b. (a,b) t -> (a,b) l = function 
      | CNil ->  EmptyL
      | CCons(next,rest) -> ConsL(next,rest)
      
    let consOrSnoc = `Cons cons 
    
    let view = `Left viewLeft
      
  end

  include T    
  
  include Seq.Make(T)

end