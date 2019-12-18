
module type Minimal = sig  

  module F : sig type ('a,'b) t end 
  
  type ('a,'b) t

  type ('a,'b) l = 
    | EmptyL : ('a,'a) l
    | ConsL : (('a,'b) F.t * ('b,'c) t) -> ('a,'c) l
    
  type ('a,'b) r = 
    | EmptyR : ('a,'a) r
    | ConsR : (('a,'b) t * ('b,'c) F.t) ->  ('a,'c) r
        
  val empty : ('a,'a) t 
  
  val singleton : ('a,'b) F.t -> ('a,'b) t
  
  val consOrSnoc : 
    [ 
    | `Cons of ('a,'b) F.t -> ('b,'c) t -> ('a,'c) t 
    | `Snoc of ('a,'b) t -> ('b,'c) F.t -> ('a,'c) t
    ]
          
  val view:
    [ 
    | `Left of ('a,'b) t -> ('a,'b) l
    | `Right of ('a,'b) t ->('a,'b) r
    ]

end


module type S = sig 
  
  module F : sig type ('a,'b) t end 
    
  type ('a,'b) t

  type ('a,'b) l = 
    | EmptyL : ('a,'a) l
    | ConsL : (('a,'b) F.t * ('b,'c) t) -> ('a,'c) l
      
  type ('a,'b) r = 
    | EmptyR : ('a,'a) r
    | ConsR : (('a,'b) t * ('b,'c) F.t) ->  ('a,'c) r
            
  val empty : ('a,'a) t 
    
  val singleton : ('a,'b) F.t -> ('a,'b) t
    
  val cons : ('a,'b) F.t -> ('b,'c) t -> ('a,'c) t 
  val snoc : ('a,'b) t -> ('b,'c) F.t -> ('a,'c) t
            
  val viewLeft : ('a,'b) t -> ('a,'b) l
  val viewRight : ('a,'b) t ->('a,'b) r
    
end


module type Seq = sig 
  
  module type Minimal = Minimal 
  
  module type S = S 
  
  module Make(X:Minimal) : S 
    with type ('a,'b) t := ('a,'b) X.t 
    and  module F := X.F 
    and  type ('a,'b) l := ('a,'b) X.l 
    and  type ('a,'b) r := ('a,'b) X.r

end