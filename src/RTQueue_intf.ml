  module type S = sig

    module F : sig type ('a,'b) t end

    module ConsList : List.S with module F := F 

    module SnocList : SnocList.S with module F := F
    
    type ('a,'b) t = 
      | RQ : (('a,'b) ConsList.t * ('b,'c) SnocList.t * ('x,'b) ConsList.t) -> ('a,'c) t
     
    val rotate : ('a,'b) ConsList.t -> ('b,'c) SnocList.t -> ('c,'d) ConsList.t -> ('a,'d) ConsList.t
    
    val revAppend : ('a,'b) ConsList.t -> ('b,'c) SnocList.t -> ('a,'c) ConsList.t
    
    val queue : ('a,'b) ConsList.t -> ('b,'c) SnocList.t -> ('x,'b) ConsList.t -> ('a,'c) t
    
    include Seq.S with type ('a,'b) t := ('a,'b) t and module F := F    
    
end

module type RTQueue = sig 
  module type S = S 
  module Make(F : sig type ('a,'b) t end) : S with module F = F 
end