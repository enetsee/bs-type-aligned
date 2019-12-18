include RTQueue_intf

module Make(F: sig type ('a,'b) t end) = struct
  
  module T = struct

    module F = F 
      
    module ConsList = List.Make(F)
      
    module SnocList = SnocList.Make(F)
      
    type ('a,'b) t = 
      | RQ : (('a,'b) ConsList.t * ('b,'c) SnocList.t * ('x,'b) ConsList.t) -> ('a,'c) t
      
    type ('a,'b) l = 
      | EmptyL : ('a,'a) l
      | ConsL : (('a,'b) F.t * ('b,'c) t) -> ('a,'c) l
        
    type ('a,'b) r = 
      | EmptyR : ('a,'a) r
      | ConsR : (('a,'b) t * ('b,'c) F.t) ->  ('a,'c) r
    
    let rec rotate : type a b c d. (a,b) ConsList.t -> (b,c) SnocList.t -> (c,d) ConsList.t -> (a,d) ConsList.t = fun f a r -> 
      match f,a,r  with
      | ConsList.CNil, SnocList.(Snoc(SNil,y)), _ -> ConsList.cons y r 
      | ConsList.CCons(x,f),SnocList.Snoc(r,y),a -> ConsList.cons x (rotate f r @@ ConsList.cons y a)
      | _ , _ , _ -> failwith "can't happen"
      
    let revAppend l r = rotate l r ConsList.empty
      
    let empty = RQ (ConsList.empty,SnocList.empty,ConsList.empty)
      
    let queue : type a b c x. (a,b) ConsList.t -> (b,c) SnocList.t -> (x,b) ConsList.t -> (a,c) t = fun f r a -> 
      match a with 
      | ConsList.CNil -> 
        let f' = revAppend f r in RQ(f',SnocList.empty,f')
      | ConsList.CCons(h,t) -> RQ(f,r,t)
      
    let singleton  x = 
      let c = ConsList.singleton x in
      queue c SnocList.empty c 
        
    let snoc (RQ(f,r,a)) x = queue f SnocList.(Snoc(r,x)) a
      
    let consOrSnoc = `Snoc snoc

    let viewLeft : type a b. (a,b) t -> (a,b) l = function
      | RQ (ConsList.CNil,SnocList.SNil,ConsList.CNil) -> EmptyL
      | RQ (ConsList.CCons(h,t),f,a) -> ConsL(h,queue t f a)
      | _ -> failwith "cannot view left"

    let view  = `Left viewLeft
    
  end

  include T

  include Seq.Make(T) 

end
