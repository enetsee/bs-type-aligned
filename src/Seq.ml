include Seq_intf

module Make(X:Minimal) : S 
  with type ('a,'b) t := ('a,'b) X.t 
  and module  F := X.F 
  and type ('a,'b) l := ('a,'b) X.l 
  and type ('a,'b) r := ('a,'b) X.r = 
struct

  let empty = X.empty 
    
  let singleton = X.singleton     
    
  let rec append : type a b c. (a,b) X.t -> (b,c) X.t -> (a,c) X.t = fun xs ys ->
    match viewLeft xs with 
    | X.EmptyL -> ys 
    | X.ConsL(next,rest)  -> 
        cons next (append rest ys )
  
  and cons : 'a 'b 'c. ('a,'b) X.F.t -> ('b,'c) X.t -> ('a,'c) X.t = 
    fun l r -> 
      match X.consOrSnoc with 
      | `Cons f -> f l r 
      | _ -> append (singleton l) r 
          
  and snoc : type a b c. (a,b) X.t -> (b,c) X.F.t -> (a,c) X.t  = 
    fun l r -> 
      match X.consOrSnoc with 
      | `Snoc f -> f l r 
      | _ -> append l (singleton r)
    
  and viewLeft : type a b. (a,b) X.t -> (a,b) X.l = fun xs ->       
    match X.view with 
    | `Left f -> f xs
    | _ -> 
      match viewRight xs with
      | X.EmptyR -> EmptyL
      | X.ConsR(rest,next) -> (
        match viewLeft rest with
        | X.EmptyL -> ConsL(next,empty)
        | X.ConsL(h,t) -> ConsL(h, snoc t next)
        ) 
                    
  and viewRight : type a b. (a,b) X.t -> (a,b) X.r = fun xs -> 
    match X.view with 
    | `Right f -> f xs
    | _ ->       
      match viewLeft xs with
      | X.EmptyL -> EmptyR      
      | X.ConsL(next,rest) -> (
        match viewRight rest with
        | X.EmptyR -> ConsR(empty,next)
        | X.ConsR(next',rest') -> ConsR( cons next next' , rest')
      )

end