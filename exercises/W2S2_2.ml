let pairwise_distinct (lup, rup, llp, rlp) =
  not (lup = rup || lup = llp || lup = rlp || rup = llp || rup = rlp || llp = rlp) ;;

let wellformed (lup, rup, llp, rlp) =
  let (lup_x, lup_y)  = lup 
  and (rup_x, rup_y)  = rup 
  and (llp_x, llp_y)  = llp 
  and (rlp_x, rlp_y)  = rlp  in
  (lup_x < rup_x) && (llp_x < rlp_x) &&  (lup_y > llp_y) && (rup_y > rlp_y);;

let rotate_point (x, y) = (y, -x);;

let reorder (p1, p2, p3, p4) = 
  let upper n1 n2 = let (_, y1) = n1  and (_, y2) = n2 in if y1 > y2 then n1 else n2 and
  left  n1 n2 =	let (x1, _) = n1 and (x2, _) = n2 in if x1 < x2 then n1 else n2 and
  right n1 n2 = let (x1, _) = n1  and (x2, _) = n2 in if x1 > x2 then n1 else n2 and
  lower n1 n2 = let (_, y1) = n1  and (_, y2) = n2 in if y1 < y2 then n1 else n2
  in let 
    up1 = upper (upper (upper p1 p2) p3) p4
  in let upper_exc_up1 p1 p2 = if p1 = up1 then p2 else if p2 = up1 then p1 else upper p1 p2 
  in let up2 = upper_exc_up1 (upper_exc_up1 (upper_exc_up1 p1 p2) p3) p4 
  and   
    lo1 = lower (lower (lower p1 p2) p3) p4 
  in let upper_exc_lo1 p1 p2 = if p1 = lo1 then p2 else if p2 = lo1 then p1 else lower p1 p2 
  in let lo2 = upper_exc_lo1 (upper_exc_lo1 (upper_exc_lo1 p1 p2) p3) p4 
  in let 
  		lup = left up1 up2 and rup = right up1 up2 and llp = left lo1 lo2 and rlp = right lo1 lo2
		in (lup, rup, llp, rlp);;


let rotate_tetragon (lup, rup, llp, rlp) =
  reorder (rotate_point lup, rotate_point rup, rotate_point llp, rotate_point rlp);;

