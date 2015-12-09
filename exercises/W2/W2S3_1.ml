type point  = { x : float; y : float; z : float };;
type dpoint = { dx : float; dy : float; dz : float };;
type physical_object = { position : point; velocity : dpoint };;

let move p dp =
	{x = p.x +. dp.dx; y = p.y +. dp.dy; z = p.z +. dp.dz };;
	
let next obj =
  {position = move obj.position obj.velocity; velocity = obj.velocity} ;;
  
let will_collide_soon p1 p2 =
	let radious = 1.0 in
		let addition_of_radious = radious +. radious in
			let { position = { x ; y ; z } } = next p1 in
				let { position = { x = x' ; y = y' ; z = z' } } = next p2 in
			 		let distance_between_2points = sqrt((x-.x') ** 2.0 +. (y -.y') ** 2.0 +. (z-.z') ** 2.0) 
						in (distance_between_2points < addition_of_radious) ;;
