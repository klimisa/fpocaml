let ccr =
  let f = function x -> cos (x /. 2.0) in 
  function a-> let d = 8.0 *. f a in 
    function b -> let d = d *. f b in 
      function c -> let d = d *. f c in
        function s -> s /. d;;
