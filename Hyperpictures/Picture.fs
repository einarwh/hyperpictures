module Picture

open Shape
open Styling
open Lens

type Picture = Lens -> (Shape * Style) list

let blank : Picture = fun _ -> []

let turn (p : Picture) = turnLens >> p 

let flip (p : Picture) = flipLens >> p 

let toss (p : Picture) = tossLens >> p 

let aboveRatio (m : int) (n : int) (p1 : Picture) (p2 : Picture) = 
  fun lens ->
    let factor = float m / float (m + n) 
    let b1, b2 = splitLensVertically factor lens
    p1 b1 @ p2 b2

let above = aboveRatio 1 1

let besideRatio (m : int) (n : int) (p1 : Picture) (p2 : Picture) =
  fun lens ->
    let factor = float m / float (m + n) 
    let b1, b2 = splitLensHorizontally factor lens
    p1 b1 @ p2 b2

let beside = besideRatio 1 1

let rec row = function
  | [] -> blank
  | [p] -> p
  | [p1; p2] -> beside p1 p2
  | p :: rest -> besideRatio 1 (List.length rest) p (row rest) 

let rec column = function
  | [] -> blank
  | [p] -> p
  | [p1; p2] -> above p1 p2
  | p :: rest -> aboveRatio 1 (List.length rest) p (row rest) 
   
let quartet nw ne sw se = 
  above (beside nw ne) (beside sw se)

let selfQuartet p = 
  quartet p p p p

let nonet nw nm ne mw mm me sw sm se =
  let row w m e = besideRatio 1 2 w (beside m e)
  aboveRatio 1 2
    (row nw nm ne)
    (above 
      (row mw mm me)
      (row sw sm se))

let selfNonet p = 
  nonet p p p p p p p p p

let zoom n nw nm ne mw mm me sw sm se =
  let rec aux i = 
    if i > 0 then nonet nw nm ne mw (aux (i - 1)) me sw sm se
    else mm
  aux n

let over p1 p2 = 
  fun lens ->
    p1 lens @ p2 lens

let rehue p =
  rehueLens >> p

let ttile hueN hueE fish = 
  let fishN = fish |> toss |> flip
  let fishE = fishN |> turn |> turn |> turn
  over fish (over (hueN fishN) (hueE fishE))

let ttile1 = ttile rehue (rehue >> rehue)

let ttile2 = ttile (rehue >> rehue) rehue

let utile hueN hueW hueS hueE fish = 
  let fishN = fish |> toss |> flip
  let fishW = turn fishN
  let fishS = turn fishW
  let fishE = turn fishS
  over (over (hueN fishN) (hueW fishW))
       (over (hueS fishS) (hueE fishE))

let utile1 = utile (rehue >> rehue) id (rehue >> rehue) id
let utile2 = utile id (rehue >> rehue) rehue (rehue >> rehue)
let utile3 = utile (rehue >> rehue) id rehue id

let side tt hueSW hueSE n fish = 
  let t = tt fish
  let rec recur n = 
    let r = if n = 1 then blank else recur (n - 1)
    quartet r r (t |> turn |> hueSW) (t |> hueSE)
  if n < 1 then blank else recur n

let sideNS = side ttile1 id rehue

let sideEW = side ttile2 (rehue >> rehue) rehue

let corner ut side1 side2 n fish = 
  let u = ut fish
  let rec fn n = 
    let c, ne, sw = 
      if n = 1 then blank, blank, blank 
               else fn (n - 1), side1 (n - 1) fish, side2 (n - 1) fish
    quartet c ne (sw |> turn) u
  if n < 1 then blank else fn n

let cornerNWSE = corner utile3 sideNS sideEW
let cornerNESW = corner utile2 sideEW sideNS

let squareLimit n fish = 
  let cornerNW = cornerNWSE n fish
  let cornerSW = cornerNESW n fish |> turn
  let cornerSE = cornerNW |> turn |> turn
  let cornerNE = cornerSW |> turn |> turn
  let sideN = sideNS n fish
  let sideW = sideEW n fish |> turn
  let sideS = sideN |> turn |> turn
  let sideE = sideW |> turn |> turn
  let center = utile1 fish
  nonet cornerNW sideN cornerNE  
        sideW center sideE
        cornerSW sideS cornerSE
        
let rec tunnelCorner n fish = 
  if n < 1 then
    blank 
  else  
    let nw = over fish (fish |> rehue |> turn |> turn)
    // let tossed = fish |> toss |> flip 
    let fishN = fish |> toss |> flip
    let fishE = fishN |> turn |> turn |> turn
    let ne = (fish |> rehue |> rehue) |> over (fishN |> rehue) |> over fishE |> turn |> turn |> turn
    let sw = ne
    let se = tunnelCorner (n - 1) fish
    quartet nw ne sw se  

let tunnel n fish = 
  let nw = tunnelCorner n fish 
  let sw = nw |> turn 
  let se = sw |> turn 
  let ne = se |> turn 
  quartet nw ne sw se 