type figure =
    Point
  | Circle of int
  | Rectangle of int * int
  | Square of int

type 'a with_location = {loc_x: float; loc_y: float; body: 'a}

(*以上デバッグ用*)

let point_of_rectangle fig = 
  match fig.body with 
    Rectangle (x,y)->
    ({loc_x=fig.loc_x -. float x /. 2.; loc_y= fig.loc_y -. float y /. 2.;body=Point},
     {loc_x=fig.loc_x -. float x /. 2.; loc_y= fig.loc_y +. float y /. 2.;body=Point},
     {loc_x=fig.loc_x +. float x /. 2.; loc_y= fig.loc_y -. float y /. 2.;body=Point},
     {loc_x=fig.loc_x +. float x /. 2.; loc_y= fig.loc_y +. float y /. 2.;body=Point})
  | _ ->raise (Sys_error "到達不能箇所_")

let overlap_point fig1 fig2 =
  match fig2.body with
    Point -> (fig1.loc_x = fig2.loc_x) && (fig1.loc_y = fig2.loc_y)
  | Circle r -> (fig2.loc_x -. fig1.loc_x) ** 2.0 +. (fig2.loc_y -. fig1.loc_y) ** 2.0 <= float_of_int (r * r)
  | Rectangle (x,y) ->  (fig2.loc_x -. float x /. 2.0 <= fig1.loc_x) &&
                        (fig2.loc_x +. float x /. 2.0 >= fig1.loc_x) &&
                        (fig2.loc_y -. float y /. 2.0 <= fig1.loc_y) &&
                        (fig2.loc_y +. float y /. 2.0 >= fig1.loc_y)
  | Square z -> raise (Sys_error "到達不能箇所P")

let overlap_circle fig1 fig2 =
  match (fig1.body,fig2.body) with
    (Circle r1,Circle r2) -> (fig1.loc_x -. fig2.loc_x) ** 2.0 +. (fig1.loc_y -. fig2.loc_y) ** 2.0 <= float_of_int (2 * (r1 + r2))
  | (Circle r,Rectangle (x,y)) -> 
    let (c1, c2, c3, c4) = point_of_rectangle fig2 
    and rr = {loc_x=fig1.loc_x; loc_y=fig1.loc_y; body=Point} in
    overlap_point c1 fig1 || overlap_point c2 fig1 || overlap_point c3 fig1 || overlap_point c4 fig1 ||
    overlap_point rr fig2
  | _ -> raise (Sys_error "到達不能箇所C")

let overlap_rectangle fig1 fig2 =
  match (fig1.body,fig2.body) with
  | (Rectangle (x1,y1),Rectangle (x2,y2)) -> 
    let (c11, c12, c13, c14) = point_of_rectangle fig1 
    and (c21, c22, c23, c24) = point_of_rectangle fig2 in
    overlap_point c11 fig2 || overlap_point c12 fig2 || overlap_point c13 fig2 || overlap_point c14 fig2 ||
    overlap_point c21 fig1 || overlap_point c22 fig1 || overlap_point c23 fig1 || overlap_point c24 fig1 
  | _ -> raise (Sys_error "到達不能箇所R")

let overlap fig1 fig2 = 
  match (fig1.body,fig2.body) with
    (Point,Square z) -> overlap_point fig1 {loc_x=fig2.loc_x; loc_y=fig2.loc_y; body=Rectangle(z,z)}
  | (Point,_) -> overlap_point fig1 fig2 
  | (Circle _,Point) -> overlap_point fig2 fig1 
  | (Circle _,_) -> overlap_circle fig1 fig2
  | (Rectangle _,Point) -> overlap_point fig2 fig1
  | (Rectangle _,Circle _) -> overlap_circle fig2 fig1
  | (Rectangle _,Square z) -> overlap_rectangle fig1 {loc_x=fig2.loc_x; loc_y=fig2.loc_y; body=Rectangle(z,z)}
  | (Rectangle _,_) -> overlap_rectangle fig1 fig2
  | (Square z,Point) -> overlap_point fig2 {loc_x=fig2.loc_x; loc_y=fig2.loc_y; body=Rectangle(z,z)} 
  | (Square z,Circle _) -> overlap_circle fig2 {loc_x=fig2.loc_x; loc_y=fig2.loc_y; body=Rectangle(z,z)}
  | (Square z,Rectangle _) -> overlap_rectangle fig2 {loc_x=fig2.loc_x; loc_y=fig2.loc_y; body=Rectangle(z,z)}
  | (Square z1,Square z2) -> overlap_rectangle {loc_x=fig1.loc_x; loc_y=fig1.loc_y; body=Rectangle(z1,z1)} 
                               {loc_x=fig2.loc_x; loc_y=fig2.loc_y; body=Rectangle(z2,z2)}