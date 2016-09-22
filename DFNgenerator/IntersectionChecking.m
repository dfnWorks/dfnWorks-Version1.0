(* ::Package:: *)

BeginPackage[ "IntersectionChecking`"]
Needs["MyCrossProduct`"];  


  CheckBoundingBox::usage = 
	"Checks if the Bounding Boxes of two polygons intersect, returns 0 if no, 1 if yes"

  LineOfIntersection::usage = 
	"Returns the Line of Intersection between two Planes"

  ReturnBoundingBox::usage = 
	"Returns the Bounding Box of the intersection between too planes"

  LineIntersection3D::usage =
	"Given two lines described by L1 = p1 + v1 and L2 = p2 + v2 that are known to intersect,
	 this returns the point where the two lines intersect. If the lines do not intersectct 
	 a vector of 0 is returned "

  PointOfLineIntersection::usage = 
	"Returns the points in R3 where two lines meet"

  distanceFromNodes::usage= 
	" Returns the minimun distance from the endpts of the line of intersection between two polygons and the vertices of the two polygons that intersect"

  FindPointsOnIntersection::usage=
	" Returns the points on teh edges of the polygons that intersect the infinite line of intersection,  if the polygons do not intersect output = {10000}"

  pointInPolygon::usage = 
	"For a given point and polygon, this returns 1 isf the point is in the polygon, 0 if not"

	
  minDistanceBetweenPolygons::usage = 
	" Finds the Distance between two parrallel Planes, If the Planes are not Parrallel It returns the min Distance between the vertices of the polygons"

  polyIntersection::usage = 
	"Checks that a polygon has at least n neighbors and the length of all of it's intersections is at least h"

  ConstructAdjanceyMatrix::usage = 
	"Returns an Adjancey Matrix Whose entires are the endpoints of the line of intersection between two planes"

  CheckForTripleIntersectionOnNew::usage = 
	"Given a list of the endpoints of line segments of intersection sections   occuring with a polygon ii, 
	this checks that no two line segments intersect one another on that polygon."

  pointOnLineSeg::usage = "*****"

  pointToLineSeg::usage = "*****"

  lineSegToLineSeg::usage = "*****"

  lineSegToLineSegSep::usage = "*****"

  findIntersections::usage = "*****"

  parallel::usage = "*****"

  CheckForTripleIntersectionOnExisting::usage = "*****"

  minDistanceBetweenpPoints::usage =
	" After the points on the line of intersection, the minimun distance between the points is calculated." 

  pointInBox::usage = 
	" Checks if a point is in the bounding box of a polygon "

  Begin[ "Private`"]


CheckBoundingBox = Compile[ {xLeft, xRight, yLeft, yRight, zLeft, zRight},
	(*Checks if the Bounding Boxes of two polygons intersect, returns 0 if no, 1 if yes*)
	intersectFlag = 0;

	If[ xRight >= xLeft && yRight >= yLeft && zRight >= zLeft, 
		intersectFlag = 1;
	];

	intersectFlag

, CompilationTarget -> "C"

]
(* CheckBoundingBox[poly1_, poly2_] = Block[ { xLeft, xRight, yLeft, yRight, zLeft, zRight, intersectFlag},
	(*Checks if the Bounding Boxes of two polygons intersect, returns 0 if no, 1 if yes*)
	intersectFlag = 0;

	xLeft  = Max[poly1[[4,1,1]], poly2[[4,1,1]]];
	xRight = Min[poly1[[4,1,2]], poly2[[4,1,2]]];

	yLeft  = Max[poly1[[4,2,1]], poly2[[4,2,1]]];
	yRight = Min[poly1[[4,2,2]], poly2[[4,2,2]]];

	zLeft  = Max[poly1[[4,3,1]], poly2[[4,3,1]]];
	zRight = Min[poly1[[4,3,2]], poly2[[4,3,2]]];

	If[ xRight >= xLeft && yRight >= yLeft && zRight >= zLeft, 
		intersectFlag = 1;
	];

	intersectFlag
] *)


findIntersections[poly1_, poly2_, eps_] := Block[ {flag, ii, kk, F1, F2, vertex1, inters, 
inters2, intersf, count, nVertices2, prevdist, currdist, s, o, endPoints, stdev},
(* Finds points on intersection using dot-product method *)
(* returns flag = {IntersectionSituation, EndPoints} *)

If[ CheckBoundingBox[Max[poly1[[4,1,1]], poly2[[4,1,1]]],Min[poly1[[4,1,2]], poly2[[4,1,2]]],
	Max[poly1[[4,2,1]], poly2[[4,2,1]]], Min[poly1[[4,2,2]], poly2[[4,2,2]]],
	Max[poly1[[4,3,1]], poly2[[4,3,1]]], Min[poly1[[4,3,2]], poly2[[4,3,2]]]] == 0, flag = {0,{}}
(* If[ CheckBoundingBox[poly1,poly2] == 0, flag = {0,{}} *)
, (* else, bounding boxes intersect *)

	(* find 4 cadidate intersection end points *)
	inters = ConstantArray[{0,0},2];
	For[ kk = 1, kk <= 2, kk++, 
		(* Print["This is loop number: "];
		Print[kk];
		Print["========================="]; *)
		(* if kk==1, finds projections of vertices of F2 to the plane of F1 (i.e. P1) *)
		If[kk == 1, F1 = poly1; F2 = poly2, F1 = poly2; F2 = poly1 ]; 

		count = 0; (* intersection point number *)
		vertex1 = F1[[3,1]]; (* one vertex on F1 *)
		inters2 = {0,0}; (* intersection points of F2 and P1 *)
		nVertices2 = F2[[1,2]]; (* number of vertices of F2 *)

		(* finds intersection point of F2 and P1 *)
			prevdist = N[Dot[ F2[[3,nVertices2]] - vertex1, F1[[2,1]] ]]; 
		For[ ii = 1, ii <= nVertices2, ii++, (* ii: current point *)
			(* vector of vertex1 to a vertex on F2 dots normal vector of F1
				its absolute value is the distance *)
			currdist = N[Dot[ F2[[3,ii]] - vertex1, F1[[2,1]] ]]; 
			(* Print[currdist]; *)
			If[ Abs[prevdist] < eps, 
				(* Print["Previous point happens to be on P1"]; *)
				count++;
				If[ ii == 1, inters2[[count]] = F2[[3,nVertices2]] (* previous point is intersection point *)
				, inters2[[count]] = F2[[3,ii-1]] ]
				(* Print[inters2[[count]]]; *)

			,
				If [currdist*prevdist < 0, (* if consecutive vertices of F2 are at opposide sides of P1 *)
					count++; 
					(* computes intersection point of F2 and P1 *)
					If[ ii == 1, inters2[[count]] = F2[[3,nVertices2]] + (F2[[3,1]] - F2[[3,nVertices2]])*Abs[prevdist]/(Abs[currdist]+Abs[prevdist])
				, inters2[[count]] = F2[[3,ii-1]] + (F2[[3,ii]] - F2[[3,ii-1]])*Abs[prevdist]/(Abs[currdist]+Abs[prevdist]) ]
					(* Print["Found two points on the oposite sides of fracture kk"];
					Print["value of count"];
					Print[count];
					Print[inters2[[count]]]; *)
				]
			];
			(* Print["intersection point"];
			Print[inters2[[count]]]; 
			Print["------------------"]; *)
			prevdist = currdist; 
			If[ count == 2, Break[] ] (* break loop when two intersection points are found *)
		]; 

		If[ count == 1, count = 2; inters2[[2]] = inters2[[1]] ]; (* if only one intersection points, happens only when a vertex of F2 is on P1 *)
		inters[[kk]] = inters2; (* intersection points on plane kk *)
		If[ count == 0, Break[] ] (* if F2 and P1 don't intersect, break the loop *)
	]; 
	If[ count == 0, flag = {0,{}} (* fractures do not intersect *)
	, (* else, 4 intersection points exist *)
(* 		If[ count > 2, Print["Error in check intersection"] ]; (* an error check *) *)

		intersf = Flatten[inters,1]; (* Flatten[intersectionPts] is an 1*4 array *)

		(* prevents finite precision problem, e.g. vertical intersection *)
		stdev = StandardDeviation[intersf];
		o = Ordering[stdev,-1];
		kk = o[[1]];

		s = Ordering[ intersf[[All,kk]] ]; 
		If[ !( s[[1]]+s[[2]] == 1+2 || s[[1]]+s[[2]] == 3+4 ), (* if the smallest two points are not on the bdy of the same poly *)
			(* the polygons intersect; middle two points form intersetion *)
			endPoints = {intersf[[ s[[2]] ]], intersf[[ s[[3]] ]]};
			endPoints = Threshold[endPoints,{"Hard",eps}]; (* if less than eps, make it 0 *)

			Which[ s[[2]]+s[[3]] == 1+2 , flag = {1,endPoints} (* if intersection is inside poly1 *)
			, s[[2]]+s[[3]] == 3+4 , flag = {2,endPoints} (* if intersection is inside poly2 *)
			, True, flag = {3,endPoints} (* else, intersection on both polys' edges *)
			];
		, (* else, intersection doesn't exist *)
			flag = flag = {0,{}}
		] (* end if intersection exists *)
	] (* end if 4 intersection points exists *)
]; (* end if bounding boxes intersect *)

flag
]


LineIntersection3D[point1_,vector1_,point2_, vector2_] := Block[{ p1 = point1,v1 = vector1,p2 = point2,v2 = vector2, 
v1xv2, v21, intPoint, denom,v21xv2}, 
(* LineIntersection3D = Compile[ {p1x, p1y, p1z, v1x, v1y, v1z, p2x, p2y, p2z, v2x, v2y, v2z}, *)
(* Given two lines described by two points and two vectors, this module returns ths point of intersection between the two lines *)
(* assume two line segs not parallel *)

(* v1 = {v1x, v1y, v1z};
v2 = {v2x, v2y, v2z};
p1 = {p1x, p1y, p1z};
p2 = {p2x, p2y, p2z}; *)
v1 = Normalize[v1]; 
v2 = Normalize[v2]; 

(* v1xv2=Cross[v1,v2]; *) 
v1xv2 = { v1[[2]]*v2[[3]] - v1[[3]]*v2[[2]], v1[[3]]*v2[[1]]-v1[[1]]*v2[[3]], v1[[1]]*v2[[2]]- v1[[2]]*v2[[1]]}; 
denom=v1xv2.v1xv2; 
v21=p2-p1; 
(* v21 cross v2 *) 
v21xv2 = { v21[[2]]*v2[[3]] - v21[[3]]*v2[[2]], v21[[3]]*v2[[1]]-v21[[1]]*v2[[3]], v21[[1]]*v2[[2]]- v21[[2]]*v2[[1]]}; 
intPoint = N[p1 + v1*v21xv2.v1xv2/denom];

intPoint
]


pointOnLineSeg[pt_,endpts_] := Block[{points,o,kk,s,flag,stdev},
(* this function check if pt is on line segment endpoints, preventing vertical alignment *)
(* NOTE: pt has to be on the infinite line of endpts *)

points = {pt,endpts[[1]],endpts[[2]]}; (* list of points on AB *)
(* prevents finite precision problem, e.g. vertical intersection *)
stdev = StandardDeviation[points];
o = Ordering[stdev,-1];
kk = o[[1]]; (* the coordinate with max stdev *)
tmp = points[[All,kk]];
s = Ordering[ points[[All,kk]] ]; 

(* Print[points," ",stdev," ",o," ",kk," ",tmp," ",s]; *)
If[ s[[2]] == 1, flag = 1, flag = 0 ]; (* if the one ranked in middle is pt, pt is on the line seg *)

flag
]


pointToLineSeg[pt_,endpts_] := Block[{AB, PA, PH, H, points, dist, kk, stmp, h, n},
AB = endpts[[2]] - endpts[[1]];
PA = endpts[[1]] - pt;
n = MyCross[PA,AB];
h = MyCross[AB,n]; (* a vector with the same direction as PH *)
PH = Projection[PA,h];
H = pt + PH; (* perpendicular foot of point P on line segment AB *)

If[ pointOnLineSeg[H,endpts] == 1, dist = Norm[PH], dist = Min[Norm[PA],Norm[endpts[[2]]-pt]] ]; 
(* if H is on AB, dist = Norm[PH], else, dist = Min[PA,PB] *)

dist
]


lineSegToLineSegSep[line1_,line2_] := Block[{dist,k},
(* NOTE: only works for separated line segs (two line segs don't intersect) *)
dist = Min[ pointToLineSeg[line1[[1]],line2] , pointToLineSeg[line1[[2]],line2] ];
For[k = 1, k <= 2, k++,
	dist = Min[ dist, pointToLineSeg[line2[[k]],line1] ]
];

dist
]


parallel[v1_,v2_,eps_] := Block[{u1,u2,dotproduct,ret},

u1 = N[Normalize[v1]];
u2 = N[Normalize[v2]];

dotproduct = Abs[u1.u2];
ret = If[ 1-eps < dotproduct && dotproduct < 1+eps, 1, 0 ];

ret
]


lineSegToLineSeg[line1_,line2_,eps_] := Block[{dist,k,p1,v1,pt,p2,v2,dotproduct},

(*Check if line 1 and line 2 intersect*)
p1 = line1[[1]];
p2 = line2[[1]];
v1 = line1[[1]]-line1[[2]];
v2 = line2[[1]]-line2[[2]];

If[ parallel[v1,v2,eps] == 1, (* two line segs in parallel *)
	If[ Norm[p1-p2]<eps || parallel[p1-p2,v1,eps] == 1,  (* if two line segs are colinear *)
		If[ pointOnLineSeg[line1[[1]],line2] == 1 || pointOnLineSeg[line1[[2]],line2] == 1, (* if two line segs overlap *)
			dist = 0
		, (* else, two line segs colinear but not overlap *)
			dist = lineSegToLineSegSep[line1,line2]
		]
	, (* else, two line segs parallel but not overlap *)
		dist = lineSegToLineSegSep[line1,line2]
	]
, (* else, two line segs not parallel *)			
	pt = LineIntersection3D[p1,v1,p2,v2]; 
	(* pt = LineIntersection3D[p1[[1]],p1[[2]],p1[[3]],v1[[1]],v1[[2]],v1[[3]],p2[[1]],p2[[2]],p2[[3]],v2[[1]],v2[[2]],v2[[3]]]; *)

	If[ pointOnLineSeg[pt,line1]==1 && pointOnLineSeg[pt,line2]==1, (*Case 1: Lines Intersection occurs on the lines*)
		dist = 0
	, (* Case 2: Line Intersection does not occur on both lines, find min distance from 4 endpoints to other line seg *)
		dist = lineSegToLineSegSep[line1,line2]
	]
];

dist
]


distanceFromNodes[poly1_, poly2_, pts_, h_] := Block[{ endpts = pts,i,j,dist,nNodes, x0, x1, x2,d}, 
	(*This module returns the minimun distance between the of line segment of intersections (pts) and the vertices of the polygons that intersect *)

	dist = 10000*h;
	x1 = endpts[[1]];
	x2 = endpts[[2]];

	nNodes = poly1[[1,2]];
	For[ i = 1, i <= nNodes, i++,
		x0 = poly1[[3,i]];
		dist = Min[dist,pointToLineSeg[x0,endpts]];
		If[dist <= h, Break[] ]
	];

	nNodes = poly2[[1,2]];
	For[ i = 1, i <= nNodes, i++,
		x0 = poly2[[3,i]];
		dist = Min[dist,pointToLineSeg[x0,endpts]];
		If[dist <= h, Break[] ]
	];

	dist
]


CheckForTripleIntersectionOnExisting[list_, endpts_, h_, eps_] := Block[{flag,dist,ii,n,line},
(* checks the distance between the new intersection and the existing intersections on an existing fracture *)
(* returns the third fracture involved in the tirple intersection *)

flag = 0;

If[ Length[list] > 0, 

n = Length[list[[1,1]]];
For[ ii = 1, ii <= n, ii ++,
	line = {{list[[1,1,ii]],list[[1,2,ii]],list[[1,3,ii]]},{list[[2,1,ii]],list[[2,2,ii]],list[[2,3,ii]]}};
	dist = lineSegToLineSeg[endpts,line,eps];
	If[ dist <= h, flag = ii; Break[] ]
]

];

flag
]


CheckForTripleIntersectionOnNew[list_, length_, eps_] := Block[{ A = list,  h = length, n, flag, line1, line2, i,j,k, p1, v1, p2, v2, pt, v, t1, t2, dist },
(* checks distance between any two intersections on the new fracture *)

flag = {0,0};

If[ Length[list] > 0, 

n = Length[list[[1,1]]];
For[ ii = 1, ii <= n && flag == {0,0}, ii++,
	For[ jj = 1, jj < ii, jj++, 
		line1 = {{list[[1,1,ii]],list[[1,2,ii]],list[[1,3,ii]]},{list[[2,1,ii]],list[[2,2,ii]],list[[2,3,ii]]}};
		line2 = {{list[[1,1,jj]],list[[1,2,jj]],list[[1,3,jj]]},{list[[2,1,jj]],list[[2,2,jj]],list[[2,3,jj]]}};
		dist = lineSegToLineSeg[line1,line2,eps];
		If[ dist <= h, flag = {ii,jj}; Break[] ]
	]
]

];

flag
]


pointInPolygon[pt_, poly_,eps_] := Block[{nNodes,inside,lastedge,ZP,lastcross,kk,i,curredge,CP,currcross},
(* determine if P is in poly using same sides check *) (* only works for convex polygons *)
nNodes = poly[[1,2]];
inside = 1;
lastedge = poly[[3,1]] - poly[[3,nNodes]]; (* vector of the last edge *)
ZP = pt - poly[[3,nNodes]]; (* vector from the last vertex to P *)
If[ Dot[ZP, poly[[2,1]] ] < eps, (* if ZP dot normal is zero, P is on the plane of poly *)
	lastcross = MyCross[lastedge, ZP]; (* cross product of the last edge and ZP *)
	kk = Ordering[Abs[lastcross],-1][[1]]; (* find the largest component of last cross *)
	For[ i = 1, i < nNodes, i++, 
		curredge = poly[[3,i+1]] - poly[[3,i]]; 
		CP = pt - poly[[3,i]]; (* vector from current vertex to P *)
		currcross = MyCross[curredge, CP];
		If[ Norm[currcross] < eps, inside = 1; Break[] ]; (* on the boundary *)
		(* if makes it here, not on the boundary *)
		If[ lastcross[[kk]]*currcross[[kk]] < 0, inside = 0; Break[] ] (* P is not on the same side of the edges *)
	]
];

inside
]


  End[]

  EndPackage[]


