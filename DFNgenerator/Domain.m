(* ::Package:: *)

BeginPackage[ "domain`"]
 
 Needs["IntersectionChecking`"]; 
 Needs["MyCrossProduct`"];  


  GenerateBoundaryBox::usage = 
	"Creates an array that described the cube that is the bounding box"

  FindNodesNextToIntersection::usage = 
	"Returns the vertices that make up the line that the point of intersection is on. "

  InsideDomain::usage = 
	" If Node 1 is inside of the domain, this module is used to recontruct the polygon"

   OutsideDomain::usage = 
	" If Node 1 is outside of the domain, this module is used to recontruct the polygon"
 
  DomainTruncation::usage = 
	"Check the polygon, ii, for intersection with the bounding box"

  FindLineOfIntersection::usage = 
	" Finds the line of Intersection between the polygon and the bounding domain wall"
 
  Begin[ "private`"]

  GenerateBoundaryBox[domain_] := Block[{d = domain,boundaryBox},

(*  Creates an array with all of the walls of the domain. Given normal vectors and centriod location *) 

	boundaryBox = ConstantArray[{{0,0,0},{0,0,0}},6];

	boundaryBox[[1,1]] = {1,0,0}; 
	boundaryBox[[1,2]] = {d[[1]],0,0};

	boundaryBox[[2,1]] = {-1,0,0}; (* outward normal vector *)
	boundaryBox[[2,2]] = {-d[[1]],0,0};

	boundaryBox[[3,1]] = {0,1,0};
	boundaryBox[[3,2]] = {0,d[[2]],0};

	boundaryBox[[4,1]] = {0,-1,0};
	boundaryBox[[4,2]] = {0,-d[[2]],0};

	boundaryBox[[5,1]] = {0,0,1};
	boundaryBox[[5,2]] = {0,0,d[[3]]};

	boundaryBox[[6,1]] = {0,0,-1}; 
	boundaryBox[[6,2]] = {0,0,-d[[3]]};

boundaryBox
  ];

  FindNodesNextToIntersection[poly_, intLine_] := Block[{ line = intLine, tmpPoints, cnt, nNodes, j, t, intPoint,eps,v,p, next},

(* Given an intersection between a polygon and boundary this returns the index of the four nodes which are spilt by the intersections. 
	tmpPoints[[1,:]] are the first two indices
	tmpPoints[[2,:]] are the next two indices
*)

	tmpPoints = {{0,0},{0,0}};
	cnt = 0;
	eps = 10^(-13);

	nNodes = poly[[1,2]];
	(* Find the Line that describes the edge between adjacenct vertices of a polygon and then check for the intersection of that edge with the infintie line of intersection *)
	For[j = 1, j <= nNodes, j++,
		t = 10;
		If[ j == nNodes, next = 1;];
		If[ j != nNodes, next = j+1;];
		(*Find Vector that describes edge between vertices j and j+1*)

                v=poly[[3,next]]-poly[[3,j]]; 
                p=poly[[3,j]]; 
	
		(* find point of intersection between line and edge of polygon*)
		intPoint = LineIntersection3D[line[[1]], line[[2]], p, v];
	
		Which [ Abs[v[[1]]] > eps , t = (intPoint[[1]] - p[[1]])/v[[1]];,
			Abs[v[[2]]] > eps , t = (intPoint[[2]] - p[[2]])/v[[2]];,
			Abs[v[[3]]] > eps , t = (intPoint[[3]] - p[[3]])/v[[3]];];


		If[0 <= t <= 1,
			If[ cnt == 0,
				tmpPoints[[1,1]] = j;
				tmpPoints[[1,2]] = next;
			];
			If[ cnt == 1,
				tmpPoints[[2,1]] = j;
				tmpPoints[[2,2]] = next;
			];
			cnt++;
		];
	];

tmpPoints
  ];


  InsideDomain[poly_, intPts_, tmpPointsArry_] := Block[{intPoints = intPts, tmpPoints = tmpPointsArry, keep, nNodes,newNumNodes,points,i,j, startPt, endPt},


	(* If the starting index of the polygon is within the domain this module is called to remove the indices outside of the domain and insert the 
		points of intersection with the domain. 
	*)

	startPt = tmpPoints[[1,1]];
	endPt = tmpPoints[[2,2]];

	nNodes = poly[[1,2]];
	(* Make Modification if line is between end Node and 1 *)

	If[ tmpPoints[[2,2]] != 1,
		keep = {0};
		For[j = 1, j <= nNodes, j++,
			If[ j <= tmpPoints[[1,1]],
				keep = AppendTo[keep,j];
			];
			If[ j >= tmpPoints[[2,2]],
				keep = AppendTo[keep,j];
			];
		];
	keep = Delete[keep,1];
	];

	(* If the the line of intersection cuts between the end point and then starting point *)
	If[ tmpPoints[[2,2]] == 1, 
		keep = {0};
		For[j = 1, j <= nNodes, j++,
			If[ j <= tmpPoints[[1,1]],
				keep = AppendTo[keep,j];
			];
		];
		keep = Delete[keep,1];
	];
	newNumNodes = Length[keep] + 2;

	(*  Create new point list *)
	points = ConstantArray[ {0,0,0}, newNumNodes];

	(*Copy the points that are being kept *)

	For[ i = 1, i <= tmpPoints[[1,1]],i++,
		If[ i <= tmpPoints[[1,1]],
			points[[i,1]] = poly[[3,i,1]];
			points[[i,2]] = poly[[3,i,2]];
			points[[i,3]] = poly[[3,i,3]];
		];
	];

	(*Insert the intersection points *)
	i = tmpPoints[[1,1]] + 1;
	points[[i,1]] = intPoints[[1,1]];
	points[[i,2]] = intPoints[[1,2]];
	points[[i,3]] = intPoints[[1,3]];

	i  = tmpPoints[[1,1]] + 2;
	points[[i,1]] = intPoints[[2,1]];
	points[[i,2]] = intPoints[[2,2]];
	points[[i,3]] = intPoints[[2,3]];

	j = tmpPoints[[2,2]];

	(*Copy the remain points  *)
	For[i = tmpPoints[[1,1]] + 3, i <= newNumNodes, i++,
		points[[i,1]] = poly[[3,j,1]];
		points[[i,2]] = poly[[3,j,2]];
		points[[i,3]] = poly[[3,j,3]];
		j++;
	];

points
  ];

 OutsideDomain[poly_, intPts_, tmpPointsArray_] := Block[{intPoints = intPts, tmpPoints = tmpPointsArray, keep, nNodes,newNumNodes,points,i,j,startPt,endPt},
	

	(* This module is called if the starting index of the polygon is outside of the domain *)

	nNodes = poly[[1,2]];
	
	(* Case 1 : only the first point is outside of the domain. *) 

	If[ tmpPoints[[1,1]] == 1 && tmpPoints[[2,2]] == 1,
		newNumNodes = nNodes + 1; 

		points = ConstantArray[ {0,0,0}, newNumNodes];

		points[[1,1]] = intPoints[[1,1]];
		points[[1,2]] = intPoints[[1,2]];
		points[[1,3]] = intPoints[[1,3]];

		For[i = 2, i <= poly[[1,2]], i++,
			points[[i,1]] = poly[[3,i,1]];
			points[[i,2]] = poly[[3,i,2]];
			points[[i,3]] = poly[[3,i,3]];
		];

		points[[newNumNodes,1]] = intPoints[[2,1]];
		points[[newNumNodes,2]] = intPoints[[2,2]];
		points[[newNumNodes,3]] = intPoints[[2,3]];

	];

	(* Case 2 : If 1 is outside the domain and is a vertex next to first intersection *)
	If[ tmpPoints[[1,1]] == 1 && tmpPoints[[2,2]]!= 1,
		startPt = tmpPoints[[1,2]];
		endPt = tmpPoints[[2,1]];
		keep = {0};

		For[j = 1, j <= nNodes, j++,
			If[  startPt <= j <= endPt,
				keep = AppendTo[keep,j];
			];
		];
		keep = Delete[keep,1];

		newNumNodes = Length[keep] + 2;

		points = ConstantArray[ {0,0,0}, newNumNodes];

		points[[1,1]] = intPoints[[1,1]];
		points[[1,2]] = intPoints[[1,2]];
		points[[1,3]] = intPoints[[1,3]];

		For[i = 2, i <= endPt, i++,
			points[[i,1]] = poly[[3,i,1]];
			points[[i,2]] = poly[[3,i,2]];
			points[[i,3]] = poly[[3,i,3]];
		];

		points[[newNumNodes,1]] = intPoints[[2,1]];
		points[[newNumNodes,2]] = intPoints[[2,2]];
		points[[newNumNodes,3]] = intPoints[[2,3]];

	];

	(* Case 3 : If 1 is outside the domain and is a vertex next to second intersection *)
	If[ tmpPoints[[1,1]] != 1 && tmpPoints[[2,2]]== 1,
		startPt = tmpPoints[[1,2]];
		endPt = tmpPoints[[2,1]];

		keep = {0};

		For[j = 1, j <= nNodes, j++,
			If[  startPt <= j <= endPt,
				keep = AppendTo[keep,j];
			];
		];

		keep = Delete[keep,1];

		newNumNodes = Length[keep] + 2;
		points = ConstantArray[ {0,0,0}, newNumNodes];

		points[[1,1]] = intPoints[[2,1]];
		points[[1,2]] = intPoints[[2,2]];
		points[[1,3]] = intPoints[[2,3]];

		points[[2,1]] = intPoints[[1,1]];
		points[[2,2]] = intPoints[[1,2]];
		points[[2,3]] = intPoints[[1,3]];

		j = 3;
		For[i = startPt, i <= endPt, i++,
			points[[j,1]] = poly[[3,i,1]];
			points[[j,2]] = poly[[3,i,2]];
			points[[j,3]] = poly[[3,i,3]];
			j++;
		];
	];

	(* Case 4 : If 1 is outside the domain and is not vertex next to an intersection *)
	If[ tmpPoints[[1,1]] != 1 && tmpPoints[[2,2]]!= 1,

		startPt = tmpPoints[[1,2]];
		endPt = tmpPoints[[2,1]];

		keep = {0};

		
		For[j = 1, j <= nNodes, j++,
			If[  startPt <= j <= endPt,
				keep = AppendTo[keep,j];
			];
		];

		keep = Delete[keep,1];
		newNumNodes = Length[keep] + 2;

		points = ConstantArray[ {0,0,0}, newNumNodes];

		points[[1,1]] = intPoints[[1,1]];
		points[[1,2]] = intPoints[[1,2]];
		points[[1,3]] = intPoints[[1,3]];

		j = 2;
		For[i = startPt, i <= endPt, i++,
			points[[j,1]] = poly[[3,i,1]];
			points[[j,2]] = poly[[3,i,2]];
			points[[j,3]] = poly[[3,i,3]];
			j++;
		];

		points[[newNumNodes,1]] = intPoints[[2,1]];
		points[[newNumNodes,2]] = intPoints[[2,2]];
		points[[newNumNodes,3]] = intPoints[[2,3]];
	];
points
  ];


  DomainTruncation[apoly_, domain_, boundaryArray_, length_, eps_] := Block[{ poly,  d = domain,h = length, boundaryBox = boundaryArray, nVertices, prevdist, currdist, tmppts, nNodes, pt, n, ntmp, pttmp, intLine,intPoint, tmpPoints, intPoints, flag, points, holder, tmp, jj,i,j,next,pt1,pt2},


	(* main module for domain truncation*)

        poly=apoly; 

	pt = poly[[3,1]]; (* point on the infinite plane *)
	n = poly[[2,1]]; (* normal vector of the fracture *) 
	
	(* Check against the all the walls of the domain *)
	For[ jj = 1, jj <= 6, jj ++,
		ntmp  = boundaryBox[[jj,1]]; (*normal vector of the jj side of the domain - tmp*)
		pttmp = boundaryBox[[jj,2]]; (*point on the jj side of the domain - tmp*)

		nVertices = poly[[1,2]]; (* number of vertices of the truncated polygon is
									initially set to have the same number of nodes
									as the original number of verticies of the polygon *)
		points = ConstantArray[{0,0,0}, nVertices+2 ]; 
									(*seems like an array to hold the coordinates of
									the new nodes*)
		nNodes = 0; (* a counter of the final new numbers of fracture verticies *)

			prevdist = Dot[ poly[[3,nVertices]] - pttmp, ntmp ];
			(*previous distance - the dot product of the domain side normal
			and the distance between vertex number nVertices and the temp point on the
			domain side. *)
		For[ ii = 1, ii <= nVertices, ii++, 
			currdist = Dot[ poly[[3,ii]] - pttmp, ntmp ];
			(*current distance, the dot product of domain side normal and 
			the distance between the iith vertex and the temporary point*) 
			If[ currdist <= 0, (* if vertex is towards the domain relative to the domain side *)
				nNodes++;
				points[[nNodes]] = poly[[3,ii]] (* preserve the vertex *)
			];
			If[ currdist*prevdist < 0, (* if crosses boundary *)
				poly[[1,1]] = 2; (* truncated *)
				nNodes++;
				If[ ii == 1, (* store point on boundary *)
					points[[nNodes]] = poly[[3,nVertices]] + (poly[[3,1]] - poly[[3,nVertices]])*N[Abs[prevdist]/(Abs[currdist]+Abs[prevdist])], 
					points[[nNodes]] = poly[[3,ii-1]] + (poly[[3,ii]] - poly[[3,ii-1]])*N[Abs[prevdist]/(Abs[currdist]+Abs[prevdist])]
				];
				If[ currdist < 0, (* if from outside to inside *)
					tmppts = points[[nNodes]]; 
					points[[nNodes]] = points[[nNodes-1]];
					points[[nNodes-1]] = tmppts (* swap the order *)
				]
			];
			prevdist = currdist
		];
(*****
		(* Find Line of intersection between the fracture and the boundary *)
		intLine = FindLineOfIntersection[n, pt,ntmp, pttmp];	
		intPoints == {};


		(*  Find the point of intersection between the ellipse and the line of intersection*)
		If[ intLine[[1,1]] != 10000,
			intPoints = PointOfLineIntersection[poly, intLine];
		 ];


		(* If there are points of intersection, ....*)
		If[ Length[intPoints] > 1,  
			tmpPoints = FindNodesNextToIntersection[poly, intLine];

			(* Check if node 1 is above/below inside/outside of current boundary *)
			i= 0;
			Which[ pttmp[[1]]!=0, i = 1, pttmp[[2]] !=0, i = 2, pttmp[[3]] != 0, i = 3; ];
	
			flag = 0;
			If[ pttmp[[i]] <0,
				If[ poly[[3,1,i]] > -d, 
					flag = 0;
				];
				If[ poly[[3,1,i]] < -d, 
					flag = 1;
				];
			];

			If[ pttmp[[i]] > 0,
				If[ poly[[3,1,i]] > d, 
					flag = 1;
				];
				If[ poly[[3,1,i]] < d, 
					flag = 0;
				];
			];

			(* If the first node is in the domain *)
			If[ flag == 0,
				points = InsideDomain[poly, intPoints, tmpPoints];
			];

			(* If the First Node is outside of the domain, remove that point at *) 
			If[ flag == 1,
				points = OutsideDomain[poly,  intPoints, tmpPoints];
			];

			(* Copy new points into poly*)
			nNodes = Length[points]; (*Gather Number of Nodes From Polygon*)
**)

		poly[[1,2]] = nNodes;(*Number of Vertices, in this case it's a square so nNodes = 4*)
             
		poly[[3]] = Take[points,nNodes] (* the first nNodes elements are preserved vertices *)           

(*****	
		(* Check if Nodes of the polygon are too close together *)
		flag = 0;
		For[j = 1, j <= nNodes, j++,
			If[ j == nNodes, next = 1;];
			If[ j != nNodes, next = j+1;];
			(* Find Vector that describes edge between vertices j and j+1 *)

			pt1 = {poly[[3,j]]};
			pt2 = {poly[[3,next]]};

			tmp  = EuclideanDistance[pt1,pt2];
			If[ tmp < h,
				poly[[1,1]] = 0; (***** definition of poly[[1,1]] has changed *)
				flag = 1
				(***** Print["Reject"]; *)
			];
		];
		If[ flag == 1, Break[] ]
*)
(***** ] **)
	]; (* end for *)
	
(* Check if vertices of the polygon are too close together *)
ii = 1;
While[ ii <= nNodes,
	If[ ii == nNodes, next = 1, next = ii+1 ];
	If[ Norm[poly[[3,ii]] - poly[[3,next]]] < (2*h), (* if distance between current and next vertex < h *)
		If[ Min[ Abs[Abs[poly[[3,ii]]]-d]] > eps, (* if current vertex is not on boundary, within an error of eps *)
			poly[[3]] = Delete[ poly[[3]],ii ] (* delete curr *)
		,
			poly[[3]] = Delete[ poly[[3]],next ] (* else, delete next *)
		]; 
		nNodes--;
		poly[[1,1]] = 3 (* will be changed if rejected later *)
	,
		ii++ (* else, proceed to the next vertex *)
	]
];
poly[[1,2]] = nNodes; (* update vertex number *)
If[ nNodes < 3, poly[[1,1]] = -5 ]; (* reject if too few vertices are left *)

poly
];


  FindLineOfIntersectionOld[normal1_, point1_, normal2_, point2_] := Block[{n1 = normal1, pt1 =  point1,n2 =  normal2, pt2 = point2,n3,d1,d2,b,m,QR,Q,R,bb,intLine},

        intLine = ConstantArray[10000,{2,3}];

        n3 = MyCross[n1,n2];  (*Vector Parallel to the Intersection*)

        d1 = Dot[n1,pt1];
        d2 = Dot[n2,pt2];

        b = {d1,d2};
        m = {{n1[[1]],n1[[2]], n1[[3]]},{n2[[1]],n2[[2]], n2[[3]]}};

        (*Solve the Underdetermined system using a qr decomposition*)

        QR = QRDecomposition[m];
        Q = QR[[1]];
        R = QR[[2]];
        bb = Dot[Transpose[Q],b];
        intLine[[1]] = LinearSolve[R,bb];
        intLine[[2]] = n3;

intLine
  ];

  FindLineOfIntersection[normal1_, pt1_, normal2_, pt2_] := Block[{n1, n2, n3,h1, h2,c1, c2,denom, intLine, tol=10^(-13)},

        n1=normal1; 
        n2=normal2; 


        (*Vector Parallel to the Intersection*)

        n3= { n1[[2]]*n2[[3]]-n1[[3]]*n2[[2]], n1[[3]]*n2[[1]]- n1[[1]]*n2[[3]], n1[[1]]*n2[[2]]- n1[[2]]*n2[[1]]}; 


        intLine=If[ n3.n3 < tol, ConstantArray[10000,{2,3} ],


        h1=n1.pt1;
        h2=n2.pt2;

        denom=1-(n1.n2)^2;
        c1=(h1-h2*(n1.n2))/denom;
        c2=(h2-h1*(n1.n2))/denom;

        {c1*n1 + c2*n2, n3} ]; 

        intLine
          ];

  End[]

  EndPackage[]




