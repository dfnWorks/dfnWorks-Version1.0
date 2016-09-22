(* ::Package:: *)

(* ::Code:: *)
BeginPackage[ "GeneratingPolygons`"]


(* ::Code:: *)
  << "ComputationalGeometry`"
  << VectorAnalysis`

  rotateNodes::usage = 
	" Rotate2 the polygon so it alligns with the plane described by the normal vector. "

  createUserDefinedCoordRec::usage=
	"as its name implies."

  TranslatePolygon::usage = 
	"Translates the points of a poly poly to there new location"

  UnTranslatePolygon::usage = 
	"Moeves Polygon back to the origin"

  CreateBoundingBox::usage = 
	"Stores the Bounding Box of a polygon"

  CreateSquare::usage = 
	"CreateSquare creates a unit square and translates it by a vector t and give the plane the normal n"

  CreateRectangle::usage = 
	"  creates a rectangle square and translates it by a vector t and give the plane the normal n"

  distcreteEllipse::usage = 
	"  Discretizes an ellipse with axis a and b or x intervals delx"

  distcreteEllipsev2::usage = 
	"  Discretizes an ellipse with axis a and b or x intervals delx"

  CreateEllipse::usage = 
	"  creates a Ellipse centered at the origin and translates it by a vector t and give the plane the normal n"

  createConvexHull::usage = 
	"Generates n random points in R^2 and then computes the convex hull of the points"

  CreateConvexPolygon::usage = 
	" CreateConvexPolygon creates a random convex ploygon using a convex hull"

  CleanupPolygons::usage =
	"Removes Bad Polygons from the Polygon set"

  GenerateTheta::usage = 
	"Generates distribution of angles theta of a given ellipse with an aspect ratio"

  Begin["`Private`"]

rotateNodes[list_, vect_, phi_] := Block[ {pointList = list, nvect = vect, out, numPoints, i, eps, \[Theta], V, e3, R, angle = phi},

	numPoints = Length[pointList];
	out = ConstantArray[ {0,0,0}, numPoints];

	(* 2D rotation *)
	If[ angle == 2*Pi, angle = Random[]*2*Pi ]; (* if angle is not specified *)
		
	R = RotationMatrix[angle,{0,0,1}];
	For [ i = 1, i <= numPoints, i++,
		out[[i]] =  Dot[R,pointList[[i]]];
	];
	pointList = out;

	If[ nvect != {0,0,1},
		(* Rotaes the polygon generated in the xy plane to have the normal vector nvect *)
	
		e3 = UnitVector[3,3];
		nvect = Normalize[nvect];

		(*Create Rotation Matrix*)
		\[Theta] = ArcCos[Dot[nvect,e3]];
		V = Cross[e3,nvect];

		R = RotationMatrix[\[Theta],V];

		For [ i = 1, i <= numPoints, i++,
			out[[i]] =  Dot[R,pointList[[i]]];
		];
	];
	If[ nvect == {0,0,1},
		For [ i = 1, i <= numPoints, i++,
			out[[i]] =  pointList[[i]];
		];
	];
	out
]

  TranslatePolygon[apoly_] := Block[{poly,  j, nNodes, translationVector}, 
	(*Translates the vertices of a polygon according to the translation vector.*)
        poly=apoly; 

	nNodes = poly[[1,2]];
	translationVector = poly[[2,2]];
	For [ j = 1, j<= nNodes, j++,
		(* Translates the points (x_j, y_j, z_j be the translation vector*)
		poly[[3,j,1]] =  poly[[3,j,1]] + translationVector[[1]];
		poly[[3,j,2]] =  poly[[3,j,2]] + translationVector[[2]];
		poly[[3,j,3]] =  poly[[3,j,3]] + translationVector[[3]];
	];
  poly
  ]

  UnTranslatePolygon[apoly_] := Block[{poly, j, nNodes, translationVector}, 
	(*Retruns the polygon to the origin*)
        poly=apoly; 
	nNodes = poly[[1,2]];
	translationVector = poly[[2,2]];
	For [ j = 1, j<= nNodes, j++,
		(* Translates the points (x_j, y_j, z_j be the translation vector*)
		poly[[3,j,1]] =  poly[[3,j,1]]  - translationVector[[1]];
		poly[[3,j,2]] =  poly[[3,j,2]]  - translationVector[[2]];
		poly[[3,j,3]] =  poly[[3,j,3]]  - translationVector[[3]];
	];

  poly
  ]

CreateBoundingBox[apoly_]:= Block[{poly, j, nNodes, minX, maxX, minY, maxY, minZ, maxZ},
  (*Finds the Bounding Box of a polygon*)
    poly=apoly; 
  (*Boundaring Box*)

	poly[[4,1]] = {Min[poly[[3,All,1]]],Max[poly[[3,All,1]]]};
	poly[[4,2]] = {Min[poly[[3,All,2]]],Max[poly[[3,All,2]]]};
	poly[[4,3]] = {Min[poly[[3,All,3]]],Max[poly[[3,All,3]]]};
  
  poly
];


(* ::Code:: *)
 createUserDefinedCoordRec[userDefCoordinates_, famn_]:=Block[{poly,
 nNodes, normal,t},

vertex = { userDefCoordinates[[1]], userDefCoordinates[[2]],userDefCoordinates[[3]], userDefCoordinates[[4]]};

	(*center*)
	t=  {	Mean[{vertex[[1,1]], vertex[[2,1]], vertex[[3,1]], vertex[[4,1]]}],
		Mean[{vertex[[1,2]], vertex[[2,2]], vertex[[3,2]], vertex[[4,2]]}],
		Mean[{vertex[[1,3]], vertex[[2,3]], vertex[[3,3]], vertex[[4,3]]}]};  
normal= Normalize[CrossProduct[(vertex[[2]]-vertex[[1]]), (vertex[[4]]-vertex[[1]])]];
xradius= EuclideanDistance[vertex[[1]],vertex[[3]]]/2;
yradius= EuclideanDistance[vertex[[2]],vertex[[4]]]/2;



(*aperture designation *)
If[ValueQ[Global`apertureFromTransmissivity ]==True,
radiusAverage=Mean[{xradius, yradius}];
kb=Global`apertureFromTransmissivity [[2]];
apertureMeanF=Global`apertureFromTransmissivity [[1]];
transmissivity=apertureMeanF*radiusAverage^kb;
mu=8.94*10^-4;
ro=997;
g=9.8;
aperture=((transmissivity*12*mu)/(ro*g))^(1/3); 
];
If[ValueQ[Global`meanAperture]==True,
aperture = Random[ Global`apDist ];
];
If[ValueQ[Global`constantAperture]==True,
aperture=Global`constantAperture;
];
If[ValueQ[Global`lengthCorrelatedAperture]==True,
radiusAverage=Mean[{xradius, yradius}];
apertureMeanF=Global`lengthCorrelatedAperture[[1]];
b=Global`lengthCorrelatedAperture[[2]];
aperture = apertureMeanF*radiusAverage^b;
];

(*permeability designation *)
If[ValueQ[Global`constantPermeability]==True && ValueQ[Global`meanAperture]==True, 
Clear[Global`constantPermeability];
]
If[ValueQ[Global`constantPermeability]==True && ValueQ[Global`lengthCorrelatedAperture]==True,
Clear[Global`constantPermeability];
]
If[ValueQ[Global`apertureFromTransmissivity ]==True,
	perm=ConstantArray[aperture*aperture/12, 3]
];
If[ValueQ[Global`constantPermeability]==True,
perm=ConstantArray[Global`constantPermeability,3];
]; 
If[ValueQ[Global`constantPermeability]==False && 
ValueQ[Global`apertureFromTransmissivity ]==False,
perm=aperture^2/12;(* in case of specified aperture and no perm. option at all*)
perm={perm, perm, perm};
];

faces={};
groupNum=0;
aspectRatio=xradius/yradius;
nNodes = 4; (* Number of Vertices, in this case it is a rectangle so nNodes = 4 *)

poly = {{1,nNodes,0, aperture}, {normal,t,perm, faces, groupNum}, vertex, ConstantArray[{0,0}, 3],{famn, aspectRatio, xradius, yradius}, {0, 0}}; (* new poly to be tested *)



poly
]



(* ::Code:: *)
CreateRectangle[normalvector_, translation_, lx_, ly_, phi_, famn_, asratio_] := Block[{poly,  xradius = lx, yradius = ly, nNodes, 
tmp, flag , cnt, normal = normalvector, t = translation, i, familyNumber = famn, aspectRatio = asratio}, 

		(* Generatesrectangle of dimensions xradius by yradius in the xy plane, and then caclulates its z values using the normal vector. The resulting 
		polygon is translated into R^3, a bounding box  is also computed. *) 

		nNodes = 4; (* Number of Vertices, in this case it is a rectangle so nNodes = 4 *)
faces={};
groupNum=0;


(*aperture designation *)
If[ValueQ[Global`apertureFromTransmissivity ]==True,
radiusAverage=Mean[{xradius, yradius}];
kb=Global`apertureFromTransmissivity [[2]];
apertureMeanF=Global`apertureFromTransmissivity [[1]];
transmissivity=apertureMeanF*radiusAverage^kb;
mu=8.94*10^-4;
ro=997;
g=9.8;
aperture=((transmissivity*12*mu)/(ro*g))^(1/3); 

];
If[ValueQ[Global`meanAperture]==True,
aperture = Random[ Global`apDist ];
];
If[ValueQ[Global`constantAperture]==True,
aperture=Global`constantAperture;
];
If[ValueQ[Global`lengthCorrelatedAperture]==True,
radiusAverage=Min[{xradius, yradius}];
apertureMeanF=Global`lengthCorrelatedAperture[[1]];
b=Global`lengthCorrelatedAperture[[2]];
aperture = apertureMeanF*radiusAverage^b;
];

(*permeability designation *)
If[ValueQ[Global`constantPermeability]==True && ValueQ[Global`meanAperture]==True, 
Clear[Global`constantPermeability];
]
If[ValueQ[Global`constantPermeability]==True && ValueQ[Global`lengthCorrelatedAperture]==True,
Clear[Global`constantPermeability];
]
If[ValueQ[Global`apertureFromTransmissivity ]==True,
	perm=ConstantArray[aperture*aperture/12, 3]
];
If[ValueQ[Global`constantPermeability]==True,
perm=ConstantArray[Global`constantPermeability,3];
]; 
If[ValueQ[Global`constantPermeability]==False && 
ValueQ[Global`apertureFromTransmissivity ]==False,
perm=aperture^2/12;(* in case of specified aperture and no perm. option at all*)
perm={perm, perm, perm};
];


	   poly = {{1,nNodes,0, aperture}, {normal,t,perm, faces, groupNum}, ConstantArray[{0,0,0}, nNodes], ConstantArray[{0,0}, 3],{familyNumber, aspectRatio, xradius, yradius},{0, 0}}; (* new poly to be tested *)
       
  
		(*Create unit sqaure with centroid at the origin; *)
		(* nodes are listed counterclockwise*)
		poly[[3,1,1]] = -xradius;   (*x value for first node*)
		poly[[3,1,2]] = -yradius;   (*y value for first node*)
			

		poly[[3,2,1]] = xradius;  (*x value for 2nd node*)
		poly[[3,2,2]] = -yradius;  (*y value for 2nd node*)

		poly[[3,3,1]] = xradius;  (*x value for 3rd node*)
		poly[[3,3,2]] = yradius;  (*y value for 3rd node*)
			
		poly[[3,4,1]] = -xradius;  (*x value for 4th node*)
		poly[[3,4,2]] = yradius;   (*y value for 4th node*)
		
		(*Rotate Plane to allgin with normal vector*)
		tmp = rotateNodes[poly[[3]], normal, phi]; 
		poly[[3]] = tmp;
																			  
		poly = TranslatePolygon[poly];	
        
		poly
];


(* ::Code:: *)
distcreteEllipse[ xradius_, yradius_, numPoints_, thetaArray_] := Block[{ a = xradius, b = yradius, n = numPoints,theta = thetaArray, points, i},

	points = ConstantArray[{0,0},n];
	For[ i = 1, i <= n, i ++, 

		points[[i,1]] = a*Cos[theta[[i]]];
		points[[i,2]] = b*Sin[theta[[i]]];
		
	];
	points
];



(* ::Code:: *)
CreateEllipse[normalvector_, translation_, lx_, ly_, numPoints_,  
thetaArray_, phi_, famn_, asratio_] := Block[{poly, xradius = lx, yradius = ly,
n = numPoints, theta =  thetaArray, nNodes, tmp, points, normal = normalvector,
t = translation, i, familyNumber = famn, aspectRatio = asratio}, 

	(* Generates an ellipse of asix length xradius by yradius in the xy plane, and then caclulates its z values using the normal vector. The resulting 
	polygon is translated into R^3, a bounding box is also computed. *) 
	permeability=perm;									
	points = distcreteEllipse[ xradius, yradius, n, theta];
	nNodes = Length[points];
	faces={};
	groupNum=0;
	

(*aperture designation *)
If[ValueQ[Global`apertureFromTransmissivity ]==True,
radiusAverage=Mean[{xradius, yradius}];
kb=Global`apertureFromTransmissivity [[2]];
apertureMeanF=Global`apertureFromTransmissivity [[1]];
transmissivity=apertureMeanF*radiusAverage^kb;
mu=8.94*10^-4;
ro=997;
g=9.8;
aperture=((transmissivity*12*mu)/(ro*g))^(1/3); 
];
If[ValueQ[Global`meanAperture]==True,
aperture = Random[ Global`apDist ];
];
If[ValueQ[Global`constantAperture]==True,
aperture=Global`constantAperture;
];
If[ValueQ[Global`lengthCorrelatedAperture]==True,
radiusAverage=Mean[{xradius, yradius}];
apertureMeanF=Global`lengthCorrelatedAperture[[1]];
b=Global`lengthCorrelatedAperture[[2]];
aperture = apertureMeanF*radiusAverage^b;
];

(*permeability designation *)
If[ValueQ[Global`constantPermeability]==True && ValueQ[Global`meanAperture]==True, 
Clear[Global`constantPermeability];
]
If[ValueQ[Global`constantPermeability]==True && ValueQ[Global`lengthCorrelatedAperture]==True,
Clear[Global`constantPermeability];
]
If[ValueQ[Global`apertureFromTransmissivity ]==True,
	perm=ConstantArray[aperture*aperture/12, 3]
];
If[ValueQ[Global`constantPermeability]==True,
perm=ConstantArray[Global`constantPermeability,3];
]; 
If[ValueQ[Global`constantPermeability]==False && 
ValueQ[Global`apertureFromTransmissivity ]==False,
perm=aperture^2/12;(* in case of specified aperture and no perm. option at all*)
perm={perm, perm, perm};
];

	poly = {{1,nNodes,0, aperture}, {normal,t,perm, faces, groupNum}, ConstantArray[{0,0,0}, nNodes], ConstantArray[{0,0}, 3],{familyNumber,aspectRatio, xradius, yradius}, {0, 0} }; (* new poly to be tested *)

	For[ i = 1, i<= nNodes, i++,
		poly[[3,i,1]] = points[[i,1]];   (*x value for first node*)
		poly[[3,i,2]] = points[[i,2]];  
	];
	
	(*Rotate Plane to allgin with normal vector*)
	tmp = rotateNodes[poly[[3]], normal, phi]; 
	poly[[3]] = tmp;

	poly = TranslatePolygon[poly];	

    poly
]



(* ::Code:: *)
createConvexHull[numpoints_] := Block[{n = numpoints, points, convexhull, out, i, index }, 

	points  = 2*RandomReal[1,{n,2}];
	convexhull = ConvexHull[points];
	out = ConstantArray[0,{Length[convexhull]}];
	For[i = 1, i<= Length[convexhull], i++,
			index = convexhull[[i]];
			out[[i]] = points[[index]];
	];
	points = out;

points
];

CreateConvexPolygon[apoly_, normalvector_, translation_, perm_, aperture_] := Block[{poly, nNodes, tmp, flag , cnt, normal = normalvector, t = translation, i, n, points}, 

        poly=apoly; 

	n = 20;
	points = createConvexHull[n];	

	nNodes = Length[points];

	poly[[1,2]]  = nNodes;
	poly[[1,4]]  = aperture;

	tmp= ConstantArray[{0,0,0}, nNodes]; 
	AppendTo[poly, tmp];
	tmp  = ConstantArray[ {0,0}, 3];
	AppendTo[poly, tmp];

	For[ i = 1, i<= nNodes, i++,
		poly[[3,i,1]] = points[[i,1]];   (*x value for first node*)
		poly[[3,i,2]] = points[[i,2]];  
	];

	poly[[2,1]] = normal;  (*Normal Vector*)
	poly[[2,2]] = t;  (*Translation Vector*)
	poly[[2,3]] = perm;  (*transmissivity Vector*)

	(*Rotate Plane to allgin with normal vector*)
	tmp = rotateNodes[poly[[3]], normal]; (***** fcn signature has changed *)
	poly[[3]] = tmp;

	poly= TranslatePolygon[poly ];


poly
];


(* ::Code:: *)
GenerateTheta[xradius_, yradius_, numPoints_, h_] := Block[{a = xradius, b = yradius, n = numPoints, tol, theta, c, del, i, tmpAngle, cnt, tmpArc}, 

	(* Tolerance for difference between arclengths of segments*)
	tol = 0.01;

	(* Number of nodes on the ellipse.*)
	(* Declare Arrays *) 
	theta = ConstantArray[0,n];

	(*Total Arclength*)
	c = Pi*(a+b)*( 1 + (3*  ((a-b)/(a+b))^2)/(10 + Sqrt[4 - 3* ((a-b)/(a+b))^2]));

	(*Arclength per section*)
	del = N[c/n];
														   
	(*If[ del <= h, Print["Error: too many points on perimeter del",del, " c=",N[c]," n=",n] ]; *)(* an error message *)

	(* Print[del]; *)

	For[i= 2, i <= n, i++,

		(*Shooting With Forward Euler*)
		tmpAngle = theta[[i-1]] + (del / Sqrt[ (-a*Sin[theta[[i-1]]])^2 + (b*Cos[theta[[i-1]]])^2]) ;

		(*Correction With Backward Euler*)
		tmpAngle = theta[[i-1]] + (del/ Sqrt[ (-a*Sin[tmpAngle])^2 + (b*Cos[tmpAngle])^2]) ;

		cnt = 0;
		tmpArc = 0;
		
		(* Continuation method while the difference between the desired arclength and current arclength is greater than the tolerance, keep trying, But try 100 times. then quit*)

		While [ Abs[tmpArc - del] >= tol  && cnt <= 1000 , 
			tmpArc = NIntegrate[Sqrt[ ( -a*Sin[t])^2 + (b*Cos[t])^2],{t,theta[[i-1]],tmpAngle}];

			(* overshot *)
			If [ tmpArc >= del + tol,
				tmpAngle = tmpAngle -0.05*tmpAngle; ;
			];

			(* undershot *)
			If[ tmpArc < del - tol, 
				tmpAngle = tmpAngle + 0.05*tmpAngle; ;
			];

			(* If the ARC Length is Close enough accept theta and move on*)
			If [Abs[tmpArc - del] <= tol,
				theta[[i]] = tmpAngle;
				Break[];
			];
		
			cnt++;
		];
		If[ cnt > 1000,
			theta[[i]] = tmpAngle;
			Break[];
		];

	];

	theta
  ];


(* ::Code:: *)
  CleanupPolygons[array_, nAccept_, numPoly_] := Block[{poly = array, accept  = nAccept, nPoly = numPoly ,j,ii, tmp}, 
		(*Sorts through the polygons and removes any polygon in the removal set, set = 0
		returns a new array of the acceptale polygons*)

		tmp = ConstantArray[0,accept];
		j = 1;
		For[ii = 1, ii <= nPoly, ii++,
			If[poly[[ii,1,1]] !=  0, (***** note: definition has changed *)
				tmp[[j]] = poly[[ii]]; 
				j++;
Print[ii];
Print[tmp[[j]]];
			];
	];
	poly = tmp;

  poly
  ];




(* ::Code:: *)
 End[]

  EndPackage[]









