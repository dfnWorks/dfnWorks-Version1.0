(* ::Package:: *)

BeginPackage[ "smallFunctions`"]

  polyPlotFunction::usage = 
	"Output is 3d plot of polygons, display using show in main window"

 plotPoints::usage = 
	" Creates a plot of all the points in the Adjancey Matrix A"

  printPolygonInformation::usage = 
	"Prints out the information concerning Polygon number ii"

  Begin[ "Private`"]

  polyPlotFunction[array_] := Module[{poly = array, nPoly, nNodes , triples, tmp, i, j, polyPlot, maxX, minX, maxY, minY, maxZ, minZ},

    minX = 10000; maxX = -10000; minY = 10000; maxY = -10000;  minZ = 10000; maxZ = -10000;

	nPoly = Length[poly];
	triples = {0};

	For [i = 1, i <= nPoly, i++,
		nNodes = poly[[i,1,2]];
        tmp = ConstantArray[{0,0,0}, nNodes];
		For [ j = 1, j<= nNodes, j++,
			tmp[[j]] = poly[[i,3,j]];

			If[ tmp[[j,1]] > maxX, maxX =  tmp[[j,1]]];
			If[ tmp[[j,2]] > maxY, maxY =  tmp[[j,2]]];
			If[ tmp[[j,3]] > maxZ, maxZ =  tmp[[j,3]]];

			If[ tmp[[j,1]] < minX, minX =  tmp[[j,1]]];
			If[ tmp[[j,2]] < minY, minY =  tmp[[j,2]]];
			If[ tmp[[j,3]] < minZ, minZ =  tmp[[j,3]]]
		];
		AppendTo[triples,tmp];
	];


	triples = Delete[triples,1];
	polyPlot = Graphics3D[Map[Polygon, triples],  Axes -> True, AxesLabel -> {x, y, z}, PlotRange->{{minX - 1,maxX + 1},{minY - 1,maxY + 1},{minZ - 1 ,maxZ + 1}}];
	
	polyPlot
  ]

plotPoints[Array_, n_] := Module[{ A = Array, nPoly = n, pts, i, j, pointPlot},
	
	pts = {0};
	For[ i = 1, i <= nPoly, i++,
		For[ j = 1, j <= A[[i,1]], j++,
				pts = AppendTo[pts, A[[i,2,j,2]]];
				pts = AppendTo[pts, A[[i,2,j,3]]];
			];
		];

	pts = Delete[pts,1];
	pointPlot = ListPointPlot3D[ {pts},PlotStyle ->  PointSize[0.01] ];

pointPlot
]

  printPolygonInformation[array_, index_] := Module[{poly = array, ii = index},

	Print["Information about Polygon:", "\t", ii];
	Print["Fractrure Set Number:","\t", poly[[ii,1,1]]];
	Print["Number of Vertices:","\t", poly[[ii,1,2]]];
	Print["Normal Vector:","\t", poly[[ii,2,1]]];
	Print["Translation Vector:","\t", poly[[ii,2,2]]];
	Print["Bounding Box", "\t", poly[[ii,4]]];
    Print["Family Number", "\t", poly[[ii,5,1]]];
    Print["Aspect Ratio", "\t", poly[[ii,5,2]]];
    Print["X length", "\t", poly[[ii,5,3]]];
    Print["Y length", "\t", poly[[ii,5,4]]];
	Print["List of Vertices:"];
	For[ i = 1, i <= poly[[ii,1,2]],i++,
		Print[poly[[ii,3,i]]];
	];
    
	Print["\n"];

]
  End[]

  EndPackage[]


 










