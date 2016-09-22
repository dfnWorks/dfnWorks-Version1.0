(* ::Package:: *)

BeginPackage[ "dumpPoints`"]

Needs["MyCrossProduct`"]; 


dumpPolygonPoints::usage = 
	"Writes polygons into avi file"

dumpIntersectionPoints::usage = 
	"Writes points of the Intersection into avs file"

dumpIntPoints::usage =
	"Output of intersection points into ASCII file"

dumpParams::usage = 
	"Writes out h length scale, and rotation information for each polygon"

dumpLengths::usage = 
	"Writes out mean and x and y lengths of each polygon"

dumpPermeability::usage = 
    "Writes out permeability for each fracture"

dumpAperture::usage = 
    "Writes out aperture for each fracture"

dumpLengthInfo::usage =
   " writes out length of polygons"

Begin[ "Private`"]


dumpPolygonPoints[array_, str_, elemCnt_] := Block[{points = array, filename = str, numElem = elemCnt, fp, numPoints,i,j},

	(* fp = OpenWrite[filename, FormatType->OutputForm]; *)
	
	points12 = Threshold[points[[All,1,All]], {"Hard",10^-4}];
	(* points[[All,1,All]] = Chop[points[[All,1,All]],10^-4]; *)
	(* points[[All,1,All]] = Threshold[points[[All,1,All]], {"Hard",10^-4}]; *)
	(* points[[All,1,All]] = NumberForm[points[[All,1,All]],8]; *)
	(* points[[All,1,All]] = SetPrecision[points[[All,1,All]],8]; *)
	(* points[[All,1,All]] = SetPrecision[Threshold[points[[All,1,All]], {"Hard",10^-4}],8]; *)

	numPoints = Length[points];
  
	(* Write Header*)
	
	PutAppend[ToString[numPoints]<>" "<>ToString[numPoints-numElem]<>" 0 0 0", filename];
	(* Write[fp, numPoints, " ", numPoints - numElem, " ", 0, " ",  0 , " ",0]; *)
    (*Write in Bulk*)
	For[i = 1, i <= numPoints, i++,
		PutAppend[ToString[i]<>" "<>ToString[NumberForm[points12[[i,1]],12]]<>" "<>ToString[NumberForm[points12[[i,2]],12]]<>" "<>ToString[NumberForm[points12[[i,3]],12]], filename ] 
		(* PutAppend[ToString[i]<>" "<>ToString[points12[[1,i,1]]]<>" "<>ToString[points12[[1,i,2]]]<>" "<>ToString[points12[[1,i,3]]], filename ] *)
		(* PutAppend[ToString[i]<>" "<>ToString[points[[i,1,1]]]<>" "<>ToString[points[[i,1,2]]]<>" "<>ToString[points[[i,1,3]]], filename ] *)
		(* PutAppend[ToString[i]<>" "<>ToString[NumberForm[Threshold[points[[i,1,1]], {"Hard",10^-4}],8]]<>" "<>
			ToString[NumberForm[Threshold[points[[i,1,2]], {"Hard",10^-4}],8]]<>" "<>ToString[NumberForm[Threshold[points[[i,1,3]], {"Hard",10^-4}],8]], filename ] *)
		(* Write[fp, i," ", points[[i,1,1]]," ", points[[i,1,2]], " ", points[[i,1,3]] ] *)
	];
	(*Connection of lines*)
	j = 1;
	For[i = 1, i <= numPoints - 1 , i++,
		If[ points[[i,2]] == points[[i+1,2]],
			PutAppend[ToString[j]<>" "<>ToString[IntegerPart[points[[i,2]]]]<>" line "<>ToString[i]<>" "<>ToString[i+1], filename];
			(* Write[fp, j, " ", IntegerPart[points[[i,2]]], " line ", i," ", i+1]; *)
			j++
		]
	]

	(* Close[filename] *)
]


dumpIntersectionPoints[array_, str_, elemCnt_] := Block[{points = array, filename = str, numElem = elemCnt, fp, numPoints,i,j},

(* fp = OpenWrite[filename, FormatType->OutputForm]; (* a memory hog *) *)
(* OpenAppend[filename]; *)

points12 = Threshold[points[[All,1,All]], {"Hard",10^-4}]; 
(* points[[All,1,All]] = Threshold[points[[All,1,All]], {"Hard",10^-4}]; (* a memory hog *) 
points[[All,1,All]] = SetPrecision[points[[All,1,All]],8]; (* a memory hog *) *)
	  
j=1;													 
{r,{numPoints}} = Reap[				 
	For[i = 1, i < Length[points], i++,
       If[ points[[i,2,1]] == points[[i+1,2,1]],  
           j++,  
		   Sow[j];
		   j=1
	   ]
    ];
	Sow[j]
];
index=FoldList[Plus,0,numPoints]; 

(* Put[index,points,str] *)
(* Export[str,{index,points}] *)

(* write a dummy header to make csplit start indexing at 1 *) 
 
For[ ifrac=1, ifrac<=Length[numPoints], ifrac++, 

    istart=index[[ifrac]]+1; 
    iend=index[[ifrac+1]]; 

    numSegs=0; 
	For[i = istart, i < iend, i++,
          If[ points[[i,2,1]] == points[[i+1,2,1]] && points[[i,2,2]] == points[[i+1,2,2]] ,  
              numSegs++  
		  ]
    ]; 

	(* Write Header*)
	PutAppend[ ToString[numPoints[[ifrac]]]<>" "<>ToString[numSegs]<>" 2 0 0", filename ];
	(* Write[fp, numPoints[[ifrac]], " ", numSegs, " ", 2, " ",  0 , " ",0]; *)
    
	(*Write in Bulk*)
	For[i = istart, i <= iend, i++,
		PutAppend[ ToString[i-istart+1]<>" "<>ToString[NumberForm[points12[[i,1]],12]]<>" "<>ToString[NumberForm[points12[[i,2]],12]]<>" "<>ToString[NumberForm[points12[[i,3]],12]], filename ]
		(* PutAppend[ ToString[i-istart+1]<>" "<>ToString[points12[[1,i,1]]]<>" "<>ToString[points12[[1,i,2]]]<>" "<>ToString[points12[[1,i,3]]], filename ] *)
		(* PutAppend[ ToString[i-istart+1]<>" "<>ToString[points[[i,1,1]]]<>" "<>ToString[points[[i,1,2]]]<>" "<>ToString[points[[i,1,3]]], filename ] *)
		(* Write[fp, i-istart+1," ", points[[i,1,1]]," ", points[[i,1,2]], " ", points[[i,1,3]] ] *)
	];

	(*Connection of lines*)
	j = 1;
	For[i = istart, i < iend, i++,
		If[ points[[i,2,1]] == points[[i+1,2,1]] && points[[i,2,2]] == points[[i+1,2,2]], 
			PutAppend[ ToString[j]<>" "<>ToString[IntegerPart[points[[i,2,1]]]]<>" line "<>ToString[i-istart+1]<>" "<>ToString[i-istart+2], filename];
			(* Write[fp, j, " ", IntegerPart[points[[i,2,1]]], " line ", i-istart+1," ", i-istart+2]; *)
			j++
		]
	];

	(*Write in Line Number*)
	PutAppend[ "2 1 1", filename ];
	(* Write[fp, 2, " ", 1, " ", 1]; *)
	PutAppend[ "a_b, integer ", filename ];
	(* WriteString[fp, "a_b, integer \n"]; *)
	PutAppend[ "b_a, integer ", filename ];
	(* WriteString[fp, "b_a, integer \n"]; *)
	For[i = istart, i <= iend, i++,
		PutAppend[ ToString[i-istart+1]<>" "<>ToString[IntegerPart[points[[i,2,1]]]]<>" "<>ToString[IntegerPart[points[[i,2,2]]]], filename ]
		(* Write[fp, i-istart+1, " ", IntegerPart[points[[i,2,1]]], " ", IntegerPart[points[[i,2,2]]]] *)
	]

] (* end For *)

(* Close[filename] *)
]
 
dumpIntPoints[array_, str_, elemCnt_] := Block[{points = array, filename = str, numElem = elemCnt, fp, numPoints,i,j},

fp = OpenWrite[filename, FormatType->OutputForm]; (* a memory hog *) 

Write[fp," x, y, z, ID of fracture1, ID of fracture 2 "];

For[ i =1, i<=Length[points], i++,
        px=SetPrecision[points[[i,1,1]], 8]; 
		py=SetPrecision[points[[i,1,2]], 8];
		pz=SetPrecision[points[[i,1,3]], 8];
Write[fp,FortranForm[px]," ", FortranForm[py], " ", FortranForm[pz],"  ",points[[i,2,1]],"  ",points[[i,2,2]]];
];
     Write[fp, " "]; 
											 
	Close[filename] 
]

dumpPermeability[ array_, str_] := Block[ {poly=array, filename=str, fp}, 
	
	fp = OpenWrite[filename, FormatType->OutputForm, PageWidth -> 1000];
	Write[fp, "permeability"];
											 
	nPoly=Length[poly]; 
											 
	For[i=1, i<=nPoly, i++, 
		perm=poly[[i,2,3]]; 
		px=SetPrecision[perm[[1]], 6]; 
		py=SetPrecision[perm[[2]], 6];
		pz=SetPrecision[perm[[3]], 6];
		Write[fp, -(i+6), " 0 0  ",FortranForm[px]," ", FortranForm[py], " ", FortranForm[pz] ]
	]; 
	Write[fp, " "]; 
											 
	Close[filename]
]											

dumpLengths[ array_, str_] := Block[ {poly=array, filename=str, fp}, 
	
	fp = OpenWrite[filename, FormatType->OutputForm, PageWidth -> 1000];
	Write[fp, "x, y and mean fracture lengths"];
											 
	nPoly=Length[poly]; 
											 
	For[i=1, i<=nPoly, i++, 
		perm=poly[[i,2,3]]; 
		x=SetPrecision[poly[[i,5,3]], 6]; 
		y=SetPrecision[poly[[i,5,4]], 6];
		mean=SetPrecision[Mean[{x,y}], 6];
		Write[fp, -(i+6), " 0 0  ",FortranForm[x]," ", FortranForm[y], " ", FortranForm[mean] ]
	]; 
	Write[fp, " "]; 
											 
	Close[filename]
]




dumpAperture[ array_, str_] := Block[ {poly=array, filename=str, fp}, 
	
	fp = OpenWrite[filename, FormatType->OutputForm, PageWidth -> 1000];
	Write[fp, "aperture"];
											 
	nPoly=Length[poly]; 
											 
	For[i=1, i<=nPoly, i++, 
		aperture=SetPrecision[poly[[i,1,4]], 6]; 
		Write[fp, -(i+6), " 0 0  ",FortranForm[aperture]]
	]; 
	Write[fp, " "]; 
											 
	Close[filename]
]											 


dumpParams[array_, hscale_, nPoints_, str_,  polyOutputFile_, lineOutputFile_, slope_, refineDist_, visualizationMode_, fam_, domain_] := Block[{poly = array,  h = hscale, numPoints = nPoints, filename = str, polyfile = polyOutputFile, linefile = lineOutputFile, i,j, fp, e3, nvect,theta, V, nPoly, x0, y0, z0, x1, y1, z1, t},

	fp = OpenWrite[filename, FormatType->OutputForm, PageWidth -> 1000]; 

	nPoly = Length[poly];
	e3 = UnitVector[3,3];

    fpp = OpenWrite["NormalVectors", FormatType->OutputForm, PageWidth -> 1000]; 

	(* Write Header*)
	(* PutAppend[ToString[nPoly],filename];
	PutAppend[ToString[h],filename];
	PutAppend[ToString[numPoints],filename];
	PutAppend[ToString[slope],filename];
	PutAppend[ToString[refineDist],filename];
	PutAppend[ToString[visualizationMode],filename];
	PutAppend[polyfile,filename];
	PutAppend[linefile,filename]; *)
	Write[fp, nPoly];
	Write[fp, h];
	Write[fp, nPoints];
	Write[fp, slope];
	Write[fp, refineDist];
	Write[fp, visualizationMode];
	Write[fp, polyfile];
    Write[fp, linefile]; 

	For[i = 1, i <= nPoly, i++,
		nvect = poly[[i,2,1]];
		nvect = Normalize[nvect];
        Write[fpp, nvect[[1]],"  ",nvect[[2]],"  ",nvect[[3]]];
		(*  Find Angle of Rotation*)
		theta = N[ArcCos[Dot[nvect,e3]]];
		(* Convert angle to degrees*)
		theta = theta * (180 / Pi);
		(* Find angle to Rotate into xy plane *)
		V =  N[MyCross[e3,nvect]];
		V = Normalize[V];
        dk=Max[domain]*3;
      
		x0 = -dk*V[[1]];
		y0 = -dk*V[[2]];
		z0 = -dk*V[[3]];

		x1 = dk*V[[1]];
		y1 = dk*V[[2]];
		z1 = dk*V[[3]];

		theta = SetPrecision[theta,12];
		x0 = SetPrecision[x0,12];
		y0 = SetPrecision[y0,12];
		z0 = SetPrecision[z0,12];

		x1 = SetPrecision[x1,12];
		y1 = SetPrecision[y1,12];
		z1 = SetPrecision[z1,12];

		(* PutAppend[ToString[fam[[i]]]<>" "<>ToString[theta]<>" "<>ToString[x0]<>" "<>ToString[y0]<>" "<>ToString[z0]<>" "<>ToString[x1]<>" "<>ToString[y1]<>" "<>ToString[z1], filename] *)
		Write[fp, i," ", theta, " ", x0, " ", y0, " ", z0, " ", x1, " ", y1, " ", z1, " ", fam[[i]] ] 
	]
    Close[fpp];
	Close[filename]
]


   dumpLengthInfo[array_, cntatp_]:=Block [{all_poly=array, fAccept, fReject, ii, cntap=cntatp},
fAccept = OpenWrite[workingdirectory<>"stat/dumpAccept.txt", FormatType->OutputForm];
fReject = OpenWrite[workingdirectory<>"stat/dumpReject.txt", FormatType->OutputForm];

For[ ii = 1, ii < cntap, ii++,


	If[all_poly[[ii,1,1]] > 0, 
		Write[fAccept,all_poly[[ii,5,1]] ];
		Write[fAccept,all_poly[[ii,5,2]] ];
		Write[fAccept,all_poly[[ii,5,3]] ];
		Write[fAccept,all_poly[[ii,5,4]] ];
		Write[fAccept,all_poly[[ii,2,1,1]] ];
		Write[fAccept,all_poly[[ii,2,1,2]] ];
		Write[fAccept,all_poly[[ii,2,1,3]] ];
		Write[fAccept,all_poly[[ii,2,2,1]] ];
		Write[fAccept,all_poly[[ii,2,2,2]] ];
		Write[fAccept,all_poly[[ii,2,2,3]] ];
	]
	If[all_poly[[ii,1,1]] < 0, 
		Write[fReject,all_poly[[ii,5,1]] ];
		Write[fReject,all_poly[[ii,5,2]] ];
		Write[fReject,all_poly[[ii,5,3]] ];
		Write[fReject,all_poly[[ii,5,4]] ];
		Write[fReject,all_poly[[ii,2,1,1]] ];
		Write[fReject,all_poly[[ii,2,1,2]] ];
		Write[fReject,all_poly[[ii,2,1,3]] ];
		Write[fReject,all_poly[[ii,2,2,1]] ];
		Write[fReject,all_poly[[ii,2,2,2]] ];
		Write[fReject,all_poly[[ii,2,2,3]] ];
	]
];

Close[workingdirectory<>"stat/dumpAccept.txt"];
Close[workingdirectory<>"stat/dumpReject.txt"]

]


  End[]

  EndPackage[]




