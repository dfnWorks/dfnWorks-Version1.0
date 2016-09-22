(* ::Package:: *)

(***************************************************************************)
(*==================================================================================*)
Print["\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"];
Print["~~~ Program: DFNWorks / DFNGen V1.0, Mathematica, Linux ~~~~~~~~~~~~~"];
Print["~~~~~~~~~~~~ April 1st, 2015.  LA-CC-14-091 ~~~~~~~~~~~~~~~~~~~~~~~~~"];
Print["\nThis program was prepared at Los Alamos National Laboratory (LANL),"];
Print["Earth and Environmental Sciences Division, Computational Earth "];
Print["Science Group (EES-16), Subsurface Flow and Transport Team."];
Print[" All rights in the program are reserved by the DOE and LANL."];
Print[" Permission is granted to the public to copy and use this software "];
Print[" without charge, provided that this Notice and any statement of "];
Print[" authorship are reproduced on all copies. Neither the U.S. Government"];
Print[" nor LANS makes any warranty, express or implied, or assumes "];    
Print[" any liability or responsibility for the use of this software."];
Print["~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"];
Print["~~~Developers: Jeffrey D. Hyman, Scott L. Painter, Nataliia Makedonska,"];
Print["~~~~~~~~~~~~~~ Carl W. Gable, Satish Karra, Ahinoam Pollack, "];
Print["~~~~~~~~~~~~~~ Quan M. Bui, Jeremy A. Harrod, T.-L. Hsein, Terry A. Miller"];
Print[" Last update Apr. 1st, 2015, by N. Makedonska (nataliia@lanl.gov)"];
Print["~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"];
Print["\n"];
(*Code Name: DFNMain.m*)
(*Code Function: It is the main mathematica section of the code for producing
the mesh of a discrete fracture network. It creates polygons representative 
of the fractures according to criteria specified in input.m. The program
checks that the fractures it creates don't have problematic configurations
(such as intersections with lengths less than h). The program outputs 
files with relevant data (such as polygon and intersection coordinates)
to be used as input files for LaGriT.  
The program calls upon many functions/subroutines to preform its mission.*)

(***************************************************************************)
((*upload/read-in packages/functions that will be used in the Mathematica script*)
(*package path was previously defined as "Directory[]<>"/DFNgenerator/";" *)
Get[ packagepath <> "rotation.m"]; 
(*The package rotation.m includes the script: rotatePoints. *)
Get[ packagepath <> "dumpPoints.m" ]; 
(*The package dumpPoints.m includes the scripts: 
dumpPoints`dumpPolygonPoints, dumpPoints`dumpIntersectionPoints, dumpPoints`dumpParams, dumpPoints`dumpPermeability,
dumpPoints`dumpAperture, dumpPoints`dumpLengthInfo. *)
Get[ packagepath <> "generatingPoints.m"];  
(*The packahe generatingPoints.m includes the scripts:
lineFunction3D, generatingPoints`DiscretizeLineOfIntersection, generatingPoints`CreateIntersectionPoints,
generatingPoints`calculateNumberOfLineElements, generatingPoints`CreatePolygonPoints, generatingPoints`SampleFisher, 
generatingPoints`CheckIntersectionPoint. *)
Get[ packagepath <> "GeneratingPolygons.m"];
(*The package generatingpolygons.m includes the scripts:
 rotateNodes, TranslatePolygon, UnTranslatePolygon, GeneratingPolygons`CreateBoundingBox,
CreateSquare, GeneratingPolygons`CreateRectangle, distcreteEllipse, distcreteEllipsev2,
GeneratingPolygons`CreateEllipse, createConvexHull, CreateConvexPolygon, CleanupPolygons,
GeneratingPolygons`GenerateTheta. *)
Get[ packagepath <> "IntersectionChecking.m"]; 
(*The package IntersectionChecking.m includes the scripts:
CheckBoundingBox, LineOfIntersection, ReturnBoundingBox, 
LineIntersection3D, PointOfLineIntersection, IntersectionChecking`distanceFromNodes,
FindPointsOnIntersection, pointInPolygon, minDistanceBetweenPolygons,
polyIntersection, ConstructAdjanceyMatrix, IntersectionChecking`CheckForTripleIntersectionOnNew,
pointOnLineSeg, IntersectionChecking`pointToLineSeg, IntersectionChecking`lineSegToLineSeg, IntersectionChecking`lineSegToLineSegSep,
IntersectionChecking`findIntersections, parallel, IntersectionChecking`CheckForTripleIntersectionOnExisting, 
minDistanceBetweenpPoints, pointInBox. *)
Get[ packagepath <> "Domain.m"]; 
(*The package domain.m includes the scripts:
domain`GenerateBoundaryBox, FindNodesNextToIntersection, InsideDomain, 
OutsideDomain, domain`DomainTruncation, FindLineOfIntersection. *)


(*$Historyradius = 0;(* save memory: saving no history *)*)
(*==================================================================================*)
$HistoryLength = 0;(* save memory: saving no history *)
SetDirectory[workingdirectory];
(*==================================================================================*)
(* Polygon Data Structure: *)
(* The polygon structure stores almost all important information about the
accepted and attempted fractures. The structure is used in three variables:
"allpoly" - storing data of all attempted fractures (including accepted)
"poly" - storing data of all accepted fractures.
"newPoly" - storing data of the new attempted fractures before it is either
accepted or rejected.

For polygon i;
poly[[i,1]] = {FractureSetNumber, NumberOfNodes,  Intersection Counter, aperture}; 
poly[[i,2]] =  {NormalVector, TranslationVector, PermeabilityVector, faces, groupNum};
poly[[i,3,j]] = Vertices(x_j, y_j, z_j);
poly[[i,4]]]  = Bounding Box : {{minX, maxX}, {minY, maxY}, {minZ, maxZ}}};
poly[[i,5]]]  = {familyNumber,aspectRatio, xradius, yradius};
poly[[i,6]] = {area, volume};
*)

(*==================================================================================*)
(* time profile: create empty variables *)
timeCreatePoly = 0;
timeFindIntersection = 0;
timeCheckIntersection = 0;
timeBadIntersection = 0;
timeCheckBoundingBox = 0;
timeCheckForTripleIntersection = 0;
timeConstructAdjanceyMatrix = 0;
timeConstrDSet = 0;
timeSparse = 0;
timeLargestConnComp = 0;
timeConstruction = 0;
timeWriteToFile = 0;
timeWriteToFile2= 0;
(*==================================================================================*)
(*Vol, area, density counters *)
totalAcptArea = 0;
totalAcptVolume = 0;
currentDensity = 0;

(*==================================================================================*)
(* Define Output Files *)
lineOutputFile = "intersections.inp";
paramsOutputFile = "params.txt";
boundingOutputFile = "bounding.inp";
polyOutputFile = "polys.inp";
permOutputFile = "perm.dat";
apertureOutputFile = "aperture.dat";
lengthsOutputFile= "length.dat";
(*==================================================================================*)
(*define domain according to domain size. *)
domain=N[domainSize/2]; 
(*==================================================================================*)
(*define the streams to the files flaFile (final lengths of fractures
accepted) and errFile (a file listing all errors and warnings) *)

flaFile = workingdirectory<>"stat/length_acc";
fla= OpenWrite[flaFile, FormatType->OutputForm];
Close[flaFile];

errFile=workingdirectory<>"stat/warnings.txt";
OpenWrite[errFile];Close[errFile];

Off[RowReduce::luc];
(*==================================================================================*)
(*perform the input file check*)
(*the script inputCheck.m sets default values for variables.
It also checks that values given in the input file are in the correct range
and of the correct form.*)
(*the inputCheck.m file also includes a lot of useful information about 
the default values of variables and the meaning of certain variables. *)
(*==================================================================================*)
Get[ packagepath <> "inputCheck.m"]; 

Print["Number of fractures requested: ", nPoly,"\n"];
(*==================================================================================*)
(* Counters for histogram for reason for rejection *)
outside = 0;
shortIntersection = 0;
closeToEdge = 0;
closeToNode = 0;
closePointToEdge =0;
triple = 0;
accepted = 0;
truncated = 0;
deformed = 0;
(*==================================================================================*)
Print["Requested number of families for each shape: \n",
	nShape[[1]]," families of ellipses;\n",
	nShape[[2]]," families of rectangles;\n",
	nShape[[3]]," user defined ellipses;\n",
	nShape[[4]]," user defined rectangles;\n"];
(*==================================================================================*)
(*definitions of a few random things. *)

(* stop is a variable dicating the reason for stopping the program.
It is used in the end when printing out the report.
the default is stop=1 -> meaning that the desired number of fractures 
has been accepted. *)
stop = 1; 
boundaryBox = domain`GenerateBoundaryBox[domain];
(* array declaration *)
poly = ConstantArray[0, acPoly]; 
rejCaused = ConstantArray[0, acPoly]; (* rejc *)
(* If production mode is set to 0, that means 
the program is in debugging mode, and the programs
will remember information about all attempted fractures.
one can access information about the attempted fractures
by printing out allpoly. 
NOTE: rejection label & intersection 
counter in allpoly are not updated;
allpoly is only for debugging use *)
If[ productionMode == 0, 
   ap = OpenWrite[workingdirectory<>"stat/allpoly"]]; 

  fa = OpenWrite[workingdirectory<>"stat/familyRejected"];
  (* allpoly = ConstantArray[0, maxPoly]; *)


(* disjoint set implemented as an array *)
(* generates an 2\[Cross]acPoly array of nested lists containing elements of -1.
for example: if acPoly =3, then dset={{-1,-1,-1},{-1,-1,-1}}.*)
dset = ConstantArray[-1,{2,acPoly}]; 
(* adjacency matrix of intersection end points *)
intPts = ConstantArray[SparseArray[{},{acPoly,acPoly}],{2,3}]; 

(* adjacency matrix (yes and no) *)
(* will mark 1 in a matrix element if the fractures with the row and column
numbers intersect. *)  
adj = SparseArray[{},{acPoly,acPoly}]; 
(* create a matrix listing the familinty numbers of the accepted fractures. *)
familyac = ConstantArray[0, acPoly];

(* coutner of successive rejections *) 
reject = 0;

(* CouNTer of ACcepted fractures *)
cntac = 0; 
cntlengthchange=cntac;

(* CouNTer of ATtemPted fractures *)
cntatp = 1; 

(* record the size of largest connected component/group of fractures. *)
largestSize = 0; 

(* index of the largest set *)
set = 0; 

nn=0;
r=0;
(*create pipe to file for storing attempted (?) lengths *) 
lengthAtmpFile=workingdirectory<>"stat/length";
OpenWrite[lengthAtmpFile,FormatType->OutputForm]; 
Close[lengthAtmpFile];

printLengthAtmp:= (
fl=OpenAppend[lengthAtmpFile,FormatType->OutputForm];
Write[fl, lengths2Write];
Close[fl];);
(*==================================================================================*)
(***** Measure the amount of time to run the section *)
(* the time measurements will be summed up to come at the total amount of time
it takes to run the program *)
tmp4 = Timing[

(* The following if statement generates an array "family" of length maxPoly (maximum number of polygons 
that may be attempted to be created during the program). The family array is
a list of family codes, where each specific family code number appears 
according to its designated distribution (as defined in the input file by
famProb - an array of probabilities of occurrence of each family. see input file
for more details on its form). The family codes are as follows:
1: 1st family of ell, 
2: 1st family of rec, 
3: 1st family of usr-ell, 
4: 1st family of usr-rec,
5: 2nd family of ell,
6: 2nd family of rec, etc.
The following section also creates an array famCode, which is a list of family
codes present in the problem. So that for example, nShape of {3,2,0,0}, will
yield a famCode={1,5,9,2,6}. 
famProb is changed in the following section by the command Accumulate, where
each number it the addition of itself and all the numbers prior to it.
so that if the original famProb was {.2,.1,.3,.2,.2}, then the final famProb
is {0.2`,0.3`,0.6,0.8,1.}. What this section does is generate a random
arrary of numbers between 0 and 1 - called family. Then it loops over family
(Length[famProb] number of times), every loop keeping a variable of famProb
constant (note that this program uses the command Table, which in Mathematica
is quite similar to the traditional "for" loop). if the random probability is 
smaller than the variable of famProb, then the random probability is replaced
by the family code that has that probability. If the random probability is
bigger than the probability of famProb, than the random probability is left
unchanged. Thus, if the first variable in famProb is 0.2 - this means that 20
percent of the polygons need to belong to the first family mentioned in 
famCode. If there are elliptical families in the problem, this code is usually
1. The program will loop over all of the randomly generated numbers between
0 and 1. Since these numbers are ramdom, there is a 20% likelihood that they
will be less than 0.2. Therefore about 20% of the numbers in family will recieve
the family code 1, after the first loop. After completing the first loop, the 
program will move on to the next accumlated probability number, say 0.3, and all
random numbers between 0.2 and 0.3 will get the next famCode (note that once
a number in family has been changed from a random number to a famCode, it cannot
be changed again to a different famCode. For example, if an element of family
was changed to famCode 1, then it won't be changed to any other family code, 
since no famProb will be more than 1. 
Also, note that if family probabilties add up to less than 1, the program
will exit with an error message. If they add up to more than 1, the program
will continue and just assign famCodes until it reaches 100 percent (the purpose
of the addition +Max[famProb] and then its subtraction is so that if the 
probabilties add up to more than 1, there won't be a scenario that a family 
element that has been given a code of 1, will be less than an accumulated
probability and then will be changed to a different family code.*)  
(* famCode's length is total number of families *)	

famCode = Flatten[Table[Table[4(k-1)+ii,{k,nShape[[ii]]}],{ii,2}]]; 
(* whose size is total number of families *)
famProb = Accumulate[famProb];
family = RandomReal[1,maxPoly];

Do[ family = Table[If[family[[ii]] < famProb[[jj]], 
famCode[[jj]]+Max[famProb], family[[ii]]],
{ii,maxPoly}],{jj,Length[famCode]}];

family=Rationalize[family-ConstantArray[Max[famProb], maxPoly]];
userEllipseCodes=Flatten[Table[i*4+3, {i, 0, nShape[[3]]-1}]];
userRectCodes=Flatten[Table[i*4+4, {i, 0, nShape[[4]]-1}]];
family=Flatten[Prepend[family,{userEllipseCodes, userRectCodes}]]; 
(*define the length of the list of family codes *)  
numFamilyCodes=Length[Select[family, #<Infinity&]];

(*==================================================================================*)
(* generate list of polygon lengths for each family *)
(* The number of families for each shape are dictated by the variable
nShape in the input file. *) 

(* if a family or more of ellipses were requested *)

If[ nShape[[1]] > 0, (* if a family or more of ellipses were requested *)

(* If chosen distribution is 1 - create a list, "eln", of polygon lengths
based on a log normal distribution and the values assigned to emean and esd 
in the input file. The eln (Ellipses' lengths according to Log Normal 
distribution) list is of length 1.5*nPoly. *)
  

  
	If[Position[edistr,1]!={},

	eln = ConstantArray[0, nShape[[1]]]; 
	For[ ii = 1, ii <= nShape[[1]], ii++, 
         If [edistr[[ii]]==1, Print[" LogNormal Size Distribution for ellipses of family # ",ii];
		eln[[ii]] = N[LogNormalDistribution[ emean[[ii]],esd[[ii]] ]]]] ];
		
         
(* If chosen distribution is 2 - create a list, "epln", of polygon lengths
based on the power-law distribution and the values assigned to emin, emax and ealpha in
the input file. The epln (Ellipses' length according to Power Law distribution) 
Note: there is no built in equation for creating a power law distribution and thus, unlike the lognormal distribution,
it is written here from scratch*)

	
	If[Position[edistr,2]!={},
    edpl=1;
   (* coutner of successive rejections *) 
      ereject = ConstantArray[0,nShape[[1]]];
      ecntlengthchange = ConstantArray[0,nShape[[1]]];
    di=Round[nPoly/nShape[[1]]];
		
		epln=ConstantArray[ConstantArray[0,di], nShape[[1]]];
    For[nm=1, nm<=nShape[[1]], nm++,
      If [edistr[[nm]]==2, Print[" Power Law Size Distribution for ellipses of family # ",nm];
		For[ii=1, ii<=di,ii++, 
			R=ii*1/di;  
			epln[[nm, ii]]=emin[[nm]]*(1-R+R*(emin[[nm]]/emax[[nm]])^ealpha[[nm]])^(-1/ealpha[[nm]])
            ] 
        ]
	]
];    
     

(*if chosen distribution is 3 - exponential distribution*)

   If[Position[edistr,3]!={},
       eell =  ConstantArray[0, nShape[[1]]];
	For[ ii = 1, ii <= nShape[[1]], ii++, 
        If [edistr[[ii]]==3, Print[" Exponential Size Distribution for ellipses of family # ",ii];
		eell[[ii]] = N[ExponentialDistribution[ 1/ellmean[[ii]]]]]
	]	];
   

(* the theta angle is the angle between points of a discretized ellipse. *)
(* theta angles for all ellipse families *)
	theta = ConstantArray[0, nShape[[1]] ]; 
	For[ ii = 1, ii <= nShape[[1]], ii++, 	
		yradius = 1;
		xradius = easpect[[ii]] * yradius;
		theta[[ii]] = N[GeneratingPolygons`GenerateTheta[xradius, yradius, enumPoints[[ii]],h ]]
		]
	
];

If[ nShape[[2]] > 0, (* has rectangles *)
(* If chosen distribution is 1 - create a list, "rln", of polygon lengths
based on a log normal distribution and the values assigned to rmean and rsd
in the input file. The rln (rectangles' lengths according to Log Normal 
distribution)  *)
	If[Position[rdistr,1]!={},

		rln = ConstantArray[0, nShape[[2]]]; 
	For[ ii = 1, ii <= nShape[[2]], ii++, 
	If [rdistr[[ii]]==1, Print[" LogNormal Size Distribution for rectangles of family # ",ii];
	rln[[ii]] = N[LogNormalDistribution[ rmean[[ii]],rsd[[ii]] ]] ]
]
]

(* If chosen distribution is 2 - create a list, "rpln", of polygon lengths
based on the power-law distribution and the values assigned to rmin, rmax and ralpha in
the input file. The rpln (rectangles' length according to Power Law distribution) 
Note: there is no built in equation for creating a power law distribution and thus, unlike the lognormal distribution,
it is written here from scratch*)
	If[Position[rdistr,2]!={},
        rdpl=1;
       di=Round[nPoly/nShape[[2]]];
       rreject=ConstantArray[0,nShape[[2]]];
        rcntlengthchange = ConstantArray[0,nShape[[2]]];
		rpln=ConstantArray[ConstantArray[0,di], nShape[[2]]];
		
    For[nm=1, nm<=nShape[[2]], nm++,
If [rdistr[[nm]]==2, Print[" Power Law Size Distribution for rectangles of family # ",nm];
		For[ii=1, ii<=di,ii++, 
			R=ii*1/di;  
			rpln[[nm, ii]]=rmin[[nm]]*(1-R+R*(rmin[[nm]]/rmax[[nm]])^ralpha[[nm]])^(-1/ralpha[[nm]])
                    
            ] 
        ]
	] ];  

     (*if chosen distribution is 3 - exponential distribution*)

   If[Position[rdistr,3]!={},
       rell =  ConstantArray[0, nShape[[2]]];
	For[ ii = 1, ii <= nShape[[2]], ii++, 
    If [rdistr[[ii]]==3, Print[" Exponential Size Distribution for rectangles of family # ",ii];
		rell[[ii]] = N[ExponentialDistribution[ 1/rellmean[[ii]]]]]
		] ] ];
             

  If[ nShape[[3]] > 0, (* has user-speficied rectangles *)
	utheta = ConstantArray[0, nShape[[3]] ]; 
	For[ ii = 1, ii <= nShape[[3]], ii++, 	
		yradius = 1;
		xradius = ueaspect[[ii]] * yradius;
		utheta[[ii]] = N[GeneratingPolygons`GenerateTheta[xradius, yradius, uenumPoints[[ii]],h ]]
	]
]
];
(*==================================================================================*)
timeCreatePoly += tmp4[[1]];

(*==================================================================================*)
					(* begin inserting fractures *)
(*==================================================================================*)
(*Inserting fractures. *)
(*Beginning of the while loop.*)
(*Create polygons until the stopping parameter has been reached. *)
(*The default is that the program will stop when the number of accepted *) 
(*polygons (cntac) equals the number of requested polygons (nPoly). *)
(*==================================================================================*)

While[ If[ValueQ[density]==True, currentDensity <= density, nPoly > cntac], 
	(*set the value of toAddTriplePts to 0. *)
If[ValueQ[density]==True && nPoly <= cntac, Break[]];


	(***** time create poly *)
	tmp4 = Timing[
  	(*stop the program if only deterministic fractures were requested and
	they were all rejected. *) 
	If[numFamilyCodes<(cntatp) && nShape[[1]]==0 && nShape[[2]]==0, 
		Print["Cannot create more fractures because only requested user defined fractures.
All user defined fractures have already been attempted, but the program has not reached the requested
number of fractures."]
		Break[];
		]; 

	(*Creates polygons/fractures*)
	

(* n-th family of each shape *)
(* so that n=1, is either the first family of ellipses, or the first family 
of rectangles, etc. n=2, indictate that the polygon is either of the second
family of ellipses or rectangles, etc.*)
 
	n = Ceiling[family[[cntatp]]/4]; 
    fractureType=Mod[family[[cntatp]],4];



	Switch[ fractureType

	
	,1, (* case: ellipse *)
		(*translation Vector*)
	(*select a random coordinate within the domain to serve as the center*)
		(*of the polygon, the point to which to translate (t). *)
		(*the point is chosen from a domain slightly larger than the domain
		  b/c there could be fractures that have a center outside the domain
		  but have part of the fracture inside the domain. *) 
		t = { RandomReal[{-domain[[1]]-domainIncrease4Random,domain[[1]]+domainIncrease4Random}], 
			RandomReal[{-domain[[2]]-domainIncrease4Random,domain[[2]]+domainIncrease4Random}], 
			RandomReal[{-domain[[3]]-domainIncrease4Random,domain[[3]]+domainIncrease4Random}]};
        (*create the polygon normal through the Fisher distribution,*)
		(*according to ephi, ekappa, and etheta, detrmined in the input file.*) 
	    
		
		normal = N[generatingPoints`SampleFisher[etheta[[n]], ephi[[n]], ekappa[[n]]]];
		normal = Normalize[normal];  
 		
		(* Create Rectangle from log normal distriubtion and fixed aspect 
		ratio*)
		yradius = 0;
        nn++;
		While[ 5*yradius < h,
             (* acpp=cntac;*)
                           
              If[edistr[[n]]==1, 
               (*if the distribution is log-normal, select a random number from
				the list of eln (Ellipses' lengths according to Log Normal 
				distribution) to serve as the y-length. *)
				yradius=Random[eln[[n]]]  ];

               If[edistr[[n]]==2,
               
			(* ri is the "r^th" number of the list of epln (Ellipses' 
			lengths according to Power-Law distribution*)
             		If[ereject[[n]]==epowerLawReject[[n]], ecntlengthchange[[n]]++;   
			
				(*Print["Just made a power law determined elliptical smaller because rejected too 
					many in a row of size", epln[[n,r]]. "Try making the domain bigger." ]; *)
			ereject[[n]]=0];
				r=di-ecntlengthchange[[n]];
               If[r>nPoly,errMessage=StringJoin["The program stopped inserting fractures
				because it tried to insert fractures (elliptical power law determined)
				at increasingly smaller sizes and has reached the minimum size. 
				Try making the domain bigger."];
				printErr];
				ri=epln[[n,r]];
            yradius=ri  
				];


  (* Exponential Distribution *)	
		If [edistr[[n]]==3,
			yradius=Random[eell[[n]]]];

	(*constant fracture radius*)
		If [edistr[[n]]==4,
			yradius=econst[[n]]
             ] ;
(*it is writing out the radius, not length*)
				lengths2Write=yradius; 
				printLengthAtmp 
		];
		(*find the xradius based on the yradius found above, and the aspect
		ratio determined in the input file.*)
		xradius = easpect[[n]]*yradius;
		newPoly = GeneratingPolygons`CreateEllipse[ normal, t, xradius, yradius, enumPoints[[n]], Global`theta[[n]], 2*Pi, family[[cntatp]],easpect[[n]] ];
       
		newPoly[[5,1]] = family[[cntatp]]; 
        newPoly[[5,2]] = easpect[[n]];
        newPoly[[5,3]] = xradius;
        newPoly[[5,4]] = yradius;
         
	,2, (* rectangle *)
		(*select a random coordinate within the domain to serve as the center*)
		(*of the polygon, the point to which to translate (t). *)
		t = { RandomReal[{-domain[[1]]-domainIncrease4Random,domain[[1]]+domainIncrease4Random}], 
			RandomReal[{-domain[[2]]-domainIncrease4Random,domain[[2]]+domainIncrease4Random}], 
			RandomReal[{-domain[[3]]-domainIncrease4Random,domain[[3]]+domainIncrease4Random}] };
        (*create the polygon normal through the Fisher distribution,*)
		(*according to ephi, ekappa, and etheta, detrmined in the input file.*)
         
		normal = N[generatingPoints`SampleFisher[rtheta[[n]], rphi[[n]], rkappa[[n]]]]; 
		normal = Normalize[normal];  
	
		(* Create Rectangle from log normal distriubtion and fixed aspect ratio*)
		yradius = 0;
		While[ 5*yradius < h,
			(*if the distribution is log-normal, select a random number from
				the list of rln (Rectangles' lengths according to Log Normal 
				distribution) to serve as the y-length. *)
			If[rdistr[[n]]==1,
			yradius = Random[rln[[n]]] ];
		
						
			(* ri is the "r^th" number of the list of rpln (Recftangles' 
			lengths according to Power-Law distribution*)
			If[rdistr[[n]]==2,
		
             If[rreject[[n]]==rpowerLawReject[[n]], rcntlengthchange[[n]]++; 
				(*Print["just made a power law determined rectangular smaller because rejected too 
					many in a row of size", rpln[[n, r]],". Try making the domain bigger."]; *)
				rreject[[n]]=0];
				r=di-rcntlengthchange[[n]];
               If[r>nPoly,errMessage=StringJoin["The program stopped inserting fractures
				because it tried to insert fractures (rectangular power law determined)
				at increasing smaller sizes and has reached the minimum size. 
				Try making the domain bigger maybe."]
				printErr];

				ri=rpln[[n,r]]; 
				yradius=ri  ];
			 (* Exponential Distribution *)	
		If [rdistr[[n]]==3,
			yradius=Random[rell[[n]]]];

	(*constant fracture radius*)
		If [rdistr[[n]]==4,
			yradius=rconst[[n]]
			];
(*it is writing out the radius, not length*)
				lengths2Write=yradius; 
				
				printLengthAtmp;  

		];

		(*find the xradius based on the yradius found above, and the aspect
		ratio determined in the input file.*)
		xradius = raspect[[n]]*yradius;
	
   
		newPoly = GeneratingPolygons`CreateRectangle[ normal, t, xradius, yradius, 2*Pi,family[[cntatp]],raspect[[n]] ];
        
        newPoly[[5,1]] = family[[cntatp]];
        newPoly[[5,2]] = raspect[[n]];
        newPoly[[5,3]] = xradius;
        newPoly[[5,4]] = yradius;
		

	,3, (* user-defined ellipse *)
		t = uetanslation[[n]];
        normal = uenormal[[n]];
		normal = Normalize[normal];  
		yradius = ueb[[n]];
		xradius = ueaspect[[n]]*yradius;
		lengths2Write=N[yradius/2]; printLengthAtmp;
		newPoly = GeneratingPolygons`CreateEllipse[ normal, t, xradius, yradius, uenumPoints[[n]], utheta[[n]], 0, family[[cntatp]], ueaspect[[n]]];

    	newPoly[[5,1]] = family[[cntatp]];
        newPoly[[5,2]] = ueaspect[[n]];
        newPoly[[5,3]] = xradius;
        newPoly[[5,4]] = yradius;

	,0, (* user-specified rectangle *)
        
		If[ValueQ[userDefCoordRec]==True,   
				 
			newPoly=GeneratingPolygons`createUserDefinedCoordRec[userDefCoordRec[[n]],family[[cntatp]]];
 				
			
			,(*else*)
		
			Print["You have defined a user specified rectangle according to 
			parameters."];
			t = urtanslation[[n]];
			normal = urnormal[[n]]; 
			normal = Normalize[normal];  
             
			yradius = urb[[n]];
			lengths2Write=N[yradius]; printLengthAtmp;
			xradius = uraspect[[n]]*yradius;
			(* GeneratingPolygons`CreateRectangle creates a rectangle square and translates it by a 
			vector t and gives the plane the normal n. The function is part of the 
			package GeneratingPolygons. *)		
			newPoly = GeneratingPolygons`CreateRectangle[ normal, t, xradius, yradius,  
			0, family[[cntatp]],uraspect[[n]]];
             newPoly[[5,1]] = family[[cntatp]];
        newPoly[[5,2]] = uraspect[[n]];
        newPoly[[5,3]] = xradius;
        newPoly[[5,4]] = yradius
		]	
];
(*==================================================================================*)
	(* Truncate Polygon to be in the Bounding Domain *)	
	newPoly = domain`DomainTruncation[newPoly,domain,boundaryBox,h,eps]; 				  
    
  
	(*domain`DomainTruncation returns status numbers. status number -5 means
	that only two or less vertices are inside the domain - this is called
	an "outside" rejection.*)
	If[ newPoly[[1,1]] == -5,  (* outside rejection *)
		outside++;
		
		Write[fa, newPoly[[5,1]]];

				(*print out a special error message if a deterministic fracture is rejected. *) 
				If[fractureType==3, errMessage=StringJoin["User defined elliptical fracture ", ToString[cntatp]," was rejected. 
					Reason: The fracture had two or less verticies inside the domain. 
					Please adjust the fracture's parameters in the input file.\n"];
					printErr];
					If[fractureType==0, errMessage=StringJoin["User defined rectangular fracture ", ToString[cntatp]," was rejected. 
					Reason: The fracture had two or less verticies inside the domain. 
					Please adjust the fracture's parameters in the input file.\n"]; 
					printErr];

		(*if in debugging mode:*)
		If[ productionMode == 0, (*allpoly[[cntatp]] = newPoly;*)
			Write[ap, newPoly]
			]; 
		cntatp++; 
		
		(*stop the program, if it attempted to create more than max number of 
		polygons.*)
		If[ cntatp == maxPoly+1, (* stopping criteria *)
			stop = 2; 
			
			Break[];
		];
		Continue[] 
	];
(*==================================================================================*)
	newPoly = GeneratingPolygons`CreateBoundingBox[newPoly];
print["made it here"]
	(*****)
	];
	timeCreatePoly += tmp4[[1]];

	(* begin checking intersections *)
	
	
	intPtsii = ConstantArray[SparseArray[{},acPoly],{2,3}]; 
	
	(* adj *)(* temporary vector of intPts[[cntac]] *)
	(* Creates a vector of length acPoly, filled with zeros. *)
	adjii = SparseArray[{},acPoly];

	(* To run in parallel, uncomment the following *)(* NOTE: statistics of rejection reasons are messed up when running in parallel *)
	(*SetSharedVariable[newPoly,intPtsii,adjii,packagepath,shortIntersection,closeToEdge,closeToNode,triple];
	SetSharedFunction[Break];
	ParallelEvaluate[Needs[ "IntersectionChecking`", packagepath <> "IntersectionChecking.m" ]]; 
	Parallel*)
	(*==================================================================================*)	
(* In this Do, jj goes from 1 to cntac-1 (count_accepted -1). *)
(* i.e. it runs on all the fractures already accepted, and ensures that 
    the new fracture doesn't cause problomatic intersecions with them. *)

       Do[ (* If[ newPoly[[1,1]] > 0, (* since newPoly is syncronized in all parallelized threads *) *)


		(*****)
		tmp50 = Timing[

		(* find intersections *)
		intersections = IntersectionChecking`findIntersections[ newPoly,poly[[jj]],eps ]];

		timeFindIntersection += tmp50[[1]];

(*==================================================================================*)
		(* if two fractures dont intersect, check the next fracture *)
		If[ intersections[[1]] == 0, Continue[] ]; 
(*==================================================================================*)	
		(* else, two fractures intersect *)

	
		tmp54 = Timing[

		(* check length of intersection *)
		(* make sure the intersections aren't too short. *)
		(*intersections[[2,1]] and [[2,2]] are the two points of intersection
		 of the polygons' perimeter. *) 
		
	If[Norm[intersections[[2,1]]-intersections[[2,2]]] <= h, 
			shortIntersection++;


			newPoly[[1,1]] = -2;
			(*List polygon jj as the cause of the rejection *)
			rejCaused[[jj]]++;
	(*print out a special error message if a deterministic fracture is rejected. *) 
	If[fractureType==3, errMessage=StringJoin["User defined elliptical fracture ", ToString[cntatp]," was rejected. 
	    Reason: Short intersection. Please adjust its parameters in the input file.\n"];
		printErr];
	If[fractureType==0, errMessage=StringJoin["User defined rectangular fracture ", ToString[cntatp]," was rejected. 
	    Reason: Short intersection. Please adjust its parameters in the input file.\n"];
		printErr];
			Break[] (* short intersection rejection *)
		];
     If[ newPoly[[1,1]] < 0, Break[] ]; (* will be removed, close to edge rejection *)
		
	(* check distance to edges *)
(*==================================================================================*)
		(* if intersection is completely inside either fracture *)
		If[ intersections[[1]] == 1 || intersections[[1]] == 2, 
	(* if intersection is completely inside either fracture *)
		(* could be collected in a function block *)
		(* If[ intersections[[1]] == 1, tmpPoly = newPoly, tmpPoly = poly[[jj]] ]; 
		(* intersection is inside tmpPoly *) *)
			tmpPoly = newPoly;
		(* kk runs on the number of verticies in the new polygon/fracture *)  
		
			For[ kk = 1, kk <= tmpPoly[[1,2]], kk++, 
				If[ kk == tmpPoly[[1,2]], next = 1, next = kk+1 ];
				(* edge is a list of two adjacent vertices *)
				(* the for loop goes thorough all edges of the new fracture
				and checks the distance between the edge and the line of intersection.
				If the distance is less than half the length scale, then the new
				fracture if rejected. *)

				edge = {tmpPoly[[3,kk]],tmpPoly[[3,next]]};
				(* find distance between intersection and the edge *)
				dist = IntersectionChecking`lineSegToLineSeg[edge,intersections[[2]],eps];
					If[dist <= h/2 && dist > eps,
					closeToEdge++;
					newPoly[[1,1]] = -6;
					(*List polygon jj as the cause of the rejection *)					
					rejCaused[[jj]]++;
         (*print out a special error message if a deterministic fracture is rejected. *) 
		If[fractureType==3, errMessage=StringJoin["User defined elliptical fracture ", ToString[cntatp]," was rejected. 
	        Reason: Intersection too close to a vertex. Please adjust its parameters in the input file.\n"];
		printErr];
		If[fractureType==0, errMessage=StringJoin["User defined rectangular fracture ", ToString[cntatp]," was rejected. 
	        Reason:Intersection too close to a vertex. Please adjust its parameters in the input file.\n"];
		printErr];
					Break[] 
				]
		 	];

			If[ newPoly[[1,1]] < 0, Break[] ]; (* will be removed, close to edge rejection *)

			tmpPoly = poly[[jj]];
			(*runs on kk, the number of vertices of an existing fracture. *) 
			For[ kk = 1, kk <= tmpPoly[[1,2]], kk++, 
				If[ kk == tmpPoly[[1,2]], next = 1, next = kk+1 ];
				(* edge is a list of two adjacent vertices of an existing fracture. 
				the loop checks the distance between the fracture edge that these
				two vertices define and the line of intersection. *)
				edge = {tmpPoly[[3,kk]],tmpPoly[[3,next]]};
				(* find distance between intersection and the edge *)
				dist = IntersectionChecking`lineSegToLineSeg[edge,intersections[[2]],eps]; 
				
				If[ dist <= h/2 && dist > eps, 
					closeToEdge++;
					newPoly[[1,1]] = -6;
					rejCaused[[jj]]++;
					Break[] 
				]
			];
			
           If[ newPoly[[1,1]] < 0, Break[] ]; (* will be removed, close to edge rejection *)


			
         (* discretize the intersection and check the distance between intersection points and edges *)
                (*Create an arrary endPoint={{0,0,0},{0,0,0}}. *)
				endPoints = ConstantArray[{0,0,0}, 2];
				(*call the intersection coordinates endPoints[[1]] and endPoints[[2]]. *)
				endPoints[[1]] = intersections[[2,1]];
				endPoints[[2]] = intersections[[2,2]] ;
          
				(*Discretize Line of Intersection*)
				tmpPoints = generatingPoints`DiscretizeLineOfIntersection[endPoints, h];
                firstP=0;
                lastP=0;
				(*number of points in the line of intersections. *)
                np=Length[tmpPoints];
                (*number of points minus the beginning and end points. *)
				lengthNew=np-2;
                tmpPoly = newPoly;
                tmpPolyInt= poly[[jj]]; 
       
	    (* loop on intersection points, if there are 10 or less points - check them all  *)       
             For[ ip = 2, ip <= np-1 ,ip++,  
               
                ipn=generatingPoints`CheckIntersectionPoint[tmpPoly, tmpPoints[[ip]],h, eps];
                ipni=generatingPoints`CheckIntersectionPoint[tmpPolyInt, tmpPoints[[ip]],  h, eps];
              
               If[ ipn == tmpPoly[[1,2]] && ipni==tmpPolyInt[[1,2]] , firstP=ip; Break[]]
                 ];

             For[ ip = np-1, ip >= 2 ,ip--,  
               
                ipn=generatingPoints`CheckIntersectionPoint[tmpPoly, tmpPoints[[ip]],h, eps];
                ipni=generatingPoints`CheckIntersectionPoint[tmpPolyInt, tmpPoints[[ip]],  h, eps];
            
               If[ ipn == tmpPoly[[1,2]] && ipni==tmpPolyInt[[1,2]] , lastP=ip; Break[]]
                 ];
                 If[lastP==np-1, lastP=np];
                 If[firstP==2, firstP=1];
            
            If[(lastP-firstP)<2, newPoly[[1,1]] = -7;
				            rejCaused[[jj]]++;
                             closePointToEdge++]; 
                             
			If[ newPoly[[1,1]] < 0, Break[] ] (* will be removed, close to edge rejection *)

		];

		(* check distance between intersection end point and edge *)
		If[ intersections[[1]] == 3 , 
		(* if intersection intersects an edge on each fracture *)

			tmpPoly = newPoly;
			For[ kk =  1, kk <= tmpPoly[[1,2]], kk++, 
				If[ kk == tmpPoly[[1,2]], next = 1, next = kk+1 ];
				edge = {tmpPoly[[3,kk]],tmpPoly[[3,next]]};
					dist1 = IntersectionChecking`pointToLineSeg[intersections[[2,1]],edge]; 
				(* find distance between 1st end point of the intersection and an edge *)
				dist2 = IntersectionChecking`pointToLineSeg[intersections[[2,2]],edge]; 
				(* find distance between 2nd end point of the intersection and an edge *)
					If[ dist1 <=h && dist2 <=h, 
					dist = Max[dist1,dist2],
					dist = Min[dist1,dist2]];
        		If[ (dist <= h/2 && dist > eps) || shint==1 ,
					closeToEdge++;
					
					newPoly[[1,1]] = -6;
					rejCaused[[jj]]++;
		(*print out a special error message if a deterministic fracture is rejected. *) 
			If[fractureType==3, errMessage=StringJoin["User defined elliptical fracture ", ToString[cntatp]," was rejected. 
                Reason: Intersection too close to a vertex. Please adjust its parameters in the input file.\n"];
		printErr];
		If[fractureType==0, errMessage=StringJoin["User defined rectangular fracture ", ToString[cntatp]," was rejected. 
	        Reason: Intersection too close to a vertex. Please adjust its parameters in the input file.\n"];
		printErr];
					Break[] 
				]
			];

		If[ newPoly[[1,1]] < 0, Break[] ]; (* will be removed, close to edge rejection *)

	(*!!!!!!!*)					  
			tmpPoly = poly[[jj]]; 
			For[ kk = 1, kk <= tmpPoly[[1,2]], kk++, 
				If[ kk == tmpPoly[[1,2]], next = 1, next = kk+1 ];
				edge = {tmpPoly[[3,kk]],tmpPoly[[3,next]]};
				dist1 = IntersectionChecking`pointToLineSeg[intersections[[2,1]],edge]; (* find distance between 1st end point of the intersection and an edge *)
				dist2 = IntersectionChecking`pointToLineSeg[intersections[[2,2]],edge]; (* find distance between 2nd end point of the intersection and an edge *)
				If[ dist1 <=h && dist2 <=h, dist = Max[dist1,dist2], dist = Min[dist1,dist2]];
              
				If[ (dist <= h/2 && dist > eps) || shint==1 ,
					closeToEdge++;
					newPoly[[1,1]] = -6;
					rejCaused[[jj]]++;
					Break[] 
				]
			];

         If[ newPoly[[1,1]] < 0, Break[] ]; (* will be removed, close to edge rejection *)

    (* discretize the intersection and check the distance between intersection points and edges *)
                endPoints = ConstantArray[{0,0,0}, 2];
				endPoints[[1]] = intersections[[2,1]];
				endPoints[[2]] = intersections[[2,2]] ;
          
				(*Discretize Line of Intersection*)
				tmpPoints = generatingPoints`DiscretizeLineOfIntersection[endPoints, h];
                firstP=0;
                lastP=0;
                np=Length[tmpPoints];
                lengthNew=np-2;
                tmpPoly = newPoly;
                tmpPolyInt= poly[[jj]]; 
           (* loop on intersection points, if there are 10 or less points - check them all  *)       
              For[ ip = 2, ip <= np-1 ,ip++,  
               
                ipn=generatingPoints`CheckIntersectionPoint[tmpPoly, tmpPoints[[ip]],h, eps];
                ipni=generatingPoints`CheckIntersectionPoint[tmpPolyInt, tmpPoints[[ip]],  h, eps];
              
               If[ ipn == tmpPoly[[1,2]] && ipni==tmpPolyInt[[1,2]] , firstP=ip; Break[]]
                 ];

             For[ ip = np-1, ip >= 2 ,ip--,  
               
                ipn=generatingPoints`CheckIntersectionPoint[tmpPoly, tmpPoints[[ip]],h, eps];
                ipni=generatingPoints`CheckIntersectionPoint[tmpPolyInt, tmpPoints[[ip]],  h, eps];
            
               If[ ipn == tmpPoly[[1,2]] && ipni==tmpPolyInt[[1,2]] , lastP=ip; Break[]]
                 ];
                 If[lastP==np-1, lastP=np];
                 If[firstP==2, firstP=1];
            
         If[(lastP-firstP)<2, newPoly[[1,1]] = -7;
				            rejCaused[[jj]]++;
                             closePointToEdge++ ]; 
                          
              If[ newPoly[[1,1]] < 0, Break[] ]; (* will be removed, close to edge rejection *)
						  
			(* check distance to vertices *)
		(* If[ Mod[family[[cntatp]],4] == 0 || Mod[family[[cntatp]],4] == 2, (* if fracture is rectangular *) *)
			(* check the distance from the intersection to all vertices on the 2 polygons *)
			dist = IntersectionChecking`distanceFromNodes[newPoly, poly[[jj]], intersections[[2]], h];
			
			If[(* newPoly[[1,1]] < 0 ||*) dist <= h/2 && dist > eps, 
				closeToNode++;
				newPoly[[1,1]] = -1;
				rejCaused[[jj]]++;
				Break[] (* close to vertex rejection *)
			]

		(*****)
		]
        ];

		timeBadIntersection += tmp54[[1]];

		(*****)
		tmp55 = Timing[

		(* check triple intersection on the intersected (old) fractures *)
		intIdxjj = Flatten[Drop[Map[First,ArrayRules[adj[[jj]]]],-1]]; (* indices of fractures intesecting with jj *)
		(* intIdxjj = Flatten[Drop[Map[First,ArrayRules[intPts[[1,1,jj]]],-1]]; (* indices of fractures intesecting with jj *)*)
		intIdxjj = Sort[intIdxjj]; (* ArrayRules doesn't display in order *)
		intPtListjj = If[ Length[intIdxjj] > 0, Normal[ intPts[[All,All,jj,intIdxjj]] ], {} ] (* list of intersections end points on fracture jj *)

		(*****)
		];
		timeSparse += tmp55[[1]];

		(*****)
		tmp53 = Timing[

		flag = IntersectionChecking`CheckForTripleIntersectionOnExisting[intPtListjj, intersections[[2]], h, eps]; 
		(* flag = # fracutre on which triple intersection occurs *)
	
	If[(* newPoly[[1,1]] < 0 ||*) flag != 0, 
		(* if two intersections intersect or if the distance between any points on different intersections is < h *)
			triple++;
			newPoly[[1,1]] = -3;
	(*print out a special error message if a deterministic fracture is rejected. *) 
		If[fractureType==3, errMessage=StringJoin["User defined elliptical fracture ", ToString[cntatp]," was rejected. 
					Reason: Created a problematic triple intersection. Please adjust the fracture's parameters in the input file.\n"];
					printErr];
					If[fractureType==0, errMessage=StringJoin["User defined	rectangular fracture ", ToString[cntatp]," was rejected. 
					Reason: Created a problematic triple intersection. Please adjust the fracture's parameters in the input file.\n"];
					printErr];
			rejCaused[[jj]]++;
		
			rejCaused[[intIdxjj[[flag]]]]++;
	
Break[] (* triple intersection rejection *)
		]]; 
			
		(*****)

		timeCheckForTripleIntersection += tmp53[[1]];

		(* the intersection is valid if it makes it here *)

		(*****)
		tmp55 = Timing[

		(* update the temporary adj matrix *)
		intPtsii[[All,All,jj]] = intersections[[2]];
		
		adjii[[jj]] = 1

		(*****)
		];
		timeSparse += tmp55[[1]]

	, { jj, cntac } (* jj goes from 1 to cntac *)
	]; (* end Do all existing fractures. the Do that has tmp50 = Timing[ 
		written after it. *)
 
	If[ newPoly[[1,1]] < 0,  (* bad intersection rejection *)
		If[ productionMode == 0, (*allpoly[[cntatp]] = newPoly;*) Write[ap, newPoly]; ]; 
 
		cntatp++; 
       Write[fa, newPoly[[5,1]]];
		If[ cntatp == maxPoly+1, (* stopping criteria *)
			stop = 2; 
			Print["Stopped: maximum attempt reached"]; 
			Break[]
		];
If [Mod[newPoly[[5,1]]-1,4]==0 && edpl==1, ereject[[n]]++];

If [Mod[newPoly[[5,1]]-2,4]==0 && rdpl==1, rreject[[n]]++];

        
		reject++; 
		If[ reject == rejPoly, 
			stop = 3; 
			Print["Stoppted: too many rejections in a row."]; 
			(*If[largestSize==0, errMessage=StringJoin["Error: Aborting Program. There is no ",
			"fracture network. Reached rejPoly number of rejections in a row and no fractures intersect. ",
			"This program does not work for one fracture, or many disconnected fractures. ",
			"Perhaps you could consider one of the following options: ",
			"making the fractures bigger, the domain smaller, increasing the number ",
			"of fractures, increasing the variable rejPoly in the file DFNMain ",
			"(which dictates the maximum number of fractures that can be rejected in a row)."]; 
			printErr; 
			If[ValueQ[test]==True, Goto[end]];
			Abort[]];*)
			Break[]
		];
		(* else, continue inserting fractures *)
		Continue[] 
	]; (* end if bad intersection *)

	(*****)
	tmp55 = Timing[
	(* indices of fractures intersecting with ii *)
	
	intIdxii = Flatten[Drop[Map[First,ArrayRules[adjii]],-1]]; 
	
	(* ArrayRules doesn't display in order *)
	intIdxii = Sort[intIdxii]; 
	intPtListii = If[Length[intIdxii] > 0, Normal[ intPtsii[[All,All,intIdxii]] ], {}] (* list of intersection end points on fracture ii *)

	(*****)
	];
	timeSparse += tmp55[[1]];

	(*****)
	tmp53 = Timing[

	(*  check triple intersections (distance between any two intersections) on the new fracture *)
	flag = IntersectionChecking`CheckForTripleIntersectionOnNew[intPtListii, h, eps]; (* flag = # fractures involved in the triple intersection *)
	If[ flag != {0,0},  (* triple intersection rejection *)
		triple++;
         Write[fa, newPoly[[5,1]]];
		newPoly[[1,1]] = -3;
		rejCaused[[intIdxii[[flag[[1]]]]]]++;
		rejCaused[[intIdxii[[flag[[2]]]]]]++;

If[ productionnMode == 0, (*allpoly[[cntatp]] = newPoly;*) Write[ap, newPoly]; ]; 
		
		cntatp++; 
		
		If[ cntatp >= maxPoly,  (* stopping criteria *)
			stop = 2; 
			Print["Stopped: maximum attempt reached."]; 
			(*If[largestSize==0, errMessage=StringJoin["Error: Aborting Program. There is no ",
			"fracture network. Reached maxPoly number of tries and no fractures intersect. ",
			"This program does not work for one fracture, or many disconnected fractures. ",
			"Perhaps you could consider one of the following options: ",
			"making the fractures bigger, the domain smaller, increasing the number ",
			"of fractures, increasing the variable maxPoly in the file DFNMain ",
			"(which dictates the maximum number of fractures that can be attempted)."]; 
			printErr; Abort[]];*)
			Break[];
		];
If [Mod[newPoly[[5,1]]-1,4]==0 && edpl==1, ereject[[n]]++];

If [Mod[newPoly[[5,1]]-2,4]==0 && rdpl==1, rreject[[n]]++];
       
		reject++; 
		If[ reject == rejPoly,
			stop = 3; 
			Print["Stoppted: too many rejections in a row."]; 
	(*		If[largestSize==0, errMessage=StringJoin["Error: Aborting Program. There is no ",
			"fracture network. Reached acPoly number of tries and no fractures intersect. ",
			"This program does not work for one fracture, or many disconnected fractures. ",
			"Perhaps you could consider one of the following options: ",
			"making the fractures bigger, the domain smaller, increasing the number ",
			"of fractures, increasing the variable acPoly in the file DFNMain ",
			"(which dictates the maximum number of fractures that can be accepted)."]; 
			printErr; Abort[]];*)
			Break[]
		];
		(* else, continue inserting fractures *)
		Continue[] 
	]

	(*****)
	];
	timeCheckForTripleIntersection += tmp53[[1]];

	(* the fracture is accepted if it makes it here *)

	fla= OpenAppend[flaFile, FormatType->OutputForm];
	Write[fla, xradius ];
	Close[flaFile];
 
	(*****)
	tmp30 = Timing[ 

	(* dset *)(* update disjoint sets *)		

	Do[
		

		root = jj; (* the root of fracture jj *)
		While[ dset[[1,root]] > 0, (* while not a root *)
			
curr = root;
			root = dset[[1,root]]; 
			dset[[1,curr]] = (cntac+1) (* compress tree height *)
	
	];

	
	If[ root != (cntac+1),  (* if fractures cntpoly and jj are in the same set *)
			dset[[1,(cntac+1)]] += dset[[1,root]]; (* update the size of set *)
			
			(* update size of largest set *)
			(*If[ dset[[1,(cntac+1)]] < largestSize,
				set = (cntac+1); largestSize = dset[[1,(cntac+1)]] ]; *)
			
			dset[[1,root]] = (cntac+1)  (* redirect the tree *)
		]
	, {jj, intIdxii} (* do jj in indices of fractures intersecting with ii *)
	](* end do *)


	(*****)
	];
	timeConstrDSet += tmp30[[1]];

	(*****)
	tmp55 = Timing[

	(* store intersection end points *) (* note: intPts is lower-triangle *)
	intPts[[All,All,(cntac+1)]] = intPtsii; 
	adj[[(cntac+1)]] = adjii;

	(* store intersection end points *) (* makes adj symmetric *)
	intPts[[All,All,All,(cntac+1)]] = intPtsii; 
	adj[[All,(cntac+1)]] = adjii;
	newPoly[[1,3]] = Length[intIdxii]; (* update number of intersections *)
	If[ newPoly[[1,3]] > 0, poly[[intIdxii,1,3]]++ ] (* update number of intersections *)

	(*****)
	];
	timeSparse += tmp55[[1]];

	Switch[ newPoly[[1,1]], 1, accepted++, 2, truncated++, 3, deformed++ ]; (* update statistics *)

(*==================================================================================*)
(*area and volume for each accepted polygons*)
(*total area and volume for all accepted polygons (before isolated fractures removed*)


(*For[i=1, i<=cntac, i++,    (*Begin Loop through all polygons*)*)


(* if number of vertices = 3, calculate the triangle area directly *)
If [newPoly[[1,2]]==3, 
	xProd = MyCrossProduct`MyCross[(newPoly[[3,2]]-newPoly[[3,1]]),(newPoly[[3,3]]-newPoly[[3,1]])];
	newPoly[[6,1]] = (Sqrt[(xProd.xProd)])*.5;(*area*) (* 1/2 * vector magnitude*)
	newPoly[[6,2]] = newPoly[[6,1]] * newPoly[[1,4]];(*area * aperture *)
	totalAcptArea += newPoly[[6,1]];
	totalAcptVolume += newPoly[[6,2]];
];

(*If Polygon has more than 3 vertices*)
If [newPoly[[1,2]]>3,
	(*polyArea sums the triangles' area for each polygon *)
	polyArea= 0; (*reset accumulator to 0 each for each polygon*)

	(* get coordinate within polygon *)
	insidePoint = newPoly[[3,1]] + .5 * (newPoly[[3,3]]-newPoly[[3,1]]);
	For[j=1, j<=newPoly[[1,2]]-1, j++,  (*the last triangle in the polygon is done seperately else newPoly[[3,j+1]] will overflow *)
		xProd =MyCrossProduct`MyCross[(insidePoint-newPoly[[3,j]]),(insidePoint-newPoly[[3,j+1]])];
		triArea = (Sqrt[xProd.xProd])*.5; (* 1/2 * vector magnitude*)

		polyArea += triArea;  
	   ]  
	(*last triangle of polygon*) clear[xProd];

	xProd = MyCrossProduct`MyCross[(insidePoint-newPoly[[3,newPoly[[1,2]]]]), (insidePoint-newPoly[[3,1]])]; 
	triArea = (Sqrt[xProd.xProd])*.5; (* 1/2 * vector magnitude*)

	polyArea += triArea; (*now has total area of polygon*) 
	newPoly[[6,1]] = polyArea;
	newPoly[[6,2]] =  newPoly[[6,1]] * newPoly[[1,4]]; (*area * aperture*) 
	totalAcptArea += newPoly[[6,1]];
	totalAcptVolume += newPoly[[6,2]];
  ];
currentDensity = totalAcptArea/domainVol;


(*==================================================================================*)
    

	poly[[(cntac+1)]] = newPoly;
     If[ productionMode == 0, (*allpoly[[cntatp]] = newPoly;*) Write[ap, newPoly ];];
	
	familyac[[(cntac+1)]] = family[[cntatp]]; (* store the family of the accepted fracture *)
    If[Mod[cntac,100]==0, Print["Accepted Cnt : ", cntac," out of ",cntatp," attempted fractures."] ];
	(*If[Mod[cntac,10]==0, Print["Current Fracture Density : ", currentDensity, "\n"] ];*) (* print every 100 accepted fractures *)
	
     If [Mod[newPoly[[5,1]]-1,4]==0 && edpl==1, ereject[[n]]=0];

     If [Mod[newPoly[[5,1]]-2,4]==0 && rdpl==1, rreject[[n]]=0];
   
     
    reject = 0; (* reset successive rejection counter *)
	cntac++;
	cntatp++;

	If[ cntatp >= maxPoly, (* stopping criteria *)
	stop = 2; 
	Print["Stopped: maximum attempt reached."];
	Break[];
	]
 
	

	(* stopping criteria *)
	If[ cntac == acPoly, Print["Successfully reached requested number of fractures."];]
	(*	cntac++; cntatp++;
		stop = 4;
		If[largestSize==0, errMessage=StringJoin["Error: Aborting Program. There is no ",
			"fracture network. Reached acPoly number of tries and no fractures intersect. ",
			"This program does not work for one fracture, or many disconnected fractures. ",
			"Perhaps you could consider one of the following options: ",
			"making the fractures bigger, the domain smaller, increasing the number ",
			"of fractures, increasing the variable acPoly in the file DFNMain ",
			"(which dictates the maximum number of fractures that can be accepted)."]; 
			printErr;
			If[ValueQ[test]==True, Goto[end]];
			Abort[]];
		errMessage=StringJoin["Warning: Stopped inserting fractures.\n",
		"Fracture network too sparse.\n",
		"Perhaps you could consider one of the following options:\n",
		"making the fractures bigger, the domain smaller, \n",
		"increasing the number of fractures, increasing the\n",
		"variable acPoly in the file DFNMain (which dictates\n",
		"the maximum number of \nfractures that can be accepted).\n"]; 
		printErr;
(* can't produce a connected component of size nPoly with acPoly accepted fractures *)
		
		Break[]
	];

*)
If [Mod[newPoly[[5,1]]-1,4]==0 && edpl==1, ecntlengthchange[[n]]++ ];

If [Mod[newPoly[[5,1]]-2,4]==0 && rdpl==1, rcntlengthchange[[n]]++ ];
cntlengthchange++;

]; (*end of while loop. *)
(* end for cntatp *)

(*==================================================================================*)
tmp33 = Timing[

(* Count number of fractures not in the largest set *)
(* Get indices of fractures in the largest set *)
remove = 0;

For[i=1, i<=cntac, i++,
nNodes=poly[[i,1,2]];
If[MemberQ[poly[[i,3,Range[nNodes], 2]],domain[[2]]]==True,  poly[[i,2,4]]=Append[poly[[i,2,4]],4] ];
If[MemberQ[poly[[i,3,Range[nNodes], 2]],-domain[[2]]]==True,  poly[[i,2,4]]=Append[poly[[i,2,4]],6]];
If[MemberQ[poly[[i,3,Range[nNodes], 1]],domain[[1]]]==True,  poly[[i,2,4]]=Append[poly[[i,2,4]],5]];
If[MemberQ[poly[[i,3,Range[nNodes], 1]],-domain[[1]]]==True,  poly[[i,2,4]]=Append[poly[[i,2,4]],3]];
If[MemberQ[poly[[i,3,Range[nNodes], 3]],domain[[3]]]==True,  poly[[i,2,4]]=Append[poly[[i,2,4]],1]];
If[MemberQ[poly[[i,3,Range[nNodes], 3]],-domain[[3]]]==True,  poly[[i,2,4]]=Append[poly[[i,2,4]],2]];
];

groupNum=1;
numberlist=Range[cntac];
j=1;
i=1;
group=ConstantArray[0,{cntac,1}];
While[numberlist!={},
	checkNumList={numberlist[[i]]};
	checkedNumList={};
	While[j==1,
		group[[groupNum]]=Append[group[[groupNum]],Position[Normal[adj[[checkNumList[[j]]]]],1]];
		group[[groupNum]]=Append[group[[groupNum]],checkNumList[[j]]];
		checkedNumList=Append[checkedNumList,checkNumList[[j]]];
		checkNumList=DeleteCases[Complement[Flatten[group[[groupNum]]],checkedNumList], 0];
		If[checkNumList=={},
			numberlist=Complement[numberlist,Flatten[group[[groupNum]]]];
			groupNum++;
			Break[];
		]
	]
];
Do[group[[i]]=DeleteCases[DeleteDuplicates[Flatten[group[[i]]]],0],
 {i, cntac}];

Do[poly[[i,2,5]]=Position[group,i][[1,1]], {i, cntac}];

group=DeleteCases[group, {_}];
group=DeleteCases[group, {}];

(*code to only keep the single largest cluster, deleting the rest*)
If [multipleClusters == 0, 
If[ValueQ[boundaryFaces]==False || boundaryFaces == {}, (*default case*)
	boundaryFaces={1,2,3,4,5,6};(*this case will accept clusters between and  all TWO faces*)
	idxLargest=Flatten[Reap[
	For[i=1, i<=Length[group], i++,
	facesInGroup=Flatten[poly[[group[[i]], 2,4]]];
	If[Length[Intersection[facesInGroup, boundaryFaces]]>=2, Sow[group[[i]]]];]][[2]],1];
	
	, (*else*) (*this case accepts clusters which connect every specified faces *)
	idxLargest=Flatten[Reap[
	For[i=1, i<=Length[group], i++,
	facesInGroup=Flatten[poly[[group[[i]], 2,4]]];
	If[Length[Intersection[facesInGroup, boundaryFaces]]==Length[boundaryFaces], Sow[group[[i]]];]]][[2]],1

];

];

(*compairs lengths of each cluster, choosing the largest cluster. If two clusters are the same size, choose the first one*)
temp1 = Length[idxLargest[[1]]];
idx = 1; 
For[i=2, i<= Length[idxLargest], i++,
    If[temp1 <  Length[idxLargest[[i]]],
    		idx = i;
            temp1 = Length[idxLargest[[i]]];
]

];
idxLargest = idxLargest[[idx]];
If[idxLargest=={}[[1]],
errMessage=StringJoin["ERROR: Aborting Program. There is no ",
			"fracture network that goes between the boundary faces. ",
			"Perhaps you could consider one of the following options: ",
			"making the fractures bigger, the domain smaller, or increasing the number ",
			"of fractures", cntac]; printErr; 
			If[ValueQ[test]==True, Goto[end]];
			Abort[]];

];



If[multipleClusters == 1,
If[ValueQ[boundaryFaces]==False || boundaryFaces == {}, (*default case*)
	boundaryFaces={1,2,3,4,5,6};(*this case will accept clusters between and and all TWO faces*)
	idxLargest=Flatten[Reap[
	For[i=1, i<=Length[group], i++,
	facesInGroup=Flatten[poly[[group[[i]], 2,4]]];
	If[Length[Intersection[facesInGroup, boundaryFaces]]>=2, Sow[group[[i]]]];]][[2]]];
	, (*else*) (*this case accepts clusters which connect every specified faces *)
	idxLargest=Flatten[Reap[
	For[i=1, i<=Length[group], i++,
	facesInGroup=Flatten[poly[[group[[i]], 2,4]]];
	If[Length[Intersection[facesInGroup, boundaryFaces]]==Length[boundaryFaces], Sow[group[[i]]];]]][[2]]];
	
	]; 
];


(*=============================================================================*)
Print["\nBefore isolated fractures removed:"];
Print["Total accepted fractures = ", cntac];
Print["Total accepted fracture surface area = ", 2*totalAcptArea]; (*multiply by 2 for surface area*)
Print["Total accepted fracture volume = ", totalAcptVolume];
(*Print["Total accepted fracture density = ", currentDensity, "\n"];*)

(*=============================================================================*)



(*idxLargest - indices of fractures in the largest set *)
(*
set=Ordering[dset[[1]], 3];

idxLargest = Reap[ 
For[ jj = 1, jj < cntac , jj++, (* loop through all the poly's *)
	root = jj;

	
	While[ dset[[1,root]] > 0, 
		(* find root *) (* note: tree height is not compressed here since not necessary *)		
		root = dset[[1,root]] ]; (*end while*)

	If[ root == set[[1]] || root== set [[2]] || root ==set[[3]], 
		Sow[jj]; Print["jj=", jj],
		remove++; 
		Switch[ poly[[jj,1,1]], 1, accepted--, 2, truncated--, 3, deformed-- ];
		(* set the polygon not in the largest set to be removed *)
		allpoly[[jj,1,1]] = -4 
 
	]
]
]; 
idxLargest = idxLargest[[2,1]];
*)

(* delete information of isolated fractures *)
If[idxLargest=={},
If[group=={}, errMessage=StringJoin["ERROR: Aborting Program. There is no ",
			"fracture network. There are no fractures intersecting with one ",
			"another. This program does not work for one fracture, or many ",
			"disconnected fractures. Perhaps you could consider one of the ",
			"following options: ",
			"making the fractures bigger, the domain smaller, or increasing the number ",
			"of fractures"]; printErr; 
			If[ValueQ[test]==True, Goto[end]];
			Abort[],
errMessage=StringJoin["ERROR: Aborting Program. There is no ",
			"fracture network that goes between the boundary faces. ",
			"Perhaps you could consider one of the following options: ",
			"making the fractures bigger, the domain smaller, or increasing the number ",
			"of fractures", cntac]; printErr; 
			If[ValueQ[test]==True, Goto[end]];
			Abort[]]];

idxLargest=Sort[idxLargest];
poly = poly[[idxLargest]];
nPoly = Length[idxLargest];

intPts = intPts[[All,All,idxLargest,idxLargest]];

adj = adj[[idxLargest,idxLargest]];

familyac= familyac[[idxLargest]]

(*****)
]; (* end Timing *)
timeLargestConnComp += tmp33[[1]];

(* compute statistics of family distribution *)
family=DeleteCases[family, \[Infinity]];
maxfam = Max[family]+1;
countac = HistogramList[ familyac,{1,maxfam,1},"Count"];
countall = HistogramList[ family[[Range[cntatp-1]]],{1,maxfam,1},"Count"];

(*****)
tmp40 = Timing[

(* construct adjacency matrix, A, used by generatingPoints`CreateIntersectionPoints *)
A = ConstantArray[{0,0}, nPoly];
For[ ii = 1, ii <= nPoly, ii++,
		A[[ii,1]] = poly[[ii,1,3]]; (* number of intersections on each fracture *)
	A[[ii,2]] = ConstantArray[{0,0,0},poly[[ii,1,3]] ];
	
	
	(* indices of fractures intersecting with ii *)
	intIdxii = Flatten[Drop[Map[First,ArrayRules[adj[[ii]]]],-1]]; 
	(* intIdxii = Flatten[Drop[Map[First,ArrayRules[intPts[[1,1,ii]]]],-1]]; (* indices of fractures intersecting with ii *)*)
	intIdxii = Sort[intIdxii]; (* ArrayRules doesn't display in order *)
	intPtListii = If[Length[intIdxii] > 0, Normal[ intPts[[All,All,ii,intIdxii]] ], {}];  (* list of intersection end points on fracture ii *)
	
	
	For[ jj = 1, jj <= poly[[ii,1,3]], jj++,
	
		A[[ii,2,jj,1]] = intIdxii[[jj]]; (* index of the other fracture *)
		A[[ii,2,jj,2]] = Flatten[intPtListii[[1,All,jj]]]; (* intersection end points *) 
		A[[ii,2,jj,3]] = Flatten[intPtListii[[2,All,jj]]]
		
		
	]
]

(*****)
]; (* end Timing *)
timeConstructAdjanceyMatrix += tmp40[[1]];

(*==================================================================================*)
(*Builds information about faces clusters intersects, stores in 'clusterInfo' *)
facesInGroup=Flatten[poly[[All, 2,4]]]; (*All polygons left are in group 1*)

clusterInfo=IdentityMatrix[6]; 
	For[i = 1, i<=5, i++,
	   For[j= i+1,j<=6, j++, (*writes adj matrix of face intersection true/false information*)
	   If[Length[Intersection[facesInGroup, {i,j}]]==2, clusterInfo[[i,j]] = 1; clusterInfo[[j,i]]=2;]
	]];



(*==================================================================================*)

(*This section calculates the area of accepted polygons using triangles and cross product area method*)
(*also gets volume for each fracture*)
(*The area of each polygon is calculated by first getting a coordinate inside the polygon. Then this coordinate is used to
make vectors from the coordinate to all the vertices, creating triangles. The triangles are summed to get the polygons area*)



finalFractArea = 0; (*total area of fractures in network *)
finalFractVol = 0;  (*total volume of fractures in netwrok *)

For[i=1, i<=Length[poly], i++,
finalFractArea += poly[[i,6,1]];
finalFractVol += poly[[i,6,2]];
]

Print["\nAfter isolated fractures removed:"];
Print["Final fractures number: ", Length[poly]];
Print["Final fractures total surface area (Total fracture Area * 2) = ", 2*finalFractArea]; (*multiply by 2 for surface area*)
Print["Final fractures total volume (Fractures Area *Fractures Aperture) = ", finalFractVol];
finalFractDensity = finalFractVol/domainVol;
Print["Final fracture density (No of fractures per unit volume), P30 = ",N[Length[poly]/domainVol]];
Print["Final fracture intensity (Area of fractures per unit volume), P32 = ", finalFractArea/domainVol];
Print["Final fracture porosity (Volume of fractures per unit volume), P33 = ", finalFractDensity];
(*==================================================================================*)
Print["\nAttempted ", cntatp-1, " fractures (accepted + rejected)" ];
(*==================================================================================*)
(*get allpoly info from file for stats report*)
If[Global`productionMode == 0,
  allpoly =  ReadList[workingdirectory<>"stat/allpoly"];
  Close[workingdirectory<>"stat/allpoly"];  ];

familyRejected = ReadList[workingdirectory<>"stat/familyRejected"]; 
Close[workingdirectory<>"stat/familyRejected"];


Print["Number of accepted polygons: ", Length[poly]];


tmp = Timing[


(*Generating Points on the Intersection*)
linePoints = generatingPoints`CreateIntersectionPoints[A, poly, h, eps];

lineCnt = generatingPoints`calculateNumberOfLineElements[linePoints];

(*Rotate Polygon Points into the XY Plane*)
polyPoints = generatingPoints`CreatePolygonPoints[poly, eps]

(*****)
];

timeWriteToFile2 += tmp[[1]];

m0 = MaxMemoryUsed[];
(*Print["Time before output: ", ttt];*)
(*==================================================================================*)

rp = OpenWrite[workingdirectory<>"ReportOptions.txt",FormatType->OutputForm];
If[createReport==1, 
Write[rp,"report "], Write[rp, "0"]];
Close[workingdirectory<>"ReportOptions.txt"];
(*==================================================================================*)
Print["Writing out the number of CPUs to use in the meshing."];
rp = OpenWrite[workingdirectory<>"nCPU.txt",FormatType->OutputForm];
Write[rp, nCPU];
Close[workingdirectory<>"nCPU.txt"];
(*==================================================================================*)
Print["Writing rejection reasons file: stat/rejReason.txt."];
rp = OpenWrite[workingdirectory<>"stat/rejReason.txt",FormatType->OutputForm];
Write[rp,"Accepted and not deformed:         ", accepted];
Write[rp,"Accepted and truncated:            ", truncated];
Write[rp,"Accepted and deformed:             ", deformed];
Write[rp,"Outside rejections:                ", outside];
Write[rp,"Short intersections rejections:    ", shortIntersection];
Write[rp,"Close to edge rejections:          ", closeToEdge];
Write[rp,"Close to vertex rejections:        ", closeToNode];
Write[rp,"Int. point close to edge rejections: ", closePointToEdge];
Write[rp,"Triple intersection rejections:    ", triple];
Write[rp,"Not in the largest set rejections: ", remove];
(*Write[rp,"Small angle at triple intersection: ", angleTripleSmall];*)
(*Write[rp,"Fracture is close to (<h) but doesn't intersect intersection : ", close2Intersection];
Write[rp,"Triple intersection point close to an end point: ", tripleCloseEndPoint];
Write[rp,"Triple intersection point close to other triple intersection points: ", tripleClose2TriplePoint];
Write[rp,"Intersections overlapping: ", overlapIntersections];
Write[rp,"Something went wrong finding point of intersection: ", somethingTripleWrong];*)
Close[workingdirectory<>"stat/rejReason.txt"];

(*==================================================================================*)

Clear[intIdxii]; (* save memory *)

tmp = Timing[
nPoly=Length[poly];
(*Write Points on the Intersection Out*)
Print["Writing out intersection points into file: ", lineOutputFile,"."]

dumpPoints`dumpIntPoints[linePoints,lineOutputFile, lineCnt]; 

(*Print["Writing out intersection points into file: ", lineOutputFile,"."];*)

(*dumpPoints`dumpIntersectionPoints[linePoints, lineOutputFile, lineCnt]; *)
Unprotect[In, Out]; (* save memory *)
Clear[In, Out];
Protect[In, Out];
ClearSystemCache[];
Print["Writing out polygon points into file: ", polyOutputFile,"."]; 
(*PrintOut Polygons*)
dumpPoints`dumpPolygonPoints[polyPoints,polyOutputFile, nPoly]; 

Unprotect[In, Out]; (* save memory *)
Clear[In, Out];
Protect[In, Out];
ClearSystemCache[];

  If [nShape[[1]]==0 && nShape[[3]] ==0 , numPoints=5, 
     If [nShape[[1]]!= 0, numPoints=Max[enumPoints]];
     If [nShape[[3]] !=0, numPoints=Max[uenumPoints]] ];   


(*Print Out Parameters*)
Print["Writing out parameters into file: ", paramsOutputFile,"."]; 
dumpPoints`dumpParams[poly,h, numPoints, paramsOutputFile ,polyOutputFile , lineOutputFile, slope, refineDist, visualizationMode, familyac, domain ];

(*print out lengths *)
Print["Writing out lengths into file: ", lengthsOutputFile,"."]; 
dumpPoints`dumpLengths[poly, lengthsOutputFile];

(*print out xyz + h values *)
Print["Writing out (bounding box + h) into file: ", boundingOutputFile,"."]; 
dumpPoints`dumpBoundingBox[poly[[Range[nPoly], 4]], nPoly, boundingOutputFile, h];

(* write Permeability *) 
Print["Writing out permeability into file: ", permOutputFile,"."]; 
dumpPoints`dumpPermeability[poly, permOutputFile];

(* write Aperture *) 
Print["Writing out aperture into file: ", apertureOutputFile,"."]; 
dumpPoints`dumpAperture[poly, apertureOutputFile]


(*If[ visualizationMode == 0, dumpPoints`dumpLengthInfo[allpoly, cntatp]]*)
(*****)
];
timeWriteToFile += tmp[[1]];
Print["Finished Network"];
(*==================================================================================*)
Print["Writing time report file: stat/time_profile.txt."]; 
rp = OpenWrite[workingdirectory<>"stat/time_profile.txt",FormatType->OutputForm];
Write[rp,timeCreatePoly, "   Create fractures"];

Write[rp,timeFindIntersection, "   Find intersections"];
Write[rp,timeBadIntersection, "   Check single intersection"];
Write[rp,timeCheckForTripleIntersection, "   Check triple intersection"];
Write[rp,timeConstrDSet, "   Construct disjoint sets"];
Write[rp,timeSparse, "   Read/write sparse matrix"];

Write[rp,timeLargestConnComp, "   Find fractures in the largest set"];
Write[rp,timeConstructAdjanceyMatrix ,"   Construct A (adjacency matrix used in generatingPoints`CreateIntersectionPoints)"];
Write[rp,timeWriteToFile2, "   Discretize intersections"];
Write[rp,timeWriteToFile, "   Write output files"];

Write[rp,"Max memory used (bytes) before output: ",NumberForm[m0,DigitBlock->3, NumberSeparator->","]];
Close[workingdirectory<>"stat/time_profile.txt"];
(*==================================================================================*)
(*semiminor = N[{Exp[emean[[1;;nShape[[1]]]]],Exp[rmean[[1;;nShape[[2]]]]],ueb[[1;;nShape[[3]]]],urb[[1;;nShape[[4]]]]}];*)
Print["Writing general report file: stat/report.txt."]; 
rp = OpenWrite[workingdirectory<>"stat/report.txt",FormatType->OutputForm];

Switch[stop,
	1, errMessage=StringJoin["Desired number of fractures (", ToString[nPoly],") accepted."],
	2, errMessage="Stopped: maximum attempt reached.",
	3, errMessage="Stoppted: too many rejections in a row.",
	4, errMessage="Stopped: fracture network too sparse.Perhaps you could consider one of the following options:"
 ," making the fractures bigger, the domain smaller, increasing the number of fractures, increasing the",
"variable acPoly in the file DFNMain (which dictates the maximum number of fractures that can be accepted)."]

WriteString[rp, errMessage,"\n",

"Final fractures number: ", Length[poly],"\n",
"Final fractures total surface area (Total fracture Area * 2) = ", 2*finalFractArea ,"\n",
"Final fractures total volume (Fractures Area *Fractures Aperture) = ", finalFractVol,"\n",

"Final fracture density (No of fractures per unit volume), P30 = ",N[Length[poly]/domainVol],"\n",
"Final fracture intensity (Area of fractures per unit volume), P32 = ", finalFractArea/domainVol,"\n",
"Final fracture porosity (Volume of fractures per unit volume), P33 = ", finalFractDensity,"\n",


"Number of families for each shape: \n",
	"*", nShape[[1]]," families of ellipses;\n",
	"*", nShape[[2]]," families of rectangles;\n",
	"*", nShape[[3]]," user defined ellipses;\n",
	"*", nShape[[4]]," user defined rectangles;\n",
     
"Domain (m) = {", -domain[[1]],"\[LessEqual]x\[LessEqual]", domain[[1]],",  ",
						        -domain[[2]],"\[LessEqual]y\[LessEqual]", domain[[2]],",  ",
    							-domain[[3]],"\[LessEqual]z\[LessEqual]", domain[[3]],"}\n", 

"Domain Volume (m^3)= ", Apply[Times, (domain*2)],"\n",

(*"Semi-minor axes = ", semiminor,"\n",*)

"h (m)= ", h,"\n", 

"Attempted fractures (number of polygons created during the program \n",
"while attempting to reach the desired number of fractures): ", cntatp-1, "\n", 

"Final number of fractures: ", nPoly,".\n",

"Accepted number of fractures (number of fractures accepted, but
that may have been eliminated because they were isolated or weren't 
part of a network that connected two of the boundary faces: ", cntac,".\n",

"Number of fractures rejected because they were isolate or not part of
a network connecting two boundary faces: ",nPoly-cntac, ".\n",

"Number of fracture networks (in the final mesh): ", Length[group], ".\n",

"Sizes of fracture networks/groups: ", (a={}; 
Do[a=Append[a, Length[group[[i]]]], {i,Length[group]}];a),".\n",
 
"Accepted family distribution:  ", countac[[2]], "\n",

"Attempted family distribution: ", countall[[2]], "\n",

"Adjacency matrix elements and size: ", adj];

Close[workingdirectory<>"stat/report.txt"];

(*==================================================================================*)
(*run the code "statistics.m" to compute statistics *)
Print["Computing and writing statistics related files "];
(* report with no figures*)
If [createVisReport==-1,
Get[packagepath <> "statistics_nofigures.m"]; 
Get[packagepath <> "writeLatexCommands_nofigures.m"]];

(* report with figures *)
If [createVisReport>-1,
Get[packagepath <> "statistics.m"]; 
If [createVisReport==1,
(*run the code "visualization.m" to plot DFN *)
Get[packagepath <> "visualization.m"];
(*run the package "writeLatexCommands.m" to create a command input file for
latex *)
Get[packagepath <> "writeLatexCommands.m"]];
 If [createVisReport==0,
Get[packagepath <> "writeLatexCommands_novis.m"]]
];
(*==================================================================================*)
Quit[0];
Label[end];)
