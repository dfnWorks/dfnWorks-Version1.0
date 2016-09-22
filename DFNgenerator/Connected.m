(* ::Package:: *)

BeginPackage[ "Connected`"]

Explore::usage =
" Checks all the adjanct polygons if they are tagged "

ConnectedComps::usage =
"Given the Adjaceny Martix for a DFN, returns the number of connected components and the fractures in each set, Uses a Depth First Search "

TagPolygonsWithConnectedSet::usage =
" Goes through the polygon set and retags each polygon as being a in the connected set."

LargestComponent::usage =
" Returns the Index of the Connected Components that has the largest number of elements"

ConnectionInformation::usage =
" Finds the min/max of the number of intersections, min/max Length of intersections, Plots the Adjancey Matrix, and a histogram of the number of connections"

Begin[ "private`"]

Explore[ array_, index_, group_, matrix_] := Module[ {list = array, i = index, Color = group, A = matrix, numNeighbors, Neighbor, j},
(* Set Recursion Limit so that if the entire netowrk is connected, Explore wont crash*)
$RecursionLimit = Max[Length[A],1000];

list[[i]] = Color;
numNeighbors = A[[i,1]];	
For[ j = 1, j <= numNeighbors, j++,
Neighbor = A[[i,2,j,1]];
If[list[[Neighbor]] == 0,
list = Explore[list, Neighbor, Color, A];
];
];

list
];

ConnectedComps[matrix_] := Module[ { A = matrix, list, i, Color},
list = ConstantArray[0,Length[A]];
Color = 0;
For[ i = 1, i <= Length[A], i++,
If[ list[[i]] == 0,
Color++;
list = Explore[list, i, Color,A];
];
];

Print["There Exist ", Color, " Connected Components"];
list
];

LargestComponent[ array_ ] := Module[{ list = array, i, numBins, bins, maxBin},

numBins = Max[list];
bins = ConstantArray[0, numBins];
For[ i = 1, i <= Length[list], i++,
bins[[list[[i]]]]++;
];

For[ i = 1, i <= numBins, i++,
Print["Component ", i , " has ", bins[[i]] , " fractures"];
];

maxBin = Ordering[bins, -1];
maxBin = maxBin[[1]];
Print["Largest Component is set ", maxBin];
maxBin
];

TagPolygonsWithConnectedSet[ arraypoly_, arraylist_] := Module[{ poly = arraypoly, list = arraylist, ii},

For[ ii = 1, ii <= Length[list], ii++,
poly[[ii,1,1]] = list[[ii]] (***** definition of poly[[ii,1,1]] has changed; doesn't matter here since this function is not used *)
];

poly
];



ConnectionInformation[matrix_] := Module [{ A = matrix, i,j, AA, maxIntersections, minIntersections, index, hist, maxLength, minLength, length},

(* maxIntersections = 0;
minIntersections = 10000;

maxLength = 0;
minLength = 10000;

AA = ConstantArray[0,{Length[A], Length[A]}];
*)
hist = ConstantArray[0,Max[A[[;;,1]]]];

For[i = 1, i <= Length[A], i++,
(*	If[ A[[i,1]] > maxIntersections, maxIntersections = A[[i,1]];];
If[ A[[i,1]] < minIntersections, minIntersections = A[[i,1]];];

For[ j = 1, j <= A[[i,1]], j++,

index = A[[i,2,j,1]];

AA[[i,index]] = 1;

length = EuclideanDistance[ A[[i,2,j,2]], A[[i,2,j,3]]];

If[ length > maxLength, maxLength = length ];
If[ length < minLength, minLength = length ];

];
*)
hist[[ A[[i,1]] ]]++
];

(*	Print["Network Stats :"];
Print["Minimun Number of Intersections :", minIntersections];
Print["Maximun Number of Intersections :", maxIntersections];
Print["Minimun Length of Intersections :", minLength];
Print["Maximun Length of Intersections :", maxLength]; *)
hist
];



End[]

EndPackage[]
