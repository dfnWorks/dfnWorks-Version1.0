(* ::Package:: *)

Begin["Global`"]

(* reason of rejections *)
fp = OpenWrite[workingdirectory<>"stat/rej", FormatType->OutputForm];
Write[fp, accepted ];
Write[fp, truncated ];
Write[fp, deformed ];
Write[fp, outside ];
Write[fp, shortIntersection ];
Write[fp, closeToEdge ];
Write[fp, closeToNode ];
Write[fp, triple ];
Write[fp, remove ];
Close[workingdirectory<>"stat/rej"];


(*rejReasons = {accepted,truncated,deformed,outside,shortIntersection,closeToEdge,closeToNode,triple};
labels = {"accepted", "truncated", "deformed", "outside", "shortIntersection", "closeToEdge", "closeToNode", "tripleIntersection"};

Export[workingdirectory<>"stat/graphRejReasons.png", BarChart[rejReasons,ChartLabels->labels,BarOrigin->Left,ImageSize -> 600,BaseStyle -> FontSize -> 18]]; *)



(* adjacency matrix plot *)
(*Export[workingdirectory<>"stat/adj.png", MatrixPlot[adj,ColorFunction->"Monochrome",MaxPlotPoints -> Infinity] ]; *)





(* rejections caused *)
fp = OpenWrite[workingdirectory<>"stat/rejCaused", FormatType->OutputForm];
For[ii = 1, ii <= cntac, ii++,
Write[fp, rejCaused[[ii]] ]
];
Close[workingdirectory<>"stat/rejCaused"];

(*rejCaused=Table[rejCaused[[ii]], {ii,1,cntac}]; 
Export[workingdirectory<>"stat/graphRejCaused.png", ListLinePlot[rejCaused,PlotRange->{{0,cntac},{0,Max[rejCaused]}}, Frame->{True,True,False,False}, FrameLabel->{"No. rejections on fracture","Accepted fracture no."},ImageSize -> 700,BaseStyle -> FontSize -> 18]];*)


(* connected component sizes *)
conn = Sort[Select[(-dset[[1,Range[cntac]]]), # > 0&],Greater] ;
fp = OpenWrite[workingdirectory<>"stat/conn", FormatType->OutputForm];
For[ii = 1, ii <= Length[conn], ii++,
Write[fp, conn[[ii]] ]
];
Close[workingdirectory<>"stat/conn"];

(*connAry = Table[conn[[ii]],{ii, 1, Length[conn]}];
Export[workingdirectory<>"stat/graphConnectedSize.png", BarChart[connAry,Frame->{True,True,False,False}, FrameLabel->{"Count","Size of connected components"},ImageSize -> 700,BaseStyle -> FontSize -> 18]]*)

(* family count *)
fp = OpenWrite[workingdirectory<>"stat/fam", FormatType->OutputForm];
For[ii = 1, ii <= Length[countac[[2]]], ii++,
Write[fp, countac[[2,ii]]," ", countall[[2,ii]] ]
];
Close[workingdirectory<>"stat/fam"];




(* normal vector distribution *)
acbyfam = ConstantArray[0,Length[countac[[2]]]];
(*countac[[2]] is max[family code number]. 
so if the highest family code is 5, Length[countac[[2]]] is 5, and
acbyfam(accepted by family) = {0,0,0,0,0}. *)

jj = 1;
For[ii = 1, ii <= Length[countac[[2]]], ii++, 
	If[countac[[2,ii]] > 0, (* if there are fractures in family ii *)
	acbyfam [[jj]] = {ii,countac[[2,ii]]}; 
(* acbyfam[[i'th number of family]]={family code, number of fractures in family *)
	jj++ ]
];

(*create a vector of vectors of the form 
{{famCode, # fractures w/ famCode},{famCode, # fractures w/ famCode},... *)
acbyfam = acbyfam[[1;;jj-1]];

(*create an array acnorm - accepted nomral *)
acnorm = ConstantArray[0,jj-1];

(*jj-1=the number of families present in the problem *)
For[ii = 1, ii <= jj-1, ii++, 
acnorm[[ii]] = ConstantArray[0,acbyfam[[ii,2]]] ];
(* create an array of zeros with length of number of fractures in the first family *)

iter = ConstantArray[1,jj-1];

For[ii = 1, ii<= Length[idxLargest], ii++,
(* run on all of the indices from 1 till the number of fractures in the biggest 
network *)
	For[kk = 1, kk <= jj-1, kk++, 
(* run on the number of different families present in the problem *)		
		If[familyac[[ii]] == acbyfam[[kk,1]], 
(* if the family code of familyac[[ii]] matches the family code of acbyfam[[kk,1]] *)
			acnorm[[kk,iter[[kk]]]] = poly[[ii,2,1]]; 
(* define the accepted normal as belonging to the first family, and mark that it
is the iter[[kk]]'th fracture added to that family]] *) 
			iter[[kk]]++ ]
	]
]; 
If[nShape[[1]]>0,

(* normal vectors of family 1 *)
(* assume elliptical fractures *)
If[countac[[2,1]]>0,    
A1 = RotationMatrix[{{Sin[etheta[[1]]]*Cos[ephi[[1]]],Sin[etheta[[1]]]*Sin[ephi[[1]]],Cos[etheta[[1]]]},{0,0,1}}];
(* choose only the normals of the fractures belonging to the first family *)
newacnorm1 = acnorm[[1]];

(* run the program for the number of fractures in the first family *) 
For[ii = 1, ii <= countac[[2,1]], ii++, 
	newacnorm1[[ii]]= A1.acnorm[[1,ii]] ];
	fp = OpenWrite[workingdirectory<>"stat/orientation", FormatType->OutputForm];
	(* fun on the number of fractures in the first family *)
	For[ii = 1, ii <= Length[newacnorm1], ii++,
		currnormal = Threshold[newacnorm1[[ii]]];
		Write[fp, currnormal[[1]], " ", currnormal[[2]], " ", currnormal[[3]] ]
]
Close[workingdirectory<>"stat/orientation"]];
];

(*Incomplete, not sure how to do in mathematica*)
(*
(*oriendation density plot*)
currnormal = Table[newacnorm1[[ii]], {ii,1,Length[newacnorm1]}];
Print["currnormal = ", currnormal];
Export[workingdirectory<>"stat/graphOrientationDistribution.png", ListDensityPlot[currnormal]
*)

(* position distribution - old, used in for matlab graphing*)
(*fp = OpenWrite[workingdirectory<>"stat/pos", FormatType->OutputForm];
For[ii = 1, ii <= Length[poly], ii++,
currtrans = Threshold[poly[[ii,2,2]]]; 
Write[fp, currtrans[[1]], " ", currtrans[[2]] ]
];Close[workingdirectory<>"stat/pos"];*)


(*currtrans = Table[{poly[[ii,2,2,1]],poly[[ii,2,2,2]]}, {ii, 1, Length[poly]}];

(*position acceped plot/graph*)
Export[workingdirectory<>"stat/graphPositionDistribution.png",ListPlot[currtrans,AspectRatio -> Automatic,PlotStyle->PointSize[.01],ImageSize -> 600,BaseStyle -> FontSize -> 16]]; 

(*Length accepted histogram graph*)
lengthac= Import[workingdirectory<>"stat/length_acc", "List"];
Export[workingdirectory<>"stat/graphAcceptedDistribution.png", Histogram[lengthac,{1}, Frame->{True,True,False,False}, FrameLabel->{"Radii of polygons","Accepted polygons"},ImageSize -> 500,BaseStyle -> FontSize -> 18]];

(*Length Attempted histogram graph*)
lengthatt=Import[workingdirectory<>"stat/length", "List"];
Export[workingdirectory<>"stat/graphAttemptedDistribution.png", Histogram[lengthatt,{1}, Frame->{True,True,False,False}, FrameLabel->{"Radii of polygons","Attempted polygons"},ImageSize -> 500,BaseStyle -> FontSize -> 18]];
*)


If[Global`productionMode == 0, (*only runs if using allpoly array (productionMode=0)*)
fp = OpenWrite[workingdirectory<>"stat/pos_all", FormatType->OutputForm];
For[ii = 1, ii < cntatp, ii++,
currtrans = Threshold[allpoly[[ii,2,2]]];
Write[fp, currtrans[[1]], " ", currtrans[[2]] ]
];
Close[workingdirectory<>"stat/pos_all"];
]

(* intersection number *)
Get[ packagepath <> "Connected.m"]; 

hist = Connected`ConnectionInformation[A]; 
(* intPlot = ListPlot[{hist},  Filling -> Axis, AxesLabel -> {" Number of Intersections ", " Counter"} ]; *)

(*old code, used for matlab graphs*)
(*fp = OpenWrite[workingdirectory<>"stat/intersection", FormatType->OutputForm];
For[ii = 1, ii <= Length[hist], ii++,
Write[fp, hist[[ii]] ]
];
Close[workingdirectory<>"stat/intersection"];*)

(*intersection histogram graph*)
(*labels = Table[ii, {ii,1,Length[hist]}]; 
Export[workingdirectory<>"stat/graphIntersectionNumber.png", BarChart[hist,Frame->{True,True,False,False},ChartLabels -> Labels, FrameLabel->{"Number of Intersections","Number of Fractures"},ImageSize -> 500,BaseStyle -> FontSize -> 18]];*)


(*Creates file with the number of fractures in each family,  *)
(*data file KEY: number accepted, num attempted, family number(1-4) for {ell,rec, uell, urec} , index num of particular family *)

(*familyAccepted=poly[[Range[cntac], 5,1]]; *)

(*
a=ConstantArray[0, Length[famCode]];    
rp = OpenWrite[workingdirectory<>"stat/familia",FormatType->OutputForm];

If[Global`visualizationMode ==0,
For[i=1, i<=Length[famCode], i++, 
Write[rp, Count[familyac, famCode[[i]]],",", Count[familyRejected, famCode[[i]]], ",",
If[Mod[famCode[[i]],4] == 1, "ell ",
If[Mod[famCode[[i]],4]==2, "rec ",
If[Mod[famCode[[i]],4]==3, "u-ell ",
If[Mod[famCode[[i]],4]==0, "u-rec "]]]] , Ceiling[famCode[[i]]/4]
]] , (*else*)

For[i=1, i<=Length[famCode], i++,
Write[rp, Count[familyac, famCode[[i]]], ",","0,", 
If[Mod[famCode[[i]],4] == 1, "ell ",
If[Mod[famCode[[i]],4]==2, "rec ",
If[Mod[famCode[[i]],4]==3, "u-ell ",
If[Mod[famCode[[i]],4]==0, "u-rec "]]]]
, Ceiling[famCode[[i]]/4]];
]]
Close[workingdirectory<>"stat/familia"];
*)

numAcceptedArray = Table[Count[familyac, famCode[[i]]], {i,1, Length[famCode]}];
numRejectedArray = Table[Count[familyRejected, famCode[[i]]], {i,1, Length[famCode]}];
labels = Table[
If[Mod[famCode[[i]],4] == 1, "ell-"<>ToString[Ceiling[famCode[[i]]/4]],
If[Mod[famCode[[i]],4]==2, "rec-"<>ToString[Ceiling[famCode[[i]]/4]],
If[Mod[famCode[[i]],4]==3, "u-ell-"<>ToString[Ceiling[famCode[[i]]/4]],
If[Mod[famCode[[i]],4]==0, "u-rec-"<>ToString[Ceiling[famCode[[i]]/4]]]]]], {i,1,Length[famCode]}];



(*Export[workingdirectory<>"stat/graphFamilyDist.png", BarChart[{numAcceptedArray, numRejectedArray}//Transpose, ChartLabels->{labels,None}, ChartLayout->"Stacked", ChartLegends->{"Accepted", "Rejected"}, ImageSize->500,BaseStyle -> FontSize -> 18,Frame->{False,True,False,False},FrameLabel->{None,"Count"}]];
*)


(*write face/cluster connectivity matrix*)
rp = OpenWrite[workingdirectory<>"stat/connectingFaces",FormatType->OutputForm];
Write[rp, clusterInfo];
Close[workingdirectory<>"stat/connectingFaces"];

(*Write total surface area*)
rp = OpenWrite[workingdirectory<>"stat/VolumesAndAreas.txt",FormatType->OutputForm];
Write[rp, "Total Accepted Fractures: ", Length[poly]];
Write[rp, "Total Surface Area(m^2): ", finalFractArea*2];
Write[rp, "Total Fracture Volume(m^3): ", finalFractVol];
Write[rp, "Domain Volume(m^3): ", domainVol];
Write[rp, "Fracture Volume / Domain Volume: ", finalFractDensity];
Write[rp, "Surface Area / Domain Volume: ", finalFractArea*2/domainVol];
Write[rp, "Number of Fractures / Domain Volume: ", N[Length[poly]/domainVol],"\n"];
Close[workingdirectory<>"stat/VolumesAndAreas.txt"];



(*write last few graphs for latex*)
(*Export[workingdirectory <> "/stat/adj.png", MatrixPlot[adj, ColorFunction -> "Monochrome"]];

ptPlots = ListPointPlot3D[acnorm, PlotRange -> {{-1, 1}, {-1, 1}, {-1, 1}}, 
BoxRatios -> {1, 1, 1}]; globe = ParametricPlot3D[{Sin[u] Cos[v], Sin[u] Sin[v], Cos[u]}, {u, 0, \[Pi]}, {v, 0, 2 \[Pi]}, 
  PlotStyle -> {Orange, Specularity[White, 10]}, Axes -> None];
Export[workingdirectory <> "stat/norm.png", Show[globe, ptPlots]];

Get[ packagepath <> "smallFunctions.m"];
If[Length[poly] >150, fractLimit = 150,(*else*) 
	fractLimit=Length[poly];]
Export[workingdirectory <> "/stat/dfn.png", 
 Show[polyPlotFunction[poly[[ Range[fractLimit]]]], 
  PlotRange -> {{-domain[[1]], domain[[1]]}*1.1, {-domain[[2]], 
      domain[[2]]}*1.1, {-domain[[3]], domain[[3]]}*1.1},ImageSize->800, BoxRatios -> domain]];

Export[workingdirectory <> "/stat/graphClusterInfo.png", MatrixPlot[clusterInfo, ImageSize->400,Mesh->True, ColorFunction->"Monochrome",FrameStyle->FontSize->20]];
*)
End[]
