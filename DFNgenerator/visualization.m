(* ::Package:: *)

Begin["smallFunctions`"]
Get[ packagepath <> "smallFunctions.m"]; 


(* plot DFN *)
If[cntac <= 150,
Export[workingdirectory<>"stat/dfn.png", 
	Show[polyPlotFunction[poly],
	PlotRange->{{-domain[[1]],domain[[1]]}*1.1,{-domain[[2]],domain[[2]]}*1.1,{-domain[[3]],domain[[3]]}*1.1},
	BoxRatios->domain] ]

, 
Export[workingdirectory<>"stat/dfn.png", 
	Show[polyPlotFunction[poly[[Range[100]]]], (* show the first 100 fractures *)
	PlotRange->{{-domain[[1]],domain[[1]]}*1.1,{-domain[[2]],domain[[2]]}*1.1,{-domain[[3]],domain[[3]]}*1.1},
	BoxRatios->domain] ];
Print["DFN too large to visulaize"] ];


(* normal vector direction plot *)
ptPlots = ListPointPlot3D[acnorm,PlotRange->{{-1,1},{-1,1},{-1,1}},BoxRatios->{1,1,1}];
globe = ParametricPlot3D[{Sin[u] Cos[v], Sin[u]Sin[v],Cos[u]},{u,0,\[Pi]},{v,0,2\[Pi]},PlotStyle->{Orange,Specularity[White,10]},Axes->None];
Export[workingdirectory<>"stat/norm.jpg", Show[globe,ptPlots] ];





End[]
