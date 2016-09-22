(* ::Package:: *)

BeginPackage[ "MyCrossProduct`"]

 MyCross::usage =
        " Cross Product of 3-D vectors - numerical  "

Begin["Private`"] 

  MyCross[ v1_, v2_] := Block[ { a,b,c,x,y,z},
   {a,b,c}=v1;
   {x,y,z}=v2;

  { b*z-c*y, c*x-a*z, a*y-b*x}

  ]


End[ ] 


EndPackage[] 

