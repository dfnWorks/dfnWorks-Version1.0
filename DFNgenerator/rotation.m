(* ::Package:: *)

BeginPackage[ "rotation`"]

Needs["MyCrossProduct`"] 

  rotatePoints::usage = 
	"Rotates a set of CoPlaner Points to be parallel to the xy plane"

  Begin[ "private`"]

  rotatePoints[list_, vect_, eps_] := Module[ {pointList = list, nvect = vect, out, numPoints, i, \[Theta], V, e3, R },

	numPoints = Length[pointList];
	out = ConstantArray[ {0,0,0}, numPoints];

	e3 = UnitVector[3,3];
	nvect = Normalize[nvect];

	If[ nvect != {0,0,1},
		(*Create Rotation Matrix*)
		\[Theta] = N[ArcCos[Dot[nvect,e3]]];
		V =  N[MyCross[nvect,e3]];

		R = RotationMatrix[\[Theta],V];

		For [ i = 1, i <= numPoints, i++,
			out[[i]] =  N[Dot[R,pointList[[i]]]];
			(*If the rotation Causes Points to be eps close of zero, set those Points to 0.*)
			If[ Abs[out[[i,1]]] < eps ,
				out[[i,1]] = 0;
			];
			If[ Abs[out[[i,2]]] < eps ,
				out[[i,2]] = 0;
			];
			If[ Abs[out[[i,3]]] < eps ,
				out[[i,3]] = 0;
			];
		];
	];
	If[ nvect == {0,0,1},
		For [ i = 1, i <= numPoints, i++,
			out[[i]] =  N[pointList[[i]]];
		];
	];
	out
]


  End[]

  EndPackage[]




