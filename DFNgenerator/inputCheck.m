(* ::Package:: *)

(*==================================================================================*)
(*function definitions*)
(*==================================================================================*)
(*a function printErr to print errors to both the file errFile and the 
terminal *)
printErr:= ( 
warnings=OpenAppend[errFile,FormatType->OutputForm];
Print[errMessage]; 
WriteString[warnings, errMessage];
Close[warnings]);
(*==================================================================================*)
(*a funcation checkLength for checking that variables are of the correct
length, and if not to print an error to the warnings file and the terminal. *) 
checkLength[variable_, variableName_, desiredLength_]:= 
( 
tmp=variable;
If[ToString[Head[tmp]]!= "List" && desiredLength==1, 
	tmp={variable}];

If[Length[tmp]!=desiredLength, 
errMessage=
"ERROR: aborting program. You've entered "<>ToString[Length[tmp]]<>
" elements, "<>ToString[tmp]<>", for the variable '"<>variableName<>"'.\n"
<>variableName<>" should have "<>ToString[desiredLength]<>" elements. 
In the input file, please correct the number of elemements.";
printErr; Quit[1]];
);
(*___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ __*)
(*a function to check that a list or single element is within a certain range
and exists and is numeric *)
checkValueRangeAbort[variable_, variableName_, minValue_, maxValue_]:=
( 
If[Length[Select[Flatten[{variable}], #<minValue &]]>0, 
errMessage="ERROR: Aborting Program. You've entered a value of "<>ToString[variable] 
<>" for '"<>variableName<>"'. '"<>variableName<>"' should have a value of "<>ToString[minValue]
<>" and above. Please correct the value in the input file.";
printErr; Quit[1],

If[Length[Select[Flatten[{variable}], #>maxValue &]]> 0, 
errMessage="ERROR: Aborting Program. You've entered a value of "<>ToString[variable] 
<>" for '"<>variableName<>"'. '"<>variableName<>"' should have a value of "<>
ToString[maxValue]<>" and below. Please correct the value in the input file.";
printErr; Quit[1]]];

If[ToString[variable]==variableName,
errMessage="ERROR: Aborting program. You did not enter a value for 
the variable '"<>variableName<>"'. This value is required to run the program 
either because you specified in the variable nShape that there are 
certain shapes of fractures present in the problem (and this
variable is necessary to define fractures of this shape), 
or this variable is always required to run the program.";
printErr; Quit[1]];

If[Element[Flatten[{variable}],Reals]==True,i=i,i=i,
errMessage="ERROR: Aborting program. You input a value of '"<>
ToString[variable]<>
"' for variable '"<>variableName<>"'. '"<>variableName<>"' should be a number. 
Please change its value to run the program.";
printErr; Quit[1]
];
);
(*___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___*)
(* a function to check if the variable has one of two specific values *) 
checkValues[variable_, variableName_, possibleValues_]:=
( 
If[variable != possibleValues[[1]] && variable != possibleValues[[2]],
errMessage="ERROR: aborting. "<>variableName<>" can only have a value of "
<>ToString[possibleValues[[1]]]<>" or "<>ToString[possibleValues[[2]]]<>
". Please correct this and re-run the program.";
printErr; Quit[1]]
);
(*___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___*)
(*ensure variable only has integers *)
integerCheck[variable_, variableName_]:=
( 
If[Element[variable, Integers]==False,
errMessage="ERROR: aborting program. U've inputted a value of "
<>ToString[variable]<>
" for the variable "<>variableName<>". 
"<>variableName<>" is only allowed to have integer values.
If you'd like to run the program, you need to change its element/s 
to integers.";
printErr; Quit[1]];
);
(*___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___*)
(*check if less than h *)
lessH[variable_, variableName_]:=
( 
If[Min[variable]<h,
errMessage="WARNING: an element of "<>variableName<>" is less than
the value of h, "<> ToString[h]<>
". This might cause many polygon rejections and program failure.";
printErr;
];
);

(*___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___*)
(*check if more than the biggest side of the domain *)
biggerDomain[variable_, variableName_]:=
( 
If[Max[variable]>Max[domainSize],
errMessage="WARNING: an element of "<>variableName<>", "
<>ToString[Max[variable]]<>", is bigger than the biggest side \n of the domain, "
<>ToString[Max[domainSize]]<>
". \nThis might cause many polygon rejections and program failure.\n"; 
printErr;
];
);
(*___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___*)
(*check if a problem is over-defined *)
overDefinedCheck[parameter1_,parameter1Name_, parameter2_, parameter2Name_]:= 
If[ValueQ[parameter1]==True && ValueQ[parameter2]==True,  
errMessage="ERROR: Aborting program. You've over-defined the
problem and entered in the input file a value for both "<>parameter1Name<>
" and "<>parameter2Name<>". These parameters describe the same property of
the simulation. Please remove from the input file either the variable "<>
parameter1Name<>" or "<>parameter2Name"<>."; 
printErr; Quit[1]];
(*==================================================================================*)
(* Input File Checks *)
(* Check validity/reasonableness of parameters entered in input file *) 
(*==================================================================================*)
(* domainSize *)
(* Description: The size of the domain (x_length*y_length*z_length). *)
(* Acceptable input: 3 element numeral list. *)
(* Required/optional: Always Required. *)
(* Range of Values: -OO <{x,y,z}< OO. *)
(* Sample input: {1,1,1} (creates a 1m*1m*1m domain). *)
(* Unit: m*m*m. *)

(*ensure domain size has three elements *)
checkLength[domainSize, "domainSize", 3];

(*ensure domainSize has a numberical value between O and OO. *)
If [domainSize[[1]] <= 0 || domainSize[[2]] <=0 || domainSize[[3]] <= 0,
errMessage = "ERROR: Aborting program. 'domainSize' {x,y,z} values cannot 
be zero or negative. Please change this in input file.";
printErr; Quit[1];] 



domainVol = domainSize[[1]]*domainSize[[2]]*domainSize[[3]];
(*==================================================================================*)
(*nPoly and density check*)
(*If[ValueQ[density] == True && ValueQ[nPoly==True,
  errMessage = "ERROR: Both nPoly and density are defined in the input file, please 
use one or the other. Use nPoly to get nPoly amount of fractures, or density to get 
a specified density."; printErr; Quit[1];*)


(*==================================================================================*)
(* eps *)
(* Description: minimum tolerance, epsilon. *)
(* Acceptable input: single real postive number. *)
(* Required/optional: Optional, otherwise given default value. *)
(* Range of Values: 0 <eps< OO. *)
(* Sample input: eps=N[Norm[domain]*10^(-10)]]. *)
(* set minimum tolerance, epsilon, to be proportional to the diagonal *) (* eps = 10^(-8); *)

(* assign default value. *) 
If[ValueQ[Global`eps]==False, Global`eps=N[Norm[domain]*10^(-10)],
(*ensure eps has a numerical value between  0 and infinity. *)
checkValueRangeAbort[Global`eps, "eps", 0, \[Infinity]];
(*ensure eps has 1 element *)
checkLength[Global`eps, "eps", 1]; 
];
(*==================================================================================*)
(* eps *)
(* Description: minimum tolerance, epsilon. *)
(* Acceptable input: single real postive number. *)
(* Required/optional: Optional, otherwise given default value. *)
(* Range of Values: 0 <eps< OO. *)
(* Sample input: eps=N[Norm[domain]*10^(-10)]]. *)
(* set minimum tolerance, epsilon, to be proportional to the diagonal *) (* eps = 10^(-8); *)

(* assign default value. *) 
If[ValueQ[eps]==False, eps=N[Norm[domain]*10^(-10)],
(*ensure eps has a numerical value between  0 and infinity. *)
checkValueRangeAbort[eps, "eps", 0, \[Infinity]];
(*ensure eps has 1 element *)
checkLength[eps, "eps", 1]; 
];
 
(*==================================================================================*)
(* h *)
(* Description: The minimum length scale. *)
(* Acceptable input: Single real positive number. *)
(* Required/optional: Always Required. *)
(* Range of Values: eps <h< OO. *)
(* Sample input: 1 (defines 1m). *)
(* Warnings: 
   - if h is less than 1/1000 of the smallest length of the domain.
   - if h is bigger than 1/10 the biggest size of the domain.*)

(*ensure h has a numerical value between eps and infinity. *)
checkValueRangeAbort[h, "h", eps, \[Infinity]];
(*ensure h size has 1 element *)
checkLength[h, "h", 1]; 
If[h<.001*Min[domainSize], 
errMessage="WARNING: your 'h' value is less than 1/1000 of the smallest length
of the domain. This might cause the mesh to be very small and computationally
expensive. You should consider making 'h' bigger.";
printErr;]

If[h>.1*Max[domainSize], 
errMessage="WARNING: your 'h' value is more than 1/10 of the longest length of the 
domain. This will cause a coarse mesh and many rejected polygons. You should consider
making 'h' smaller";
printErr;]

(*==================================================================================*)
(* slope *)
(* Description:  *)
(* Acceptable input:  *)
(* Required/optional: Always Required. *)
(* Range of Values: -OO <slope< OO. *)
(* Sample input: 2 *)
If[ValueQ[slope] == False,
errMessage = "ERROR:'slope' is undefined in input file. 'slope' is a mandatory 
parameter. Please define 'slope' in input file. ";
printErr; Quit[1]; ]

checkValueRangeAbort[slope, "slope", 0, \[Infinity]];

(*==================================================================================*)
(* refineDist *)
(* Description:  *)
(* Acceptable input: . *)
(* Required/optional: Always Required. *)
(* Range of Values:  *)
(* Sample input: *)
(*==================================================================================*)
(* productionMode *)
(* Description: 0 - debugging mode (keeps record of all attempted fractures).
 1- production mode. *)
(* Acceptable input: 0 or 1. *)
(* Required/optional: Always Required. *)
(* Range of Values: 0 or 1. *)
(* Sample input: 1 (production mode). *)
checkValueRangeAbort[productionMode, "productionMode", 0, 1];
(*default*)
If[ValueQ[productionMode] == False, productionMode==1];

(*ensure visualizationMode has 1 element *)
checkLength[productionMode, "productionMode", 1]; 
(*ensure visualizationMode has either 0 or 1 as a value *)
checkValues[productionMode, "productionMode", {0,1}]

checkValueRangeAbort[visualizationMode, "visualizationMode", 0, 1];
(*default*)
If[ValueQ[visualizationMode] == False, visualizationMode==1];

(*ensure visualizationMode has 1 element *)
checkLength[visualizationMode, "visualizationMode", 1]; 
(*ensure visualizationMode has either 0 or 1 as a value *)
checkValues[visualizationMode, "visualizationMode", {0,1}]
(*==================================================================================*)
(* nShape *)
(* Description: Numbers of each type of family.*)
(* Acceptable input: 0-4 element integer list. *)
(* Required/optional: Always Required. *)
(* Range of Values: Each integer in {a,b,c,d} can be any integer 0 and higher. *)
(* Sample input: {1,2,3,4} -> 1 family of elliptical fractures, 
2 fractures of rectangular families, 3 user defined ellipses,
and 4 user defined rectangles. Note that every user defined fracture is 
considered a family. *)

(*ensure nShape has a numerical value between eps and OO. *)
checkValueRangeAbort[nShape, "nShape", 0, \[Infinity]];

(*ensure nShape only has integers *)
integerCheck[nShape, "nShape"];

(*if it's not already in list format - move it to list format*)
If[Head[nShape]==Integer, nShape={nShape}];

(*change it to have 4 elements if it doesn't already have *)
If[Length[nShape]!= 4, 
nShape=Flatten[Append[nShape, ConstantArray[0, 4-Length[nShape]]]]];
(*==================================================================================*)
(* famProb *)
(* Description: The probability (0-1) of encountering a specific family of
				fractures in the network (not including user defined fractures 
				whose probability is 100% so long as they don't create 
				problomatic intersections one with another). *)
(* Acceptable input: A list of real numbers with the number of
					elements equal the total number of elliptical
					and rectangular families  (not including user 
					defined factures). All elements must add up to 1.*)

(* Required/optional: Required only if there are randomly generated elliptical
					  or rectangular families. *)
(* Range of Values: 0 <={a,b,c,d,e....}=< 1 *)
(* Sample input: if nShape={2,1,4,6}, famProb can have a value of
				 famProb={0.25,0.25,0.5}, which means 25% of the randomly generated 
				 fractures will be from the first elliptical families, 25% of the 
				 fractures will be from the second family of elliptical families and
				 half of the fractures will be from the first rectangular family of 
				 fractures. The 10 individual user defined fractures will 100% be in 
				 the fracture network unless they intersect with one another because 
				 user defined fractures are inserted first into the network.  Note 
				 that every user defined fracture is considered a family.*)

(*check if famProb should have a value (are there randomly generated 
fractures?)*)
If[nShape[[1]]!=0 || nShape[[2]]!=0,
(*if famProb should have a value, ensure all famProb elements have a numerical value
 between 0 and 1. *)
checkValueRangeAbort[famProb, "famProb", 0, 1];
(*ensure famProb has the same number of elements as the sum of randomly 
generated elliptical and rectangular families. *)
If[Length[famProb]!= nShape[[1]]+nShape[[2]],
errMessage="ERROR: Aborting Program. The number of requested random rectangular and 
elliptical families: "<>
ToString[nShape[[1]]+nShape[[2]]]<>
", and the number of defined family-probabilities (in variable famProb): "<>
ToString[Length[famProb]]<>", do not match. Please return to the input file and 
ensure you have specified family probabilities (through the variable famProb) 
for the sum of the requsted number of elliptical and rectangular families.
(As you may recall from your thorough reading of the manual, no probability need 
be specified for the user defined fractures).";
printErr; Quit[1]];

(*check that the family probabilities add up to 1 or more. Abort if add up
to less than 1. *)
If[Total[famProb]<1,
errMessage=StringJoin["ERROR: Aborting Program. The sum of the elements of famProb equal ",
ToString[Total[famProb]],". Family probabilities should add up",
"to 1. Please return to the input file, correct the varaible famProb, so the ",
"sum of its elements equal to 1."]; printErr; Quit[1]];

(*check that the family probabilities add up to 1 or more. Issue warning if
add up to more than 1. *)
If[Total[famProb]>1,
errMessage=StringJoin["WARNING: the sum of the elements of famProb equal ",
ToString[Total[famProb]],". Family probabilities should add up ",
"to 1. The program will only accept the families realted to the first ",
"100% of probabilities (that is, if the first family has 0.80 probabilities, and ",
"the second family has 0.40 probability, the program will consider the second ",
"family as having only 0.20 percent probability). It is recommended that you stop ",
"the program, and correct the varaible famProb, so the sum of its elements equal ",
"to 1."]; printErr],
If[ValueQ[famProb]==False, 
	famProb={},
	If[famProb!= {}, 
		errMessage="WARNING: you've entered a value of "<>ToString[famProb]<>
" for the variable famProb even though none was necessary becasue there are no 
randomly generated fractures you specified in nShape. The program has
automatically changed the value of famProb for you to famProb={};\n";
printErr;famProb={}]
]
]
(*==================================================================================*)
(* constantAperture *)
(* Description: Constant aperture to all fractures. *) 
(* Acceptable input: Single positive real number. *)
(* Required/optional: Semi-optional - one of the aperture parameters must be
					  defined in the input file. The program will not run 
					  if one of the following is not given: constantAperture,
					  meanAperture, apertureFromTransmissivity , or
					  lengthCorrelatedAperture. *)
(* Range of Values: eps < constantAperture <largest domain size *)
(* Sample input: Log[ 10^-3 ]. *)
(* Units: m *)

If[ValueQ[constantAperture]==True,
(*ensure constantAperture has a numerical value between 10^-30 and 1*)
checkValueRangeAbort[constantAperture, "constantAperture", 10^-30, 1];
(*ensure meanPermeability has 1 element *)
checkLength[constantAperture, "constantAperture", 1]; 
];
(*==================================================================================*)
(* constantPermeability *)
(* Description: Constant permeability to all fractures.*) 
(* Acceptable input: Single positive real number. *)
(* Required/optional: Optional. *)
(* Range of Values: 10^-30 <constantPermeability< 1. *)
(* Sample input: 10^-13. *)
(* Units: m^2 *)

If[ValueQ[constantPermeability]==True,
(*ensure meanPermeability has a numerical value between 10^-30 and 1*)
checkValueRangeAbort[constantPermeability, "constantPermeability", 10^-30, 1];
(*ensure meanPermeability has 1 element *)
checkLength[constantPermeability, "constantPermeability", 1]; 

If[ValueQ[constantPermeability]==True && ValueQ[meanAperture]==True,
errMessage="WARNING: Both constantPermeabiliy and meanAperture are defined in input file. 
constantPermeabiliy will be ignored. In order to use constantPermeability, you must also use 
constantAperture.\n";
printErr;
]
If[ValueQ[constantPermeability]==True && ValueQ[lengthCorrelatedAperture]==True,
errMessage="WARNING: Both constantPermeability and lengthCorrelatedAperture are defined in input file. 
constantPermeability will be ignored. In order to use constantPermeability, you must also use 
constantAperture.\n";
printErr;
]

];
(*==================================================================================*)
(* meanPermeability *)
(* Description: mean of log normal distribution of permeability. 
				Each fracture is assigned a permeability value according to 
				a lognormal distribution. The aperture value isn't used to
				generate the fractures, but is used in writing the input 
				files for the flow solution softwares. *) 
(* Acceptable input: Single real number. *)
(* Required/optional: Optional, otherwise given default value. *)
(* Range of Values: 10^-30 <meanPermeability< 1. *)
(* Sample input: Log[ 10^-13 ]. *)
(* Units: m^2 *)
If[ValueQ[meanPermeability]==True,
(*ensure meanPermeability has a numerical value between 10^-30 and 1*)
checkValueRangeAbort[meanPermeability, "meanPermeability", 10^-30, 1];
(*ensure meanPermeability has 1 element *)
checkLength[meanPermeability, "meanPermeability", 1]; 
];
(*==================================================================================*)
(*multipleClusters*)
If[ValueQ[multipleClusters]== False,
errMessage="multipleClusters has no value. multipleClusters must be 
equal to 0 or 1. 0 to keep only the largest cluster, and 1 to keep 
all clusters.";
printErr; Quit[1];]

If[multipleClusters != 0 && multipleClusters != 1,
errMessage="multipleClusters does not have a value of 0 or 1. multipleClusters must be 
equal to 0 or 1. 0 to keep only the largest cluster, and 1 to keep 
all clusters.";
printErr; Quit[1];]
(*==================================================================================*)
If[ValueQ[density]==True && ValueQ[nPoly]==False,
errMessage="When using density, both density and nPoly must be defined. Please define 
nPoly in input file. Program will stop at nPoly or when requested density has been 
reached. Whichever comes first.";
printErr; Quit[1];]


(*==================================================================================*)
(* sdPermeability *) 
(* Description: Standard deviation of log normal distribution of permeability. 
				Each fracture is assigned an permeability value according to 
				a lognormal distribution. The permeability value isn't used to
				generate the fractures, but is used in writing the input 
				files for the flow solution softwares. *)
(* Acceptable input: Single real number. *)
(* Required/optional: Optional, otherwise given default value. *)
(* Range of Values: 0 <sdPermeability< \[Infinity]. *)
(* Sample input: 0.3. *)

If[ValueQ[sdPermeability]==True,
(*ensure sdPermeability has a numerical value between 0 and \[Infinity]*)
checkValueRangeAbort[sdPermeability, "sdPermeability", 0, \[Infinity]];
(*ensure sdPermeability has 1 element *)
checkLength[sdPermeability, "sdPermeability", 1]; 
];

(* set up the Permeability lognormal DISTribution, pdist, using meanPermeability and
sdPermeability, which were defined in the input file.*)
If[ValueQ[meanPermeability]==True && ValueQ[sdPermeability]==True,
pdist = N[LogNormalDistribution[ meanPermeability, sdPermeability ]]]; 
(*==================================================================================*)
(* apertureFromTransmissivity  *)
(* Description: Variable with two elements F, and Kb, {F, Kb}. 
				Together these define the aperture and prermeability for 
				the fractures, according to the equations: 
radiusAverage=Mean[{xradius, yradius}];
kb=Global`apertureFromTransmissivity [[2]];
apertureMeanF=Global`apertureFromTransmissivity [[1]];
transmissivity=apertureMeanF*radiusAverage^kb;
aperture=(transmissivity*12)^(1/3); 
perm=aperture^2/12;
perm={perm, perm, perm};
				F is the average aperture of the fractures in the problem.
				Radius - is the radius of each specific fracture.
				Kb is the power to which the radius increases/decreases.  
				The aperture and permeability values are not used to generate the
				fractures, but used in writing the input files for the 
				flow solution software. 
				If this option is chosen, then the permeability will be defined
				according to the aperture as described in the equations above.
				*)
(* Acceptable input: two real numbers in a list. *)
(* Required/optional: Some value for aperture is required - but can be
					  given either through the variables meanAperture 
					  together with stdAperture, or through 
					  apertureFromTransmissivity . *)
(* Range of Values: 0 < apertureFromTransmissivity [[1]] < OO (F). 
				    0 < apertureFromTransmissivity [[2]] < 2 (Kb). *)	
(* Sample input: {10^-5, 0.5} (F value of 10^-5, Kb value of 0.5). *)
(* Units: F is in m. *)

overDefinedCheck[meanAperture,"meanAperture", apertureFromTransmissivity ,
"apertureFromTransmissivity "];
overDefinedCheck[meanAperture,"meanAperture", lengthCorrelatedAperture,
"lengthCorrelatedAperture"];
overDefinedCheck[meanAperture,"meanAperture", constantAperture,
"constantAperture"];
overDefinedCheck[apertureFromTransmissivity ,"apertureFromTransmissivity ", 
constantAperture,"constantAperture"];
overDefinedCheck[apertureFromTransmissivity ,"apertureFromTransmissivity ", 
constantPermeability,"constantPermeability"];
overDefinedCheck[apertureFromTransmissivity ,"apertureFromTransmissivity ", 
lengthCorrelatedAperture,"lengthCorrelatedAperture"];
overDefinedCheck[lengthCorrelatedAperture,"lengthCorrelatedAperture", 
constantAperture,"constantAperture"];

If[ValueQ[apertureFromTransmissivity ]==False && 
ValueQ[meanAperture]==False && ValueQ[stdAperture]==False && 
ValueQ[lengthCorrelatedAperture]==False && ValueQ[constantAperture]==False,  
errMessage="ERROR: aborting program. We have to inform you that you did not 
enter a value for either apertureFromTransmissivity  or constantAperture,
or lengthCorrelatedAperture, or the combination of stdAperture and meanAperture
- leaving the aperture value undefined. 
Please enter a value for one of these variables, and try running 
the program again.";
printErr; Quit[1];]

If[ValueQ[apertureFromTransmissivity ]==True,

(*ensure F has a numerical value between 0 and infinity*)
checkValueRangeAbort[apertureFromTransmissivity [[1]], "apertureFromTransmissivity [[1]]", 0, \[Infinity]];
(*ensure Kb has a numerical value between 0 and 2*)
checkValueRangeAbort[apertureFromTransmissivity [[2]], "apertureFromTransmissivity [[2]]", 0, 2];

(*ensure apertureFromTransmissivity  has 2 element *)
checkLength[apertureFromTransmissivity , "apertureFromTransmissivity ", 2] 
(*issue warning if meanAperture has a value above above half the size of the
domain. *)
];
(*==================================================================================*)
(*General aperture check. *)
(*Make sure at least 1 and no more than 1 aperture type defined*)
If[ ValueQ[meanAperture]==False && ValueQ[constantAperture]==False
&& ValueQ[apertureFromTransmissivity ]==False && ValueQ[lengthCorrelatedAperture]==False,
errMessage="ERROR: No aperture specification. At least one aperture 
type must be used. Please check input for correct aperture type. ";
printErr; Quit[1];]

If[ValueQ[meanAperture]==True && ValueQ[constantAperture]==True,
errMessage = "Both 'meanAperture' and 'constantAperture' are defined. Only one 
aperture type can used. Please fix this in input file. ";
printErr; Quit[1]; ]

If[ValueQ[meanAperture]==True && ValueQ[apertureFromTransmissivity ]==True,
errMessage = "Both 'meanAperture' and 'apertureFromTransmissivity ' are defined. Only one 
aperture type can used. Please fix this in input file. ";
printErr; Quit[1]; ]

If[ValueQ[meanAperture]==True && ValueQ[lengthCorrelatedAperture]==True,
errMessage = "Both 'meanAperture' and 'lengthCorrelatedAperture' are defined. Only one 
aperture type can used. Please fix this in input file. ";
printErr; Quit[1]; ]

If[ValueQ[apertureFromTransmissivity ]==True && ValueQ[lengthCorrelatedAperture]==True,
errMessage = "Both 'lengthCorrelatedAperture' and 'apertureFromTransmissivity ' are defined. Only one 
aperture type can used. Please fix this in input file. ";
printErr; Quit[1]; ]

If[ValueQ[constantAperture]==True && ValueQ[lengthCorrelatedAperture]==True,
errMessage = "Both 'constantAperture' and 'lengthCorrelatedAperture' are defined. Only one 
aperture type can used. Please fix this in input file. ";
printErr; Quit[1]; ]

If[ValueQ[apertureFromTransmissivity ]==True && ValueQ[constantAperture]==True,
errMessage = "Both 'lengthCorrelatedAperture' and 'apertureFromTransmissivity ' are defined. Only one 
aperture type can used. Please fix this in input file. ";
printErr; Quit[1]; ]


(*==================================================================================*)

(* lengthCorrelatedAperture *)
(* Description: Determination of the aperture according to the radius,
				following the equation: aperture=F*R^b. Where F is the
				mean aperture, R is the fracture radius and be is a 
				power. The aperture value isn't used to generate the
				fractures, but is used in writing the input files for the 
				flow solution software. *)
(* Acceptable input: Two positive real numbers in list format
					 {F, b}. *)
(* Required/optional: Semi-optional-> either lengthCorrelatedAperture,
					  or meanAperture with stdAperture, or
					  apertureFromTransmissivity  must be defined. *)
(* Range of Values:0 < lengthCorrelatedAperture[[1]] < OO (F). 
				    0.5 < lengthCorrelatedAperture[[2]] < 2 (b). *)	
(* Sample input: {10^-5, 0.5} (F value of 10^-5, b value of 0.5). *)
(* Units: F is in m. *)

If[ValueQ[lengthCorrelatedAperture]==True,

(*ensure F has a numerical value between 0 and infinity*)
checkValueRangeAbort[lengthCorrelatedAperture[[1]], "lengthCorrelatedAperture[[1]]", 0, \[Infinity]];
(*ensure b has a numerical value between 0.5 and 2*)
checkValueRangeAbort[lengthCorrelatedAperture[[2]], "lengthCorrelatedAperture[[2]]", 0.5, 2];

(*ensure lengthCorrelatedAperture has 2 element *)
checkLength[lengthCorrelatedAperture, "lengthCorrelatedAperture", 2] 
(*issue warning if meanAperture has a value above above half the size of the
domain. *)
];
(*==================================================================================*)
(* meanAperture *)
(* Description: Mean of log normal distribution of aperture. Each fracture
				is assigned an aperture value according to a lognormal 
				distribution. The aperture value isn't used to generate the
				fractures, but is used in writing the input files for the 
				flow solution software. *)
(* Acceptable input: Single real number. *)
(* Required/optional: Always Required. *)
(* Range of Values: eps < meanAperture <largest domain size *)
(* Sample input: Log[ 10^-3 ]. *)
(* Units: m *)

If[ValueQ[meanAperture]==True,
(*ensure meanAperture has a numerical value between 0 and infinity*)
(*checkValueRangeAbort[meanAperture, "meanAperture", 0, \[Infinity]];*)
(*ensure meanAperture has 1 element *)
checkLength[meanAperture, "meanAperture", 1]; 
(*issue warning if meanAperture has a value above above half the size of the
domain. *)
If[meanAperture>.5*Max[domainSize], 
errMessage="You've assigned the fractures a mean aperture value of: "
<>ToString[meanAperture]<>", which is higher than half the size of the
biggest domain side. This seems slightly un-natural. Perhpas you 
might consider lowering the fracture aperture.";
printErr; Quit[1]];
];

(*==================================================================================*)
(* stdAperture *)
(* Description: Standard deviation of log normal distribution of aperture. 
				Each fracture is assigned an aperture value according to 
				a lognormal distribution. The aperture value isn't used to
				generate the fractures, but is used in writing the input 
				files for the flow solution softwares. *)
(* Acceptable input: Single real number. *)
(* Required/optional: Always Required. *)
(* Range of Values: 0<stdAperture< OO. *)
(* Sample input: 0.3. *)

If[ValueQ[stdAperture]==True,
(*ensure stdAperture has a numerical value between 0 and infinity*)
checkValueRangeAbort[stdAperture, "stdAperture", 0, \[Infinity]];
(*ensure stdAperture has 1 element *)
checkLength[stdAperture, "stdAperture", 1]; 
];

(* set up the APerture lognormal DISTribution, apdist, using meanAperture and
stdAperture, which were defined in the input file.*)
If[ ValueQ[meanAperture]==True && ValueQ[stdAperture]==True, 
apDist = N[LogNormalDistribution[ meanAperture, stdAperture ]]]; 

(*==================================================================================*)
(* easpect *)
(* Description: Aspect ratio of the elliptical families. *)
(* Acceptable input: A list of real numbers with the number of elements 
					equal the number of randomly generated
					elliptical families. *)
(* Required/optional: Only when there are randomly generated elliptical 
					  families required in the problem. *)
(* Range of Values: 0 < easpect < OO. *)
(* Sample input: if there are three elliptical families, easpect could be
				 {2,3,1} \[Dash] meaning the fractures of the first elliptical
				 family will have aspect ratios of 2, the fracures of the
				 second elliptical families will have an aspect ratio of 
				 3, and the third elliptical family fractures will have 
				 aspect ratios of 1*)
If[nShape[[1]]>0,
(*ensure easpect has a numerical value between  0 and infinity. *)
checkValueRangeAbort[easpect, "easpect", 0, \[Infinity] ];
(*ensure easpect has nShape[[1]] elements *)
checkLength[easpect, "easpect", nShape[[1]]]; 
];
(*==================================================================================*)
(* etheta *)
(* Description: the angles the elliptical family fractures' normals make 
				with the vertical z-axis. *)
(* Acceptable input: A list of real numbers with the number of elements 
					equal the number of randomly generated
					elliptical families. *)
(* Required/optional: Only when there are randomly generated elliptical 
					  families required in the problem. *)
(* Range of Values: 0 < etheta < 2*pi *)
(* Sample input: if there are three elliptical families, ethetta could 
				 be {pi/4, pi/6, 2pi/3}, and these would be the theta 
				 angles of elliptical fractures from the first, second
				 and third elliptical families, respectfully. *)
(* Units: radians. *)
If[nShape[[1]]>0,
(*ensure etheta has a numerical value between  0 and 2pi. *)
checkValueRangeAbort[etheta, "etheta", 0, N[2*Pi]];
(*ensure etheta has nShape[[1]] elements *)
checkLength[etheta, "etheta", nShape[[1]]]; 
(*if it's not already in list format - move it to list format*)
If[Head[etheta]==Real || Head[etheta]==Integer, etheta={etheta}];
];
(*==================================================================================*)
(* ephi *)
(* Description: The angles the elliptical family fractures' normals make 
				with the x-axis. *)
(* Acceptable input: A list of real numbers with the number of elements 
					equal the number of randomly generated
					elliptical families. *)
(* Required/optional: Only when there are randomly generated elliptical 
					  families required in the problem. *)
(* Range of Values: 0 < ephi < 2*pi. *)
(* Sample input: if there are three elliptical families, ethetta could be
				 {pi/4, pi/6, 2pi/3}, and these would be the phi angles 
				 of elliptical fractures from the first, second and third
				 elliptical families, respectfully. *)
If[nShape[[1]]>0,
(*ensure ephi has a numerical value between  0 and 2pi. *)
checkValueRangeAbort[ephi, "ephi", N[-2*pi], N[2*pi]];
(*ensure ephi has nShape[[1]] elements *)
checkLength[ephi, "ephi", nShape[[1]]]; 
(*if it's not already in list format - move it to list format*)
If[Head[ephi]==Integer || Head[ephi]==Real, ephi={ephi}];
];
(*==================================================================================*)
(* ekappa *)
(* Description: Kr. a parameter for the fisher distributions. The bigger,
				the more similar (less diverging) are the normal of the 
				fractures of the elliptical family fractures. *)
(* Acceptable input: A list of real numbers with the number of elements 
					equal the number of randomly generated
					elliptical families. *)
(* Required/optional: Only when there are randomly generated elliptical 
					  families required in the problem. *)
(* Range of Values: 0 <ekappa< OO. *)
(* Sample input: three elliptical families could have ekappa of {2.6,20,9},
				 and those number would be the kappa values for the first, 
				 second and third randomly generated elliptical families,
				 respectfully.   *)
If[nShape[[1]]>0,
(*ensure ekappa has a numerical value between  0 and infinity. *)
checkValueRangeAbort[ekappa, "ekappa", 0, \[Infinity]];
(*ensure ekappa has nShape[[1]] elements *)
checkLength[ekappa, "ekappa", nShape[[1]]]; 
(*if it's not already in list format - move it to list format*)
If[Head[ekappa]==Real || Head[ekappa]==Integer, ekappa={ekappa}];
];
(*==================================================================================*)
(* enumPoints *)
(* Description: The number of verticies for the elliptical fractures. 
				To be represented discretly/numerically, the ellipses are 
				actually polygons with a finite number of vertices.*)
(* Acceptable input: A list of positive integers with the number of elements 
					equal the number of randomly generated
					elliptical families. *)
(* Required/optional: Only when there are randomly generated elliptical 
					  families required in the problem. *)
(* Range of Values: 5 <enumPoints< OO
					***** though theoretically, there should be a 
					warning here or somewhere about edges being smaller
					than h.*** *)
(* Sample input: 3 elliptical families could have enumpoints={9, 19, 10}, 
				which are respectfully the number of vertices of the 
				fractures from the first,second and third families of 
				randomly generated elliptical fractures. *)
If[nShape[[1]]>0,
(*ensure enumPoints has a numerical value between  5 and infinity. *)
checkValueRangeAbort[enumPoints, "enumPoints", 5, \[Infinity]];
(*ensure ekappa has nShape[[1]] elements *)
checkLength[enumPoints, "enumPoints", nShape[[1]]]; 
(*if it's not already in list format - move it to list format*)
If[Head[enumPoints]==Real || Head[enumPoints]==Integer, enumPoints={enumPoints}];

(*ensure enumPoints only has integers *)
integerCheck[enumPoints, "enumPoints"];

];
(*==================================================================================*)
(* edistr *)
(* Description: The Flags for size distribution of fractures*)
(* Acceptable input: A list of positive integers. *)
(* Required/optional: Only when there are randomly generated elliptical 
					  families required in the problem. *)
(* Range of Values: 1, 2, 3,4 
					*)
(* Sample input: 1 - Log Nornmal Size Distribution
				2 - Power law Distribution
				3 - Exponential Distribution
				4 - Constant size of all polygons *)
If[nShape[[1]]>0,
(*ensure enumPoints has a numerical value between  5 and infinity. *)
checkValueRangeAbort[edistr, "edistr", 1,4];
(*ensure ekappa has nShape[[1]] elements *)
checkLength[edistr, "edistr", nShape[[1]]]; 
(*if it's not already in list format - move it to list format*)
(*ensure enumPoints only has integers *)
integerCheck[edistr, "edistr"];

];
(*==================================================================================*)
(* emean *)
(* Description: The mean value for the lognormal distribution of 
				main-axes lengths of the randomly generated elliptical 
				family fractures. *)
(* Acceptable input: A list of real numbers with the number of elements 
					equal the number of randomly generated
					elliptical families. *)
(* Required/optional: Only when there are random elliptical families 
					  required in the problem, and their lengths are 
					  determined by log normal distribution (edistr=1). *)
(* Range of Values: h < emean < OO. *)
(* Sample input: if there are 3 families of ellipses, emean could be 
				 {.8, .5,.7}, which are respectfully the mean value of the
				 first, second and third elliptical families. *)
If[Position[edistr,1]!={},
If[nShape[[1]]>0,
(*ensure emean has nShape[[1]] elements *)
checkLength[emean, "emean", nShape[[1]]]; 
(*if it's not already in list format - move it to list format*)
If[Head[emean]==Real || Head[emean]==Integer, emean={emean}];

(*issue warning if emean is less than h *)
lessH[emean, "emean"];

(*issue warning if emean is more than the biggest side of the domain.*)
biggerDomain[emean, "emean"];
]];
(*==================================================================================*)
(* ellmean *)
(* Description: The mean value for the exponential distribution of 
				main-axes lengths of the randomly generated elliptical 
				family fractures. *)
(* Acceptable input: A list of real numbers with the number of elements 
					equal the number of randomly generated
					elliptical families. *)
(* Required/optional: Only when there are random elliptical families 
					  required in the problem, and their lengths are 
					  determined by exponential distribution (edistr=3). *)
(* Range of Values: h < ellmean < OO. *)
(* Sample input: if there are 3 families of ellipses, ellmean could be 
				 {.8, .5,.7}, which are respectfully the mean value of the
				 first, second and third elliptical families. *)
If[Position[edistr,3]!={},
If[nShape[[1]]>0,
(*ensure emean has nShape[[1]] elements *)
checkLength[ellmean, "ellmean", nShape[[1]]]; 
(*if it's not already in list format - move it to list format*)
If[Head[ellmean]==Real || Head[ellmean]==Integer, ellmean={ellmean}];

(*issue warning if ellmean is less than h *)
lessH[ellmean, "ellmean"];

(*issue warning if ellmean is more than the biggest side of the domain.*)
biggerDomain[ellmean, "ellmean"];
]];
(*==================================================================================*)
(* esd *)
(* Description: The standard deviation of the lognormal distribution of 
				the lengths of the main axis of the randomly generated
				elliptical family fractures.  *)
(* Acceptable input: A list of real numbers with the number of elements 
					equal the number of randomly generated
					elliptical families. *)
(* Required/optional: Only when there are randomly generated elliptical 
					  families required in the problem, and their lengths  
                      are determined by log normal distribution 
					 (edistr=1). *)
(* Range of Values: 0 <esd< OO. *)
(* Sample input: if there are 3 families of ellipses, esd could be  
				{.8, .5,.7}, which are respectfully the standard deviation 
				value of the first, second and third elliptical families. *)
If[Position[edistr,1]!={},
If[nShape[[1]]>0,
(*ensure esd has a numerical value between  0 and infinity. *)
checkValueRangeAbort[esd, "esd",0, \[Infinity]];
(*ensure esd has nShape[[1]] elements *)
checkLength[esd, "esd", nShape[[1]]]; 
(*if it's not already in list format - move it to list format*)
If[Head[esd]==Real || Head[esd]==Integer, esd={esd}];
]];
(*==================================================================================*)
(* emin *)
(* Description: The minimum value of the truncated power law distribution
				dictating the lengths of the main axis of the elliptical 
				family fractures. *)
(* Acceptable input: A list of real numbers with the number of elements 
					equal the number of randomly generated
					elliptical families. *)
(* Required/optional: Only when there are randomly generated elliptical 
					  families required in the problem, and their lengths  
                      are determined by power law distribution 
					 (edistr=2). *)
(* Range of Values: h < emin < biggest side of the domain. *)
(* Sample input: if there are 3 families of ellipses, emin could be  
				{.8, .5,.7}, which are respectfully the minimum cutoff 
				value of the first, second and third elliptical families. *)

If[Position[edistr,2]!={},
If[nShape[[1]]>0,
(*ensure emin has a numerical value between  0 and infinity. *)
checkValueRangeAbort[emin, "emin",0, \[Infinity]];
(*ensure emin has nShape[[1]] elements *)
checkLength[emin, "emin", nShape[[1]]]; 
(*if it's not already in list format - move it to list format*)
If[Head[emin]==Real || Head[emin]==Integer, emin={emin}];

(*issue warning if emean is less than h *)
lessH[emin, "emin"];

(*issue warning if emean is more than the biggest side of the domain.*)
biggerDomain[emin, "emin"];
]];

(*==================================================================================*)
(* emax *)
(* Description: The maximum value of the truncated power law distribution
				dictating the lengths of the main axis of the elliptical 
				family fractures. *)
(* Acceptable input: A list of real numbers with the number of elements 
					equal the number of randomly generated
					elliptical families. *)
(* Required/optional: Only when there are randomly generated elliptical 
					  families required in the problem, and their lengths  
                      are determined by power law distribution 
					 (edistr=2). *)
(* Range of Values: emin < emax < biggest side of the domain *)
(* Sample input: if there are 3 families of ellipses, emax could be  
				 {.8, .5,.7}, which are respectfully the maximum cutoff 
				 value of the first, second and third elliptical families. *)

If[Position[edistr,2]!={},
If[nShape[[1]]>0,
(*ensure emax has a numerical value between  0 and infinity. *)
checkValueRangeAbort[emax, "emax",0, \[Infinity]];
(*ensure emax has nShape[[1]] elements *)
checkLength[emax, "emax", nShape[[1]]]; 
(*if it's not already in list format - move it to list format*)
If[Head[emax]==Real || Head[emax]==Integer, emax={emax}];

(*issue warning if emax is less than emin *)
Do[If[emax[[i]]<emin[[i]],
errMessage="ERROR: aborting program. One of the elements of emax 
has a lower value than its matching element in emin. emax cannot have a 
lower value than emin. Please correct the value of emax or emin in the input file."; 
printErr; Quit[1]],
{i, Length[emax]}];

(*issue warning if emax is more than the biggest side of the domain.*)
biggerDomain[emax, "emax"];
]];

(*==================================================================================*)
(* ealpha *)
(* Description: the alpha value of the truncated power law distribution 
				of lengths of the main axis of the randomly generated
				elliptical family fractures. *)
(* Acceptable input: A list of real numbers with the number of elements 
					equal the number of randomly generated
					elliptical families. *)
(* Required/optional: Only when there are randomly generated elliptical 
					  families required in the problem, and their lengths  
                      are determined by power law distribution 
					 (edistr=2). *)
(* Range of Values: 0 < ealpha < OO. *)
(* Sample input: if there are 3 families of ellipses, alpha could be  
				{.8, .5,.7}, which are respectfully the alpha value of 
				the first, second and third elliptical families. *)

If[Position[edistr,2]!={},
If[nShape[[1]]>0,
(*ensure ealpha has a numerical value between  0 and infinity. *)
checkValueRangeAbort[ealpha, "ealpha",0, \[Infinity]];
(*ensure ealpha has nShape[[1]] elements *)
checkLength[ealpha, "ealpha", nShape[[1]]]; 
(*if it's not already in list format - move it to list format*)
If[Head[ealpha]==Real || Head[ealpha]==Integer, ealpha={ealpha}];
]];
(*==================================================================================*)
(* rdistr *)
(* Description: Type of distribution of the lengths of the rectangular 
				families. 1 - lognormal distribution; 2 - truncated power 
				law distribution, 3 - Exponential distribution, 4- Constant Size.). *)
(* Acceptable input: 1 through 4. *)
(* Required/optional: Only when there are random rectangular families 
					  required in the problem. *)
(* Range of Values: 1 through 4. *)
(* Sample input: 1 (lognormal distribution). *)

If[nShape[[2]]>0,
(*ensure rdistr has a numerical value between  1 and 2. *)
checkValueRangeAbort[rdistr, "rdistr", 1, 4];
(*ensure rdistr has 1 element *)
checkLength[rdistr, "rdistr", nShape[[2]]] 
(*ensure rdistr has either 2 or 1 as a value *)
(*checkValues[rdistr, "rdistr", {4, 3,2,1}]*)
];
(*==================================================================================*)
(* rconst *)
(* Description: rconst is a length of rectangulars in case of constant size of 
of  the rectangular families. *)
(* Acceptable input: A  real number *)
(* Required/optional: Only when there is rdist=4. *)
(* Range of Values: 0 < rconst < OO. *)
If [Position[rdistr,4]!={},
If[nShape[[2]]>0,
(*ensure rconst has a numerical value between  0 and infinity. *)
checkValueRangeAbort[rconst, "rconst", 0, \[Infinity] ];
(*ensure rconst has nShape[[2]] elements *)
checkLength[rconst, "rconst", nShape[[2]]]; 
] ];
(*==================================================================================*)
(* econst *)
(* Description: econst is a length of ellipses in case of constant size of 
of  the ellipses families. *)
(* Acceptable input: A  real number *)
(* Required/optional: Only when there is edist=4. *)
(* Range of Values: 0 < econst < OO. *)
If [Position[edistr,4]!={},
If[nShape[[1]]>0,
(*ensure raspect has a numerical value between  0 and infinity. *)
checkValueRangeAbort[econst, "econst", 0, \[Infinity] ];
(*ensure econst has nShape[[1]] elements *)
checkLength[econst, "econst", nShape[[1]]]; 
] ];
(*==================================================================================*)
(* raspect *)
(* Description: Aspect ratio of the rectangular families. *)
(* Acceptable input: A list of real numbers with the number of elements 
					equal the number of randomly generated
					rectangular families. *)
(* Required/optional: Only when there are randomly generated rectangular 
					  families required in the problem. *)
(* Range of Values: 0 < raspect < OO. *)
(* Sample input: if there are three rectangular families, raspect could be
				 {2,3,1} \[Dash] meaning the fractures of the first elliptical
				 family will have aspect ratios of 2, the fracures of the
				 second elliptical families will have an aspect ratio of 
				 3, and the third elliptical family fractures will have 
				 aspect ratios of 1*)
If[nShape[[2]]>0,
(*ensure raspect has a numerical value between  0 and infinity. *)
checkValueRangeAbort[raspect, "raspect", 0, \[Infinity] ];
(*ensure raspect has nShape[[2]] elements *)
checkLength[raspect, "raspect", nShape[[2]]]; 
];
(*==================================================================================*)
(* rtheta *)
(* Description: the angles the rectangular family fractures' normals make 
				with the vertical z-axis. *)
(* Acceptable input: A list of real numbers with the number of elements 
					equal the number of randomly generated
					rectangular families. *)
(* Required/optional: Only when there are randomly generated rectangular 
					  families required in the problem. *)
(* Range of Values: 0 < rtheta < 2*pi *)
(* Sample input: if there are three rectangular families, ethetta could 
				 be {pi/4, pi/6, 2pi/3}, and these would be the theta 
				 angles of rectangular fractures from the first, second
				 and third rectangular families, respectfully. *)
(* Units: radians. *)
If[nShape[[2]]>0,
(*ensure rtheta has a numerical value between  0 and 2pi. *)
checkValueRangeAbort[rtheta, "rtheta", 0, N[2*Pi]];
(*ensure rtheta has nShape[[2]] elements *)
checkLength[rtheta, "rtheta", nShape[[2]]]; 
(*if it's not already in list format - move it to list format*)
If[Head[rtheta]==Real || Head[rtheta]==Integer, rtheta={rtheta}];
];
(*==================================================================================*)
(* rphi *)
(* Description: The angles the rectangular family fractures' normals make 
				with the x-axis. *)
(* Acceptable input: A list of real numbers with the number of elements 
					equal the number of randomly generated
					rectangular families. *)
(* Required/optional: Only when there are randomly generated rectangular 
					  families required in the problem. *)
(* Range of Values: 0 < rphi < 2*pi. *)
(* Sample input: if there are three rectangular families, ethetta could be
				 {pi/4, pi/6, 2pi/3}, and these would be the phi angles 
				 of rectangular fractures from the first, second and third
				 rectangular families, respectfully. *)
If[nShape[[2]]>0,
(*ensure rphi has a numerical value between  0 and 2pi. *)
checkValueRangeAbort[rphi, "rphi", 0, N[2*pi]];
(*ensure rphi has nShape[[2]] elements *)
checkLength[rphi, "rphi", nShape[[2]]]; 
(*if it's not already in list format - move it to list format*)
If[Head[rphi]==Integer || Head[rphi]==Real, rphi={rphi}];
];
(*==================================================================================*)
(* rkappa *)
(* Description: Kr. a parameter for the fisher distributions. The bigger,
				the more similar (less diverging) are the normal of the 
				fractures of the rectangular family fractures. *)
(* Acceptable input: A list of real numbers with the number of elements 
					equal the number of randomly generated
					rectangular families. *)
(* Required/optional: Only when there are randomly generated rectangular 
					  families required in the problem. *)
(* Range of Values: 0 <rkappa< OO. *)
(* Sample input: three rectangular families could have rkappa of {2.6,20,9},
				 and those number would be the kappa values for the first, 
				 second and third randomly generated rectangular families,
				 respectfully.   *)
If[nShape[[2]]>0,
(*ensure rkappa has a numerical value between  0 and infinity. *)
checkValueRangeAbort[rkappa, "rkappa", 0, \[Infinity]];
(*ensure rkappa has nShape[[2]] elements *)
checkLength[rkappa, "rkappa", nShape[[2]]]; 
(*if it's not already in list format - move it to list format*)
If[Head[rkappa]==Real || Head[rkappa]==Integer, rkappa={rkappa}];
];

(*==================================================================================*)
(* rmean *)
(* Description: The mean value for the lognormal distribution of 
				main-axes lengths of the randomly generated rectangular 
				family fractures. *)
(* Acceptable input: A list of real numbers with the number of elements 
					equal the number of randomly generated
					rectangular families. *)
(* Required/optional: Only when there are random rectangular families 
					  required in the problem, and their lengths are 
					  determined by log normal distribution (edistr=1). *)
(* Range of Values: h < rmean < OO. *)
(* Sample input: if there are 3 families of ellipses, rmean could be 
				 {.8, .5,.7}, which are respectfully the mean value of the
				 first, second and third rectangular families. *)
If[Position[rdistr,1]!={},
If[nShape[[2]]>0,
(*ensure rmean has nShape[[2]] elements *)
checkLength[rmean, "rmean", nShape[[2]]]; 
(*if it's not already in list format - move it to list format*)
If[Head[rmean]==Real || Head[rmean]==Integer, rmean={rmean}];

(*issue warning if rmean is less than h *)
lessH[rmean, "rmean"];

(*issue warning if rmean is more than the biggest side of the domain.*)
biggerDomain[rmean, "rmean"];
]];
(*==================================================================================*)
(* rellmean *)
(* Description: The mean value for the exponential distribution of 
				main-axes lengths of the randomly generated rectangular 
				family fractures. *)
(* Acceptable input: A list of real numbers with the number of elements 
					equal the number of randomly generated
					rectangular families. *)
(* Required/optional: Only when there are random rectangular families 
					  required in the problem, and their lengths are 
					  determined by exponential distribution (rdistr=3). *)
(* Range of Values: h < rellmean < OO. *)

If[Position[rdistr,3]!={},
If[nShape[[2]]>0,
(*ensure rellmean has nShape[[2]] elements *)
checkLength[rellmean, "rellmean", nShape[[2]]]; 
(*if it's not already in list format - move it to list format*)
If[Head[rellmean]==Real || Head[rellmean]==Integer, rellmean={rellmean}];

(*issue warning if rellmean is less than h *)
lessH[rellmean, "rellmean"];

(*issue warning if rellmean is more than the biggest side of the domain.*)
biggerDomain[rellmean, "rellmean"];
]];
(*==================================================================================*)
(* rsd *)
(* Description: The standard deviation of the lognormal distribution of 
				the lengths of the main axis of the randomly generated
				rectangular family fractures.  *)
(* Acceptable input: A list of real numbers with the number of elements 
					equal the number of randomly generated
					rectangular families. *)
(* Required/optional: Only when there are randomly generated rectangular 
					  families required in the problem, and their lengths  
                      are determined by log normal distribution 
					 (edistr=1). *)
(* Range of Values: 0 <rsd< OO. *)
(* Sample input: if there are 3 families of ellipses, rsd could be  
				{.8, .5,.7}, which are respectfully the standard deviation 
				value of the first, second and third rectangular families. *)
If[Position[rdistr,1]!={},
If[nShape[[2]]>0,
(*ensure rsd has a numerical value between  0 and infinity. *)
checkValueRangeAbort[rsd, "rsd",0, \[Infinity]];
(*ensure rsd has nShape[[2]] elements *)
checkLength[rsd, "rsd", nShape[[2]]]; 
(*if it's not already in list format - move it to list format*)
If[Head[rsd]==Real || Head[rsd]==Integer, rsd={rsd}];
]];
(*==================================================================================*)
(* rmin *)
(* Description: The minimum value of the truncated power law distribution
				dictating the lengths of the main axis of the rectangular 
				family fractures. *)
(* Acceptable input: A list of real numbers with the number of elements 
					equal the number of randomly generated
					rectangular families. *)
(* Required/optional: Only when there are randomly generated rectangular 
					  families required in the problem, and their lengths  
                      are determined by power law distribution 
					 (edistr=2). *)
(* Range of Values: h < rmin < biggest side of the domain. *)
(* Sample input: if there are 3 families of ellipses, rmin could be  
				{.8, .5,.7}, which are respectfully the minimum cutoff 
				value of the first, second and third rectangular families. *)

If[Position[rdistr,2]!={},
If[nShape[[2]]>0,
(*ensure rmin has a numerical value between  0 and infinity. *)
checkValueRangeAbort[rmin, "rmin",0, \[Infinity]];
(*ensure rmin has nShape[[2]] elements *)
checkLength[rmin, "rmin", nShape[[2]]]; 
(*if it's not already in list format - move it to list format*)
If[Head[rmin]==Real || Head[rmin]==Integer, rmin={rmin}];

(*issue warning if emean is less than h *)
lessH[rmin, "rmin"];

(*issue warning if emean is more than the biggest side of the domain.*)
biggerDomain[rmin, "rmin"];
]];

(*==================================================================================*)
(* rmax *)
(* Description: The maximum value of the truncated power law distribution
				dictating the lengths of the main axis of the rectangular 
				family fractures. *)
(* Acceptable input: A list of real numbers with the number of elements 
					equal the number of randomly generated
					rectangular families. *)
(* Required/optional: Only when there are randomly generated rectangular 
					  families required in the problem, and their lengths  
                      are determined by power law distribution 
					 (edistr=2). *)
(* Range of Values: rmin < rmax < biggest side of the domain *)
(* Sample input: if there are 3 families of ellipses, rmax could be  
				 {.8, .5,.7}, which are respectfully the maximum cutoff 
				 value of the first, second and third rectangular families. *)

If[Position[rdistr,2]!={},
If[nShape[[2]]>0,
(*ensure rmax has a numerical value between  0 and infinity. *)
checkValueRangeAbort[rmax, "rmax",0, \[Infinity]];
(*ensure rmax has nShape[[2]] elements *)
checkLength[rmax, "rmax", nShape[[2]]]; 
(*if it's not already in list format - move it to list format*)
If[Head[rmax]==Real || Head[rmax]==Integer, rmax={rmax}];

(*issue warning if rmax is less than rmin *)
Do[If[rmax[[i]]<rmin[[i]],
errMessage="ERROR: Aborting program. One of the elements of rmax 
has a lower value than its matching element in rmin. rmax cannot 
have a lower value than rmin. Please correct the value of rmax 
or emin in the input file."; 
printErr; Quit[1]],
{i, Length[rmax]}];

(*issue warning if rmax is more than the biggest side of the domain.*)
biggerDomain[rmax, "rmax"];
]];

(*==================================================================================*)
(* ralpha *)
(* Description: the alpha value of the truncated power law distribution 
				of lengths of the main axis of the randomly generated
				rectangular family fractures. *)
(* Acceptable input: A list of real numbers with the number of elements 
					equal the number of randomly generated
					rectangular families. *)
(* Required/optional: Only when there are randomly generated rectangular 
					  families required in the problem, and their lengths  
                      are determined by power law distribution 
					 (edistr=2). *)
(* Range of Values: 0 < ralpha < OO. *)
(* Sample input: if there are 3 families of ellipses, alpha could be  
				{.8, .5,.7}, which are respectfully the alpha value of 
				the first, second and third rectangular families. *)

If[Position[rdistr,2]!={},
If[nShape[[2]]>0,
(*ensure ralpha has a numerical value between  0 and infinity. *)
checkValueRangeAbort[ralpha, "ralpha",0, \[Infinity]];
(*ensure ralpha has nShape[[2]] elements *)
checkLength[ralpha, "ralpha", nShape[[2]]]; 
(*if it's not already in list format - move it to list format*)
If[Head[ralpha]==Real || Head[ralpha]==Integer, ralpha={ralpha}];
]];
(*==================================================================================*)
(* ueb *) 
(* Description: The longest radius (b) of user defined ellipses. *)
(* Acceptable input: A list of real numbers with the number of elements 
					equaling the number of user defined elliptical 
					fractures. *)
(* Required/optional: Required only when there are user defined elliptical
					  fractures. *)
(* Range of Values: h < ueb < biggest side of the domain. *)
(* Sample input: if there are three user defined ellipses, ueb could be 
				 {5,5,2}, which means the first two of the user defines
				 elliptical fractures will have a main axes's length of 5,
				 and the third user defined elliptical fracture will have 
				 a length of 2. Note that every user defined fracture is 
				 considered a family. *)
If[nShape[[3]]>0,

(*ensure ueb has a numerical value between  0 and infinity. *)
checkValueRangeAbort[ueb, "ueb",0, \[Infinity]];
(*ensure ueb has nShape[[3]] elements *)
checkLength[ueb, "ueb", nShape[[3]]]; 
(*if it's not already in list format - move it to list format*)
If[Head[ueb]==Real || Head[ueb]==Integer, ueb={ueb}];

(*issue warning if ueb is less than h *)
lessH[ueb, "ueb"];

(*issue warning if ueb is more than the biggest side of the domain.*)
biggerDomain[ueb, "ueb"];
];

(*==================================================================================*)
(* ueaspect *)
(* Description: The aspect ratios of user defined ellipses. *)
(* Acceptable input: A list of real numbers with the number of elements 
					equaling the number of user defined elliptical 
					fractures. *)
(* Required/optional: Required only when there are user defined elliptical
					  fractures. *)
(* Range of Values: 0 < ueaspect < OO. *)
(* Sample input: if there are three user defined elliptical fractures, 
				 ueaspect could be {1,2,1}, which means the first and 
				 third fractures will have the same aspect ration of 1, 
				 and the second elliptical user defined fracture will 
				 have an aspect ration of 2. *)

If[nShape[[3]]>0,

(*ensure ueaspect has a numerical value between  0 and infinity. *)
checkValueRangeAbort[ueaspect, "ueaspect",0, \[Infinity]];
(*ensure ueaspect has nShape[[3]] elements *)
checkLength[ueaspect, "ueaspect", nShape[[3]]]; 
(*if it's not already in list format - move it to list format*)
If[Head[ueaspect]==Real || Head[ueaspect]==Integer, ueaspect={ueaspect}];

];
(*==================================================================================*)
(* uetanslation *) 
(* Description: The translation vectors for the user defined elliptical 
				fractures. *)
(* Acceptable input: A list of vectors with the number of vectors 
					equaling the number of user defined elliptical 
					fractures. *)
(* Required/optional: Required only when there are user defined elliptical
					  fractures. *)
(* Range of Values: -OO <{x,y,z}< OO. *)
(* Sample input: for two elliptical fractures etanslation could be 
				 {{-2,0,0}, {-1,2,0}}. The first and second vectors specify
				 respectfully the coordinate location of the center of the 
				 first and second user defined elliptical fractures. *)

If[nShape[[3]]>0,

(*ensure uetanslation has a numerical value between  0 and infinity. *)
checkValueRangeAbort[uetanslation, "uetanslation",-\[Infinity], \[Infinity]];
(*ensure ueaspect has nShape[[3]] elements *)
checkLength[uetanslation, "uetanslation", nShape[[3]]]; 
For[i=1, i<=nShape[[3]], i++,
	If[Length[uetanslation[[i]]]!=3,
	errMessage=StringJoin["ERROR: aborting program. You've entered a value
	of '",ToString[uetanslation[[i]]], "' for one of the translation vectors.
	There should be 3 elements (x,y,z) in each vector and you've entered
	only ", ToString[Length[uetanslation[[i]]]], " for one of the vectors."];
	printErr; Quit[1]];
]
];
(*==================================================================================*)
(* uenormal *) 
(* Description: The normal vector of the user defined elliptical 
				fractures. *)
(* Acceptable input: A list of real numbers with the number of elements 
					equaling the number of user defined elliptical 
					fractures. *)
(* Required/optional: Required only when there are user defined elliptical
					  fractures. *)
(* Range of Values: -OO <{x,y,z}< OO. *)
(* Sample input: for two user defined fractures, the normal values could
				 be {{1,-0.1,0},{1,0.6,0}}, the first and second vectors 
				 specify the normal vectors of the first and second user 
				 defined elliptical fractures.  *)
If[nShape[[3]]>0,

(*ensure uenormal has a numerical value between  0 and infinity. *)
checkValueRangeAbort[uetanslation, "uenormal",-\[Infinity], \[Infinity]];
(*ensure ueaspect has nShape[[3]] elements *)
checkLength[uenormal, "uenormal", nShape[[3]]]; 
For[i=1, i<=nShape[[3]], i++,
	If[Length[uenormal[[i]]]!=3,
	errMessage=StringJoin["ERROR: aborting program. You've entered a value
	of '",ToString[uenormal[[i]]], "' for one of the user ellipse normal vectors.
	There should be 3 elements (x,y,z) in each noraml vector"];
	printErr; Quit[1]];
]
];
(*==================================================================================*)
(* uenumPoints *)
(* Description: The number of vertices the user defined ellipses should 
				have. *)
(* Acceptable input: A list of positive integers with the number of elements 
					equaling the number of user defined elliptical 
					fractures. *)
(* Required/optional: Required only when there are user defined elliptical
					  fractures. *)
(* Range of Values: 5 <{x,y,z}< OO. *)
(* Sample input: for two user defined elliptical fractures, enumPoints 
				 could be {9,19}, meaning the first user defined
				 elliptical fracture will be represented as a polygon 
				 with 9 vertices, and the second user defined elliptical
				 fracture will have 19 vertices. *)
If[nShape[[3]]>0,

(*ensure uenumPoints has a numerical value between  0 and infinity. *)
checkValueRangeAbort[uenumPoints, "uenumPoints",5, \[Infinity]];
(*ensure uenumPoints has nShape[[3]] elements *)
checkLength[uenumPoints, "uenumPoints", nShape[[3]]]; 
(*ensure uenumPoints only has integers *)
integerCheck[uenumPoints, "uenumPoints"];

];
(*==================================================================================*)
(* urb *)
(* Description: The longest length (b) of user defined rectangular 
				fractures. *)
(* Acceptable input: A list of real numbers with the number of elements 
					equaling the number of user defined rectangular 
					fractures. *)
(* Required/optional: Required only when there are user defined rectangular
					  fractures. *)
(* Range of Values: h < urb < biggest side of the domain. *)
(* Sample input: if there are three user defined rectangles, urb could be 
				 {5,5,2}, which means the first two of the user defines
				 rectangular fractures will have a main axes's length of 5,
				 and the third user defined rectangular fracture will have 
				 a length of 2. Note that every user defined fracture is 
				 considered a family. *)

(* make sure the problem isn't over-defined, and that urb and 
userDefCoordRec are not both specified in the input file *)
If[(ValueQ[urb]==True||ValueQ[uraspect]== True || ValueQ[urtanslation] ==True
|| ValueQ[urnormal]==True) && ValueQ[userDefCoordRec]==True,
errMessage="ERROR: Aborting program. You've over defined 
a user defined rectangular fracture, by giving for it both coordinates 
(userDefCoordRec) and parameters (urb, uraspect, urtanslation, urnomral).
Please remove from the input file either the variable userDefCoordRec
or the parameters: urb, uraspect, urtanslation, urnomral."; 
printErr; Quit[1]];

If[nShape[[4]]>0,
	If[ValueQ[userDefCoordRec]==False,
(*ensure urb has a numerical value between  0 and infinity. *)
checkValueRangeAbort[urb, "urb",0, \[Infinity]];
(*ensure ueb has nShape[[4]] elements *)
checkLength[urb, "urb", nShape[[4]]]; 
(*if it's not already in list format - move it to list format*)
If[Head[urb]==Real || Head[urb]==Integer, urb={urb}];

(*issue warning if ueb is less than h *)
lessH[urb, "urb"];

(*issue warning if urb is more than the biggest side of the domain.*)
biggerDomain[urb, "urb"];
]
];
(*==================================================================================*)
(*User coordinate rectangle checks *)
If[ValueQ[userDefCoordRec]==True && nShape[[4]]>0,

(*Make sure user inputed 4 vertices per rectangle *)
	For[i=1, i<Length[userDefCoordRec], i++,
		If[ Length[userDefCoordRec[[i]]]!=4,
		errMessage="ERROR: Aborting program. User defined coordinates for 
		user-rectangle "<>ToString[i]<>" does not have 4 vertices. Please correct this 
		in input file.";
		printErr; Quit[1];]   ]

(*make sure nShape and number of rec specified by user coordinates match*)
If[ nShape[[4]] != Length[userDefCoordRec],
	errMessage="ERROR: Aborting program. You have "<>ToString[nShape[[4]]]<> " user coordinate 
rectangles defined in nShape, but only "<>ToString[Length[userDefCoordRec]]<>" rectangles coordinates defined in 
userDefCoordRec.";
printErr;
Quit[1]; ]

(*check if user defined rectangular coordinates are on a plane*)
	For[i=1, i<nShape[[4]], i++,
	p1 = userDefCoordRec[[i,1]];
	p2 = userDefCoordRec[[i,2]];
	p3 = userDefCoordRec[[i,3]];
	p4 = userDefCoordRec[[i,4]];
	norm = Normalize[CrossProduct[(p2-p1), (p4-p1)]]; Print["norm = ", norm];
		If[Abs[norm.(p1-p3)]>Global`eps,
		errMessage="Aborting program. User coordinate specified rectangle "<>ToString[i]<>" is not co-planar. 
			This will cause problems in meshing. Please fix this in input file.";
		printErr;
		Quit[1];
		]
	]
]
(*==================================================================================*)
(* uraspect *)
(* Description: The aspect ratios of user defined rectangular fractures. *)
(* Acceptable input: A list of real numbers with the number of elements 
					equaling the number of user defined rectangular 
					fractures. *)
(* Required/optional: Required only when there are user defined rectangular
					  fractures. *)
(* Range of Values: 0 < uraspect < OO. *)
(* Sample input: if there are three user defined rectangular fractures, 
				 uraspect could be {1,2,1}, which means the first and 
				 third fractures will have the same aspect ration of 1, 
				 and the second rectangular user defined fracture will 
				 have an aspect ration of 2. *)

If[nShape[[4]]>0,
If[ValueQ[userDefCoordRec]==False,
(*ensure uraspect has a numerical value between  0 and infinity. *)
checkValueRangeAbort[uraspect, "uraspect",0, \[Infinity]];
(*ensure uraspect has nShape[[4]] elements *)
checkLength[uraspect, "uraspect", nShape[[4]]]; 
(*if it's not already in list format - move it to list format*)
If[Head[uraspect]==Real || Head[uraspect]==Integer, uraspect={uraspect}];

]];
(*==================================================================================*)
(* urtanslation *) 
(* Description: The translation vectors for the user defined rectangular 
				fractures. *)
(* Acceptable input: A list of vectors with the number of vectors 
					equaling the number of user defined rectangular 
					fractures. *)
(* Required/optional: Required only when there are user defined rectangular
					  fractures. *)
(* Range of Values: -OO <{x,y,z}< OO. *)
(* Sample input: for two rectangular fractures etanslation could be 
				 {{-2,0,0}, {-1,2,0}}. The first and second vectors specify
				 respectfully the coordinate location of the center of the 
				 first and second user defined rectangular fractures. *)

If[nShape[[4]]>0,
If[ValueQ[userDefCoordRec]==False,
(*ensure urtanslation has a numerical value between  0 and infinity. *)
checkValueRangeAbort[urtanslation, "urtanslation",-\[Infinity], \[Infinity]];
(*ensure ueaspect has nShape[[4]] elements *)
checkLength[urtanslation, "urtanslation", nShape[[4]]]; 
For[i=1, i<=nShape[[4]], i++,
	If[Length[urtanslation[[i]]]!=3,
	errMessage="ERROR: aborting program. You've entered a value
	of '"<>ToString[urtanslation[[i]]]<>"' for one of the translation vectors.
	There should be 3 elements (x,y,z) in each vector and you've entered
	only "<>ToString[Length[urtanslation[[i]]]]<>" for one of the vectors.";
	printErr; Quit[1]];
]
]];
(*==================================================================================*)
(* urnormal *) 
(* Description: The normal vector of the user defined rectangular 
				fractures. *)
(* Acceptable input: A list of real numbers with the number of elements 
					equaling the number of user defined rectangular 
					fractures. *)
(* Required/optional: Required only when there are user defined rectangular
					  fractures. *)
(* Range of Values: -OO <{x,y,z}< OO. *)
(* Sample input: for two user defined fractures, the normal values could
				 be {{1,-0.1,0},{1,0.6,0}}, the first and second vectors 
				 specify the normal vectors of the first and second user 
				 defined rectangular fractures.  *)
If[nShape[[4]]>0,
If[ValueQ[userDefCoordRec]==False,
(*ensure urnormal has a numerical value between  0 and infinity. *)
checkValueRangeAbort[urnormal, "urnormal",-\[Infinity], \[Infinity]];
(*ensure ueaspect has nShape[[4]] elements *)
checkLength[urnormal, "urnormal", nShape[[4]]]; 
For[i=1, i<=nShape[[4]], i++,
	If[Length[urnormal[[i]]]!=3,
	errMessage=StringJoin["ERROR: aborting program. You've entered a value
	of '",ToString[urnormal[[i]]], "' for one of the translation vectors.
	There should be 3 elements (x,y,z) in each vector and you've entered
	only ", ToString[Length[userDefCoordRec[[i]]]], " for one of the vectors."];
	printErr; Quit[1]];
]]
];
(*==================================================================================*)
(* userDefCoordRec *) 
(* Description: Coordinates for user defined rectangular. *)
(* Acceptable input: A list of lists of 4 {x,y,z} coordinate point, 
					the vertices of the rectangles. *)
(* Required/optional: Required only when there are user defined rectangular
					  fractures defined by coordinates. *)
(* Range of Values: -OO <{x,y,z}< OO. *)
(* Sample input: for two rectangles, 
			   {{{x11, y11, z11},{x12, y12, z12}, {x13, y13, z13}, {x14, y14, z14}},
               {{x21, y21, z21},{x22, y22, z22}, {x23, y23, z23}, {x24, y24, z24}}}*)
(*If[nShape[[4]]>0,
If[ValueQ[userDefCoordRec]==True, 

For[i=1, i<=Length[userDefCoordRec], i++,
	If[Element[userDefCoordRec[[i]], Reals] == True, i=i,i=i,
		errMessage=StringJoin["ERROR: Aborting program. You inputted a value of '",
		ToString[userDefCoordRec[[i]]], "' for one of the user defined coordinates for 
		rectangles. All coordinates should be numerical values. Please change 
		the value in the input file."];			
		printErr; Quit[1]];
	Do[If[Length[userDefCoordRec[[i,j]]]!=3,
	errMessage=StringJoin["ERROR: aborting program. You've entered a value
	of '",ToString[urnormal[[i]]], "' for one of the translation vectors.
	There should be 3 elements (x,y,z) in each vector and you've entered
	only ", ToString[Length[userDefCoordRec[[i]]]], " for one of the vectors."];
	printErr; Quit[1]],
	{j,4}]
]]];
	*)
(*==================================================================================*)
(* boundaryFaces *) 
(* Description: A list of the domain sides for which boundary conditions 
				are given. The program will only keep groups of fracture
				networks that connect between two of the domain sides 
				at which there are specified boundary conditions. 
				1 = top = positive z direction (0,0,1) 
				2 = bottom = bottom = negative z direction (0,0,-1), 
				3 = left_w = left or west = negative x direction (-1,0,0),
				4 = front_s = front or south = negative y direction (0,-1,0), 
				5 = right_e = right or east = positive x direction (1,0,0), 
				6 = back_n = back or north = positive y direction (0,1,0).*)
(* Acceptable input: a list with integers between 1 and 6. *)
(* Required/optional: Optional, otherwise given default value. *)
(* Range of Values: 1,2,3,4,5,6. *)
(* Sample input: {1,2,3} (will save all fracture networks that connect 
				 between faces 1 & 2, or 1&3, or 2 & 3). *) 

(* assign default value. *) 
(*NOTE:default has its own way of functioning, see DFNMain.m Default value is there*)

If[ValueQ[boundaryFaces]==True,
(*ensure boundaryFaces has a numerical value between  1 and 6. *)
checkValueRangeAbort[boundaryFaces, "boundaryFaces", 0, 6];

(* ensure boundaryFaces is an integer. *)
integerCheck[boundaryFaces, "boundaryFaces"];

]
(*==================================================================================*)
(* nPoly *)
(* Description: number of fractures. *)
(* Acceptable input: single real postive number. *)
(* Required: nPoly needs to be defined in input file *)
(* Range of Values: 0 <nPoly< OO. *)
(* Sample input: 5. *)

(* calculate number of fractures *)
If [ValueQ[nPoly] == False,
	errMessage = "ERROR: Aborting program. There is no parameter for nPoly in
input file. nPoly is a mandatory parameter. Please add nPoly to input file.";
printErr; Quit[1]; ]
 
(* ensure nPoly is an integer. *)
integerCheck[nPoly, "nPoly"]; 

If [ nPoly < 1, 
	errMessage= "ERROR: Aborting program. You inputted a value of 
	"<>ToString[nPoly]<> " for variable 'nPoly'. 'nPoly' should be 
	an interger greater than 1. Please change its value.";
	printErr; Quit[1]; ]


(*==================================================================================*)
(*have program set acPoly*)
(*nPoly was being ignored with certain input parameters, having the program set acPoly
makes it easier to understand for the user. Now they just say how many fractures they 
want and run. acPoly will be set to nPoly *)

If [ValueQ[acPoly]==False, 
Global`acPoly = nPoly;
]
(*value for nPoly will be checked below*)

(*==================================================================================*)
(* maxPoly *)
(* Description: The maximum number of fractures that the program will
				attempt to create before stopping the program (the 
				variable exists to set a limit to the number of fractures
				the program will attempt to create, and avoid a situation 
				where the program will endlessly attempt to create more
				fractures in order to reach a certain requested size of a 
				network, but won't succeed b/c the fractures will keep on 
				being rejected for various reasons). *)
(* Acceptable input: single real postive integer. *)
(* Required/optional: Optional, otherwise given default value. *)
(* Range of Values: nPoly <maxPoly< OO. *)
(* Sample input: {1,1,1} (creates a 1m*1m*1m domain). *)

(* will attempt at most maxPoly *)
(* assign default value. *) 
If[ValueQ[maxPoly]==False, maxPoly=100*nPoly,
(* ensure maxPoly is an integer. *)
integerCheck[maxPoly, "maxPoly"];
(*ensure maxPoly has a numerical value between  nPoly and infinity. *)
checkValueRangeAbort[maxPoly, "maxPoly", nPoly, \[Infinity]];
(*ensure maxPoly has 1 element *)
checkLength[maxPoly, "maxPoly", 1];
(*ensure maxPoly is >= acPoly,*)
If[maxPoly<acPoly,
errMessage = "ERROR: maxPoly is less than acPoly. maxPoly must be greater or equal to 
acPoly.";
printErr; Quit[1]; ]  
];
(*==================================================================================*)
(* acPoly *) 
(* Description: The program will accept at most acPoly number of fractures
				before stopping the program. When the program keeps on 
				creating fractures until it reaches a network of a certain 
				size, there is a possibility that the program will accept 
				endless amounts of fractures and no network will form (if
				for example the domain is big and the fractures are small).
				This variabel exists to avoid such a situation and set 
				a limit on the number of accepted fractures. *)
(* Acceptable input: single real postive integer. *)
(* Required/optional: Optional, otherwise given default value. *)
(* Range of Values: nPoly <=acPoly< OO. *)
(* Sample input: {1,1,1} (creates a 1m*1m*1m domain). *)
(* will accept at most acPoly fractures *)

(* ensure acPoly is an integer. *)
integerCheck[acPoly, "acPoly"];
(*ensure acPoly has a numerical value between  nPoly and infinity. *)
checkValueRangeAbort[acPoly, "acPoly", nPoly, \[Infinity]];
(*ensure acPoly has 1 element *)
checkLength[acPoly, "acPoly", 1]; 

(*==================================================================================*)
(* rejPoly *) 
(* Description: The number of maximum rejections in a row before the  
				program will stop.
				After the program fails to create a fracture, it tries again
				to create a fracture of the same size but in a different 
				position.*)
(* Acceptable input: single real postive integer. *)
(* Required/optional: Optional, otherwise given default value. *)
(* Range of Values: 0 < rejPoly< OO. *)
(* Sample input: rejPoly=20. *)

(* assign default value. *)
If[ValueQ[rejPoly]==False, rejPoly=100,
(* ensure rejPoly is an integer. *)
integerCheck[rejPoly, "rejPoly"];
(*ensure rejPoly has a numerical value between  0 and infinity. *)
checkValueRangeAbort[rejPoly, "rejPoly", 0, \[Infinity]];
(*ensure rejPoly has 1 element *)
checkLength[rejPoly, "rejPoly", 1]; 
];
(*==================================================================================*)
(* rpowerLawReject *)
(* Description: maximum number of rejections for power law determined rectangular fractures
before trying to insert fracture of a smaller size. *)
(* Acceptable input: integer postive number. *)
(* Required/optional: optional. *)
(* Range of Values: 0 <nPoly< OO. *)
(* Sample input: 5. *)

If [nShape[[2]]>0 && Position[rdistr, 2]!= {},
(*ensure eps has a numerical value between  0 and infinity. *)
checkValueRangeAbort[rpowerLawReject, "rpowerLawReject", 0, \[Infinity]];
(*ensure eps has nShape[[2]] elements *)
checkLength[rpowerLawReject, "rpowerLawReject", nShape[[2]]]; 
(*ensure rpowerLawRejects only has integers *)
integerCheck[rpowerLawReject, "rpowerLawReject"]
];
(*==================================================================================*)
(* epowerLawReject *)
(* Description: maximum number of rejections for power law determined elliptical fractures
before trying to insert fracture of a smaller size. *)
(* Acceptable input: integer postive number. *)
(* Required/optional: optional. *)
(* Range of Values: 0 <nPoly< OO. *)
(* Sample input: 5. *)

If [nShape[[1]]>0 && Position[edistr, 2]!= {},
(*ensure eps has a numerical value between  0 and infinity. *)
checkValueRangeAbort[epowerLawReject, "epowerLawReject", 0, \[Infinity]];
(*ensure eps has nShape[[1]] elements *)
checkLength[epowerLawReject, "epowerLawReject", nShape[[1]]]; 
(*ensure epowerLawRejects only has integers *)
integerCheck[epowerLawReject, "epowerLawReject"]
];
(*==================================================================================*)
(*density check, rough estimate based on input file*)
(*Give warning if estimated total fracture volume > domain volume*)
(*density check will only work for cntac stopping paramerter*)
(*Cannot estimate fracture volumes for other stopping parameters since number of fractures is unknown*)
(*estVolume = (number of fractures requested)*(family probabliliy %)*(area)*(aperture)     *)
(*NOTE: The order of these If statements matter, else famProb values will not be correct*)
estArea =0;
estVolume =0;
famProbIndex = 1;


If [ ValueQ[userDefCoordRec]==False , (*estimates when using userDefCoordRec not coded yet*)

If[ ValueQ[Global`meanAperture]==True, apTmp = meanAperture;] 
If[ ValueQ[Global`constantAperture] ==True, apTmp= constantAperture;] 


(*ellipse*)
If [ nShape[[1]]>=1 && Position[edistr,1]!={}, (*lognormal distribution*)  (* Print["In If 1"];*)(* TESTING*)
	
	If[ ValueQ[lengthCorrelatedAperture]==True, 
		For[i=1, i<=nShape[[1]], i++,
		apTmp =lengthCorrelatedAperture[[1]] * Mean[{E^emean[[i]], easpect[[i]]*E^emean[[i]]}]^lengthCorrelatedAperture[[2]];
		estAreaFam1 = Global`famProb[[famProbIndex]]*nPoly *Pi*(E^emean[[i]])^2*easpect[[i]];
		estVolume += estAreaFam*apTmp;
		estArea += estAreaFam1;
		famProbIndex++; 
		],(*else*)

		For[i=1, i<=nShape[[1]], i++,
		estAreaFam1 = Global`famProb[[famProbIndex]]*nPoly *Pi*(E^emean[[i]])^2*easpect[[i]];
		estVolume += estAreaFam1*apTmp; 
		estArea += estAreaFam1;
		famProbIndex++;
		]
(*Print["Estimated family 1 area, (edistr = 1) = ", estAreaFam1]; Print["famprobindex = ", famProbIndex];*)(*TESTING*)
	]
]; 
If[ nShape[[1]] >=1 && Position[edistr,2]!={}, (*truncated power law distribution *) (* Print["In If 2"]; *) (*TESTING*)
	If[ValueQ[lengthCorrelatedAperture]==True,
		For[i=1, i<=nShape[[1]], i++,		
		apTmpMaxLength = lengthCorrelatedAperture[[1]] * Mean[{emax[[i]], easpect[[i]]*emax[[i]]}]^lengthCorrelatedAperture[[2]];
		apTmpMinLength = lengthCorrelatedAperture[[1]] * Mean[{emin[[i]], easpect[[i]]*emin[[i]]}]^lengthCorrelatedAperture[[2]];		
		estAreaFam1Min = .9 * Global`famProb[[famProbIndex]]*nPoly*Pi*(emin[[i]])^2 * easpect[[i]]; (*80% from emin*)
		estAreaFam1Max = .1 * Global`famProb[[famProbIndex]]*nPoly*Pi*(emax[[i]])^2 * easpect[[i]]; (*20% from emax*)
		estVolume += estAreaFam1Min * apTmpMinLength; 
		estVolume == estAreaFam1Max * apTmpMaxLength;
		estArea += esetAreaFam1Min + estAreaFam1Max;
		famProbIndex++ 
(*Print["estVolume = ", estVolume];*)(*TESTING*)
		], (*else*) 

		For[i=1, i<=nShape[[1]], i++,
		estAreaFam1Min = .9*Global`famProb[[famProbIndex]]*nPoly*Pi*(emin[[i]])^2 * easpect[[i]]; 
		estAreaFam1Max = .1*Global`famProb[[famProbIndex]]*nPoly*Pi*(emax[[i]])^2 * easpect[[i]];
		estVolume +=  (estAreaFam1Min+estAreaFam1Max) * apTmp;
		estArea += estAreaFam1Min + estAreaFam1Max; 
		famProbIndex++	 
(*TESTING*)(*Print["estVolume = ", estVolume];	*)
		]
	]
(*Print["Estimated family 1 area, (edistr = 2) = ", estAreaFam1Min+estAreaFam1Max]; Print["famprobindex = ", famProbIndex];*) (*TESTING*)
] 
(*Rectangles*)
If[ nShape[[2]] >= 1 && Position[rdistr,1]!={}, (*lognormal distribution*) (*Print["if 3"]; *)(*TESTING*)
	If[ValueQ[lengthCorrelatedAperture]==True,  
		For[i=1, i<=nShape[[2]], i++,
		apTmp = lengthCorrelatedAperture[[1]] * Mean[{E^rmean[[i]], raspect[[i]]*E^rmean[[i]]}]^lengthCorrelatedAperture[[2]];
		estAreaFam2 = Global`famProb[[famProbIndex]]*nPoly*(E^rmean[[i]]*2)^2*raspect[[i]];
		estVolume += estAreaFam2 *apTmp; (*Print["apTmp if 3 = ", apTmp];*)
		(*Print["est family 3 area = ", estAreaFam2];
		Print["Estimated vol = ",estVolume];*)(*TESTING*)
		estArea += estAreaFam2;
		famProbIndex++;
		],(*else*)
	
		For[i=1, i<=nShape[[2]], i++, 
		
		estAreaFam2 = Global`famProb[[famProbIndex]]*nPoly*(E^rmean[[i]]*2)^2*raspect[[i]];
		estVolume += estAreaFam2*apTmp;
		estArea += estAreaFam2;
		famProbIndex++ 
(*Print["estVolume = ", estVolume]*) (*TESTING*)

		]
(*Print["Estimated family 2 area, (edistr = 1) = ", estAreaFam2]; Print["famprobindex = ", famProbIndex];*) (*TESTING*)
	]
]	
If[nShape[[2]] >=1 && Position[rdistr,2]!={}, (*truncated power law distribution*) 
	If[ValueQ[lengthCorrelatedAperture]==True,
		For[i=1, i<=nShape[[2]], i++,
		apTmpMaxLength = lengthCorrelatedAperture[[1]] * Mean[{rmax[[i]], raspect[[i]]*rmax[[i]]}]^lengthCorrelatedAperture[[2]];
		apTmpMinLength = lengthCorrelatedAperture[[1]] * Mean[{rmin[[i]], raspect[[i]]*rmin[[i]]}]^lengthCorrelatedAperture[[2]]; 
		estAreaFam2Min = .9 * Global`famProb[[famProbIndex]]*nPoly*(rmin[[i]])^2 * raspect[[i]];
		estAreaFam2Max = .1 * Global`famProb[[famProbIndex]]*nPoly*(rmax[[i]])^2 * raspect[[i]];
		estVolume += estAreaFam2Min * apTmpMinLength;
		estVolume += estAreaFam2Max * apTmpMaxLength;
		estArea += estAreaFam2Min + estAreaFam2Max;
		], (*else*) 
	
		For[i=1, i<=nShape[[2]], i++,
		estAreaFam2Min = .9*Global`famProb[[famProbIndex]]*nPoly*Pi*(rmin[[i]])^2 * raspect[[i]];
		estAreaFam2Max = .1*Global`famProb[[famProbIndex]]*nPoly*Pi*(rmax[[i]])^2 * raspect[[i]];
		estVolume += (estAreaFam2Min + estAreaFam2Max) * apTmp;
		estArea += estAreaFam2Min + estAreaFam2Max;
		famProbIndex++;  
		]
(*Print["Estimated family 2 area, (edistr = 2) = ", estAreaFam2Min + estAreaFam2Max];Print["famprobindex = ", famProbIndex];*)(*TESTING*)
	]
]
(*User Ellipses*) (*famProb not used for user defined fractures *) 
If[nShape[[3]]>=1,  (*Print["If 5"];  *) (*TESTING*)
	If[ValueQ[lengthCorrelatedAperture]==True,
		For[i=1, i<=nShape[[3]], i++,
		apTmp =  lengthCorrelatedAperture[[1]] * Mean[{ueb[[i]], ueaspect[[i]]*ueb[[i]]}]^lengthCorrelatedAperture[[2]]; 
		estAreaUell =  Pi*(ueb[[i]])^2*ueaspect[[i]];
		estVolume += estAreaUell * apTmp;
		estArea += estAreaUell;
(*Print["estVolume = ", estVolume];*)(*TESTING*)
		], (*else*)
	
		For[i=1, i<=nShape[[3]], i++,
		estAreaUell =  Pi*(ueb[[i]])^2*ueaspect[[i]];
		estVolume += estAreaUell * apTmp;
		estArea += estAreaUell
		(*Print["estVolume = ", estVolume];*) (*TESTING*)
		]			
	]
]	
(*User Rectangles*)
If[nShape[[4]]>=1, (* Print["If 6"]; *) (*TESTING*)
	If[ValueQ[lengthCorrelatedAperture]==True,
		For[i=1, i<=nShape[[4]], i++,
		apTmp =  lengthCorrelatedAperture[[1]] * Mean[{urb[[i]], uraspect[[i]]*urb[[i]]}]^lengthCorrelatedAperture[[2]];
		estAreaUrec = (urb[[i]]*2)^2 * uraspect[[i]]; 
		estVolume += estAreaUrec * apTmp;
		estArea += estAreaUrec
	(*	Print["estVolume = ", estVolume];*) (*TESTING*)
		],(*else*)

		For[i=1, i<=nShape[[4]], i++,
		estAreaUrec = (urb[[i]]*2)^2 * uraspect[[i]];
		estVolume += estAreaUrec * apTmp;
		estArea += estAreaUrec
	(*	Print["estVolume = ", estVolume];*) (*TESTING*)
		]
(*Print["Estimated Urec area = ", estArea];*) (*TESTING*)
	]
]

(*Print["\nEstimations before isolated fractures removed and outside fractures truncated(still testing accuracy):"];
Print["Estimated fracture surface Area = ", 2*N[estArea]];
Print["Estimated fracture volume = ", N[estVolume]];
Print["Domain = ", domainSize[[1]]," x ",domainSize[[2]]," x ",domainSize[[3]]];
Print["(Estimated fracture colume)/(Domain volume) = ", N[estVolume/domainVol], "\n"];*)


]


(*==================================================================================*)
(* createReport *)
(* Description: Whether to print out the statistics report or not. *)
(* Acceptable input: 0 or 1. *)
(* Required/optional: optional. *)
(* Range of Values: 0,1.  1- print report. default.
						  0- don't print report. *)
(* Sample input: 1. *)

(* assign default value. *) 
If[ValueQ[createReport]==False, createReport=1];
(*ensure createReport has 1 element *)
checkLength[createReport, "createReport", 1]; 
(*ensure createReport has either 0 or 1 as a value *)
checkValues[createReport, "createReport", {0,1}];
(*==================================================================================*)
(* bufferOnOff *)
(* Description: Whether to create buffers around intersections or not. *)
(* Acceptable input: 0 or 1. *)
(* Required/optional: optional. *)
(* Range of Values: 0,1.  1- Create buffers. default.
						  0- Don't create buffers. *)
(* Sample input: 1. *)

(* assign default value. *) 
If[ValueQ[bufferOnOff]==False, 
	If[matrixMesh==1, bufferOnOff=1, bufferOnOff=0]];
(*ensure bufferOnOff has 1 element *)
checkLength[bufferOnOff, "bufferOnOff", 1]; 
(*ensure bufferOnOff has either 0 or 1 as a value *)
checkValues[bufferOnOff, "bufferOnOff", {0,1}];


(*==================================================================================*)
(* matrixDiagnosticOnOff *)
(* Description: Whether to create a diagnostic gmv after creating the 
matrix mesh . *)
(* Acceptable input: 0 or 1. *)
(* Required/optional: optional. *)
(* Range of Values: 0,1.  1- Create gmv. default.
						  0- Don't create gmv. *)
(* Sample input: 1. *)

(* assign default value. *) 
If[ValueQ[matrixDiagnosticOnOff]==False, matrixDiagnosticOnOff=0];
(*ensure matrixDiagnosticOnOff has 1 element *)
checkLength[matrixDiagnosticOnOff, "matrixDiagnosticOnOff", 1]; 
(*ensure matrixDiagnosticOnOff has either 0 or 1 as a value *)
checkValues[matrixDiagnosticOnOff, "matrixDiagnosticOnOff", {0,1}];
(*==================================================================================*)
(* nCPU *)
(* Description: The number of CPUs to use during meshing. *)
(* Acceptable input: integer above 0. *)
(* Required/optional: optional. *)
(* Range of Values: 0->OO.  1- default. *)
(* Sample input: 1. *)

(* assign default value. *) 
If[ValueQ[nCPU]==False, nCPU=8];
(*ensure nCPU has 1 element *)
checkLength[nCPU, "nCPU", 1]; 
(*ensure nCPU has a numerical value between  0 and infinity. *)
checkValueRangeAbort[nCPU, "nCPU", 0, \[Infinity]];
(*==================================================================================*)
(*explanation of different variables used in the code - but are not input
file variables *)
(*==================================================================================*)




If[ValueQ[createVisReport] == False, createVisReport=-1];


(*==================================================================================*)
(* intPtsii *)
(* creates a matrix of 2x3, where each element is a vector of length
	 acPoly filled with zeros. *)
(* intPtsii = ConstantArray[SparseArray[{},acPoly],{2,4}]; *)
(*intPtsii[[All,All,jj]] = intersections[[2]];*)
(* the reason the matrix is 2*3 is because it shows the intersection end
   points. 
   so that for example, if acPoly was 7, an intptsii could like as follows:


)
meaning that there are two intersection of the current fracture with fractures
number 2 and 7. the first intersection has endpoints (0.06. 0.22, -2.1213) and
(0.06, 0.22, 2.11). the second intersection has endpoints (1.25, 4.16, -1.2)
and (1.25, 4.16, -.58).*) 


(*==================================================================================*)
(*domainIncrease4Random*)
(*the program chooses a random center for each fracture. it chooses
this random center (or translation) by choosing a random point from 
the domain. However, it wouldn't be correct to choose only centers from
inside the problem domain. perhaps there are fractures that have their center
outside the domain, but are partially inside the domain. 
therefore the domain is increased in each direction by the biggest radius 
of the fractures.*) 

(* assign default value. *) 
If[ValueQ[domainIncrease4Random]==False, 
If[nShape[[2]]==0 && nShape[[1]]==0, domainIncrease4Random=0];

If[nShape[[1]]!=0 && nShape[[2]]!=0, 
domainIncrease4Random=Max[If[Position[edistr,1]!={}, Max[emean],Max[emax]], 
If[Position[rdistr,1]!={}, Max[rmean],Max[rmax]]]];

If[nShape[[2]]!=0 && nShape[[1]]==0, 
If[Position[rdistr,1]!={},domainIncrease4Random=Max[rmean],domainIncrease4Random=Max[rmax]]];

If[nShape[[2]]==0 && nShape[[1]]!=0, 
If[Position[edistr,1]!={}, domainIncrease4Random=Max[emean],domainIncrease4Random=Max[emax]]]
];

