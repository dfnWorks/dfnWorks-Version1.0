(* ::Package:: *)

latex = OpenWrite[workingdirectory<>"stat/stat.tex"]; 
Close[workingdirectory<>"stat/stat.tex"];
latex=OpenAppend[workingdirectory<>"stat/stat.tex"];
WriteString[latex, 
"\\documentclass[a4paper,11pt]{article}
\\usepackage{fullpage}
\\usepackage{graphicx}
\\usepackage{listings}
\\usepackage{float}
\\usepackage[section]{placeins}
\\usepackage{setspace}
\\usepackage{framed}
\[IndentingNewLine]\\title{DFN Statistics}
%\\author{Name}
\\begin{document}
\\maketitle \n"];

If[Import[workingdirectory<>"stat/warnings.txt", "Text"]!="",
WriteString[latex,
"\\begin{framed}
\\begin{center}
\\lstinputlisting{warnings.txt}
\\end{center}
\\end{framed}"]];

WriteString[latex, 
"This file contains data regarding the results of the DFN generation process.\[IndentingNewLine]
\\begingroup
\\def\\addvspace#1{}
\\tableofcontents
\\endgroup\[IndentingNewLine]
\\section{General Info}
\\lstinputlisting{report.txt}\[IndentingNewLine]
\\section{Mesh Info}
\\lstinputlisting{finalmesh.txt}

\\section{Time Profile}
\\lstinputlisting{time_profile.txt}\[IndentingNewLine]

\[IndentingNewLine]\\section{Rejection Reasons}
\[IndentingNewLine]\\lstinputlisting{rejReason.txt}

\[IndentingNewLine]\\newpage
\\section{Mathematica Input File}
\\lstinputlisting{mathInput.m}
\[IndentingNewLine]\\end{document}"]
Close[latex]; 


