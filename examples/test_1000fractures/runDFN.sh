#!/bin/sh
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~ Three main parts of DFNWorks ~~~~~~~~~~~~~~~~~~~~~~~~~  
# 1. DFNGen:  Mathemtica script for fractures generation, python script for meshing
# using  LAGRIT, calls LaTeX for statistical report.
#
# 2. DFNFlow: first runs python scripts to prepare all the data for PFLOTRAN input,
# then runs PFLOTRAN
#
# 3. DFNTrans: runs DFNTrans C code executable for particle tracking.

# Note a), each of the parts have their own input  parameters control files, that shuld be 
# edited before running the current script.

# Note b), each of the parts can be performed independently. For example, once DFNGen generates a
# fracture network, the multiple flow solutions can be obtained for the same DFN 
# with different boundary condititions.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

echo 'Copyright (c) 2016, Los Alamos National Security, LLC,All rights reserved.'
echo 'Copyright 2016. Los Alamos National Security, LLC. This software was produced under U.S. Government contract DE-AC52-06NA25396 for Los Alamos National Laboratory (LANL), which is operated by Los Alamos National Security, LLC for the U.S. Department of Energy. The U.S. Government has rights to use, reproduce, and distribute this software.  NEITHER THE GOVERNMENT NOR LOS ALAMOS NATIONAL SECURITY, LLC MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.  If software is modified to produce derivative works, such modified software should be clearly marked, so as not to confuse it with the version available from LANL.'

echo 'Additionally, redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:'
echo ' 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.'
echo '2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.'
echo ' 3. Neither the name of Los Alamos National Security, LLC, Los Alamos National Laboratory, LANL, the U.S. Government, nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.'

echo 'THIS SOFTWARE IS PROVIDED BY LOS ALAMOS NATIONAL SECURITY, LLC AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL LOS ALAMOS NATIONAL SECURITY, LLC OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.'



if [ $# -ne 3 ] ; then 
 echo 'Not enough input arguments supplied'
 echo '1 - name of directory for outputs'
 echo '2 - input filename'
 echo '3 - number of CPU used for meshing and flow solver'
exit 1
fi

 
  
# path settings for  Mathematica, PFLOTRAN, python, LAGRIT, and DFNWorks source code
#~~~~~~~~~~~~~~~~~~ 
 # For PETSC to be used for PFLOTRAN
export PETSC_DIR=/home/satkarra/src/petsc-git/petsc-for-pflotran
export PETSC_ARCH=Ubuntu-14.04-nodebug


# mathematica source files path
export DFNGEN_PATH=/home/nataliia/DFNWorks_UBUNTU/DFNgenerator
export DFNWORKS_PATH=/home/nataliia/DFNWorks_UBUNTU/
export MATH_KERNEL=/n/local_linux/mathematica8.0/Executables/math
export DFNFLOW_PATH=/home/nataliia/DFNWorks_UBUNTU/PFLOTRAN_pyscripts
export DFNTRANS_PATH=/home/nataliia/DFNWorks_UBUNTU/ParticleTracking
export PFLOTRAN_DIR=/home/satkarra/src/pflotran-dev-Ubuntu-14.04/

export python_dfn=/home/nataliia/anaconda2/bin/python2.7
export lagrit_dfn=/n/swdev/LAGRIT/bin/lagrit_lin
#export dfnworks_python=/home/satkarra/src/dfnworks-python
export dfnworks_python=/home/nataliia/DFNWorks_UBUNTU/PFLOTRAN_pyscripts
export PYTHONPATH=/home/satkarra/src
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
echo "Running job"
echo $1
 
echo "Creating output directories"
# create a directory for the DFNWorks output as specified by the first positional parameter
if [ -d $1 ] ; then 
	rm -f -r $1/*
else 
	mkdir -p $1
fi

cp $2 $1 # copy the input file into the specified output directory
# inside the output directory - create a sub-directory "stat" (where the statistics files will be stored) 
if [ -d $1/stat ] ; then 
	rm -r -f $1/stat/*
else 
	mkdir $1/stat
fi


date # print date on terminal

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. DFNGen: Mathematica commands for generating the DFN
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
echo "Creating DFN"
#create a file mathCmd with all of the Mathematica commands.

# clear all previously created Mathematica symbols. 
echo 'Clear["Global`*"]; Clear["Private`*"];'		> mathCmd

# create variable "packagepath" with the path to the folder ontaining the DFN generator code
echo 'packagepath = "'${DFNGEN_PATH}/'";' 	>> mathCmd										
# create variable "workingdirectory" with the path to the folder
# specified as the location of Mathematica output (the first positional parameter).
echo 'workingdirectory = Directory[]<>"/'$1'/";'	>> mathCmd									
# Evaluate all of the expression in the specified (Mathematica) input file (the second positional parameter)	
echo 'Get[ Directory[]<>"/'$2'" ];' 			>> mathCmd	

# run DFNMain and compute how long it takes to run
echo 't = Timing[  Get[packagepath<>"DFNMain.m"]  ][[1]];'  >> mathCmd   
echo 'm = MaxMemoryUsed[];'				>> mathCmd

#define path to the file "time profile"
echo 'rp = OpenAppend[workingdirectory<>"stat/time_profile.txt",FormatType->OutputForm];' >> mathCmd 

# write the total time running time to the file "time profile"
echo 'Write[rp,t, "   total time (seconds)"];' 		>> mathCmd      

# write the max memory used to the file "time profile"
echo 'Write[rp,"Max memory used (bytes):               ", 		
NumberForm[m, DigitBlock -> 3, NumberSeparator -> ","]];' >> mathCmd	

# close the file "time profile"
echo 'Close[workingdirectory<>"stat/time_profile.txt"];'>> mathCmd

# till here the code just created the file mathCmd 

# on Linux - run the created file mathCmd using the Mathematica Kernel/language
 $MATH_KERNEL < mathCmd 

OUT=$?
if [ $OUT -eq 1 ]; then
   exit 1
fi

echo DFN Generation is Done!



if  [ -f $1/params.txt ] 
then 
	echo "Meshing proceeds" 
else
        exit 1
fi
	
# /Applications/Mathematica.app/Contents/MacOS/MathKernel < mathCmd # on Mac
rm -f mathCmd # force removal of the file mathCmd

# copy Mathematica input, Python code into the output directory or sub-directories


# copy Mathematica input, Python code, Matlab code
cp $2 $1

cp ${DFNGEN_PATH}/dfn/new_mesh_DFN.py $1
cp ${DFNGEN_PATH}/dfn/user_function.lgi $1


cp $2 $1/stat/mathInput.m


#  LaGriT: meshes DFN 

cd $1

$python_dfn new_mesh_DFN.py params.txt $3

# PDFLatex: creates report file of DFN statistics
mv finalmesh.txt stat/
cd stat

 
pdflatex stat.tex > log_pdflatex
mv stat.pdf .. 
rm -f stat.aux stat.log mathInput.m

echo "Statistical summary of generated DFN is in stat.pdf file." 
echo
echo

cd ../

# if you would like to stop here
# exit 1

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. DFNFlow: Flow solution
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
echo "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
echo "~~~~ Program: DFNWorks / DFNFlow lead by PFLOTRAN ~~~~~~~~"
echo "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
echo "~~~ Following python scripts will run to prepare ~~~~~~~~~"
echo "~~~~~~ input data for PFLOTRAN running ~~~~~~~~~~~~~~~~~~~"
echo "~~ Developers: Satish Karra, Nataliia Makedonska ~~~~~~~~~"
echo "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" 

# create input files for PFLOTRAN
ln -s ${DFNFLOW_PATH}/driver.py

ln -s ${DFNFLOW_PATH}/postprocess.py

$python_dfn driver.py

rm -rf materialid.dat lagritrun*.txt full_mesh.uge materialid.inp

#run PFLOTRAN
#

#echo $DFNWORKS_PATH
cp ${DFNWORKS_PATH}/test_1000fractures/dfn_explicit.in .
echo "Make sure you use proper input file for PFLOTRAN!!!"
#mpirun -np $3 $PFLOTRAN_DIR/src/pflotran/pflotran -pflotranin dfn_explicit.in
${PETSC_DIR}/${PETSC_ARCH}/bin/mpirun -np $3 $PFLOTRAN_DIR/src/pflotran/pflotran -pflotranin dfn_explicit.in

$python_dfn postprocess.py

cat dfn_explicit-cellinfo-001-rank*.dat > cellinfo.dat  
cat dfn_explicit-darcyvel-001-rank*.dat > darcyvel.dat  

rm -f dfn_explicit-cellinfo*.dat dfn_explicit-darcyvel*.dat

# if you would like to stop here
# exit 1

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5. Running Particle Tracking code 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# create a link to executable

ln -s ${DFNTRANS_PATH}/DFNTrans

# copy the control file for Particle Tracking
echo "Make sure you use proper input file for Transport!!!"
cp ${DFNWORKS_PATH}/test_1000fractures/PTDFN_control.dat .

./DFNTrans 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
echo "Job complete"
echo $1
date # print out the time again
