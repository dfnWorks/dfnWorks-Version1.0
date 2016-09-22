dfnWorks Shell Script read me

file name : runDFN.sh
What it does: The shell script executes the dfnworks workflow from start to finish.

Requirements:

1) Mathematica8.0 or newer

2) Python 2.7

3) PETSC libraries	

4) PFLOTRAN

5) LaGriT

6) OpenMPI

7) gcc compiler

##########################################Setup:
change the paths in  the file runDFN.sh to those appropriate for your machine. 
  
# path settings for  Mathematica, PFLOTRAN, python, LAGRIT, and DFNWorks source code
#~~~~~~~~~~~~~~~~~~ 
 # For PETSC to be used for PFLOTRAN
export PETSC_DIR=/home/satkarra/src/petsc-git/petsc
export PETSC_ARCH=RHEL-6.5-nodebug


# mathematica source files path
export DFNGEN_PATH=/home/nataliia/DFNWorks_UBUNTU/DFNgenerator/
export DFNWORKS_PATH=/home/nataliia/DFNWorks_UBUNTU/
export MATH_KERNEL=/n/local_linux/mathematica8.0/Executables/math
export DFNFLOW_PATH=/home/nataliia/DFNWorks_UBUNTU/PFLOTRAN_pyscripts/
export DFNTRANS_PATH=/home/nataliia/DFNWorks_UBUNTU/ParticleTracking
export PFLOTRAN_DIR=/home/satkarra/src/pflotran-dev-Ubuntu-14.04/
export python_dfn=/scratch/er/dharp/source/epd-7.3-1-rh3-x86_64/bin/python
export lagrit_dfn=/n/swdev/LAGRIT/bin/lagrit_lin
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~  

 


Information about the paths:

DFNGEN_PATH : DFNgenerator folder containing the mathematica scripts for fracture generation. 
 
MATH_KERNEL : mathematica command line executable (math kernel) 

PETSC_DIR and PETSC_ARCH are for PETSC libraries used by PFLOTRAN. 

DFNFLOW_PATH :  PFLOTRAN_pyscripts folder containing python scripts to prepare mesh for PFLOTRAN.
PFLOTRAN_DIR  : location of pflotran
 python_dfn : location of python2.7

lagrit_dfn : location of LaGriT	

Particle tracking: all the source code of DFN transport realization, including makefile and executable (DFNTrans)  
 
##########################################
Execution:

The shell scripts takes three command line inputs

input_1: jobname 
	a new folder named input_1 will be created in the current directory, all files will be produced therein
	
input_2: mathematica input file (*.m)

input_3: number of cpus (integer)
	Number of CPUs used for parallel meshing of fractures and in pflotran

To run the work flow 

>> ./runDFN.sh jobname input.m 4

This will create a folder named "jobname" and use the parameters in input.m to generate a fracture network in that folder. Meshing and flow solution will be computed using 4 processors. 

##########################################

Output: 

Once the job is complete, the following files should appear in the directory jobname.

params.txt  : parameter file from dfnGEN

polys.inp : avs file of fracture boundaries

intersections.inp: avs file of discretized intersection lines between fractures

stat.pdf: pdf with network information. 

full_mesh.gmv: meshed network readable by gmv

full_mesh.inp: meshed network in avs format 

tri_fracture.stor: Control volume information for PFLOTRAN

boundary_output.txt
 
pboundary_bottom.zone: nodes on bottom boundary

pboundary_left_w.zone: nodes on left (west) boundary

pboundary_front_s.zone: nodes on front(south) boundary

pboundary_right_e.zone: nodes on right(east) boundary

pboundary_back_n.zone: nodes on back(north) boundary

pboundary_top.zone: nodes on top boundary

allboundaries.zone: concatenation of *.zone

perm.dat: fracture based permeability

aperture.dat: fracture based aperture

dfn_explicit-000.xmf: Initial conditions for pressure

dfn_explicit-001.xmf: Steady state conditions (reads in dfn_explicit-domain.h5 for visualization)

dfn_explicit-domain.h5: Binary file of steady state conditions

dfn_explicit.h5: Binary file of Steady state conditions

dfn_properties.h5: Binary file of mesh information 

cellinfo.dat: concatenation of pflotran output, pressure

darcyvel.dat: concatenation of pflotran output, fluxes

particle tracking output folder. The ouput should be set in PTDFN_control.dat file (the README file is in DFNWorks/ParticleTracking/). 

##################################

