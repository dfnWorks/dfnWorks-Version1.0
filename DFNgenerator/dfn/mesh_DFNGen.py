#!/n/local_linux/bin/python2.7
########## this version merges meshes in parallel 
# Python Code to take the output from mathematica and convert the data into a grid DFN
#
# Usage (default): meshDFN
# Default input file: params.txt
# Default number of processors N_CPU: 4
# Default refine_factor: 1
#
# Optional Argument List
# 1 Argument
# Usage: meshDFN filename
#		Default file name: params.txt
# 2 Argument
# Usage: meshDFN filename N_CPU
#		Default file name: params.txt
#		Default N_CPU: 4 (integer)
# 3 Argument
# Usage: meshDFN filename N_CPU refine_factor
#		Default file name: params.txt
#		Default N_CPU: 4 (integer)
#		Default refine_factor: 1 (integer, allowable values, 1, 2, 4, 8)
#
# Jeffrey Hyman
# EES-16 LANL
# jhyman@lanl.gov
# Command Line Input is the params.txt file output from mathematica
# params.txt needs to be f the format
# nPoly  		Number of polygons in the Set
# h			Built in Length Scale
# 1 THETA X1 Y1 Z1 X2 Y2 Z2  
# 2
# ...
# nPoly THETA X1 Y1 Z1 X2 Y2 Z2

# Where  THETA = angle of rotation to correct polygons
# and a line about which the polygon needs to be rotated is described by the line 
# (X1,Y1,Z1) and (X2, Y2,Z2)

# This script combines the files:
#	create_params.py
#	create_mesh_poly.py
#	create_lagrit_input.py
#	create_merge_poly.py
#	run_lagrit.py
#	runs lagrit < mergepoly.lgi
#	cleanup.py
# Output files are meshi.gmv, meshi.inp, full_mesh.gmv and full_mesh.inp
# individual polygons and merges them into one .inp and .gmv in lagrit
#
# Set up to run triangulation of each fracture polygon in parallel. Set N_CPU to number
# of jobs to run in parallel.
#
from sys import *
from string import *
import os
import time
import sys
from numpy import genfromtxt

def parse_params_file(filename, refine_factor):
	''' Read in params.txt file and parse information'''

	print "\nParse Params.txt: Starting"
	fin = open(filename, 'r')
	# Line 1 is the number of polygons
	nPoly = int(fin.readline())
	print "Number of Polygons:", nPoly
	digits = len(str(nPoly)) ### 

	#Line 2 is the h scale
	h = float(fin.readline())
	print "H_SCALE", h
	if(refine_factor > 1):
		h = h/float(refine_factor)
		print "modified H_SCALE", h
		
	# Line 3 numberOfPoints in a Polygon
	numPoints = int(fin.readline())
	print "Number of Points per polygon", numPoints

	# Line 4 slope is the rate at which the mesh will get coarser in refinement
	slope = float(fin.readline())

	# Line 5 refine_dist is the distance from the intersections which refinement will be performed
	refine_dist = float(fin.readline())

	# Line 6 is the visualization mode: '1' is on, '0' is off.
	visualMode= int(fin.readline())

	# Line 7 is the filename of the avs for the polygon
	poly_file = fin.readline().rstrip()
	print "Polygon File:", poly_file

	# Line 8 is the filename of the avs for the intersections
	intersection_file = fin.readline().rstrip()
	print "Intersection File:", intersection_file
	fin.close()

	print "Parse Params.txt: Complete\n"
	return(nPoly, digits, h, numPoints, slope, refine_dist, visualMode, 
		poly_file, intersection_file)

def split_intersection_file(intersection_file, digits):
	print "Splitting the Intersection File:", intersection_file 
	
	# remove the quotation marks from Mathematica ouput
	os.system("sed -i 's/\"//g' " + intersection_file)

	cmd = 'csplit -n ' + str(digits) + ' -ks -f intersections ' \
	+ intersection_file + " '/ 2 0 0/' {"  + str(nPoly-1) + "}" 
	os.system(cmd) 

	os.system('rm -rf intersections')
	os.mkdir('intersections')

	# rename the files for LaGriT
	cmd = 'mv -f intersections%s  intersections/intersections_%s.inp' 
	for i in range(1,nPoly+1):
		os.system(cmd%(str(i).zfill(digits), str(i).zfill(digits))) 

	print "Splitting the Intersection File: Complete\n" 

def split_poly_file(poly_file, nPoly, digits):

	print "Splitting the Polys File:", poly_file
	os.system("sed -i 's/\"//g' " + poly_file)
	os.system('rm -rf polys')
	os.mkdir('polys')
	f = open(poly_file,'r')
	first_line = f.readline()
	first_line = first_line.split()
	nnodes = int(first_line[0])
	nelements = int(first_line[1])

	nodes_list = []
	for i in range(nnodes):
		nodes_list.append(f.readline())
	id_list = []
	for i in range(nelements):
		this_line = f.readline()
		this_line = this_line.split()
		id_list.append(int(this_line[1]))
	f.close()

	nPoly = id_list[nelements-1]
	id_start = id_list[0]
	count_list = []
	j=1
	for i in range(nelements):
		if (id_list[i] == id_start):
			j = j+1
		else:
			count_list.append(j)
			id_start = id_list[i]
			j = 2
	count_list.append(j)

	if len(count_list) != nPoly:
		print 'Something is wrong in split_poly_file'
		print 'Stop to debug'

	nodes_offset = 0
	for i1 in range(1,nPoly+1):
		if (nPoly > 1000 and (i1 % 1000) == 0):
			print 'Splitting file number ' + str(i1)
		poly_name = 'polys/poly_' + str(i1).zfill(digits)+ '.inp'
		i2 = count_list[i1-1]
		f_out = open(poly_name, 'w')
		f_out.write(str(i2) + ' ' + str(i2-1) + ' '\
			+ str(0) + ' ' + str(0) + ' ' + str(0) + '\n')
		for i3 in range(i2):
			tmp = str(i3+1) + nodes_list[i3+nodes_offset][nodes_list[i3+nodes_offset].find(' '):]
			f_out.write(tmp)
		nodes_offset = nodes_offset + i2
		for i3 in range(i2-1):
			tmp = str(i3+1) + ' ' + str(i1) +  ' line ' \
				+ str(i3+1) + ' ' + str(i3+2) + '\n'
			f_out.write(tmp)
		f_out.flush()
		f_out.close()
	print "Splitting the Polys File: Complete"

def create_parameter_mlgi_file(filename, nPoly):
	#Section 2 : Outputs parameteri.mlgi files used in running LaGriT Script
	print "\nCreating parameteri.mlgi files"
	os.system('rm -rf parameters')
	os.mkdir('parameters')

	#Go through the list and write out parameter file for each polygon
	#to be an input file for LaGriT
	data = genfromtxt(filename, skip_header = 8)
	for i in range(nPoly):
		
		frac_id = str(int(data[i,0]))
		long_name = str(int(data[i,0])).zfill(digits) 	
		theta = data[i,1]	
		x1 = data[i,2]	
		y1 = data[i,3]	
		z1 = data[i,4]	
		x2 = data[i,5]	
		y2 = data[i,6]	
		z2 = data[i,7]	

		fparameter_name = 'parameters/parameters_' + long_name + '.mlgi'
		f = open(fparameter_name, 'w')
		f.write('define / ID / ' + frac_id + '\n')
		f.write('define / reduced_AVS / mesh_reduced_' + long_name + '.inp \n')
		f.write('define / reduced_GMV / mesh_reduced_' + long_name + '.gmv \n')
		f.write('define / reduced_GMV_2D / mesh_reduced_2D_' + long_name + '.gmv\n')
		f.write('define / OUTFILE_GMV / mesh_' + long_name + '.gmv\n')
		f.write('define / OUTFILE_AVS / mesh_' + long_name + '.inp\n')
		f.write('define / POLY_FILE / poly_' + long_name + '.inp\n')
		f.write('define / QUAD_FILE / tmp_quad_' + frac_id + '.inp\n')
		f.write('define / EXCAVATE_FILE / tmp_excavate_' + frac_id + '.inp\n')
		f.write('define / PRE_FINAL_FILE / tmp_pre_final_'+frac_id + '.inp\n')
		f.write('define / PRE_FINAL_MASSAGE / tmp_pre_final_massage_' + frac_id +'.gmv\n')
		f.write('define / H_SCALE / ' + str(h) + '\n')
		f.write('define / H_SCALE2 / ' + str(1.5*h) + '\n')
		f.write('define / H_SCALE4 / ' + str(3*h) + '\n')
		f.write('define / H_SCALE5 / ' + str(8*h) + '\n')
		f.write('define / H_SCALE6 / ' + str(16*h) + '\n')
		f.write('define / H_SCALE6 / ' + str(16*h) + '\n')
		f.write('define / H_PRIME / ' + str(0.05*h) + '\n')
		f.write('define / H_PRIME2 / ' + str(0.3*h) + '\n')
		f.write('define / H_PRIME_M / ' + str(h/40.0) + '\n')
		f.write('define / PURTURB / ' + str(3*h) + '\n')
		f.write('define / PARAM_A / '+str(slope)+'\n')	
		f.write('define / PARAM_A0 / '+str(refine_dist)+'\n')	
		f.write('define / PARAM_B / '+str(h*(1-slope*refine_dist))+'\n')	
		f.write('define / THETA  / '+str(theta)+'\n')
		f.write('define / X1 / '+str(x1)+'\n')
		f.write('define / Y1 / '+str(y1)+'\n')
		f.write('define / Z1 / '+str(z1)+'\n')
		f.write('define / X2 / '+str(x2)+'\n')
		f.write('define / Y2 / '+str(y2)+'\n')
		f.write('define / Z2 / '+str(z2)+'\n')
		f.write('finish \n')
		f.flush()
		f.close()

	print "Creating parameteri.mlgi files: Complete\n"


def create_lagrit_scripts(production_mode, N_CPU, refine_factor): 

	#########################################
	#Section 2 : Creates LaGriT script to be run for each polygon

	#Switches to control the LaGriT output
	#Network visualization mode "ON" ouputs the triangulated mesh
	#for each fracture without any refinement. The goal is to visualize
	#the network structure instead of outputing the appropriate values
	#for computation

	print "Writing LaGriT Control Files"
	#Go through the list and write out parameter file for each polygon
	#to be an input file for LaGriT

	lagrit_input = '''infile %s 
#LaGriT Script
# Name the input files that contain the polygons 
# and lines of intersection. 

# define / POLY_FILE / POLYFILE 
define / POLY_FILE / %s 
define / LINE_FILE / %s 

# Define parameters such as: 
# length scale to refine triangular mesh  
# purturbation distance to break symmetry of refined mesh# 

# Read in line and polygon files  
read / POLY_FILE / mo_poly_work 
read / LINE_FILE / mo_line_work 
# input ID, mo_poly_work, mo_line_work  

'''
	#
	# START: Refine the point distribution
	#
	if(refine_factor > 1):
		lagrit_input += 'extrude / mo_quad_work / mo_line_work / const / H_SCALE5 / volume / 0. 0. 1.  \n'
		if (refine_factor == 2):
			lagrit_input += 'refine/constant/imt1/linear/element/1 0 0 /-1.,0.,0./inclusive amr 2  \n'

		if (refine_factor == 4):
			lagrit_input += 'refine/constant/imt1/linear/element/1 0 0 /-1.,0.,0./inclusive amr 2  \n'
			lagrit_input += 'refine/constant/imt1/linear/element/1 0 0 /-1.,0.,0./inclusive amr 2  \n'
			
		if (refine_factor == 8):
			lagrit_input +='refine/constant/imt1/linear/element/1 0 0 /-1.,0.,0./inclusive amr 2  \n'
			lagrit_input +='refine/constant/imt1/linear/element/1 0 0 /-1.,0.,0./inclusive amr 2  \n'
			lagrit_input +='refine/constant/imt1/linear/element/1 0 0 /-1.,0.,0./inclusive amr 2  \n'
			
		lagrit_input += ''' 
grid2grid / tree_to_fe / mo_quad_work / mo_quad_work  
extract/surfmesh/1,0,0/mo_ext_work/mo_quad_work/external 
compute / distance_field / mo_ext_work / mo_line_work / dfield 
pset / pdel_work / attribute / dfield / 1 0 0 / H_SCALE4 / gt 
rmpoint / pset get pdel_work / inclusive 
rmpoint / compress  
cmo / delete / mo_quad_work 
cmo / delete / mo_line_work
cmo / move / mo_line_work / mo_ext_work 
rmpoint / compress  
'''	
		# END: Refine the point distribution
		#
	lagrit_input += ''' 
cmo / create / mo_pts / / / triplane 
copypts / mo_pts / mo_poly_work 
cmo / select / mo_pts 
triangulate / counterclockwise 

cmo / setatt / mo_pts / imt / 1 0 0 / 1 
resetpts / itp 
cmo / delete / mo_poly_work 

cmo / select / mo_pts 

'''
	if(visualMode == 0):
		lagrit_input += '''
# Creates a Coarse Mesh and then refines it using the distance field from intersections 

massage / H_SCALE6 / 1.e-5 / 1.e-5 
smooth;recon 0;smooth;recon 0;smooth;recon 0  
smooth;recon 0;smooth;recon 0;smooth;recon 0 
		'''

		if numPoints == 4:
			lagrit_input += '''
resetpts / itp 
pset / p_move / attribute / itp / 1 0 0 / 0 / eq 
perturb/ pset get p_move / PURTURB PURTURB 0.0
 
'''
		lagrit_input += ''' 
massage / H_SCALE5 / 1.e-5 / 1.e-5 
smooth;recon 0;smooth;recon 0;smooth;recon 0 
smooth;recon 0;smooth;recon 0;smooth;recon 0

cmo/addatt/ mo_pts /x_four/vdouble/scalar/nnodes 
cmo/addatt/ mo_pts /fac_n/vdouble/scalar/nnodes 
massage2/user_function.lgi/H_SCALE/fac_n/1.e-5/1.e-5/1 0 0/strictmergelength 

# Extrude and excavate the lines of intersection
cmo / select / mo_line_work 

extrude / mo_quad / mo_line_work / const / H_SCALE / volume / 0. 0. 1. 
'''
		if (production_mode == 0):
			lagrit_input += '''
dump / avs / QUAD_FILE / mo_quad 
cmo / delete / mo_quad 
read / QUAD_FILE / mo_quad 
'''
		else:
			lagrit_input += 'cmo / select / mo_quad \n'
		
		lagrit_input += ''' 
trans / 1 0 0 / 0. 0. 0. / 0. 0. H_PRIME_M 
hextotet / 2 / mo_tri / mo_quad 
cmo / delete / mo_quad 

addmesh / excavate / mo_excavate / mo_pts / mo_tri 
cmo / delete / mo_tri 
cmo / delete / mo_pts 

cmo / create / mo_final / / / triplane 
copypts / mo_final / mo_excavate  
compute / distance_field / mo_final / mo_line_work / dfield 
cmo / printatt / mo_final / dfield / minmax 
pset / pdel / attribute dfield / 1,0,0 / lt H_PRIME2 
rmpoint / pset,get,pdel / inclusive  
rmpoint / compress  
copypts / mo_final / mo_line_work  

cmo / select / mo_final 

cmo / setatt / mo_final / imt / 1 0 0 / ID 
cmo / setatt / mo_final / itp / 1 0 0 / 0 
cmo / printatt / mo_final / -xyz- / minmax 
trans/ 1 0 0 / zero / xyz 
cmo / setatt / mo_final / zic / 1 0 0 / 0.0 
cmo / printatt / mo_final / -xyz- / minmax 
connect 

trans / 1 0 0 / original / xyz 
cmo / printatt / mo_final / -xyz- / minmax 

cmo / delete / mo_line_work 
cmo / delete / mo_excavate

cmo / select / mo_final 

resetpts / itp 

'''
		if (production_mode == 0):
			lagrit_input += 'dump / gmv / PRE_FINAL_MASSAGE / mo_final \n'
		
		lagrit_input += '''
pset / pref / attribute / dfield / 1,0,0 / lt / 1.e-6 
pset / pregion / attribute / dfield / 1,0,0 / lt / H_SCALE 
pset / pboundary / attribute / itp / 1,0,0 / eq / 10 
pset / psmooth / not / pregion pref pboundary 
massage / H_SCALE / 1.e-5 / 1.e-5 / pset get pref / & 
nosmooth / strictmergelenth 

smooth / position / esug / pset get psmooth; recon 0; 
smooth / position / esug / pset get psmooth; recon 0; 
smooth / position / esug / pset get psmooth; recon 0; 
smooth / position / esug / pset get psmooth; recon 0; 
# Rotate 
rotateln / 1 0 0 / nocopy / X1, Y1, Z1 / X2, Y2, Z2 / THETA / 0.,0.,0.,/  

cmo / printatt / mo_final / -xyz- / minmax 

recon 1 

resetpts / itp 

cmo / addatt / mo_final / unit_area_normal / xyz / vnorm 
cmo / addatt / mo_final / scalar / xnorm ynorm znorm / vnorm 
cmo / DELATT / mo_final / vnorm 

'''
		# Clean up before output to GMV/AVS
		if (production_mode == 1):
			lagrit_input += '''
cmo / DELATT / mo_final / x_four 
cmo / DELATT / mo_final / fac_n 
cmo / DELATT / mo_final / rf_field_name 
cmo / DELATT / mo_final / xnorm 
cmo / DELATT / mo_final / ynorm 
cmo / DELATT / mo_final / znorm 
cmo / DELATT / mo_final / a_b 
cmo / setatt / mo_final / ipolydat / no 
cmo / modatt / mo_final / icr1 / ioflag / l 
cmo / modatt / mo_final / isn1 / ioflag / l 
		
'''
		lagrit_input += '''
dump / OUTFILE_GMV / mo_final 
dump / OUTFILE_AVS / mo_final

''' 
	else:
		lagrit_input += '''
cmo / setatt / mo_pts / imt / 1 0 0 / ID 
cmo / setatt / mo_pts / itetclr / 1 0 0 / ID 
resetpts / itp 

cmo / setatt / mo_line_work / imt / 1 0 0 / ID 
cmo / setatt / mo_line_work / itetclr / 1 0 0 / ID

addmesh / merge / mo_final / mo_pts / mo_line_work 
cmo / delete / mo_pts 
cmo / delete / mo_line_work 

cmo / select / mo_final 
dump / reduced_GMV_2D / mo_final  
# Rotate 
rotateln / 1 0 0 / nocopy / X1, Y1, Z1 / X2, Y2, Z2 / THETA / 0.,0.,0.,/ 

cmo / printatt / mo_final / -xyz- / minmax 

cmo / modatt / mo_final / icr1 / ioflag / l 
cmo / modatt / mo_final / isn1 / ioflag / l
dump / reduced_AVS / mo_final 

'''

	lagrit_input += '''
 
quality 
cmo / delete / mo_final 
cmo / status / brief 
finish
 
'''
	for i in range(1,N_CPU+1):

		file_name = 'mesh_poly_CPU' + str(i) + '.lgi'
		f = open(file_name, 'w')
		#Name of parameter Input File
		fparameter_name = 'parameters_CPU' + str(i) + '.mlgi' 
		fintersection_name = 'intersections_CPU' + str(i) + '.inp'
		fpoly_name = 'polys_CPU' + str(i) + '.inp'

		parameters = (fparameter_name, fpoly_name, fintersection_name) 
		f.write(lagrit_input%parameters)
		f.flush()
		f.close()

	print 'Writing LaGriT Control Files: Complete'

def triangulate_polygons(nPloy, N_CPU):

	'''
	Section 3 : Triangulate Each Polygon
	This procedure is run in parallel
	
	This python script runs lagrit to mesh each individual fractures
	Line Format
	os.system( "lagrit < mesh_polyi.lgi ")
	''' 
	
	print "\nTriangulate Polygons:", nPoly

	os.system('rm -rf lagrit_logs')
	os.mkdir('lagrit_logs')

	# If the number of processors is greater than the number of 
	# polygons, reset N_CPU
	if ( N_CPU > nPoly):
		N_CPU = nPoly

	os.system("rm -f log_lagrit*  ")
	nfinished = 0
	pid_index = {}
	pid_index_CPU = {}
	for i in range(N_CPU):

		i_cpu = str(i+1).zfill(digits)
 
		cmd = 'ln -s polys/poly_' + i_cpu + '.inp ' \
			+ 'polys_CPU' + str(i+1) + '.inp'
		os.system(cmd)

		cmd = 'ln -s parameters/parameters_' + i_cpu + '.mlgi '\
			 + 'parameters_CPU' + str(i+1) + '.mlgi'
		os.system(cmd)

		cmd = 'ln -s intersections/intersections_' + i_cpu + '.inp '\
			 + 'intersections_CPU' + str(i+1) + '.inp'
		os.system(cmd)

		cmd = lagirt_path + ' < mesh_poly_CPU' + str(i+1) + '.lgi' \
			 + ' > lagrit_logs/log_lagrit_' + i_cpu 
			  
		# fork N_CPU jobs
		pid = os.fork()
		# if parent
		if pid != 0:
			pid_index[pid] = i+1
			pid_index_CPU[pid] = i+1
			print 'Job %d PID[%s] of %d started 0 finished'\
				 %(pid_index[pid],pid,nPoly)
		# if child
		else:
			os.system(cmd)
			os._exit(0)

	nstarted = N_CPU
	# wait for processes to complete to fork next jobs
	while( nfinished < nPoly ):
		# check for returned process
		(pid, status) = os.waitpid( 0, os.WNOHANG )
	#	time.sleep(1)

		# if process has completed, fork next job
		if pid:
			#print 'Value of pid ' + str(pid)
			#print 'Which CPU has completed: ' + str(pid_index_CPU[pid])

			#cmd = 'mv mesh*.gmv gmv/'
			#os.system(cmd)

			name = str(nstarted+1).zfill(digits)
			  
			pid_tmp = pid
			nfinished += 1 

			cmd = 'rm polys_CPU' + \
				str(pid_index_CPU[pid]) + '.inp'
			os.system(cmd)

			cmd = 'rm intersections_CPU'\
				 + str(pid_index_CPU[pid]) + '.inp'
			os.system(cmd)

			cmd = 'rm parameters_CPU' \
				+ str(pid_index_CPU[pid]) + '.mlgi'
			os.system(cmd)

			cmd = 'ln -s polys/poly_' + name + '.inp '\
			 + 'polys_CPU' + str(pid_index_CPU[pid]) + '.inp'
			os.system(cmd)

			cmd = 'ln -s parameters/parameters_' + name \
				 + '.mlgi ' + 'parameters_CPU' \
				 + str(pid_index_CPU[pid]) + '.mlgi'
			os.system(cmd)

			cmd = 'ln -s intersections/intersections_' + name \
				 + '.inp ' + 'intersections_CPU' \
				+  str(pid_index_CPU[pid]) + '.inp'
			os.system(cmd)
			  
			cmd = lagirt_path + ' < mesh_poly_CPU' + \
				 str(pid_index_CPU[pid]) + '.lgi' + \
				 ' > lagrit_logs/log_lagrit' + name 

			if( nstarted < nPoly ):
				pid = os.fork()
				nstarted = nstarted + 1
				if pid != 0:
					print 'Job %d PID[%s] of %d started %d finished PID[%s]' %(nstarted,pid,nPoly,pid_index[pid_tmp],pid_tmp)	
					pid_index[pid] = nstarted
					pid_index_CPU[pid] = pid_index_CPU[pid_tmp]

				else:
					#print cmd
					os.system(cmd)
					os._exit(0)
			else:
				print 'Job %d PID[xxxxx] of %d started %d finished PID[%s]' %(nstarted,nPoly,pid_index[pid],pid)

	print 'Main process exiting.'
	print 'Triangulating Polygons: Complete'


def clean_up_directory(production_mode, NCPU):

	if (production_mode == 0):
		os.system("rm -f tmp_quad*.inp  ")
	for i in range(1, N_CPU+1):
		os.system("rm -f intersections_CPU" + str(i) + ".inp  ")
		os.system("rm -f parameters_CPU" + str(i) + ".mlgi  ")
		os.system("rm -f polys_CPU" + str(i) + ".inp  ")
		os.system("rm -f mesh_poly_CPU" + str(i) + ".lgi  ")


def create_merge_poly_files(N_CPU, nPoly, digits):
	'''
	Section 4 : Create merge_poly file
	 Creates a lagrit script that reads in each mesh, appends it to the main mesh, and then deletes that mesh object
	 Then duplicate points are removed from the main mesh using EPS_FILTER 
	 The points are compressed, and then written in the files full_mesh.gmv, full_mesh.inp, and an FEHM dump is preformed.
	'''
	
	print "Writing : merge_poly.lgi"

	part_size = nPoly/N_CPU + 1 ###v number of fractures in each part
	endis = range(part_size, nPoly + part_size, part_size) 
	endis[-1] = nPoly
	

	lagrit_input = '''
read / %s / mo_%d 
define / MO_NAME_FRAC / mo_%d
'''
	if (visualMode == 0):
		lagrit_input += '''
cmo / addatt / MO_NAME_FRAC / volume / evol_onen
math / sum / MO_NAME_FRAC / evol_sum / 1 0 0 / MO_NAME_FRAC / evol_one 
''' 
	lagrit_input += '''
addmesh / merge / cmo_tmp / cmo_tmp / mo_%d
cmo / delete / mo_%d
'''
	lagrit_input_2 = '#Writing out merged fractures\n' 
	if (visualMode == 0):
		lagrit_input_2 += '''
mo / addatt/ cmo_tmp / volume / evol_all
math / sum / cmo_tmp / evol_sum / 1 0 0 / cmo_tmp / evol_all '''
	lagrit_input_2 += ''' 
cmo select cmo_tmp
dump lagrit part%d.lg cmo_tmp
finish \n 
'''

	j = 0 # Counter for cpus 
	fout = 'merge_poly_part_1.lgi'
	f = open(fout, 'w')
	for i in range(1, nPoly + 1):
		if(visualMode == 0): 
			tmp = 'mesh_' + str(i).zfill(digits) + '.inp'
		else:
			tmp = 'mesh_reduced_' +  str(i).zfill(digits)+ '.inp'

		f.write(lagrit_input%(tmp,i,i,i,i))

		# if i is the last fracture in the cpu set
		# move to the next cpu set	
		if i == endis[j]:
			f.write(lagrit_input_2%(j+1))
			f.flush()
			f.close()
			
			j += 1
			fout = 'merge_poly_part_'+str(j+1)+'.lgi'
			f = open(fout,'w') 

	f.flush() 
	f.close() 
	os.system('rm ' + fout) ###

	lagrit_input  = '''
read / lagrit / part%d.lg / junk / binary
addmesh / merge / mo_all / mo_all / cmo_tmp 
cmo / delete / cmo_tmp 

	'''
	f = open('merge_rmpts.lgi','w')
	for j in range(1,len(endis)+1):
		f.write(lagrit_input%(j))

	# Append meshes complete
	lagrit_input = ''' 
# Appending the meshes complete 
# LaGriT Code to remove duplicates and output the mesh
cmo / select / mo_all 
#recon 1
define / EPS / 1.e-6
define / EPS_FILTER / 1.e-4 
pset / pinter / attribute / dfield / 1,0,0 / lt / EPS 
filter / pset get pinter / EPS_FILTER 
rmpoint / compress 
# SORT can affect a_b attribute
sort / mo_all / index / ascending / ikey / imt xic yic zic 
reorder / mo_all / ikey 
cmo / DELATT / mo_all / ikey
'''
	 
	if(visualMode == 0): 
		lagrit_input += '''
resetpts / itp 
boundary_components 
dump / full_mesh.gmv / mo_all
dump / full_mesh.inp / mo_all
dump / stor / tri_fracture / mo_all / ascii
'''
	# in case of FEHM run dump all FEHM files
	#	f.write('dump / fehm / tri_fracture / mo_all \n')
	# in case of PFLOTRAN run dump stor file only
	else:
		lagrit_input += '''
cmo / modatt / mo_all / icr1 / ioflag / l
cmo / modatt / mo_all / isn1 / ioflag / l
cmo / modatt / mo_all / itp1 / ioflag / l
'''
#		if (nPoly >= 100):
#			lagrit_input += '''
#math / modulo / mo_all / itetclr / 1,0,0 / mo_all / itetclr / 100 
#math / add / mo_all / itetclr / 1,0,0 / mo_all / itetclr / 1
#math / modulo / mo_all / imt1 / 1,0,0 / mo_all / imt1 / 100
#math / add / mo_all / imt1 / 1,0,0 / mo_all / imt1 / 1
#'''
		lagrit_input += '''
dump / reduced_full_mesh.gmv / mo_all 
dump / reduced_full_mesh.inp / mo_all
'''
	lagrit_input += '''
quality 
finish
'''
	f.write(lagrit_input)
	f.flush()
	f.close()

	return len(endis)


def merge_the_meshes(nPoly, N_CPU, lagrit_merge, n_jobs):
	''' Section 6 : Merge the Meshes
	 Merges all the meshes together, deletes duplicate points, 
		dumps the .gmv and fehm files
	'''
	print "\nMerging triangulated polygon meshes"
	os.system("rm -f log_merge_poly_output")

	for j in range(1, n_jobs + 1):
		pid = os.fork()
		if pid == 0: # clone a child job
			cmd = lagrit_merge +' < merge_poly_part_%d.lgi > log_merge_poly_part%d' 
			os.system(cmd%(j,j))
			os._exit(0)
		else:
			print 'Merging part ', j, ' of ', n_jobs 

	# wait for all child processes to complete
	j = 0
	while j < n_jobs:
		(pid, status) = os.waitpid(0,os.WNOHANG)
		if pid > 0:
			print 'Process ' + str(j+1) + ' finished'
			j += 1 

	os.system(lagrit_merge+' < merge_rmpts.lgi > log_merge_all') # run remove points

	print "Merging triangulated polygon meshes: Complete\n"

def create_cleanup_file():
	#########################################
	#Section 5 : Create Clean up file
	# Creates a python script to remove the files mesh_polyi.lgi, parametersi.mlgi, merge_poly.lgi, and run_lagrit.py
	# The individual meshes, meshi.gmv and meshi.inp are NOT removed. 
	python_input = '''
import os 
# Remove merge_poly.lgi
os.system("rm -f part* log_merge* merge*") 
# Remove folders containing intersections, parameters, polys, lagrit logs files.
os.system("rm -r intersections/ parameters/ polys/ lagrit_logs/") 
# Remove lagrit log outputs
os.system("rm -f logx3dgen outx3dgen domainattr.lgi printxyz.out bound_zones.lgi mater_zones.lgi ") 
os.system("rm -f boundary_top.zone boundary_bottom.zone boundary_left_w.zone boundary_front_s.zone boundary_right_e.zone   boundary_back_n.zone") 
# Move individual gmv, avs into folder 'meshes'
os.system("mkdir meshes") 
os.system("mkdir polymeshes") 
os.system("mv mesh*.inp ./polymeshes") 
os.system("mv mesh*.gmv ./meshes") 
os.system("rm -r -f meshes/ polymeshes/")

	'''
	f = open('cleanup.py', 'w')
	f.write(python_input)
	f.close()


def redefine_zones():
	'''Section 8 : redefine zones 
	Creates lagrit script to define domain size
'''
	lagrit_input = '''
read / gmv / full_mesh.gmv / mo 
cmo / printatt / mo/ -xyz- / minmax 
finish

	'''
	f=open('domainattr.lgi','w')
	f.write(lagrit_input) 
	f.flush()
	f.close()
	os.system(lagirt_path + " < domainattr.lgi > printxyz.out")
	# python script to read data from lagrit output file 
	fil = open('printxyz.out','r')
	for line in fil:
		k=line.split()
		for word in line.split():
			if word == 'xic':
				x_min = float(k[1])
				x_max = float(k[2])
			if word=='yic':
				y_min = float(k[1])
				y_max = float(k[2])
			if word=='zic':
				z_min = float(k[1])
				z_max = float(k[2])

	fil.close()
	#lagrit scripts to create new zone files: boundary zones

	eps = h*0.0001
	
	parameters = (x_max - eps, x_min + eps, y_max - eps, \
			 y_min + eps, z_max - eps, z_min + eps)
	lagrit_input = '''
read / gmv / full_mesh.gmv / mo
define / XMAX / %e 
define / XMIN / %e 
define / YMAX / %e 
define / YMIN / %e 
define / ZMAX / %e 
define / ZMIN / %e 

pset / top/ attribute / zic / 1,0,0/ gt /ZMAX 
pset / bottom/ attribute/ zic/ 1,0,0/ lt/ZMIN 
pset / left_w / attribute/ xic/ 1,0,0 /lt / XMIN
pset / front_s / attribute/ yic / 1,0,0 / gt/YMAX
pset / right_e / attribute/ xic/1,0,0/ gt/XMAX
pset / back_n / attribute/ yic/ 1,0,0 / lt/YMIN
pset/-all-/ zone / boundary / ascii
finish

'''
	f=open('bound_zones.lgi','w')
	f.write(lagrit_input%parameters)
	f.flush()
	f.close()
	os.system(lagirt_path + " < bound_zones.lgi > boundary_output.txt ")
	os.system("cp boundary_bottom.zone pboundary_bottom.zone")
	os.system("cp boundary_left_w.zone pboundary_left_w.zone")
	os.system("cp boundary_front_s.zone pboundary_front_s.zone")
	os.system("cp boundary_right_e.zone pboundary_right_e.zone")
	os.system("cp boundary_back_n.zone pboundary_back_n.zone")
	os.system("cp boundary_top.zone pboundary_top.zone")
	for i in range(0,2):
		os.system("sed -i '$d' boundary_top.zone ")
		os.system("sed -i '$d' boundary_bottom.zone ")
		os.system("sed -i '$d' boundary_left_w.zone ")
		os.system("sed -i '$d' boundary_front_s.zone ")
		os.system("sed -i '$d' boundary_right_e.zone ")

	os.system("sed -i '1d' boundary_bottom.zone ")
	os.system("sed -i '1d' boundary_left_w.zone ")
	os.system("sed -i '1d' boundary_front_s.zone ")
	os.system("sed -i '1d' boundary_right_e.zone ")
	os.system("sed -i '1d' boundary_back_n.zone ")
	os.system("cat boundary_top.zone boundary_bottom.zone boundary_left_w.zone boundary_front_s.zone boundary_right_e.zone   boundary_back_n.zone  > allboundaries.zone ")



################### MAIN ###############
print ('='*80)
os.system("date")
print '''Python Script to parse Mathematica output and mesh it using LaGriT 

Last Update March 11 2015 by Jeffrey Hyman
EES - 16, LANL
jhyman@lanl.gov
'''

refine_factor = 1
N_CPU = 1

#Production mode "ON" outputs the final results for computation, 
#cleaning up all the temporary attributes needed during refinement.
#Note that the visualization mode must be "OFF" in order to run
#in produciton mode. "dfield" can also be turn ON/OFF. 
#*1: "ON", *0: "OFF". 
#dfield = 0

production_mode = 1

lagirt_path = os.environ['lagrit_dfn']
#lagrit_merge = '/n/swdev/LAGRIT/bin/lagrit_lin'
lagrit_merge = os.environ['lagrit_dfn']
python_path = os.environ['python_dfn']

#Open the file of parameters output from mathematica
if (len(sys.argv) == 1):
	filename = 'params.txt'
	print "Number of CPU's to use (default):", N_CPU
	print "Reading in file (default):", filename 

elif (len(sys.argv) == 2):
	filename = sys.argv[1] 
	print "Reading in file:", filename 
	print "Number of CPU's to use (default):", N_CPU
	
elif (len(sys.argv) == 3):
	filename = sys.argv[1] 
	N_CPU = int(sys.argv[2])
	print "Reading in file:", filename 
	print "Number of CPU's to use:", N_CPU
	
elif (len(sys.argv) == 4):
	filename = sys.argv[1] 
	N_CPU = int(sys.argv[2])
	refine_factor = int(sys.argv[3])
	print "Reading in file:", filename
	print "Number of CPU's to use:", N_CPU
	print "Mesh Refine Factor:", refine_factor

nPoly, digits, h, numPoints, slope, refine_dist, visualMode, poly_file, intersection_file = parse_params_file(filename, refine_factor)

split_intersection_file(intersection_file, digits)

split_poly_file(poly_file, nPoly, digits)

create_parameter_mlgi_file(filename, nPoly)

create_lagrit_scripts(production_mode, N_CPU, refine_factor)

triangulate_polygons(nPoly, N_CPU)

clean_up_directory(production_mode, N_CPU)

n_jobs = create_merge_poly_files(N_CPU, nPoly, digits)

merge_the_meshes(nPoly, N_CPU, lagrit_merge, n_jobs)

if(visualMode == 0): 
	redefine_zones()

#os.system('ls -ltrh')	
if production_mode > 0:
	create_cleanup_file()
	cmd = python_path + ' cleanup.py'
	#os.system("python2.7 cleanup.py")
	os.system(cmd)
f=open('finalmesh.txt','w')
f.write('The final mesh of DFN consists of: \n')
#ALL DONE!
if(visualMode == 0): 
	print "Output files for FEHM calculations are written in :"
	print "   full_mesh.gmv"
	print "   full_mesh.inp"
	print "   tri_fracture.stor"
	finp=open('full_mesh.inp','r')
	fstor=open('tri_fracture.stor','r')
	g = finp.readline()
	g = g.split()
	NumElems = int(g.pop(1))
	NumIntNodes = int(g.pop(0))
	f.write(str(NumElems)+' triangular elements; \n')
	f.write(str(NumIntNodes)+'  nodes / control volume cells; \n')
	finp.close()
	fstor.readline()
	fstor.readline()
	gs = fstor.readline()
	gs = gs.split()
	NumCoeff = int(gs.pop(0))
	f.write(str(NumCoeff)+' geometrical coefficients / control volume faces. \n')
	fstor.close()
else:
	print "Output files for visualization are written in :"
	print "   reduced_full_mesh.gmv"
	print "   reduced_full_mesh.inp"
	finp=open('reduced_full_mesh.inp','r')
	g = finp.readline()
	g = g.split()
	NumElems = int(g.pop(1))
	NumIntNodes = int(g.pop(0))
	f.write(str(NumElems)+' triangular elements; \n')
	f.write(str(NumIntNodes)+'  nodes / control volume cells. \n')
	finp.close()
	
f.close()
os.system("date")
print ('='*80)


