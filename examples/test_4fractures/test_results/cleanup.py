
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

	