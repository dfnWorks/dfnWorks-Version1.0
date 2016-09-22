# the scripts rewrites aperture.dat file and perm.dat file from one format to
# format that is used by FEHM (including zone number)

import sys
import time
import os
import numpy as np

start_time = time.time()
if len(sys.argv) < 2: 
    sys.exit('ERROR: Usage - python perm_aper.py [perm_filename][aper_filename]') 
perm_file = sys.argv[1]
aper_file = sys.argv[2]

permn_file='perm1.dat'
apern_file='aper1.dat'

cmd = 'mv '+ str(perm_file) +'  '+str(permn_file)
failure = os.system(cmd)
if failure:
    print 'Unable to move perm file'; sys.exit(1)

cmd= 'mv '+str(aper_file)+'  '+str(apern_file)    
failure = os.system(cmd)
if failure:
    print 'Unable to move aperture file'; sys.exit(1)
print('--> Finished renaming initial files')

fap = open('aperture.dat','w')
fp = open('perm.dat','w')
fap.write('aperure \n')
fp.write('permeability \n')

aperture=np.genfromtxt(apern_file, skip_header=1)

	
with open(permn_file,'r') as frp:
         firstline=frp.readline() 
         perm=[map (float, line.split()) for line in frp]

nmater=len(aperture) 
for i in range(0, int(nmater)):
	fap.write(str(int(-1.*(i+7)))+'  0   0   '+str(aperture[i])+'\n')
	fp.write(str(int(-1.*(i+7)))+'  0   0   '+str(perm[i][0])+'  '+str(perm[i][0])+'   '+str(perm[i][0])+'\n')
	


fap.close()
fp.close() 
print 'Done'
