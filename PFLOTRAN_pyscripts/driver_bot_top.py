#!/usr/bin/env python

import os,sys

try:
    dfnworks_python_dir = os.environ['dfnworks_python']
except KeyError:
    print('dfnworks_python must point to the dfnworks-python installation directory and be defined in system environmental variables.')
    sys.exit(1)

sys.path.append(dfnworks_python_dir)

from dfnworks import *

dfn = dfnworks(inp_file='full_mesh.inp',perm_file='perm.dat',aper_file='aperture.dat')
dfn.lagrit2pflotran()
#dfn.zone2ex(zone_file='pboundary_back_n.zone',face='north')
#dfn.zone2ex(zone_file='pboundary_front_s.zone',face='south')
#dfn.zone2ex(zone_file='pboundary_left_w.zone',face='west')
#dfn.zone2ex(zone_file='pboundary_right_e.zone',face='east')
dfn.zone2ex(zone_file='pboundary_top.zone',face='top')
dfn.zone2ex(zone_file='pboundary_bottom.zone',face='bottom')
dfn.parse_pflotran_vtk()
