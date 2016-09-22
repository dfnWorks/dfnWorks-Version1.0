#!/usr/bin/env python
#--------------------------------------------------------------------------
# Code to generate vtk files for postprocessing
# Satish Karra, LANL
# Dec. 1, 2015
#--------------------------------------------------------------------------

import os,sys

try:
    dfnworks_python_dir = os.environ['dfnworks_python']
except KeyError:
    print('dfnworks_python must point to the dfnworks-python installation directory and be defined in system environmental variables.')
    sys.exit(1)

sys.path.append(dfnworks_python_dir)


from dfnworks import *

dfn = dfnworks(inp_file='full_mesh.inp')
dfn.parse_pflotran_vtk()
