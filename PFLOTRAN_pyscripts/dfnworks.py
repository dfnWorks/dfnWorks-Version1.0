""" Class for dfnworks data """

__author__ = "Satish Karra"
__version__ = "1.0"
__maintainer__ = "Satish Karra"
__email__ = "satkarra@lanl.gov"

try:
    pflotran_dir = os.environ['PFLOTRAN_DIR']
except:
    pflotran_dir = ''

from matplotlib import rc
from matplotlib import pyplot as plt
import pyvtk as pv
from mpl_toolkits.mplot3d import axes3d
from scipy import spatial as spsp
import numpy as np
import os, sys

from dfntools import *

rc('text', usetex=True)

try:
    lagrit_path = os.environ['lagrit_dfn']
except KeyError:
    sys.exit('LAGRIT_DFN must point to the LaGriT DFN path.')


mesh_types_allowed = ['dfn','volume','mixed'] # mixed is dfn + volume mesh

class dfnworks(Frozen):
    """
    Class for dfnworks data manipulation
    """

    def __init__(self, inp_file='', uge_file='', vtk_file='', mesh_type='dfn', perm_file='', aper_file='',perm_cell_file='',aper_cell_file=''):
        """

        :type mesh_file: *inp filename
        """
        self._vtk_file = vtk_file
        self._inp_file = inp_file
        self._uge_file = uge_file
        self._mesh_type = mesh_type
        self._perm_file = perm_file
        self._aper_file = aper_file
        self._perm_cell_file = perm_cell_file
        self._aper_cell_file = aper_cell_file
        self._freeze()

    def inp2vtk(self, inp_file=''):

        """

        :rtype : object
        """
        if self._inp_file:
            inp_file = self._inp_file
        else:
            self._inp_file = inp_file

        if inp_file == '':
            sys.exit('ERROR: Please provide inp filename!')

        if self._vtk_file:
            vtk_file = self._vtk_file
        else:
            vtk_file = inp_file[:-4]

        self._vtk_file = vtk_file + '.vtk'

        print("--> Reading inp data")

        with open(inp_file, 'r') as f:
            line = f.readline()
            num_nodes = int(line.strip(' ').split()[0])
            num_elems = int(line.strip(' ').split()[1])

            coord = np.zeros((num_nodes, 3), 'float')
            elem_list_tri = []
            elem_list_tetra = []

            for i in range(num_nodes):
                line = f.readline()
                coord[i, 0] = float(line.strip(' ').split()[1])
                coord[i, 1] = float(line.strip(' ').split()[2])
                coord[i, 2] = float(line.strip(' ').split()[3])

            for i in range(num_elems):
                line = f.readline().strip(' ').split()
                line.pop(0)
                line.pop(0)
                elem_type = line.pop(0)
                if elem_type == 'tri':
                    elem_list_tri.append([int(i) - 1 for i in line])
                if elem_type == 'tet':
                    elem_list_tetra.append([int(i) - 1 for i in line])

        print('--> Writing inp data to vtk format')

        vtk = pv.VtkData(pv.UnstructuredGrid(coord, tetra=elem_list_tetra, triangle=elem_list_tri),
                         'Unstructured pflotran grid')
        vtk.tofile(vtk_file)

    def zone2ex(self, uge_file='', zone_file='', face='', ):

        if self._uge_file:
            uge_file = self._uge_file
        else:
            self._uge_file = uge_file

        uge_file = self._uge_file

        if uge_file == '':
            sys.exit('ERROR: Please provide uge filename!')
        if zone_file == '':
            sys.exit('ERROR: Please provide boundary zone filename!')
        if face == '':
            sys.exit('ERROR: Please provide face name among: top, bottom, north, south, east, west !')

        # Ex filename
        ex_file = zone_file.strip('zone') + 'ex'

        # Opening the input file
        print('--> Opening zone file')
        f = open(zone_file, 'r')
        f.readline()
        f.readline()
        f.readline()

        # Read number of boundary nodes
        print('--> Calculating number of nodes')
        NumNodes = int(f.readline())

        Node_array = np.zeros(NumNodes, 'int')

        # Read the boundary node ids
        print('--> Reading boundary node ids')

        if (NumNodes < 10):
            g = f.readline()
            node_array = g.split()
            # Convert string to integer array
            node_array = [int(id) for id in node_array]
            Node_array = np.asarray(node_array)
        else:
            for i in range(NumNodes / 10 + 1):
                g = f.readline()
                node_array = g.split()
                # Convert string to integer array
                node_array = [int(id) for id in node_array]
                if (NumNodes - 10 * i < 10):
                    for j in range(NumNodes % 10):
                        Node_array[i * 10 + j] = node_array[j]
                else:
                    for j in range(10):
                        Node_array[i * 10 + j] = node_array[j]

        f.close()

        print('--> Finished with zone file')

        # Opening uge file
        print('--> Opening uge file')
        f = open(uge_file, 'r')

        # Reading cell ids, cells centers and cell volumes
        g = f.readline()
        g = g.split()
        NumCells = int(g[1])

        Cell_id = np.zeros(NumCells, 'int')
        Cell_coord = np.zeros((NumCells, 3), 'float')
        Cell_vol = np.zeros(NumCells, 'float')
        Boundary_cell_area = np.zeros(NumNodes, 'float')
        for cells in range(NumCells):
            g = f.readline()
            g = g.split()
            Cell_id[cells] = int(g.pop(0))
            g = [float(id) for id in g]
            Cell_vol[cells] = g.pop(3)
            Cell_coord[cells] = g

        f.close()

        print('--> Finished with uge file')

        for i in range(NumNodes):
            Boundary_cell_area[i] = 1.e20  # Fix the area to a large number

        print('--> Finished calculating boundary connections')

        boundary_cell_coord = [Cell_coord[Cell_id[i - 1] - 1] for i in Node_array]
        epsilon = 1e-0  # Make distance really small
        if (face == 'top'):
            boundary_cell_coord = [[cell[0], cell[1], cell[2] + epsilon] for cell in boundary_cell_coord]
        elif (face == 'bottom'):
            boundary_cell_coord = [[cell[0], cell[1], cell[2] - epsilon] for cell in boundary_cell_coord]
        elif (face == 'north'):
            boundary_cell_coord = [[cell[0], cell[1] + epsilon, cell[2]] for cell in boundary_cell_coord]
        elif (face == 'south'):
            boundary_cell_coord = [[cell[0], cell[1] - epsilon, cell[2]] for cell in boundary_cell_coord]
        elif (face == 'east'):
            boundary_cell_coord = [[cell[0] + epsilon, cell[1], cell[2]] for cell in boundary_cell_coord]
        elif (face == 'west'):
            boundary_cell_coord = [[cell[0] - epsilon, cell[1], cell[2]] for cell in boundary_cell_coord]
        else:
            sys.exit('ERROR: unknown face. Select one of: top, bottom, east, west, north, south.')

        with open(ex_file, 'w') as f:
            f.write('CONNECTIONS\t%i\n' % Node_array.size)
            for idx, cell in enumerate(boundary_cell_coord):
                f.write('%i\t%.6e\t%.6e\t%.6e\t%.6e\n' % (
                    Node_array[idx], cell[0], cell[1], cell[2], Boundary_cell_area[idx]))

        print('--> Finished writing ex file "' + ex_file + '" corresponding to the zone file: ' + zone_file)


    def extract_common_nodes(self, volume_mesh_uge_file='', dfn_mesh_uge_file='', common_table_file='',
                             combined_uge_file='combined.uge'):

        print('--> Extracting nodes common to the volume and dfn meshes')

        table_file = common_table_file
        dat = np.genfromtxt(table_file, skip_header=7)
        common_dat = [[arr[0], arr[5]] for arr in dat if arr[1] == 21]

        file = dfn_mesh_uge_file
        f = open(file, 'r')
        num_cells = int(f.readline().strip('').split()[1])
        cell_count = num_cells

        cell_list = []
        for i in range(num_cells):
            line = f.readline().strip('').split()
            cell_list.append(line)

        conn_list = []
        num_conns = int(f.readline().strip('').split()[1])
        for i in range(num_conns):
            line = f.readline().strip('').split()
            conn_list.append(line)

        f.close()

        file = volume_mesh_uge_file
        f = open(file, 'r')
        num_cells = int(f.readline().strip('').split()[1])

        for i in range(num_cells):
            line = f.readline().strip('').split()
            line[0] = str(int(line[0]) + cell_count)
            cell_list.append(line)

        num_conns = int(f.readline().strip('').split()[1])
        for i in range(num_conns):
            line = f.readline().strip('').split()
            line[0] = str(int(line[0]) + cell_count)
            line[1] = str(int(line[1]) + cell_count)
            conn_list.append(line)

        f.close()

        epsilon = 1.e-3
        area = 1.e9

        for dat in common_dat:
            conn_list.append([cell_list[int(dat[0]) - 1][0], cell_list[int(dat[1]) - 1][0],
                              str(float(cell_list[int(dat[0]) - 1][1]) + epsilon),
                              str(float(cell_list[int(dat[0]) - 1][2]) + epsilon),
                              str(float(cell_list[int(dat[0]) - 1][3]) + epsilon), str(area)])

        for dat in common_dat:
            cell_list[int(dat[1]) - 1][1] = str(float(cell_list[int(dat[1]) - 1][1]) + epsilon)
            cell_list[int(dat[1]) - 1][2] = str(float(cell_list[int(dat[1]) - 1][2]) + epsilon)
            cell_list[int(dat[1]) - 1][3] = str(float(cell_list[int(dat[1]) - 1][3]) + epsilon)

        with open(combined_uge_file, 'w') as f:
            f.write('CELLS\t%i\n' % len(cell_list))
            for cell in cell_list:
                f.write('%i\t%.6e\t%.6e\t%.6e\t%.6e\n' % (
                    int(cell[0]), float(cell[1]), float(cell[2]), float(cell[3]), float(cell[4])))
            f.write('CONNECTIONS\t%i\n' % len(conn_list))
            for conn in conn_list:
                f.write('%i\t%i\t%.6e\t%.6e\t%.6e\t%.6e\n' % (
                    int(conn[0]), int(conn[1]), float(conn[2]), float(conn[3]), float(conn[4]), float(conn[5])))

    def parse_pflotran_vtk(self, grid_vtk_file=''):

        import glob

        if grid_vtk_file:
            self._vtk_file = grid_vtk_file
        else:
            self.inp2vtk()

        grid_file = self._vtk_file
        files = glob.glob('*-[0-9][0-9][0-9].vtk')
        with open(grid_file, 'r') as f:
            grid = f.readlines()[3:]

        out_dir = 'parsed_vtk'
        for line in grid:
            if 'POINTS' in line:
                num_cells = line.strip(' ').split()[1]

        for file in files:
            with open(file, 'r') as f:
                pflotran_out = f.readlines()[4:]
            pflotran_out = [w.replace('CELL_DATA', 'POINT_DATA') for w in pflotran_out]
            header = ['# vtk DataFile Version 2.0\n',
                      'PFLOTRAN output\n',
                      'ASCII\n']
            filename = out_dir + '/' + file
            if not os.path.exists(os.path.dirname(filename)):
                os.makedirs(os.path.dirname(filename))
            with open(filename, 'w') as f:
                for line in header:
                    f.write(line)
                for line in grid:
                    f.write(line)
                f.write('\n')
                f.write('\n')
                if 'vel' in file:
                    f.write('POINT_DATA\t' + num_cells + '\n')
                for line in pflotran_out:
                    f.write(line)

    def lagrit2pflotran(self, inp_file='', mesh_type='', hex2tet=False):

        print('--> Writing pflotran uge file from lagrit')

        if inp_file:
            self._inp_file = inp_file
        else:
            inp_file = self._inp_file

        if inp_file == '':
            sys.exit('ERROR: Please provide inp filename!')

        if mesh_type:
            if mesh_type in mesh_types_allowed:
                self._mesh_type = mesh_type
            else:
                sys.exit('ERROR: Unknown mesh type. Select one of dfn, volume or mixed!')
        else:
            mesh_type = self._mesh_type

        if mesh_type == '':
            sys.exit('ERROR: Please provide mesh type!')

        d = inp_file[:-4]
        d1 = d + '_tet.inp'
        fid = open('%s.lgi' % d, 'w')  # Open file
        fid.write('read / avs / ' + inp_file + '/ mo1\n')
        if hex2tet:
            fid.write('create / cmo / cmo_tet\n')
            fid.write('grid2grid / hextotet6  /  cmo_tet / mo1\n')
            fid.write('dump / avs / ' + '%s' % d1 + ' / cmo_tet \n')  # write avs file
            fid.write('dump / pflotran / ' + '%s' % d + ' / cmo_tet / nofilter_zero \n')  # write uge file
        else:
            fid.write('dump / pflotran / ' + '%s' % d + ' / mo1 / nofilter_zero \n')  # write uge file
        fid.write('finish\n\n')
        fid.close()

        self._uge_file = d + '.uge'

        cmd = lagrit_path + '<%s.lgi' % d + '>lagrit_uge.txt'
        failure = os.system(cmd)
        if failure:
            sys.exit('Failed to run LaGrit to get initial .uge file')

        if mesh_type == 'dfn':
            self.write_perms_and_correct_volumes_areas() # Make sure perm and aper files are specified


    def inp2gmv(self, inp_file=''):

        if inp_file:
            self._inp_file = inp_file
        else:
            inp_file = self._inp_file

        if inp_file == '':
            sys.exit('ERROR: inp file must be specified in inp2gmv!')

        gmv_file = inp_file[:-4] + '.gmv'

        with open('inp2gmv.lgi', 'w') as fid:
            fid.write('read / avs / ' + inp_file + ' / mo\n')
            fid.write('dump / gmv / ' + gmv_file + ' / mo\n')
            fid.write('finish \n\n')

        cmd = lagrit_path + ' <inp2gmv.lgi ' + '>lagrit_inp2gmv.txt'
        failure = os.system(cmd)
        if failure:
            sys.exit('ERROR: Failed to run LaGrit to get gmv from inp file!')

        print('--> Finished writing gmv format from avs format')


    def write_perms_and_correct_volumes_areas(self, inp_file='', uge_file='', perm_file='', aper_file=''):

        import h5py

        if inp_file:
            self._inp_file = inp_file
        else:
            inp_file = self._inp_file

        if inp_file == '':
            sys.exit('ERROR: inp file must be specified!')

        if uge_file:
            self._uge_file = uge_file
        else:
            uge_file = self._uge_file

        if uge_file == '':
            sys.exit('ERROR: uge file must be specified!')

        if perm_file:
            self._perm_file = perm_file
        else:
            perm_file = self._perm_file

        if perm_file == '' and self._perm_cell_file == '':
            sys.exit('ERROR: perm file must be specified!')

        if aper_file:
            self._aper_file = aper_file
        else:
            aper_file = self._aper_file

        if aper_file == '' and self._aper_cell_file == '':
            sys.exit('ERROR: aperture file must be specified!')

        # reading *.inp file
        print('--> Reading *.inp file')
        print('--> Calling Lagrit')

        fid = open('materialid.lgi', 'w')
        fid.write('read / avs / ' + inp_file + ' / mo\n')
        fid.write('cmo / modatt / mo / isn / ioflag / l\n')
        fid.write('cmo / modatt / mo / x_four / ioflag / l\n')
        fid.write('cmo / modatt / mo / fac_n / ioflag / l\n')
        fid.write('cmo / modatt / mo / dfield / ioflag / l\n')
        fid.write('cmo / modatt / mo / rf_field / ioflag / l\n')
        fid.write('cmo / modatt / mo / a_b / ioflag / l\n')
        fid.write('cmo / modatt / mo / b_a / ioflag / l\n')
        fid.write('cmo / modatt / mo / xnorm / ioflag / l\n')
        fid.write('cmo / modatt / mo / ynorm / ioflag / l\n')
        fid.write('cmo / modatt / mo / znorm / ioflag / l\n')
        fid.write('cmo / modatt / mo / evol_one / ioflag / l\n')
        fid.write('cmo / modatt / mo / evol_all / ioflag / l\n')
        fid.write('cmo / modatt / mo / numbnd / ioflag / l\n')
        fid.write('cmo / modatt / mo / id_numb / ioflag / l\n')
        fid.write('cmo / modatt / mo / evol_all / ioflag / l\n')
        fid.write('cmo / modatt / mo / itp / ioflag / l\n')
        fid.write('cmo / modatt / mo / icr / ioflag / l\n')
        fid.write('cmo / modatt / mo / meshid / ioflag / l\n')
        fid.write('cmo / modatt / mo / id_n_1 / ioflag / l\n')
        fid.write('cmo / modatt / mo / id_n_2 / ioflag / l\n')
        fid.write('cmo / modatt / mo / pt_gtg / ioflag / l\n')
        fid.write('dump / materialid.inp / mo\n')
        fid.write('finish\n\n')
        fid.close()

        cmd = lagrit_path + ' <materialid.lgi >lagrit_materialid.txt'
        failure = os.system(cmd)
        if failure:
            sys.exit('Failed to run LaGrit to get material IDs')

        inp_file = 'materialid.inp'

        f = open(inp_file, 'r')
        g = f.readline()
        g = g.split()
        NumElems = int(g.pop(1))
        NumIntNodes = int(g.pop(0))

        for i in range(NumIntNodes):
            f.readline()

        for j in range(NumElems):
            f.readline()

        f.readline()
        f.readline()
        materialid = []
        for j in range(NumIntNodes):
            g = f.readline()
            g = g.split()
            g = g.pop(1)
            materialid.append(g)
        materialid = np.array(materialid)

        for i in range(NumIntNodes):
            materialid[i] = int(float(materialid[i]))

        print('--> Finished reading material id')

        # Writing to values again to files
        fid = open('materialid.dat', 'w')
        for i in range(NumIntNodes):
            fid.write('%s' % (materialid[i]))
            fid.write('\n')
        fid.close()
        print('--> Finished writing material ids to file')

        if perm_file:
            filename = 'dfn_properties.h5'
            h5file = h5py.File(filename, mode='w')

            print('--> Beginning writing to HDF5 file')
            print('--> Allocating cell index array')
            iarray = np.zeros(NumIntNodes, '=i4')
            print('--> Writing cell indices')
            # add cell ids to file
            for i in range(NumIntNodes):
                iarray[i] = i + 1
            dataset_name = 'Cell Ids'
            h5dset = h5file.create_dataset(dataset_name, data=iarray)

            print ('--> Allocating permeability array')
            perm = np.zeros(NumIntNodes, '=f8')

            print('--> reading permeability data')
            print('--> Note: this script assumes isotropic permeability')
            f = open(perm_file, 'r')
            f.readline()
            perm_list = []
            while True:
                h = f.readline()
                h = h.split()
                if h == []:
                    break
                h.pop(5)
                h.pop(4)
                h.pop(1)
                h.pop(1)
                perm_list.append(h)

            perm_list = np.array(perm_list)

            for i in range(NumIntNodes):
                materialid[i] = -1 * int(materialid[i]) - 6

            for i in range(NumIntNodes):
                j = np.where(materialid[i] == perm_list[:, 0])[0]
                perm[i] = float(perm_list[j, 1][0])

            dataset_name = 'Permeability'
            h5dset = h5file.create_dataset(dataset_name, data=perm)
            f.close()

            h5file.close()
            print('--> Done writing permeability to h5 file')

        if self._perm_cell_file:
            filename = 'dfn_properties.h5'
            h5file = h5py.File(filename, mode='w')

            print('--> Beginning writing to HDF5 file')
            print('--> Allocating cell index array')
            iarray = np.zeros(NumIntNodes, '=i4')
            print('--> Writing cell indices')
            # add cell ids to file
            for i in range(NumIntNodes):
                iarray[i] = i + 1
            dataset_name = 'Cell Ids'
            h5dset = h5file.create_dataset(dataset_name, data=iarray)

            print ('--> Allocating permeability array')
            perm = np.zeros(NumIntNodes, '=f8')

            print('--> reading permeability data')
            print('--> Note: this script assumes isotropic permeability')
            f = open(self._perm_cell_file, 'r')
            f.readline()
            perm_list = []
            while True:
                h = f.readline()
                h = h.split()
                if h == []:
                    break
                h.pop(0)
                perm_list.append(h)

            perm_list = [float(perm[0]) for perm in perm_list]

            dataset_name = 'Permeability'
            h5dset = h5file.create_dataset(dataset_name, data=perm_list)
            f.close()

            h5file.close()
            print('--> Done writing permeability to h5 file')

        cmd = 'rm -f materialid.inp'
        failure = os.system(cmd)
        if failure:
            sys.exit('Failed to remove materialid.inp')

        print('--> reading aperture data')

        if aper_file:
            f = open(aper_file, 'r')
            f.readline()
            aper_list = []
            while True:
                h = f.readline()
                h = h.split()
                if h == []:
                    break
                h.pop(1)
                h.pop(1)
                aper_list.append(h)

        if self._aper_cell_file:
            f = open(self._aper_cell_file, 'r')
            f.readline()
            aper_list = []
            while True:
                h = f.readline()
                h = h.split()
                if h == []:
                    break
                h.pop(0)
                aper_list.append(h)

        aper_list = np.array(aper_list)

        # Opening uge file
        print('--> Creating a new uge file with correct volumes and areas')

        fid = open(uge_file[:-4] + '_vol_area.uge', 'w')

        with open(uge_file, 'r') as f:
            # Reading cell ids, cells centers and cell volumes
            g = f.readline()
            g = g.split()
            NumCells = int(g[1])

            Cell_id = np.zeros(NumCells, 'int')
            Cell_coord = np.zeros((NumCells, 3), 'float')
            Cell_vol = np.zeros(NumCells, 'float')
            for cells in range(NumCells):
                g = f.readline()
                g = g.split()
                Cell_id[cells] = int(g.pop(0))
                g = [float(id) for id in g]
                Cell_vol[cells] = g.pop(3)
                Cell_coord[cells] = g
                if aper_file:
                    j = np.where(materialid[cells] == aper_list[:, 0])[0]
                    j = j[0]
                    Cell_vol[cells] = Cell_vol[cells] * float(aper_list[j, 1])
                elif self._aper_cell_file:
                    Cell_vol[cells] = Cell_vol[cells] * float(aper_list[cells, :][0])
                else:
                    print('ERROR: aper_file or aper_cell_file needs to be specified!')

            # Writing to values again to files
            fid.write('CELLS\t%i\n' % NumCells)
            for i in range(NumCells):
                fid.write('%i\t%E\t%E\t%E\t%E\n' % (
                    Cell_id[i], Cell_coord[i, 0], Cell_coord[i, 1], Cell_coord[i, 2], Cell_vol[i]))

            line = f.readline()
            NumConns = int(line.strip('').split()[1])
            fid.write(line)
            for conns in range(NumConns):
                g = f.readline()
                g = g.strip('').split()
                if aper_file:
                    i = np.where(materialid[int(g[0]) - 1] == aper_list[:, 0])[0]
                    j = np.where(materialid[int(g[1]) - 1] == aper_list[:, 0])[0]
                    i = i[0]
                    j = j[0]
                    aperture = (float(aper_list[i, 1]) + float(aper_list[j, 1])) * 0.5
                elif self._aper_cell_file:
                    aperture = (float(aper_list[int(g[0]) - 1, :][0]) + float(aper_list[int(g[1]) - 1, :][0])) * 0.5
                else:
                    print('ERROR: aper_file or aper_cell_file needs to be specified!')

                g[5] = str(float(g[5]) * aperture)
                fid.write('%s\t%s\t%s\t%s\t%s\t%s\n' % (g[0], g[1], g[2], g[3], g[4], g[5]))

            lines = f.read()
            fid.write(lines)

        fid.close()

