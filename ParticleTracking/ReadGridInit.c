#include <stdio.h>
#include <search.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "FuncDef.h"  
#include <unistd.h>

struct inpfile {
  char filename[120];
  long int flag;
  double param;
}; 

//////////////////////////////////////////////////////////////////////////////
void ReadInit()
/******* function reads total number of nodes, cells, fractures in domain *****/
/**** and allocates memory for data structures: NODE, CELL, FRACTURE **********/
{ 

  /************** opening file params.txt ***************************************/
  int i, nf;
  char cs;
  
  struct inpfile inputfile;
   
  inputfile=Control_File("param:", 6);

  printf("\n OPEN AND READ FILE: %s \n \n", inputfile.filename);

  FILE *fpp=OpenFile (inputfile.filename,"r");
  
  fscanf(fpp,"%d\n",&nfract);
  
  printf(" Number of fractures in the domain = %d \n", nfract);
 
  /********************** opening file an inp file ***************************/
  
  int nn,  j;
 
  inputfile=Control_File("inp:", 4);
  
  printf("\n OPEN AND READ AVS FILE: %s \n \n", inputfile.filename);
  
  FILE *fp = OpenFile (inputfile.filename,"r");
 
  fscanf(fp, "%d %d %d %d %d\n", &nnodes, &ncells, &nn, &nn, &nn ); 
 
  printf(" Total number of nodes: %d, Total number of elements (triangles): %d\n", nnodes, ncells);
 
  fclose(fp);

  /******************* open and read stor file *******************************/

  unsigned  int nedges;
  unsigned  int snode_edge;
  int area_coef;
  

  inputfile = Control_File("stor:",5 );

  printf("\n OPEN AND READ STOR FILE: %s\n \n", inputfile.filename); 

  FILE *fps = OpenFile (inputfile.filename,"r");
 
  /* Read the head of the file */
  for (i=0; i<2; i++)
    {
      do 
	cs=fgetc(fps); 
      while (cs!='\n');
   
    }
  int node1;
  fscanf (fps," %d %d %d %d %d \n", &nedges, &node1, &snode_edge, &area_coef, &max_neighb );
  printf (" Total number of edges in Voronoy polygons = %d, total number of nodes = %d \n", nedges, node1);
  if (node1!=nnodes)
    printf("the number of nodes in inp is not equal to number of nodes in stor file! \n");

  fclose(fps);
  /***** after reading the number of nodes "nnodes"
	 the number of fractures "nfract"
	 the number of cells "ncells"
         the memory is allocated for data structures ************************/             
 
  node=(struct vertex*) malloc (nnodes*sizeof(struct vertex));
     
  for (i=0; i<nnodes; i++)
    {
      node[i].indnodes=(unsigned int*) malloc(max_neighb*sizeof(unsigned int)); 
      node[i].type=(unsigned int*) malloc(max_neighb*sizeof(unsigned int)); 
      node[i].flux=(double*) malloc(max_neighb*sizeof(double)); 
      node[i].area=(double*) malloc(max_neighb*sizeof(double)); 
      node[i].cells=(unsigned  int**) malloc (max_neighb*sizeof(unsigned int*));
      node[i].fracts=(unsigned  int**) malloc (max_neighb*sizeof(unsigned  int*));
      for (j=0; j<max_neighb; j++)
	{
	  node[i].cells[j]=(unsigned  int*)malloc(4*sizeof(unsigned  int));
	  node[i].fracts[j]=(unsigned  int*)malloc(4*sizeof(unsigned  int));
	}
    }
 
  if (node==NULL)
    printf("Allocation memory problem - node\n");    
    
  /********** allocate memory for cell structure *****************************/

  cell=(struct element*) malloc (ncells*sizeof(struct element));

  if (cell==NULL)
    printf("Allocation memory problem - cell\n"); 

  /********** allocate memory for fracture structure *****************************/

  fracture=(struct material*)malloc (nfract*sizeof(struct material)); 

  if (fracture==NULL)
    printf("Allocation memory problem - fracture\n");       
 
  printf("\n Memory allocation is done successfully \n");
  /*****************reading the orientation angle and norm components 
                 of every fracture from params.txt file*************************/
  int nnf;
  for (i=0; i<7; i++)
    {
      do 
	cs=fgetc(fpp); 
      while (cs!='\n');
   
    }

  for (i=0; i<nfract; i++)
    {
      fscanf(fpp,"%d %f  %f %f %d %f %f %d %d\n",&nf, &fracture[i].theta, 
	     &fracture[i].nvect_xy[0], &fracture[i].nvect_xy[1], &nnf, 
	     &fracture[i].nvect_z[0], &fracture[i].nvect_z[1],&nnf, &nnf);
     
    }

  fclose(fpp);   

  return;
}
////////////////////////////////////////////////////////////////////////////////////////////



void  ReadDataFiles ()

{
  /******** function reads Grid data from inp and stor files ***************/
  /******** also reads the pressure values and fluxes from FEHM flow solution ****/

  
  
  struct inpfile inputfile;
 
  
  unsigned   int nedges;
  unsigned   int snode_edge;
  unsigned   int area_coef;
  unsigned   int n1, j, l, ln;
  unsigned   int nnv;
  unsigned   int ncv;
  char c[3],line[20], cs;
  unsigned long i;

  /* nedges - total number  of edges in Voronoy polygons in whole domain */
  /* snode_edge - number of edges + number of nodes +1 */
  /* area_coef - Can be (1,3, or 4) number of area coefficients. The area/distance coefficients are the area of a Voronoi face divided by the Delaunay edge length. This coefficient is either written as a scalar (1), a vector area (3), or both vector and scalar areas are written (4). */
  /* max_neighb - maximum number of neighboring nodes */                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
  /* nnv - number of nodes variables */  
  /* ncv - number of cells variables */    

  inputfile = Control_File("inp:",4 );
  
  FILE *fp = OpenFile (inputfile.filename,"r");
   
  printf("\n OPEN AND READ FILE: %s \n ",inputfile.filename);
 
  fscanf(fp, "%d %d %d %d %d\n", &n1, &n1, &nnv, &ncv, &n1 ); 

 
  /* read 3D coordinations: x in [0], y in [1], and z in [2] for every node i+1 */ 

  for (i=0; i<nnodes; i++)
    {
      fscanf(fp,"%d  %lf %lf %lf \n", &n1, &node[i].coord[0], &node[i].coord[1], &node[i].coord[2]);
    }

 // printf("\n Nodes 3D coordinations are read  \n"); 

  /* read a connectivity list: number of fracture and 3 node numbers for each cell (triangle) */
 
  unsigned  int current_fract=1, previous_fract=1;
  unsigned  int cell_f=0;

  fracture[0].firstcell=1;

  for (i=0; i<ncells; i++)
    {
      fscanf(fp, "%d  %d  %c %c %c %d %d %d \n", &n1, &cell[i].fracture, 
	     &c[0],&c[1],&c[2], &cell[i].node_ind[0], &cell[i].node_ind[1], &cell[i].node_ind[2]);
       
      current_fract=cell[i].fracture;
      if (current_fract!=previous_fract)
	{
	  fracture[current_fract-1].firstcell=i+1;
	  previous_fract=current_fract;
	  fracture[current_fract-2].numbcells=i+1-fracture[current_fract-2].firstcell;
	  cell_f=i;
	}        
    }
    
  fracture[nfract-1].numbcells=ncells-cell_f;
 

  /* read the next lines, which show node attributes and thier order in next data block */     
 
  fscanf(fp, "%d", &n1);

  int k=1,imt1_ind=0, itp1_ind=0, ni[nnv];
  char  var_name[20]; 

  for (i=0; i<nnv; i++)
    {
      fscanf(fp, "%d", &ni[i]);
      k=k*ni[i];
    }

  if (k!=1)
    printf(" Please check nodes attributes in inp file\n");

  /* read node's attribute's names and search for imt1 and itp1 */
  for (i=0; i<nnv; i++)
    {
      fscanf(fp, "%s %s \n", &var_name, &line);
      int res=strncmp(var_name,"imt1,",5);
      if (res==0) 
	imt1_ind=i;
            
      int res1=strncmp(var_name,"itp1,",5);
      if (res1==0) 
	itp1_ind=i;
    }
  /* read variables imt1 - node material (in our case it is fracture id) and itp1 - type of node */
  /* type of node: 0 - interior; 10- exterior; 2- interior interface; 12- exterior interface */

 
  double fn;
  int fr=0, currentfr=1;
  k=0;
  fracture[k].firstnode=1;

  for (i=0; i<nnodes; i++)   
    {
      node[i].fracture[0]=0;
      node[i].fracture[1]=0;
      for (j=0; j<itp1_ind+2; j++)
	{
	  fscanf(fp, "%lf ", &fn);
                
	  if (j==imt1_ind+1)
            {
	      fr=(int)fn;
	      if (currentfr != fr)
		{
		  fracture[k].lastnode=i;
		  k=k+1;
		  fracture[k].firstnode=i+1;
		  currentfr=fr;
		}
	      node[i].fracture[0]=(int)fn;         
            }
	  if (j==itp1_ind+1) 
	    node[i].typeN=(int)fn;
        }  
     
      do 
	cs=fgetc(fp); 
      while (cs!='\n');
    }
  fracture[nfract-1].lastnode=nnodes;
 // printf(" \n Material types and type of nodes are read \n"); 
  fclose(fp) ;
 

 
  /******************* open and read stor file *******************************/

  inputfile = Control_File("stor:",5 );

  FILE *fps= OpenFile (inputfile.filename,"r");
  
  printf("\n OPEN AND READ FILE: %s \n \n", inputfile.filename);
  
  /* Read the head of the file */
  for (i=0; i<2; i++)
    {
      do 
	cs=fgetc(fps); 
      while (cs!='\n');
    }
   
  unsigned int  node1;
  fscanf (fps," %d %d %d %d %d \n", &nedges, &node1, &snode_edge, &area_coef, &max_neighb );

  if (node1!=nnodes)
    printf("the number of nodes in inp file is not equal to number of nodes in stor file! \n");


  /* Read volumes of Voronoy polygons. Each polygon and it's volume is associated 
     with one node (that is a center of polygon) *************************/ 

  for (i=0; i<nnodes; i++)
    {
      fscanf(fps,"%lf", &node[i].pvolume);
    }
 // printf(" \n Volumes of Voronoi polygons are read \n"); 

  /* Read an array with number of neighbors for each node */ 

  unsigned int cn,pn;
   
  fscanf(fps, "%d", &pn);  

  for (i=0; i<nnodes; i++)
    { 
      fscanf(fps, "%d", &cn);
      node[i].numneighb=cn-pn;
      pn=cn;
    }
       
  /* Read indexes of neighboring nodes (including its node number)*/
 
  for (i=0; i<nnodes; i++)
    {
      for (j=0; j<node[i].numneighb; j++)
	{
	  fscanf(fps, "%d", &node[i].indnodes[j]);
	} 
    } 
 // printf("  \n Number of neighbors of each node and their indices are read \n ");

  /***Read pointers to area cofficients, zeros and diagonal elements in stor file **/

  for (i=0; i<nedges; i++)
    {
      fscanf(fps, "%d",&cn);
    }

  /*read zeros*/
  for (i=0; i<nnodes+1; i++)
    {
      fscanf(fps, "%d",&cn);
    }
  /* read diagonal elements*/  

  int count=0, count1=0;
       
  fscanf(fps, "%d", &pn);  
  for (i=0; i<nnodes-1; i++)
    { 
      fscanf(fps, "%d", &cn);
      count=count+cn-pn;
      count1=count1+node[i].numneighb;
      pn=cn;
    }
  /**** reading area coefficients *************************/  
  
  for (i=0; i<nnodes; i++)
    {
    node[i].aperture=0.0;
      for (j=0; j<max_neighb; j++)
	{
	  if (j<node[i].numneighb)
            {
	      fscanf(fps, "%lf", &node[i].area[j]);
	       
            }
	} 
    } 
  fclose(fps);
 
 
 

  /***** reading the data obtained in flow solution: pressure and mass fluxes ***/
  /**** the data from FEHM or PFLOTRAN *******************/
  printf("\n----------------FLOW SOLUTION DATA READING--------------------\n"); 
   int res;
  inputfile=Control_File("FEHM:",5);
  res=strncmp(inputfile.filename,"yes",3);
  if (res==0)
  {
    ReadFEHMfile(nedges);
  
     }
  else
    {     
      inputfile=Control_File("PFLOTRAN:",9);
      res=strncmp(inputfile.filename,"yes",3);
      if (res==0)
      {
	ReadPFLOTRANfile(nedges);
      }
      else
	{
	  printf("\n FLOW SOLUTION SOURCE IS NOT DEFINED. Program is terminated. \n");
	  exit(1);
	}
    }
     
    inputfile=Control_File("aperture:",9);
  
  res=strncmp(inputfile.filename,"yes",3);
  
  if (res==0)
    {
    
    AreaTimesAperture();
    }
    else
    { 
     printf("\n There is no aperture file is defined. All fractures will have constant aperture = 1.0 [m]. \n");
     
     for (i=0; i<nnodes; i++)
   	node[i].aperture=1.0;
     } 
  /***** ordering neighboring nodes, fluxes and area coefficients ******/
  for (i=0; i<nnodes; i++)
    {
      node[i].numneighb=(node[i].numneighb)-1;
      l=0;
      for (j=0; j<(node[i].numneighb)+1; j++)
	{
	  if (node[i].indnodes[j]!=i+1)           
	    {
	      node[i].indnodes[l]=node[i].indnodes[j];   
	      
	      node[i].type[l]=node[node[i].indnodes[l]-1].typeN;
	      node[i].flux[l]=node[i].flux[j];
	      
 
	      if (node[i].area[j]<0)
		node[i].area[l]=node[i].area[j]*(-1.0);
              else
                node[i].area[l]=node[i].area[j];
                   
                 
	      for (k=0; k<4; k++)
		{
		  node[i].cells[l][k]=0;
		  node[i].fracts[l][k]=0;
		}
	      l++;
	    }
	}
    }
 

  /***** search for neighboring cells *******************/

  for (ln=0; ln<ncells; ln++)
    {


      cell[ln].veloc_ind[0]=0;  
      cell[ln].veloc_ind[1]=0;
      cell[ln].veloc_ind[2]=0;

      AdjacentCells(ln, cell[ln].node_ind[0],cell[ln].node_ind[1],cell[ln].node_ind[2]);
      AdjacentCells(ln, cell[ln].node_ind[1],cell[ln].node_ind[2],cell[ln].node_ind[0]);
      AdjacentCells(ln, cell[ln].node_ind[2],cell[ln].node_ind[0],cell[ln].node_ind[1]);

 
    } //loop on ln 

   


  return;
}
/////////////////////////////////////////////////////////////////////////////
/*** function to define adjacent cells for each node  *********************/

void AdjacentCells(int  ln, int in, int  jn, int  kn)
{
  int jj,kk;
  for(jj=0; jj<node[in-1].numneighb; jj++)
    {
      if (node[in-1].indnodes[jj]==jn) 
	{
	  for (kk=0; kk<4; kk++)
	    {
	      if (node[in-1].cells[jj][kk]==0)
		{
		  node[in-1].cells[jj][kk]=ln+1;
		  node[in-1].fracts[jj][kk]=cell[ln].fracture;
                  if (cell[ln].fracture != node[in-1].fracture[0])
		    node[in-1].fracture[1]=cell[ln].fracture;
		  break;
		}
	    }
	}

      if (node[in-1].indnodes[jj]==kn) 
	{
	  for (kk=0; kk<4; kk++)
	    {
	      if (node[in-1].cells[jj][kk]==0)
		{
		  node[in-1].cells[jj][kk]=ln+1;
		  node[in-1].fracts[jj][kk]=cell[ln].fracture;
		  if (cell[ln].fracture != node[in-1].fracture[0])
		    node[in-1].fracture[1]=cell[ln].fracture;
		  break;
		}
	    }
	}
    } //loop on jj

  return;
}
////////////////////////////////////////////////////////////////////////////

 
void ReadBoundaryNodes()
{
  struct inpfile inputfile;
  /****Functions reads node's numbers assigned in BC from boundary.zone file *************/
  inputfile = Control_File("boundary:",9 );
 
  printf("\n OPEN AND READ FILE: %s \n ", inputfile.filename); 
  printf("\n Read the number and indices of nodes defined in flow in and flow out zones  \n");
  
  FILE *fpc = OpenFile (inputfile.filename,"r");
 
  int zonenumb_in=0, zonenumb_out=0;
  /* input from control.dat file */
  inputfile = Control_Data("in-flow-boundary:",17 );
  zonenumb_in=inputfile.flag;
  inputfile = Control_Data("out-flow-boundary:",18 );
  zonenumb_out=inputfile.flag;
  

  char zonen_in[10]={0},zonen_out[10]={0}, line[10]={0};
  int  nzone_out, i, res1, res2, fn, nn, res, nf, flag1=0, flag2=0;
  fscanf(fpc, "%s \n", &line);
  do
    {
      fn=0;
      fscanf(fpc," %d  ", &fn);
      fscanf(fpc," %s ",  &line);
  
      
      if  (fn==zonenumb_in) 
  
	{
	  flag1=1;
	  fscanf(fpc, " %s ", &line);
	  fscanf(fpc, " %d", &nzone_in);
	  /** memory allocation for nodezone_in nodes ******/
	  nodezonein=(unsigned int*) malloc (nzone_in*sizeof(unsigned int));

	  for (i=0; i<nzone_in; i++)
	    {
	      fscanf(fpc," %d ", &nf);
	      nodezonein[i]=nf;

	      if(node[nodezonein[i]-1].typeN<300)
	      node[nodezonein[i]-1].typeN=node[nodezonein[i]-1].typeN+300;
	    }
	}
      else 
	{
	 
	  if (fn==zonenumb_out)

	    {
	      flag2=1;
	      fscanf(fpc, "\n %s \n", &line);
	      fscanf(fpc, " %d\n",&nzone_out);
          /** memory allocation for nodezoneout nodes ******/
	  nodezoneout=(unsigned int*) malloc (nzone_out*sizeof(unsigned int));
	  
	      for (i=0; i<nzone_out; i++)
		{
		  fscanf(fpc," %d ", &nf);
		  nodezoneout[i]=nf;
		  if(node[nodezoneout[i]-1].typeN<200)
		  node[nodezoneout[i]-1].typeN=node[nodezoneout[i]-1].typeN+200;
		}
	    }
	  else
	    {
	      fscanf(fpc, " %s ", &line);
	      fscanf(fpc, " %d",&nn);
 
	      for (i=0; i<nn; i++)
		{
		  fscanf(fpc," %d ", &nf);
		}
	    }
	}
    }
  while ((res=strncmp(line,"stop", 4)));

  if ((flag1=0) || (flag2=0))
    {
      printf("Please check zones in boundary zone file.\n");
      exit(1);
    }

  printf("\n Number of nodes %d in flow-in zone.  \n", nzone_in);
  printf("\n Number of nodes %d in flow-out  zone.  \n", nzone_out);
 
  fclose(fpc);
  
  /*** calculate the sum of mass fluxes on inflow boundary and on outflow boundary
***/
double density1=0.0;
inputfile=Control_Param("density:",8);
   density1=inputfile.param;
//   printf(" density %lf \n", density1);
unsigned int j;
double sum_in=0,sumin=0.0;
double sum_out=0.,sumout=0.0, areasumin=0.0, areasumout=0.0;

// for (i=0; i<nzone_in; i++)
// {
 //if (nodezonein[i]==23)
// for (j=0; j<node[nodezonein[i]-1].numneighb; j++)
 //    {
     
  
 //    sum_in=sum_in+node[nodezonein[i]-1].flux[j];
 //     areasumin=areasumin+node[nodezonein[i]-1].area[j];
 //     }
 //   }
    
 //    printf ("Total in-flow volumetric flux = %12.5e [m^3/s]\n", sum_in);
 //if (areasumin>0)
// sumin=sum_in/areasumin;
// else 
 //sumin==0;
  //    printf ("Total in-flow flux = %12.5e [m/s]\n", sumin);
     

//for (i=0; i<nzone_out; i++)
//{
// for (j=0; j<node[nodezoneout[i]-1].numneighb; j++)
 //{
 //    areasumout=areasumout+node[nodezoneout[i]-1].area[j];
 //     sum_out=sum_out+node[nodezoneout[i]-1].flux[j];
// }
//}
//printf ("Total out-flow volumetric flux = %12.5e [m^3/s]\n", sum_out);

//if (areasumout>0)
//sumout=sum_out/areasumout;
//else 
//sumout=0.0;
//printf ("Total out-flow flux = %12.5e [m/s]\n", sumout);

   /***************************/
  
  return;
}
///////////////////////////////////////////////////////////////////
/**** Function opens file for reading or writing *********/
/**** If error - terminates the program *******************/ 
FILE *OpenFile(char filen[120], char fileopt[2])
{
  FILE* filepointer;
  
  if ((filepointer = fopen (filen,fileopt)) == NULL)
    {
      printf ("File %s could not be opened. Program is terminated. \n", filen);
      exit(1);
    }
  
  return filepointer;
}
//////////////////////////////////////////////////////////////////// 
void ReadPFLOTRANfile(int nedges)
{
  /**********Function opens and reads file (PFLOTRAN data )*************/
  struct inpfile inputfile;
   int n1=0, n2=0,i,n_flux,j,k, res1;
  char wordc[60];
  int n_area;
  double floatnumber, areavalue;
  double length;
  
  
  inputfile = Control_File("PFLOTRAN_uge:",13 );
 
  FILE *pfu= OpenFile (inputfile.filename,"r");
 
  
  density=1.0;
  printf("\n PFLOTRAN: OPEN AND READ FILE %s to read area coefficients. \n \n", inputfile.filename);
  
  fscanf(pfu," %s ",&wordc);
  fscanf(pfu," %d ",&n1);
  
  for (i=0; i<n1; i++)
  {
  fscanf(pfu," %d %f %f %f %f ", &n2,  &floatnumber, &floatnumber, &floatnumber, &floatnumber);
  }
  
  fscanf(pfu," %s  %d",&wordc, &n_area);
  
    res1=strncmp(wordc,"CONNECTIONS",11);
      if (res1!=0)
      {
  printf ("\n  CAN'T READ AREA COEFFICIENTS FROM UGE FILE. Program is terminated. \n");
  exit(1);
       }
  n1=0;
  n2=0;
 for (i=0; i<n_area; i++)
    {
      fscanf(pfu," %d  %d  %lf %lf %lf %lf ", &n1, &n2, &floatnumber, &floatnumber, &floatnumber, &areavalue);
 // the area from PFLOTRAN is not normalized, we normalize it here by dividing by lebgth of Delaunay edge 
     
      
      for (j=0; j<node[n1-1].numneighb; j++)
        {
	  if (node[n1-1].indnodes[j]==n2)
	    {
	    length=sqrt(pow(node[n1-1].coord[0]-node[node[n1-1].indnodes[j]-1].coord[0],2)+pow(node[n1-1].coord[1]-node[node[n1-1].indnodes[j]-1].coord[1],2)+pow(node[n1-1].coord[2]-node[node[n1-1].indnodes[j]-1].coord[2],2));
	    
	      node[n1-1].area[j]=areavalue;

  
	  
	      for (k=0; k<node[n2-1].numneighb; k++)
                {
		  if (node[n2-1].indnodes[k]==n1)
		    {
		    
		    length=sqrt(pow(node[n2-1].coord[0]-node[node[n2-1].indnodes[k]-1].coord[0],2)+pow(node[n2-1].coord[1]-node[node[n2-1].indnodes[k]-1].coord[1],2)+pow(node[n2-1].coord[2]-node[node[n2-1].indnodes[k]-1].coord[2],2));
		    
		      node[n2-1].area[k]=areavalue;

		      break;
		    }
                }
	      break;
	    }
        } 
    }
  
    
  fclose(pfu);
  
  /****** reading an aperture file  **********/
     int res, res_a;
  inputfile=Control_File("aperture:",9);
  
  res=strncmp(inputfile.filename,"yes",3);
  
  if (res==0)
    {
    
    AreaTimesAperture();
    
    }
    
  else
  {
   printf("\n There is no aperture file is defined. All fractures will have constant aperture = 1.0 [m]. \n");
   for (i=0; i<nnodes; i++)
   	node[i].aperture=thickness;
   }
/**********************************************/
  
  inputfile = Control_File("PFLOTRAN_vel:",13 );
 
  FILE *pf= OpenFile (inputfile.filename,"r");
 
  printf("\n PFLOTRAN: OPEN AND READ FILE: %s \n \n", inputfile.filename);
 
  // reading fluxes 
  double l_flux=0.0;
  double l_dens=0.0;
  double l_area=0.0;
  char cs, csp;
   n1=0;
   n2=0;
  int flag=0;
  n_flux=(nedges-nnodes)/2.;

// check the darcyvel file format;
   csp=' ';

 
     do
{       
        cs=fgetc(pf);
        if ((cs!=' ') && (csp==' '))
           n2++;
         csp=cs;
} 
     while (cs!='\n');

    if (n2>5)
       flag=1;
     
      n2=0;  
    rewind(pf);   

  for (i=0; i<n_flux; i++)
    {
      if (flag==0)
      fscanf(pf," %d  %d  %lf %lf  ", &n1, &n2, &l_flux, &l_dens);
      else
      fscanf(pf," %d  %d  %lf %lf %lf  ", &n1, &n2, &l_flux, &l_dens, &l_area);
    
     
      for (j=0; j<node[n1-1].numneighb; j++)
        {
	  if (node[n1-1].indnodes[j]==n2)
	    {
	//recommend to uncomment in case of PFLOTRAN solver and uniform mesh
	//node[n1-1].flux[j]=l_flux;
	
	     // unstructured grid
	      node[n1-1].flux[j]=l_flux*(node[n1-1].area[j]);
           
	      for (k=0; k<node[n2-1].numneighb; k++)
                {
		  if (node[n2-1].indnodes[k]==n1)
		    {
       //recommend to uncomment in case of PFLOTRAN solver and uniform mesh
	//	node[n2-1].flux[k]=l_flux;
		
		   //unstructured grid  
		   node[n2-1].flux[k]=(l_flux*(-1.0)*(node[n2-1].area[k]));
		    
		      break;
		    }
                }
	      break;
	    }
        } 
    }
    
    
    
  fclose(pf);
  
  inputfile = Control_File("PFLOTRAN_cell:",14 );
 
  FILE *fp= OpenFile (inputfile.filename,"r");
 
  printf("\n PFLOTRAN: OPEN AND READ FILE: %s \n \n", inputfile.filename);
  
     
  //reading pressure    
  printf(" Reading pressure \n");
  double maxpr=0.0, minpr=100.0, l_pres=0.0;   
  for (i=0; i<nnodes; i++)
    {
      fscanf(fp," %d %lf %lf %lf %lf \n", &n1, &l_flux, &l_flux, &l_dens, &l_pres);

      node[n1-1].pressure=l_pres/pow(10,6);

      if (node[n1-1].pressure>maxpr)
	maxpr=node[n1-1].pressure;
      if (node[n1-1].pressure<minpr)
	minpr=node[n1-1].pressure;
    }
  fclose(fp);
  printf(" MAX pressure %5.8e [MPa]; MIN pressure %5.8e [MPa] \n", maxpr, minpr);
  
     
     
  return;
}
/////////////////////////////////////////////////////////////////////
void ReadFEHMfile(int nedges)
{
  /**********Function opens and reads tri_frac.fin file (FEHM data )*************/
  int i, j;
  char cs, line[16];
  
  struct inpfile inputfile;
  
  inputfile = Control_File("FEHM_fin:",9 );
  
  FILE *fpr= OpenFile (inputfile.filename,"r");
  
  printf("\n FEHM: OPEN AND READ FILE: %s \n \n", inputfile.filename);

  /* Read the head of the file */
  for (i=0; i<4; i++)
    {
      do 
	cs=fgetc(fpr); 
      while (cs!='\n');
    }

  /* reading the pressure on nodes , Pressure in MPa units*/    
  fscanf(fpr, "%s \n",&line);
  double maxpr=0.0, minpr=100.0;
    
  int res=strncmp(line,"pressure",8);
  if (res==0) 
    { 
      printf(" Reading %s\n",line);
      for (i=0; i<nnodes; i++)
	{
	  fscanf(fpr,"%lf",&node[i].pressure);
	  if (node[i].pressure>maxpr)
	    maxpr=node[i].pressure;
	  if (node[i].pressure<minpr)
	    minpr=node[i].pressure;
	}
    }
  printf(" MAX pressure %5.8e MIN pressure %5.8e \n", maxpr, minpr);
  /* reading fluxes on edges*/      

  fscanf(fpr, "%s %s\n",&line, &line);
  int res1=strncmp(line,"flux",4);
  if (res1==0) 
    { 
      printf("\n Reading %s\n",line);
    }
  else
    {
      printf(" something is wrong with flux reading. please check.\n");
    }
        
  fscanf(fpr,"%d\n",&i);
      
  if (nedges!=i)
    {
      printf("number of edges in *.fin file (%d )is not the same as number of edges in *.stor file (%d)!\n",i, nedges);
    }
  /* reading fluxes */

  for (i=0; i<nnodes; i++)
    {
      for (j=0; j<max_neighb; j++)
	{
	  if (j<node[i].numneighb)
            {
	      fscanf(fpr, "%lf", &node[i].flux[j]);
            }
	} 
    } 
    
  fclose(fpr);
  printf("\n Fluxes  are read from FEHM file \n");
  return;
}
/////////////////////////////////////////////////////////////////////////////
void WritingInit()
{
  /**** Functions write the data structure of nodes, cells and fractures 
	into files in ASCII format with explanations
	(Good for debugging)***************************************/
   char filename[125];
   sprintf(filename,"%s/nodes",maindir);     
  FILE *wp = OpenFile (filename,"w");
  printf("\n Output initial data structure  \n");
  /************* output to file node ************************/
  int i,j,k;
  for (i=0; i<nnodes; i++)
    {      
      fprintf(wp,"\n Node %d, type %d, x=%5.15e, y=%5.15e, z=%5.15e, aperture=%5.15e, \n",i+1, 
	      node[i].typeN, node[i].coord[0],node[i].coord[1],node[i].coord[2], node[i].aperture);
      fprintf(wp, " Belongs to fracture/fractures %d %d \n", node[i].fracture[0], 
	      node[i].fracture[1]); 
      fprintf(wp,"Pressure %5.15e,   polygon volume %5.15e , number of neighbours %d \n", 
	      node[i].pressure,  node[i].pvolume, node[i].numneighb);

      for (j=0; j<node[i].numneighb; j++)
	{
	  fprintf(wp,"%d. face %d  %d (type %d), flux %5.15e, area %5.12e and belongs to cells:\n",
		  j+1, i+1,node[i].indnodes[j], node[i].type[j], node[i].flux[j], node[i].area[j]);

	  for (k=0; k<4; k++)
	    {
	      if (node[i].cells[j][k]!=0)
		fprintf(wp,"(%d) %d  in fracture %d; ", k+1, 
			node[i].cells[j][k],node[i].fracts[j][k]);
	    }
           
	  fprintf(wp,"\n");
	} //loop j
    } //loop i
  fclose(wp);


  // printf("\n Node's init data is written into  file \n");
  /* Fracture's structure includes indices of cells that belong to fructure and three nodes numbers*/  
  sprintf(filename,"%s/fractures",maindir); 
  FILE *wf = OpenFile (filename,"w");
    
  for (i=0; i<nfract; i++)
    {  
      fprintf(wf,"\n Fracture %d first node# %d last node# %d and theta %f\n", 
	      i+1, fracture[i].firstnode, fracture[i].lastnode, fracture[i].theta);
      fprintf(wf,"\n Fracture %d has %d cells, starting from %d \n", i+1, 
	      fracture[i].numbcells, fracture[i].firstcell);

      for (j=0; j<fracture[i].numbcells; j++)
	{
	  fprintf(wf," cell %d, nodes: %d %d %d \n", fracture[i].firstcell+j, 
		  cell[fracture[i].firstcell+j-1].node_ind[0], 
		  cell[fracture[i].firstcell+j-1].node_ind[1],
		  cell[fracture[i].firstcell+j-1].node_ind[2]);

	}//loop j
 
    }   //loop i     
  // printf("\n Fracture's data is written into  file \n");  
  fclose(wf);
  return;
}
///////////////////////////////////////////////////////////////////////////
void CheckGrid()
{
  /***** function checks the grid: are there are nodes that are defined as 
	 internal or external, howether they belong to two fractures 
	 and should be defined as interface nodes **************************/
  int i, j, k, r=0;
  printf("  GRID CHECK starts \n"); 
  for (i=0; i<nnodes; i++)
    {
      if ((node[i].typeN==0) || (node[i].typeN==10))   
	{
	  r=0;
	  for (j=0; j<node[i].numneighb; j++)
	    {
	      for (k=0; k<4; k++)
		{ 
		  if ((node[i].fracts[j][k]!=node[i].fracture[0]) && (node[i].fracts[j][k]!=0))
		    {
         	      printf("GRID ERROR: fracture %d node %d / fracture %d \n", node[i].fracture[0], i+1,node[i].fracts[j][k]);
         	      printf(" node %d  %lf  %lf  %lf \n",  i+1, node[i+1].coord[0],  node[i+1].coord[1],node[i+1].coord[2]);
         	      r++;
         	      break;
		    }
		}
	      if (r>0)
		break;  
	    }
         	
	}
    }    
  printf("  GRID CHECK is done \n"); 
  return;
} 
/////////////////////////////////////////////////////////////////////////////
struct inpfile Control_File(char fileobject[], int ctr)
/******** function reads control.dat file;
	  returns the file name of the input(or output) file *****/
{ 
   
  FILE *cf=OpenFile("PTDFN_control.dat","r");
  struct inpfile inputfile;
  char fileline[ctr];
 
  inputfile.flag=-1;
  int res2=1;
  do
    {
      fscanf(cf,"\n %s",&fileline);
  
      int res1=strncmp(fileline,fileobject,ctr);
      if (res1==0)
	{
	  fscanf(cf,"%s",&inputfile.filename);
    
	  inputfile.flag=1;
  
	  break;
	}
    }    
  while (res2=strncmp(fileline,"END",3));  
  if (inputfile.flag<0)
    {
      printf("\n There is no %s input found in PTDFN_control.dat. Program is terminated. \n", fileobject);   
      exit(1);
    }
  fclose(cf);
  return inputfile;
} 
////////////////////////////////////////////////////////////////////////////

struct inpfile Control_Data(char fileobject[], int ctr)
{ 
  /******** function reads control.dat file;
	    reads the input data parameters *************/
           
  FILE *cf=OpenFile("PTDFN_control.dat","r");
  struct inpfile inputfile;
  char fileline[ctr];
 
  inputfile.flag=-1;
  int res2=1;
  do
    {
      fscanf(cf,"%s",&fileline);
  
      int res1=strncmp(fileline,fileobject,ctr);
      if (res1==0)
	{
	  fscanf(cf,"%ld",&inputfile.flag);

	  break;
	}
    }    
  while (res2=strncmp(fileline,"END",3));  
  if (inputfile.flag<0)
    {
      printf("\n There is no %s input found in PTDFN_control.dat. Program is terminated. \n", fileobject);   
      exit(1);
    }
  fclose(cf);
  return inputfile;
} 
////////////////////////////////////////////////////////////////////////////
struct inpfile Control_Param(char fileobject[], int ctr)
{ 
  /******** function reads control.dat file;
	    reads the input data parameters *************/
           
  FILE *cf=OpenFile("PTDFN_control.dat","r");
  struct inpfile inputfile;
  char fileline[ctr];
 
  inputfile.flag=-1;
  int res2=1;
  do
    {
      fscanf(cf,"%s",&fileline);
  
      int res1=strncmp(fileline,fileobject,ctr);
      if (res1==0)
	{
	  fscanf(cf,"%lf",&inputfile.param);
	  inputfile.flag=1;
	  break;
	}
    }    
  while (res2=strncmp(fileline,"END",3));  
  if (inputfile.flag<0)
    {
      printf("\n There is no %s input found in PTDFN_control.dat. Program is terminated. \n", fileobject);   
      exit(1);
    }
  fclose(cf);
  return inputfile;
} 
////////////////////////////////////////////////////////////////////////////
void AreaTimesAperture()
{
/****** function reads aperture from file
       DO NOT CHANGE AREA ******************************/
  struct inpfile inputfile;
  char cs;
  int i, j,res_a;    
 
  inputfile=Control_File("aperture_type:",14);
  
   res_a=strncmp(inputfile.filename,"frac",4);
// if given aperture is const per fracture    
  if (res_a==0)
   {
   
   inputfile = Control_File("aperture_file:",14 );
 
  FILE *ad = OpenFile (inputfile.filename,"r");
   printf("\n OPEN AND READ FILE: %s \n ",inputfile.filename); 
 double currentap=0.0, aperturem[nfract];
 
 int apmat=0, zn; 
 
     do 
	cs=fgetc(ad); 
      while (cs!='\n');
 
 for (i=0; i<nfract; i++)
    {
 fscanf(ad,"%d %d %d %lf \n", &apmat, &zn, &zn, &currentap);
 apmat=apmat*(-1)-6;
 aperturem[apmat-1]=currentap;
    }
  fclose(ad);
  
 
  
  for (i=0; i<nnodes; i++)
    {
    
 // define an aperture of the cell with current node as cellcenter   
    if (node[i].fracture[1]==0)
    node[i].aperture=aperturem[node[i].fracture[0]-1];
    else
    {
     if (aperturem[node[i].fracture[0]-1]>=aperturem[node[i].fracture[1]-1])
         node[i].aperture=aperturem[node[i].fracture[0]-1];
      else
         node[i].aperture=aperturem[node[i].fracture[1]-1];   
    }
   
  
   
  
    } 
 }
 
   else
   {
  res_a=strncmp(inputfile.filename,"cell",4);
// if given aperture is given per cell    
  if (res_a==0)
 {
 
  inputfile = Control_File("aperture_file:",14 );
 
  FILE *ada = OpenFile (inputfile.filename,"r");
  
  printf("\n OPEN AND READ FILE: %s \n ",inputfile.filename); 
 
  double currentap=0.0;
  double *aperturem;
  
   aperturem = (double*)malloc(nnodes*sizeof(double));
 
 
 int apmat=0, zn; 
 char c;
 
     do 
     {
 
	c=fgetc(ada); 

	}
      while (c!='\n');
  
 for (i=0; i<nnodes; i++)
    {
 fscanf(ada,"%d %lf \n", &apmat, &currentap);
 
 aperturem[apmat-1]=currentap;

    }
  fclose(ada);
  
  
  
  for (i=0; i<nnodes; i++)
    {
  
  // define an aperture of the cell with current node as cellcenter   
   
    node[i].aperture=aperturem[i];
    
    }
    free(aperturem);
 }
 
  } 
return;
}
