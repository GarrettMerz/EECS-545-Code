#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <algorithm>
#include <memory.h>
#include <TH2D.h>

using namespace std;
int relax()
{

  //Declare array, histogram
  static const int A = 100;
  double h= 1/A;
  double a = (2/3)*A;
  double b = A;
  double startnum = (b - a)/ 2;
  double Potentials[A][A];
  double Mu[A][A];
  double TempPotentials[A][A];
  int maxtime = 1000;
  int timecount = 0;
  double H0 = 1;
  TCanvas *c1 = new TCanvas("c1", "c1",900,900);
  c1->cd();
  TH2D *ContourPlot = new TH2D("Contour Plot", "Relaxation Method", A, 0., 1., A, 0., 1.);
  
  //Initialize
  for (int i=1; i <= A-2; i++)
    {for (int j=1; j <= A-2; j++)
 	{Potentials[i][j]= 1;
	 Mu[i][j]=30;}
    }
  for (int i=startnum; i <= startnum + a - 1; i++)
	{for (int j=startnum; j <= startnum+ a -1; j++)
 		{
		Mu[i][j]= 1;		
		}
	}
  
  for (int j=0; j <= A-1; j++)
 	{
	Potentials[0][j]= 0;
	Potentials[A-1][j]= H0*(A-1);			
	}
  for (int i=0; i <= A-1; i++)
 	{
	Potentials[i][0]= H0*i;
	Potentials[i][A-1]= H0*i;			
	}

  //Loop
  while (timecount <= 1000)
	{
	 for (int i=1; i <= A-2; i++)
  	  {for (int j=1; j <= A-2; j++)
	   {TempPotentials[i][j] = ((Mu[i][j-1]+Mu[i][j])*Potentials[i][j-1] + (Mu[i-1][j]+Mu[i][j])*Potentials[i-1][j] + (Mu[i][j+1]+Mu[i][j])*Potentials[i][j+1] + (Mu[i+1][j]+Mu[i][j])*Potentials[i+1][j])/(Mu[i+1][j]+Mu[i-1][j]+Mu[i][j+1]+Mu[i][j-1]+4*Mu[i][j]);}
	  }

	for (int j=0; j <= A-1; j++)
 	{
	TempPotentials[0][j]= 0;
	TempPotentials[A-1][j]= H0*(A-1);			
	}
  	for (int i=0; i <= A-1; i++)
 	{
	TempPotentials[i][0]= H0*i;
	TempPotentials[i][A-1]= H0*i;			
	}
	for (int i=0; i<=A-1; i++)	
	    {for (int j=0; j <= A-1; j++)
	       { Potentials[i][j] = TempPotentials[i][j];}
	    } 	

	timecount++;
	}


  //Print
  for (int i=0; i<=A-1; i++)	
    {for (int j=0; j <= A-1; j++)
       { cout << Potentials[i][j] << " ";
	 ContourPlot->SetBinContent(i+1,j+1, Potentials[i][j]);
	}
     cout << endl;
    }
  gStyle->SetPalette(57);
  ContourPlot->Draw("COLZ");

  return 0;
	
}
