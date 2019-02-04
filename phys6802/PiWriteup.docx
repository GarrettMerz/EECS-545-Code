#include <TRandom3.h>
#include <iostream>
#include <TMath.h>
using namespace std;


void PiCalc()
{
	Double_t piNum[5][25];
	Double_t Errors[5][25];
	Double_t piMean[5];
	Double_t piSig[5];
	Double_t Total[5]= {0,0,0,0,0};
	Double_t ErrorSum[5] = {0,0,0,0,0};
	Double_t Count[25];
	static const Int_t imax = 1000000;
	static const Int_t numpoints= imax / 100;
	Int_t k=0;
	Int_t k2=0;	
	Int_t Seedlist[25]= {17, 1234, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18, 19, 20, 21, 22, 23, 24};
	

	TCanvas *c1 = new TCanvas("c1","PiGraph",200,10,700,500);
	// c1->Divide(2,3);
	
		for (Int_t j=0; j<25; j++)
		{
			TRandom3 *r3 = new TRandom3(Seedlist[j]); 

			Double_t x,y;
			Double_t xplot[numpoints], yplot[numpoints], piDev[numpoints];
			Double_t xplot2[numpoints], yplot2[numpoints], piDev2[numpoints];
			Double_t igood = 0 ;
				for (Int_t i=1; i<=imax; i++)
				{
					x = r3 -> Rndm(i);
					y = r3 -> Rndm(i);
		
					if (x*x + y*y <= 1)
					{
						igood++;
						Double_t pitemp= 4*(igood)/i;
					}

					if ((Seedlist[j]==17) && (i%100 == 0))
					{
				//	cout << igood << " good ;" << i << " tries; " << "pi= " << pitemp << endl;
					xplot[k] = i;
					yplot[k] = pitemp;
					piDev[k] = pitemp - TMath::Pi();
					k++;
					}
					if ((Seedlist[j]==1234) && (i%100 == 0))
					{
				//	cout << igood << " good ;" << i << " tries; " << "pi= " << pitemp << endl;
					xplot2[k2] = i;
					yplot2[k2] = pitemp;
					piDev2[k2] = pitemp - TMath::Pi();
					k2++;
					}
					
					for (int count = 0; count < 5; count++)
					{ 
						if (TMath::Log10(i) == (count + 2))
						{
						piNum[count][j] = pitemp;
						}	
					}
				}

		}


		for (int count=0; count < 5; count++)
		{
			for(int j=0; j<25; j++)
			{
			Total[count] = Total[count] + piNum[count][j];
			piMean[count] = Total[count]/25;		
			}
		
		}

		TF1 *myfit = new TF1("myfit","[0]+[1]/TMath::Sqrt(x)", 300000, 1000000);
		myfit->SetParName(0,"a");
		myfit->SetParName(1,"b");
		myfit->SetParameter(0,0);
		myfit->SetParameter(1,10);

		TF1 *myfit2 = new TF1("myfit2","[0]/TMath::Sqrt(x)", 300000, 1000000);
		myfit2->SetParName(0,"b");
		myfit2->SetParameter(0,3);


	//	c1->cd(1);
		TGraph *seedplot = new TGraph(numpoints, xplot, yplot);
		seedplot-> SetMarkerColor(4);
		seedplot-> SetMarkerStyle(20);
		seedplot-> SetTitle("Pi vs. n, Seed 17");
	//	seedplot-> Draw("ap");

		
	//	c1->cd(5);
		TH1D *gg = new TH1D ("GaussianGraph", "GaussianGraph", 30, 3.1, 3.2);

		for (Int_t i = 0; i < 5; i++)
		{
			for (Int_t l=0; l<25; l++)
			{
			Errors[i][l] = piNum[i][l]-piMean[i];
			ErrorSum[i]= ErrorSum[i] + (Errors[i][l]*Errors[i][l]);
				if (i == 3)
				{
				gg->Fill(piNum[i][l]);
				Count[l]= l;
				}
			}

	//	gg -> Draw();

		piSig[i]= sqrt( ErrorSum[i] / 24);
		cout << "for " << pow(10, i+2) << " tries, pi = " << piMean[i] << " +- " << piSig[i] << endl;
		}


	//	c1->cd(2);
		TGraph *errorplot = new TGraph(numpoints, xplot, piDev);
		errorplot-> SetMarkerColor(4);
		errorplot-> SetMarkerStyle(20);
		errorplot-> SetTitle("Deviations vs. n, Seed 17");
		errorplot-> Draw("ap");
		errorplot->Fit("myfit2", "R");

		Int_t nn = 5;
		Double_t xp[5];
		Double_t yp[5];		
		Double_t yerr[5];
		Double_t xerr[5];
		
		for (int i = 0; i < 5; i++)
		{
		Int_t index = pow(10, i) - 1;
		xp[i]= xplot[index];
		yp[i] = piDev[index];
		xerr[i]=0;
		yerr[i]=piSig[i];
		}

		TGraphErrors *barplot = new TGraphErrors(nn, xp, yp, xerr, yerr);
		barplot-> SetMarkerColor(3);
		barplot-> SetMarkerStyle(20);
		barplot-> Draw("P");
	//	barplot->Fit("myfit", "R");
		
	//	c1->cd(3);
		TGraph *seedplot2 = new TGraph(numpoints, xplot2, yplot2);
		seedplot2-> SetMarkerColor(4);
		seedplot2-> SetMarkerStyle(20);
		seedplot2-> SetTitle("Pi vs. n, Seed 1234");
	//	seedplot2-> Draw("ap");

	//	c1->cd(4);
		TGraph *errorplot2 = new TGraph(numpoints, xplot2, piDev2);
		errorplot2-> SetMarkerColor(4);
		errorplot2-> SetMarkerStyle(20);
		errorplot2-> SetTitle("Deviations vs. n, Seed 1234");
	//	errorplot2-> Draw("ap");
	//	errorplot2->Fit("myfit2", "R");


		Double_t xp2[5];
		Double_t yp2[5];		
		Double_t yerr2[5];
		Double_t xerr2[5];
		
		for (int i = 0; i < 5; i++)
		{
		Int_t index = pow(10, i) - 1;
		xp2[i]= xplot2[index];
		yp2[i] = piDev2[index];
		xerr2[i]=0;
		yerr2[i]=piSig[i];
		}
		
		TGraphErrors *barplot2 = new TGraphErrors(nn, xp2, yp2, xerr2, yerr2);
		barplot2-> SetMarkerColor(3);
		barplot2-> SetMarkerStyle(20);
	//	barplot2-> Draw("P");
	//	barplot2->Fit("myfit", "R");
						
return;
}
