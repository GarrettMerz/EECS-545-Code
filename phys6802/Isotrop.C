#include <TRandom3.h>
#include <iostream>
#include <TMath.h>
using namespace std;


void Isotrop(int opt = 1)
{
	TCanvas *c1 = new TCanvas("c1","Isotrop",200,10,700,500);
	Double_t igood = 0;
	Int_t imax = 1000000;
	TH2F *cosplot = new TH2F("Cosine Plot","Cosine Plot", 100, -2*TMath::Pi(), 2*TMath::Pi(), 100,-1, 1);
	TH3F *sphereplot = new TH3F("Sphere Plot","Sphere Plot", 100, -1, 1, 100,-1, 1, 100,-1, 1);
	TH1F *thetaplot = new TH1F("Theta Plot","Theta Plot", 100, 0, TMath::Pi());
	TH1F *phiplot = new TH1F("Phi Plot","Phi Plot", 100, 0, 2*TMath::Pi());
	TH2F *sinplot = new TH2F("Sin Plot","Sin Plot", 100, 0, 2*TMath::Pi(), 100, 0, 1);
	Int_t firstcount, secondcount, thirdcount;
	
	TRandom3 *r3 = new TRandom3(1234); 

	Double_t x, y, x1, x2;
	Double_t theta, phi;
	for (Int_t i=1; i<=imax; i++)
	{
	x1 = r3 -> Rndm(i);
	x = 2*TMath::Pi()*x1;
	x2 = r3 -> Rndm(i);
	y= (2*x2)-1;
		if (TMath::Cos(x) >= y)
		{
		igood++;
		cosplot->Fill(x,y);
		}
	theta = TMath::ASin(y);
	phi = x;
	sphereplot->Fill(TMath::Cos(theta)*TMath::Cos(phi), TMath::Cos(theta)*TMath::Sin(phi), TMath::Sin(theta));
	

	thetaplot->Fill(theta);
	phiplot->Fill(phi);
	sinplot->Fill(phi, TMath::Sin(theta));

	if ((TMath::Sin(theta) >= 0)  && (TMath::Sin(theta) <= 0.10))
		{firstcount++;}
	else if ((TMath::Sin(theta) >= 0.65) && (TMath::Sin(theta) <= 0.75))
		{secondcount++;} 
	else if ((TMath::Sin(theta) >= 0.90) && (TMath::Sin(theta) <= 1.0))
		{thirdcount++;} 
	
	}

	cout<< "First Area is " << firstcount << endl;
	cout<< "Second Area is " << secondcount << endl;
	cout<< "Third Area is " << thirdcount << endl;


if	(opt == 1)
	{cosplot->Draw();}
else if (opt == 2)
	{sphereplot->Draw();}
else if (opt == 3)
	{thetaplot->Draw();}
else if (opt == 4)
	{phiplot->Draw();}
else if (opt == 5)
	{sinplot->Draw();}

return;
}
