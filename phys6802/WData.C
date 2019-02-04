#include <TRandom3.h>
#include <iostream>
#include <TMath.h>
#include <Riostream.h>
using namespace std;


void WData(int opt = 3)
{

	TCanvas *c1 = new TCanvas("c1","RootData",200,10,700,500);
	
	ifstream in;
	in.open("WDist.shi");

	Double_t xvar1, xvar2, theta, phi;
	Double_t x,y,z;
	Double_t yval;
	Int_t nlines = 0;
	TH1F *Data = new TH1F("f","W Dist Data",300,0,300);
	TH1F *UDist = new TH1F("Up Quark Distribution","Up Quark Distribution", 100,0,1);
	TH1F *DBarDist = new TH1F("DBar Quark Distribution","DBar Quark Distribution", 100,0,1);
	TH1F *g = new TH1F("GetRandom Data","GetRandom Data",300,0,300);
	TRandom3 *r1 = new TRandom3(12);
	TRandom3 *r2 = new TRandom3(123);
	TRandom3 *r3 = new TRandom3(1234);
   
	while (1)
	{ 
	in >> x >> yval;
	y = yval / 100;
	if (!in.good())
		{break;}
	if (nlines < 301)
		{//cout << x << "  " << y << endl;
		}
	f->Fill(x, y);
	nlines++;
	}

	in.close();

	ifstream inUp;
	inUp.open("UpPDF.shi");
	Double_t xup,yup;
	Int_t nlinesUp = 0;
	while (1)
	{ 
	inUp >> xup >> yup;
	if (!inUp.good())
		{break;}
		if (nlinesUp < 301)
		{//cout << xup << "  " << yup << endl;
		}
	UDist->Fill(xup, yup);
	nlinesUp++;
	}
	inUp.close();
	
	ifstream inDBar;
	inDBar.open("DBarPDF.shi");
	Double_t xDBar,yDBar;
	Int_t nlinesDBar = 0;
	while (1)
	{ 
	inDBar >> xDBar >> yDBar;
	if (!inDBar.good())
		{break;}
	DBarDist->Fill(xDBar, yDBar);
	nlinesDBar++;
	}
	inDBar.close();


	TH1F *WPeak = new TH1F("W Resonance","W Resonance",50,75,100);
	TH1F *EtFix = new TH1F("W Transverse Energy","W Transverse Energy (Fixed Mass)", 45,75,120);
	TH1F *LongPt = new TH1F("W Longitudinal Momentum","W Longitudinal Momentum", 1600,-8000,8000);
	TH1F *EtVar = new TH1F("W Transverse Energy","W Transverse Energy (Spread Mass)", 45,75,120);
	TH1F *MTPlot = new TH1F("W Transverse Mass","W Transverse Mass", 100,0,100);
	TH1F *EtBoost = new TH1F("W Transverse Energy","W Transverse Energy (Boosted)", 45,75,120);
	TF1 *BreitWig= new TF1("BreitWig", "TMath::BreitWigner(x, 80.385, 2.085)", 75, 100);
	TF1 *WMinus= new TF1("WMinus", "(1+x)*(1+x)", -1, 1);
	TF1 *WPlus= new TF1("WPlus", "(1-x)*(1-x)", -1, 1);
	TH1F *WMinusPlot= new TH1F("WMinus", "W- Distribution", 100, -1, 1);
	TH1F *WPlusPlot= new TH1F("WPlus", "W+ Distribution", 100, -1, 1);
	TH1F *LeptPt = new TH1F("Lepton Pt","Lepton Pt", 100, 0,100);
	TH1F *LeptEta = new TH1F("Lepton Eta","Lepton Eta", 1000,-10,10);
	TH1F *LeptPhi = new TH1F("Lepton Phi","Lepton Phi", 1000,-1*TMath::Pi(),TMath::Pi());
	TH1F *LeptEt = new TH1F("Lepton Et","Lepton Et", 100, 0,100);
	TH1F *LeptTheta = new TH1F("Lepton Theta","Lepton Theta", 1000, 0, TMath::Pi());

	for (Int_t i=0; i < 1000000; i++)
	{
	Double_t WPlusCos = WPlus->GetRandom();
	Double_t WMinusCos = WMinus->GetRandom();
	WPlusPlot->Fill(WPlusCos);
	WMinusPlot->Fill(WMinusCos);
	Double_t VelSign = -1;
	Double_t LongSign = -1;
	Double_t Wvar1 = r1 -> Rndm(i);
	Double_t Wphi = 2*TMath::Pi()*Wvar1;
	Double_t TransFlag= r2-> Rndm(i);
	if (TransFlag < 0.5)
	{VelSign = 1;}

	z=f->GetRandom();
	g->Fill(z);
	Double_t z2= BreitWig->GetRandom();
	WPeak->Fill(z2);
	Double_t WMass= 80.385;
	Double_t WVel= z/z2;
	Double_t Et1 = sqrt(z*z+WMass*WMass);
	Double_t Et2 = sqrt(z*z+z2*z2);
	EtFix->Fill(Et1);
	EtVar->Fill(Et2);
	TLorentzVector WVect(WVel*TMath::Cos(Wphi), WVel*TMath::Sin(Wphi), 0, Et2);
	WVect.Boost(-1*WVel*TMath::Cos(Wphi), -1*WVel*TMath::Sin(Wphi), 0);
	Double_t Et3=WVect.E();
	EtBoost->Fill(Et3);

	Double_t up=UDist->GetRandom();
	Double_t DBar=DBarDist->GetRandom();
	Double_t LongFlag= r3-> Rndm(i);
	if (LongFlag < 0.5)
	{LongSign = 1;}

	Double_t PUp= LongSign*7000*up;
	Double_t PDBar = -1*LongSign*7000*DBar;

	Double_t EUp = sqrt(PUp*PUp+0.0023*0.0023);
	Double_t EDBar = sqrt(PDBar*PDBar+0.0048*0.0048);
	Double_t WLongPt = PUp + PDBar;
	LongPt->Fill(WLongPt);
	Double_t WEnergy = sqrt(WLongPt*WLongPt+Et2*Et2);
	Double_t WLongVel = WLongPt / WEnergy;
//	cout << WLongVel << endl;
	TLorentzVector WVector(0, 0, 0, z2);
	WVector.Boost(WVel*TMath::Cos(Wphi), WVel*TMath::Sin(Wphi), WLongVel);

	Double_t xvar1 = r3 -> Rndm(i);
	Double_t phi = 2*TMath::Pi()*xvar1;
	Double_t xvar2 = r3 -> Rndm(i);
	Double_t theta = TMath::ASin((2*xvar2)-1);
	TLorentzVector Lept((z2/2)*TMath::Cos(theta)*TMath::Cos(phi), (z2/2)*TMath::Cos(theta)*TMath::Sin(phi), (z2/2)*TMath::Sin(theta), (z2/2));
	TLorentzVector Neut(-1*(z2/2)*TMath::Cos(theta)*TMath::Cos(phi), -1*(z2/2)*TMath::Cos(theta)*TMath::Sin(phi), -1*(z2/2)*TMath::Sin(theta), (z2/2));
	Lept.Boost(WVel*TMath::Cos(Wphi), WVel*TMath::Sin(Wphi), WLongVel);
	Neut.Boost(WVel*TMath::Cos(Wphi), WVel*TMath::Sin(Wphi), WLongVel);
	Double_t DPhi= Lept.Phi() - Neut.Phi();
	LeptPt->Fill(Lept.Pt());
	LeptEta->Fill(-1*TMath::Log(TMath::Tan((Lept.Theta()/2))));
	LeptPhi->Fill(Lept.Phi());
	LeptEt->Fill(Lept.Et());
	LeptTheta->Fill(Lept.Theta());
	MT= sqrt(2*Lept.Et()*Neut.Et()*(1-TMath::Cos(DPhi)));
	MTPlot->Fill(MT);
	}	
	
	
	if (opt == 1)
	{f->Draw();
	f->GetXaxis()-> SetTitle("pT (GeV)");
	f->GetYaxis()-> SetTitle("Differential Cross-Section (Gev)^-1");
	f->GetYaxis()-> SetTitleOffset(1.3);
	}
	else if (opt == 2)
	{g->Draw();
	g->GetXaxis()-> SetTitle("pT (GeV)");
	g->GetYaxis()-> SetTitle("Events");
	g->GetYaxis()-> SetTitleOffset(1.3);
	}
	if (opt == 3)
	{
	WPeak->Draw();
	WPeak->GetXaxis()-> SetTitle("W Mass (GeV)");
	WPeak->GetYaxis()-> SetTitle("Events");
	WPeak->GetYaxis()-> SetTitleOffset(1.3);
	}
	if (opt == 4)
	{
	EtFix->Draw();
	EtFix->GetXaxis()-> SetTitle("Transverse Energy (GeV)");
	EtFix->GetYaxis()-> SetTitle("Events");
	EtFix->GetYaxis()-> SetTitleOffset(1.3);
	}
	if (opt == 5)
	{
	EtVar->Draw();
	EtVar->GetXaxis()-> SetTitle("Transverse Energy (GeV)");
	EtVar->GetYaxis()-> SetTitle("Events");
	EtVar->GetYaxis()-> SetTitleOffset(1.3);
	}
	if (opt == 6)
	{
	EtBoost->Draw();
	EtBoost->GetXaxis()-> SetTitle("Transverse Energy (GeV)");
	EtBoost->GetYaxis()-> SetTitle("Events");
	EtBoost->GetYaxis()-> SetTitleOffset(1.3);
	}
	if (opt == 7)
	{
	UDist->Draw();
	UDist->GetXaxis()-> SetTitle("Momentum Fraction");
	UDist->GetYaxis()-> SetTitle("Events");
	UDist->GetYaxis()-> SetTitleOffset(1.3);
	}
	if (opt == 8)
	{
	DBarDist->Draw();
	DBarDist->GetXaxis()-> SetTitle("Momentum Fraction");
	DBarDist->GetYaxis()-> SetTitle("Events");
	DBarDist->GetYaxis()-> SetTitleOffset(1.3);
	}
	else if (opt == 9)
	{LongPt->Draw();
	LongPt->GetXaxis()-> SetTitle("pL (GeV)");
	LongPt->GetYaxis()-> SetTitle("Events");
	LongPt->GetYaxis()-> SetTitleOffset(1.3);
	}
	else if (opt == 10)
	{LeptPt->Draw();
	LeptPt->GetXaxis()-> SetTitle("Lept Pt (GeV)");
	LeptPt->GetYaxis()-> SetTitle("Events");
	LeptPt->GetYaxis()-> SetTitleOffset(1.3);
	}
	else if (opt == 11)
	{LeptEta->Draw();
	LeptEta->GetXaxis()-> SetTitle("Lept Eta");
	LeptEta->GetYaxis()-> SetTitle("Events");
	LeptEta->GetYaxis()-> SetTitleOffset(1.3);
	}
	else if (opt == 12)
	{LeptPhi->Draw();
	LeptPhi->GetXaxis()-> SetTitle("Lept Phi");
	LeptPhi->GetYaxis()-> SetTitle("Events");
	LeptPhi->GetYaxis()-> SetTitleOffset(1.3);
	}
	else if (opt == 13)
	{LeptEt->Draw();
	LeptEt->GetXaxis()-> SetTitle("Lept Et (GeV)");
	LeptEt->GetYaxis()-> SetTitle("Events");
	LeptEt->GetYaxis()-> SetTitleOffset(1.3);
	}
	else if (opt == 14)
	{LeptTheta->Draw();
	LeptTheta->GetXaxis()-> SetTitle("Lept Theta");
	LeptTheta->GetYaxis()-> SetTitle("Events");
	LeptTheta->GetYaxis()-> SetTitleOffset(1.3);
	}
	else if (opt == 15)
	{MTPlot->Draw();
	MTPlot->GetXaxis()-> SetTitle("Transverse Mass (GeV)");
	MTPlot->GetYaxis()-> SetTitle("Events");
	MTPlot->GetYaxis()-> SetTitleOffset(1.3);
	}
	else if (opt == 16)
	{WPlusPlot->Draw();
	WPlusPlot->GetXaxis()-> SetTitle("Cosine (theta)");
	WPlusPlot->GetYaxis()-> SetTitle("Events");
	WPlusPlot->GetYaxis()-> SetTitleOffset(1.3);
	}
	else if (opt == 17)
	{WMinusPlot->Draw();
	WMinusPlot->GetXaxis()-> SetTitle("Cosine (theta)");
	WMinusPlot->GetYaxis()-> SetTitle("Events");
	WMinusPlot->GetYaxis()-> SetTitleOffset(1.3);
	}


}

