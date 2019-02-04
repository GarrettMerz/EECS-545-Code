#include "/usr/local/include/fst/fstlib.h"
#include "/usr/local/include/fst/fst.h"
#include "/usr/local/include/fst/float-weight.h"
#include "/usr/local/include/fst/vector-fst.h"
#include "my-fst.cpp"
#include "my-intersect.h"
#include "/usr/local/include/fst/compose.h"
#include "/usr/local/include/fst/connect.h"
#include <vector>
#include <stdio.h>

using namespace std;
using namespace fst;


extern "C" {

    void ReweightFst(MyLogFst *fst, double (*EditProb) (int)) {
      fst->Reweight(EditProb);
    }
      
    void GetAllEditsHelper(MyLogFst &fst, int state, vector<int> str, vector<vector<int> > &res) {
      if (fst.Final(state) != MyLogWeight::Zero()) { 
        str.push_back(0);
        res.push_back(str);
        str.pop_back();
      }
      for (ArcIterator<MyLogFst> aiter(fst,state); ! aiter.Done(); aiter.Next()) {
        const MyLogArc &arc = aiter.Value();
        str.push_back(fst.GetEdit(arc.nextstate)); 
        GetAllEditsHelper(fst, arc.nextstate, str, res);
        str.pop_back();
      }
    }
    int ** GetAllEdits(MyLogFst *fst, int **lengths, int *num_strings) {
      vector<int> str(0);
      vector<vector<int> > res_vector;
      GetAllEditsHelper(*fst, fst->Start(), str, res_vector);
      *lengths = (int*) malloc(res_vector.size()*sizeof(int));
      int ** res = (int**) malloc(res_vector.size()*sizeof(int*));
      for (int i = 0; i < res_vector.size(); i++) {
        res[i] = (int*) malloc(res_vector[i].size()*sizeof(int)); 
        (*lengths)[i] = res_vector[i].size();
        for (int j = 0; j < res_vector[i].size(); j++) {
          res[i][j] = res_vector[i][j];
          }
        }
        *num_strings = res_vector.size();
        // printf("[%d, %d, %d]\n", res[0][0], res[0][1], res[0][2]);
        return res;
    }
    int* Sample(MyLogFst *fst, int *size) {
      vector<int> res_ = fst->Sample();
      int length = res_.size();
      *size = length;
      int *res;
      res = (int*) malloc(length*sizeof(int));
      for (int i = 0; i < length; ++i) {
        res[i] = res_[i];
      }
      return res;
    }
    
    int* GetBestEdits(MyLogFst *fst, int *size) {
      vector<int> res_ = fst->GetBestEdits();
      int length = res_.size();
      *size = length;
      int *res;
      res = (int*) malloc(length*sizeof(int));
      for (int i = 0; i < length; ++i) {
        res[i] = res_[i];
      }
      return res;
    }
    
    double Random(MTRand *rng, double max) {
      double rn = rng->rand(max);
      return rn;
    }
      
    void TestFstLib(char *filename) {
        MyLogFst fst;
        fst.AddState();
        fst.SetStart(0);
        fst.AddArc(0, MyLogArc(1,1,1.0,1));
        fst.AddArc(0, MyLogArc(2,2,1.0,1));
        fst.AddState();
        fst.AddArc(1, MyLogArc(3,3,1.0,2));
        fst.AddState();
        fst.SetFinal(2, 1.0);
        fst.Write(filename);
    }

    MyLogFst* NewFst(MTRand *rng) {
      MyLogFst *out = new MyLogFst(*rng);
      out->AddState();
      out->SetStart(0);
      return out;
    }
    MTRand* NewRng(int seed) {
      MTRand *rng = new MTRand(seed);
      return rng;
    }

    void FinalizeFst(MyLogFst *in) {
      delete in;
    }
    
    void FinalizeRng(MTRand *rng) {
      delete rng;
    }
    
    double GetBeta(MyLogFst *in, int state) {
      return in->GetBeta(state) ;
    }
    
    double GetBest(MyLogFst *in, int state) {
      return in->GetBest(state);
    }

    int AddState(MyLogFst *in, int edit) {
      return in->AddState(edit);
    }

    void SetFinal(MyLogFst *in, int state, float weight) {
      in->SetFinal(state, weight);
    }

    void SetStart(MyLogFst *in, int state) {
       in->SetStart(state);
    }
    
    void Normalize(MyLogFst *in) {
      in->Normalize();
    }
    
    int GetStart(MyLogFst *in) {
      return in->Start();
    }

    void AddArc(MyLogFst *in, int stateFrom, int stateTo, int label, float weight) {
      in->AddArc(stateFrom, MyLogArc(label, label, weight, stateTo));
    }
    
    void ArcSort(MyLogFst *in) {
      ArcSort(in, ILabelCompare<MyLogArc>());
    }

    MyLogFst *Compose(MyLogFst *inl, MyLogFst *inr) {
      MyLogFst *out = new MyLogFst();
      ArcSort(inr, ILabelCompare<MyLogArc>());
      fst::Compose(*inl, *inr, out);
      return out;
    }

    void Union(MyLogFst *inl, MyLogFst *inr) {
        fst::Union(inl, *inr);
    }
    
    MyLogFst *Intersect(MyLogFst *inl, MyLogFst *inr) {
      IntersectFstOptions<MyLogArc> nopts;
      nopts.gc_limit = 0;
      // nopts.connect = true;
      // MyIntersectFst<MyLogArc> inter = MyIntersectFst<MyLogArc>(*inl, *inr, nopts);
      IntersectFst<MyLogArc> inter = IntersectFst<MyLogArc>(*inl, *inr, nopts);
      // MyLogFst *out = inter.ToMyFst();
      MyLogFst *out = new MyLogFst(inter);
      Connect(out);
      return out;

      // 
      // MyLogFst *out = new MyLogFst();
      // fst::Intersect(*inl, *inr, out);
      // return out;
    }
    
    MyLogFst *Determinize(MyLogFst *in) {
      MyLogFst *out = new MyLogFst ();
      fst::Determinize(*in, out);
      return out;
    }

    void *Minimize(MyLogFst *in) {
      fst::Minimize(in);
    }

    void Concat(MyLogFst *inl, MyLogFst *inr) {
      fst::Concat(inl, *inr);
    }

    void ProjectInput(MyLogFst *in) {
      fst::Project(in, PROJECT_INPUT);
    }

    void ProjectOutput(MyLogFst *in) {
      fst::Project(in, PROJECT_OUTPUT);
    }

    MyLogFst *ShortestPath(MyLogFst *in) {
      MyLogFst *path = new MyLogFst();
      fst::ShortestPath(*in, path);
      return path;
    }

    int  WriteToFile(MyLogFst *in, char *filename) {
      return in->Write(filename);
    }

    MyLogFst *ReadFromFile(char *filename) {
      return MyLogFst::Read(filename);
    }
}
