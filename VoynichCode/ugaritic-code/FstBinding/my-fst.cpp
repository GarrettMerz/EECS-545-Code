#include "/usr/local/include/fst/fstlib.h"
#include "/usr/local/include/fst/fst.h"
#include "/usr/local/include/fst/vector-fst.h"
#include <vector>
#include <math.h>
#include <stdio.h>
#include "MersenneTwister.h"

#ifndef FST_LIB_MYFST_H__
#define FST_LIB_MYFST_H__


namespace fst {
  
template <class A> class MyFst;
template <class F, class G> void Cast(const F &, F *);

template <class A>
struct MyState {
  typedef A Arc;
  typedef typename A::Weight Weight;
  typedef typename A::StateId StateId;
  
  MyState() : final(Weight::Zero()), beta(NAN), best(NAN), besti(-2), edit(-1), niepsilons(0), noepsilons(0) {}
  
  Weight final;         // Final weight
  vector<A> arcs;       // Arcs representation
  double beta;           // log prob of paths from here to end
  double best;           // log prob of best path from here to end 
  int besti;            // index of arc along best path to end
  int edit;
  size_t niepsilons;    // # of input epsilons
  size_t noepsilons;    // # of output epsilons
};

// This is an almost exact copy of the VectorFstBaseImpl in vector-fst.h
// The only changes are to replace VectorState<A> throughout with MyState<A> 
// Added a function 'PrepareBetas'
template <class A>
class MyFstImpl : public VectorFstBaseImpl< MyState<A> > {
 public:
  using FstImpl<A>::SetType;
  using FstImpl<A>::SetProperties;
  using FstImpl<A>::Properties;
  using FstImpl<A>::WriteHeader;

  using VectorFstBaseImpl<MyState<A> >::Start;
  using VectorFstBaseImpl<MyState<A> >::NumStates;

  friend class MutableArcIterator<MyFst<A> >;

  typedef VectorFstBaseImpl< MyState<A> > BaseImpl;
  typedef typename A::Weight Weight;
  typedef typename A::StateId StateId;

  public : MyFstImpl() {
    SetType("my");
    SetProperties(kNullProperties | kStaticProperties);
  };
  explicit MyFstImpl(const Fst<A> &fst);
  explicit MyFstImpl(const ComposeFst<A> &fst);

  static MyFstImpl<A> *Read(istream &strm, const FstReadOptions &opts);

  size_t NumInputEpsilons(StateId s) const { return GetState(s)->niepsilons; }

  size_t NumOutputEpsilons(StateId s) const { return GetState(s)->noepsilons; }

  bool Write(ostream &strm, const FstWriteOptions &opts) const;
  
  
  // void SetBetas() {
  //   for (StateIterator< MyFst<A> > siter(this);
  //         !siter.Done(); 
  //         siter.Next()) {
  //     StateId s = siter.Value();
  //     MyState<A> *state = GetState(s);
  //     int num_arcs = state->arcs.size();
  //     state->betas.clear();
  //     state->betas.resize(num_arcs, *( float* ) nan);
  //   }
  // }      

  void SetStart(StateId s) {
    BaseImpl::SetStart(s);
    SetProperties(Properties() & kSetStartProperties);
    if (Properties() & kAcyclic)
      SetProperties(Properties() | kInitialAcyclic);
  }

  void SetFinal(StateId s, Weight w) {
    Weight ow = Final(s);
    if (ow != Weight::Zero() && ow != Weight::One())
      SetProperties(Properties() & ~kWeighted);
    BaseImpl::SetFinal(s, w);
    if (w != Weight::Zero() && w != Weight::One()) {
      SetProperties(Properties() | kWeighted);
      SetProperties(Properties() & ~kUnweighted);
    }
    SetProperties(Properties() &
                  (kSetFinalProperties | kWeighted | kUnweighted));
  }

  StateId AddState() {
    StateId s = BaseImpl::AddState();
    SetProperties(Properties() & kAddStateProperties);
    return s;
  }
  
  StateId AddState(int ed) {
    StateId s = BaseImpl::AddState();
    GetState(s)->edit = ed;
    SetProperties(Properties() & kAddStateProperties);
    return s;
  }

  void AddArc(StateId s, const A &arc) {
    MyState<A> *state = GetState(s);
    if (arc.ilabel != arc.olabel) {
      SetProperties(Properties() | kNotAcceptor);
      SetProperties(Properties() & ~kAcceptor);
    }
    if (arc.ilabel == 0) {
      ++state->niepsilons;
      SetProperties(Properties() | kIEpsilons);
      SetProperties(Properties() & ~kNoIEpsilons);
      if (arc.olabel == 0) {
        SetProperties(Properties() | kEpsilons);
        SetProperties(Properties() & ~kNoEpsilons);
      }
    }
    if (arc.olabel == 0) {
      ++state->noepsilons;
      SetProperties(Properties() | kOEpsilons);
      SetProperties(Properties() & ~kNoOEpsilons);
    }
    if (!state->arcs.empty()) {
      A &parc = state->arcs.back();
      if (parc.ilabel > arc.ilabel) {
        SetProperties(Properties() | kNotILabelSorted);
        SetProperties(Properties() & ~kILabelSorted);
      }
      if (parc.olabel > arc.olabel) {
        SetProperties(Properties() | kNotOLabelSorted);
        SetProperties(Properties() & ~kOLabelSorted);
      }
    }
    if (arc.weight != Weight::Zero() && arc.weight != Weight::One()) {
      SetProperties(Properties() | kWeighted);
      SetProperties(Properties() & ~kUnweighted);
    }
    if (arc.nextstate <= s) {
      SetProperties(Properties() | kNotTopSorted);
      SetProperties(Properties() & ~kTopSorted);
    }
    SetProperties(Properties() &
                  (kAddArcProperties | kAcceptor |
                   kNoEpsilons | kNoIEpsilons | kNoOEpsilons |
                   kILabelSorted | kOLabelSorted | kUnweighted | kTopSorted));
    if (Properties() & kTopSorted)
      SetProperties(Properties() | kAcyclic | kInitialAcyclic);
    BaseImpl::AddArc(s, arc);
  }

  void DeleteStates(const vector<StateId> &dstates) {
    BaseImpl::DeleteStates(dstates);
    SetProperties(Properties() & kDeleteStatesProperties);
  }

  void DeleteStates() {
    BaseImpl::DeleteStates();
    SetProperties(kNullProperties | kStaticProperties);
  }

  void DeleteArcs(StateId s, size_t n) {
    const vector<A> &arcs = GetState(s)->arcs;
    for (size_t i = 0; i < n; ++i) {
      size_t j = arcs.size() - i - 1;
      if (arcs[j].ilabel == 0)
        --GetState(s)->niepsilons;
      if (arcs[j].olabel == 0)
        --GetState(s)->noepsilons;
    }
    BaseImpl::DeleteArcs(s, n);
    SetProperties(Properties() & kDeleteArcsProperties);
  }

  void DeleteArcs(StateId s) {
    GetState(s)->niepsilons = 0;
    GetState(s)->noepsilons = 0;
    BaseImpl::DeleteArcs(s);
    SetProperties(Properties() & kDeleteArcsProperties);
  }

 private:
  // Properties always true of this Fst class
  static const uint64 kStaticProperties = kExpanded | kMutable;
  // Current file format version
  static const int kFileVersion = 2;
  // Minimum file format version supported
  static const int kMinFileVersion = 1;

  DISALLOW_COPY_AND_ASSIGN(MyFstImpl);
};

template <class A>
MyFstImpl<A>::MyFstImpl(const Fst<A> &fst) {
  SetType("my");
  SetProperties(fst.Properties(kCopyProperties, false) | kStaticProperties);
  SetInputSymbols(fst.InputSymbols());
  SetOutputSymbols(fst.OutputSymbols());
  BaseImpl::SetStart(fst.Start());

  for (StateIterator< Fst<A> > siter(fst);
       !siter.Done();
       siter.Next()) {
    StateId s = siter.Value();
    BaseImpl::AddState();
    BaseImpl::SetFinal(s, fst.Final(s));
    ReserveArcs(s, fst.NumArcs(s));
    for (ArcIterator< Fst<A> > aiter(fst, s);
         !aiter.Done();
         aiter.Next()) {
      const A &arc = aiter.Value();
      BaseImpl::AddArc(s, arc);
      if (arc.ilabel == 0)
        ++GetState(s)->niepsilons;
      if (arc.olabel == 0)
        ++GetState(s)->noepsilons;
    }
  }
}

template <class A>
MyFstImpl<A>::MyFstImpl(const ComposeFst<A> &fst) {
  
  typedef typename A::StateId StateId;
  typedef typename SequenceComposeFilter<A>::FilterState FilterState;
  typedef ComposeStateTuple<StateId, FilterState> StateTuple;
  
  SetType("my");
  SetProperties(fst.Properties(kCopyProperties, false) | kStaticProperties);
  SetInputSymbols(fst.InputSymbols());
  SetOutputSymbols(fst.OutputSymbols());
  BaseImpl::SetStart(fst.Start());

  for (StateIterator< ComposeFst<A> > siter(fst);
       !siter.Done();
       siter.Next()) {
    StateId s = siter.Value();
    BaseImpl::AddState();
    StateTuple tuple = siter.GetTuple();
    StateId s1 = tuple.state_id1;
    StateId s2 = tuple.state_id2;
    MyFst<A>* fst1 = (MyFst<A>*) fst.impl_->fst1_;
    GetState(s)->edit = fst1->impl_->GetState(s1)->edit;
    BaseImpl::SetFinal(s, fst.Final(s));
    ReserveArcs(s, fst.NumArcs(s));
    for (ArcIterator< Fst<A> > aiter(fst, s);
         !aiter.Done();
         aiter.Next()) {
      const A &arc = aiter.Value();
      BaseImpl::AddArc(s, arc);
      if (arc.ilabel == 0)
        ++GetState(s)->niepsilons;
      if (arc.olabel == 0)
        ++GetState(s)->noepsilons;
    }
  }
}


// Again, copied VectorFst from vector-fst.h, changing VectorFstImpl<A> to MyFstImpl<A>
template <class A>
class MyFst : public MutableFst<A> {
 public:
  friend class StateIterator< MyFst<A> >;
  friend class ArcIterator< MyFst<A> >;
  friend class MutableArcIterator< MyFst<A> >;
  template <class F, class G> void friend Cast(const F &, G *);

  typedef A Arc;
  typedef typename A::Weight Weight;
  typedef typename A::StateId StateId;
  typedef MyFstImpl<A> Impl;

  MyFst() : impl_(new MyFstImpl<A>) {}
  MyFst(const MTRand &rng) : impl_(new MyFstImpl<A>), rng(rng) {}

  MyFst(const MyFst<A> &fst) : impl_(fst.impl_) {
    impl_->IncrRefCount();
  }
  explicit MyFst(const Fst<A> &fst) : impl_(new MyFstImpl<A>(fst)) {}
  explicit MyFst(const ComposeFst<A> &fst) : impl_(new MyFstImpl<A>(fst)) {}
  
  virtual ~MyFst() { if (!impl_->DecrRefCount()) delete impl_; }

  MyFst<A> &operator=(const MyFst<A> &fst) {
    if (this != &fst) {
      if (!impl_->DecrRefCount()) delete impl_;
      fst.impl_->IncrRefCount();
      impl_ = fst.impl_;
    }
    return *this;
  }

  virtual MyFst<A> &operator=(const Fst<A> &fst) {
    if (this != &fst) {
      if (!impl_->DecrRefCount()) delete impl_;
      impl_ = new MyFstImpl<A>(fst);
    }
    return *this;
  }

  virtual MyFst<A> &operator=(const ComposeFst<A> &fst) {
    if (!impl_->DecrRefCount()) delete impl_;
    impl_ = new MyFstImpl<A>(fst);
    return *this;
  }

  void Normalize() {
    for (StateIterator< MyFst<A> > siter(*this);
          !siter.Done();
          siter.Next()) {
      StateId s = siter.Value();
      double sum = exp(-this->Final(s).Value());
      for (ArcIterator< MyFst<A> > aiter(*this, s);
            !aiter.Done();
            aiter.Next()) {
        const A &arc = aiter.Value();
        sum += exp(-arc.weight.Value());
      }
      double log_sum = log(sum);
      SetFinal(s, this->Final(s).Value() + log_sum);
      for (MutableArcIterator< MyFst<A> > aiter(this, s);
            !aiter.Done();
            aiter.Next()) {
        A arc = aiter.Value();
        arc.weight = Weight(arc.weight.Value() + log_sum);
        aiter.SetValue(arc);
      
      }
    }
  }
  
  int GetEdit(StateId s) {
    return (impl_->GetState(s)->edit);
  }
  
  vector<int> Sample() {
    vector<int> res;
    SampleAux(this->Start(), res);
    res.push_back(0);
    return res;
  }
  private : void SampleAux(StateId s, vector<int> &res) {
    double sum = 0.0;
    for (ArcIterator< MyFst<A> > aiter(*this, s);
          !aiter.Done();
          aiter.Next()) {
      const A &arc = aiter.Value();
      double beta = impl_->GetState(arc.nextstate)->beta;
      sum += exp(-(arc.weight.Value() + beta));
    }
    sum += exp(-this->Final(s).Value());

    double r = this->rng.rand(sum);
    double p = 0.0;
    for (ArcIterator< MyFst<A> > aiter(*this, s);
          !aiter.Done();
          aiter.Next()) {
      const A &arc = aiter.Value();
      double beta = impl_->GetState(arc.nextstate)->beta;
      p += exp(-(arc.weight.Value() + beta));
      if (p > r) {
        res.push_back(impl_->GetState(arc.nextstate)->edit);
        SampleAux(arc.nextstate, res);
        break;
      }
    }
    // we sampled the final state so don't do anything more...
  }
  
  public : void Reweight(double (*EditProb) (int)) {
    MutateCheck();
    for (StateIterator< MyFst<A> > siter(*this);
          !siter.Done();
          siter.Next()) {
      StateId s = siter.Value();
      MyState<A> *state = impl_->GetState(s);
      state->beta = NAN;
      state->best = NAN;
      state->besti = -2;
      for (int i = 0; i++; i < state->arcs.size()) {
        A arc = state->arcs[i];
        MyState<A> *nstate = impl_->GetState(arc.nextstate);
        int edit = nstate->edit;
        double weight = (*EditProb) (edit);
        arc.weight = Weight(weight);
      }
      
      // for (ArcIterator< MyFst<A> > aiter(*this, s);
      //       !aiter.Done();
      //       aiter.Next()) {
      //   const A &arc         = aiter.Value();
      //   int edit      = impl_->GetState(arc.nextstate)->edit;
      //   double weight = (*EditProb) (edit);
      //   arc.weight    = Weight(weight);
      // }
    }   
  }
  
  public : double GetBeta(StateId s) {
    MyState<A> *state = impl_->GetState(s);
    if (isnan(state->beta)) {
      double sum = exp(-this->Final(s).Value());
      for (ArcIterator< MyFst<A> > aiter(*this, s); 
          !aiter.Done(); 
          aiter.Next()) {
        const A &arc = aiter.Value();
        double prod = arc.weight.Value() + GetBeta(arc.nextstate);
        sum += exp(-prod);
      }
      double log_sum = -log(sum);
      state->beta = log_sum;
    }
    return state->beta;
  }

  public : double GetBest(StateId s) {
    MyState<A> *state = impl_->GetState(s);
    if (isnan(state->best)) {
      double best = this->Final(s).Value();
      int besti = -1;
      int i = 0;
      for (ArcIterator< MyFst<A> > aiter(*this, s);
          !aiter.Done();
          aiter.Next()) {
        const A &arc = aiter.Value();
        double prod = arc.weight.Value() + GetBest(arc.nextstate);
        if (prod < best) {
          best = prod;
          besti = i;
        }
        i++;
      }
      state->besti = besti;
      state->best = best;
    }
    return state->best;
  }
  
  private : void BestAux(StateId s, vector<int> &res) {
    int besti = impl_->GetState(s)->besti;
    if (besti != -1) {
      const A &arc = impl_->GetState(s)->arcs[besti];
      res.push_back(impl_->GetState(arc.nextstate)->edit);
      BestAux(arc.nextstate, res);
    }
  } 
  
  public : vector<int> GetBestEdits() {
    StateId s = this->Start();
    double throwaway = this->GetBest(s);
    vector<int> res;
    BestAux(s, res);
    res.push_back(0);
    return res;
  }

  virtual StateId Start() const { return impl_->Start(); }

  virtual Weight Final(StateId s) const { return impl_->Final(s); }

  virtual StateId NumStates() const { return impl_->NumStates(); }

  virtual size_t NumArcs(StateId s) const { return impl_->NumArcs(s); }

  virtual size_t NumInputEpsilons(StateId s) const {
    return impl_->NumInputEpsilons(s);
  }

  virtual size_t NumOutputEpsilons(StateId s) const {
    return impl_->NumOutputEpsilons(s);
  }

  virtual uint64 Properties(uint64 mask, bool test) const {
    if (test) {
      uint64 known, test = TestProperties(*this, mask, &known);
      impl_->SetProperties(test, known);
      return test & mask;
    } else {
      return impl_->Properties(mask);
    }
  }

  virtual const string& Type() const { return impl_->Type(); }

  // Get a copy of this MyFst
  virtual MyFst<A> *Copy(bool reset = false) const {
    impl_->IncrRefCount();
    return new MyFst<A>(impl_);
  }

  // Read a MyFst from an input stream; return NULL on error DELEGATING to VectorFst::Read
  static MyFst<A> *Read(istream &strm, const FstReadOptions &opts) {
    VectorFst<A> *fst = VectorFst<A>::Read(strm, opts);
    MyFst<A> *res = fst ? new MyFst<A>(*fst) : 0;
    delete fst;
    return res;
  }

  // Read a MyFst from a file; return NULL on error
  // Empty filename reads from standard input
  static MyFst<A> *Read(const string &filename) {
    if (!filename.empty()) {
      ifstream strm(filename.c_str(), ifstream::in | ifstream::binary);
      if (!strm) {
        LOG(ERROR) << "MyFst::Read: Can't open file: " << filename;
        return 0;
      }
      return Read(strm, FstReadOptions(filename));
    } else {
      return Read(std::cin, FstReadOptions("standard input"));
    }
  }

  // Write a MyFst to an output stream; return false on error DELEGATING TO Vecto
  virtual bool Write(ostream &strm, const FstWriteOptions &opts) const {
    VectorFst<A> *fst = new VectorFst<A>(*this);
    bool res = fst->Write(strm, opts);
    delete fst;
    return res;
  }

  // Write a MyFst to a file; return false on error
  // Empty filename writes to standard output
  virtual bool Write(const string &filename) const {
    if (!filename.empty()) {
      ofstream strm(filename.c_str(), ofstream::out | ofstream::binary);
      if (!strm) {
        LOG(ERROR) << "MyFst::Write: Can't open file: " << filename;
        return false;
      }
      return Write(strm, FstWriteOptions(filename));
    } else {
      return Write(std::cout, FstWriteOptions("standard output"));
    }
  }

  virtual SymbolTable* InputSymbols() {
    return impl_->InputSymbols();
  }

  virtual SymbolTable* OutputSymbols() {
    return impl_->OutputSymbols();
  }

  virtual const SymbolTable* InputSymbols() const {
    return impl_->InputSymbols();
  }

  virtual const SymbolTable* OutputSymbols() const {
    return impl_->OutputSymbols();
  }

  virtual void SetStart(StateId s) {
    MutateCheck();
    impl_->SetStart(s);
  }

  virtual void SetFinal(StateId s, Weight w) {
    MutateCheck();
    impl_->SetFinal(s, w);
  }

  virtual void SetProperties(uint64 props, uint64 mask) {
    impl_->SetProperties(props, mask);
  }

  virtual StateId AddState() {
    MutateCheck();
    return impl_->AddState();
  }

  virtual StateId AddState(int ed) {
    MutateCheck();
    return impl_->AddState(ed);
  }


  virtual void AddArc(StateId s, const A &arc) {
    MutateCheck();
    impl_->AddArc(s, arc);
  }

  virtual void DeleteStates(const vector<StateId> &dstates) {
    MutateCheck();
    impl_->DeleteStates(dstates);
  }

  virtual void DeleteStates() {
    MutateCheck();
    impl_->DeleteStates();
  }

  virtual void DeleteArcs(StateId s, size_t n) {
    MutateCheck();
    impl_->DeleteArcs(s, n);
  }

  virtual void DeleteArcs(StateId s) {
    MutateCheck();
    impl_->DeleteArcs(s);
  }

  virtual void SetInputSymbols(const SymbolTable* isyms) {
    MutateCheck();
    impl_->SetInputSymbols(isyms);
  }

  virtual void SetOutputSymbols(const SymbolTable* osyms) {
    MutateCheck();
    impl_->SetOutputSymbols(osyms);
  }

  void ReserveStates(StateId n) {
    MutateCheck();
    impl_->ReserveStates(n);
  }

  void ReserveArcs(StateId s, size_t n) {
    MutateCheck();
    impl_->ReserveArcs(s, n);
  }

  virtual void InitStateIterator(StateIteratorData<A> *data) const {
    impl_->InitStateIterator(data);
  }

  virtual void InitArcIterator(StateId s, ArcIteratorData<A> *data) const {
    impl_->InitArcIterator(s, data);
  }

  virtual inline
  void InitMutableArcIterator(StateId s, MutableArcIteratorData<A> *);

 private:
  explicit MyFst(MyFstImpl<A> *impl) : impl_(impl) {}

  void MutateCheck() {
    // Copy on write
    if (impl_->RefCount() > 1) {
      impl_->DecrRefCount();
      impl_ = new MyFstImpl<A>(*this);
    }
  }

 public:
  MyFstImpl<A> *impl_;  // FST's impl
  MTRand rng;
};

// Specialization for MyFst; see generic version in fst.h
// for sample usage (but use the MyFst type!). This version
// should inline.
template <class A>
class StateIterator< MyFst<A> > {
 public:
  typedef typename A::StateId StateId;

  explicit StateIterator(const MyFst<A> &fst)
    : nstates_(fst.impl_->NumStates()), s_(0) {}

  bool Done() const { return s_ >= nstates_; }

  StateId Value() const { return s_; }

  void Next() { ++s_; }

  void Reset() { s_ = 0; }

 private:
  StateId nstates_;
  StateId s_;

  DISALLOW_COPY_AND_ASSIGN(StateIterator);
};

// Specialization for MyFst; see generic version in fst.h
// for sample usage (but use the MyFst type!). This version
// should inline.
template <class A>
class ArcIterator< MyFst<A> > {
 public:
  typedef typename A::StateId StateId;

  ArcIterator(const MyFst<A> &fst, StateId s)
    : arcs_(fst.impl_->GetState(s)->arcs), i_(0) {}

  bool Done() const { return i_ >= arcs_.size(); }

  const A& Value() const { return arcs_[i_]; }

  void Next() { ++i_; }

  void Reset() { i_ = 0; }

  void Seek(size_t a) { i_ = a; }

  size_t Position() const { return i_; }

 private:
  const vector<A>& arcs_;
  size_t i_;

  DISALLOW_COPY_AND_ASSIGN(ArcIterator);
};

// Specialization for MyFst; see generic version in fst.h
// for sample usage (but use the MyFst type!). This version
// should inline.
template <class A>
class MutableArcIterator< MyFst<A> > : public MutableArcIteratorBase<A> {
 public:
  typedef typename A::StateId StateId;
  typedef typename A::Weight Weight;

  MutableArcIterator(MyFst<A> *fst, StateId s) : i_(0) {
    fst->MutateCheck();
    state_ = fst->impl_->GetState(s);
    properties_ = &fst->impl_->properties_;
  }

  bool Done() const { return i_ >= state_->arcs.size(); }

  const A& Value() const { return state_->arcs[i_]; }

  void Next() { ++i_; }

  size_t Position() const { return i_; }

  void Reset() { i_ = 0; }

  void Seek(size_t a) { i_ = a; }

  void SetValue(const A &arc) {
    A& oarc = state_->arcs[i_];
    if (oarc.ilabel != oarc.olabel)
      *properties_ &= ~kNotAcceptor;
    if (oarc.ilabel == 0) {
      --state_->niepsilons;
      *properties_ &= ~kIEpsilons;
      if (oarc.olabel == 0)
        *properties_ &= ~kEpsilons;
    }
    if (oarc.olabel == 0) {
      --state_->noepsilons;
      *properties_ &= ~kOEpsilons;
    }
    if (oarc.weight != Weight::Zero() && oarc.weight != Weight::One())
      *properties_ &= ~kWeighted;
    oarc = arc;
    if (arc.ilabel != arc.olabel)
      *properties_ |= kNotAcceptor;
    if (arc.ilabel == 0) {
      ++state_->niepsilons;
      *properties_ |= kIEpsilons;
      if (arc.olabel == 0)
        *properties_ |= kEpsilons;
    }
    if (arc.olabel == 0) {
      ++state_->noepsilons;
      *properties_ |= kOEpsilons;
    }
    if (arc.weight != Weight::Zero() && arc.weight != Weight::One())
      *properties_ |= kWeighted;
    *properties_ &= kSetArcProperties | kNotAcceptor |
                    kEpsilons | kIEpsilons | kOEpsilons | kWeighted;
  }

 private:
  virtual bool Done_() const { return Done(); }
  virtual const A& Value_() const { return Value(); }
  virtual void Next_() { Next(); }
  virtual size_t Position_() const { return Position(); }
  virtual void Reset_() { Reset(); }
  virtual void Seek_(size_t a) { Seek(a); }
  virtual void SetValue_(const A &a) { SetValue(a); }

  struct MyState<A> *state_;
  uint64 *properties_;
  size_t i_;

  DISALLOW_COPY_AND_ASSIGN(MutableArcIterator);
};

// Provide information needed for the generic mutable arc iterator
template <class A> inline
void MyFst<A>::InitMutableArcIterator(StateId s, MutableArcIteratorData<A> *data) {
  data->base = new MutableArcIterator< MyFst<A> >(this, s);
}


// useful aliases 
typedef LogWeightTpl<double> MyLogWeight;
typedef ArcTpl<MyLogWeight> MyLogArc;
typedef MyFst<MyLogArc> MyLogFst;

}

#endif
