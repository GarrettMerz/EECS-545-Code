#include "wrapperOcamlFromC.h"
#include "wrapperCFromCpp.h"
#include <stdio.h>
#include <math.h>

static struct custom_operations fst_operations = {
    "www.dmwit.com.open_fst",
    finalizeFst,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

static struct custom_operations rng_operations = {
    "www.dmwit.com.open_fst",
    finalizeRng,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};


CAMLprim value testFstLib(value filename) {
    CAMLparam1(filename);
    TestFstLib(String_val(filename));
    CAMLreturn(Val_unit);
}
CAMLprim value convert_int_array(int *a, int length) {
    CAMLparam0();
    CAMLlocal1(res);
    res = caml_alloc(length, 0);
    int i;
    for (i = 0; i < length; i++) {
      Store_field(res, i, Val_int(a[i]));
    }
    CAMLreturn(res);
}

CAMLprim value sample(value fst) {
    CAMLparam1(fst);
    int *res_;
    int size;
    res_ = Sample(fstValue(fst), &size);
    CAMLlocal1(res);
    res = caml_alloc(size, 0);
    int i;
    for (i = 0; i < size; i++) {
      Store_field(res, i, Val_int(res_[i]));
    }
    free(res_);
    CAMLreturn(res);
}

CAMLprim value getBestEdits(value fst) {
    CAMLparam1(fst);
    int *res_;
    int size;
    res_ = GetBestEdits(fstValue(fst), &size);
    CAMLlocal1(res);
    res = caml_alloc(size, 0);
    int i;
    for (i = 0; i < size; i++) {
      Store_field(res, i, Val_int(res_[i]));
    }
    free(res_);
    CAMLreturn(res);
}

CAMLprim value getAllEdits(value fst) {
    CAMLparam1(fst);
    int *lengths;
    int size;
    int **strings = GetAllEdits(fstValue(fst), &lengths, &size);
    CAMLlocal1(res);
    res = caml_alloc(size, 0);
    int i;
    for (i = 0; i < size; i++) {
      Store_field(res, i, convert_int_array(strings[i], lengths[i]));
    };
    for (i = 0; i < size; i++) {
      free(strings[i]);
    }
    free(strings);
    free(lengths);
    CAMLreturn(res);
}

CAMLprim value newFst(value rng) {
    CAMLparam1(rng);
    CAMLlocal2(fstBlock, pairBlock);
    fstBlock = caml_alloc_custom(&fst_operations, sizeof(struct Fst **), 1, 1000);
    fstValue(fstBlock) = NewFst(rngValue(rng));
    pairBlock = caml_alloc_tuple(2);
    Store_field(pairBlock, 0, fstBlock);
    Store_field(pairBlock, 1, Val_int(0));
    CAMLreturn(pairBlock);
}

CAMLprim value newRng(value seed) {
    CAMLparam1(seed);
    CAMLlocal1(rngBlock);
    rngBlock = caml_alloc_custom(&rng_operations, sizeof(struct Rng **), 1, 1000);
    rngValue(rngBlock) = NewRng(Int_val(seed));
    CAMLreturn(rngBlock);
}

CAMLprim value rand_float(value rng, value max) {
    double rn = Random(rngValue(rng), Double_val(max));
    value res = caml_copy_double(rn);
    return res;
}

void finalizeFst(value fst) {
    FinalizeFst(fstValue(fst));
}

void finalizeRng(value rng) {
    FinalizeRng(rngValue(rng));
}
  
double getEditProb(int n) {
    static value * closure_f = NULL;
    if (closure_f == NULL) {
      /* First time around, look up by name */
      closure_f = caml_named_value("edit_prob");
    }
    value res = caml_callback(*closure_f, Val_int(n));
    return Double_val(res);
}
CAMLprim value reweightFst(value fst) {
    CAMLparam1(fst);
    ReweightFst(fstValue(fst), &getEditProb);
    CAMLreturn(Val_unit);
}
CAMLprim value getBeta(value fst, value state) {
    CAMLparam2(fst, state);
    double beta = GetBeta(fstValue(fst), Int_val(state));
    value res = caml_copy_double(beta);
    CAMLreturn(res);
}

CAMLprim value getBest(value fst, value state) {
    CAMLparam2(fst, state);
    double best = GetBest(fstValue(fst), Int_val(state));
    value res = caml_copy_double(best);
    CAMLreturn(res);
}

CAMLprim value addState(value fst, value edit) {
    CAMLparam2(fst, edit);
    CAMLlocal1(state);
    state = Val_int(AddState(fstValue(fst), Int_val(edit)));
    CAMLreturn(state);
}

CAMLprim value normalize(value fst) {
    CAMLparam1(fst);
    Normalize(fstValue(fst));
    CAMLreturn(Val_unit);
}

CAMLprim value arcSort(value fst) {
    CAMLparam1(fst);
    ArcSort(fstValue(fst));
    CAMLreturn(fst);
}

CAMLprim value setFinal(value fst, value state, value weight) {
    CAMLparam2(fst, state);
    SetFinal(fstValue(fst), Int_val(state), Double_val(weight));
    CAMLreturn(Val_unit);
}

CAMLprim value setStart(value fst, value state) {
   CAMLparam2(fst, state);
   SetStart(fstValue(fst), Int_val(state));
   CAMLreturn(Val_unit);
}

CAMLprim value getStart(value fst) {
  CAMLparam1(fst);
  int start = GetStart(fstValue(fst));
  CAMLreturn(Val_int(start));
}

CAMLprim value addArc(value fst, value stateFrom, value stateTo, value label, value weight) {
    CAMLparam4(fst, stateFrom, stateTo, label);
    AddArc(fstValue(fst), Int_val(stateFrom), Int_val(stateTo), Int_val(label), Double_val(weight));
    CAMLreturn(Val_unit);
}

CAMLprim value compose(value fstl, value fstr) {
    CAMLparam2(fstl, fstr);
    CAMLlocal1(fstBlock);
    fstBlock = caml_alloc_custom(&fst_operations, sizeof(struct Fst **), 1, 1000);
    fstValue(fstBlock) = Compose(fstValue(fstl), fstValue(fstr));
    CAMLreturn(fstBlock);
}

CAMLprim value union_(value fstl, value fstr) {
    CAMLparam2(fstl, fstr);
    Union(fstValue(fstl), fstValue(fstr));
    CAMLreturn(fstl);
}
CAMLprim value minimize(value fst) {
    CAMLparam1(fst);
    Minimize(fstValue(fst));
    CAMLreturn(fst);
}
CAMLprim value determinize(value fst) {
    CAMLparam1(fst);
    CAMLlocal1(fstBlock);
    fstBlock = caml_alloc_custom(&fst_operations, sizeof(struct Fst **), 1, 1000);
    fstValue(fstBlock) = Determinize(fstValue(fst));
    CAMLreturn(fstBlock);
}   
CAMLprim value intersect_(value fstl, value fstr) {
    CAMLparam2(fstl, fstr);
    CAMLlocal1(fstBlock);
    fstBlock = caml_alloc_custom(&fst_operations, sizeof(struct Fst **), 1, 1000); 
    fstValue(fstBlock) = Intersect(fstValue(fstl), fstValue(fstr));
    CAMLreturn(fstBlock);
}

CAMLprim value concat(value fstl, value fstr) {
    CAMLparam2(fstl, fstr);
    Concat(fstValue(fstl), fstValue(fstr));
    CAMLreturn(fstl);
}

CAMLprim value projectInput(value fst) {
    CAMLparam1(fst);
    ProjectInput(fstValue(fst));
    CAMLreturn(fst);
}

CAMLprim value projectOutput(value fst) {
    CAMLparam1(fst);
    ProjectOutput(fstValue(fst));
    CAMLreturn(fst);
}

CAMLprim value shortestPath(value fstIn) {
    CAMLparam1(fstIn);
    CAMLlocal1(fstOut);
    fstOut = caml_alloc_custom(&fst_operations, sizeof(struct Fst **), 1, 1000);
    fstValue(fstOut) = ShortestPath(fstValue(fstIn));
    CAMLreturn(fstOut);
}

/* for now, we don't worry about filenames with '\0' in them; let
 * things fail catastrophically for people who try to do something
 * as stupid as that */
CAMLprim value writeToFile(value fst, value filename) {
    CAMLparam2(fst, filename);
    CAMLreturn(Val_bool(WriteToFile(fstValue(fst), String_val(filename))));
}

CAMLprim value readFromFile(value filename) {
    CAMLparam1(filename);
    CAMLlocal1(fstBlock);
    fstBlock = caml_alloc_custom(&fst_operations, sizeof(struct Fst **), 1, 1000);
    fstValue(fstBlock) = ReadFromFile(String_val(filename));
    CAMLreturn(fstBlock);
}

