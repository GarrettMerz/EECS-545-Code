#pragma once

struct Fst;
struct Rng;
struct Arc;

void TestFstLib    (char *filename);
struct Rng* NewRng (int seed);
struct Fst* NewFst (struct Rng *rng);
void FinalizeFst   (struct Fst *in);
void FinalizeRng   (struct Rng *rng);
int  AddState      (struct Fst *in, int edit);
void SetFinal      (struct Fst *in, int state, float weight);
void SetStart      (struct Fst *in, int state);
int GetStart       (struct Fst *in);
double GetBeta     (struct Fst *in, int state);
double GetBest     (struct Fst *in, int state);
void Normalize     (struct Fst *in);
void AddArc        (struct Fst *in, int stateFrom, int stateTo, int label, float weight);
void ArcSort       (struct Fst *in);
struct Fst *Compose     (struct Fst *inl, struct Fst *inr);
void Union              (struct Fst *inl, struct Fst *inr);
void Minimize           (struct Fst *in);
struct Fst *Determinize (struct Fst *in);
struct Fst *Intersect   (struct Fst *inl, struct Fst *inr);
void Concat             (struct Fst *inl, struct Fst *inr);
void ProjectInput       (struct Fst *in);
void ProjectOutput      (struct Fst *in);
struct Fst *ShortestPath  (struct Fst *in);
int  WriteToFile          (struct Fst *in, char *filename);
struct Fst *ReadFromFile  (char *filename);
int ** GetAllEdits      (struct Fst *in, int **lengths, int *size);
int * Sample              (struct Fst *in, int *size);
int * GetBestEdits        (struct Fst *in, int *size); 
double Random             (struct Rng *rng, double max);
void ReweightFst          (struct Fst *fst, double (*pt2EditProb) (int)); 