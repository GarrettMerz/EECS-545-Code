// compose-main.h
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// Author: riley@google.com (Michael Riley)
//
// \file
// Composes two FSTs. Includes helper function for fstcompose.cc
// that templates the main on the arc type to support multiple and
// extensible arc types.

#ifndef FST_COMPOSE_MAIN_H__
#define FST_COMPOSE_MAIN_H__

#include "fst/bin/main.h"
#include "fst/lib/compose.h"
#include "fst/lib/vector-fst.h"

DECLARE_bool(connect);

namespace fst {

// Main function for fstcompose templated on the arc type.
template <class Arc>
int ComposeMain(int argc, char **argv, istream &strm,
                const FstReadOptions &opts) {
  Fst<Arc> *ifst1 = Fst<Arc>::Read(strm, opts);
  if (!ifst1) return 1;

  Fst<Arc> *ifst2;
  if (strcmp(argv[2], "-"))
    ifst2 = Fst<Arc>::Read(argv[2]);
  else
    ifst2 = Fst<Arc>::Read(std::cin, FstReadOptions());
  if (!ifst2) return 1;

  ComposeOptions copts(FLAGS_connect);
  VectorFst<Arc> ofst;
  Compose(*ifst1, *ifst2, &ofst, copts);
  ofst.Write(argc > 3 ? argv[3] : "");

  return 0;
}

}  // namespace fst

#endif  // FST_COMPOSE_MAIN_H__