// fstcompile.cc
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
// Creates binary FSTs from simple text format used by AT&T
// (see http://www.research.att.com/projects/mohri/fsm/doc4/fsm.5.html).

#include "fst/bin/compile-main.h"

namespace fst {

// Register templated main for common arcs types
REGISTER_FST_MAIN(CompileMain, StdArc);
REGISTER_FST_MAIN(CompileMain, LogArc);

}  // namespace fst


using fst::CallFstMain;

int main(int argc, char **argv) {
  string usage = "Creates binary FSTs from simple text format.\n\n  Usage: ";
  usage += argv[0];
  usage += " [text.fst [binary.fst]]\n";
  usage += "  Flags: acceptor, arc_type, fst_type, isymbols, keep_isymbols\n";
  usage += "  keep_osymbols, keep_state_numbering, osymbols, ssymbols\n";
  InitFst(usage.c_str(), &argc, &argv, true);
  if (argc > 3) {
    ShowUsage();
    return 1;
  }
  // Invokes CompileMain<Arc> where arc type is determined from the flag.
  return CallFstMain("CompileMain", argc, argv, FLAGS_arc_type);
}
