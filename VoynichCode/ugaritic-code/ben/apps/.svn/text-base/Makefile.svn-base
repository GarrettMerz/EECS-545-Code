export OCAMLMAKEFILE = OCamlMakefile
export BINDIR = ~/bin
export PACKS = str extlib hashSet num unix # micmatch_pcre hashtbl_ext
export PP = camlp5find $(PACKS)
export BCSUFFIX = .bc
export OCAMLFLAGS = -g


define PROJ_mean_dev
  SOURCES = ../util.ml main_mean_dev.ml
  RESULT = mean_dev
endef
export PROJ_mean_dev
# 
# define PROJ_highest_feat
#   SOURCES = ../util.ml highest_feat.ml
#   RESULT = highest_feat
# endef
# export PROJ_highest_feat
# 
# define PROJ_subsets
#   SOURCES = ../util.ml subsets.ml
#   RESULT = subsets
# endef
# export PROJ_subsets

# define PROJ_lines
#   SOURCES = ../util.ml lines.ml
#   RESULT = lines
# endef
# export PROJ_lines

define PROJ_shuffle
  SOURCES = ../util.ml shuffle.ml
  RESULT = shuffle
endef
export PROJ_shuffle

# define PROJ_split
#   SOURCES = ../util.ml split.ml
#   RESULT = split
# endef
# export PROJ_split
# 
# define PROJ_interleave
#   SOURCES = ../util.ml interleave.ml
#   RESULT = interleave
# endef
# export PROJ_interleave

define PROJ_skip_lines
  SOURCES = ../util.ml skip_lines.ml
  RESULT = skip_lines
endef
export PROJ_skip_lines

ifndef SUBPROJS
  export SUBPROJS = skip_lines mean_dev shuffle
endif

all:	dc nc
install: all
	install -c -m 755 $(SUBPROJS) $(SUBPROJS:=.bc) $(BINDIR)

%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@

