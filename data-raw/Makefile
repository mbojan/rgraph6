rscript:=Rscript
rscript_flags:=
geng:=nauty-geng

# Check GNU make version
$(if $(filter grouped-target,${.FEATURES}) \
	,,$(error Your version of GNU make DOES NOT support grouped targets. Parallel procesing may lead some of the steps to be executed unnecesarily more than once or perhaps to err.))

all: $(addprefix ../data/,g6.rda d6.rda s6.rda) \
	$(addprefix ../inst/exdata/,sample-geng-header.g6 sample-geng-noheader.g6 sample-geng-header.s6 sample-geng-noheader.s6)

$(addprefix ../data/,g6.rda d6.rda s6.rda) &: g-d-s-6.R
	$(rscript) $(rscript_flags) $<

# Sample g6 and s6 files generated with Nauty-geng
# (install `nauty` package on Debian derivatives)
../inst/exdata/sample-geng-header.g6:
	$(geng) 5 -lhc $@

../inst/exdata/sample-geng-noheader.g6:
	$(geng) 5 -lc $@

../inst/exdata/sample-geng-header.s6:
	$(geng) 5 -lshc $@

../inst/exdata/sample-geng-noheader.s6:
	$(geng) 5 -lsc $@

geng: $(addprefix ../inst/exdata/,sample-geng-header.g6 sample-geng-noheader.g6 sample-geng-header.s6 sample-geng-noheader.s6)

.PHONY: all geng
