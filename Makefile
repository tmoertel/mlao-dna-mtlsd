# This a Makefile.  Run "make" from the command line to have the
# GNU Make program read this file and use the rules within it to
# perform the statistical analyses and generate the resulting
# charts and other outputs.
#
# Tom Moertel <tom@mlao.org>
# 2009-10-11


all_charts = $(hvf_charts) $(mtlvusc_charts)

# MTLSD historical vs. forecasted spending growth
hvf_analysis = mtlsd-re-taxes-hist-vs-forecasted-analysis.R
hvf_charts = mtlsd-hist-and-forecasted-re-taxes.pdf \
             mtlsd-hist-and-forecasted-re-taxes.png

# MTLSD vs. USCSD historical vs. forecasted spending
mtlvusc_analysis = mtlsd-re-taxes-mtl-vs-usc-analysis.R
mtlvusc_charts = mtl-tax-penalty-vs-usc.pdf mtl-tax-penalty-vs-usc.png


default: all

.PHONY: default

.PHONY: all
all: $(all_charts)

$(hvf_charts): $(hvf_analysis)
	./$(hvf_analysis)

$(mtlvusc_charts): $(mtlvusc_analysis)
	./$(mtlvusc_analysis)


.PHONY: clean
clean:
	rm -f $(all_charts)
