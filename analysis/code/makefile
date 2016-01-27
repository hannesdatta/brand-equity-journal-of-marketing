# Makefile for \analysis of the BAV project
# by Hannes Datta
# requires GNU Make

include ../../tools/common

TEMP_DIR = ../temp
OUTPUT_DIR = ../output


###### BUILD COMMANDS ######

.PHONY : run

build: $(TEMP_DIR)/datasets.RData

###### DATA ANALYSIS ######

# analyze data
run : analysis.R proc_analysis.R proc_sur.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "analysis.R"

$(TEMP_DIR)/datasets.RData : preclean.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "preclean.R"
