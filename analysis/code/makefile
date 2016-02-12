# Makefile for \analysis of the BAV project
# by Hannes Datta
# requires GNU Make

include ../../tools/common

TEMP_DIR = ../temp
OUTPUT_DIR = ../output


###### BUILD COMMANDS ######

.PHONY : run

build: $(OUTPUT_DIR)/results.RData

###### DATA ANALYSIS ######

# analyze data
$(OUTPUT_DIR)/results.RData: analysis.R attraction.R itersur.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "analysis.R"
