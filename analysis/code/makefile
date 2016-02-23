# Makefile for \analysis of the BAV project
# by Hannes Datta
# requires GNU Make

include ../../tools/common

TEMP_DIR = ../temp
OUTPUT_DIR = ../output


###### BUILD COMMANDS ######

.PHONY: build

build: $(OUTPUT_DIR)/results.RData

###### DATA ANALYSIS ######

# analyze data
$(OUTPUT_DIR)/results.RData: analysis.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "analysis.R"
