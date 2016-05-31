# Makefile for \analysis of the BAV project
# by Hannes Datta
# requires GNU Make

include ../../tools/common

TEMP_DIR = ../temp
OUTPUT_DIR = ../output


###### BUILD COMMANDS ######

.PHONY: build

analyze: $(OUTPUT_DIR)/results.RData
equity: $(OUTPUT_DIR)/equity.RData

# csv: see below

###### DATA ANALYSIS ######

# analyze data
$(OUTPUT_DIR)/results.RData: analysis.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "analysis.R"

# compute brand equity
$(OUTPUT_DIR)/equity.RData: modelselection_equity.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "modelselection_equity.R"

csv: make_csv.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "make_csv.R"
