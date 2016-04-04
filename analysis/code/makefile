# Makefile for \analysis of the BAV project
# by Hannes Datta
# requires GNU Make

include ../../tools/common

TEMP_DIR = ../temp
OUTPUT_DIR = ../output


###### BUILD COMMANDS ######

.PHONY: build

build: $(OUTPUT_DIR)/results.RData
equity: $(OUTPUT_DIR)/equity.RData

###### DATA ANALYSIS ######

# analyze data
$(OUTPUT_DIR)/results.RData: analysis.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "analysis.R"

# compute brand equity
$(OUTPUT_DIR)/equity.RData: process_results.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "process_results.R"

summarize: summarize.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "summarize.R"
