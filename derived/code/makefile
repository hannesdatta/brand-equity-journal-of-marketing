# Makefile for \derived of the BAV project
# by Hannes Datta
# requires GNU Make

include ../../tools/common

TEMP_DIR = ../temp
OUTPUT_DIR = ../output


###### BUILD COMMANDS ######

#.PHONY : run

# complete build
build: $(OUTPUT_DIR)/datasets.RData

# part builds
raw: $(TEMP_DIR)/rawdata.RData
attr: $(TEMP_DIR)/attributes.RData


###### DATA PREPARATION ######

# convert sas7bdat to RData
$(TEMP_DIR)/rawdata.RData : rawdata.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "rawdata.R"

# read attribute-level information from XLS
$(TEMP_DIR)/attributes.RData : attributes.R
	"$(RBIN32)Rterm.exe" --vanilla --args "" < "attributes.R"

# pre-clean data, indicate attributes in column names, remove last level
$(OUTPUT_DIR)/datasets.RData : preclean.R $(TEMP_DIR)/attributes.RData $(TEMP_DIR)/rawdata.RData
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "preclean.R"

