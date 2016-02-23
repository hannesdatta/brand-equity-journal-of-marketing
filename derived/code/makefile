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
raw: $(TEMP_DIR)/iri_sales.RData
attr: $(TEMP_DIR)/attributes.RData
cpindex: $(TEMP_DIR)/cpi.RData
bavdat: $(TEMP_DIR)/bav.RData

###### DATA PREPARATION ######

# convert sales data from sas7bdat to RData
$(TEMP_DIR)/iri_sales.RData : iri_sales.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "iri_sales.R"

# convert BAV data from sas7bdat to RData
$(TEMP_DIR)/bav.RData : bav.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "bav.R"

# read in CPI data and map to category names (cat_name)
$(TEMP_DIR)/cpi.RData: cpi.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "cpi.R"

# read attribute-level information from XLS
$(TEMP_DIR)/attributes.RData : attributes.R ../../../Attributes_check/cat_attrdummy_lookup.xlsx
	"$(RBIN32)Rterm.exe" --vanilla --args "" < "attributes.R"

# pre-clean data, indicate attributes in column names, remove last level
$(OUTPUT_DIR)/datasets.RData : preclean.R $(TEMP_DIR)/attributes.RData $(TEMP_DIR)/iri_sales.RData $(TEMP_DIR)/cpi.RData $(TEMP_DIR)/bav.RData
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "preclean.R"

# pre-clean data, indicate attributes in column names, remove last level
plots : plots.R $(OUTPUT_DIR)/datasets.RData
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "plots.R"
