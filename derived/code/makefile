# Makefile for \derived of the BAV project
# by Hannes Datta
# requires GNU Make

include ../../tools/common

TEMP_DIR = ../temp
OUTPUT_DIR = ../output


###### BUILD COMMANDS ######

#.PHONY : run

# complete build
build: $(OUTPUT_DIR)/datasets.RData surv
buildp: $(OUTPUT_DIR)/datasets.RData surv plots

# part builds
raw: $(TEMP_DIR)/iri_sales.RData
attr: $(TEMP_DIR)/attributes.RData
cpindex: $(TEMP_DIR)/cpi.RData
bavdat: $(TEMP_DIR)/bav.RData
bavdel: $(TEMP_DIR)/bav_deletes.RData
surv: $(OUTPUT_DIR)/survey.csv

###### DATA PREPARATION ######

# convert sales data from sas7bdat to RData
$(TEMP_DIR)/iri_sales.RData : iri_sales.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "iri_sales.R"

# convert BAV data from sas7bdat to RData
$(TEMP_DIR)/bav.RData : bav.R $(TEMP_DIR)/iri_sales.RData
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "bav.R"

# convert BAV data from sas7bdat to RData
$(OUTPUT_DIR)/survey.csv : survey.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "survey.R"

# read in CPI data and map to category names (cat_name)
CPIDEPENDENCIES := ../../raw/cpi/catmapping.csv ../../raw/cpi/CPI\ and\ GDP\ figures.sav
$(TEMP_DIR)/cpi.RData: cpi.R $(CPIDEPENDENCIES)
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "cpi.R"

# read attribute-level information from XLS
$(TEMP_DIR)/attributes.RData : attributes.R ../../../Attributes_check/cat_attrdummy_lookup.xlsx
	"$(RBIN32)Rterm.exe" --vanilla --args "" < "attributes.R"

# read in information on which BAV brands to delete/retain, by category
$(TEMP_DIR)/bav_deletes.RData : bav_deletes.R ../../raw/bav_deletes/bav_deletes.xlsx
	"$(RBIN32)Rterm.exe" --vanilla --args "" < "bav_deletes.R"

# pre-clean data, indicate attributes in column names, remove last level
$(OUTPUT_DIR)/datasets.RData : preclean.R $(TEMP_DIR)/attributes.RData $(TEMP_DIR)/iri_sales.RData $(TEMP_DIR)/cpi.RData $(TEMP_DIR)/bav.RData $(TEMP_DIR)/bav_deletes.RData
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "preclean.R"

# pre-clean data, indicate attributes in column names, remove last level
plots : plots.R $(OUTPUT_DIR)/datasets.RData
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "plots.R"
plots2 : plots.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "plots.R"
