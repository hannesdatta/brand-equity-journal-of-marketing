# Makefile for \report of the BAV project
# by Hannes Datta
# requires GNU Make

include ../../tools/common

TEMP_DIR = ../temp
OUTPUT_DIR = ../output

build: meta

###### BUILD COMMANDS ######


meta: meta_analysis.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "meta_analysis.R"

stata: second_stage.do
	$(STATABIN) /e do second_stage.do