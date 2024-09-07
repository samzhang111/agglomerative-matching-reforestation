.PHONY: all clean

INDEX_SIZE := 10000
SEED ?= 11
USE_WEIGHTS ?= 1

# All files involving private lands with the .csv extension in the ../import/output folder
INPUT_FILES := $(wildcard ../import/output/*priv*.csv)

# Stems of the basenames of the files
STEMS := $(notdir $(basename $(INPUT_FILES)))

# Output files based on the stems
OUTPUTS := $(addprefix output/, $(addsuffix _superlands_$(INDEX_SIZE)_$(SEED).csv, $(STEMS)))

# 'all' target to produce all output files
all: $(OUTPUTS)

# Rule to run python script and produce the output file
output/%_superlands_$(INDEX_SIZE)_$(SEED).csv: ../import/output/%.csv
	mkdir -p output/$(SEED)
	python src/construct_superlands.py $(basename $(notdir $<)) $(SEED) $(INDEX_SIZE) $(USE_WEIGHTS)

clean:
	rm -rf output/*
