.PHONY: import construct_superlands matching

all: import construct_superlands matching

import:
	cd import && make

construct_superlands:
	cd construct_superlands && make

matching:
	cd matching && make

scaffold:
	mkdir -p import/input import/output construct_superlands/output construct_superlands/cache construct_superlands/scratch matching/output/figures matching/scratch summarize/output visualize/output
