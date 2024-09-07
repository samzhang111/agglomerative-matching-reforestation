OUTPUTS := $(wildcard output/com_*.csv)

# Define the main task
all: $(OUTPUTS)

# Define how to create the outputs
$(OUTPUTS): input/v3/all_lands_v3.shp src/import_shapefiles.R
	Rscript src/import_shapefiles.R

# Clean task to remove generated files
clean:
	rm -f $(OUTPUTS)
