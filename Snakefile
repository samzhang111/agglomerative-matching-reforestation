"""
Snakemake workflow for agglomerative matching analysis
of land tenure regimes and restoration outcomes in Brazil's Atlantic Forest
"""

configfile: "config.yaml"

# Import required functions
import os
from pathlib import Path

# Helper function to get all seeds for a given weight configuration
def get_seeds(use_weights):
    if use_weights:
        return list(range(config["seeds"]["with_weights"]["start"], 
                         config["seeds"]["with_weights"]["end"] + 1))
    else:
        return list(range(config["seeds"]["no_weights"]["start"], 
                         config["seeds"]["no_weights"]["end"] + 1))

# Get all land type combinations
PRIVATE_COMPARISONS = config["land_types"]["private_comparisons"]
DIRECT_COMPARISONS = config["land_types"]["direct_comparisons"]
ALL_COMPARISONS = PRIVATE_COMPARISONS + DIRECT_COMPARISONS

# Define seed ranges
NO_WEIGHT_SEEDS = get_seeds(False)  # 1-499
WEIGHT_SEEDS = get_seeds(True)      # 500-999
ALL_SEEDS = NO_WEIGHT_SEEDS + WEIGHT_SEEDS

INDEX_SIZE = config["index_size"]

# Final target rule
rule all:
    input:
        # Final visualization outputs
        "visualize/output/main_results.pdf",
        "visualize/output/balance_plots.pdf", 
        "visualize/output/balance_plots.png",
        "visualize/output/counts_and_areas_by_land_types.pdf",
        "visualize/output/distributions_by_land_types.pdf",
        "visualize/output/explanatory_map.pdf"

# Import shapefiles to CSV
rule import_shapefiles:
    input:
        shapefile="import/input/v3/all_lands_v3.shp"
    output:
        expand("import/output/{land_type}.csv", land_type=ALL_COMPARISONS)
    resources:
        time=config["resources"]["import"]["time"],
        mem=config["resources"]["import"]["mem"],
        cpus=config["resources"]["import"]["cpus"]
    shell:
        """
        cd import
        Rscript src/import_shapefiles.R
        """

# Construct superlands for private land comparisons
rule construct_superlands:
    input:
        csv="import/output/{land_type}.csv"
    output:
        superlands="construct_superlands/output/{seed}/{land_type}_superlands_{index_size}_{seed}.csv"
    params:
        use_weights=lambda wc: 1 if int(wc.seed) >= config["seeds"]["with_weights"]["start"] else 0
    resources:
        time=config["resources"]["construct_superlands"]["time"],
        mem=config["resources"]["construct_superlands"]["mem"], 
        cpus=config["resources"]["construct_superlands"]["cpus"]
    shell:
        """
        cd construct_superlands
        mkdir -p output/{wildcards.seed}
        python src/construct_superlands.py {wildcards.land_type} {wildcards.seed} {wildcards.index_size} {params.use_weights}
        """

# Matching analysis (handles both superlands and direct comparisons)
# The matching_superlands.R script is the main script that handles all matching
rule matching:
    input:
        # All required superlands for private comparisons  
        superlands=expand("construct_superlands/output/{seed}/{land_type}_superlands_{index_size}_{seed}.csv",
                         seed="{seed}", land_type=PRIVATE_COMPARISONS, index_size=INDEX_SIZE)
    output:
        # Aggregated results from matching_superlands.R
        all_results="matching/output/{seed}_{index_size}/all_results_{seed}_{index_size}.csv",
        all_balance="matching/output/{seed}_{index_size}/all_balance_{seed}_{index_size}.csv"
    resources:
        time=config["resources"]["matching"]["time"],
        mem=config["resources"]["matching"]["mem"],
        cpus=config["resources"]["matching"]["cpus"]
    shell:
        """
        cd matching
        mkdir -p output/{wildcards.seed}_{wildcards.index_size}
        Rscript src/matching_superlands.R {wildcards.seed} {wildcards.index_size}
        """

# Summarize all results across seeds
rule summarize:
    input:
        # All matching results from both weighted and non-weighted runs
        balance_files=expand("matching/output/{seed}_{index_size}/all_balance_{seed}_{index_size}.csv", 
                           seed=ALL_SEEDS, index_size=INDEX_SIZE),
        results_files=expand("matching/output/{seed}_{index_size}/all_results_{seed}_{index_size}.csv",
                           seed=ALL_SEEDS, index_size=INDEX_SIZE)
    output:
        balances="summarize/output/balances.csv",
        results="summarize/output/results.csv"
    resources:
        time=config["resources"]["summarize"]["time"],
        mem=config["resources"]["summarize"]["mem"],
        cpus=config["resources"]["summarize"]["cpus"]
    shell:
        """
        cd summarize
        python src/summarize_balance.py
        """

# Generate main results visualization
rule visualize_results:
    input:
        balances="summarize/output/balances.csv",
        results="summarize/output/results.csv"
    output:
        main_results="visualize/output/main_results.pdf",
        balance_plots_pdf="visualize/output/balance_plots.pdf",
        balance_plots_png="visualize/output/balance_plots.png",
        table_s1="visualize/output/table_s1_results.csv",
        summary_averages="visualize/output/summary_averages.csv"
    resources:
        time=config["resources"]["visualize"]["time"],
        mem=config["resources"]["visualize"]["mem"],
        cpus=config["resources"]["visualize"]["cpus"]
    shell:
        """
        cd visualize
        Rscript src/results.R
        """

# Generate descriptive visualizations
rule visualize_descriptives:
    input:
        # Raw import data
        expand("import/output/{land_type}.csv", land_type=ALL_COMPARISONS)
    output:
        counts_areas="visualize/output/counts_and_areas_by_land_types.pdf", 
        distributions="visualize/output/distributions_by_land_types.pdf",
        explanatory_map="visualize/output/explanatory_map.pdf"
    resources:
        time=config["resources"]["visualize"]["time"],
        mem=config["resources"]["visualize"]["mem"],
        cpus=config["resources"]["visualize"]["cpus"]
    shell:
        """
        cd visualize
        Rscript src/descriptives.R
        """

# Rule to generate superlands for all private comparisons and all seeds
rule all_superlands:
    input:
        expand("construct_superlands/output/{seed}/{land_type}_superlands_{index_size}_{seed}.csv",
               seed=ALL_SEEDS,
               land_type=PRIVATE_COMPARISONS, 
               index_size=INDEX_SIZE)

# Rule to generate all matching results
rule all_matching:
    input:
        # Private comparisons (need superlands)
        expand("matching/output/{seed}_{index_size}/all_results_{seed}_{index_size}.csv",
               seed=ALL_SEEDS, index_size=INDEX_SIZE)

# Helper rule to run just the import step
rule import_only:
    input:
        expand("import/output/{land_type}.csv", land_type=ALL_COMPARISONS)