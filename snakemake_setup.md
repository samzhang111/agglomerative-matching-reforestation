# Snakemake Workflow Setup

This document provides quick setup instructions for the Snakemake-based workflow.

## Quick Start

1. **Install dependencies**:
   ```bash
   pip install -r requirements.txt
   pip install snakemake
   ```

2. **Setup directories**:
   ```bash
   make scaffold
   ```

3. **Place input data**:
   - Copy `all_lands_v3.shp` (and associated files) to `import/input/v3/`

4. **Run workflow**:
   ```bash
   # Local execution (for testing/development)
   snakemake --profile profiles/local --dry-run  # Preview
   snakemake --profile profiles/local            # Execute
   
   # SLURM execution (for production)
   snakemake --profile profiles/slurm --dry-run  # Preview  
   snakemake --profile profiles/slurm            # Execute
   ```

## Configuration

Edit `config.yaml` to customize:
- **Seeds**: Number of random seeds and weight configurations
- **Resources**: SLURM memory/time requirements  
- **Index size**: Nearest neighbor index size
- **Land types**: Which land type combinations to process

## Troubleshooting

- **SLURM issues**: Check `logs/slurm_*.out` for job output
- **Missing files**: Run `snakemake --profile profiles/local --dry-run` to see dependency tree
- **Resource limits**: Adjust `config.yaml` resource requirements for your cluster
