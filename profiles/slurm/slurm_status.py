#!/usr/bin/env python
"""
SLURM status script for Snakemake
Checks the status of SLURM jobs
"""
import subprocess
import sys
import time

jobid = sys.argv[1]

try:
    # Check job status using squeue
    result = subprocess.run(
        ["squeue", "-j", jobid, "-h", "-o", "%T"],
        capture_output=True,
        text=True,
        timeout=10
    )
    
    if result.returncode == 0:
        status = result.stdout.strip()
        if status == "":
            # Job not found in squeue, check if it completed
            # Check sacct for job history
            result = subprocess.run(
                ["sacct", "-j", jobid, "-n", "-X", "-o", "State"],
                capture_output=True, 
                text=True,
                timeout=10
            )
            
            if result.returncode == 0:
                status = result.stdout.strip().split()[0] if result.stdout.strip() else ""
                if status in ["COMPLETED", "FAILED", "CANCELLED", "TIMEOUT", "NODE_FAIL", "OUT_OF_MEMORY"]:
                    if status == "COMPLETED":
                        print("success")
                    else:
                        print("failed")
                else:
                    print("running")
            else:
                print("failed")
        elif status in ["PENDING", "PD"]:
            print("running")
        elif status in ["RUNNING", "R", "COMPLETING", "CG"]:
            print("running") 
        elif status in ["COMPLETED", "CD"]:
            print("success")
        elif status in ["FAILED", "F", "CANCELLED", "CA", "TIMEOUT", "TO", "NODE_FAIL", "NF", "OUT_OF_MEMORY", "OOM"]:
            print("failed")
        else:
            print("running")
    else:
        print("failed")
        
except subprocess.TimeoutExpired:
    print("running")
except Exception:
    print("failed")