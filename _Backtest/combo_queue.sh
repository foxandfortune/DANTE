#!/bin/sh
cd /home/claude/foxandfortune/dante
for v in C1_lam050 C3_lam050_nl4 C2_lam025 C4_lam025_nl8; do for lg in MBB WBB; do Rscript /home/claude/_Backtest/run_variant.R $v $lg; done; done
