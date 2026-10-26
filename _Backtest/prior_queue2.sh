#!/bin/sh
cd /home/claude/foxandfortune/dante
P=/home/claude/_Backtest/priors/output
for v in Q_rtg90 Q_rtg85 Q_rtgmin0; do
  BT_PRIORS_DIR=$P/priors_chosen BT_TAG=_pch BT_SEASONS="2021:2026" Rscript /home/claude/_Backtest/run_variant.R $v MBB
  BT_PRIORS_DIR=$P/priors_chosen BT_TAG=_pch BT_SEASONS="2023:2026" Rscript /home/claude/_Backtest/run_variant.R $v WBB
done
echo QUEUE_DONE
