#!/bin/sh
# Round 4 runs: priors x blending, each league on its seasons with test priors
cd /home/claude/foxandfortune/dante
P=/home/claude/_Backtest/priors/output
run() { # variant priors_folder tag
  for lg in MBB WBB; do
    if [ "$lg" = MBB ]; then S="2021:2026"; else S="2023:2026"; fi
    BT_PRIORS_DIR=$P/$2 BT_TAG=$3 BT_SEASONS=$S Rscript /home/claude/_Backtest/run_variant.R $1 $lg
  done
}
run P_C1 priors_chosen _pch
run P_C1 priors_P0 _pp0
run P_C1 empty _pnone
run Q_off1 priors_chosen _pch
run Q_off2 priors_chosen _pch
run Q_slow priors_chosen _pch
run Q_min0 priors_chosen _pch
run Q_off05 priors_chosen _pch
run Q_off4 priors_chosen _pch
run Q_fast priors_chosen _pch
run Q_min30 priors_chosen _pch
run P_C1 priors_chosen_roster _pcr
echo QUEUE_DONE
