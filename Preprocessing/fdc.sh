#!/bin/bash
export MRTRIX_NTHREADS=$1

#Subjects bblids
mapfile -t pnc < /work/scripts/MRtrix_recon/subject_lists/ltn_bblids.csv;

#Log FC and FDC
mkdir /work/data/MRtrix_recon/template/log_fc_10_new
mkdir /work/data/MRtrix_recon/template/fdc_10_new

cp /work/data/MRtrix_recon/template/fc_10_new/index.mif /work/data/MRtrix_recon/template/log_fc_10_new
cp /work/data/MRtrix_recon/template/fc_10_new/directions.mif /work/data/MRtrix_recon/template/log_fc_10_new
cp /work/data/MRtrix_recon/template/fc_10_new/index.mif /work/data/MRtrix_recon/template/fdc_10_new
cp /work/data/MRtrix_recon/template/fc_10_new/directions.mif /work/data/MRtrix_recon/template/fdc_10_new

for bblid in ${pnc[@]};
do
mrcalc \
	/work/data/MRtrix_recon/template/fc_10_new/sub-${bblid}.mif \
	-log \
	/work/data/MRtrix_recon/template/log_fc_10_new/sub-${bblid}.mif

mrcalc \
        /work/data/MRtrix_recon/template/fd_10_new/sub-${bblid}.mif \
	/work/data/MRtrix_recon/template/fc_10_new/sub-${bblid}.mif \
        -mult \
        /work/data/MRtrix_recon/template/fdc_10_new/sub-${bblid}.mif \
	-force

done

