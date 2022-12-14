#!/bin/bash

export MRTRIX_NTHREADS=$2
bblid=$1

#Segment FOD to fixels - estimated AFD (FD)
fod2fixel \
	-mask \
	/work/data/MRtrix_recon/template/template_mask_new.mif \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/sub-${bblid}_fod_temp_NR_new.mif \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/fixel_NR_new \
	-afd \
	sub-${bblid}_fd.mif \
	-disp \
	sub-${bblid}_disp.mif \
	-force

#Reorient fixel
fixelreorient \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/fixel_NR_new \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/sub-${bblid}_sub2temp_new.mif \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/fixel_new \
	-force

rm -rf /work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/fixel_NR_new/

#Assign subject fixel to template fixel
fixelcorrespondence \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/fixel_new/sub-${bblid}_fd.mif \
	/work/data/MRtrix_recon/template/fixel_mask_10_new \
	/work/data/MRtrix_recon/template/fd_10_new \
	sub-${bblid}.mif
	
#Compute Fibre cross-section (FC)
warp2metric \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/sub-${bblid}_sub2temp_new.mif \
	-fc \
	/work/data/MRtrix_recon/template/fixel_mask_10_new \
	/work/data/MRtrix_recon/template/fc_10_new \
	sub-${bblid}.mif \
	-force



