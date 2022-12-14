#!/bin/bash
export MRTRIX_NTHREADS=$2
bblid=$1

#Intensity normalisation
mtnormalise \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/sub-${bblid}_wmfod_new.mif \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/sub-${bblid}_wmfod_new_norm.mif \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/sub-${bblid}_gm_new.mif \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/sub-${bblid}_gm_new_norm.mif \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/sub-${bblid}_csf_new.mif \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/sub-${bblid}_csf_new_norm.mif \
	-mask \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/anat/sub-${bblid}_anatmask_s.mif \
	-force

#If required to save space, then remove those files
#rm -rf /work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/sub-${bblid}_wmfod_new.mif
#rm -rf /work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/sub-${bblid}_gm_new.mif
#rm -rf /work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/sub-${bblid}_csf_new.mif
   
