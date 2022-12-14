#!/bin/bash
export MRTRIX_NTHREADS=$2
bblid=$1
	
#Spherical deconvolution
ss3t_csd_beta1 \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/sub-${bblid}_ses-PNC1.mif \
	/work/data/MRtrix_recon/template/av_wm_response_new.txt \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/sub-${bblid}_wmfod_new.mif \
	/work/data/MRtrix_recon/template/av_gm_response_new.txt \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/sub-${bblid}_gm_new.mif \
	/work/data/MRtrix_recon/template/av_csf_response_new.txt \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/sub-${bblid}_csf_new.mif \
	-mask \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/anat/sub-${bblid}_anatmask_s.mif \
	-force


   
