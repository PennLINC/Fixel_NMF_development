#!/bin/bash
export MRTRIX_NTHREADS=$2
bblid=$1 

#Estimate response function
dwi2response dhollander \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/sub-${bblid}_ses-PNC1.mif \
	/work/data/MRtrix_recon/template/sub-${bblid}/sub-${bblid}_response_wm.txt \
	/work/data/MRtrix_recon/template/sub-${bblid}/sub-${bblid}_response_gm.txt \
	/work/data/MRtrix_recon/template/sub-${bblid}/sub-${bblid}_response_csf.txt \
	-force

  
