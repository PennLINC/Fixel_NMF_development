#!/bin/bash
export MRTRIX_NTHREADS=$2
bblid=$1

#Convert files
mrconvert -grad \
	/work/data/qsiprep_outputs/sub-${bblid}/ses-PNC1/dwi/sub-${bblid}_*dwi.b \
	/work/data/qsiprep_outputs/sub-${bblid}/ses-PNC1/dwi/sub-${bblid}_*dwi.nii.gz \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/sub-${bblid}_ses-PNC1.mif \
	-force

mrconvert \
        /work/data/qsiprep_outputs/sub-${bblid}/anat/sub-${bblid}_desc-brain_mask.nii.gz \
        /work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/anat/sub-${bblid}_anatmask.mif \
	-force

#Update size of the mask to match dimensions of diffusion image
mrresize \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/anat/sub-${bblid}_anatmask.mif \
	-size 156,186,158 \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/anat/sub-${bblid}_anatmask_s.mif \
	-force

rm -rf /work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/anat/sub-${bblid}_anatmask.mif
   
