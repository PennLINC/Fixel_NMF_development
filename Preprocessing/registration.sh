#!/bin/bash
export MRTRIX_NTHREADS=$2
bblid=$1
	
#Calculate individual subject's FOD warping to the FOD template
mrregister \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/sub-${bblid}_wmfod_new_norm.mif \
	-mask1 \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/anat/sub-${bblid}_anatmask_s.mif \
	/work/data/MRtrix_recon/template/wmfod_template_new.mif \
	-nl_warp \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/sub-${bblid}_sub2temp_new.mif \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/sub-${bblid}_temp2sub_new.mif \
	-force

#Warp masks into template space
mrtransform \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/anat/sub-${bblid}_anatmask_s.mif \
	-warp \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/sub-${bblid}_sub2temp_new.mif \
	-interp nearest \
	-datatype bit \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/anat/sub-${bblid}_mask_temp_new.mif \
	-force

#Warp FODs to template space
mrtransform \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/sub-${bblid}_wmfod_new_norm.mif \
	-warp \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/sub-${bblid}_sub2temp_new.mif \
	-noreorientation \
	/work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/sub-${bblid}_fod_temp_NR_new.mif \
	-force

  
rm -f /work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/sub-${bblid}_temp2sub_new.mif
 
