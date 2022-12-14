#!/bin/bash
export MRTRIX_NTHREADS=$1

#Subjects
readarray -t template < /work/scripts/MRtrix_recon/subject_lists/template.txt;

#Create directories necessary for population template
mkdir /work/data/MRtrix_recon/template/fod_input_new
mkdir /work/data/MRtrix_recon/template/mask_input_new

#Copy mask and FODs to new directories 
for bblid in ${template[@]};
do
cp /work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/sub-${bblid}_wmfod_new_norm.mif /work/data/MRtrix_recon/template/fod_input_new/sub-${bblid}.mif
cp /work/data/MRtrix_recon/sub-${bblid}/ses-PNC1/dwi/sub-${bblid}_mask.mif /work/data/MRtrix_recon/template/mask_input_new/sub-${bblid}.mif
done

population_template \
	/work/data/MRtrix_recon/template/fod_input_new \
	-mask_dir \
	/work/data/MRtrix_recon/template/mask_input_new \
	/work/data/MRtrix_recon/template/wmfod_template_new.mif \
	-voxel_size 1.25 \
	-force
  
