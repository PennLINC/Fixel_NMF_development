#!/bin/bash
export MRTRIX_NTHREADS=$1

#Intersection of all masks
mrmath \
	/work/data/MRtrix_recon/sub-*/ses-PNC1/anat/sub-*_mask_temp_new.mif \
	min \
	/work/data/MRtrix_recon/template/template_mask_new.mif \
	-datatype bit \
	-force

#Compute fixel mask while testing 2 different values of fmls_peak_value that determines the inclusion/exclusion of fixels. This step ultimately determines the fixel mask in which statistical analyses will be performed. Here, I didn't choose the recommended value of 0.06 because other values like 0.08 or 0.10 do include crossing fixels but remove a lot of false positive in non white matter areas. I ended up choosing the solution at fmls_peak_value 0.10
#fod2fixel \
#	-mask \
#	/work/data/MRtrix_recon/template/template_mask_new.mif \
#	-fmls_peak_value 0.08 \
#	/work/data/MRtrix_recon/template/wmfod_template_new.mif \
#	/work/data/MRtrix_recon/template/fixel_mask_08_new \
#	-force

fod2fixel \
        -mask \
        /work/data/MRtrix_recon/template/template_mask_new.mif \
        -fmls_peak_value 0.10 \
	/work/data/MRtrix_recon/template/wmfod_template_new.mif \
	/work/data/MRtrix_recon/template/fixel_mask_10_new \
	-force



  
