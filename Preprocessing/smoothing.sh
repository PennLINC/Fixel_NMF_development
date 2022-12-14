#!/bin/bash
export MRTRIX_NTHREADS=$1

#Generate fixel-fixel connectivity matrix
	/cbica/projects/pnc_fixel_cs/bin/fixelconnectivity \
        /cbica/projects/pnc_fixel_cs/data/MRtrix_recon/template/fixel_mask_10_new/ \
        /cbica/projects/pnc_fixel_cs/data/MRtrix_recon/template/tracks_2million_sift.tck \
        /cbica/projects/pnc_fixel_cs/data/MRtrix_recon/template/matrix/

#Smooth fixel using the fixel-fixel connectivity
	/cbica/projects/pnc_fixel_cs/bin/fixelfilter \
	/cbica/projects/pnc_fixel_cs/data/MRtrix_recon/template/fd_10_new \
	smooth \
	/cbica/projects/pnc_fixel_cs/data/MRtrix_recon/template/fd_10_smoothed_10fwhm_new \
	-matrix \
	/cbica/projects/pnc_fixel_cs/data/MRtrix_recon/template/matrix \
	-fwhm 10


	/cbica/projects/pnc_fixel_cs/bin/fixelfilter \
	/cbica/projects/pnc_fixel_cs/data/MRtrix_recon/template/log_fc_10_new \
	smooth \
	/cbica/projects/pnc_fixel_cs/data/MRtrix_recon/template/log_fc_10_smoothed_10fwhm_new \
	-matrix \
	/cbica/projects/pnc_fixel_cs/data/MRtrix_recon/template/matrix \
	-fwhm 10

	/cbica/projects/pnc_fixel_cs/bin/fixelfilter \
        /cbica/projects/pnc_fixel_cs/data/MRtrix_recon/template/fdc_10_new \
	smooth \
        /cbica/projects/pnc_fixel_cs/data/MRtrix_recon/template/fdc_10_smoothed_10fwhm_new \
	-matrix \
        /cbica/projects/pnc_fixel_cs/data/MRtrix_recon/template/matrix \
	-fwhm 10

