#$ -pe threaded 8
#$ -l h_vmem=20G

#!/bin/bash
SIF=/cbica/projects/pnc_fixel_cs/scripts/ss3t_csd_beta.sif

#Create the mean response function
singularity exec \
	-B /cbica/projects/pnc_fixel_cs:/work \
	$SIF bash /work/scripts/MRtrix_recon/response_mean.sh

