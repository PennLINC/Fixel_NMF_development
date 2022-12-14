#$ -pe threaded 8-12
#$ -l h_vmem=64G
#$ -M josiane.i.bourque@gmail.com
#$ -m e
#$ -m a
#$ -o /cbica/projects/pnc_fixel_cs/scripts/MRtrix_recon/script_output/smooth.$JOB_ID.stdout
#$ -e /cbica/projects/pnc_fixel_cs/scripts/MRtrix_recon/script_output/smooth.$JOB_ID.stderr

#!/bin/bash
project=/cbica/projects/pnc_fixel_cs
TMPDIR=/tmp
LOCALTMPDIR=\$SBIA_TMPDIR
#i=(6,10);


#Smoothing
bash ${project}/scripts/MRtrix_recon/smoothing.sh $NSLOTS


rm -rf \$LOCALTMPDIR

