#$ -pe threaded 8-12
#$ -l h_vmem=64G
#$ -M josiane.i.bourque@gmail.com
#$ -m e
#$ -m a
#$ -o /cbica/projects/pnc_fixel_cs/scripts/MRtrix_recon/script_output/tracks.$JOB_ID.stdout
#$ -e /cbica/projects/pnc_fixel_cs/scripts/MRtrix_recon/script_output/tracks.$JOB_ID.stderr

#!/bin/bash
project=/cbica/projects/pnc_fixel_cs
SIF=${project}/scripts/qsiprep-0.13.0.sif
TMPDIR=/tmp
LOCALTMPDIR=\$SBIA_TMPDIR

#Whole-brain tractography
singularity exec --cleanenv \
        -B ${project}:/work \
        $SIF bash /work/scripts/MRtrix_recon/tracks.sh $NSLOTS


rm -rf \$LOCALTMPDIR

