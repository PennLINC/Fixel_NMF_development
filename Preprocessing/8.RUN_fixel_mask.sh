#$ -pe threaded 8-12
#$ -l h_vmem=32G
#$ -M josiane.i.bourque@gmail.com
#$ -m e
#$ -m a
#$ -o /cbica/projects/pnc_fixel_cs/scripts/MRtrix_recon/script_output/fixel_mask.$JOB_ID.stdout
#$ -e /cbica/projects/pnc_fixel_cs/scripts/MRtrix_recon/script_output/fixel_mask.$JOB_ID.stderr

#!/bin/bash
project=/cbica/projects/pnc_fixel_cs
SIF=${project}/scripts/qsiprep-0.4.5.simg
TMPDIR=/tmp
LOCALTMPDIR=\$SBIA_TMPDIR

#Fixel mask
singularity exec --cleanenv \
        -B ${project}:/work \
        $SIF bash /work/scripts/MRtrix_recon/fixel_mask.sh $NSLOTS


rm -rf \$LOCALTMPDIR

