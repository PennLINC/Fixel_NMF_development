#$ -pe threaded 8-12
#$ -l h_vmem=64G
#$ -M josiane.i.bourque@gmail.com
#$ -m e
#$ -m a
#$ -o /cbica/projects/pnc_fixel_cs/scripts/MRtrix_recon/script_output/template.$JOB_ID.stdout
#$ -e /cbica/projects/pnc_fixel_cs/scripts/MRtrix_recon/script_output/template.$JOB_ID.stderr

#!/bin/bash
project=/cbica/projects/pnc_fixel_cs
SIF=${project}/scripts/qsiprep-0.13.0.sif #at that time we created a new singularity image with the latest updates from MRtrix3
TMPDIR=/tmp
LOCALTMPDIR=\$SBIA_TMPDIR

#Population template
singularity exec --cleanenv \
        -B ${project}:/work \
        $SIF bash /work/scripts/MRtrix_recon/template_creation.sh $NSLOTS


rm -rf \$LOCALTMPDIR

