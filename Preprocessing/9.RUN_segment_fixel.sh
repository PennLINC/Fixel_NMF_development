#!/bin/bash
mkdir -p /cbica/projects/pnc_fixel_cs/scripts/MRtrix_recon/SEG_jobscripts_try1

project=/cbica/projects/pnc_fixel_cs
while read subject
do
    echo SUBJECT: ${subject}
    OUTPUT_PROOF=${project}/data/MRtrix_recon/template/fd_10_new/sub-${subject}.mif
    if [ -f ${OUTPUT_PROOF} ];
    then
            echo Found ${OUTPUT_PROOF}
            continue
    fi
    echo submit $subject

    cat <<EOS > SEG_jobscripts_try1/${subject}.sh
#!/bin/bash
#$ -cwd
#$ -N s${subject}_all
#$ -pe threaded 8-18
#$ -l h_vmem=64G
#$ -M josiane.i.bourque@gmail.com
#$ -o /cbica/projects/pnc_fixel_cs/scripts/MRtrix_recon/script_output/segment.$JOB_ID.stdout
#$ -e /cbica/projects/pnc_fixel_cs/scripts/MRtrix_recon/script_output/segment.$JOB_ID.stderr
TMPDIR=/tmp
LOCALTMPDIR=\$SBIA_TMPDIR
SIF=${project}/scripts/qsiprep-0.13.0.sif

singularity exec --cleanenv \\
  -B ${project}:/work \\
  -B \$LOCALTMPDIR:\$TMPDIR \\
  \$SIF \
  bash /work/scripts/MRtrix_recon/segment_fixel.sh $subject \$NSLOTS

rm -rf \$LOCALTMPDIR

EOS
done < subject_lists/ltn_bblids.csv

cd SEG_jobscripts_try1
for f in *sh
do
    qsub $f
done
