#!/bin/bash
mkdir -p /cbica/projects/pnc_fixel_cs/scripts/MRtrix_recon/nii2mif_jobscripts_try1

project=/cbica/projects/pnc_fixel_cs
while read subject
do
    echo SUBJECT: ${subject}
    OUTPUT_PROOF=${project}/data/MRtrix_recon/sub-${subject}/ses-PNC1/dwi/sub-${subject}_ses-PNC1.mif
    if [ -f ${OUTPUT_PROOF} ];
    then
            echo Found ${OUTPUT_PROOF}
            continue
    fi
    echo submit $subject

    cat <<EOS > nii2mif_jobscripts_try1/${subject}.sh
#!/bin/bash
#$ -cwd
#$ -N s${subject}_all
#$ -pe threaded 8-18
#$ -l h_vmem=64G
#$ -M josiane.i.bourque@gmail.com
#$ -o /cbica/projects/pnc_fixel_cs/scripts/MRtrix_recon/script_output/nii2mif.$JOB_ID.stdout
#$ -e /cbica/projects/pnc_fixel_cs/scripts/MRtrix_recon/script_output/nii2mif.$JOB_ID.stderr
TMPDIR=/tmp
LOCALTMPDIR=\$SBIA_TMPDIR
SIF=${project}/scripts/qsiprep-0.4.5.simg

singularity exec --cleanenv \\
  -B ${project}:/work \\
  -B \$LOCALTMPDIR:\$TMPDIR \\
  \$SIF \
  bash /work/scripts/MRtrix_recon/nii2mif.sh $subject \$NSLOTS

rm -rf \$LOCALTMPDIR

EOS
done < subject_lists/ltn_bblids.csv

cd nii2mif_jobscripts_try1
for f in *sh
do
    qsub $f
done
