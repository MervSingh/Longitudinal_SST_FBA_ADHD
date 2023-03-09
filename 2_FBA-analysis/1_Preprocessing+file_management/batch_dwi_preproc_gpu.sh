#!/bin/bash
#
# This batch file calls on your subject
# list (named subject_list.txt). And 
# runs the job_dwi_preproc.sh file for 
# each subject. It saves the ouput
# and error files in their specified
# directories.


# Set subject list
SUBJLIST=`cat subject_list5.txt` 

for SUBJ in $SUBJLIST

do

subid=`echo $SUBJ|awk '{print $1}' FS=","`
waveid=`echo $SUBJ|awk '{print $2}' FS=","`

sbatch --export sub=${subid},wave=${waveid} --job-name dwi_preproc_gpu --gres=gpu:P100:1 --partition=m3h --ntasks=1 --mem=8000 --account=tg69 --time=10:00:00 -o /scratch/tg69/dwi_preproc/code/output/"${SUBJ}"_output_%j.txt -e /scratch/tg69/dwi_preproc/code/error/"${SUBJ}"_error_%j.txt job_dwi_preproc_gpu.sh

done
