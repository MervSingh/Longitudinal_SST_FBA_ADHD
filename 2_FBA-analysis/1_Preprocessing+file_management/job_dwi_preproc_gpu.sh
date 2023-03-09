#!/bin/bash

#this script preprocesses diffusion data and prepares it for probabilistic ssmt-csd

#load packages and set paths
dicoms_dir='/projects/tg69/dicoms'
working_dir='/scratch/tg69/dwi_preproc'
fs_dir='/scratch/tg69/freesurfer_hires'

module load fsl/6.0.1
module load cuda/9.1
module load mrtrix3tissue/5.2.8
module load ants/2.3.1
module load freesurfer/6.0

#set variables
subid="sub-${sub}"
sessid="ses-${wave}"
fs_id="sub-${sub}_${wave}"
export SUBJECTS_DIR=/scratch/tg69/freesurfer_hires

#begin job
echo "Working on ${subid} ${sessid}"

mkdir -p -v $working_dir
cd $working_dir
mkdir -p -v $subid/$sessid
cd $subid/$sessid

#copy dicoms and unzip files
echo Copy dicoms and unzip files

cp $dicoms_dir/${wave}/*$sub* .
tar xjf NICAP*tar.bz2

#convert T1 and DWI from nii to mif format, concatenat fmaps
echo Convert nii to mif, concatenate fmaps

if [ $wave == wave3 ]
then 
	mrconvert NICAP*/*MPRAGE* T1.mif 
	mrconvert NICAP*/*CMRR_B2800*2.4mm/ dwi.mif
	mrcat NICAP*/*CMRR_Blip_U_2.4mm NICAP*/*CMRR_Blip_D_2.4mm b0s.mif
else
	mrconvert NICAP*/*MPRAGE* T1.mif 
	mrconvert NICAP*/*CMRR_B2800*2.4/ dwi.mif
	mrcat NICAP*/*CMRR_Blip_U* NICAP*/*CMRR_Blip_D* b0s.mif
fi

#delete dicoms
rm -r -f NICAP*


###DENOISE, UNRING, PREPROCESS
echo Denoise 
dwidenoise dwi.mif dwi_denoised.mif -noise noise.mif
	
	#calculate and check difference between raw and denoised images
	mrcalc dwi.mif dwi_denoised.mif -subtract residual.mif

echo Unring
mrdegibbs dwi_denoised.mif dwi_denoised_unringed.mif -axes 0,1

	#calculate and check difference between denoised and unringed images
	mrcalc dwi_denoised.mif dwi_denoised_unringed.mif -subtract residualUnringed.mif

echo Eddy
dwifslpreproc.patched dwi_denoised_unringed.mif dwi_denoised_unringed_preproc.mif -rpe_header -se_epi b0s.mif -eddy_options="--very_verbose --slm=linear --repol --ol_type=both --mporder=8 --slspec=/scratch/tg69/dwi_preproc/code/slspec.txt --s2v_niter=5 --s2v_lambda=1 --s2v_interp=trilinear" -eddyqc_all eddyoutput -nocleanup -scratch dwifslpreprocdir

#cd into most recently created fslpreproc working directory
cd dwifslpreprocdir

#Assuming eddy_quad has not been patched yet delete the empty dwi_post_eddy.qc directory
rm -df dwi_post_eddy.qc

#Run eddy_quad (eddyqc) from temporary working folder
eddy_quad dwi_post_eddy -idx eddy_indices.txt -par eddy_config.txt -m eddy_mask.nii -b bvals -f field_map.nii.gz

#Move eddyqc output to "eddyoutput" folder within main subject folder
mv dwi_post_eddy.qc ../eddyoutput
cd $working_dir/$subid/$sessid


###CREATE ANATOMICAL MASK
#create mean B0s (as nii) for registration
echo Creating mean B0 for registration
dwiextract dwi_denoised_unringed_preproc.mif b0_preprocessed.mif -bzero -force
mrmath b0_preprocessed.mif mean mean_b0_preprocessed.mif -axis 3 -force
mrconvert mean_b0_preprocessed.mif mean_b0_preprocessed.nii.gz -force

#copy freesurfer files for registration (as nii)
echo Copying freesurfer images for registration
if [ -d $fs_dir/$fs_id ]; then
        cp $fs_dir/$fs_id/mri/orig.mgz .
        cp $fs_dir/$fs_id/mri/brainmask.mgz .
elif [ -d $fs_dir/Motion/$fs_id ]; then
        cp $fs_dir/Motion/$fs_id/mri/orig.mgz .
        cp $fs_dir/Motion/$fs_id/mri/brainmask.mgz .
elif [ -d $fs_dir/Motion2/$fs_id ]; then
        cp $fs_dir/Motion2/$fs_id/mri/orig.mgz .
        cp $fs_dir/Motion2/$fs_id/mri/brainmask.mgz .
fi
mrconvert brainmask.mgz brainmask.nii.gz -force
mrconvert orig.mgz orig.nii.gz -force

#co-register T1 to DWI
echo "Co-registering FS and DWI"
if [ -d $fs_dir/$fs_id ]; then
	bbregister --s $fs_id --mov mean_b0_preprocessed.nii.gz --init-coreg --lta dwi2fs.lta --dti
	mri_vol2vol --mov brainmask.nii.gz --targ mean_b0_preprocessed.nii.gz --lta-inv dwi2fs.lta --o fs2dwi.mgz --no-resample
elif [ -d $fs_dir/Motion/$fs_id ]; then
	bbregister --s Motion/$fs_id --mov mean_b0_preprocessed.nii.gz --init-coreg --lta dwi2fs.lta --dti
	mri_vol2vol --mov brainmask.nii.gz --targ mean_b0_preprocessed.nii.gz --lta-inv dwi2fs.lta --o fs2dwi.mgz --no-resample
elif [ -d $fs_dir/Motion2/$fs_id ]; then
	bbregister --s Motion2/$fs_id --mov mean_b0_preprocessed.nii.gz --init-coreg --lta dwi2fs.lta --dti
        mri_vol2vol --mov brainmask.nii.gz --targ mean_b0_preprocessed.nii.gz --lta-inv dwi2fs.lta --o fs2dwi.mgz --no-resample
fi

#create T1 mask from co-registered image
echo "Creating co-registered T1 mask"
mrconvert fs2dwi.mgz fs2dwi.nii.gz -force
fslmaths fs2dwi.nii.gz -bin fs2dwi_bin.nii.gz
mrconvert fs2dwi_bin.nii.gz fs2dwi_bin.mif -force


###CREATE DIFFUSION MASK FOR BIAS CORRECTION
echo Create diffusion mask for csd
dwi2mask dwi_denoised_unringed_preproc.mif mask_dwi.mif -force
mrgrid fs2dwi_bin.mif regrid - -template dwi_denoised_unringed_preproc.mif -interp linear -datatype bit -force | maskfilter -  median mask_anat.mif -force
mrcalc mask_dwi.mif mask_anat.mif -and mask_combined.mif -force


###BIAS CORRECT, ESTIMATE RESPONSE FUNCTIONS, AND UPSAMPLE
#run bias correction using ANTS
echo Bias correct with ANTS
dwibiascorrect ants dwi_denoised_unringed_preproc.mif dwi_denoised_unringed_preproc_unbiased.mif -mask mask_combined.mif -force

	#QC bias correction
	mrconvert dwi_denoised_unringed_preproc.mif dwi_denoised_unringed_preproc.nii.gz -force
	mrconvert dwi_denoised_unringed_preproc_unbiased.mif dwi_denoised_unringed_preproc_unbiased.nii.gz -force
	echo $subid $sessid $(fslstats dwi_denoised_unringed_preproc.nii.gz -R) $(fslstats dwi_denoised_unringed_preproc_unbiased.nii.gz -R) >> $working_dir/voxel_intensities.txt

#estimate response function using DHOLLANDER
echo Estimate response function with DHOLLANDER
dwi2response dhollander dwi_denoised_unringed_preproc_unbiased.mif response_wm_unbiased.txt response_gm_unbiased.txt response_csf_unbiased.txt -voxels voxels_unbiased.mif -force

#upsample DWI
echo Upsample dwi
mrgrid dwi_denoised_unringed_preproc_unbiased.mif regrid dwi_denoised_unringed_preproc_unbiased_upsampled.mif -voxel 1.5 -force


###CREATE DIFFUSION MASK POST BIAS CORRECTION
#create final DWI mask and upsample
echo Create diffusion mask post bias correction, and upsample it
dwi2mask dwi_denoised_unringed_preproc_unbiased.mif mask_dwi_unbiased.mif -force
mrcalc mask_dwi_unbiased.mif mask_anat.mif -and mask_unbiased_combined.mif -force
mrgrid mask_unbiased_combined.mif regrid - -template dwi_denoised_unringed_preproc_unbiased_upsampled.mif  -interp linear -datatype bit -force | maskfilter -  median mask_unbiased_combined_upsampled.mif -force
