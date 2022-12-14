#!/bin/bash

work=/cbica/projects/pnc_fixel_cs

#Average response function from template subjects
responsemean \
	${work}/data/MRtrix_recon/template/sub-*/sub-*_response_wm.txt \
	${work}/data/MRtrix_recon/template/av_wm_response_new.txt \
	-force

responsemean \
        ${work}/data/MRtrix_recon/template/sub-*/sub-*_response_gm.txt \
        ${work}/data/MRtrix_recon/template/av_gm_response_new.txt \
	-force

responsemean \
        ${work}/data/MRtrix_recon/template/sub-*/sub-*_response_csf.txt \
        ${work}/data/MRtrix_recon/template/av_csf_response_new.txt \
	-force
