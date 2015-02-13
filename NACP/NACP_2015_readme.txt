{\rtf1\ansi\ansicpg1252\cocoartf1344\cocoasubrtf720
{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
\margl1440\margr1440\vieww10800\viewh14940\viewkind0
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural

\f0\fs24 \cf0 Tree Ring to biomass analysis for the 2015 NACP Conference in Washington DC.\
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural
\cf0 Ross TRW to biomass workflow:\
\
1) pregapfilling_TRW_formatting\
2) christy_gapfilling\
3)Post_GF_TRW_to_biomas\
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural
\cf0 \
\
Compared Niwot Ridge and Morgan Monroe State Forest\
\
Flurin Supplied Biomass estimates for Niwot Ridge, \
	plotB_detrended_chronos.txt\'97niwot plot B\
	plotC_dendtrended_chronos.txt\'97niwot plot C\
	site_biomass_increment_recon_plotB_kg_per_ha.txt\'97Niwot biomass for plot B kg/ha\
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural
\cf0 	site_biomass_increment_recon_plotC_kg_per_ha.txt\'97Niwot biomass for plot C kg/ha\
\
\
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural
\cf0 		\
Ross Supplied MMF data and code. \
	mmf_dated_allspp.rwl\'97dated MMF samples with all species present\
	mmf_undated.rwl\'97undated MMF samples\
	mm_all_trees.rwl\'97Dated and undated trees together.  This file gets plugged into R!\
\
	DOE_dbh_recon.csv\'97diameter reconstruction for DOE sites (only MMF as of now)\
	tree_data3.csv\'97Meta data of trees from DOE project\
	MMF_bm_spp_cum.csv\'97cumulative biomass estimates per species at MMF (Jenkins general 		hardwood Model)\
	MMF_bm_spp_inc.csv\'97biomass increment per species at MMF (Jenkins general hardwood		 model)\
		\
Used Christy\'92s gap filling scripts \'97gamm to fill in undated trees and fill in to pith\
			gap_filled_dbh.recon\'97	\
\
\
contains climate correlations using CRU/NCEP gridded data products\
	used Dave Frank\'92s fancy functions to carry out correlations\
	mmf.climate.csv\'97CRU/NCEP climate for MMF from 1901-2010\
	niwot.CRUNCEP.climate.csv\'97CUR/NCEP climate for MMF from 1901-2010	\
\
\
Scripts\
	allfunctionsdavid.v13.R\'97Dave Franks Fancy functions\
	ameriflux_maps_NACP15_poster.R\'97scripts for map generation\
	aNpp_FLuxes_cliamtecorr2\'97climate correlations between TR indices and CRU/NCEP Climate 		and fluxes\
	christy_gapfilling\'97contains proceedures for gamm gap filling, RUN AFTER: 		pregapfilling_TRW_formatting.R\
	compare_biomass_nep\'97Flurin\'92s code for comparing tower NEP to TR estimates of biomass.  		Ross Modified (original: Niwot_MMF_compare_biomass.R)\
	correls_poster_Ross\'97Flurin\'92s script for fancy climate correlations between TR indices and 		CRU/NCEP climate \
	mmf.detrending\'97dplR code for detrending MMF chronologies\
	mmf_NACP_abstract (all files with this name)\'97Scripts used to generate rough figures for 		abstract submission.  Overlook and use full analysis scripts, files, and figures.\
	Post_GF_TRW_to_biomass\'97Changes gap-filled ring widths into biomass estimates.\
\
\
	}