# aq_mp_tox_shiny

**Repository for Shiny application for Aquatic Microplastics Toxicology Review project**

*Southern California Coastal Water Research Project*

Contributors: Heili Lowman, Leah Thornton Hampton, Scott Coffin, Emily Darin

Primary data contact: Leah Thornton Hampton (leahth@sccwrp.org)

Primary coding contact: Heili Lowman (heilil@sccwrp.org)



## To-do List

**Scott**

*Scott* Make app faster (specifically SSD) 
*Scott* Make plots piecewise

**Heili**

*Heili* Help fix "submit button" in SSD 
*Heili* Make sure app works on web app 
*Heili* Add widget for data input 
*Heili* Add button to plot corrected vs ‘raw’ SSDs at will etc.

**Leah**

*Leah* Number to mass conversion. Consider using the average obtained from Koelmans' distributions. Note they are now revising these to make them compartment specific and with higher quality data. Bart will keep us posted.

*Leah* Add a translocation size boundary and slope of the PSD that a user could set- would both affect bioavailability.

*Leah* Effect metrics. Add type of effect threshold concentration metric as a categorial variable, but also add a column of assessment factors; ‘AF_dose descriptor’. This would allow users to align thresholds.

*Leah* Exposure duration. USe assessment factors that also roughly align data obtained for differences in duration. Add a column for the ‘AF_time’. Include AF options for different regulatory frameworks

*Leah* MP bioavailability. Assess maximum ingestible particle size for all species and add a column for that. Use existing values in Koelman's SI. Then the # of ingestible particles can be expressed as a fraction of the # of particles for the 1 – 5000 um size continuum of environmental MP.

*Leah* Add rescaling methods to align threshold effect concentrations assessed for particle of different size, assuming food dilution as a ‘baseline toxicity’ mode of action for plastic.

*Leah* If you include surface charge, you would have to include pH and salinity.

*Leah* If you include in vivo studies with organic chemicals on MP, you would need to include organism lipid content

