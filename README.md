# On-Multifractionality-of-Spherical-Random-Fields-with-Cosmological-Applications

#This repository includes the Python codes and R codes which were implemented to obtain the results of my third PhD project titled as "On Multifractionality of Spherical Random Fields with Cosmological Applications".

#Instructions on how to run the python files:

These python files were taken from Wang(2021) which were used in the paper by Hamann, et al(2021) and they were modified 
to obtain the AC discrepancy values for Planck SMICA 2015 map.

Instructions:

We strongly recommend running these files on a supercomputer as these files need high memory and computational power.

Please note that these python files depend on the python package "healpy". Therefore, the codes could only be run within a Linux environment. 

[1] First, run the "coef_cmb.py" python file. It will generate the .mat files, "cmbmap.mat" and "smica2015_I_inpainted_HL2048.mat".

[2] Next, run the "ACDsmica15.py" python file. It will generate the .mat file, "ACDSMICA2015_L1500_HL1024_lag10.mat" and the 
    "acd_SMICA2015.txt" file.

[3] Finally, run the "ACDplt.py" python file to obtain the AC discrepancy map for Planck SMICA 2015.


References:

[1] Hamann, J., Gia, Q. T. L., Sloan, I. H., Wang, Y. G., & Womersley, R. S. (2021).
     A new probe of Gaussianity and isotropywith application to cosmic microwave background maps.
     To appear in Internat. J. Modern Phys. C, (pp. 1–26).

[2] Wang, Y. G. (2021). CMBProbe, the Python package for generating AC discrepancy maps.
     https://github.com/wangyg19/CMBProbe. Accessed 23 March 2021.
