# Insight_Memory_Effect
Data for manuscript investigating the Insight & Insight-realted better memory (Becker, Sommer, Cabeza, 2023)

Abstract:
Creative solutions often involve insight - a rapid reorganisation and integration of internal representations (representational change, RC) - triggering positive emotions, and a feeling of suddenness about the solution. This insight process is associated with a more profound memory consolidation but its neural basis remains unclear. Here, we propose that insight reflects sudden multivariate pattern changes associated with enhanced solution-relevant representations (RC) efficiently integrated into the solution process. This sudden conceptual update (RC) leads to awareness of the solution accompanied by an emotional and suddenness response in hippocampus and amygdala resulting in enhanced encoding of the solution. While under-going fMRI scanning, participants completed a visual task (Mooney images) whose solution is often accompanied by insight and participants’ memory for the solution was tested five days later. The study yielded four main findings. First, multivariate pattern analyses showed that when solving Mooney images was accompanied by insight, posterior fusiform gyrus (pFusG) and inferior lateral occipital cortex (iLOC) exhibited a sudden conceptual update and this information was more efficiently integrated into a network of solution-processing brain areas. Second, this network included amygdala and hippocampus who exhibited increased insight-related activity during solution. Third, this sudden conceptual update in pFusG and iLOC as well as anterior hippocampal activity predicted insight-related better memory. This is the first study providing direct empirical evidence for a plausible insight-related RC mechanism and its relationship to enhanced long-term memory formation. These findings highlight the importance of fostering insight-driven learning experiences as an effective way for problem-solving and knowledge retention.

This repository contains all behavioral and aggregated RSA single trial and preprocessed univariate fMRI data relevant for 
all quantitative output for the above mentioned manuscript. 


1. System requirements & Installation guide
- R version 4.3.0   https://cran.r-project.org/bin/windows/base/
- RStudio    https://posit.co/download/rstudio-desktop/
- Installation takes ~5-10 min 


2. Instructions for use
- download all the files from the repository
- open file BeckerSommerCabeza_2023a_main.R  in RStudio
- install relevant packages (load libraries: line 29-50; installation time: 5-10min)
- change the working directory in line 77 to where to downloaded the data
- run the code (pressing "source" or line by line pressing "run")
- expected run time (normal desktop computer: ~3min)

3. Expected output
	1) Behavioral data analysis (lines: 82 - 343)
	                - latent factor analysis on insight measure (lines: 132-139)
					- insight memory effect (lines: 247-343)
					
	2) Univariate ROI analysis (line: 346-656)
	                - measuring AHA experience (lines: 346-553)
					- insight memory effect (lines: 559-656)
					
	3) RSA analysis - measuring representational change (line: 658-841)
                    - insight-memory effect (lines: 751:781; 813:841)
					
	4) Plotting all outputs (lines: 845-877)
