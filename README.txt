
Data for manuscript: Neural Mechanisms of Creative Problem Solving - From Representational Change to Memory Formation
Authors: Becker, Sommer, Cabeza (2024)

Abstract:
Despite the need for innovative solutions to contemporary challenges, the neural mechanisms driving creative problem-solving, including representational change and its relation to memory, still remain largely unknown. We focused on the powerful creative process of insight, wherein rapid knowledge reorganisation and integration—termed representational change—yield solutions that evoke suddenness, certainty, positive emotion, and enduring memory. We posit that this process manifests as stronger shifts in activation patterns within brain regions housing solution-relevant information, including the visual cortex for visual problems, alongside regions linked to feelings of emotion, suddenness and subsequent memory. Our findings substantiate these hypotheses, revealing stronger representational changes in visual cortex, conjoined with activations in the amygdala and hippocampus—forming an interconnected network. Importantly, the representational change and hippocampal effects serve as predictive markers of subsequent memory. This study provides evidence of an integrated insight mechanism influencing memory. Implications extend to education, emphasising the significance of discovery-based learning approaches for boosting knowledge retention.

This repository contains all behavioral and aggregated RSA single trial and preprocessed univariate fMRI data relevant for 
all quantitative output for the above mentioned manuscript. 


1. System requirements & Installation guide
- R version 4.3.0   https://cran.r-project.org/bin/windows/base/
- RStudio    https://posit.co/download/rstudio-desktop/
- Installation takes ~5-10 min 


2. Instructions for use

2a. Main (fMRI): Mooney identification task + subsequent memory task
- download all the files from the repository (keep folder structure as is)
- open file BeckerSommerCabeza_2023a_main.R  in RStudio
- install relevant packages (load libraries: line 30-46; installation time: 5-10min)
- change the working directory in line 73 to where to downloaded the data
- run the code (pressing "source" or line by line pressing "run")
- expected run time (normal desktop computer: ~3min)

->  Expected output
	1) Behavioral data analysis (lines: 76 - 473)
	                - measurement model analysis on insight measure (lines: 219-226)
			- demographics (lines: 284-293)
			- insight/performance/memory measures (lines: 298-354)
			- false alarm rate(recognition) (lines: 398-403)
			- insight memory effect (lines: 405-454)
			- correlation insight experience anagrams / Mooney image identification (lines: 458-471)
					
	2) Univariate ROI analysis Amygdala & anterior/posterior hippocampus (line: 474-710)
	                - insight effect: single trial (standard space) (lines: 493-532)
			- insight effect: parametric modulation (subject space) (lines: 538-591)
			- exploratory: which dimension is driving univariate insight effects (lines: 594-602)
			- insight memory effect (standard space) (lines: 604-637)
			- exploratory: which dimension is driving univariate insight memory effects in aHC (lines: 639-674)
			- control analysis: univariate effects in VOTC-RC areas (lines: 675-710)
					
	3) multivariate RSA analyses (lines: 711 - 932) 
			- Multivoxel Pattern Similarity - Insight (lines: 725-748)
                        - Multivoxel Pattern Similarity - Insight Memory (lines: 750-792)
			- RSA - AlexNet - Insight (lines: 793-827)
                        - RSA - AlexNet - Insight Memory (lines: 828-862)
			- RSA - Word2Vec- Insight (lines: 863-898)
                        - RSA - Word2Vec- Insight Memory (lines: 899- 932)
					
	4) Plotting all outputs (lines: 845-877)


2b. Control: Object recognition experiment
- download all the files from the repository (keep folder structure as is)
- open file BeckerSommerCabeza_2024_controlexp_objrecognition.R  in RStudio
- change the working directory in line 46 to where to downloaded the data
- run the code (pressing "source" or line by line pressing "run")
- expected run time (normal desktop computer: ~30sec)

->  Expected output
	1) Descriptive data from experiment and response time  (lines: 85-86)
	2) Response time distribution (lines: 62-78)
	3) Response time parameters (lines: 81-83)


2c. Control (behavioral): Mooney identification task + subsequent memory task
- download all the files from the repository (keep folder structure as is)
- open file BeckerSommerCabeza_2024_controlexp_behavior.R  in RStudio
- change the working directory in line 27
- run the code (pressing "source" or line by line pressing "run")
- expected run time (normal desktop computer: ~30sec)

->  Expected output
	1) Demographics lines 33ff
	2) Accuracy adjustment (to approximate correctly counted but wrong examples in fMRI sample line 107
	3) Behavioral (performance) data: (lines 268:324)
	4) False Alarm Rate (line 189)
	5a) Accuracy effect (line 340)
	5b) Solution time effect (line 348)
	6) Insight Memory Advantage (categorical/continuous): (lines 357:365; 377:383)
	7) Figure S7-A:D: (lines: 463:467)