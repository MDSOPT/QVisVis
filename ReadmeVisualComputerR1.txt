Code for QVisVis Solution Agreement Visualization and Utitiles Package

The files included are listed as follows.

VisualComputerR1Files.zip - The files used to create the visualizations for the Visual Computer revision 1 submission.

3DCode.R - Creates test manifolds for the framework.
CalcPartialAgreements.R	- Given agreements between multcple onfigurations, with each comparison relative t a base configuration
CategoryAgreeTableCompare.R	- Creates a table of comparison agreements for two derived configurations with an original configuration across a data categorization.
CategoryAgreeTableSingle.R     - Creates a table of agreements a derived configuration with an original configuration across a data categorization.
GenAgree.R - Calculates agreement across k between two configurations
GenAgreeDist.R	-Calculates agreement across k between two configurations given distance matrices
HMapCategoryTableComparegg.R  - Utilizes output from CategoryAgreeTableCompare.R to give a heatmap of a category table agreement comparison
HMapCategoryTableSinglegg.R - Utilizes output from CategoryAgreeTableSingle.R to give a heatmap of a category table agreement
HMapComparegg.R	- A heat map of the differences in agreement between each of two derived configurations and a source configuration across k
HMapSinglegg.R	- A heat map of the  agreement between a derived configuration and a source configuration across k
PartialAgree.R	- Given agreements between three configurations, creates partial agreements in a manner analogous to partial correlation coefficients.
PLiftComparegg.R - A lift diagram/ROC curve with agreement of two derived configurations vs. a source configuration plotted across across k relative to expected agreement.
PLiftSinglegg.R	Y - A lift diagram/ROC curve with agreement of a derived configurations vs. a source configuration plotted across across k relative to expected agreement.
PlotAgree3D2Dgg.R - For a dimensionality reduction from 3 to 2 dimensions, plots the agreement results on both the source 3D map and the destination 2D map.
ScatterCompare3D.R - A scatterplot showing of the differences in agreement between each of two derived configurations and a source configuration for set k on 3D plot of source configuration.
ScatterCompareAnimategg.R - Animate ScatterCompare3D.R across a set of parameter values
ScatterComparegg.R -  - A scatterplot showing of the differences in agreement between each of two derived configurations and a source configuration for set k on 2D plot of destination configuration.
ScatterSingle3D.R - A scatterplot showing of the agreement between a derived configuration and a source configuration for set k on a 3D plot of source configuratio.
ScatterSingleAnimategg.R - Animate ScatterSingle3D.R across a set of parameter values
ScatterSinglegg.R - A scatterplot showing of the agreement between a derived configuration and a source configuration for set k on 2D plot of destination configuration.
wfLinearDecline.R - Creates a set of weights that linearly decline with k for GenAgree.R and GenAgreeDist.R
wfLinearIncline.R - Creates a set of weights that linearly incline with k for GenAgree.R and GenAgreeDist.R