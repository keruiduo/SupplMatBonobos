# SupplMatBonobos
Supplementary material for the paper 'Improving the workflow to crack Small, Unbalanced, Noisy, but Genuine (SUNG) datasets in bioacoustics: the case of bonobo calls'

This repository contains the following files:
- dataset_raw.txt: The raw data for the whole study
- dataset_processed: The processed data as they are saved after the curation phase
- 1_Curation.html: The code and explanations for the initial curation phase of the data
- 2_pDFA.html: The code and explanations for the permuted discriminant analysis
- 3_Hyper-Parameter_Tuning.html: The code and explanations to perform hyper-parameter tuning for SVM, neural networks and xgboost for different set features
- 4_Computing_results.html: The code and explanations to compute all needed results to assess classification performances for various classifiers (including stacked learners), different features sets and different management strategies for sequences of calls
- 5_Analysis_Classification_Performances.html: The code and explanations to analyze classification performances in different configurations (classifiers, feature sets, management of sequences of calls)
- 6_Analysis_SUMAP_and_Silhouette.html: The code and explanations to compute and display supervised UMAP and silhouette scores for individual signatures and call types
- 7_Example_mlr_svm.html: The code and explanations for a simple ML pipeline which consists in first conducting an exploration of the data with a supervised UMAP (with the uwot package in R) and then using svm to assess the classification of Bonobo individual vocal signatures (with the mlr package in R)
- The raw and processed datasets (the processed dataset is the output of the curation phase)
- The file 'mlr_dense_nn.R' contains functions to add dense neural networks to the mlr ML framework
- The files 'Bali - GA.R', 'rules - Super class.R', 'rules - Rules for groups.R' and 'rules - Rules for rows.R' contain the code for the in-house genetic algorithm to study the different strategies for the sequences of calls
- The file 'Comparing the classification performances with different MFCC sets.pdf' contains a brief comparison of the classification performances of SVM for call types and individual signatures with different MFCC sets

Please note that html files cannot be easily visualized in Github. The simplest approach is to download all the files (as a zip archive) and then open them with any browser.

DISCLAIMER: This code is provided AS IS without express or implied warranty.

% Developed by Christophe Coupé, The University of Hong Kong (Hong Kong) & Vincent Arnaud, Université du Québec à Chicoutimi (Canada)

% Last modified: October 2022
