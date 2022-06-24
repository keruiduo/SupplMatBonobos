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
- 6_Analysis_UMAP_and_Silhouette.html: The code and explanations to compute and display various UMAP and silhouette scores for the raw data and the outputs of different classifiers
- 7_Example_mlr_svm.html: The code and explanations for a simple ML pipeline which consists in using svm to assess the classification of Bonobo individual vocal signatures (with the mlr package in R)
