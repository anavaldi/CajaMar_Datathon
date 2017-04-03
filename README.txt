The files in this .zip contains:

- DataShots_UGR_Final.pdf: Final presentation of DataShots. We explain how we have faced the challenge.
- functions.R: Script with several functions. It is applied in New_Variables_FullSeq.R.
- New_Variables_FullSeq.R: Reads train2 and test2 and create new variables (the most important is the seq variable).
- ./data/fullseq2.zip: Output of New_Variables_FullSeq.R. Input of RecommendationEngine_exp1_X.R, RecommendationEngine_exp2_X.R and 
  XGBooost_2_Data_Preprocessing_Model.R.
- RecommendationEngine_exp1_X.R: Code of RSM 1.
- RecommendationEngine_exp2_X.R: Code of RSM 2.
- XGBooost_2_Data_Preprocessing_Model.R: Code of XGBoost 2. Reads the output of New_Variables_FullSeq.R, creates new ones and builds XGBoost models.
- XGBoost_MisionTest.R: Write validation and test predictions. Computes accuracy.
- Test_Mission.txt: Results of test customers with 1 predicted product.
- Test_Mission_5_Products.txt: Results of test customers with 5 predicted products.