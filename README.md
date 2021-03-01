# semLavaan
This is my final attempt to use Lavaan. I think I get it now as to how Lavaan works and how SEM in general works.

IMPORTANT SCRIPT:

1. recodeData.R 
=> explain and recoding items into numerical item for Lavaan analysis

2. step1.R
=> loading all necessary packages 

3. step2.R
=> all data preprocessing
=> this script handle all missing values using MICE processes
=> after further thoughts and reading and rendering the data using MICE for imputation (prior to this writing), I've come to a decision that, MICE will only be using ON the desired variables ONLY and will not be imposed on the entire dataset because of the use of the computational power that MICE needed as well as due to the time taken (it took to process, freaking 6 hours +) for the machine to process entire datasets

4. step3.R
=> adding core analysis code using Lavaan
=> this script handle all the analyses
=> after further study on how Lavaan works for SEM, I think I'm ready!
=> the github page: https://benwhalley.github.io/just-enough-r/mediation-and-covariance-modelling.html really help me further understand how to properly perform the analysis
=> they literally show steps by steps, and hopefully will help me finishing my papers and my thesis writings!
