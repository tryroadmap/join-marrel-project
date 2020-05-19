# Data

### File descriptions
* train.csv - the training set
* test.csv - the test set
* sample_submission.csv - a sample submission file in the correct format
### Data fields table
#### Column Name	Description
- Stocknum	Stock number  inventory number
- Modelname	Categorical
- voltage	230 or 115
- Thickness	Categorical
- Keyfeature
- Keyfeature2
- Weight
- Realstocknum


### File descriptions Smart-selector-decision.csv
* train.csv - the training set
* test.csv - the test set
* sample_submission.csv - a sample submission file in the correct format

### Data fields table Smart Selector Decision
#### Column Name	Description
- id	The table is partitioned for this column(?)
- Process	Engine, mig, plasma, stick, tig
- Voltage	230 V, 115 V & 230 V, 115 V
- Metaltype	Steel type - Mostly Steel and Stainless, Some Aluminum; All Aluminum

- Thickness	Steel / Stainless / Chrome-Moly;
- Measurement - Less than 1/16; ¼ - 5/16; Over 3/8; 5/16th of greater; 3/16 – 1/4; 1/16th or less
- Portability	Portable or mobile   OR Often, Rarely, or Never (portable)
- Usage	Frequent, Medium, Low
- Simul	Yes, Some fluctuation is OK; Yes Arc quality not affected, No;
- Weldproc	Stick, Wire, TIG; Stick
- Soundlevel	Less Noise; Standard
- Message	We recognize that you selected Standard Sound Level, but based on other selections, here is the product we recommend.


- Recmodel
- Recnote
- Alt1
- Alt1note
- alt2note
