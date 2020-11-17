adaptation_skill_acquisition
===============================

Created by Roderick Seow

Data
-------------

data/human_per_game.csv
* Table of measures per game collected from human participants

data/models_slowdown_per_game.csv
data/models_noslow_per_game.csv
data/models_decay_per_game.csv
* Table of measures per game from ACT-R models
* slowdown + noslow for first set of model comparisons
* decay for second set of model comparisons

data/control_values_est_best
* Table of controller parameters per game from ACT-R models
* Contains parameters for individual models and each control parameter

Analyses
-------------------

analyses.Rmd
analyses.nb
* .Rmd file contains the analyses presented in the manuscript
* To run analyses, set the directory in the analysis script to where the data files are downloaded.

Model code
-------------------

model_code/first_set/
* Contains model codes for models in first set of comparisons
* File name indicates set parameters: weight-(1/4/8)-exp-(1/0995/05)-slow-(y/n)-thrust-(0067/001)

model_code/decay_set/
* Contains model codes for models in second set of comparisons
* Models only vary in decay parameter (0.9/0.99/0.991-0.999)

<<<<<<< HEAD
Model simulation
=======
Model code scripts will require ACT-R software to run - please email yseow@andrew.cmu.edu if interested.

Analyses
>>>>>>> ec7d2439d278b093c439371bb9b4fb801bcbf8f8
-------------------
act_r_addons/
* Contains files and instructions for running a model simulation

<<<<<<< HEAD
=======
analyses.Rmd
analyses.nb
* .Rmd file contains the analyses presented in the manuscript
* To run analyses, set the directory in the analysis script to where the data files are downloaded.
>>>>>>> ec7d2439d278b093c439371bb9b4fb801bcbf8f8
