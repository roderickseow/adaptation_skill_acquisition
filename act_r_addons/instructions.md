Instructions for running a model simulation (Windows / Mac)

Installing ACT-R
-------------------

* Download the ACT-R standalone software for your OS from: http://act-r.psy.cmu.edu/software/
    * Direct download: [Windows](http://act-r.psy.cmu.edu/actr7.x/win-standalone.zip); [Mac](http://act-r.psy.cmu.edu/actr7.x/mac_standalone.zip)
* Decompress the downloaded package; the main directory should be "ACT-R"
* Download the ["act_r_addons" folder](https://github.com/roderickseow/adaptation_skill_acquisition/tree/main/act_r_addons)
* Copy the downloaded files into the "ACT-R" folder, replacing files as necessary

Installing the game server
---------------------------

* Download the server package for your OS from this repository
    * [Windows](https://github.com/roderickseow/adaptation_skill_acquisition/blob/main/SpaceFortressServer-win.zip); [Mac](https://github.com/roderickseow/adaptation_skill_acquisition/blob/main/SpaceFortressServer-mac.tgz)
* Decompress the downloaded package in the "ACT-R" folder

Running a simulation
--------------------------
* Model files are located in the "model_code" folder in this repository
* Place the .lisp file of the model of interest into the "ACT-R" folder
* Run "SpaceFortressServer" 
    * For Windows users, the application is in the "SpaceFortressServer-win32-x64" folder
    * For Mac users, the application is in the "SpaceFortressServer-mac" folder
* Follow the instructions provided in the "ACT-R" folder to start running ACT-R
* In the ACT-R terminal, enter with the parentheses: (load "sf-model-runner.lisp")
    * You might have to change the working directory to the "ACT-R" folder: (:CD "\<Insert path to ACT-R folder\>")
* Enter: (load "\<Insert model filename\>")
* Enter: (run-condition 1 40 1 T)
    * This will run 40 games in condition "MMMMM" with a visual display of the gameplay on the server window
    * Feel free to replace 40 with a smaller number n where n is the number of games; e.g. (run-condition 1 1 1 T) will run 1 game instead
    * To run the model with other conditions:
        * LLLLM: (run-condition 2 n 1 T)
        * HHHHM: (run-condition 3 n 1 T)
        * LHLHM: (run-condition 4 n 1 T)
        * HLHLM: (run-condition 5 n 1 T)
        * HHHHH: (run-condition 1 n 2 T)
        * LLLLH: (run-condition 2 n 2 T)
        * MMMMH: (run-condition 3 n 2 T)
        * LMLMH: (run-condition 4 n 2 T)
        * MLMLH: (run-condition 5 n 2 T)