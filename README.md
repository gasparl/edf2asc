# Eyelink EDF to ASCII conversion on Windows

Place all .edf files in the x directory. (Extension must be lowercase.) From the terminal, run the following at the path of the x directory:
sh e2a.sh

The .edf file(s) will be in the edf directory, while the asc directory will have the .msg (events) and .dat (eye tracking data) files in ascii format.

### example analysis

You can process the .msg and .dat files e.g. as done in the example_analysis.R script. (The related script in the saccade_detection folder are from Microsaccade Toolbox 0.9; Engbert, R., Sinn, P., Mergenthaler, K., & Trukenbrod, H.)
