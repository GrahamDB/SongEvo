## Test environments
* local OS install (MacOS Mojave version 10.14.2), R 3.5.2 (using devtools::check)
* win-builder (i.e. devtools::check_win_devel and devtools::check_win_release)
* R-Hub (Windows Server 2008 R2 SP1, R-devel, 32/64 bit)
* R-Hub (Ubuntu Linux 16.04 LTS, R-release, GCC)

## R CMD check results
* There were no ERRORs or WARNINGs.
* For OS and Windows Server 2008 through R-Hub, there were no NOTEs. 
* For win-builder and Ubuntu Linux through R-Hub, there was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Raymond Danner <dannerR@uncw.edu>'

New submission

Possibly mis-spelled words in DESCRIPTION:
  SongEvo (12:26, 13:5)
  
>Response from author (RMD): this NOTE accurately describes that this is a new submission.  SongEvo is not mis-spelled.
