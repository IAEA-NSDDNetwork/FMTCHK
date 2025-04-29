# FMTCHK
ENSDF format and syntax checking. 

FMTCHK is part of the [ENSDF Analysis and Utility Programs](https://nds.iaea.org/public/ensdf_pgm/).

Address any feedback to the Brookhaven National Nuclear Data Center  NNDC@BNL.GOV

## Change history

#### 2022-10
The version is now changed to the latest, 10.5d. The source code was tested on Windows, Linux, and Mac OS with gfortran. 
The Makefile will build an executable for the appropriate operating system with the "make" command.

#### 2022-09
Removed inconsistent warning message, E > Q + Parent decay level

#### 2018-11
Version 10.5d

Added lower case check to dsid            

#### 2016-01
Removed check for NP to be equal to BR on N card.
Suppression of many warning messages, e.g., absence of MR for mixed multipolarities, Y2K check, messages when T, L, or S are relabeled, QP is negative.
The NNDCLIB library included in the package has been modified to compile with gfortran, intel fortran, LF 95 compilers on all platforms

## Disclaimer

Neither the author nor anybody else makes any warranty, express or implied, or assumes any legal liability or responsibility for the accuracy, completeness or usefulness of any information disclosed, or represents that its use would not infringe privately owned rights.
