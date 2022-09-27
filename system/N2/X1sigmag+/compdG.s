#!/bin/tcsh
#
#   The program is to compare the deltaG_cal with the 
# deltaG_theory or deltaG_exp .

# fort.21 ---  the file of experimental deltaG .
# fort.23 ---  the file of calculated   E .
#
# fort.14 ---  the compared result of deltaG .
# fort.15 ---  average of energy percentage errors for each alam.

  set  ala = $argv[1]
#-----------------------------------------------------------
#  num ------   the number of compared deltaG .
#                 num = # of experiment/theory energies.
# naucm1 ----  = 0 , read Evib_cal in a.u. generated from previous code
#              = 1 , read Evib_cal in cm_1
# naucm2 ----  = 0 , read Evib_exp in a.u. generated from previous code
#              = 1 , read Evib_exp in cm_1
# nexpt -----  = 0 , read experimental energies from fort.21 in a.u.
#              = 1 , read in cm_1 from fort.21
# alam ------  = Lambda,  is the variational adjustable 
#                             parameter in V_ecm(R).
#-----------------------------------------------------------
#         num  naucm1  naucm2  nexpt  alam
  echo "  22     0      1       1     $ala  " >! fort.5
# echo "  91     0      0       1     $ala  " >! fort.5

    cp    deltaG.exp    fort.21

  nice +4 compdG.x < fort.5 >! fort.14

  cp fort.14 deltaG.comp
