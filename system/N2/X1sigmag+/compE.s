#!/bin/tcsh
#         compE.s
# The program is to compare the Evib_cal and the Evib_theory or Evib_exp .

# fort.21 ---  the file of experimental deltaG .
# fort.23 ---  the file of calculated   E .
#
# fort.14 ---  the compraed result of Evib .

#  num -------  the number of compared Evib .
# naucm1 -----  = 0 , read Evib_cal in a.u.
#               = 1 , read Evib_cal in cm_1
# naucm2 -----  = 0 , read Evib_exp in a.u.
#               = 1 , read Evib_exp in cm_1

#         num      naucm1 naucm2
  echo "  22       0      0         " >! fort.5

  cp deltaG.exp    fort.21

  nice +4 compE.x < fort.5 >! fort.14



