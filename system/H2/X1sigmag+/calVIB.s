#!/bin/tcsh
#           calVIB.s
#
#  NUM is the number of the calculated energy state ; 
#  It shoule be less than the number of the bound state .

   set NUM = 22 
 
   echo " $NUM "    >! fort.5

#           We                Wexe            Weye
   echo "   1.074643D-02      6.526677D-05   -1.028821D-08 " >> fort.5

   nice +4 calVIB.x < fort.5 >! fort.14 
