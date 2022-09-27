#!/bin/tcsh    
#                vibdm.s
#  To calculate the bound energy of diatomic molecular potential :
#  
# 1.)   SMMS potential :
#     V(x) = -De*beta*(1.0/beta + a1*x + a2*x**2 
#                               + a3*x**3 +... )*exp(-a1*beta*x)
#    The cofficients a(n) of Sun's Modified Murrell Sorbie (SMMS)
#  potential are the functions of De, beta, and vibrational constants.
#  
# 2.)   Huxley-Murrell-Sorbie (HMS) potential :
#          V_MS(R)=-De*(1 + a1*x + a2*x**2 
#                                + a3*x**3 + ...)*exp(-a1*x)  
#
# 3.)   Sun-Feng (ECM) potential :
#          V_ecm(R) = V_MS + Lamta(R)*delta_V(R)
#          V_MS:  Murrell & Sorbie potential in which the
#                 (a1,a2,a3) are calculated using SF formulae.
#          Lamta(R)   = lamta*(x/R)*[1-exp(-lamta^2*x/R)]
#          delta_V(R) = V_MS - V_0
#                                For  R < Re :
#     ntn = 0, V_0 = V_Morse   , V_ecm(R) = V_ecm;
#         = 1, V_0 = V_Morse   , V_ecm(R) = V_MS ;
#         = 2, V_0 = V_Morse   , V_ecm(R) = V_0  ;
#         = 3, V_0 = V_Rydberg , V_ecm(R) = V_0  ;
#         = 4, V_0 = V_PG      , V_ecm(R) = V_0  .
#
#    If you want to use the other potential , please modified 
#  the corresponding cofficients and the FUNCTION POT in vibdm.f .
#  
#    Please adjust the NCC1,ST1I,ST1F so as to converge the Energy !!!
#    The bound energy is less than zero, the energy above zero 
#  is continuous energy !!!
#======================================================================

#  set SF = 0    #  pot = SMS or Huxley-Murrell potential
   set SF = 1    #  pot = SF potential
#
#   If the potential computed is not the above ones. Please add
# function POT(R) in vibdm.f and add parameter SF in vibdm.s
#
   set  De   = $argv[1]
   set  beta = $argv[2]
   set  mal  = $argv[3]  
   set lamta = $argv[4]
   set    ms = $argv[5]
   set   ntn = $argv[6]

#----------------------------------------------------
#  mal == 1, read coeff. a(i) from fort.5;
#       = 2, read a(i) generated from previous code. 
#   ms -- number of terms in V(R) expansion.
#----------------------------------------------------
#         mal
  echo " $mal "    >! fort.5
#-----
#         ms  Nturn
  echo " $ms   $ntn  "   >> fort.5
#-----
#         Title
  echo "   VIBRATIONAL LEVELS OF N2-X1sigmag+ "  >> fort.5
#-----
#        Imu = 1 ,to use the atomic mass respectively .
#            = 2 ,to use the molecular mass .
#  echo " 1 "  >> fort.5
   echo " 2 "  >> fort.5
#-----
#    amA -- The mass of the atomic A;
#    amB -- The mass of the atomic B;
#                 amA & amB use non-atomic unit  !!!
#          amA        amB
  echo " 14.00       14.00  "    >> fort.5
#-----
#    amu -- The reduced mass of molecule AB
#           amu
  echo " 7.00153720             "   >> fort.5
#-----
#        N -- The number of the roots; N = nbmax in secentv.s .
#  delta_E -- The  E_step (cm-1) used in renormalization .
#         N  delta_E
  echo "  24     5    " >> fort.5  
#=====
# !!! Next three numbers will have the effect on the accuracy 
#     of the energy. Adjust them until the energy converge.   
#   NCC1 -- number of potential points;                     
#   ST1I -- R_left,  R_min for V(R);   
#   ST1F -- R_right, R_max used to cal. R_step :
#            R_step = (ST1F - ST1I)/(NCC1-1) = HH1
#         NCC1    ST1I    ST1F                    
  echo "  12000   0.8     8.0   " >> fort.5
#--- 
#      De -- molecular dissociation energy.
#    beta  = 1.0, for SF potential function.
#  alamta  = variational parameter in SF V(R).
#      SF  = 1, use SF V(R); = 0, use MMS.
#      Re -- equilibrium internuclear distance.
#          De   beta    lamta   SF    Re
# echo  " $De  $beta   $lamta  $SF  2.0743  "  >> fort.5
  echo  " $De  $beta   $lamta  $SF  2.020   "  >> fort.5
#--- 
#              We
  echo  "   1.074643D-02   "  >> fort.5 
#---                                          
#----------------------------------------------------------
# The following codes are written by Weiguo Sun, but as if 
# now they are NOT used by ECM. (They are for other purposes.)
#                                             ***** ********
# mset -- number of set of solution vectors  X;
#           usually, set  mset =< 20.          
# mcst -- number of vib. constants in a vector X;
#           usually, set  mcst =< 6.             
#             A(mcst, mcst);  b(mcst, 1)
#   mv -- number of vib. states in A used to 
#           solve linear equation    A * X = b
#             set  mv =< 95.                  
#    X = ( We, WeXe, WeYe, WeZe, WeTe, WeSe, ...)
#                                                
#        mset mcst  mv 
# echo  "  3    4   12   "  >> fort.5 
  echo  "  4    5   12   "  >> fort.5 
#---
# nvs(i) -- value of initial vib. quantum state 
#           v+1 in each set of mset vectors, e.g.
#               i  = 1, 2, 3, 4, 5   (mset=5)
#           nvs(i) = 1, 3, 6, 8, 9
#            v_ini = 0, 2, 5, 7, 8
#
#    When mcst=6, the states used in a set are :
#  nvs(1)=1 :  v = 0, 1, 2, 3, 4, 5.
#  nvs(3)=6 :  v = 5, 6, 7, 8, 9,10.
#
#         nvs(i) = 1, mset
  echo  "  1   2   3   4   "  >> fort.5 
# echo  "  1   2   4   6   "  >> fort.5 
#---
 if ($mal == 1) then     # Coefficients an !
#             a1   
   echo  "  1.455674E+00  " >> fort.5
#             a2   
   echo  "  -2.300690E-03 " >> fort.5
#             a3   
   echo  "  1.817460E-06  " >> fort.5
#             a4   
   echo  "  -9.570639E-10 " >> fort.5
#             a5   
   echo  "  3.779000E-13  " >> fort.5
 endif
#---------------------------------------------------

  echo "          "
  echo "          "
  echo "  Begin to execuate vibdm.x !  for  lamta = $lamta "

   nice +4   vibdm.x < fort.5 >! fort.10 

  echo "          "
  echo "        The vibdm.x has been finished ! "
  echo "          "
  echo "    The result of energy is in  Out.vibdm . " 
  echo "          "
#


