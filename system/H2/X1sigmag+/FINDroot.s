#!/bin/csh
#               FINDroot.s
#-----------------------------------------------------------------
#   This script uses the Brent's method (through FUNCTION zbrent) to 
# find the ROOT of an user supplied ONE-dimensional function FUNC 
# known to lie between x1 & x2.  The root, returned as zbrent, will
# be refined until its accuracy is tol.
#
#----------------------------------------------------------------
#   Read data :
# ktyp = 1, -->   General Modified Murrell-Sorbie potential, for 
#               example,  for n=4 :
#               f(a1)=D*a1**4 - 6*f2*a1**2 - 12*f3*a1 -12*f4=0.0      (1.)
#               where D=De*Be with Be an adjustable parameter, a1, 
#               the ROOT,  is the first expansion coefficient
#               of the Murrell-Sorbie (modified Rydberg) potential.
#                      ******* ******
#               This is the Sun's formulae.
#
# ktyp = 2, --> f(x)=de*a1**4 - 6*f2*a1**2 - 4*f3*a1 - f4=0.0          (2.)
#               for ms=3, Huxley-Murrell formulae for MS potential :
#                   J. Chem. Soc. Faraday Trans 2, 79, 323(1983)
#                 For MS potential [ ms=4; SF's (ECM) formulae ] :
#               f(x)=de*a1**5 - 10.0d0*f2*a1**3 - 10.0d0*f3*a1**2
#                             - 5.0d0*f4*a1 - f5 = 0.0
#                 For MS potential [ ms=5; SF's (ECM) formulae ] :
#               f(x)=de*a1**6 - 15.0d0*f2*a1**4 - 20.0d0*f3*a1**3
#                      - 15.0d0*f4*a1**2 - 6.0d0*f5*a1 - f6 = 0.0
#
#
# ktyp = 3, --> f(x)=Ev/De + exp(-a1*x)*f(a1,a2,a3;x)=0.0              (3.)
#                 where   f(a1,a2,a3;x)=1 + a1*x + a2*x**2 + a3*x**3
#               and x = R - Re.
#               Eq.(2) is the equation of calculating the classical
#               turning point (CTP), x=x_ctp, for Murrel-Sorbie potential.
#                              ***
#                 When the MS eigenvalues match the experimental
#               vibrational energies, the CTPs are the ones of the
#               experimental diatomic potential V(R).
#
# ktyp = 4, --> f(x)= -Ev/De + [1-exp(-a*x)]**2 + g(x) =0.0            (4.)
#               where   g(x)=d*a**3*x**3*exp(-2*a*x)*(1+b*a*x)
#               and x = R - Re.      Eq.(3) is the equation of
#               calculating the classical turning point (CTP), x=x_ctp,
#                                                        ***
#               for Hulburt & Hirschfelder (HH) diatomic potential
#               which is the modified Morse potential.
#
#----------------------------------------------------------------
#   x1, x2 -- The left and right bounds of the ROOT of FUNC.
#               Usually, x1 < 0.0,  x2 > 0.0 .
#      tol -- The convergence criterion.
#      Ev  -- The vibrational eigenvalues.
#-----------------------------------------------------------------
#  For a1 used in Murrell-Sorbie potential (ktyp=1, 2) :
#                                           ******  *
#              ms =  n-1, is the order of MS potential.
#     b1,f2,f3,f4 -- parameters used in Eq.(1.); b1 = Beta.
#            mall = 1, use f2,...,f6 from fort.5 (intermediate case);
#                 > 1, use f2,...,f6 from fort.2 (use the f's from previous code).
#
#  For ktyp=4 :
#     we, wexe, be, & ae  are the experimental spectropic constants.
#
#===========================================================================
#set mgo = 1  # Function in Sun's Modified Murrell - Sorbie potential
 
#set mgo = 2  # For the MS function used in Huxley-Murrell formulae

 set mgo = 3  # For the SF (ECM) potential formulae
 
 set  d1 = $argv[1]
 set  b1 = $argv[2]
 set mal = $argv[3]
 set La1 = $argv[4]
 set  ms = $argv[5]

#set mgo = 4  # To calculate x_ctp of Murrell -- Sorbie potential
#set mgo = 5  # To calculate x_ctp of Hulburt & Hirschfelder potential
 
#-----------------------------------------------------------------
 if ($mgo < 5) then
#        ktyp  
# echo "  1   " >!  fort.5    # For Sun's formulae
  echo "  2   " >!  fort.5    # For Huxley-Murrell's and SF's formulae
#---
#          x1     x2      tol    mall
  echo "  2.50   3.20   1.0E-09  $mal " >> fort.5 # Good for ms=3,4,5
#---
#   f2,f3,f4 are the functions of de,b1,mu & we ONLY for Sun's formulae !
#   When mall = 2, all input f's from fort.5 are NOT used.  b1 = beta .
#         ms   de   beta   f2    f3    f4
  echo " $ms  $d1    $b1   0.0   0.0   0.0  " >> fort.5 
#---
#         f5    f6       
  echo " 0.0   0.0  " >> fort.5 
#---
 endif
#-----------------------------------------------------------------

 echo "                                                "
 echo "      Find the ROOT of a user supplied function "
 echo "                                                "

  nice  +4  FINDroot.x  < fort.5 >! fort.6


 echo "              Finish FINDroot !         "
 echo "                                                "
 echo "                                                "

# rm *



