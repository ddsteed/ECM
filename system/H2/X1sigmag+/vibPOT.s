#!/bin/csh
#                 vibPOT.s
#-----------------------------------------------------------------
#   To evaluate vibrational potentials for diatomic molecules :
#
# 1.) MORSE:      V(R) = De*{ exp(-2*alpha*x) - 2*exp(-alpha*x) }
#             where                x = (R-Re)/Re
#
# 2.) Harmonic Oscillator:      V(R) = k*(R-Re)**2/2
#
# 3.) Sun's Modified Murrell-Sorbie (x=R-Re) :
#           V_SMMS(R)=-De*beta*(1/beta + a1*x + a2*x**2 
#                                 + a3*x**3 + ...)*exp(-a1*beta*x)
#
# 4.) Huxley-Murrell-Sorbie (x=R-Re) :
#           V_MS(R)=-De*(1 + a1*x + a2*x**2 
#                                 + a3*x**3 + ...)*exp(-a1*x)
#
# 5.) SF : V_ecm(R) = V_MS + Lamta(R)*delta_V(R)
#          V_MS:  Murrell & Sorbie potential in which the
#                 (a1,a2,a3) are calculated using SF formulae.
#          Lamta(R)   = lamta*(x/R)*[1-exp(-lamta^2*x/R)]
#          delta_V(R) = V_MS - V_0
#            For  R < Re :
#      Nturn = 0, V_ecm(R) = V_MS   For ms = 3 ONLY ;
#            = 1, V_ecm(R) = V_0,   V_0 = V_Morse   ;
#            = 2, V_ecm(R) = V_0,   V_0 = V_Rydberg ;
#            = 3, V_ecm(R) = V_0,   V_0 = V_PG      .
#
#  a1, a2, a3 are functions of De, beta, and force constants f2,f3,f4.
#  f2, f3 & f4 are the functions of De, beta, mu, we, wexe, ae.
#     ( De, Re, a1, a2, a3;  mu, we, wexe, ae ) are in a.u. 
#-----------------------------------------------------------------
#  Read data (FREE format):
#      M = 1, calculate Morse vibrational potentials;
#        = 2, calculate Harmonic Oscillate potentials.
#        = 3, calculate SF (ECM) potentials.
#        = 4, calculate Huxley-Murrell-Sorbie potentials.
#
#      N -- the number of potential energies wanted;
#   beta == b1, width (adjustable) parameter;
#  lamta == La1, adjustable parameter;
#     De -- molecular dissociation energy;
#     Re -- the equilibrium internuclear distance (R in a.u.);
#     R1 -- the beginning R value;
#     R2 -- the end R value.
#----------------------------------------
#  For Morse potential :
#  alpha -- exponential parameter;
#----------------------------------------
#  For Harmonic oscillator :
#     mu -- the reduced mass (a.u.)
#     We -- vibrational constant (a.u.)
#----------------------------------------
#  For Huxley-Murrell-Sorbie potential :
#    a1,a2,a3.. -- The expansion coefficients in units of
#                          a1(ao-1), a2(a0-2), a3(ao-3).
#        iforce -- =0, do NOT calculate spectroscopic parameters.
#                       calculate f2, f3, f4 :
#                  =1, for Harmonic Oscillator.
#                  =2, for NON-Harmonic Oscillator.
#                       iforce=1, & 2 corresponds to Sun's formulae.
#                  =3, using Sun-Feng's formulae for ECM . 
#                  =4, using Huxley-Murrell-Sorbie formulae.
#                       (If a1 =/= 0.0, HMS's a2 & a3 are also obtained)
#        ishift -- =0, do NOT shift the potential V(i);
#                  >0, shift V(i) such that the V_minimum is ZERO.
#
#           amu -- the reduced mass (in ATOMIC MASS) of the molecule.
#            we -- vibrational constant (in a.u.) of the molecule.
#            ms -- The highest power in the expansion of MS potential.
#
#          mall -- = 1, calculate f2,...,f6, & potentials.
#                  > 1, calculate f2,...,f6 ONLY.
#
#   Code will calculate f's for given De, b1, we, & mu.
#   Code will calculate expansion coefficients a2,a3,... for
#     given a1, De, b1, and f's.  a1 is given using FINDroot.f .
#-----------------------------------------------------------------

  set mgo = 1       # For Morse potential
# set mgo = 2       # For Harmonic Oscillator
# set mgo = 3       # For SF (ECM) potential .
# set mgo = 4       # For Huxley-Murrell -- Sorbie potential
 
 if ($mgo == 3 || $mgo == 4 ) then
# set ifor = 0      # iforce = 0
# set ifor = 1      # iforce = 1
# set ifor = 2      # iforce = 2
# ================================
#--- for Feng Hao (ECM)  :
  set ifor = 3      # iforce = 3 ; 
#--- Please modify ms in FINDroot.s
# ================================
#--- for Huxley-Murrell-Sorbie :
# set ifor = 4      # iforce = 4

  set  d1 = $argv[1]      
  set  b1 = $argv[2]      
  set mal = $argv[3]   
  set  ab = $argv[4]   # ab is the input value of the root a1.
  set La1 = $argv[5]
  set  ms = $argv[6]
  set ntn = $argv[7]
  set  bd = $argv[8]      
 endif

#=======================================================================
# nv, rmx, nry are only available for Sun's purpose to cal. vib. ener. 
# level, (NOW, it seems that its functions is substituded by another 
# scripts EvjRun.s) 
# and they are NOT used to cal. potential curve. 
#
# set  nv =  9    #  nv = Number of vibrational states wanted.
  set  nv =  4

  set rmx = 12.0  #  Checking for  V(R->rmx=inf) = De .
  set nry = 70   
  
#=======================================================================
 if ($mgo == 1) then

#        M
  echo " 1 "  >! fort.5       #  Morse potential
#         N      De      Re    alpha   R1  R2    Add
# echo " 1000 0.4480    2.02   2.5885  0.4 10.0  0.4480 " >> fort.5
  echo " 1000 0.1819    1.4011 1.4110  0.4 10.0  0.1819 " >> fort.5 # H2;
#
#==========================
 else if ($mgo == 2) then

#        M
  echo " 2 "  >! fort.5       # Harmonic Oscillator potential
#         N      mu       Re       We      R1  R2
  echo " 300  12763.01  1.9515 0.0107464  0.0 5.0 " >> fort.5 # Herzberg(1979)
#
#==========================
 else if ($mgo == 3 || $mgo == 4 ) then
#          M
  echo " $mgo "  >! fort.5 
#--------------------------
#   For HMS formulae, set ms = 3 ONLY !
#   For SF  formulae, set ms = 3 ONLY NOW !

    set  iu = 0  # Input spectroscopy constants in H.a.u.
                 #    and Re in a_0 (Bohr)
#   set  iu = 1  # Input spectroscopy constants in cm_1
                 #    and Re in A~0 (anstrom)

#         N   iforce  ishift  ms  mall   iu
  echo " 1000  $ifor     1   $ms  $mal  $iu " >> fort.5     
#
#--------------------------
#    a1 is the ROOT of f(a1) (Sun's formulae) :
#        de*b1*a1**4 - 6*f2*a1**2 - 12*f3*a1 -12*f4 = 0.0   for ms=3
#  and is obtained using the code FINDroot.f
#
#         De beta lamta    Re     R1   R2     a1      
# echo " $d1  $b1  $La1  2.0743   1.2  6.50   $ab     " >> fort.5
  echo " $d1  $b1  $La1  2.020    1.2  6.50   $ab     " >> fort.5  
#
#--------------------------
#            For R < Re :
#  Nturn = 0, Set V_ecm(R) = V_MS(R)   for ms = 3 ONLY !
#            For ms = 4, 5 :
#        = 1, Set V_ecm(R) = V_Morse(R)   ;
#        = 2, Set V_ecm(R) = V_Rydberg(R) ;
#        = 3, Set V_ecm(R) = V_PG(R)      .
#   Nad  > 0, To shift V(R) by Add ;
#   Nad  = 0, To shift V(R) by b (= -De) which is found by code.
#
#           Add      Nad  Nturn
  echo "  0.1819     0    $ntn " >> fort.5
#--------------------------
#
 if ($ifor > 0) then  
#        Imu = 1 ,to use the atomic mass respectively .
#            = 2 ,to use the molecular mass .
#  echo " 1 "   >> fort.5
   echo " 2 "   >> fort.5
#--------------------------
#    amA -- The mass of the atomic A;
#    amB -- The mass of the atomic B;
#                 amA & amB use non-atomic unit  !!!
#    amu -- The reduced mass of molecule AB
#          amA        amB        amu
  echo "   1.0        1.0        0.50391261 " >> fort.5
#--------------------------
#       Vibrational & rotational constants : 
#            Her(Herzberg; 1979); LK(Lofthus & Krupenie; 1977)
#       We             WeXe         WeYe         WeZe   
 echo " 2.0063D-02  5.5282D-04    3.7038D-06     0.0  " >> fort.5 #(Herzberg)
#-
#          WeTe          WeSe          WeRe   
 echo "    0.0           0.0           0.0   " >> fort.5
#---
#           be           alphae        gamae
  echo "   0.0           0.0           0.0  " >> fort.5
#-
#          Der           betae
  echo "   0.0           0.0   " >> fort.5  # 
 endif 
#--------------------------
#
 if ($ifor == 4) then       # For Huxley-Murrell formulae
#            wexe           be              ae    
  echo "  6.52668D-05     9.104455D-06     7.872892D-08  " >> fort.5 
 endif
#--------------------------
#
   if ( $ifor == 3) then    # For SF (ECM) potential

# Compute the force constants for ms = 3, 4 or 5 :
     set ini = 1   # When ms = 4 and 5, code calculates f2,... 
#    set ini = 2   # When ms = 4 and 5, use following f2,...

#  When ms = 4, 5  and  ini = 1 :
#    set met = 1  # Use Broyden method to compute f3,...
     set met = 2  # Use  Newton method to compute f3,...
#
#=======================================================================
# iab, kab  are only available for Sun's purpose to cal. vib. ener.    |
# level, (NOW, it seems that its functions is substituded by another   |
# scripts EvjRun.s)                                                    |
# and they are NOT used to cal. potential curve.                       |
#=======================================================================

#  The roots (x1, x2) of the quadratic equation :
#                  a*x*x + b*x + c = 0
#    q = -0.5d0*(b + sign(1.0,b)*dsqrt( b*b - 4.0*a*c) )
#                      x1 = q/a ;      x2 = c/q
#  When ms = 3, 5  :
#    set iab = 1   #  Use x1 as f(3)
     set iab = 2   #  Use x2 as f(3)
#
#  When ms = 4  and  ini = 2 :
     set kab = 1   # Use Eq.1 to get F5;
#    set kab = 2   # Use Eq.2 to get F5.
#
#             ini  method   iab    kab
     echo  " $ini   $met   $iab   $kab "  >! fort.12
#--------------------------
#=======================================================================
# Np,Ny,bryd,hs1,hs2, Ner, nv, nj, aye, ...,  are, ae3--ae7, sx3 --ax7 |
# are only available for Sun's purpose to cal.                         |
# vib. ener. level                                                     |
# (NOW, it seems that its functions is substituded by another          |
# scripts EvjRun.s)                                                    |
# and they are NOT used to cal. potential curve.                       |
#=======================================================================
#  Np  = 1, Use f's from V_ECM & perturbation theory;
#      = 0, Use f's from numerical derivatives.
#  Ny  = 1, Use Vmorse for numerical derivatives;
#      = 2, Use Vrydberg for numerical derivatives;
#            Exponential of Vryd. : a = b * dsqrt( amu/De )
#              b=We + bryd;   originally, b=We.
#      = 3, Use Vpseudo_gaussian for numerical derivatives.
# bryd - The variational constant used to adjust Vryd(R).
#            bryd is meaningless for Vmorse & Vp_g.
#  hs1, hs2 -- Estimated initial stepsize used by code "dfridr".
#                hs1 for Vrydberg(R);   hs2 for Vp_g(R).
#            Good range for  hs1 :  0.001 --> 4.0  for H_2.
#             Np Ny bryd  hs1    hs2
#    echo  "  1  2  $bd  0.001  0.01  "  >> fort.12
#    echo  "  1  2  $bd  0.001  0.01  "  >> fort.12
     echo  "  1  2  $bd  2.000  2.50  "  >> fort.12
#    echo  "  0  2  $bd  2.000  1.50  "  >> fort.12
#--------------------------
#    V(R->inf=Rmax) = De = Sum_n  f_n*(Rbig - Re)**n/n!
#  Nryd = The maximum R value for  V(R->Rmax=inf).
#  Rmax = The maximum R value for  V(R->Rmax=inf).
# Rless = The R value in    Rbig = Rmax +/- Rless.
# Dconv = The tolerance used to check if V(Rmax) = De.
#             Nryd  Rmax  Rless  Dconv 
     echo  "  $nry  $rmx  0.01   5E-07  "  >> fort.12
#    echo  "  $nry  $rmx  0.05   1E-07  "  >> fort.12
#--------------------------
#  Ner = 0, Do NOT calculate ro-vibrational energies E_vj ;
#      > 0, Calculate E_vj using perturbation formulae and
#             force constants (f2,f3,...,f8) from numerical
#             derivatives (usually set Ner=7 for seven f's).
#               Ner = number of force constants if Ner > 0 .
#   nv = the number of vibrational states wanted.
#          (nv-1) - the HIGHEST vibrational state.
#            Ner  nv 
#    echo  "  0  $nv "  >> fort.12
     echo  "  7  $nv "  >> fort.12
#--------------------------
#   nj - number of rotational states in each v state.
#     (nj-1) - the HIGHEST rotational state in v state.
#       To have  [ SUM of Evj(i,j) ] = E(v_max) = De :
#     Set nj >= 1 + j_max (of vib. state) = 1 + (nj-1)
#               nj(i) = 1, nv
#    echo  " 10  9  9  8  8  7  6  5  4  4 "  >> fort.12
     echo  " 10 10 10  9  9  8  7  6  5  4 "  >> fort.12
   if ($nv > 10) then
     echo  "  4  3  2  2  2  1  1  1  1  1 "  >> fort.12
   endif
   if ($nv > 20) then
     echo  "  1  1  1  1  1  1  1  1  1  1 "  >> fort.12
   endif
#--------------------------
#    When calculate VIB-ROTational energies :
#  aye, ..., are = 0, Zero calc. constants 
#                    WeY, WeZ, WeT, WeS, WeR;
#                = +1, Do NOT change sign of these constants;
#                = -1, Change the sign of these constants.
#            aye  aze  ate  ase  are
     echo "  1.0  1.0  1.0  1.0  1.0  "  >> fort.12
#    echo "  1.0  1.0  1.0  1.0  0.0  "  >> fort.12
#---
#  abe,aae,age,
#  ae3 --- ae7 = 0, Zero calc. ROT. constants Eta_3 -- Eta_7;
#              = +1, Do NOT change sign of these constants;
#              = -1, Change the sign of the cal. ROT. constants.
#            abe  aae  age  ae3  ae4  ae5  ae6  ae7 
     echo "  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  "  >> fort.12
#    echo "  1.0  1.0  1.0 -1.0 -1.0 -1.0 -1.0  1.0  "  >> fort.12
#      abe -> Be;  aae -> Alpha_e;  age -> Gamma_e.
#---
#  ade,abt,ax2,
#  ax3 --- ax7 = 0, Zero calc. ROT. constants Xsi_2 -- Xsi_7;
#              = +1, Do NOT change sign of these constants;
#              = -1, Change the sign of the cal. ROT. constants.
#            ade  abt  ax2  ax3  ax4  ax5  ax6  ax7 
     echo "  1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  "  >> fort.12
#    echo " -1.0  1.0  1.0  1.0  1.0  1.0  1.0  1.0  "  >> fort.12
#    echo " -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0  0.0  "  >> fort.12
#
#      ade -> D_e;  abt -> Beta_e.
#      ade =  1.0, -> + D_e;  ade = -1.0, -> - D_e .
#    A. L. G. Rees, Proc. Phys. Soc.(London)59, 998(1947) :
#              E(v,j) = ..., + D_e*[J*(J+1)]**2 + ...
#    G. Herzberg (1953) :
#              E(v,j) = ..., - D_e*[J*(J+1)]**2 + ...
#--------------------------
#
#=======================================================================
#  The following F's may be used ONLY for ms=3, ini=2 & Np=1 :
#
     echo  "  3.69734795D-01   "  >> fort.12   #   ----  F2  
     echo  " -1.3109242269E+00 "  >> fort.12   #   ----  F3  
     echo  "  4.1934919603E+00 "  >> fort.12   #   ----  F4 
     echo  "  2.7192325459E+01 "  >> fort.12   #   ----  F5 
     echo  " -4.5072365834E+02 "  >> fort.12   #   ----  F6 
   endif
 endif
#=======================================================================

 echo "                                                    "
 echo "                                                    "
 echo "      Calculate f's & potentials of a central potential "
 echo "                                                        "

  nice  +4  vibPOT.x  < fort.5 >! fort.6


 echo "                      Finish vibPOT !   "

# rm *









