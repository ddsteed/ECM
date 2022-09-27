#!/bin/csh
#        secentv.s     (NOT for scattering calculations !)
#---------------------------------------------------------------------
#   This script sets up the input information for code secentv.f which
# solves a 2nd order differential equation through Numerov method
# for central SPHERICAL potentials & for given BOUND-STATE energies.
#
#---------------------------------------------------------------------
# Now, it is available for the following potentials potf(R):
#
#   1.) Gaussion type or of Slater type ;
#
#   2.) Spherical central potential V0*exp(-alpha*R) ;
#
#   3.) Simple harmonic potential   V(x)=0.5d0*ak*x**2 ;
#              where  x=R-Re
#
#   4.) Morse potential  V(x)=de[exp(-2*a*x) - 2*exp(-a*x)] + de ;
#              where  x=(R-Re)/Re 
#
#   5.) Screened central potential  V(x)=-Z*dexp(-x/D)/x
#
#   6.) Sun-Murrell--Sorbie potential  :
#              temp = 1/beta + a1*x + a2*x**2 + a3*x**3 +
#                     a4*x**4 + a5*x**5
#              V_MS(x) = - De*beta*temp*exp(-a1*beta*x)
#              where x = R - Re
#
#   7.) Huxley-Murrell-Sorbie :
#              V_MS(x)=-De*(1 + a1*x + a2*x**2 + a3*x**3 + ...)*exp(-a1*x)
#              where x = R - Re
#
#   8.) SF (ECM) : 
#              V_ecm(R) = V_MS(R) + Lamta(R)*delta_V(R)
#
#              V_MS:  Murrell & Sorbie potential in which the
#                 (a1,a2,a3, ...) are calculated using SF formulae.
#              Lamta(R)   = lamta*(x/R)*[1-exp(-lamta^2*x/R)]
#              delta_V(R) = V_MS - V_0
#                For  R < Re :
#              Nturn = 0, V_ecm(R) = V_MS   for ms = 3 ONLY ;
#                For  ms = 4, 5 :
#                = 1, V_ecm(R) = V_0,   V_0 = V_Morse   ;
#                = 2, V_ecm(R) = V_0,   V_0 = V_Rydberg ;
#                = 3, V_ecm(R) = V_0,   V_0 = V_PG      .
#
#   9.) Numerical potential:	read it from fort.20
#---------------------------------------------------------------------

# set kf  = 0           # kf_cof = 0 ---Sun's formulae to cal. f2,f3,f4,...
  set kf  = 1           # kf_cof = 1 ---Feng's formulae . 
#-- ms : the number of terms in V(R) expansion; ms = 3 or 5 when kf=1.

# set pot = Gaussion
# set pot = Slater
# set pot = Central
# set pot = SHO         #  Simple Harmonic Ocillator
  set pot = Morse
# set pot = SMS         #  MS --> Sun-Murrell-Sorbie (SMS) potential
# set pot = HMS         #  MS --> Huxley--Murrell-Sorbie (SMS) potential
# set pot = SF          #  ECM potential
# set pot = NP          #  Numerical potential

# set mtyp = 3          #  SHO;        ktyp = 3
  set mtyp = 4          #  Morse;      ktyp = 4
# set mtyp = 5          #  MS or ECM;  ktyp = 6, 7, 8
# set mtyp = 9          #  Numerical potential
  
# set isf = 0           #  SMS's an
  set isf = 1           #  SF's ECM potential  V_ecm(R)
                        #  isf is only available when mtyp = 5

 if ($mtyp == 4) then   # Morse
   set nv1 = 0          #  Code calculates we,  wexe for given De, alpha;
#  set nv1 = 1          #  Code calculates De, alpha for given we,  wexe;
#  set nv1 = 2          #  Code calculates wexe, alpha for given De,  we;
#  set nv1 = 3          #  Code calculates alpha (ONLY) for given Re,wexe.
   set  b1 = $argv[2]   
 endif

#  for pot = Murrell-Sorbie and ECM ONLY :
 if ($mtyp == 5) then
   set  d1 = $argv[1]  
   set  b1 = $argv[2]   
   set mal = $argv[3]   
   set La1 = $argv[4]   
   set  ms = $argv[5]   
   set ntn = $argv[6]   
  
#    For an GIVEN De = d1, use ntyp = 3 to generate we, wexe, ...
  
#  set ntyp = 1         #  Use Morse eigenvalues as first guess
#  set ntyp = 2         #  Use exp't vibrational energies as 1st guess
   set ntyp = 3         #  Input SMS energies OR the modified
                        #  exp't energies as 1st guess
#  set ntyp = 4         #  Use we,... GIVEN by code as 1st guess.
 endif

# For all values of mtyp :
#  set kfre = 0    # do NOT calculate free wave
#  set kfre = 1    # calculate free wave
   set kfre = 2    # cal. we, ..., weze from evaluated E_v's.

   set vsi = 1     # = v + 1, v is the quantum # of INITIAL state
 
   set vsf = 25    # = v'+ 1, v'is the quantum # of  FINAL  state

   set  vn = 25    #  vn = $vsf - $vsi + 1

#    The initial state to AVERAGE we, ...;
#      nci >= vsi + 2 ;   ncf <= vsf
   set nci = 1     
#  set nci = 7    

#     The  final  state to AVERAGE we, ....
#  set ncf = 1     
   set ncf = 4
#--------------------------------------------------------------------------
#    ALWAYS CHECK the wavefunction of the HIGHEST v state.
#
#      When get 'Floating exception', try to INCREASE rmin;
#    When the HIGHLY excited states are NOT converged, try
#    to INCREASE rmax, OR change the values of parameters :
#                     kstd,  kcon,  vtol, & dev
#
#      Since Linux system admits 99 units for output ONLY
#    and we keep units 1--29 for other purpose,       all
#    states with quantum number v >= 60  (vsi >= 61) will
#    be written (starting) from unit 30 & up.      So you 
#    might perform your calculations in TWO stages :      
#           First,   run from v =  0 --> v = 59 ;
#           Second,  run from v = 60 --> v =120
#
#========================================================================== 
#       (Always CHECK wavefunctions for EVERY E_v !)
#
#--- All reading are in FREE formats ---
#
#  rmin -- the minimun radius for the r mesh.  DO NOT set rmin=0.0, 
#          otherwise the centrifugal term WILL blow up !   do NOT set rmin
#          to a TOO SMALL number, or you may get Floating exception !
#  rmax -- the maximum radius for the r mesh.
#            CHOOSE rmin & rmax such that the eigenfunctions of the highest
#          eigenvalues at these TWO radial bounds have very small values.
# rstep -- the step lenth of radius r.
# rmatch - the matching boundary radius (e.g. rmatch~Re). This value 
#          should be in the classically ALLOWED region. For wavefunctions
#          which are ZERO at re, rmatch must be slightly off Re to get 
#          the converged wavefunctions !
#      rmax = 5.00  is GOOD  for v =< 30
#      rmax = 8.00  is GOOD  for v =< 50
#      rmax =10.00  is GOOD  for v =< 56
#      rmax =14.00  is GOOD  for v =< 60
#       These data are DIFFERENT from molecule to molecule
#--- 
#        kf_cof
 echo "  $kf " >! fort.5
#         rmin    rmax    rstep    rmatch  
#echo "   0.20    5.00    0.002    1.40   " >> fort.5
#echo "   0.20    8.00    0.002    1.40   " >> fort.5
 echo "   0.20   20.00    0.002    1.40   " >> fort.5
#echo "   0.80    8.00    0.003    2.0743 " >> fort.5
#echo "   1.00    4.00    0.002    2.0743 " >> fort.5  
#
#    re -- equilibrium internuclear distance in A2 molecule.
#            R_e = 2.10973 a0 = 1.11642 A .
#   rmu -- the reduced mass of A2 molecule. = amu
#   iel -- the angular momentum quantum number.
#
#  ktyp  = 1, Gaussion or Slater; 
#        = 2, Spherical central potential; vc*exp(-xl*r) ;
#        = 3, Simple Harmonic;    
#        = 4, MORSE potential; 
#        = 5, Screened central potential
#        = 6, SMS --> Sun-Murrell-Sorbie potential;
#        = 7,  MS --> Huxley-Murrell-Sorbie potential;
#        = 8,  SF --> ECM potential.
#        = 9,  Numerical potential;  Remmber to set mtyp = 9
#                                                   ********
#  kfre  = 0, do NOT calculate free wave;
#        = 1, calculate free wave;
#        = 2, calculate we, wexe, weye from evaluated E_v's.
#--- 
#         re        rmu    iel ktyp   Isf   kfre 
 echo " 1.4011     918.0752 0   4    $isf  $kfre " >> fort.5
#echo " 2.020    12763.01   0   7    $isf  $kfre " >> fort.5
#echo " 2.020    12763.01   0   8    $isf  $kfre " >> fort.5   
#echo " 2.0743   12763.01   0   8    $isf  $kfre " >> fort.5
#echo " 2.020    12763.01   0   9    $isf  $kfre " >> fort.5
# 
#  nvp -- the # of terms in the potential expansion. 
#           nvp is NOT used for ktyp = 6, 7, 8
#           nvp = 2 for Morse potential.
#   ng -- the number of r points on which the wavefunction is calculated.
# nbmax - the (read-in) maximum # of bound states.   nbmax = EWpara
#   nbi - the # of INITIAL bound state used;  nbi = v_ini + 1 .
#   nwv - the # of  FINAL  bound state used;  nwv = v_fin + 1 .
#         write wavefunctions to fort.9  for v =< nwv.
#           if nwv < nbmax,   code sets  nbmax = nwv.
#      For AVERAGING spectroscopic constants :  
#   nci - the INITIAL state to AVERAGE we, ...;
#   ncf - the  FINAL  state to AVERAGE we, ....
#---
	
    set nvp = 2
  
    set  ng = 30
	
#       nvp  ng nbmax  nbi  nwv  nci  ncf    
 echo " $nvp $ng 90   $vsi $vsf $nci $ncf " >> fort.5   # for Morse
#echo " $nvp 50  90   $vsi $vsf $nci $ncf " >> fort.5   # for Murrell-Sorbie
#echo " $nvp 50  61   $vsi $vsf $nci $ncf " >> fort.5   
#echo " $nvp 50  30   $vsi $vsf $nci $ncf " >> fort.5   
# 
#     nv(i), vc(i), xl(i) are correspond to :
# ktyp=1; V(r) => vc(i)*r**nv(i)*exp(-xl(i)*r**2) ;
#           or => vc(i)*r**nv(i)*exp(-xl(i)*r) .
#
#     =2; V(r) => vc(i)*exp(-xl(i)*r) .
#
#     =3; V(r) => 0.5*[rmu*vc(i)**2]*r**2 ; vc=we==omega.
#
#     =4; V(r) => vc(i)*[exp(-2*xl(i)*r) - 2*exp(-xl(i)*r)]
#
#----------------------------------------------------------------- 
#    Parameters used in ENERGY convergence studies :
#      ( Convergence parameters may produce UNphysical wavefunctions ! )
#
#      [ Since the TRUE physical convergence criterion
#        is the PROPERTY of the vibrational wavefunction :
#          The number of NODES == the QUANTUM numvber v
#        You may have to use different vtol(v) 
#        for different state E_v !  CHECK WAVESfunctions !  ]
#
#      Parameters for convergence in subroutine boundv:
#  kstd -- = 0, dabs(Ecorr/E)      .gt. tol (GOOD) ;
#          = 1, dabs(qeng*Ecorr/E) .gt. tol        ;
#          > 1, dabs( E - E0)      .gt. tol ( OK ) .   # CHECK waves
#  kcon -- the input iteration numbers for eigenvalues.
#  qeng -- the input correction factor for energy convergence;
#  vto1 -- The tolerance for the NUMERICAL bound-state eigenvalues.
#   dev -- Energy correction devider in  E_v = E_v + Ecor/dev(v).
#   ul0 -- The values of the "left" wavefunction (to be propagated
#          outward) at the FIRST r-mesh point.
#   ur0 -- The values of the "right" wavefunction (to be propagated
#          inward) at the LAST r-mesh point.
#      kcon & dev are KEY parameters.  
#--- 
#      kstd(i) kcon(i) qeng(i) vtol(i) dev(i) ul0(i) ur0(i)     i=1,nbmax
     cat    EWpara     >>  fort.5
#--------------------------
 if ($mtyp < 5) then
#   SHO/Morse eigenvalues will be evaluated from vc & xl.
#   For Morse : vc(1) == De;  xl(1) == alpha
#               vc(2) == we;  xl(2) == wexe
#   When nv(1)=0, code calculates we, wexe for given De, alpha, rmu, & Re;
#   When nv(1)=1, code calculates De, alpha for given we, wexe, rmu, & Re;
#   When nv(1)=2, code calculates wexe, alpha for given De, we, rmu, & Re;
#   When nv(1)=3, code calculates alpha (ONLY) for given  wexe, rmu, & Re.
#         nv(i)    vc(i)          xl(i)  (i=1,nvp)    # for N2
   if ($nvp == 1) then
     echo " $nv1  0.1744962        1.41101       " >> fort.5 
   else if ($nvp == 2) then
     echo " $nv1  0.18186D+00     1.4110D+00      " >> fort.5 # For ktyp=4 
     echo "  0   0.10746436D-01  0.65264950D-04  " >> fort.5 # Exp't data
#    echo "  0   0.20062D-01     0.55282D-03     " >> fort.5 # Morse data
   endif

#---
  if ($mtyp == 4) then
#     Use the previously calculated MS eigenvalues as the first GUESS for E(i)
#     Or use the modified exp't energies as the first GUESS for E(i)
#   M-S  :     E(i)     i=1,nbmax   ( copied from fort.19 )
#    cat  Evib    >>  fort.5           # Evib   contains E(i)'s (unit in H.a.u.)
  endif
#-------------------------------------------------------------------------------
#
#   For Sun-Feng's (ECM) or Sun-Murrell-Sorbie potential ONLY :
#       **********  ***  ** *** ******* ******
 else if ($mtyp == 5) then     
#
# naucm= 0, read we, wexe,... Add in Hartree; = 1, in cm-1.
#           De is always in H.a.u.
# beta = an adjustable width parameter.
#   ms = 3, 4, 5 : The highest power in the expansion of MS potential.
# mall = 1, use the a1 from fort.5 (intermediate case);
#      > 1, use the a1 from fort.4 (use the a1 from previous code).
#  msv = v, the code'll generate approximate energies starting from v.
#               20 =< msv =< nwv .
#
#        naucm  De beta lamta  ms  ntyp  mall  msv  a1    
#  echo "  0   $d1  $b1 $La1  $ms $ntyp  $mal  41  0.0  " >> fort.5 # 
#  echo "  0   $d1  $b1 $La1  $ms $ntyp  $mal   0  0.0  " >> fort.5 # 
   echo "  1   $d1  $b1 $La1  $ms $ntyp  $mal   0  0.0  " >> fort.5 # 
#--------------------------
#   amu = reduce mass .( Atomic unit)  = rmu
#           For  R < Re :
# Nturn = 0, V_ecm(R) = V_MS   for ms = 3 ONLY ;
#           For  ms = 4, 5 :
#       = 1, V_ecm(R) = V_0,   V_0 = V_Morse   ;
#       = 2, V_ecm(R) = V_0,   V_0 = V_Rydberg ;
#       = 3, V_ecm(R) = V_0,   V_0 = V_PG      .
#
#           amu       Nturn
   echo "  918.0752   $ntn    " >> fort.5
#--------------------------
#--------------------------
  if ($ntyp == 1) then
#     Use the eigenvalues of Morse potential as the first GUESS for E(i)
#   The we, wexe from Morse may differ from those of exp't.
#   Since they are from Morse potential, so WEYE,WEZE,WETE,WESE are 
#                                           all equal to zero
#              WE             WEXE        WEYE    WEZE   # For N2
   echo " 0.10736785D-01 0.64329547D-04   0.0     0.0   " >> fort.5  
#             WETE     WESE 
   echo "     0.0      0.0    " >> fort.5  
#----
#   Add = the energy shifting constant for electronic EXCITED states.
#   Add = 0.0 for electronic GROUND state;  > 0.0 for excited state.
#             Add
   echo "     0.0     " >> fort.5  
#  echo "   30000.0   " >> fort.5  
  endif
#--------------------------
  if ($ntyp >= 2) then
#     Use exp't vibrational energies as the first GUESS for E(i)
#            Her(Herzberg	;  1979); LK(Lofthus & Krupenie; 1977)
#         WE          WEXE        WEYE      WEZE
   echo " 2358.57     14.324     -0.00226  -0.00024 " >> fort.5 # 
#             WETE      WESE 
   echo "     0.0       0.0  " >> fort.5 
#----
#   Add = the energy shifting constant for electronic EXCITED states.
#   Add = 0.0 for electronic GROUND state;  > 0.0 for excited state.
#             Add
   echo "     0.0     " >> fort.5  
#  echo "   30000.0   " >> fort.5  
  endif
#--------------------------
  if ($ntyp == 3) then
#     Use the previously calculated MS eigenvalues as the first GUESS for E(i)
#     Or use the modified exp't energies as the first GUESS for E(i)
#   M-S  :     E(i)     i=1,nbmax   ( copied from fort.19 )
    cat  Evib    >>  fort.5           # Evib   contains E(i)'s (unit in H.a.u.)
  endif
#--------------------------
  if ($ntyp == 4) then
#     Use we, wexe, weye given by code as the first GUESS for E(i)
#              WE        WEXE        WEYE      WEZE
   echo " 0.10056E-01  7.3357E-05  -1.8225E-07 0.0 " >> fort.5 
#              WETE      WESE 
   echo " 1.045356E-04  0.0  " >> fort.5  # (Morioka)
  endif
 endif
#--------------------------
# rmesh -- the r (vector) points on which the wavefunction is interpolated.
#        rmesh(i)   i=1,ng      (f15.9)
  if ($mtyp < 4) then
    echo " 1.928572d0  " >! fort.22
    echo " 2.156645d0  " >> fort.22
    echo " 2.661743d0  " >> fort.22
  else if ($mtyp >= 4 ) then 
#   cp /home1/sun/scatt/ou/scripts/SchrodNUM/MORSE.pts30   fort.22
#   cp /home1/sun/scatt/ou/scripts/SchrodNUM/MORSE.pts50   fort.22
#   cp /home1/sun/bound/ecmpes/system/MORSE.pts50*         temp.gz
#   cp /home1/feng/temp/job1/ECM/system/MORSE.pts50*       temp.gz

# The following file is used to set radius on which the wavefunctions are
# calculated. It should be consistent with $ng.
    cp ~/home1/feng/bound/ecmpes/system/H2/X1sigmag+/MORSE.pts$ng.gz  temp.gz
      gunzip temp.gz
      mv  temp  fort.22
  endif
#  
#--------------------------
#   Read numerical potential vnum(r) if neccessary :
#
#    mpu =  0, read POT in Hartree; = 1, in eV; = 2, in cm-1.
#     nr -- The # of potential points to be read;  nr < 10,000.
#    ri0 -- The first radial value of the numerical potential
#  rste1 -- The step length of the numerical potential
#   Evib -- Modified E(i)  (Previously copied from fort.19)
#
 if ($mtyp == 9) then
#          mpu   nr     ri0     rste1
  echo "   1     1000   0.400   0.096  " >> fort.5
  cat  Evib    >>  fort.5     # Evib => E(i)  i=1,nbmax
  cp   Vnum  fort.20          # Numerical potential
 endif
#
#========================================================================== 

 echo "  Running secentv.x for bounded ($pot) SPHERICAL potential "
 
   nice  +4  secentv.x < fort.5 >! fort.6

    cp  fort.3  out.wavpts    # This file should be saved for BFVCC
    cp  fort.6  out.cal
    cp  fort.7  out.wavE
 
    cp  fort.9  out.wave0
      
      set vj = 0
  foreach vm (1 2 3 4 5 6)
      set vk = 9
      set vr = $vm$vj
       @ vk += $vm            # assign the result of $vk + $vm to $vk
    if ($vn  >= $vr) then
      cp  fort.$vk   out.wave$vm
    endif
  end

  
  if ($mtyp == 5) then
     cp  fort.8  out.potORI
     cp  fort.18 out.pot$b1.g$La1
  endif

 echo "                                  "
 echo "               Finish secentv !   "







