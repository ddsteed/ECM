C     program  vibPOT
c-----------------------------------------------------------------
c          Modified by   Weiguo Sun   07/03/1999
c
c     This program is to calculate the vibrational potentials
c   for diatomic molecules :
c 1.) MORSE:      V(R) = De*{ exp(-2*alpha*x) - 2*exp(-alpha*x) }
c             where     x = (R-Re)/Re
c 2.) Harmonic Oscillator:      V(R) = k*(R-Re)**2/2
c
c 3.) Sun's Modified Murrell-Sorbie (x=R-Re) :
c           V_SMMS(R)=-De*beta*(1/beta + a1*x + a2*x**2 
c                                 + a3*x**3 + ...)*exp(-a1*beta*x)
c
c 4.) Huxley-Murrell-Sorbie (x=R-Re) :
c        V_MS(R)=-De*(1 + a1*x + a2*x**2 + a3*x**3 + ...)*exp(-a1*x)
C
C 5.) SF (ECM) : V_ecm(R) = V_MS + Lamta(R)*delta_V(R)
c          V_MS:  Murrell & Sorbie potential in which the
c                 (a1,a2,a3) are calculated using SF formulae.
c          Lamta(R)   = lamta*(x/R)*[1-exp(-lamta^2*x/R)]
c          delta_V(R) = V_MS - V_0
c            For  R < Re :
C      Nturn = 0, V_ecm(R) = V_MS  For ms = 3 ONLY ;
C            = 1, V_ecm(R) = V_0,  V_0 = V_Morse   ;
C            = 2, V_ecm(R) = V_0,  V_0 = V_Rydberg ;
C            = 3, V_ecm(R) = V_0,  V_0 = V_PG      .
C
C    The program will output the cofficients an of MS potential 
C  to fort.4  when energy = 1 .
C                  ==========
C
C  NOTES :
C
C 1.>  !!!  Search for  "NP=" & "NP1=" !!!
C
C 2.>   *   Whenever the subroutine "funcv" or the function "fmin"
C         is used,  the "fvec" array defined in common is CHENGED  *
C
c-----------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      parameter  (Na=2500, Nb=40, Nc=20, Nd=100)
      dimension  gg1(Nd),ge1(Nd),gg2(Nd),ge2(Nd),gg3(Nd),ge3(Nd)
      dimension  ggf(Nd)
      dimension  rr(Na),v(Na),u(Na),ff(Nb),an(Nc),gg(Nd),ge(Nd)
      dimension  vmos(Na),vryd(Na),vpg0(Na),ums0(Na),vms0(Na),nj(200)
      common /aafcom/ aaf(4,10)
      common /fms/ ms, Ny
      common /Eswitch/ aye,aze,ate,ase,are
      common /Eswitc1/ abe,aae,age,ae3,ae4,ae5,ae6,ae7
      common /Eswitc2/ ade,abt,ax2,ax3,ax4,ax5,ax6,ax7
      common /spectra/ amu,Re,De,We,WeXe,WeYe,WeZe,WeTe,WeSe,WeRe
      common /spectrb/ Be,alphae,gamae,Der,betae,bryd  
c-----------------------------------------------------------------
c  Read data :
c      M = 1, calculate Morse vibrational potentials;
c        = 2, calculate Harmonic Oscillate potentials.
c        = 3, calculate Sun-Feng (ECM) potentials.
c        = 4, calculate Murrell-Sorbie potentials.
C
c      N -- the number of potential energies wanted;
c     De -- the energy scaling constant ("ionization energy");
c     Re -- the equilibrium internuclear distance (R in a.u.);
c     R1 -- the beginning R value;
c     R2 -- the end R value.
c
c   beta == width (adjustable) parameter for SMMS.
c alamta == lamta, variational adjustable parameter for ECM.
c----------------------------------
c  For Morse potential :
c  alpha -- exponential parameter;
c----------------------------------
c  For Harmonic oscillator :
c    amu -- the reduced mass (a.u.)
c     We -- vibrational constant (a.u.)
c----------------------------------
c  For Murrell-Sorbie potential :
c    a1, a2, a3 -- The expansion coefficients in units of
c                  a1(ao-1), a2(a0-2), a3(ao-3).
c        iforce -- =0, do NOT calculate spectroscopic parameters;
c                  =1, calculate f2, f3, f4 for Harmonic Oscillator.
c                  =2, calculate f2, f3, f4 for NON-Harmonic Oscillator.
c                       iforce=1, & 2 corresponds to Sun's formulae.
c                  =3, calculate f2, f3, f4 Using Sun-Feng formulae.
c                  =4, calculate f2, f3, f4 Using Hurley-Murrell formulae.
c                       (If a1 =/= 0.0, HM's a2 & a3 are also obtained)
c
c        ishift -- =0, do NOT shift the potential V(i);
c                  >0, shift V(i) such that the V_minimum is ZERO.
c           amu -- the reduced mass (in a.u.) of the molecule.
c            we -- vibrational constant (in a.u.) of the molecule.
c
c            ms -- The highest power in the expansion of the MS potential.
c                    For Huxley & Murrell, set ms = 3 ONLY !
c                    For Sun's fformula,   set 10 > ms >= 3 .
c
c          mall -- = 1, calculate f2,...,f6, & potentials.
c                  > 1, calculate f2,...,f6 ONLY.
c
c   iu = 0, Input spectroscopy constants in H.a.u. & Re in a_0 (Bohr);
c      = 1, Input spectroscopy constants in cm_1 & Re in A~0 (anstrom).
c
c   For iforce > 0,  a1 =/= 0.0,  a2=0.0 (input value), the code 
c will calculate a2 & a3.
c      *****************
c
c  Nturn = 0, Set V_ecm(R) = V_MS(R)   for ms = 3 ONLY !
c               For ms > 3 :
c        = 1, Set V_ecm(R) = V_Morse(R)   ;
c        = 2, Set V_ecm(R) = V_Rydberg(R) ;
c        = 3, Set V_ecm(R) = V_PG(R)      .
c   Nad  > 0, To shift V(R) by Add ;
c   Nad  = 0, To shift V(R) by b (= -De) which is found by code.
c
c    ini = 1, Cal. f3,... using standard method.
c        = 2, Cal. f3,... using approxi. method.
c
c  When ms = 4, 5  and  ini = 1 :
c    met = 1, Use Broyden method to compute f3,...
c        = 2, Use  Newton method to compute f3,...
c
c  The roots (x1, x2) of the quadratic equation :
c                  a*x*x + b*x + c = 0
c    q = -0.5d0*(b + sign(1.0,b)*dsqrt( b*b - 4.0*a*c) )
c                      x1 = q/a ;      x2 = c/q
c  When ms = 3, 5  :
c    iab = 1, Use x1 as f(3)
c        = 2, Use x2 as f(3)
c
c  When ms = 4  and  ini = 2 :
c    kab = 1, Use Eq.1 to get F5;
c        = 2, Use Eq.2 to get F5.
c
c  Np  = 1, Use f's from V_ECM & perturbation theory;
c      = 0, Use f's from Vrydberg & n_th derivatives.
c  Ny  = 1, Use Vmorse as V(R) for n_th derivatives.
c      = 2, Use Vrydberg as V(R) for n_th derivatives.
c             Exponential of Vryd. : a = b * dsqrt( amu/De )
c              b=We + bryd;          originally, b=We.
c      = 3, Use Vpseudo-gaussian as V(R) for n_th derivatives.
c bryd - The variational constant used to adjust Vryd(R).
c            bryd is meaningless for Vmorse & Vp_g.
c  hs -- Estimated initial stepsize used by code "dfridr".
c            Good range for  hs :  0.001 --> 4.0  for H_2.
c
c=========================================================================
      read (5,*) M
	if (M .eq. 2) go to 30
	if (M .eq. 3) go to 50
	if (M .eq. 4) go to 50
      read (5,*) N,De,Re,alpha,R1,R2,Add
c-------------------------------------
        Rdel=(R2-R1)/N
        R=R1-Rdel
        write(6,510) 
      do 10 i=1,N
        R=R+Rdel
        x=(R-Re)/Re
        rr(i)=R
        v(i)=De*(dexp(-2.0d0*alpha*x) - 2.0d0*dexp(-alpha*x))
        write(6,520) rr(i),v(i)
        write(7,520) rr(i),v(i)+Add
   10 continue
	go to 900
C-------------------------------------------------------------------
   30 read (5,*) N,amu,Re,We,R1,R2
	ak=amu*We**2
        Rdel=(R2-R1)/N
        R=R1-Rdel
        write(6,515) 
      do 40 i=1,N
        R=R+Rdel
	rr(i)=R
	v(i)=0.50d0*ak*(R-Re)**2
        write(6,520) rr(i),v(i)
   40 continue
	go to 900
C-------------------------------------------------------------------
C    an(1) MUST be solved using code FINDroot.f
C       De in Hartree; Re, R1 & R2 in ao;
C       we in Hartree; amu in AMU.
C            ff(2) in Hartree*/(a_o**2)
C            ff(3) in Hartree*/(a_o**3)
C            ff(4) in Hartree*/(a_o**4)
C            ff(5) in Hartree*/(a_o**5)
C            ff(6) in Hartree*/(a_o**6);
C    an(1) in 1/a_o     ;  an(2) in 1/(a_o**2);
C    an(3) in 1/(a_o**3);  an(4) in 1/(a_o**4);
C    an(5) in 1/(a_o**5).
C-------------------------------------------------------------------
   50     read(5,*) N,iforce,ishift,ms,mall,iu
          read(5,*) De,beta,alamta,Re,R1,R2,an(1)
          read(5,*) Add, Nad, Nturn
C---
            a1 = an(1)
          if (an(1) .gt. 95.0) then
            read(4,*) a1
              an(1) = a1
          endif
C---
            D1 = De*beta
C-------------------------------------------------
C  Calculate force constants f & coefficients an.
C=================================================
      IF (iforce .eq. 0) goto 75
        read (5,*) Imu
        read (5,*) amA,amB,amu1
        read (5,*) We,WeXe,WeYe,WeZe
        read (5,*) WeTe,WeSe,WeRe
        read (5,*) Be,alphae,gamae
        read (5,*) Der,betae
C
C--- Prepare CONVERSION factors :
           utoh = 3.42317725E+07
           cmao = 5.29177249E-09
         autocm = 219474.63067
         aotoA0 = 0.5291772490
         aJtoau = 0.229371d0
           AMAE = 1.6605655D-27/9.1093897D-31
	  RBOHR = 0.529177249D0
C-------------------------------------------------
C     AMAE=mass_unit/mass_e
C     AMAE=1.6605655D-27/9.1093897D-31=1822.9163
C-------------------------------------------------
      if (iu .eq. 1) then
	     R1 = R1/RBOHR
	     R2 = R2/RBOHR
	     Re = Re/RBOHR
             We = We/autocm
	   WeXe = WeXe/autocm
	   WeYe = WeYe/autocm
	   WeZe = WeZe/autocm
	   WeTe = WeTe/autocm
	   WeSe = WeSe/autocm
	   WeRe = WeRe/autocm
             Be = Be/autocm
         alphae = alphae/autocm
          gamae = gamae/autocm
            Der = Der/autocm
          betae = betae/autocm
	endif

	write(6,501) Re*RBOHR,Re
	write(6,502)
	write(6,503)'   We = ',We*autocm,' cm_1  = ',We, ' H.a.u.'
	write(6,503)' Wexe = ',Wexe*autocm,' cm_1  = ',Wexe, ' H.a.u.'
	write(6,503)' Weye = ',Weye*autocm,' cm_1  = ',Weye, ' H.a.u.'
	write(6,503)' Weze = ',Weze*autocm,' cm_1  = ',Weze, ' H.a.u.'
	write(6,503)' Wete = ',Wete*autocm,' cm_1  = ',Wete, ' H.a.u.'
	write(6,503)' Wese = ',Wese*autocm,' cm_1  = ',Wese, ' H.a.u.'
	write(6,503)' Were = ',Were*autocm,' cm_1  = ',Were, ' H.a.u.'
	write(6,503)'   Be = ',Be*autocm,' cm_1  = ',Be, ' H.a.u.'
	write(6,503)'alphe = ',alphae*autocm,' cm_1  = ',alphae, ' H.a.u.'
	write(6,503)'gamae = ',gamae*autocm,' cm_1  = ',gamae, ' H.a.u.'
	write(6,503)'Der = ',Der*autocm,' cm_1  = ',Der, ' H.a.u.'
	write(6,503)'betae = ',betae*autocm,' cm_1  = ',betae, ' H.a.u.'

C-------------------------------------------------
C  Change mass unit into atomic unit from amu :
C-------------------------------------------------
        if ( Imu .eq. 1 ) then
          AMU=( AMB*AMA/(AMA+AMB) )*AMAE
        else if ( Imu .eq. 2 ) then
          AMU = amu1*AMAE
        endif

C--- Prefactor :
            ss0 = 0.021657164d0
C--- Calculate the nth spectroscopic parameters
C      f2 is in Hartree/(a_o**2).   f2 has CORRECT value
C    when multiplied by prefactor ss0.
C---      using Weiguo Sun's formulae
C      For NON-Harmonic Oscillator : 
C    f2 = ss0* 4*pi*pi*C*C * amu*we*we = ss0* 1.000004496 * amu*we*we
C
           f2=ss0*1.000004496*(utoh*amu)*(autocm*we*cmao)**2
           ff(2)=f2
           an(2)=0.5d0*(a1**2 - f2/D1)
C-------------------------------------------------------------
C--- Sun's formulae :
          IF ( iforce .eq. 2 ) THEN
             do 60 k=3,ms+1
                temp = (- 1.0)**k * 2.0d0*(k-1)
                temp = temp*dsqrt(f2**k)/facx(k)
               ff(k) = temp/(D1**(k/2.0d0 - 1.0) )
  60         continue

               write(6,530) alamta,De,beta,D1
             do k=2,ms+1
               write(6,550) k, ff(k)
               write(27,560)   ff(k)
               write(2,560)    ff(k)
             enddo
c              write(2,560) (ff(k), k=2,ms+1)

              IF (mall .gt. 1) GOTO  900
c--------
               do 65 k=3,ms
                 temp = - 0.5d0*ff(k)/D1 + (-a1)**k *(k-1)/facx(k)
                 do i=2,k-1
                   temp = temp - (-a1)**(k-i)*an(i)/facx(k-i)
                 enddo
                 an(k) = temp
  65           continue

               write(6,540) 
             do k=1,ms
               write(6,550) k, an(k)
               write(3,560) an(k)
             enddo
          ENDIF
c
C===  Feng's formulae (used in ECM) :
c
        IF ( iforce .eq. 3 ) THEN
	       read(12,*) ini,method,iab,kab
	       read(12,*) Np,Ny,bryd,hs1,hs2
	       read(12,*) Nryd,Rmax,Rless,Dconv
	       read(12,*) Ner,nv
	       read(12,*) ( nj(k), k=1,nv)
	       read(12,*) aye,aze,ate,ase,are
	       read(12,*) abe,aae,age,ae3,ae4,ae5,ae6,ae7
	       read(12,*) ade,abt,ax2,ax3,ax4,ax5,ax6,ax7
               ij = 6
	     do i=1,2
               write(ij,800) Np,Ny,Ner,nv,bryd,hs1,hs2
	       do k=1,nv
	         write(ij,810) k-1, nj(k)
	       enddo
             write(ij,820) aye,aze,ate,ase,are,abe,aae,age,ae3,ae4,
     # ae5,ae6,ae7,ade,abt,ax2,ax3,ax4,ax5,ax6,ax7
               if (i .eq. 1) ij = 35
	     enddo
c
  	     do 111 k = 2,ms+1
  	       read(12,*) ff(k)
111  	     continue

  	       ff(2) = amu*we*we
          IF ((ms .eq. 4) .or. (ms .eq. 5)) THEN
c
c -----   Calculate (f2,...,f5) for ms=4 or (f2,...,f6) for ms=5 :
c
             if (ini .eq. 1) then
c
c --- Sove NONlinear equations using standard method
c
  	         call calaaf
  	           Nff = ms - 1
  	         call calff(ff,Nff,Nb,method)
 	       else
c
c --- Sove NONlinear equations using approximate method
c
  	         call getff(ff,Nb,iab,kab)
 	       endif
c
c--------------------------------------------------- 
c---  Rydberg nth force constants
c           b = dsqrt(ff(2)/De)
c    	      do 112 i = 3, ms+1
c	        ff(i) = (-1)**i*(i-1)*ff(2)*b**(i-2)
c112 		continue
c---  Morse nth force constants
cc          a = dsqrt(0.5d0*ff(2)/De)
c           a = Re * dsqrt(0.5d0*ff(2)/De)
c           do 113 i = 3, ms+1
c	        ff(i) = ff(2)*(-1)**i*(2**(i-1)-1)*a**(i-2)
c113 		continue
c--------------------------------------------------- 
c
          ELSEIF (ms .eq. 3 .and. ini .eq. 1) THEN
c -----        Calculate f2, f3, and f4 ONLY
c
  	           a = amu*We
                  fa = 1675.0d0/(48.0d0*amu*We*We)
                  fb = -12.0d0*Re*(amu*We*Re)**2
                  fc = -36.0d0*amu**3*We**4*Re**2 
                  fc = fc - 24.0d0*Re*(amu*We*Re)**5*alphae
                  fc = fc - 335.0d0*amu**2*We**2*WeXe
C ---
               call  quadratic(fa,fb,fc,x1,x2)
                 write(6,532) fa, fb, fc, iab, x1, x2
             if (iab .eq. 1)then
               ff(3) = x1
             else
               ff(3) = x2
             endif
C ---
               ff(4) = 5.0d0*ff(3)*ff(3)/(3.0d0*amu*We*We) 
               ff(4) = ff(4) - 16.0d0*(amu*We)**2*WeXe
C --------
c              ff(5) = De*a1**5-10.0d0*ff(2)*a1**3-10.0d0*ff(3)*a1**2
c    #  -  5.0d0*ff(4)*a1
c              ff(6) = De*a1**6-15.0d0*ff(2)*a1**4-20.0d0*ff(3)*a1**3
c    #  - 15.0d0*ff(4)*a1**2-6.0d0*ff(5)*a1
C --------
          ENDIF
c-------------------------------------------------------- 
c   Generate the data needed by V(Pseudo-Gaussian; R)
c-------------------------------------------------------- 
             call  Vpotdata
c-------------------------------------------------------- 
c  Calculate force constants ff == gg  using
c  the definitions :   f_n = [ d~nV(R) ]/[dR~n] 
c  and  the function code "dfridr" .
c
c    hs -- an estimated initial stepsize; it needs
c not be small, but rather should be an increment
c in R' over which function fpot changes substantially.
c   err -- An estimate of the error in the derivative. 
c-----
c  Calculate numerical "force constants" for :
c    V_morse(R),  as  ks=1;     V_rydberg,  as  ks=2;
c    V_pg(R),     as  ks=3.
c-------------------------------------------------------- 
               ms0 = ms
             if (Ner .gt. 0 .and. ms .lt. Ner+1) ms0 = Ner
c
           do ks=1,3
             do k=2,Nryd+1
               if (ks .eq. 1) then
		       gg1(k) = fmorse(k)
                   ge1(k) = 0.0
                     if (ks .eq. Ny) then
                       gg(k) = gg1(k)
                       ge(k) = ge1(k)
                     endif
               elseif (ks .eq. 2) then
		     gg2(k) = dfridr(k,ks,Re,hs1,err)
                 ge2(k) = err
		     ggf(k) = fryd(k,Re)
                   if (ks .eq. Ny) then
                     gg(k) = gg2(k)
                     ge(k) = ge2(k)
                   endif
               elseif (ks .eq. 3) then
		     gg3(k) = dfridr(k,ks,Re,hs2,err)
                 ge3(k) = err
                   if (ks .eq. Ny) then
                     gg(k) = gg3(k)
                     ge(k) = ge3(k)
                   endif
               endif
             enddo
c
           enddo
c--------------------------------------------------- 
c  Calculate coefficients an(i) using the above f's
c  OR the above g's.
c--------------------------------------------------- 
               write( 6,534) alamta, De, Re
               write(35,534) alamta, De, Re
             do k=2,ms0+1
               write( 6,550) k, ff(k), gg(k), ge(k)
               write(35,550) k, ff(k), gg(k), ge(k)
c-
               if (Np .eq. 0)  ff(k) = gg(k)
                 write(2,560)  ff(k)
                 write(27,*)   ff(k)
             enddo
c---
               write(6,536)
             do k=2,ms0+1
                ffk =  ff(k)/(aJtoau * aotoA0**k)
                ggk =  gg(k)/(aJtoau * aotoA0**k)
                ery = ggf(k)/(aJtoau * aotoA0**k)
               write(6,550) k, ffk, ggk, ery
C              write(6,550) k, ffk, ggk, ge(k)
             enddo
c--------------------------------------------------- 
c Print numerical "force constants"
c--------------------------------------------------- 
             write( 6,750) 
             write(35,750) 
           do k=2,Nryd
               write( 6,550) k, gg1(k), gg2(k), gg3(k)
               write(35,550) k, gg1(k), gg2(k), gg3(k)
             if (k .eq. ms0+1) then
	         write( 6,*)
	         write(35,*)
             endif 
           enddo
c---
             write( 6,760) 
             write(35,760) 
           do k=2,ms0+1
             write( 6,550) k, ge1(k), ge2(k), ge3(k)
             write(35,550) k, ge1(k), ge2(k), ge3(k)
           enddo
c--------------------------------------------------- 
c  V(R->inf) = De = Sum_n  f_n*(R - Re)**n/n! 
c--------------------------------------------------- 
                 kk = 0              
                 kj = 0              
                 bk = 1.0              
               vmsg = 0.0
               vmsl = 0.0
               vdeg = 0.0
               vdel = 0.0
		     Rg = Rmax + Rless
		     Rl = Rmax - Rless
             write( 6,730) De, Rmax, Rless, Dconv, Nryd
             write(35,730) De, Rmax, Rless, Dconv, Nryd
           do k=2,Nryd
               bk = bk*k
             vmsg = vmsg + gg1(k)*(Rg - Re)**k/bk
             vmsl = vmsl + gg1(k)*(Rl - Re)**k/bk
c
             vdeg = vdeg + ggf(k)*(Rg - Re)**k/bk
             vdel = vdel + ggf(k)*(Rl - Re)**k/bk
        write( 6,740) k, vmsg, vmsl, vdeg, vdel, vdeg-vdel
        write(35,740) k, vmsg, vmsl, vdeg, vdel, vdeg-vdel
c            if ( abs(vdeg - De) .lt. Dconv .and. kk .eq. 0 ) then
             if ( abs(vdeg - vdel) .lt. Dconv .and. kk .eq. 0 ) then
	         write( 6,*)
	         write(35,*)
                 kk = 1              
                 kc = k              
                vde = vdeg
             endif 
             if ( abs(vmsg - vmsl) .lt. Dconv .and. kj .eq. 0 ) then
                 kj = 1              
                 km = k              
                vdm = vmsg
             endif 
           enddo
             write( 6,742) kc, vde, km, vdm
C=================================================
c  Calculate ro-vibrational energies E_vj for AB.
C-------------------------------------------------
           IF (Ner .gt. 0) then
             call  calEvj(nv,nj,200,Nd,gg)
	     ENDIF
C=================================================
c
           IF (mall .gt. 1) GOTO  900
       
               an(2) = 0.5d0*(a1*a1-ff(2)/De)
               an(3) = a1*an(2)-a1**3/3.0d0-ff(3)/(6.0d0*De)
             if (ms .ge. 4) then
               an(4) = (a1**4-6.0d0*ff(2)*a1*a1/De-4.0d0*ff(3)*a1/De
     #                  - ff(4)/De)/24.0d0
             endif
               if (ms .ge. 5) then
                 an(5) = (a1**5-10.0d0*ff(2)*a1**3/De-10.0d0*ff(3)*
     #                 a1*a1/De-5.0d0*ff(4)*a1/De-ff(5)/De)/120.0d0
               endif

               write(6,542) 
             do k=1,ms
               write(6,550) k, an(k)
               write(3,560) an(k)
             enddo
c
        ENDIF
c
C===  End Feng's formulae.
C
C------------------------------------------------------------------
C=== Huxley & Murrell: J. Chem. Soc. Faraday Trans 2, 79, 323(1983) 
C      For Huxley-Murrell-Sorbie's formulae :
C
         IF (iforce .eq. 4) THEN
             read (5,*) wexe, be, ae
           if (iu .eq. 1) then
             wexe =wexe/autocm
               be =be/autocm
               ae =ae/autocm
           endif
               write(6,570) 
           f0 = 1.0 + ae*we/(6.0*be**2)
C----- For non-harmonic model
C          f2=1.000004496*amu*we*we
C----- For harmonic model
           f2=amu*we**2
           f3 = - 3.0*f2*f0/Re
           f4 = f2*( 15.0*f0*f0 - 8.0*wexe/be )/(Re*Re)
                ff(2) = f2
                ff(3) = f3
                ff(4) = f4
              write(6,580) f2, f3, f4
              do i=2,6
                  write(2,560) ff(i)
              enddo
            if (a1 .ne. 0.0d0) then
              an(2) = 0.5d0*(a1**2 - f2/De)
              an(3) = a1*an(2) - a1**3/3.0d0 - f3/(6.0*De)
                write(6,590) an(1),an(2),an(3)
            endif
c
C--- Huxley-Murrell coefficients are ready for V_hms(R)
c
         ENDIF
C=================================================
C  Calculate diatomic potentials if an are ready
C-------------------------------------------------
  75    Rdel=(R2-R1)/N
           R=R1-Rdel
        if (M .eq. 3) then
          write(8,600) 
          write(9,610) ms
          write(10,615) 
          write(13,655) 
          write(14,660) 
          write(15,670) 
          write(16,680) 
          write(17,690) 
        elseif (M .eq. 4) then
          write(8,605) 
          write(11,665) 
        endif
           i1 = 0
C-------------------------
C- Prepair  3-term Huxley-Murrell-Sorbie potential V_hms(R)=u(i) ;
C      OR :
C- Prepair  3-term Murrell-Sorbie potential V_ms(R)=u(i) ;
C- Prepair ms-term Murrell-Sorbie potential V_ms(R)=v(i) .
C    These potentials used Sun-Feng formulae for coefficients a(n).
C=========================
         Rtemp = 0.0d0
         Vtemp = 0.0d0
      do 80 i=1,N
          R=R+Rdel
        if ((R-Re) .ge. 0.5d0 .and. i1 .eq. 0) i1=i
          x=R-Re
          temp=1.0d0/beta
        do k=1,ms
          temp=temp + an(k)*x**k
          if (k .eq. 3) temp3=temp  
        enddo
          rr(i)=R
C---
C-   v/u might be the V_hms(R) OR the V_ms(R; SF coefficients)   
C---
          v(i) = - De*beta*temp*dexp(-a1*beta*x) 
          u(i) = - De*beta*temp3*dexp(-a1*beta*x) 
c
C=== Prepair Sun-Feng (ECM) potential for M = 3 :
c
   	  IF ( M .eq. 3 ) THEN
c
C- For Morse potential
 	         f2 = amu*We*We
    	       alph = dsqrt(0.5d0*f2/De)
  	     Vmorse = De*(dexp(-2.0d0*alph*x)-2.0d0*dexp(-alph*x))
  	    vmos(i) = Vmorse
c
C- For Rydberg potential
  	        Ralph = dsqrt(f2/De)
  	     Vrydberg = -De*(1.0d0+Ralph*x)*dexp(-Ralph*x)
  	      vryd(i) = Vrydberg
C
C- For Pseudo-Gaussion (PG) potential
 	     Pbeta = 0.5d0*dsqrt(4.0d0+f2*Re*Re/De) - 1.0d0
   	     Pf1 = 1.0d0-(Re/R)*(Re/R)
   	     Pf2 = 1.0d0-(R/Re)*(R/Re)
  	     Vpg = -De*(1.0d0+Pbeta*Pf1)*dexp(Pbeta*Pf2)
         vpg0(i) = Vpg
c
c- For Sun-Feng (ECM) potential :
c
c- fy is the force-field variational function  LAMDA(R)
c
C           fy = (x/R)
C           if (ms .eq. 3) then
C      	   fy = fy
C     	else if (ms .eq. 4) then
C      	   fy = (dabs(fy))**(1.0001d0)*x/dabs(x)
C     	else if (ms .eq. 5) then
C     	   fy = (dabs(fy))**(2.0001d0)*x/dabs(x)
C     	endif

  	      fy = ( x/R )**(ms-2)
     	      fy = fy*(1.0d0-dexp(-alamta**2.0d0*(x/Re)))
     	      fy = fy*alamta
         vms0(i) = v(i)
         ums0(i) = u(i)
C---
             Vsf1 = v(i)
          if (Nturn .lt. 2) then
             Vsf2 = Vmorse
          elseif (Nturn .eq. 2) then
             Vsf2 = Vrydberg
          elseif (Nturn .eq. 3) then
             Vsf2 = Vpg
          endif
C---
C  Next v(i) is the ms-term ECM potential 
C---
  	     v(i) = (fy+1.0d0)*Vsf1 - fy*Vsf2
c---
C  Next u(i) is the 3-term ECM potential 
C---
             Vsf1 = u(i)
  	     u(i) = (fy+1.0d0)*Vsf1 - fy*Vsf2
C---
C=== Finish Sun-Feng (ECM) potential
c
 	  ENDIF
c
   80 continue     
C========================================
C Find the switch value, Rtemp, of R
C----------------------------------------
           R=R + Rdel
           Rtemp = 0.0
      do 100 i=N, 2, -1
           R=R - Rdel
        if (R .lt. Re .and. Rtemp .eq. 0.0) then
            Rtemp = R
            Vecm0 = v(i)
          if (Nturn .eq. 0) then
             Vdif = dabs( v(i)-vms0(i) )
             Vmsp = vms0(i)
          elseif (Nturn .eq. 1) then
             Vdif = dabs( v(i)-vmos(i) )
             Vmsp = vmos(i)
          elseif (Nturn .eq. 2) then
             Vdif = dabs( v(i)-vryd(i) )
             Vmsp = vryd(i)
          elseif (Nturn .eq. 3) then
             Vdif = dabs( v(i)-vpg0(i) )
             Vmsp = vpg0(i)
          endif
            Vtemp = Vdif
        endif
  100 continue
C---------------------------------------------
C Set v(i) = Vecm = vmos(i)  for R =< Rtemp
C---------------------------------------------
           R=R1-Rdel
      do i=1, N
           R=R +Rdel
        if ((M .eq. 3) .and. (R .lt. Re)) then
          if (Nturn .eq. 0) then
            v(i) = vms0(i)
            u(i) = ums0(i)
          elseif (Nturn .eq. 1) then
            v(i) = vmos(i)
            u(i) = vmos(i)
          elseif (Nturn .eq. 2) then
  	      v(i) = vryd(i)
  	      u(i) = vryd(i)
          elseif (Nturn .eq. 3) then
  	      v(i) = vpg0(i)
  	      u(i) = vpg0(i)
          endif
        endif
          write(8,520)  rr(i),u(i)
          write(9,520)  rr(i),v(i)
          write(10,520) rr(i),v(i)-u(i)
      enddo
C---
          if (M .eq. 3) then
            if (Rtemp .gt. R1) then
	      write(6,525) Re, Rtemp, Vecm0, Vmsp, Vtemp
	    else
              write(6,527) Re,R1,Rtemp,Vtemp
	    endif
	  endif
C-
            b0 = 0.0d0
        if (ishift .eq. 0) go to 150
      do i=2,N
        if (i .le. i1 .and. v(i) .lt. v(i-1)) b= v(i)  
      enddo
C
        if (b .lt. 0.0) b=-b
c
        if (Nad .eq. 0) b0 = b
        if (Nad .gt. 0) b0 = Add
C
C--- Shift potential v(i) such that the minimum is zero.
C
        if (M .eq. 4) then
          write(6,620)  b0
          write(11,630) b0
        else if (M .eq. 3) then
          write(6,625)  b0
          write(11,635) b0
        endif
C
  150 do 200 i=1,N
            vshi = v(i) + b0
          write(6,520)  rr(i),vshi
          write(11,522) rr(i),vshi*autocm
        if (M .eq. 3) then
          write(13,520) rr(i),u(i) + b0
          write(14,520) rr(i),vshi
          write(15,520) rr(i),vmos(i) + b0
          write(16,520) rr(i),vryd(i) + b0
          write(17,520) rr(i),vpg0(i) + b0
          write(18,520) rr(i),vshi
        endif
  200 continue
C
C------ Print the cofficients an of HMS or SF (ECM) potential .
c         write(4,720) 
      do  222 i = 2, ms
c         write(4,550) i, an(i)
          write(4,560) an(i)
222   continue
C--------------  End print
C
C====================================================================
  501 format(//'  The Radial is Re = ',f10.6,' Anstr.  = ',f10.6,' a_0')
  502 format(//'  The Spectroscopic Constants are: '//)
  503 format(5x,A,1pe16.8,A,1pe16.8,A,/)
  510 format(//'  ***  MORSE vibrational potentials  ***'/,
     #'   R(a0)            V(R)  '/)
  515 format(//'  ***  Harmonic Oscillator potentials ***'/,
     #'   R(a0)            V(R)  '/)
  520 format(f10.6,1x,(1PE24.16))
  521 format(f10.6,1x,3(1PE24.16))
  522 format(f10.6,2x,f17.6)
c 525 format(//3x,'The largest R (< R_e) of |V_ecm - V_ms| > ',
c    # 1pe12.6,' is ',1pe9.3,' a_0')
  525 format(//9x,'V_ecm(R) = V_model(R) for those R =< Rtemp : ', 
     #//3x,'Re(a0)',4x,'Rtemp(a0)',2x,'V_ecm(cut;R)',2x,
     #'V_model(R)',4x,'V_dif(R)',/5(f10.6,2x) )
c    #10x,'V_dif(R)',/2x,f10.6,1x,3(1pe16.8,2x) )
c 527 format(//3x,'The value of |V_EMS - V_MS| between R = ',
c    # f10.6,' and ',f10.6,//3x,'are all < ',1pe12.6)
  527 format(//5x,'The cutoff values when Rtemp < Rmin : ',
     #/5x,'Re(a0)',4x,'Rmin(a0)',4x,'Rtemp(a0)',
     #3x,'V_ecm(cut;R)',/4x,3(f12.8,2x),1pe16.8)
  530 format(//' For Sun-Murrell-Sorbie potentials, Lambda = ',
     #f8.4,2x,':',//,'   The dissociation energy   De  = ',
     #1PE14.6,//,'   The adjustable parameter beta = ',1PE14.6,
     #//,'                     D = De*beta = ',1PE14.6,
     #//,'   and the nth force constants are ',
     #//5x,'  n   f(n; Har/ao**n) '/)
  532 format(//5x,'{ iab = 1, use x1 of (a*x*x + b*x +c) as f(3);',
     #/5x,'      = 2, use x2 of (a*x*x + b*x +c) as f(3)  }',/
     #/20x,' a =',1PE16.8,/20x,' b =',1PE16.8,/20x,' c =',1PE16.8,/
     #/5x,'  iab =',i2,';     x1 =',1PE16.8,/20x,'x2 =',1PE16.8,/)
  534 format(//3x,'For Sun-Feng (ECM) potentials, Lambda = ',f8.4,
     #2x,':',//,'   The dissociation energy   De  = ',1PE16.8,
     #//,'   Equili. intern. distance  Re  = ',1PE16.8,
     #//3x,' f(n) are from perturbation-theory formulae ',
     # /3x,'          and for Rydberg-like ECM V(R);',
     #//3x,'g_(n) are from numerical derivative code ',
     #//3x,'  err are the errors of g_(n).',
     #////14x,'   The nth force constants are : ',
     #//5x,"  n   f(n; Har/ao**n)  g_(n; Har/ao**n)     err(g) "/)
  536 format(///11x,'The nth force constants in other units are : ',
     #//7x,'    e(n) are the force constatns from analytical ',
     # /7x,'         derivatives of Rydberg potential.       ',
     #//10x,'      1 aJ = 1 attojoule = 0.229371 Har. ',
     # /10x,'             1 ao = 0.529177249 A ',
     #//5x,"  n    f(n; aJ/A**n)    g_(n; aJ/A**n)   e(n; aJ/A**n)"/)
C    #//5x,"  n    f(n; aJ/A**n)    g_(n; aJ/A**n)      err(g) "/)
  540 format(//' The expansion coefficients in Sun - ',
     #/,'Murrell - Sorbie potentials are :',
     #//5x,'  n    a(n; 1/ao**n) '/)
  542 format(//' The expansion coefficients in SF potential are : ',
     #//5x,'  n    a(n; 1/ao**n) '/)
c 546 format(//5x,'Equilibrium internuclear distance',4x,
c    #'Re =',1PE16.8,/39x,'Rstep =',1PE16.8,
c    #//7x,'n',8x,'R(ao)',7x,'g_n(2;Har/ao**2)     err(g) '/)
  550 format(5x,i3,2x,3(1PE16.8,x) )
  560 format(1PE24.16)
  570 format(//' --- Using Huxley-Murrell formulae --- ')
  580 format(//' The nth force constants for',
     #' Murrell - Sorbie potentials are :',
     #//5x,'F2 =',1PE14.6,4x,
     #/5x,'F3 =',1PE14.6,4x,
     #/5x,'F4 =',1PE14.6,4x)
  590 format(//' The expansion coefficients in',
     #' Sun-Murrell-Sorbie potentials are :',
     #//5x,'a1 =',1PE14.6,4x,
     #/5x,'a2 =',1PE14.6,4x,
     #/5x,'a3 =',1PE14.6,4x)
c 600 format(//'  ***  3-term Sun-Murrell-Sorbie potentials ***',/,
  600 format(//'  ***  3-term Sun-Feng (ECM) potentials ***',/,
     #'             ( Before shifting ) ',//,
     #'    R(a0)            V(R)  '/)
  605 format(//'  ***  Huxley-Murrell-Sorbie potentials ***',/,
     #'             ( Before shifting ) ',//,
     #'    R(a0)            V(R)  '/)
c 610 format(//'  ***  ms-term Sun-Murrell-Sorbie potentials ***',/,
  610 format(//'  ***  ms-term Sun-Feng (ECM) potentials ***;  ',
     #' ms =',i3,/8x,'       ( Before shifting ) ',//,
     #'     R(a0)            V(R)  '/)
c 615 format(//'  ***  Sun-Murrell-Sorbie potentials ***',/,
  615 format(//'  ***  ECM potential differences ***',/,
     #'          ( Before shifting ) ',//,
     #'    R(a0)       V(R) - V(R;3-term) '/)
  620 format(//'  ***  Huxley-Murrell-Sorbie potentials ***',/,
     #'       ( After shifting ) ',//,
     #'   The shifting constant is  Bshift =',f12.8,//
     #'    R(a0)         V(R; a.u.)  '/)
  625 format(//'  ***  Sun-Feng (ECM) potentials ***',/,
     #'           ( After shifting ) ',//,
     #'   The shifting constant is  Bshift =',f12.8,//
     #'    R(a0)         V(R; a.u.)  '/)
  630 format(//'  ***  Huxley-Murrell-Sorbie potentials ***',/,
     #'       ( After shifting ) ',//,
     #'   The shifting constant is  Bshift =',f12.8,//
     #'    R(a0)         V(R; cm-1)  '/)
  635 format(//'  ***  Sun-Feng (ECM) potentials ***',/,
     #'           ( After shifting ) ',//,
     #'   The shifting constant is  Bshift =',f12.8,//
     #'    R(a0)         V(R; cm-1)  '/)
  655 format(//'  ***  3-term Sun-Feng (ECM) potentials ***',/,
     #'             ( After shifting ) ',//,
     #'    R(a0)         V(R; a.u.)  '/)
  660 format(//'  ***  ms-term Sun-Feng (ECM) potentials ***',/,
     #'             ( After shifting ) ',//,
     #'    R(a0)         V(R; a.u.)  '/)
  665 format(//'  ***  Huxley-Murrell-Sorbie potentials ***',/,
     #'             ( After shifting ) ',//,
     #'    R(a0)         V(R; a.u.)  '/)
  670 format(//4x,'  ***  Morse potentials ***',/3x,
     #'       ( After shifting ) ',//,
     #'    R(a0)         V(R; a.u.)  '/)
  680 format(//3x,'  ***  Rydberg potentials ***',/3x,
     #'       ( After shifting ) ',//,
     #'    R(a0)         V(R; a.u.)  '/)
  690 format(//1x,'  ***  Pseudo-Gaussion (PG) potentials ***',
     #/7x,'       ( After shifting ) ',//,
     #'    R(a0)         V(R; a.u.)  '/)
  720 format(//' Expansion coefficients a(n) of ECM potential :',
     #//5x,'  n    a(n; 1/ao**n) '/)
  730 format(///6x,'--%&*  Checking the qualities of force',
     #' constants  *&%-- ',/ 
     #/8x,'  As R --> inf == Rmax, physically CORRECT force ',
     #/8x,'         constants f_n should satisfy : ',
     #//8x,'V(R->inf=Rmax)_i = De = Sum_n  f_n*(Rm_i - Re)**n/n! ',
     #//7x,'    De == De_g :   Rm_g = Rmax + Rless ;  i == g ',         
     # /7x,'    De == De_l :   Rm_l = Rmax - Rless ;  i == l ',         
     #//10x,'For De_i(m) = De_i(Morse) :  f_n == f_n(Morse)  ',
     # /10x,'For De_i(r) = De_i(Rydbg) :  f_n == f_n(Rydbg)  ',
     #//10x,'Dconv - Tolerance used to check if V(Rmax) = De ', 
     #//11x,'De',11x,'Rmax',10x,'Rless',11x,'Dconv',8x,'Nryd',
     #/4x,4(1pe14.6,x),3x,i3,
     #///'  n',5x,'De_g(m)',7x,'De_l(m)',7x,'De_g(r)',
     #7x,'De_l(r)',4x,'{De_g-De_l}_r ',/)
  740 format(i3,x,5(1pe14.6) )
  742 format(//17x,'V(R->inf=Rmax) = De  is reached with : ', 
     #//23x,'  n',5x,'De(Rydbg; a.u)',/23x,i3,3x,1pe16.8,
     #//23x,'  n',5x,'De(Morse; a.u)',/23x,i3,3x,1pe16.8,/)
  750 format(///3x,'Force constants ( h ) are from numerical ',
     #'derivative code  "dfridr" ;',
     #/3x,'Force constants ( p & g ) are from ANAlytical formulae .',
     #/3x,'         For  V_p-g(R),  2 =< n =< 11  ONLY  . ',
     #//5x,"From    V_morse(R)      V_rydberg(R)       V_p-g(R)  ",
     # /5x,"  n   p(n; Har/ao**n)  g(n; Har/ao**n)  h(n; Har/ao**n) "/)
  760 format(//12x,'The ERRORS for these force constants are : ',
     #//5x,"  n     e( V_morse )    e( V_rydberg )     e( V_p-g ) "/)
  800 format(///13x,'=#=  OUTPUT from "vibPOT.f"  =#= ',/
     #/6x,'The switches for NUMerical derivatives & E_vj : ',/
     #/3x,"[  Np = 0, use f's from n_th derivatives; ",
     #/3x,"      = 1, use f's from V_ecm & perturbation theory . ",
     #/3x,'   Ny = 1, use  Vmorse  for analytical derivatives; ',
     #/3x,'      = 2, use Vrydberg for numerical  derivatives; ',
     #/3x,'            Expon. of Vryd. : a = b*dsqrt(amu/De) ',
     #/3x,'            b = We + bryd;    originally b = We . ',
     #/3x,'      = 3, use Vpseudo_gaussian for nume. derivatives. ',
     #/3x,'  Ner = 0, do NOT cal. ro-vib energies E_vj ; ',
     #/3x,"      > 0, calc. E_vj from f'n by num. derivatives. ",
     #/3x,"             Set Ner = # of force constants wanted.  ",
     #/3x,'   nv -> nv is the # of vibrational states  wanted.   ]',
     #//14x,'         Np     Ny     Ner     nv ',/
     #23x,i2,5x,i2,5x,i2,6x,i2//
     #/3x,'[ bryd - Variational constant used to adjust Vryd(R), ',
     #/3x,'              bryd is meaningless for Ny = 1, 3 .     ',
     #/3x,'  hs1, hs2 --, Estimated initial stepsize used by the ',
     #/3x,'                 numerical derivative code "dfirdr" ; ',
     #/3x,'                 hs1 for Vrydberg,   hs2 for V_p-g  . ]',
     #//3x,' bryd =',f16.12,3x,'hs1 =',f8.4,3x,'hs2 =',f8.4,/
     #//3x,'   nj = is no. of rotational states in each vib. state :',
     #//23x,'  v      nj  ',/)
  810 format(23x,i3,4x,i4)
  820 format(///3x,'Switches for scale SOME calc. VIB-ROT constants :',
     #//3x,'[ aye, ..., are = 0, Zero calc. VIB. constants ',
     # /3x,'                     WeYe, WeZe, WeTe, WeSe, WeRe ; ',
     # /3x,'                = 1, Do NOT change sign of constants; ',
     # /3x,'                =-1, Change the sign of VIB. constants. ',
     #//3x,'  abe, aae, age, ',
     # /3x,'  ae3  ---  ae7 = 0, Zero calc. ROT. constants  ',
     # /3x,'                     B_e,Alpha_e,Gamma_e,Eta3,...,Eta7 ; ',
     # /3x,'                = 1, Do NOT change sign of constants; ',
     # /3x,'                =-1, Change the sign of ROT. constants. ',
     #//3x,'  ade, abt, ax2, ',
     # /3x,'  ax3  ---  ax7 = 0, Zero calc. ROT. constants  ',
     # /3x,'                     D_e,Beta_e,Xsi2,Xsi3, ...,Xsi7 ; ',
     # /3x,'                = 1, Do NOT change sign of constants; ',
     # /3x,'                =-1, Change the sign of ROT. constants. ]',
     #//8x,'        aye  aze  ate  ase  are   ',
     #/15x,f4.1,1x,f4.1,1x,f4.1,1x,f4.1,1x,f4.1,1x,
     #//3x,'        abe  aae  age  ae3  ae4  ae5  ae6  ae7  ',
     #/10x,f4.1,1x,f4.1,1x,f4.1,1x,f4.1,1x,f4.1,1x,f4.1,
     #1x,f4.1,1x,f4.1,
     #//3x,'        ade  abt  ax2  ax3  ax4  ax5  ax6  ax7  ',
     #/10x,f4.1,1x,f4.1,1x,f4.1,1x,f4.1,1x,f4.1,1x,f4.1,
     #1x,f4.1,1x,f4.1,/)
c
  900   stop
      end
c
      REAL*8 FUNCTION FACX(I)
        implicit real*8(a-h,o-z)
C-----------------------------------------------------------------
C     THIS IS A FACTORIAL FUNCTION  I!
C-----------------------------------------------------------------
      DIMENSION TABLE(15)
      DATA TABLE/1.0D+00,2.0D+00,6.0D+00,24.0D+00,120.0D+00,720.0D+00,
     1      5040.0D+00,40320.0D+00,362880.0D+00,
     2      36288.0D+02,399168.0D+02,4790016.0D+02,62270208.0D+02,
     3      871782912.0D+02,1307674368.0D+03/
c---
        FACX=1.0
      IF (I)95,100,10
   10 IF (I-15)20,20,30
   20 FACX=TABLE(I)
      GO TO 200
   30 FJ=16.0D+00
      FACX=TABLE(15)
      DO 40 J=16,I
      FACX=FACX*FJ
   40 FJ=FJ+1.0D+00
      GO TO 200
   95 FACX=0.0D+00
  100 CONTINUE
  200 RETURN
      END
C===
c
      subroutine  getff(ff,n1,iab,kab)
      implicit real*8(a-h,o-z)
      common /fms/ ms, Ny
      common /spectra/ amu,Re,De,We,WeXe,WeYe,WeZe,WeTe,WeSe,WeRe
      common /spectrb/ Be,alphae,gamae,Der,betae,bryd  
      dimension  ff(n1)
c
c -- Use the Eqs. of ms=3 to get the approx. (F3, F4) for later using
c
  	        a = amu*We
               fa = 1675.0d0/(48.0d0*amu*We*We)
               fb = -12.0d0*Re*(amu*We*Re)**2
               fc = -36.0d0*amu**3*We**4*Re**2 
               fc = fc - 24.0d0*Re*(amu*We*Re)**5*alphae
               fc = fc - 335.0d0*amu**2*We**2*WeXe
            call  quadratic(fa,fb,fc,x1,x2)
                write(6,100) fa, fb, fc, iab, x1, x2
              if (iab .eq. 1)then
                ff(3) = x1
              else
                ff(3) = x2
              endif
            ff(4) = 5.0d0*ff(3)*ff(3)/(3.0d0*amu*We*We) 
            ff(4) = ff(4) - 16.0d0*(amu*We)**2*WeXe
c
c -- Solve F5 for ms=4 OR (F5, F6) for ms=5 :
c
      IF (ms .eq. 4) THEN
        if (kab .eq. 1) then
          f5 = - 3.0/(2.0*amu*a*Re**4) + 335.*ff(4)/(384.0*a**5 *Re**6)
          f0 = (1.0 + 95.0/(8.0*a**2 *Re**4) )*ff(3)/(2.0*a**3 *Re**3)
          ff(5) = (96.0*a**5 *Re**5/19.0) * (f5 - f0 - alphae)
        else
          f5 = WeXe - 5.0*ff(3)**2/(48.0*We*a**3) + ff(4)/(16.0*a*a)
          ff(5) = dsqrt( 92160.0*We*a**5 * f5/217.0 )
        endif
      ELSEIF (ms .eq. 5) THEN
          F5a = 19.0/(96.0*a**5 *Re**5)
          F6a = -(5.0 + 134939.0/(720.0*a*a*Re**4))/(256.0*a**5 *Re**4)
          F5b = 217.0/(92160.0*We*a**5)
          F6b = 177.0*ff(4)/(55296.0*We*a**5)
           f0 = - (3.0 + 175.0/(8.0*a*a*Re**4))/(2.0*amu*a*Re**4) 
           f0 = f0 - (1.0+95.0/(8.0*a*a*Re**4))*ff(3)/(2.0*a**3 *Re**3) 
         F56a = f0 + 335.0*ff(4)/(384.0*a**5 *Re**6) - alphae
         F56b = WeXe - 5.0*ff(3)**2/(48.0*We*a**3) + ff(4)/(16.0*a*a)
           Aa = F5b
           Ab = - F6b*F5a/F6a
           Ac =   F6b*F56a/F6a  - F56b
c-
         call  quadratic(Aa,Ab,Ac,x1,x2)
            write(6,110) Aa, Ab, Ac, iab, x1, x2
         if (iab .eq. 1)then
           ff(5) = x1
         else
           ff(5) = x2
         endif
c-
c        if (iab .eq. 1)then
c          ff(5) = 0.5d0*( - Ab + dsqrt( Ab*Ab - 4.0*Aa*Ac ) )/Aa
c        else
c          ff(5) = 0.5d0*( - Ab - dsqrt( Ab*Ab - 4.0*Aa*Ac ) )/Aa
c        endif
c-
           ff(6) = ( F56a - F5a*ff(5) )/F6a
      ENDIF
c-
  100 format(//5x,'{ iab = 1, use x1 of (a*x*x + b*x +c) as f(3);',
     #/5x,'      = 2, use x2 of (a*x*x + b*x +c) as f(3)  }',/
     #/20x,' a =',1PE16.8,/20x,' b =',1PE16.8,/20x,' c =',1PE16.8,/
     #/5x,'  iab =',i2,';     x1 =',1PE16.8,/20x,'x2 =',1PE16.8,/)
  110 format(//5x,'{ iab = 1, use x1 of (A*x*x + B*x +C) as f(5);',
     #/5x,'      = 2, use x2 of (A*x*x + B*x +C) as f(5)  }',/
     #/20x,' A =',1PE16.8,/20x,' B =',1PE16.8,/20x,' C =',1PE16.8,/
     #/5x,'  iab =',i2,';     x1 =',1PE16.8,/20x,'x2 =',1PE16.8,/)
c-
        return
      end
C===
      subroutine calaaf
      implicit real*8(a-h,o-z)
      common /aafcom/ aaf(4,10)
      common /fms/ ms, Ny
      common /spectra/ amu,Re,De,We,WeXe,WeYe,WeZe,WeTe,WeSe,WeRe
      common /spectrb/ Be,alphae,gamae,Der,betae,bryd  
c---
  	       alp = amu*We
  	  aaf(1,1) = 5.0d0/(48.0d0*We*alp**3)
   	  aaf(1,2) = -1.0d0/(16.0d0*alp**2)
  	  aaf(1,3) = 217.0d0/(92160.0d0*We*alp**5)
  	  aaf(1,4) = 177.0d0/(55296.0d0*We*alp**5)
  	  aaf(1,5) = -WeXe

    	  aaf(2,1) = -17.0d0/(2304.0d0*We*alp**4)
  	  aaf(2,2) = -7.0d0/(288.0d0*We*alp**4)
  	  aaf(2,3) = 1.0d0/(288.0d0*alp**3)
  	  aaf(2,4) = -829.0d0/(3317760.0d0*We*alp**6)
  	  aaf(2,5) = -WeYe

  	  aaf(3,1) = -7.0d0/(5120.0d0*We*alp**5)
  	  aaf(3,2) = -11.0d0/(9216.0d0*We*alp**5)
  	  aaf(3,3) =  -WeZe

   	  aaf(4,1) = -(1.0d0+95.0d0/(8.0d0*alp**2*Re**4))/
     # (2.0d0*alp**3*Re**3)
  	  aaf(4,2) = 335.0d0/(384.0d0*alp**5*Re**6)
  	  aaf(4,3) = -19.0d0/(96.0d0*alp**5*Re**5)
  	  aaf(4,4) = (5.0d0+134939.0d0/(720.0d0*alp**2*Re**4))/
     # (256.0d0*alp**5*Re**4)
  	  aaf(4,5) = -(3.0d0+175.0d0/(8.0d0*alp**2*Re**4))/
     # (2.0d0*amu*alp*Re**4) - alphae
        if (ms .eq. 4) then
  	   aaf(4,5) = -3.0d0/(2.0d0*amu*alp*Re**4) - alphae
        endif
C
  	  do 10 k = 1, 4
  	    write(24,100)aaf(k,1),aaf(k,2),aaf(k,3),aaf(k,4),aaf(k,5)
10  	continue
100  	format(1x,5(1pe16.7))
C
 	  return
  	end
C===
      subroutine calff(ff,N,N1,method)
        implicit real*8(a-h,o-z)
      parameter (NP=40)
      dimension  x(NP),fvec(NP),ff(N1)
      logical check
C-----------------------------------------------------
C N=ms-1; N=3 : f3, f4, f5     => x1, x2, x3 ;  
c         N=4 : f3, f4, f5, f6 => x1, x2, x3, x4.
C-----------------------------------------------------
  	  check = .false.
  	do 10 k = 1, N
  	   x(k) = ff(k+2)
10  	continue
      if (method .eq. 1) then
  	  call broydn(x,N,NP,check)
      else if (method .eq. 2) then
  	  call newt(x,N,NP,check)
      endif
C
  	  call funcv(NP,N,x,fvec)
  	do 20 k = 1,N
  	   write(25,*)"fvec(",k,") = ",fvec(k)
20  	continue
  	do 30 k = 1, N
  	   ff(k+2) = x(k) 
30  	continue
  	return
  	end
C===
      subroutine funcv(n1,n,x,fvec)
        implicit real*8(a-h,o-z)
      dimension  x(n1),fvec(n1)
      common /aafcom/ aaf(4,10)
      common /fms/ ms, Ny
c---
      if (ms .eq. 5) then
       fvec(1) = aaf(1,1)*x(1)**2+aaf(1,2)*x(2)+aaf(1,3)*x(3)**2
     #  +aaf(1,4)*x(2)*x(4)+aaf(1,5)
       fvec(2) = aaf(2,1)*x(2)**2+aaf(2,2)*x(1)*x(3)
     #  +aaf(2,3)*x(4)+aaf(2,4)*x(4)**2+aaf(2,5)
       fvec(3) = aaf(3,1)*x(3)**2+aaf(3,2)*x(2)*x(4)+aaf(3,3)
       fvec(4) = aaf(4,1)*x(1)+aaf(4,2)*x(2)
     #  +aaf(4,3)*x(3)+aaf(4,4)*x(4)+aaf(4,5)
c
      else if (ms .eq. 4) then
       fvec(1) = aaf(1,1)*x(1)**2+aaf(1,2)*x(2)+aaf(1,3)*x(3)**2
     #  +aaf(1,5)
       fvec(2) = aaf(2,1)*x(2)**2+aaf(2,2)*x(1)*x(3)
     #  +aaf(2,5)
       fvec(3) = aaf(4,1)*x(1)+aaf(4,2)*x(2)
     #  +aaf(4,3)*x(3)+aaf(4,5)
      endif
        return 
      end
C===
      SUBROUTINE broydn(x,n,n1,check)
        implicit real*8(a-h,o-z)
      LOGICAL check,restrt,sing,skip
      PARAMETER (NP=40,MAXITS=20000,EPS=1.e-30,TOLF=1.e-30)
      PARAMETER (TOLMIN=1.e-30,TOLX=EPS,STPMX=900.0)
C     LOGICAL restrt,sing,skip
      COMMON /newtv/ fvec(NP),nn
      dimension  x(n1),c(NP),d(NP),fvcold(NP),P(NP),qt(NP,NP)
      dimension  r(NP,NP),s(NP),t(NP),w(NP),xold(NP),g(NP)
      EXTERNAL fmin
c---
CU    USES fdjac,fmin,lnsrch,qrdcmp,qrupdt,rsolv
c---
      nn=n
C
c- "fmin" produces NEW array  fvec :
c
      f=fmin(n1,x)
        test=0.
      do 11 i=1,n
        if(abs(fvec(i)).gt.test)test=abs(fvec(i))
11    continue
        if (test .lt. 0.01*TOLF) return
          sum=0.
      do 12 i=1,n
        sum=sum+x(i)**2
12    continue
        stpmax=STPMX*max(sqrt(sum),float(n))
        restrt=.true.
      do 44 its=1,MAXITS
        if(restrt)then
          call fdjac(n,x,fvec,NP,r)
          call qrdcmp(r,n,NP,c,d,sing)
            if(sing) pause 'singular Jacobian in broydn'
          do 14 i=1,n
            do 13 j=1,n
              qt(i,j)=0.
13          continue
            qt(i,i)=1.
14        continue
          do 18 k=1,n-1
            if(c(k).ne.0.)then
              do 17 j=1,n
                sum=0.
                do 15 i=k,n
                  sum=sum+r(i,k)*qt(i,j)
15              continue
                sum=sum/c(k)
                do 16 i=k,n
                  qt(i,j)=qt(i,j)-sum*r(i,k)
16              continue
17            continue
            endif
18        continue
          do 21 i=1,n
            r(i,i)=d(i)
            do 19 j=1,i-1
              r(i,j)=0.
19          continue
21        continue
        else
          do 22 i=1,n
            s(i)=x(i)-xold(i)
22        continue
          do 24 i=1,n
            sum=0.
            do 23 j=i,n
              sum=sum+r(i,j)*s(j)
23          continue
            t(i)=sum
24        continue
          skip=.true.
          do 26 i=1,n
            sum=0.
            do 25 j=1,n
              sum=sum+qt(j,i)*t(j)
25          continue
            w(i)=fvec(i)-fvcold(i)-sum
            if(abs(w(i)).ge.EPS*(abs(fvec(i))+abs(fvcold(i))))then
              skip=.false.
            else
              w(i)=0.
            endif
26        continue
          if(.not.skip)then
            do 28 i=1,n
              sum=0.
              do 27 j=1,n
                sum=sum+qt(i,j)*w(j)
27            continue
              t(i)=sum
28          continue
            den=0.
            do 29 i=1,n
              den=den+s(i)**2
29          continue
            do 31 i=1,n
              s(i)=s(i)/den
31          continue
              call qrupdt(r,qt,n,NP,t,s)
            do 32 i=1,n
              if(r(i,i).eq.0.) pause 'r singular in broydn'
              d(i)=r(i,i)
32          continue
          endif
        endif
        do 34 i=1,n
          sum=0.
          do 33 j=1,n
            sum=sum+qt(i,j)*fvec(j)
33        continue
          g(i)=sum
34      continue
        do 36 i=n,1,-1
          sum=0.
          do 35 j=1,i
            sum=sum+r(j,i)*g(j)
35        continue
          g(i)=sum
36      continue
        do 37 i=1,n
          xold(i)=x(i)
          fvcold(i)=fvec(i)
37      continue
        fold=f
        do 39 i=1,n
          sum=0.
          do 38 j=1,n
            sum=sum+qt(i,j)*fvec(j)
38        continue
          p(i)=-sum
39      continue
          call rsolv(r,n,NP,d,p)
C
c- "lnsrch" produces NEW array  fvec :
c
        call lnsrch(NP,n,xold,fold,g,p,x,f,stpmax,check)
C       call lnsrch(NP,n,xold,fold,g,p,x,f,stpmax,check,fmin)
          test=0.
        do 41 i=1,n
          if(abs(fvec(i)).gt.test)test=abs(fvec(i))
41      continue
        if(test.lt.TOLF)then
          check=.false.
          return
        endif
        if(check)then
          if(restrt)then
            return
          else
            test=0.
            den=max(f,.5*n)
            do 42 i=1,n
              temp=abs(g(i))*max(abs(x(i)),1.)/den
              if(temp.gt.test)test=temp
42          continue
            if(test.lt.TOLMIN)then
              return
            else
              restrt=.true.
            endif
          endif
        else
          restrt=.false.
          test=0.
          do 43 i=1,n
            temp=(abs(x(i)-xold(i)))/max(abs(x(i)),1.)
            if(temp.gt.test)test=temp
43        continue
          if(test.lt.TOLX)return
        endif
44    continue
        pause 'MAXITS exceeded in broydn'
      END

CN 

      SUBROUTINE fdjac(n,x,fvec,np,df)
        implicit real*8(a-h,o-z)
      PARAMETER (NP1=40,EPS=1.e-12)
      dimension  df(np,np),fvec(np),x(np),f(NP1)
c---
c     dimension  df(np,np),fvec(n),x(n),f(NP1)
CU    USES funcv
c---
      do 12 j=1,n
        temp=x(j)
        h=EPS*abs(temp)
        if(h.eq.0.)h=EPS
        x(j)=temp+h
           h=x(j)-temp
        call funcv(np,n,x,f)
           x(j)=temp
        do 11 i=1,n
          df(i,j)=(f(i)-fvec(i))/h
11      continue
12    continue
      return
      END

CN

C     SUBROUTINE lnsrch(np,n,xold,fold,g,p,x,f,stpmax,check,func)
c     SUBROUTINE lnsrch(np,n,xold,fold,g,p,x,f,stpmax,check,fmin)
c===============
      SUBROUTINE lnsrch(np,n,xold,fold,g,p,x,f,stpmax,check)
        implicit real*8(a-h,o-z)
      LOGICAL check
      PARAMETER (ALF=1.e-12,TOLX=1.e-12)
      dimension  g(np),p(np),x(np),xold(np)
      EXTERNAL fmin
C---
C     EXTERNAL func
CU    USES func
C-  There is NO an external function called "func" in this code !
CU    USES fmin
C---
      check=.false.
        sum=0.0d0
      do 11 i=1,n
        sum=sum+p(i)*p(i)
11    continue
      sum=sqrt(sum)
      if(sum.gt.stpmax)then
        do 12 i=1,n
          p(i)=p(i)*stpmax/sum
12      continue
      endif
      slope=0.
      do 13 i=1,n
        slope=slope+g(i)*p(i)
13    continue
      test=0.
      do 14 i=1,n
        temp=abs(p(i))/max(abs(xold(i)),1.)
        if(temp.gt.test)test=temp
14    continue
      alamin=TOLX/test
      alam=1.
1     continue
        do 15 i=1,n
          x(i)=xold(i)+alam*p(i)
15      continue
C
c- "fmin" produces NEW array  fvec :
c
        f=fmin(np,x)
C       f=func(x)
        if(alam.lt.alamin)then
          do 16 i=1,n
            x(i)=xold(i)
16        continue
          check=.true.
          return
        else if(f .le.  fold+ALF*alam*slope )then
          return
        else
          if(alam.eq.1.)then
            tmplam=-slope/(2.*(f-fold-slope))
          else
            rhs1=f-fold-alam*slope
            rhs2=f2-fold2-alam2*slope
            a=(rhs1/alam**2-rhs2/alam2**2)/(alam-alam2)
            b=(-alam2*rhs1/alam**2+alam*rhs2/alam2**2)/(alam-alam2)
            if(a.eq.0.)then
              tmplam=-slope/(2.*b)
            else
              disc=b*b-3.*a*slope
              tmplam=(-b+sqrt(disc))/(3.*a)
            endif
            if(tmplam.gt..5*alam)tmplam=.5*alam
          endif
        endif
        alam2=alam
        f2=f
        fold2=fold
        alam=max(tmplam,.1*alam)
      goto 1
      END

CN

      SUBROUTINE qrupdt(r,qt,n,np,u,v)
        implicit real*8(a-h,o-z)
      dimension  r(np,np),qt(np,np),u(np),v(np)
c---
CU    USES rotate
c---
      do 11 k=n,1,-1
        if(u(k).ne.0.)goto 1
11    continue
        k=1
1     do 12 i=k-1,1,-1
        call rotate(r,qt,n,np,i,u(i),-u(i+1))
        if(u(i).eq.0.)then
          u(i)=abs(u(i+1))
        else if(abs(u(i)).gt.abs(u(i+1)))then
          u(i)=abs(u(i))*sqrt(1.+(u(i+1)/u(i))**2)
        else
          u(i)=abs(u(i+1))*sqrt(1.+(u(i)/u(i+1))**2)
        endif
12    continue
      do 13 j=1,n
        r(1,j)=r(1,j)+u(1)*v(j)
13    continue
      do 14 i=1,k-1
        call rotate(r,qt,n,np,i,r(i,i),-r(i+1,i))
14    continue
        return
      END

CN

      REAL*8 FUNCTION fmin(n1,x)
        implicit real*8(a-h,o-z)
      PARAMETER (NP=40)
      COMMON /newtv/ fvec(NP),n
      dimension  x(n1)
      SAVE /newtv/
c---
CU    USES funcv
c---
      call funcv(NP,n,x,fvec)
        sum=0.
      do 11 i=1,n
        sum=sum+fvec(i)**2
11    continue
        fmin=0.5*sum
      return
      END

CN
      SUBROUTINE qrdcmp(a,n,np,c,d,sing)
        implicit real*8(a-h,o-z)
      dimension  a(np,np),c(np),d(np)
      LOGICAL sing
c---
      sing=.false.
        scale=0.
      do 17 k=1,n-1
        do 11 i=k,n
          scale=max(scale,abs(a(i,k)))
11      continue
        if(scale.eq.0.)then
          sing=.true.
          c(k)=0.
          d(k)=0.
        else
          do 12 i=k,n
            a(i,k)=a(i,k)/scale
12        continue
          sum=0.
          do 13 i=k,n
            sum=sum+a(i,k)**2
13        continue
          sigma=sign(sqrt(sum),a(k,k))
          a(k,k)=a(k,k)+sigma
          c(k)=sigma*a(k,k)
          d(k)=-scale*sigma
          do 16 j=k+1,n
            sum=0.
            do 14 i=k,n
              sum=sum+a(i,k)*a(i,j)
14          continue
            tau=sum/c(k)
            do 15 i=k,n
              a(i,j)=a(i,j)-tau*a(i,k)
15          continue
16        continue
        endif
17    continue
        d(n)=a(n,n)
      if(d(n).eq.0.)sing=.true.
        return
      END

CN

      SUBROUTINE rsolv(a,n,np,d,b)
        implicit real*8(a-h,o-z)
      dimension  a(np,np),b(np),d(np)
c---
      b(n)=b(n)/d(n)
      do 12 i=n-1,1,-1
        sum=0.
        do 11 j=i+1,n
          sum=sum+a(i,j)*b(j)
11      continue
        b(i)=(b(i)-sum)/d(i)
12    continue
      return
      END

CN

      SUBROUTINE rotate(r,qt,n,np,i,a,b)
        implicit real*8(a-h,o-z)
      dimension  r(np,np),qt(np,np)
c---
      if(a .eq. 0.) then
        c=0.
        s=sign(1.,b)
      else if(abs(a).gt.abs(b))then
        fact=b/a
        c=sign(1./sqrt(1.+fact**2),a)
        s=fact*c
      else
        fact=a/b
        s=sign(1./sqrt(1.+fact**2),b)
        c=fact*s
      endif
      do 11 j=i,n
        y=r(i,j)
        w=r(i+1,j)
        r(i,j)=c*y-s*w
        r(i+1,j)=s*y+c*w
11    continue
      do 12 j=1,n
        y=qt(i,j)
        w=qt(i+1,j)
        qt(i,j)=c*y-s*w
        qt(i+1,j)=s*y+c*w
12    continue
      return
      END

CN
      SUBROUTINE newt(x,n,n1,check)
        implicit real*8(a-h,o-z)
      LOGICAL check
      PARAMETER (NP=40,MAXITS=90000,TOLF=1.e-30,TOLMIN=1.e-30)
      PARAMETER (TOLX=1.e-30,STPMX=900.0)
      dimension  x(n1),fjac(NP,NP),g(NP),p(NP),xold(NP),indx(NP)
      COMMON /newtv/ fvec(NP),nn
      SAVE /newtv/
      EXTERNAL fmin
c---
CU    USES fdjac,fmin,lnsrch,lubksb,ludcmp
c---
        nn=n
C
c- "fmin" produces NEW array  fvec :
c
      f=fmin(n1,x)
        test=0.
      do 11 i=1,n
        if(abs(fvec(i)).gt.test)test=abs(fvec(i))
11    continue
      if(test.lt..01*TOLF)return
      sum=0.
      do 12 i=1,n
        sum=sum+x(i)**2
12    continue
      stpmax=STPMX*max(sqrt(sum),float(n))
C
c- "fdjac" produces ANOTHER array  fvec :
c
      do 21 its=1,MAXITS
        call fdjac(n,x,fvec,NP,fjac)
        do 14 i=1,n
          sum=0.
          do 13 j=1,n
            sum=sum+fjac(j,i)*fvec(j)
13        continue
          g(i)=sum
14      continue
        do 15 i=1,n
          xold(i)=x(i)
15      continue
        fold=f
        do 16 i=1,n
          p(i)=-fvec(i)
16      continue
        call ludcmp(fjac,n,NP,indx,d)
        call lubksb(fjac,n,NP,indx,p)
C
c- "lnsrch" produces ANOTHER array  fvec :
c
        call lnsrch(NP,n,xold,fold,g,p,x,f,stpmax,check)
C       call lnsrch(NP,n,xold,fold,g,p,x,f,stpmax,check,fmin)
          test=0.
        do 17 i=1,n
          if(abs(fvec(i)).gt.test)test=abs(fvec(i))
17      continue
        if(test.lt.TOLF)then
          check=.false.
          return
        endif
        if(check)then
          test=0.
          den=max(f,.5*n)
          do 18 i=1,n
            temp=abs(g(i))*max(abs(x(i)),1.)/den
            if(temp.gt.test)test=temp
18        continue
          if(test.lt.TOLMIN)then
            check=.true.
          else
            check=.false.
          endif
          return
        endif
        test=0.
        do 19 i=1,n
          temp=(abs(x(i)-xold(i)))/max(abs(x(i)),1.)
          if(temp.gt.test)test=temp
19      continue
        if(test.lt.TOLX)return
21    continue
        pause 'MAXITS exceeded in newt'
      END
C===
      SUBROUTINE lubksb(a,n,np,indx,b)
        implicit real*8(a-h,o-z)
      dimension  a(np,np),b(np),indx(np)
c---
C     dimension  a(np,np),b(n),indx(n)
c---
      ii=0
      do 12 i=1,n
        ll=indx(i)
        sum=b(ll)
        b(ll)=b(i)
        if (ii.ne.0)then
          do 11 j=ii,i-1
            sum=sum-a(i,j)*b(j)
11        continue
        else if (sum.ne.0.) then
          ii=i
        endif
        b(i)=sum
12    continue
      do 14 i=n,1,-1
        sum=b(i)
        do 13 j=i+1,n
          sum=sum-a(i,j)*b(j)
13      continue
        b(i)=sum/a(i,i)
14    continue
      return
      END

CN

      SUBROUTINE ludcmp(a,n,np,indx,d)
        implicit real*8(a-h,o-z)
      PARAMETER (NMAX=500,TINY=1.0e-20)
      dimension  indx(np),a(np,np),vv(NMAX)
c---
C     dimension  indx(n),a(np,np),vv(NMAX)
c---
        d=1.
      do 12 i=1,n
        aamax=0.
        do 11 j=1,n
          if (abs(a(i,j)).gt.aamax) aamax=abs(a(i,j))
11      continue
        if (aamax.eq.0.) pause 'singular matrix in ludcmp'
        vv(i)=1./aamax
12    continue
      do 19 j=1,n
        do 14 i=1,j-1
          sum=a(i,j)
          do 13 k=1,i-1
            sum=sum-a(i,k)*a(k,j)
13        continue
          a(i,j)=sum
14      continue
        aamax=0.
        do 16 i=j,n
          sum=a(i,j)
          do 15 k=1,j-1
            sum=sum-a(i,k)*a(k,j)
15        continue
          a(i,j)=sum
          dum=vv(i)*abs(sum)
          if (dum.ge.aamax) then
            imax=i
            aamax=dum
          endif
16      continue
        if (j.ne.imax)then
          do 17 k=1,n
            dum=a(imax,k)
            a(imax,k)=a(j,k)
            a(j,k)=dum
17        continue
          d=-d
          vv(imax)=vv(j)
        endif
        indx(j)=imax
        if(a(j,j).eq.0.)a(j,j)=TINY
        if(j.ne.n)then
          dum=1./a(j,j)
          do 18 i=j+1,n
            a(i,j)=a(i,j)*dum
18        continue
        endif
19    continue
        return
      END
C===
C
      subroutine quadratic(a,b,c,x1,x2)
      implicit real*8(a-h,o-z)
c----------------------------------------------------------
C   Find the roots, x1 & x2, of a quadratic equation
C               a*x*x + b*x + c = 0
C using the skills on P. 178 of "Numerical Reccipes".
c       write(6,*) '  sign(1.0, b) = ', sign(1.0,b)
c----------------------------------------------------------
      q = -0.5d0*(b + sign(1.0,b)*dsqrt( b*b - 4.0*a*c) )
        x1 = q/a
        x2 = c/q
c---
        return
      end
C===
c
      function dfridr(n,ns,x,h,err)
      implicit real*8(a-h,o-z)
c----------------------------------------------------------
c   Returns the derivative of a function func at a point
c x by Ridders' method of polynomial extrapolation. The 
c value h is input as an estimated initial stepsize; it
c needs not be small, but rather should be an increment
c in x over which func changes substantially. An estimate
c of the error in the derivative is returned as err.
c   Parameters:  Stepsize is decreased by CON at each
c iteration. Max size of tableau is set by NTAB. Return
c when error is SAFE worse than the best so far.
c   n -- The order of derivative.
c
c             Experiments on parameters :
c          --------------------------------
c           SAFE               Results   err          h
c ---------------------------  -------  ------  -----------
c 1.001, 1.01, 1.5, 2.0, 5.0    GOOD    1*E-16  0.001 - 4.0
c
c   Results are NOT sensitive to  CON, NTAB.
c     EXTERNAL func
c-
c     PARAMETER (CON=1.4,CON2=CON*CON,BIG=1.E30,NTAB=10,SAFE=2.)
c----------------------------------------------------------
      EXTERNAL fpot
      PARAMETER (NTAB=100)
      dimension  a(NTAB,NTAB)
c---------------------------------
c  ns=2 for V_rydberg(R)
c---------------------------------
      if (ns .eq. 2) then
         CON = 1.40d0
         BIG = 1.0E+30
        SAFE = 2.0
c---------------------------------
c  ns=3 for V_pseudo-gaussian(R)
c---------------------------------
      elseif (ns .eq. 3) then
         CON = 5.0d0
         BIG = 1.0E+30
        SAFE = 3.0
      endif
        CON2 = CON*CON
c----------------------------------------------------------
      if(h.eq.0.) pause 'h must be nonzero in dfridr'
        hh=h
c     a(1,1)=(func(x+hh)-func(x-hh))/(2.0*hh)
      a(1,1)=(fpot(x+hh,n,ns)-fpot(x-hh,n,ns))/(2.0*hh)
        err=BIG
c----------------------------------------------------------
c   Successive columns in the Neville tableau will go to
c smaller stepsizes and higher orders of extrapolation.
c----------------------------------------------------------
      do 12 i=2,NTAB
c---------------------------------
c Try new, smaller stepsize.
c---------------------------------
          hh=hh/CON
c         a(1,i)=(func(x+hh)-func(x-hh))/(2.0*hh)
          a(1,i)=(fpot(x+hh,n,ns)-fpot(x-hh,n,ns))/(2.0*hh)
          fac=CON2
c-------------------------------------------
c Compute extrapolations of various orders,
c requiring no new function evaluations.
c-------------------------------------------
        do 11 j=2,i
          a(j,i)=(a(j-1,i)*fac-a(j-1,i-1))/(fac-1.)
          fac=CON2*fac
          errt=max(abs(a(j,i)-a(j-1,i)),abs(a(j,i)-a(j-1,i-1)))
          if (errt.le.err) then
            err=errt
            dfridr=a(j,i)
          endif
  11    continue
          if(abs(a(i,i)-a(i-1,i-1)).ge.SAFE*err)return
  12  continue
        return
      END
C
      subroutine Vpotdata
      implicit real*8(a-h,o-z)
      common /spectra/ amu,Re,De,We,WeXe,WeYe,WeZe,WeTe,WeSe,WeRe
      common /spectrb/ Be,alphae,gamae,Der,betae,bryd  
      common /pgdata1/ bt,c0,c02,c03,c04,c05,c06,c07,c08,c09,c010
      common /pgdata2/ Re2,Re4,Re6,Re8,Re10,Re12,Re14,Re16,Re18,Re20
      common /vnumpot/ ff2, betam
c----------------------------------------------------------
        ff2 = amu*We*We
      betam = dsqrt( ff2/(2.0*De) )
         bt = 0.50d0*dsqrt(4.0d0 + ff2*Re*Re/De) - 1.0
         c0 = - 2.0d0*bt
c
       Re2 = Re*Re
       Re4 = Re2*Re2
       Re6 = Re4*Re2
       Re8 = Re6*Re2
      Re10 = Re8*Re2
      Re12 = Re10*Re2
      Re14 = Re12*Re2
      Re16 = Re14*Re2
      Re18 = Re16*Re2
      Re20 = Re18*Re2
c
      c02  = c0*c0
      c03  = c02*c0
      c04  = c03*c0
      c05  = c04*c0
      c06  = c05*c0
      c07  = c06*c0
      c08  = c07*c0
      c09  = c08*c0
      c010 = c09*c0
c
        return
      END
C
      function fmorse(n)
      implicit real*8(a-h,o-z)
      common /vnumpot/ ff2, betam
c----------------------------------------------------------
      fmorse = (-1)**n * ( 2.0**(n-1) - 1.0 ) * betam**(n-2) * ff2 
c----------------------------------------------------------
        return
      END
c
      function fryd(n,R)
      implicit real*8(a-h,o-z)
      common /spectra/ amu,Re,De,We,WeXe,WeYe,WeZe,WeTe,WeSe,WeRe
      common /spectrb/ Be,alphae,gamae,Der,betae,bryd  
      common /fms/ ms, Ny
c----------------------------------------------------------
c   The nth derivative of Rydberg potentials;  n >= 2.
c----------------------------------------------------------
      if (Ny .eq. 2) then
         a = ( We + bryd ) * dsqrt(amu/De)
      endif
      ep = dexp( -a*(R-Re) )
      Vr = - De*( 1.0 + a*(R-Re) )*ep
      fryd = (-1.0)**n * a**n * ( n*De*ep + Vr )
c----------------------------------------------------------
        return
      END
c
      function fpot(R,n,ns)
      implicit real*8(a-h,o-z)
      common /spectra/ amu,Re,De,We,WeXe,WeYe,WeZe,WeTe,WeSe,WeRe
      common /spectrb/ Be,alphae,gamae,Der,betae,bryd  
      common /pgdata1/ bt,c0,c02,c03,c04,c05,c06,c07,c08,c09,c010
      common /pgdata2/ Re2,Re4,Re6,Re8,Re10,Re12,Re14,Re16,Re18,Re20
      common /vnumpot/ ff2, betam
      common /fms/ ms, Ny
c----------------------------------------------------------
      if (ns .eq. 2) goto  100
      if (ns .eq. 3) goto  200
c----------------------------------------------------------
c   The nth derivative of Rydberg potentials;  n >= 2.
c 	  f(2) = amu*We*We ;  
c       d/(dR){ f[ V_rydberg(R) ] } = {fpot}'
c
c100    a = dsqrt(ff2/De)
c100    a = We * dsqrt(amu/De)
c----------------------------------------------------------
 100  if (Ny .eq. 2) then
         a = ( We + bryd ) * dsqrt(amu/De)
      endif
        m = n - 1
      ep = dexp( -a*(R-Re) )
      Vr = - De*( 1.0 + a*(R-Re) )*ep
      fpot = (-1.0)**m * a**m * ( m*De*ep + Vr )
        goto  800
c----------------------------------------------------------
c   The nth derivative of V(Pseudo-Gaussian; R) ;
c           2 =< n <= 11  ONLY .
c 	V(n)[p-g; R] = d/(dR){ V(n-1)[p-g; R] } = {fpot}'
c----------------------------------------------------------
 200  b2 = 1.0d0 - R*R/(Re*Re)
      g1 = De*dexp(bt*b2)
      g2 = bt*g1
c
      R2 = R*R
      R3 = R2*R
      R4 = R3*R
      R5 = R4*R
      R6 = R5*R
      R7 = R6*R
      R8 = R7*R
      R9 = R8*R
      R10= R9*R
      R11= R10*R
      R12= R11*R
c
        h1 = 0.0
        h2 = 0.0
      if (n .eq. 2) then
        h1 = c0*Re2/R3 - c0*R/Re2
        h2 = c0/R - c0*R/Re2
          goto  300     
      elseif (n .eq. 3) then
        h1 = c02/R2 - 3.0*c0*Re2/R4 - c0/Re2 - c02*R2/Re4
        h2 = -c0/R2 - c0/Re2 + c02/Re2 - c02*R2/Re4
          goto  300     
      elseif (n .eq. 4) then
        h1 = -3.0*c02*R/Re4 - c03*R3/Re6 + c03/(R*Re2)
        h1 = h1 - 5.0*c02/R3 + 12.0*c0*Re2/R5
        h2 = -3.0*c02*R/Re4 + c03*R/Re4 - c03*R3/Re6
        h2 = h2 - c02/(R*Re2) + 2.0*c0/R3
          goto  300     
      elseif (n .eq. 5) then
        h1 = -3.0*c02/Re4 + c04/Re4 - 6.0*c03/(R2*Re2) -27.0*c02/R4
        h1 = h1 - 60.0*c0*Re2/R6 - 6.0*c03*R2/Re6 - c04*R4/Re8
        h2 = -3.0*c02/Re4 + 3.0*c02/(R2*Re2) - 6.0*c0/R4 
        h2 = h2 - 6.0*c03*R2/Re6 + c04*R2/Re6 - c04*R4/Re8
          goto  300     
      elseif (n .eq. 6) then
        h1 = 39.0*c03/(R3*Re2) - 168.0*c02/R5 + 360.0*c0*Re2/R7
        h1 = h1 - 15.0*c03*R/Re6 - 10.0*c04*R3/Re8 + c05*R/Re6
        h1 = h1 - 6.0*c04/(R*Re4) - c05*R5/Re10
        h2 = - 12.0*c02/(R3*Re2) + 24.0*c0/R5 - 15.0*c03*R/Re6
        h2 = h2 + 2.0*c04*R/Re6 - 10.0*c04*R3/Re8 + 3.0*c03/(R*Re4)
        h2 = h2 + c05*R3/Re8 - c05*R5/Re10
          goto  300     
      elseif (n .eq. 7) then
        h1 = -285.0*c03/(R4*Re2) + 1200.0*c02/R6 - 2520.0*c0*Re2/R8 
        h1 = h1 - 15.0*c03/Re6 - 45.0*c04*R2/Re8 - 5.0*c05/Re6
        h1 = h1 + 45.0*c04/(R2*Re4) - 15.0*c05*R4/Re10 
        h1 = h1 + c06*R2/Re8 - c06*R6/Re12
        h2 =  60.0*c02/(R4*Re2) - 120.0*c0/R6 - 15.0*c03/Re6 
        h2 = h2 + 5.0*c04/Re6 - 45.0*c04*R2/Re8 - 15.0*c03/(R2*Re4)
        h2 = h2 + 5.0*c05*R2/Re8 - 15.0*c05*R4/Re10 + c06*R4/Re10 
        h2 = h2 - c06*R6/Re12 
          goto  300     
      elseif (n .eq. 8) then
        h1 = 2340.0*c03/(R5*Re2) - 9720.0*c02/R7 + 20160.0*c0*Re2/R9
        h1 = h1 - 105.0*c04*R/Re8 - 375.0*c04/(R3*Re4) 
        h1 = h1 - 105.0*c05*R3/Re10 - 21.0*c06*R5/Re12
        h1 = h1 - 3.0*c06*R/Re8 + 45.0*c05/(R*Re6) + c07*R3/Re10
        h1 = h1 - c07*R7/Re14
        h2 = -360.0*c02/(R5*Re2) + 720.0*c0/R7 - 105.0*c04*R/Re8
        h2 = h2 + 90.0*c03/(R3*Re4) + 15.0*c05*R/Re8 
        h2 = h2 - 105.0*c05*R3/Re10 + 9.0*c06*R3/Re10 
        h2 = h2 - 21.0*c06*R5/Re12 - 15.00*c04/(R*Re6) + c07*R5/Re12 
        h2 = h2 - c07*R7/Re14
          goto  300     
      elseif (n .eq. 9) then
        h1 = -21420.*c03/(R6*Re2) + 88200.0*c02/R8 
        h1 = h1 - 181440.0*c0*Re2/R10 - 105.0*c04/Re8 
        h1 = h1 + 3465.*c04/(R4*Re4) - 420.*c05*R2/Re10 
        h1 = h1 + 42.0*c06/Re8 - 210.0*c06*R4/Re12 
        h1 = h1 - 420.0*c05/(R2*Re6) - 28.0*c07*R6/Re14 
        h1 = h1 + c08*R4/Re12 - c08*R8/Re16
        h2 = 2520.0*c02/(R6*Re2) - 5040.0*c0/R8 - 105.0*c04/Re8
        h2 = h2 - 630.0*c03/(R4*Re4) - 420.0*c05*R2/Re10 
        h2 = h2 + 42.*c06*R2/Re10 - 210.0*c06*R4/Re12 
        h2 = h2 + 105.0*c04/(R2*Re6) + 14.0*c07*R4/Re12 
        h2 = h2 - 28.0*c07*R6/Re14 + c08*R6/Re14 - c08*R8/Re16
          goto  300     
      elseif (n .eq. 10) then
        h1 = 216720.0*c03/(R7*Re2) - 887040.0*c02/R9 
        h1 = h1 + 1814400*c0*Re2/R11
        h1 = h1 - 35280.0*c04/(R5*Re4) - 945.0*c05*R/Re10 
        h1 = h1 - 1260.0*c06*R3/Re12 + 4305.0*c05/(R3*Re6) 
        h1 = h1 - 378.0*c07*R5/Re14 + 4.0*c08*R3/Re12
        h1 = h1 - 36.0*c08*R7/Re16 - 420.0*c06/(R*Re8) 
        h1 = h1  + 42.0*c07*R/Re10 + c09*R5/Re14 - c09*R9/Re18
        h2 = -20160.0*c02/(R7*Re2) + 40320.*c0/R9 
        h2 = h2 + 5040.0*c03/(R5*Re4) - 945.0*c05*R/Re10
        h2 = h2 + 84.0*c06*R/Re10 - 1260.0*c06*R3/Re12
        h2 = h2 - 840.0*c04/(R3*Re6) + 98.0*c07*R3/Re12 
        h2 = h2 - 378.0*c07*R5/Re14 + 20.0*c08*R5/Re14
        h2 = h2 - 36.0*c08*R7/Re16 + 105.0*c05/(R*Re8)
        h2 = h2 + c09*R7/Re16 - c09*R9/Re18
          goto  300     
      elseif (n .eq. 11) then
        h1 = -2404080.0*c03/(R8*Re2) + 9797760.0*c02/R10 
        h1 = h1 - 19958400.0*c0*Re2/R12 + 393120.0*c04/(R6*Re4)
        h1 = h1 - 945.0*c05/Re10 - 4725.0*c06*R2/Re12 
        h1 = h1 - 48195.0*c05/(R4*Re6) - 3150.0*c07*R4/Re14
        h1 = h1 + 54.0*c08*R2/Re12 - 630.0*c08*R6/Re16 
        h1 = h1 - 378.0*c07/Re10 + 4725.0*c06/(R2*Re8)
        h1 = h1 + 9.0*c09*R4/Re14 - 45.0*c09*R8/Re18
        h1 = h1 + c010*R6/Re16 - c010*R10/Re20
        h2 = 181440.0*c02/(R8*Re2) - 362880.0*c0/R10 
        h2 = h2 - 45360.0*c03/(R6*Re4) - 945.0*c05/Re10
        h2 = h2 + 189.0*c06/Re10 - 4725.0*c06*R2/Re12
        h2 = h2 + 7560.0*c04/(R4*Re6) + 378.0*c07*R2/Re12
        h2 = h2 - 3150.0*c07*R4/Re14 + 198.0*c08*R4/Re14 
        h2 = h2 - 630.0*c08*R6/Re16 - 945.0*c05/(R2*Re8) 
        h2 = h2 + 27.0*c09*R6/Re16 - 45.0*c09*R8/Re18
        h2 = h2 + c010*R8/Re18 - c010*R10/Re20
      endif
c---
 300  fpot = g1*h1 + g2*h2
c---
c       goto  800
c----------------------------------------------------------
 800    return
      END
C
      subroutine  calEvj(nv,nj,Lj,nd,gg)
      implicit real*8(a-h,o-z)
      common /spectra/ amu,Re,De,We,WeXe,WeYe,WeZe,WeTe,WeSe,WeRe
      common /spectrb/ Be,alphae,gamae,Der,betae,bryd  
      common /spectr0/ w0,we0,wex,wey,wez,wet,wes,wer
      common /spectr1/ Bee,ale,gae,eta3,eta4,eta5,eta6,eta7
      common /spectr2/ Dee,bete,xsi2,xsi3,xsi4,xsi5,xsi6,xsi7
      common /Eswitch/ aye,aze,ate,ase,are
      common /Eswitc1/ abe,aae,age,ae3,ae4,ae5,ae6,ae7
      common /Eswitc2/ ade,abt,ax2,ax3,ax4,ax5,ax6,ax7
      dimension  gg(nd),nj(Lj),Ev(200),Eu(200),Ew(200),Ex(200)
      dimension  aj1(200,200),aj2(200,200),Evj(200,200)
      dimension  Euj(200,200),Ewj(200,200)
      dimension  Em(200),Eh(200),Ej(200),Er(200)
c----------------------------------------------------------
c  (nv-1) - The HIGHEST vibrational state.
c         nv = the number of vibrational states used.
c  nj - Number of rotational states in each v state.
c  nd - The size of the force constants array "gg".
c  gg - The force constants array. 
c==========================================================
       rydev = 13.60569809d0
        auev = 27.21139618d0
        aucm = 219474.6306d0
	rinert = amu*Re*Re
	Brigid = 1.0/(2.0*rinert)
c----------------------------------------------------------
c  Calculate coefficients for ro-vib constants
c----------------------------------------------------------
      call  vjcoef
c----------------------------------------------------------
c  Calculate vibrational-rotational constants for E_vj
c----------------------------------------------------------
      call  convj(gg,nd)
c----------------------------------------------------------
c  Write out vib-rot constants
c----------------------------------------------------------
        ij = 6
      do i=1,2
          write(ij,500)
        if (WeXe .ne. 0.) px = 100.0*(abs(wex) - abs(WeXe))/wex
        if (WeYe .ne. 0.) py = 100.0*(abs(wey) - abs(WeYe))/wey
        if (WeZe .ne. 0.) pz = 100.0*(abs(wez) - abs(WeZe))/wez
        if (WeTe .ne. 0.) pt = 100.0*(abs(wet) - abs(WeTe))/wet
        write(ij,510) w0,we0,We,We,wex,WeXe,px,wey,WeYe,py,wez,WeZe,
     #   pz,wet,WeTe,pt,wes,wer
        write(ij,520)
        write(ij,510) w0*aucm,we0*aucm,We*aucm,We*aucm,
     #   wex*aucm,WeXe*aucm,px,wey*aucm,WeYe*aucm,py,
     #   wez*aucm,WeZe*aucm,pz,wet*aucm,WeTe*aucm,pt,
     #   wes*aucm,wer*aucm
c
        write(ij,530) 
          if (Be .ne. 0.0)     rb = 100.0*(abs(Bee)- abs(Be))/Bee
          if (alphae .ne. 0.0) ra = 100.0*(abs(ale)- abs(alphae))/ale
          if (gamae .ne. 0.0)  rg = 100.0*(abs(gae)- abs(gamae))/gae
          if (Der .ne. 0.0)    sd = 100.0*(abs(Dee)- abs(Der))/Dee
          if (betae .ne. 0.0)  sb = 100.0*(abs(bete)-abs(betae))/bete
        write(ij,540) Bee,Be,rb,ale,alphae,ra,gae,gamae,rg,eta3,eta4,
     #   eta5,eta6,eta7,     Dee,Der,sd,bete,betae,sb,xsi2,xsi3,xsi4,
     #   xsi5,xsi6,xsi7
        write(ij,550) 
        write(ij,540) Bee*aucm,Be*aucm,rb,ale*aucm,alphae*aucm,
     #   ra,gae*aucm,gamae*aucm,rg,eta3*aucm,eta4*aucm,
     #   eta5*aucm,eta6*aucm,eta7*aucm,   
     #   Dee*aucm,Der*aucm,sd,bete*aucm,betae*aucm,sb,xsi2*aucm,
     #   xsi3*aucm,xsi4*aucm,xsi5*aucm,xsi6*aucm,xsi7*aucm  
           if (i .eq. 1) ij = 35
      enddo
c----------------------------------------------------------
c  Calculate Morse parameters :
c----------------------------------------------------------
           al0 = Re * dsqrt( 0.50*amu*We*We/De )
         wexem = al0*al0/(2.0*amu*Re*Re)
c----------------------------------------------------------
c  Check if the vibrational constants reproduce De 
c      c : calculated data;  i : input data.
c----------------------------------------------------------
c--- From quadratic form :
        DisE2c = We*We/(4.0*wex)
        DisE2i = We*We/(4.0*WeXe)
c--- From CUBIC form :
        DisE3c = 2.0*( dsqrt(wex**2 - 3.0*We*wey) )**3
        DisE3c = DisE3c - wex*(2.0*wex**2 - 9.0*We*wey)
        DisE3c = DisE3c/(27.0*wey*wey)
c
        DisE3i = 2.0*( dsqrt(WeXe**2 - 3.0*We*WeYe) )**3
        DisE3i = DisE3i - WeXe*(2.0*WeXe**2 - 9.0*We*WeYe)
        if (abs(WeYe) .gt. 0.0) then
	    DisE3i = DisE3i/(27.0*WeYe*WeYe)
        else
	    DisE3i = 0.0
        endif
          E2cp = 100.0*abs(DisE2c - De)/De 
          E2ip = 100.0*abs(DisE2i - De)/De 
          E3cp = 100.0*abs(DisE3c - De)/De 
          E3ip = 100.0*abs(DisE3i - De)/De 
        if (DisE3i .eq. 0.0)  E3ip = 0.0
c----------------------------------------------------------
c   Print and compare the calculated De to see the quality
c of the constants (We, wex, wey, ...)
c----------------------------------------------------------
        write(6,552) De, DisE2c, DisE2i, DisE3c, DisE3i,
     # E2cp, E2ip, E3cp, E3ip 
c----------------------------------------------------------
c  Calculate vib-rot energies using vib-rot constants
c----------------------------------------------------------
         nv1 = nv+1
         nv2 = nv+ 20
      do 20 i=1,nv2
           bv = 1.0d0*( i - 1 )
          bv0 = bv + 0.5d0
         bv02 =  bv0*bv0
         bv03 = bv02*bv0
         bv04 = bv03*bv0
         bv05 = bv04*bv0
         bv06 = bv05*bv0
         bv07 = bv06*bv0
          evb =  w0 + (We + we0)*bv0 - wex*bv02 + aye*wey*bv03
          evb = evb + aze*wez*bv04 + ate*wet*bv05 + ase*wes*bv06
c
c--- VIBrational energies :
        Ev(i) = evb + are*wer*bv07
c
c--- Approximate VIBrational energies :
        Eu(i) = We*bv0 -  wex*bv02
        Ex(i) = We*bv0 - WeXe*bv02
        Em(i) = We*bv0 - wexem*bv02
        Eh(i) = We*bv0 
c--- New terms of VIBrational energies :
        Ew(i) = w0 + we0*bv0
c
            nj0 = nj(i)
        if (nj(1) .gt. 0 .and. i .le. nv1) then
          do j=1,nj0
             bj = 1.0d0*( j - 1 )
            bj0 = bj*(bj + 1.0)
            bj2 = bj0*bj0
            ejb = abe*Bee*bj0 -  aae*ale*bj0*bv0 
            ejb = ejb + age* gae*bj0*bv02 
            ejb = ejb - ae3*eta3*bj0*bv03 - ae4*eta4*bj0*bv04
            ejb = ejb - ae5*eta5*bj0*bv05 - ae6*eta6*bj0*bv06
            ej1 = ejb - ae7*eta7*bj0*bv07
            ej1a= ej1 - abe*Bee*bj0
c---
c           ejb = ejb + ae3*eta3*bj0*bv03 + ae4*eta4*bj0*bv04
c           ejb = ejb + ae5*eta5*bj0*bv05 + ae6*eta6*bj0*bv06
c           ej1 = ejb + ae7*eta7*bj0*bv07
c
c--- Herzberg (1953) :
C           ejc = - ade*Dee*bj2 - abt*bete*bj2*bv0 
c--- A. L. G. Rees [ Proc. Phys. Soc. (London)59,998(1947) ] :
            ejc =   ade*Dee*bj2 - abt*bete*bj2*bv0 
c
            ejc = ejc + ax2*xsi2*bj2*bv02 
            ejc = ejc + ax3*xsi3*bj2*bv03  + ax4*xsi4*bj2*bv04
            ejc = ejc + ax5*xsi5*bj2*bv05  + ax6*xsi6*bj2*bv06
            ej2 = ejc + ax7*xsi7*bj2*bv07 
            ej2a= ej2 - ade*Dee*bj2
c
c--- VIBrational-ROTational energies :
            Evj(i,j) = Ev(i) + ej1 + ej2
c--- ROTational energies :
            if (i .eq. 1) Ej(j) = abe*Bee*bj0 + ade*Dee*bj2
c--- VIBrational-ROTational COUpling energies :
            Ewj(i,j) = ej1a + ej2a
c--- Other energy forms :
            Euj(i,j) = Ev(i) + ej1 
            aj1(i,j) = ej1 
            aj2(i,j) = ej2 
c--- Rigid rotor energies at R=Re :
            if (i .eq. 1) Er(j) = Brigid*bj*(bj+1.0)
          enddo
        endif
c
  20  continue
c----------------------------------------------------------
c  Write out vib-rot energies
c----------------------------------------------------------
        write( 6,560) 
        write(35,560) 
        write(36,560) 
            Emax = 0.0
          sumdif = 0.0
              kk = 0
      do 30 i=1,nv
          kv = i - 1
            difv = Ev(i) - Ev(i-1)
              if (i .eq. 1) difv = Ev(i)
          if ( Ev(i) .lt. Ev(i-1) .and. kk .eq. 0) then
              kk = 1
            write( 6,*)
            write(35,*)
            write(36,*)
          endif
        write(35,570) kv, Ev(i), Ev(i)*aucm, difv, difv*aucm
        write( 6,570) kv, Ev(i), Ev(i)*aucm, difv, difv*aucm
        write(36,570) kv, Ev(i)
          if ( Ev(i) .gt. Ev(i-1) )   Emax = Ev(i)
          if (difv .gt. 0.0) sumdif = sumdif + difv
  30  continue
        write( 6,575) sumdif, Emax
        write(35,575) sumdif, Emax
c
        write( 6,580)
        write(35,580)
          kk = 0
      do i=1,nv
            kv = i - 1
          difv = Ev(i) - Ev(i-1)
            if (i .eq. 1) difv = Ev(1)
          if ( Ev(i) .lt. Ev(i-1) .and. kk .eq. 0) then
              kk = 1
            write( 6,*)
            write(35,*)
          endif
        write( 6,570) kv, Ev(i), Ew(i), difv
        write(35,570) kv, Ev(i), Ew(i), difv
      enddo
c
        write( 6,585) 
        write(35,585) 
          kk = 0
      do i=1,nv
            kv = i - 1
          if ( Ev(i) .lt. Ev(i-1) .and. kk .eq. 0) then
              kk = 1
            write( 6,*)
            write(35,*)
          endif
        write(35,570) kv, Ev(i), Ew(i), Eu(i), Ex(i)
        write( 6,570) kv, Ev(i), Ew(i), Eu(i), Ex(i)
      enddo
c
        write( 6,590) wex,WeXe,wexem,wex*aucm,WeXe*aucm,wexem*aucm 
        write(35,590) wex,WeXe,wexem,wex*aucm,WeXe*aucm,wexem*aucm 
          kk = 0
      do i=1,nv2
         kv = i - 1
        ev0 = Ev(i)
        ex0 = Ex(i)
        em0 = Em(i)
        eh0 = Eh(i)
          if ( Ev(i) .lt. Ev(i-1) ) ev0 = 0.0
          if ( Ex(i) .lt. Ex(i-1) ) ex0 = 0.0
          if ( Em(i) .lt. Em(i-1) ) em0 = 0.0
c         if ( Eh(i) .lt. Eh(i-1) ) eh0 = 0.0
          if ( Ev(i) .lt. Ev(i-1) .and. kk .eq. 0) then
              kk = 1
            write( 6,*)
            write(35,*)
          endif
        write(35,570) kv, ev0, ex0, em0, eh0
        write( 6,570) kv, ev0, ex0, em0, eh0
      enddo
c
c-- bjj is classicle rotational ANGular momentum
c-- bvj is rotational ANGular velocity
c
        write( 6,710) rinert, Brigid
      do j=1,nj(1)
	     bj = j*1.0 - 1.0
	    bjj = dsqrt( bj*(bj + 1.0) )
	    bvj = bjj/rinert
        write( 6,570) j-1, Ej(j), Er(j), bjj, bvj
        write(35,570) j-1, Ej(j), Er(j), bjj, bvj
        write(36,570) j-1, Ej(j)
      enddo
c
      if (nj(1) .gt. 0) then
          write( 6,600) ade 
          write(35,600) ade
          write(36,600) ade
            sumdif = 0.0
             difj0 = Evj(1,1)
            Evjmax = 0.0
            jj = 0
        do i=1,nv
            kv = i - 1
            nj0 = nj(i)
             kk = 0
               if (Evj(i,1) .le. Emax) then
                 if (Evj(i,1) .gt. Evjmax) then
                   Evjmax = Evj(i,1)
                 endif
               endif
          do j=1,nj0
            kj = j - 1
               if (j .ge. 2) difvj = Evj(i,j) - Evj(i,j-1)
               if (j .eq. 1) difvj = difj0
             if ( Evj(i,j) .lt. Evj(i+1,1) ) then
		    sumdif = sumdif + difvj
             endif
             if ( Evj(i,j) .gt. Evj(i+1,1) .and. kk .eq. 0) then
                   difj0 = Evj(i+1,1) - Evj(i,j-1)
               if (Evj(i,j-1) .gt. Evjmax) then
                 if (Evj(i,j-1) .le. Emax)  Evjmax = Evj(i,j-1)
               endif
                 kk = 1
               write( 6,*)
               write(35,*)
               write(36,*)
             endif
           write(35,610) kv,kj,Evj(i,j),Evj(i,j)*aucm,difvj,difvj*aucm
           write( 6,610) kv,kj,Evj(i,j),Evj(i,j)*aucm,difvj,difvj*aucm
           write(36,610) kv,kj,Evj(i,j)
             if ( Evj(i,j) .eq. Emax) jj = 1
             if ( Evj(i,j) .eq. Emax) sumdif = sumdif + difvj
          enddo
            if (jj .eq. 1) then
              write( 6,*)
              write(35,*)
              write(36,*)
            else
              write( 6,602)
              write(35,602)
              write(36,602)
            endif
        enddo
          write(35,605) sumdif, Evjmax
          write( 6,605) sumdif, Evjmax
c
          write(35,630) 
          write( 6,635) 
            jj = 0
        do i=1,nv
            kv = i - 1
            nj0 = nj(i)
             kk = 0
          do j=1,nj0
              kj = j - 1
            if ( Evj(i,j) .gt. Evj(i+1,1) .and. kk .eq. 0) then
                kk = 1
              write(35,*) 
              write( 6,*)
            endif
            write(35,610) kv,kj,Evj(i,j),Ev(i),aj1(i,j)+aj2(i,j)
            write( 6,610) kv,kj,Evj(i,j),Ewj(i,j),Ev(i),Ej(j)
              if ( Evj(i,j) .eq. Emax) jj = 1
          enddo
            if (jj .eq. 1) then
              write( 6,*)
              write(35,*) 
            else
              write( 6,602)
              write(35,602)
            endif
        enddo
c
          write(35,640) 
        do i=1,nv
            kv = i - 1
            nj0 = nj(i)
          do j=1,nj0
            kj = j - 1
            write(35,610) kv,kj,Evj(i,j),Ev(i),aj1(i,j),aj2(i,j)
          enddo
            write(35,*) 
        enddo
c
          write(35,650) 
        do i=1,nv
            kv = i - 1
            nj0 = nj(i)
          do j=1,nj0
            kj = j - 1
            write(35,610) kv,kj,Evj(i,j),Euj(i,j),aj1(i,j)
          enddo
            write(35,*) 
        enddo
c
      endif
c
c----------------------------------------------------------
 500  format(///10x,'--- Ro-vibrational constants & energies --- ',/
     #/6x,'All calculated constants are the functions of the input ', 
     #/6x,'"We", "Re", & "mu",  and are based on the perturbation ',
     #/6x,'theory,  except that "We" is the input value.',
     #///8x,'** The vibrational constants (in a.u.) are : ** ',
     #/6x,'evaluated using force constants from NUMerical deriv.', 
     #//10x,'[ Error% = 100.0*(|Calc.| - |Input|)/Calc. ] ')
 510  format(/5x,'          Calculated              Input ',
     #13x,'Error% ',
     #//3x,' W0  = ', 1PE20.12,/3x,' We0 = ', 1PE20.12,
     # /3x,' We  = ', 1PE20.12,2x,1PE20.12,
     # /3x,'WeXe = ', 1PE20.12,2x,1PE20.12,2x,1PE13.5,
     # /3x,'WeYe = ', 1PE20.12,2x,1PE20.12,2x,1PE13.5,
     # /3x,'WeZe = ', 1PE20.12,2x,1PE20.12,2x,1PE13.5,
     # /3x,'WeTe = ', 1PE20.12,2x,1PE20.12,2x,1PE13.5,
     # /3x,'WeSe = ', 1PE20.12,/3x,'WeRe = ', 1PE20.12 )
 520  format(//13x,'The vibrational constants (in cm-1) are : ')
 530  format(///12x,'## The rotational constants (in a.u.) are : ## ')
 540  format(/5x,'             Calculated               Input ',
     #13x,'Error% ',
     #//3x,'     Be = ', 1PE20.12,2x,1PE20.12,2x,1PE13.5,
     # /3x,'Alpha_e = ', 1PE20.12,2x,1PE20.12,2x,1PE13.5,
     # /3x,'Gamma_e = ', 1PE20.12,2x,1PE20.12,2x,1PE13.5,
     # /3x,' Eta_e3 = ', 1PE20.12, /3x,' Eta_e4 = ', 1PE20.12,
     # /3x,' Eta_e5 = ', 1PE20.12, /3x,' Eta_e6 = ', 1PE20.12,
     # /3x,' Eta_e7 = ', 1PE20.12,
     #//3x,'    D_e = ', 1PE20.12,2x,1PE20.12,2x,1PE13.5, 
     # /3x,' Beta_e = ', 1PE20.12,2x,1PE20.12,2x,1PE13.5,
     # /3x,' Xsi_e2 = ', 1PE20.12, /3x,' Xsi_e3 = ', 1PE20.12,
     # /3x,' Xsi_e4 = ', 1PE20.12, /3x,' Xsi_e5 = ', 1PE20.12,
     # /3x,' Xsi_e6 = ', 1PE20.12, /3x,' Xsi_e7 = ', 1PE20.12 )
 550  format(//15x,'The rotational constants (in cm-1) are : ')
 552  format(///15x,'    Check VIBRATIONAL constants : ',
     #//8x,"[ As V(R)=De, De can be calculated by Rees's Eq. ",
     # /8x,"            from  VIBrational constants.         ",
     #//8x,"    Quadratic potential form :   ",
     # /8x,"  De_2c -- De from calculated vibrational constants; ",
     # /8x,"  De_2i -- De from  inputted  vibrational constants. ",
     #//8x,"      CUBIC   potential form (much accurate) :   ",
     # /8x,"  De_3c -- De from calculated vibrational constants; ",
     # /8x,"  De_3i -- De from  inputted  vibrational constants. ",
     #//8x,"  Higher_power potential form is much more accurate. ",
     # /8x,"    Set De_3i & D3i% = 0.0 if inputted WeYe is 0.0 . ",
     #//8x,"       Dei = Input 'TRUE' dissociation energy De :   ",
     #//8x,"           D2c% = 100 * | De_2c - Dei |/Dei ;   ",
     # /8x,"           D2i% = 100 * | De_2i - Dei |/Dei ;   ",
     # /8x,"           D3c% = 100 * | De_3c - Dei |/Dei ;   ",
     # /8x,"           D3i% = 100 * | De_3i - Dei |/Dei .         ]",
     #//18x,"    Dei == De = ",f12.8,'  a.u.',
     #//4x," De_2c(a.u)      De_2i(a.u)      De_3c(a.u)     ",  
     # " De_3i(a.u) ",/2x,4(1f12.8,4x),
     #//8x,"          Percent errors of generated De : ",
     #/4x,"   D2c%            D2i%             D3c%           D3i% ",           
     #/2x,4(1f12.6,4x) )
 560  format(///17x,'=== The VIBrational energies are : === ',
     #//19x,'    [  Ev_dif = E(v) - E(v-1)  ] ',
     #//5x,'v      E(v; a.u.)        E(v; cm-1)     Ev_dif(a.u)',
     #'   Ev_dif(cm-1)',/)
 570  format(3x,i3,1PE18.9,1PE18.9,x,1PE13.5,x,1PE13.5)
 575  format(/12x,'The SUM of  Ev_dif(a.u) = ',1PE16.8,
     #/12x,'Maximum energy  Ev(max) = ',1PE16.8,/)
 580  format(//5x,'v      E(v; a.u.)      Enew(v; a.u.)',
     #'    Ev_dif(a.u.)',/)
 585  format(//16x,'Comparing vibrational energies (I) : ',
     #//12x,'[ Enew(v) = w0 + we0*(v + 0.5) ; ',
     # /12x,'    Ea(v) = We*(v + 0.5) - wexe*(v + 0.5)**2 ; ',
     # /12x,'    Eb(v) = We*(v + 0.5) - WeXe*(v + 0.5)**2 ; ',
     # /12x,'     E(v) = Enew(v) + Ea(v) - ...            ; ',
     #//12x,'          wexe is calculated by code; ',
     # /12x,'          WeXe is  the  INPUT  value;          ',
     # /12x,'       w0 & we0 are calculated NEW terms.        ',
     #//12x,'     If mtyp = 1, w0 & we0 are NOT defined.     ] ',
     #//5x,'v      E(v; a.u.)     Enew(v; a.u.)     Ea(v; a.u.)',
     #'   Eb(v; a.u.)',/)
 590  format(//17x,'Comparing vibrational energies (II) : ',
     #//12x,' [  Eb(v) = We*(v + 0.5) - WeXe *(v + 0.5)**2 ; ',
     # /12x,'    Em(v) = We*(v + 0.5) - wexem*(v + 0.5)**2 ; ',
     # /12x,'    Eh(v) = We*(v + 0.5)                      . ',
     #//12x,'          WeXe   is  the   INPUT  value;        ',
     # /12x,'          wexem  is  from  Morse formulae.       ',
     #//12x,"    Eb is the approximate exp't vib. energies; ",
     # /12x,"    Em is the  Morse   vibrational   energies; ",
     # /12x,"    Eh is the   SHO    vibrational   energies.   ] ",
     #//10x,"        Calculated        Input         Morse   ",    
     # /10x,"           wex            WeXe          wexem   ",
     # /9x,"WeXe =",3(1PE14.6,x),'  a.u. ',
     # /15x,3(1PE14.6,x),'  cm-1 ',
     #//5x,'v      E(v; a.u.)      Eb(v; a.u.)      Em(v; a.u.)',
     #'   Eh(v; a.u.)',/)
 600  format(///21x,'The VIB-ROTational energies are : ',
     #//15x,'[ A. L. G. Rees, Proc. Phys. Soc.(London) ',
     # /15x,'             59, 998(1947) :              ',
     # /15x,'  E(v,j) = ..., + D_e*J*J*(J+1)**2 + ... ',
     #//15x,'       G. Herzberg, (1953) :              ',
     # /15x,'  E(v,j) = ..., - D_e*J*J*(J+1)**2 + ... ',
     #//16x,'       Evj_dif = E(v,j) - E(v,j-1)         ] ',
     #//15x,"{ ade =  1.0, use   Rees's   definition; ",
     # /15x,"      = -1.0, use Herzberg's definition.    } ",
     #//31x,"  ade =",f5.1,//
     #5x,'v  j    E(v,j; a.u.)      E(v,j; cm-1)    Evj_dif(a.u)',
     #'  Evj_dif(cm-1)',/)
 602  format(16x,'-----',13x,'-----',12x,'-----',9x,'-----',/)
 605  format(/12x,'The SUM of Evj_dif(a.u) = ',1PE16.8,
     #/12x,'Maximum energy Evj(max) = ',1PE16.8,/)
 610  format(3x,2i3,1PE18.9,1PE18.9,x,1PE13.5,x,1PE13.5)
 630  format(///13x,'Contributions to E_vj are (Part I) : ',
     #//7x,'[   E(v,j) = E(v) + Ej ;     Ej = Ej1 + Ej2  ; ',
     # /7x,'  Ej1 = E{v;j*(j*1)} ;  Ej2 = E{v;j*j*(j*1)**2}  ] ',/
     #/5x,'v  j    E(v,j; a.u.)        E(v;a.u.)       Ej(a.u.) ',/)
 635  format(///18x,'Separate contributions to E_vj  : ',
     #//12x,'[   E(v,j) = E(v-j_coupling) + E(v) + E(j) ; ',
     #//12x,'    E(v-j_coup) = Vib-Rot_coupling energies; ',
     # /12x,'           E(v) =    Vibrational   energies; ',
     # /12x,'           E(j) =    Rotational    energies.  ] ',/
     #/5x,'v  j    E(v,j; a.u.)       E(v-j_coup)     E(v; a.u.) ',
     #'    E(j; a.u.) ',/)
 640  format(///17x,'Contributions to E_vj are (Part II) : ',
     #//19x,'[  E(v,j) = E(v) + Ej1 + Ej2  ] ',/
     #/5x,'v  j    E(v,j; a.u.)        E(v;a.u.)       Ej1(a.u.) ',
     #'    Ej2(a.u.) ',/)
 650  format(///12x,'Contributions to E_vj are (Part III) : ',
     #//16x,"[   E'(v,j) = E(v) + Ej1  ] ",/
     #/5x,"v  j    E(v,j; a.u.)      E'(v,j; a.u.)     Ej1(a.u.) ",/)
 710  format(///19x,'===  Rotational energies :  === ',
     #//14x,"[ EJ(J) = Be*J(J+1) + D_e*J*J*(J+1)**2        ",
     # /14x,"  Er(J) = Brigid * J(J+1)                     ",
     # /14x,"     JJ = dsqrt( J(J+1) )                     ",
     # /14x,"   w(J) = dsqrt( J(J+1) )/Inertia             ",
     #//14x,"  EJ(J) = Non-rigid rotor  rot.  energies ;   ",
     # /14x,"  Er(J) = Rigid rotor rotational energies ;   ",
     # /14x,"     JJ = Classicle rot. angular momentum ;   ",
     # /14x,"   w(J) = Rotational angular velocity.      ] ",
     #//14x,'  Inertia =   u*Re*Re   = ',1PE16.8,
     # /14x,'   Brigid = 1/2*u*Re*Re = ',1PE16.8,
     #//5x,'J      EJ(J; a.u)        Er(J; a.u)         JJ  ',
     #'         w(J)   ',/)
c----------------------------------------------------------
 800    return
      END
C
      subroutine  vjcoef
      implicit real*8(a-h,o-z)
      common /spectra/ amu,Re,De,We,WeXe,WeYe,WeZe,WeTe,WeSe,WeRe
      common /spectrb/ Be,alphae,gamae,Der,betae,bryd  
      common /vjcoef0/ a0,a1,a2,a3,a4,a5,a6,a7,a8 
      common /vjcoef1/ b3,b4,b5,b6,b7,b8
c----------------------------------------------------------
c   Calculate the coefficients used to determine the
c vibrational-rotational constants WeXe,..., Alfa_e,...
c----------------------------------------------------------
       a = amu*We
      ar = dsqrt(a)
      a0 =   1.0/(Re*Re)   
      a1 = - 2.0/(Re*Re*Re*ar)   
      a2 =   3.0/(Re*Re*Re*Re*a)   
      a3 = - 4.0/( Re**5 * ar**3)   
      a4 =   5.0/( Re**6 *  a**2)   
      a5 = - 6.0/( Re**7 * ar**5)   
      a6 =   7.0/( Re**8 *  a**3)   
      a7 = - 8.0/( Re**9 * ar**7)   
      a8 =   9.0/(Re**10 *  a**4)
c
      b3 =   1.0/(    6.0 * ar**3)
      b4 =   1.0/(   24.0 *  a**2)
      b5 =   1.0/(  120.0 * ar**5)
      b6 =   1.0/(  720.0 *  a**3)
      b7 =   1.0/( 5040.0 * ar**7)
      b8 =   1.0/(40320.0 *  a**4)
c----------------------------------------------------------
        return
      END
C
      subroutine  convj(gg,nd)
      implicit real*8(a-h,o-z)
      common /spectra/ amu,Re,De,We,WeXe,WeYe,WeZe,WeTe,WeSe,WeRe
      common /spectrb/ Be,alphae,gamae,Der,betae,bryd  
      common /spectr0/ w0,we0,wex,wey,wez,wet,wes,wer
      common /spectr1/ Bee,ale,gae,eta3,eta4,eta5,eta6,eta7
      common /spectr2/ Dee,bete,xsi2,xsi3,xsi4,xsi5,xsi6,xsi7
      common /vjcoef0/ a0,a1,a2,a3,a4,a5,a6,a7,a8 
      common /vjcoef1/ b3,b4,b5,b6,b7,b8
      dimension  gg(nd)
c----------------------------------------------------------
      f2 = gg(2)     
      f3 = gg(3)     
      f4 = gg(4)     
      f5 = gg(5)     
      f6 = gg(6)     
      f7 = gg(7)     
      f8 = gg(8)     
c
      f32 = f3*f3
      f42 = f4*f4
      f52 = f5*f5
      f62 = f6*f6
      f72 = f7*f7
      f82 = f8*f8
c
      a12 = a1*a1
      a22 = a2*a2
      a32 = a3*a3
      a42 = a4*a4
      a52 = a5*a5
      a62 = a6*a6
      a72 = a7*a7
      a82 = a8*a8
c
      b32 = b3*b3
      b42 = b4*b4
      b52 = b5*b5
      b62 = b6*b6
      b72 = b7*b7
      b82 = b8*b8
c
      w0 = 3.0*b4*f4/8.0 + 315.0*b8*f8/128.0 - 7.0*b32*f32/(16.0*We)
      w0 = w0 - 1107.0*b52*f52/(256.0*We) - 945.*b4*b6*f4*f6/(128.*We)
      w0 = w0 - 1155.0*b3*b7*f3*f7/(128.0*We)
      w0 = w0 - 180675.0*b72*f72/(2048.0*We)
      w0 = w0 - 89775.0*b6*b8*f6*f8/(512.0*We)
c
      we0 = 25.0*b6*f6/8.0 - 67.0*b42*f42/(16.0*We)
      we0 = we0 - 95.0*b3*b5*f3*f5/(8.0*We) - 19277.*b62*f62/(256.*We)
      we0 = we0 - 22029.0*b5*b7*f5*f7/(128.0*We)
      we0 = we0 - 10521.0*b4*b8*f4*f8/(64.0*We)
      we0 = we0 - 5450499.0*b82*f82/(2048.0*We)
c
      wex = 3.0*b4*f4/2.0 + 245.0*b8*f8/16.0 - 15.0*b32*f32/(4.0*We)
      wex = wex - 1085.*b52*f52/(32.0*We) - 885.*b4*b6*f4*f6/(16.*We)
      wex = wex - 1365.0*b3*b7*f3*f7/(16.0*We)
      wex = wex - 444381.0*b72*f72/(512.0*We)
      wex = wex - 204771.0*b6*b8*f6*f8/(128.0*We)
      wex = - wex 
c
      wey = 5.0*b6*f6/2.0 - 17.0*b42*f42/(4.0*We) 
      wey = wey - 35.0*b3*b5*f3*f5/(2.*We) - 4145.0*b62*f62/(32.0*We)
      wey = wey - 5355.0*b5*b7*f5*f7/(16.0*We)
      wey = wey - 2205.0*b4*b8*f4*f8/(8.0*We)
      wey = wey - 2947595.0*b82*f82/(512.0*We)
c
      wez = 35.0*b8*f8/8.0 - 315.*b52*f52/(16.*We) 
      wez = wez - 165.*b4*b6*f4*f6/(8.*We) - 315.*b3*b7*f3*f7/(8.*We)
      wez = wez - 82005.*b72*f72/(128.*We) -33845.*b6*b8*f6*f8/(32.*We)
c
      wet = - 393.0*b62*f62/(16.0*We) - 693.0*b5*b7*f5*f7/(8.0*We)
      wet = wet - 189.*b4*b8*f4*f8/(4.*We) - 239841.*b82*f82/(128.0*We)
c
      wes = - 3003.0*b72*f72/(32.0*We) - 889.0*b6*b8*f6*f8/(8.0*We)
c
      wer = - 3985.0*b82*f82/(32.0*We)
c---
      Bee = a0 + 3.0*a4/8.0 + 315.0*a8/128.0 - 7.0*a3*b3*f3/(8.0*We)     
      Bee = Bee - 1155.0*a7*b3*f3/(128.0*We) - 3.0*a2*b4*f4/(4.0*We)
      Bee = Bee - 945.0*a6*b4*f4/(128.0*We) - 15.0*a1*b5*f5/(8.0*We)
      Bee = Bee - 1107.*a5*b5*f5/(128.0*We) - 945.*a4*b6*f6/(128.*We)
      Bee = Bee - 89775.*a8*b6*f6/(512.*We) - 1155.*a3*b7*f7/(128.*We)
      Bee = Bee - 180675.*a7*b7*f7/(1024.*We) - 315.*a2*b8*f8/(32.*We)
      Bee = Bee - 89775.0*a6*b8*f8/(512.0*We)
      Bee = Bee/(2.0*amu)
c
      ale = a2 + 25.0*a6/8.0 - 3.0*a1*b3*f3/We - 95.0*a5*b3*f3/(8.*We)
      ale = ale - 67.0*a4*b4*f4/(8.0*We) - 10521.0*a8*b4*f4/(64.0*We)
      ale = ale - 95.0*a3*b5*f5/(8.0*We) - 22029.0*a7*b5*f5/(128.*We)
      ale = ale - 75.0*a2*b6*f6/(8.0*We) - 19277.0*a6*b6*f6/(128.*We)
      ale = ale - 175.0*a1*b7*f7/(8.*We) - 22029.0*a5*b7*f7/(128.*We)
      ale = ale - 10521.0*a4*b8*f8/(64.0*We) 
      ale = ale - 5450499.0*a8*b8*f8/(1024.0*We)
      ale = - ale/(2.0*amu)
c
      gae = 3.0*a4/2.0 + 245.0*a8/16.0 - 15.0*a3*b3*f3/(2.0*We)
      gae = gae - 1365.0*a7*b3*f3/(16.0*We) - 3.0*a2*b4*f4/We
      gae = gae - 885.0*a6*b4*f4/(16.0*We) - 15.0*a1*b5*f5/(2.0*We)
      gae = gae - 1085.0*a5*b5*f5/(16.*We) - 885.*a4*b6*f6/(16.0*We)
      gae = gae - 204771.0*a8*b6*f6/(128.0*We) 
      gae = gae - 1365.*a3*b7*f7/(16.*We) - 444381.*a7*b7*f7/(256.*We) 
      gae = gae - 245.0*a2*b8*f8/(4.0*We) - 204771.*a6*b8*f8/(128.*We) 
      gae = gae/(2.0*amu)
c
c--- Bee, ale, gae are calculated by DEFINITION !
c
      et3 = 5.0*a6/2.0 - 35.0*a5*b3*f3/(2.0*We) - 17.*a4*b4*f4/(2.*We)
      et3 = et3 - 2205.0*a8*b4*f4/(8.0*We) - 35.0*a3*b5*f5/(2.0*We)
      et3 = et3 - 5355.0*a7*b5*f5/(16.0*We) - 15.*a2*b6*f6/(2.0*We)
      et3 = et3 - 4145.0*a6*b6*f6/(16.0*We) - 35.*a1*b7*f7/(2.0*We)
      et3 = et3 - 5355.0*a5*b7*f7/(16.0*We) - 2205.*a4*b8*f8/(8.*We)
      et3 = et3 - 2947595.0*a8*b8*f8/(256.0*We)
      eta3 = et3/(2.0*amu)
c
      et4 = 35.*a8/8.0 - 315.*a7*b3*f3/(8.*We) - 165.*a6*b4*f4/(8.*We)
      et4 = et4 - 315.0*a5*b5*f5/(8.0*We) - 165.0*a4*b6*f6/(8.0*We)
      et4 = et4 - 33845.0*a8*b6*f6/(32.0*We) - 315.0*a3*b7*f7/(8.0*We)
      et4 = et4 - 82005.0*a7*b7*f7/(64.0*We) - 35.0*a2*b8*f8/(2.0*We)
      et4 = et4 - 33845.0*a6*b8*f8/(32.0*We)
      eta4 = et4/(2.0*amu)
c
      et5 = - 189.0*a8*b4*f4/(4.0*We) - 693.0*a7*b5*f5/(8.0*We)
      et5 = et5 - 393.0*a6*b6*f6/(8.0*We) - 693.0*a5*b7*f7/(8.0*We)
      et5 = et5 - 189.0*a4*b8*f8/(4.0*We) - 239841.*a8*b8*f8/(64.*We)
      eta5 = et5/(2.0*amu)
c
      et6 = - 889.0*a8*b6*f6/(8.0*We) - 3003.0*a7*b7*f7/(16.0*We)
      et6 = et6 - 889.0*a6*b8*f8/(8.0*We)
      eta6 = et6/(2.0*amu)
c
      eta7 = - 3985.0*a8*b8*f8/(32.0*amu*We)
c---
      Dee = a12/(2.0*We) + 7.0*a32/(16.0*We) + 3.0*a2*a4/(4.0*We)
      Dee = Dee + 15.0*a1*a5/(8.0*We) + 1107.0*a52/(256.0*We)
      Dee = Dee + 945.0*a4*a6/(128.0*We) + 1155.0*a3*a7/(128.0*We)
      Dee = Dee + 180675.0*a72/(2048.0*We) + 315.0*a2*a8/(32.0*We)
      Dee = Dee + 89775.0*a6*a8/(512.0*We)
      Dee = Dee/(4.0*amu*amu)
c
      bet = a22/(2.0*We) + 3.0*a1*a3/We + 67.0*a42/(16.0*We)
      bet = bet + 95.0*a3*a5/(8.0*We) + 75.0*a2*a6/(8.0*We)
      bet = bet + 19277.0*a62/(256.0*We) + 175.0*a1*a7/(8.0*We)
      bet = bet + 22029.0*a5*a7/(128.0*We) + 10521.0*a4*a8/(64.0*We)
      bet = bet + 5450499.0*a82/(2048.0*We)
      bete = bet/(4.0*amu*amu)
c
      xs2 = 15.0*a32/(4.0*We) + 3.0*a2*a4/We + 15.0*a1*a5/(2.0*We)
      xs2 = xs2 + 1085.0*a52/(32.0*We) + 885.0*a4*a6/(16.0*We)
      xs2 = xs2 + 1365.0*a3*a7/(16.0*We) + 444381.0*a72/(512.0*We)
      xs2 = xs2 + 245.0*a2*a8/(4.0*We) + 204771.0*a6*a8/(128.0*We)
      xsi2 = xs2/(4.0*amu*amu)
c
      xs3 = 17.0*a42/(4.0*We) + 35.0*a3*a5/(2.*We) + 15.*a2*a6/(2.*We)
      xs3 = xs3 + 4145.0*a62/(32.0*We) + 35.0*a1*a7/(2.0*We)
      xs3 = xs3 + 5355.0*a5*a7/(16.0*We) + 2205.0*a4*a8/(8.0*We)
      xs3 = xs3 + 2947595.0*a82/(512.0*We)
      xsi3 = xs3/(4.0*amu*amu)
c
      xs4 = 315.0*a52/(16.0*We) + 165.0*a4*a6/(8.0*We)
      xs4 = xs4 + 315.0*a3*a7/(8.0*We) + 82005.0*a72/(128.0*We)
      xs4 = xs4 + 35.0*a2*a8/(2.0*We) + 33845.0*a6*a8/(32.0*We)
      xsi4 = xs4/(4.0*amu*amu)
c
      xs5 = 393.0*a62/(16.0*We) + 693.0*a5*a7/(8.0*We)
      xs5 = xs5 + 189.0*a4*a8/(4.0*We) + 239841.0*a82/(128.0*We)
      xsi5 = xs5/(4.0*amu*amu)
c
      xs6 = 3003.0*a72/(32.0*We) + 889.0*a6*a8/(8.0*We)
      xsi6 = xs6/(4.0*amu*amu)
c
      xsi7 = 3985.0*a82/(128.0*amu*amu*We)
c
c----------------------------------------------------------
        return
      END

