C     Program  vibrotE
c 
c     This program is to calculate the vibrational/ro-vibrational
c   energies, or scattering threshold energies and channel energies.
c----------------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      dimension E(200),Eryd(200),Ethre(200),Echan(200),Eev(200)
      dimension Evr(200,200),Eau(200),Evb(200),av(200)
      dimension nj(200)
      common /spectra/ amu,Re,De,Wee,We,WeXe,WeYe,WeZe,WeTe
      common /spectrb/ Be,alphae,gamae,Der,betae,bryd,dryd
      common /spectr0/ w0,we0,wex,wey,wez,wet,wes,wer,v0,r0
      common /spectr1/ Bee,ale,gae,eta3,eta4,eta5,eta6,eta7
      common /spectr2/ Dee,bete,xsi2,xsi3,xsi4,xsi5,xsi6,xsi7
      common /Eswitch/ aye,aze,ate,ase,are
      common /Eswitc1/ abe,aae,age,ae3,ae4,ae5,ae6,ae7
      common /Eswitc2/ ade,abt,ax2,ax3,ax4,ax5,ax6,ax7
      common /bdt/ beta,br4,br6,br8,br10,br12,br14
c----------------------------------------------------------------------
c
c On input:
c
c       mtyp  = 1, use input vib-rot constants to cal. Ev & Evj;
c             = 2, use numerical derivatives of V(R) to cal. Ev & Evj.
c        Mvr  = 1, for vibration; = 2 for rot-vibration; =0 for BOTH.
c         NE  = The # of input SCATTERING energies
c         Nv  = # of vibrational states considered
c                Nv=6 means that v=0,1,2,3,4,5
c        iniv = Quantum # of the initial vibrational reference state
c        jnir = Quantum # of the initial  rotational reference state
c                 (Usually take iniv=v0=0, jnir=j0=0)
c         Nw  = 0, input (WE,...,Be,...) in a.u.;
c             = 1, input (WE,...,Be,...) in cm-1, code will convert them.
c       nj(i) = # of rotational states in the i_th vibrational state.
c
c        beta = WIDTH (adjustable) parameter of the potential.
c
c       E(i)  = The energies of the scattered particle (in eV)
c   We, WeXe  = Experimental vibrational energy constants (in Hartree)
c  Be,alphae,Der= Experimental  rotational energy constants (in Hartree)
c
c   Using Sun's perturbation formulae to calculate energies Evj.
c
c----------------------------------------------------------------------
       rbohr = 0.529177249d0
       rydev = 13.60569809d0
 	  auev = 27.21139618d0
        aucm = 219474.6306d0
        amae = 1.6605655D-27/9.1093897D-31
c----------------------------------------------------------------------
c      amae=mass_unit/mass_e
C      amae=1.6605655D-27/9.1093897D-31=1822.9163
c----------------------------------------------------------------------
      read (5,*) mtyp
      read (5,*) Mvr,NE,Nv,iniv,jnir,Nw
      read (5,*) Re,De,beta
      read (5,*) ams,bms,am0
        if (am0 .eq. 0.0) am0 = ams*bms/(ams+bms)
          amu = am0 * amae
        if (Nw .gt. 0) then
          Re  = Re/rbohr
          De  = De/aucm
        endif
      read (5,*) (nj(k), k=1,Nv)
        write( 6,100) mtyp,Mvr,NE,Nv,iniv,jnir,Nw,beta,
     #Re,De,ams,bms,am0,amu
        write(35,100) mtyp,Mvr,NE,Nv,iniv,jnir,Nw,beta,
     #Re,De,ams,bms,am0,amu
        do k=1,Nv
          write( 6,105) k-1, nj(k) 
          write(35,105) k-1, nj(k) 
        enddo
c----------------------------------------------------------------------
          nj1 = nj(1)
        nj(1) = 1
c----------------------------------------------------------------------
        v0=DFLOAT(iniv)
        r0=DFLOAT(jnir)
      if (NE .gt. 0) then
        read (5,*) (E(i), i=1,NE)
          write(6,108) 
        do i=1,NE
          write(6,110) i, E(i) 
        enddo 
      endif
c----------------------------------------------------------------------
      read (5,*) We,WeXe,WeYe
      read (5,*) WeZe,WeTe,WeSe
      read (5,*) Be,alphae,gamae
      read (5,*) Der,betae
        write(6,115) We,WeXe,WeYe,WeZe,WeTe,WeSe,Be,alphae,gamae,
     #Der,betae
c-
      read(5,*) aye,aze,ate,ase,are
      read(5,*) abe,aae,age,ae3,ae4,ae5,ae6,ae7
      read(5,*) ade,abt,ax2,ax3,ax4,ax5,ax6,ax7
c--- 
      if (Nw .gt. 0) then
         We  = We/aucm
        WeXe = WeXe/aucm
        WeYe = WeYe/aucm
        WeZe = WeZe/aucm
        WeTe = WeTe/aucm
        WeSe = WeSe/aucm
          Be =   Be/aucm
       alphae= alphae/aucm
       gamae = gamae/aucm
         Der =  Der/aucm
       betae = betae/aucm
      endif
         Wee = We
c----------------------------------------------------------------------
        if (mtyp .eq. 2) goto 10
c----------------------------------------------------------------------
          w0 = 0.0
         we0 = 0.0
         wex = WeXe
         wey = WeYe
         wez = WeZe
         wet = WeTe
         wes = WeSe
         wer = 0.0
         Bee = Be
         ale = alphae
         gae = gamae
         Dee = Der
        bete = betae
c----------------------------------------------------------------------
c  Cal. vibrational-rotational REFERENCE energies Eref for mtyp=1
c----------------------------------------------------------------------
        call  calEvj(1,nj,200,Ethre,Evr)
          Erefv = Ethre(1)
          Erefr = Evr(1,1)
c----------------------------------------------------------------------
c  Calculate vib.-rotational energies Ev==Evb; Evj=Evr for mtyp=1
c----------------------------------------------------------------------
          nj(1) = nj1
        call  calEvj(Nv,nj,200,Evb,Evr)
          goto 15
c======================================================================
c  Cal. force constants and vib. constants used in Ev/Evj for mtyp=2
c----------------------------------------------------------------------
   10   call  VRener(Nv,nj,200)
c----------------------------------------------------------------------
c  Cal. vibrational-rotational REFERENCE energies  Eref for mtyp=2
c----------------------------------------------------------------------
        call  calEvj(1,nj,200,Ethre,Evr)
          Erefv = Ethre(1)
          Erefr = Evr(1,1)
c----------------------------------------------------------------------
c  Calculate vib.-rotational energies Ev==Evb; Evj=Evr  for mtyp=2
c----------------------------------------------------------------------
          nj(1) = nj1
        call  calEvj(Nv,nj,200,Evb,Evr)
c======================================================================
   15   if (Mvr .eq. 2) go to 50 
c----------------------------------------------------------------------
c  Calculate vibrational THREshold energies  Ethre.
c----------------------------------------------------------------------
          write(6,120) 
      do k=1,Nv 
	  kv=k-1   
	  av(k)=DFLOAT(kv) 
	  avn=av(k)
c--- 
c Ethre(k) are in Hartree
c        Ethre(k)=Wee*(avn-v0) - WeXe*(avn*(avn+1)-v0*(v0+1))    
c--- 
         Ethre(k)=Evb(k) - Erefv
c Eev(k) are in eV
	     Eev(k)=Ethre(k)*auev
c Convert energy from eV to Rydberg
          Eryd(k)=Eev(k)/rydev
c--- 
          dife=Eev(k) - Eev(k-1)
        write(6,130) kv,Eryd(k),Ethre(k),Eev(k),dife
      enddo
c
c----------------------------------------------------------------------
c  Calculate scattering (vibrational) CHANNEL energies  Echan.
c----------------------------------------------------------------------
      DO i=1,NE
c Convert energy from eV to Hartree (a.u.)
          Eau(i)=E(i)/auev
          write(6,140) Eau(i),E(i)
	  do k=1,Nv
	    Echan(k)=Eau(i) - Ethre(k)
            Eev(k)=Echan(k)*auev
          write(6,130) k-1,Echan(k),Eev(k)
        enddo
      ENDDO  
          write(6,*)
c----------------------------------------------------------------------
        if (Mvr .eq. 1) go to 900
C======================================================================
c  Calculate vibrational-rotational THREshold energies  Evr.
c----------------------------------------------------------------------
   50     write(6,150)
C======================================================================
           Evr0 = Erefr
      if (nj(1) .gt. 0) then
        do 65 k=1,Nv
             kv = k - 1
            njr = nj(k)
               Devr = 0.0
          do 60 i=1,njr
              ir = i - 1
             Evr(k,i) = Evr(k,i) - Evr0
           if (i .gt. 1) Devr = Evr(k,i) - Evr(k,i-1)
              Evrev = Evr(k,i)*auev
               Devr = Devr*auev
             write(6,160) kv,ir,Evr(k,i),Evrev,Devr
  60      continue
 	       write(6,*)
  65    continue
      endif
C----------------------------------------------------------------------
c  Calculate vibrational-rotational CHANNEL energies  Echan.
c----------------------------------------------------------------------
      IF (NE .gt. 0) THEN
        do 80 i=1,NE
	    Eau(i)=E(i)/auev
	    write(6,170) Eau(i),E(i)
	  do 75 k=1,Nv
	       kv = k-1
            njr = nj(k)
          do 70 j=1,njr
	         jr = j-1
	        Echan(k)=Eau(i) - Evr(k,j)
	          Eev(k)=Echan(k)*auev
            write(6,160) kv,jr,Echan(k),Eev(k)
  70      continue
            write(6,*) 
  75      continue
  80    continue
      ENDIF
C---
        write(6,200) 
          kk = 0
      do i=1,Nv
	    if ( Evb(i) .lt. Evb(i-1) .and. kk .eq. 0 ) then
            kk = 1
            write(6,*)
          endif
        write(6,210) i-1, Evb(i)
      enddo
C-
        write(6,220) 
          kk = 0
      do i=1,Nv
	    if ( Evb(i) .lt. Evb(i-1) .and. kk .eq. 0 ) then
            kk = 1
            write(6,*)
          endif
        write(6,230) i-1, Evb(i)*aucm
      enddo
C-
        write(6,240) 
      do i=1,Nv
         dif = Evb(i) - Evb(i-1)
           if (i .eq. 1) dif = 0.0
        write(6,230) i-1, dif*aucm
      enddo
C----------------------------------------------------------------------
  100 format(///16x,'**=  Output of "vibrotE.f"  =** ',/
     #/9x,'Switches to calculate VIB-ROTational energies :',/
     #/6x,'[ mtyp = 1, use input vib-rot consts to cal. Ev & Evj;',
     #/6x,'       = 2, use num. derivatives of V(R) for Ev & Evj.',/
     #/6x,'            ! Run both mtyp=1, 2 to compare Ev(max) ! ',/
     #/6x,'   Mvr = 1, for vib.; = 2 for rot-vib.; = 0, for BOTH.',
     #/6x,'    NE = The number of input SCATTERING energies.',
     #/6x,'    Nv = The number of vibrational states considered.',
     #/6x,'              Nv=6 means that v=0,1,2,3,4,5 ',
     #/6x,'  iniv = Quantum no. of vibrational reference state.',
     #/6x,'  jnir = Quantum no. of  rotational reference state.',
     #/6x,'    Nw = 0, input (WE,...,Be,...) in a.u.; ',
     #/6x,'       = 1, input in cm-1, code will convert them.     ',
     #/6x,'  beta = WIDTH parameter of a Rydberg-like potential. ]',
     #//10x,'mtyp  Mvr  NE  Nv  iniv  jnir  Nw   beta ',
     # /10x,i3,3x,i2,3x,i2,2x,i2,2x,i3,3x,i3,3x,i2,1x,f10.7//
     #/6x,'{   Re = Equilibrium internuclear distance in ao. ',
     #/6x,'    De = The dissociation energy of AB in a.u.    ',
     #/6x,'  Amas = The MASS of atom A in amu.               ',
     #/6x,'  Bmas = The MASS of atom B in amu.               } ',
     #//9x,'                                    Reduced_mass_of_AB ',
     # /3x,'Re(ao)',3x,'De(a.u)     Amas       Bmas',
     #'     (in amu)    (in a.u) ',
     # /1f9.5,1f10.6,1x,1f10.6,1x,1f10.6,1x,1f10.6,1f14.6,
     #//3x,'   nj = is no. of rotational states in each vib. state :',
     #//23x,'  v      nj  ',/)
  105 format(23x,i3,4x,i4)
  108 format(//15x,'The input SCATTERING energies : ',
     #//23x,'  i      E(i;eV) ',/)
  110 format(23x,i3,3x,f10.5)
  115 format(//2x,'   The input ro-vibrational constants are : '//,
     #7x,'  We ',11x,'WeXe',11x,' WeYe',/3(1PE16.8),//
     #7x,'WeZe ',11x,'WeTe',11x,' WeSe',/3(1PE16.8),//
     #7x,'  Be ',10x,'Alphae',10x,'Gammae',/3(1PE16.8),//
     #7x,' Der ',11x,'betae',/2(1PE16.8),/)
  120 format(//15x,'** Vibrational threshold & channel energies **'//,
     #18x,'The vibrational threshold energies are : ',//,
     #3x,'v     Ethre(Rydberg)    Ethre(a.u.)      Ethre(eV) ',
     #'   Dif(v-1,v; eV)',/)
  130 format(x,i3,4f16.7)
  140 format(///1x,'For scattering energy =',f12.6,'  au  =',f12.6,
     #' eV',//' The channel energies (Ev = Kv**2 = 2*E) are : ',
     #//3x,'v      Echan(a.u.)      Echan(eV)  ',/)
C
  150 format(/6x,' * Ro-vibrational threshold & channel energies *'//,
     #9x,' The ro-vibrational threshold energies are : ',//,
     #3x,'      v   j   Ethre(a.u.)   Ethre(eV)   Delta_Evj(eV) ',/)
  160 format(7x,i3,i4,3f13.7)
  170 format(///2x,' For scattering energy =',f12.6,'  au  =',f12.6,
     #' eV',//,6x,' The channel energies (Evj = Kvj**2 = 2*E) are :',
     #//3x,'      v   j   Echan(a.u.)   Echan(eV)  ',/)
  200 format(//2x,'* Vibrational energies *'//,
     #3x,'  v      Evib(a.u.) ',/)
  210 format(3x,i3,1PE18.8)
  220 format(//2x,'* Vibrational energies *'//,
     #3x,'  v       Evib(cm-1) ',/)
  230 format(3x,i3,F18.6,F16.6)
  240 format(//3x,'* Vibrational energies *'//,
     #3x,'  v    Delta_Evib(cm-1) ',/)
C 240 format(//12x,'* Vibrational energies *'//,
C    #3x,'  v       Evib(cm-1)    Delta_Evib(cm-1) ',/)
C----------------------------------------------------------------------
  900   stop
      end
C
C==========
      subroutine  VRener(nv,nj,Lj)
c-----------------------------------------------------------------
      implicit real*8 (a-h,o-z)
c     parameter  (Na=2500, Nd=300)
      parameter  (Nd=300)
      dimension  gg1(Nd),ge1(Nd),gg2(Nd),ge2(Nd)
      dimension   gg(Nd),ge(Nd), ggf(Nd), nj(Lj)
      common /Eswitch/ aye,aze,ate,ase,are
      common /Eswitc1/ abe,aae,age,ae3,ae4,ae5,ae6,ae7
      common /Eswitc2/ ade,abt,ax2,ax3,ax4,ax5,ax6,ax7
      common /spectra/ amu,Re,De,Wee,We,WeXe,WeYe,WeZe,WeTe
      common /spectrb/ Be,alphae,gamae,Der,betae,bryd,dryd
      common /fms/ ms, Mryd, Mwe, Ny, nw0, nvd
      common /bdt/ beta,br4,br6,br8,br10,br12,br14
c-----------------------------------------------------------------
c    Read data :
c  Ner = number of force constants f's wanted.
c
c     Use following V(R) to calculate n_th force constants f_n's.
c      |
c      | = 1, use Vmorse as V(R).
c      |
c      | = 2, use Vryd(R)=-De*ba*[1/ba + a*x ]*exp(-a*ba*x)      1.)
c      |               a = b * dsqrt( amu/De ) ;   x = R - Re.
c      |             b=We + bryd ;  originally, b=We.
c      |             ba == beta is the potential WIDTH parameter.
c      |------------------
c      | The GENERALIZED Rydberg potentials are defined as :
c      |
c      |      Vryd(R)=-De*ba*[1/ba +a*x +d*a*x*x]*exp(-a*ba*x)   2.)
c      |
c      |      Uryd(R)=-De*ba*[1/ba+a**d*M_SUM_k=1 x**k]*exp(-a*ba*x)  3.)
c      |
c Ny = |      Uryd(R)=-De*ba*[1/ba+M_SUM_k=1 a**k*x**k]*exp(-a*ba*x)  4.)
c      |       |
c      |       | M = 2,           use Eq 2.);
c      | = 3 ->| M > 2, d > 0.0,  use Eq 3.);
c      |       | M > 2, d = 0.0,  use Eq 4.).
c      |       |
c      |------------------
c      | = 4, use
c      |    Vryd(R)=-De*ba*[1/ba+a*M_SUM_k=1 x**k]*exp(-a*ba*dryd*x)  5.)
c      |
c      |------------------
c      | = 5, use
c      |      func(R)=[exp(-a*x)/beta - (a+bryd)*x]*exp(-a*x)
c      |      Vryd(R)= De*beta*func(R)                                6.)
c      |
c      | = 6, use Vpseudo_gaussian  as V(R).
c
c bryd - The variational constant used to adjust Vryd(R).
c dryd = d = k or a positive REAL number.
c Mryd = M = number of terms in summation.
c              As dryd=0.0,  code use  a**k  but NOT a**d;
c              As dryd=1, Mryd=1, you get ORIginal Vrydberg(R).
c      bryd, dryd, Mryd are meaningless for Vmorse & Vp_g.
c
c  hs -- Estimated initial stepsize used by code "dfridr".
c            Good range for  hs :  0.001 --> 4.0  for H_2.
c              hs1 for Vrydberg;   hs2 for V_p-g .
c
c   Mwe = 1, New  We = We + bryd ;  = 0, We = We.
c  Nryd = Number of f_n's calculated using Vryd(R) and
c           is used to cal. V(R->Rmax=inf).
c  Rmax = The maximum R value for  V(R->Rmax=inf).
c Rless = The R value in    Rbig = Rmax +/- Rless.
c Dconv = The tolerance used to check if V(Rmax) = De.
c
c   nw0 = 1, E(v) = w0 + (We + We0)*(v+0.5) + WeXe*(v+0.5)**2 + ...
c       = 0, E(v) = We*(v+0.5) + WeXe*(v+0.5)**2 + ...
c   nvd = number of vib. states used in comparison with
c           the input Ev or energy differencies. nvd =< n_expt.
c           nvd is slightly smaller than n_expt which is the
c           no. of input Ev OR energy differencies for comparison.
c
c br4,br6,br8,br10,br12,br14 = The scaling switches for the
c long range force const f_n(long;R=Re) = d~n/(dr~n)[1/R**k]
c which will be added into f_n(R=Re) = V_rydberg~n(R=Re).
c   f_n(long;R=Re) = (-1.0)**n *(k+n-1)!/[(k-1)!*R**(k+n)]
c which are important for Wande-Waals molecules and
c quasi-stable molecules.
c    brn = 1.0, Add f_n(long;R=Re); = 1.0, do NOT add.
c
c=========================================================================
             read(5,*) Ner,Ny,bryd,dryd,Mryd,hs1,hs2
             read(5,*) Mwe,Nryd,Rmax,Rless,Dconv
             read(5,*) nw0,nvd
             read(5,*) br4,br6,br8,br10,br12,br14
               ij = 6
	     do i=1,2
             write(ij,800) Mwe,Ner,Ny,bryd,dryd,Mryd,hs1,hs2
             write(ij,804) nw0,nvd
             write(ij,820) aye,aze,ate,ase,are,abe,aae,age,ae3,ae4,
     # ae5,ae6,ae7,ade,abt,ax2,ax3,ax4,ax5,ax6,ax7,
     # br4,br6,br8,br10,br12,br14
               if (i .eq. 1) ij = 35
	     enddo
c-------------------------------------------------------- 
c  Prepare CONVERSION factors :
c-------------------------------------------------------- 
         aotoA0 = 0.5291772490
         aJtoau = 0.229371d0
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
c in R0 over which function fpot changes substantially.
c   err -- An estimate of the error in the derivative. 
c-----
c  Calculate numerical "force constants" for :
c    V_morse(R),  as  ks=1;     V_rydberg,  as  ks=2;
c    V_pg(R),     as  ks=3.
c-------------------------------------------------------- 
           do ks=1,3
             do k=2,Nryd+1
               if (ks .eq. 1) then
		       gg1(k) = fmorse(k)
                   ge1(k) = 0.0
                     if (Ny .eq. 1) then
                       gg(k) = gg1(k)
                       ge(k) = ge1(k)
                     endif
               elseif (ks .eq. 2) then
		     ggf(k) = fryd(k,Re)
                   if (Ny .ge. 2 .and. Ny .le. 8) then
                     gg(k) = ggf(k)
                     ge(k) = 0.0
                   endif
               elseif (ks .eq. 3) then
		     gg2(k) = dfridr(k,ks,Re,hs2,err)
                 ge2(k) = err
                   if (Ny .eq. 9) then
                     gg(k) = gg2(k)
                     ge(k) = ge2(k)
                   endif
               endif
             enddo
c
           enddo
c--------------------------------------------------- 
                 alpha0 = We*dsqrt( amu/De )
                  beta0 = We*dsqrt( amu/(2.0*De) )
               write( 6,534) De, Re, alpha0, beta0
               write(35,534) De, Re, alpha0, beta0
             do k=2,Ner+1
               write( 6,550) k, gg(k), ge(k)
               write(35,550) k, gg(k), ge(k)
             enddo
c---
               write(6,536)
             do k=2,Ner+1
C               ggk =  gg(k)/(aJtoau * aotoA0**k)
                ery = ggf(k)/(aJtoau * aotoA0**k)
               write(6,550) k, ggf(k), ery
C              write(6,550) k, ggk, ery
             enddo
c--------------------------------------------------- 
c Print numerical "force constants"
c--------------------------------------------------- 
             write( 6,750) 
             write(35,750) 
           do k=2,Nryd
               write( 6,550) k, gg1(k), gg(k),  gg2(k)
               write(35,550) k, gg1(k), gg(k),  gg2(k)
             if (k .eq. Ner+1) then
               write( 6,*)
               write(35,*)
             endif
           enddo
             write( 6,760) 
             write(35,760) 
           do k=2,Ner+1
             write( 6,550) k, ge1(k), ge(k), ge2(k)
             write(35,550) k, ge1(k), ge(k), ge2(k)
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
            vmg0 = vmsg
            vmsg = vmsg + gg1(k)*(Rg - Re)**k/bk
            vmsl = vmsl + gg1(k)*(Rl - Re)**k/bk
c
            vdg0 = vdeg
            vdeg = vdeg + ggf(k)*(Rg - Re)**k/bk
            vdel = vdel + ggf(k)*(Rl - Re)**k/bk
       write( 6,740) k, vmsg, vmsl, vdeg, vdel, vdeg-vdel
       write(35,740) k, vmsg, vmsl, vdeg, vdel, vdeg-vdel
            if ( abs(vdeg - vdel) .lt. Dconv .and. kk .eq. 0 ) then
              write( 6,732)
              write(35,732)
                kk = 1
                kc = k
               vde = vdeg
            endif
            if ( abs(vdeg - vdg0) .lt. Dconv .and. kk .eq. 0 ) then
              write( 6,732)
              write(35,732)
                kk = 1
                kc = k
               vde = vdeg
            endif
            if ( abs(vmsg - vmsl) .lt. Dconv .and. kj .eq. 0 ) then
              write( 6,734)
              write(35,734)
                kj = 1
                km = k
               vdm = vmsg
            endif
            if ( abs(vmsg - vmg0) .lt. Dconv .and. kj .eq. 0 ) then
              write( 6,734)
              write(35,734)
                kj = 1
                km = k
               vdm = vmsg
            endif
          enddo
            write( 6,742) kc, vde, km, vdm
            write(35,742) kc, vde, km, vdm
C=================================================
           IF (Ner .gt. 0) then
c-------------------------------------------------
c  Define NEW vibrational constant  We if wanted
c-------------------------------------------------
             if (Mwe .gt. 0) then
                 We = We + bryd
               write( 6,745) Wee, We
             endif
c-------------------------------------------------
c  Calculate coefficients for ro-vib constants
c-------------------------------------------------
             call  vjcoef
c-------------------------------------------------
c  Calculate vib.-rotational constants for E_vj
c-------------------------------------------------
             call  convj(gg,Nd)
	     ENDIF
C====================================================================
  534 format(//3x,'The dissociation energy    De  = ',1PE16.8,
     #//,'   Equili. intern. distance   Re  = ',1PE16.8,
     #//,'   Rydberg exponental para. alpha = ',1PE16.8,
     # /,'     alpha = We * dsqrt( amu/De ) ', 
     #//,'    Morse  exponental para. beta0 = ',1PE16.8,
     # /,'     beta0 = We * dsqrt( amu/(2*De) ) ',/
     #//,'   f_(n) are from ANAlytical derivative code. ',
C    #//,'   f_(n) are from numerical derivative code. ',
     #//9x,'  err are the errors of f_(n).',
     #///8x,'   The nth force constants are : ',
     #//5x,"  n   f_(n; Har/ao**n)     err(f) "/)
  536 format(///4x,'The nth force constants in other units are : ',
C    #//3x,' e_(n) are the force constatns from analytical ',
     #//3x,'    The force constatns are from analytical ',
     # /3x,'       derivatives of Rydberg potential. ',
     #//3x,'      1 aJ = 1 attojoule = 0.229371 Har. ',
     # /3x,'             1 ao = 0.529177249 A ',
     #//5x,"  n   f_(n; Har/ao**n)  f_(n; aJ/A**n) "/)
C    #//5x,"  n    f_(n; aJ/A**n)   e_(n; aJ/A**n) "/)
c    #//5x,"  n    f_(n; aJ/A**n)      err(f) "/)
  550 format(5x,i3,2x,3(1PE16.8,x) )
  560 format(1PE24.16)
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
     # /10x," Nryd - No of f_n's used to cal. V(Rmax) ",
     #//8x,'De',11x,'Rmax',10x,'Rless',11x,'Dconv',8x,'Nryd',
     #/1x,4(1pe14.6,x),3x,i3,
     #///'  n',5x,'De_g(m)',7x,'De_l(m)',7x,'De_g(r)',
     #7x,'De_l(r)',4x,'{De_g-De_l}_r ',/)
  732 format(36x,'V_rydberg(R) converged ',/)
  734 format( 9x,'V_morse(R) converged ',/)
  740 format(i3,x,5(1pe14.6) )
  742 format(//17x,'V(R->inf=Rmax) = De  is reached with : ',
     #//23x,'  n',5x,'De(Rydbg; a.u)',/23x,i3,3x,1pe16.8,
     #//23x,'  n',5x,'De(Morse; a.u)',/23x,i3,3x,1pe16.8,/)
  745 format(//22x,'Input We         Corrected We',
     #/22x,8(1H-),9x,12(1H-),/17x,1PE16.8,3x,1PE16.8)
  750 format(///3x,'Force constants ( h ) are from numerical ',
     #'derivative code  "dfridr" ;',
     #/3x,'Force constants ( p & f ) are from ANAlytical formulae :',
     #/3x,'         For  V_p-g(R),  2 =< n =< 11  ONLY  . ',
     #//5x,"From    V_morse(R)      V_rydberg(R)       V_p-g(R)  ",
     # /5x,"  n   p(n; Har/ao**n)  f(n; Har/ao**n)  h(n; Har/ao**n) "/)
  760 format(//12x,'The ERRORS for these force constants are : ',
     #//5x,"  n     e( V_morse )    e( V_rydberg )     e( V_p-g ) "/)
  800 format(///13x,'=#=  OUTPUT from "VRener.f"  =#= ',/
     #/6x,'The switches for NUMerical derivatives & E_vj : ',/
     #/2x,"[   Mwe = 1, New  We = We + bryd ;  = 0, We = We . ",
     #/6x,"Ner = No of f_n's used to cal. ro-vib constants. ",/
     #/12x,"Use following V(R) to cal. n_th force constants f_n's.",
     #/8x,'|',/8x,'| = 1, use Vmorse as V(R).',
     #/8x,'|',/8x,'| = 2, use Vryd(R) = -Db *[1/ba + a*x ]*exp(-a1*x)',
     #7x,'1.)',/8x,'|',16x,"ba==beta;  Db==De*ba;  a1==a(1)*ba ",
     #/8x,'|',16x,
     #'a = b * dsqrt( amu/De ) ;   x = R - Re.',/8x,'|',16x,
     #'b = We + bryd ;  originally,  b = We.',/8x,'|',18(1H-),
     #/8x,'| The GENERALIZED Rydberg potentials are defined as : ',
     #/8x,'|',/8x,'|',6x,
     #'Vryd(R) = -Db *[1/ba + a*x + d*a*x*x ] *exp(-a1*x)  2.) ',
     #/8x,'|',/8x,'|',6x,
     #'Uryd(R)=-Db*[1/ba+a**d * M_SUM_k=1 x**k]*exp(-a1*x) 3.) ',
     #/8x,'|',/3x,'Ny = |',6x,
     #'Uryd(R)=-Db*[1/ba+M_SUM_k=1 a**k * x**k]*exp(-a1*x) 4.) ',
     #/8x,'|',/8x,'|',7x,'|',/8x,'|',7x,'| M = 2,',11x,'use Eq 2.);',
     #/8x,'| = 3 ->| M > 2, d > 0.0,  use Eq 3.); ',
     #/8x,'|       | M > 2, d = 0.0,  use Eq 4.). ',
     #/8x,'|       |',/8x,'|',18(1H-),/8x,'| = 4, use',/8x,'|',6x,
     #'Vryd(R)=-Db*[1/ba + a* M_SUM_k=1 x**k]*exp(-a1*dryd*x) 5.) ',
     #/8x,'|       |',/8x,'|',18(1H-),/8x,'| = 5, use',/8x,'|',6x,
     #'Vryd(R)= Db*[exp(-a*x)/ba - (a+bryd)*x]*exp(-a*x)      6.) ',
     #/8x,'|',/8x,'| = 6, use Vpseudo_gaussian  as V(R). ',/
     #/4x," bryd - The variational constant used to adjust Vryd(R), ",
     #/4x," dryd = d = integer k OR a positive REAL number , ",
     #/6x,"        As dryd=0.0,  code uses  a**k  but NOT  a**d ",
     #/4x," Mryd = M = number of terms in summation of Uryd. ",
     #/6x,"    As Mryd=1, dryd=1 you get ORIginal Vrydberg(R);",
     #/6x,"    As Mryd=2, & k=2, coeffi. of x**2 is  dryd*a  .",/
     #/6x,"    bryd, dryd, Mryd are meaningless for Ny = 1 & 6 .   ]",
     #//10x,'Mwe   Ner  Ny         bryd         dryd    Mryd ',/
     #10x,i2,4x,i2,3x,i2,2x,f16.12,2x,f8.5,2x,i3//
     #/3x,'[ hs1, hs2 --, Estimated initial stepsize used by the ',
     #/3x,'                 numerical derivative code "dfirdr" ; ',
     #/3x,'                 hs1 for Vrydberg,   hs2 for Vp_g  .   ]',
     #//16x,'hs1 =',f8.4,4x,'hs2 =',f8.4,//)
  804 format(/3x,"[ nw0 = 1, E(v)=Enew(v)+We*(v+0.5)",
     #"+WeXe*(v+0.5)**2+...",/
     #/3x,"      = 0, E(v)=We*(v+0.5)+WeXe*(v+0.5)**2+...        ",/
     #/3x,"  nvd = # of vib. states used in comparison with the ",
     #/3x,"        input Ev or energy differencies. nvd =< n_expt ",
     #/3x,"        which is the # of input (experimental) Ev OR ",
     #/3x,"        energy differencies for comparison.            ] ",
     #//20x,"nw0   nvd ",
     #/20x,i2,5x,i2/)
  820 format(//3x,'Switches for scale SOME calc. VIB-ROT constants :',
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
     #1x,f4.1,1x,f4.1,///
     #/5x,"br4,br6,br8,br10,br12,br14 = Scaling switches for long ",
     #/5x,"range corce const f_n(long;R=Re) = d~n/(dr~n)[1/R**k]  ",
     #/5x,"which will be added into f_n(R=Re) = V_rydberg~n(R=Re) ",
     #/5x,"and are important for Wande-Waals molecules and some ",
     #/5x,"quasi-stable molecular electronic states . ",/
     #/5x,"f_n(long;R=Re) = (-1.0)**n *(k+n-1)!/[(k-1)!*R**(k+n)] ",
     #/8x,"brn = 1.0, Add f_n(long;R=Re); = 0.0, do NOT add. ",
     #//4x,"           br4   br6   br8  br10  br12  br14 ",
     #/14x,f4.1,2x,f4.1,2x,f4.1,2x,f4.1,2x,f4.1,2x,f4.1,/)
c
  900   return
      end
C===
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
c     dimension  a(NTAB,NTAB)
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
      common /spectra/ amu,Re,De,Wee,We,WeXe,WeYe,WeZe,WeTe
      common /spectrb/ Be,alphae,gamae,Der,betae,bryd,dryd
      common /pgdata1/ bt,c0,c02,c03,c04,c05,c06,c07,c08,c09,c010
      common /pgdata2/ Re2,Re4,Re6,Re8,Re10,Re12,Re14,Re16,Re18,Re20
      common /vnumpot/ ff2, betap
c----------------------------------------------------------
       ff2 = amu*We*We
      betap= dsqrt( ff2/(2.0*De) )
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
      common /vnumpot/ ff2, betap
c----------------------------------------------------------
      fmorse = (-1)**n * ( 2.0**(n-1) - 1.0 ) * betap**(n-2) * ff2 
c----------------------------------------------------------
        return
      END
c
      function fryd(n,R)
      implicit real*8(a-h,o-z)
      common /spectra/ amu,Re,De,Wee,We,WeXe,WeYe,WeZe,WeTe
      common /spectrb/ Be,alphae,gamae,Der,betae,bryd,dryd
      common /fms/ ms, Mryd, Mwe, Ny, nw0, nvd
      common /bdt/ beta,br4,br6,br8,br10,br12,br14
c----------------------------------------------------------
c   The nth derivative of Rydberg potentials;  n >= 2. 
c----------------------------------------------------------
         a = ( We + bryd ) * dsqrt(amu/De)
        ep = dexp( -a*beta*(R-Re) )
      if (Ny .eq. 2) then
        Vr = - De*beta*( 1.0/beta + a*(R-Re) )*ep
        fryd = (-1.0)**n * (a*beta)**n * ( n*De*ep + Vr )
          goto 50
c----------------------------------------------------------
c   The nth derivative of the GENERALIZED Rydberg 
c potentials.
c----------------------------------------------------------
      elseif (Ny .eq. 3 .or. Ny .eq. 4) then
          fnx = 0.0
            x = R - Re
          bnn = facx(n)
            if (Ny .eq. 3) then
              adr = a**dryd  
                if (Mryd .eq. 2) adr = a
            elseif (Ny .eq. 4) then
               ep = dexp( -a*dryd*beta*x )
              adr = a
            endif
              ane = (1.0/beta)*ep*(-1.0*a*beta)**n 
        do 40 k=1,Mryd
            if (Ny .eq. 3 .and. dryd .eq. 0.0) adr = a**k
               bk = facx(k)
              fn0 = 0.0
          do 30 i=0,n
            if (i .ge. (n-k) ) then
                 bn = facx(n-i)
                 b0 = facx(i)
		    cni = bnn/(bn*b0)
              if (Mryd .eq. 2 .and. k .eq. 2) then
                if (Ny .eq. 3) adr = dryd*a
              endif
                bkn = facx(k-n+i)
		    pki = bk/bkn
                 ai = (-1.0*a*beta)**i 
                fn0 = fn0 + cni*pki*ai*ep*x**(k-n+i)
            endif
 30       continue
              fnx = fnx + adr*fn0
 40     continue
             fryd = -De*beta * ( ane + fnx )
               goto 50
c---
      elseif (Ny .ge. 5 .and. Ny .le. 8) then
           a = We * dsqrt( amu/De )
           x = R - Re
          ep = dexp(-a*x)
          a1 = (-a)**n
          a2 = (-a)**(n-1)
           b = We * dsqrt( amu/(2.0*De) )
          eb = dexp(-b*x)
          eb2= dexp(-2.0*b*x)
          Vr = - De*( 1.0 + (a - bryd)*x )*ep
          temp = a1*Vr - n*a2*(a - bryd)*De*ep
c-
        if (Ny .eq. 5) then
          fryd = temp
               goto 50
        endif
c-
        if (Ny .eq. 6) then
          b1 = (-b)**n
          b2 = (-b)**(n-1)
          b3 = (-2.0*b)**n
          fryd = b3*eb2 - n*bryd*b2*eb - b1*(2.0 + bryd*x)*eb 
          fryd = (De*fryd + temp)/2.0
               goto 50
        endif
c-
C         bm = (-bryd)**Mryd          
C         gb = dexp(-bm*x/Re)
c-
          bm = bryd**Mryd          
          gb = dexp(-x/Re)
        if (Ny .eq. 7 .or. Ny .eq. 8) then
           Vm = De*( eb2 - 2.0*eb )
           Vr = - De*( 1.0 + a*x )*ep
           b0 = bm*(2.0 - gb)
C          b0 = bryd*gb
          vrn = a1*( Vr + n*De*ep)
          vmn = 2.0*De*( 2**(n-1) *eb2 - eb ) * (-b)**n
             dn = facx(n)
            tem = 0.0
c
          do i=0,n
            di = facx(i)
            d1 = facx(n-i)
            dd = dn/(d1*di)
C
C             bni = bryd*gb*( - bm/Re )**(n-i)
C             bni = bryd*gb*( -1.0/Re )**(n-i)
C
            if (i .gt. 0) then
		  vri = (Vr + i*De*ep) * (-a)**i
              vmi = 2.0*De*( 2**(i-1) *eb2 - eb ) * (-b)**i
            else
		  vri = Vr
		  vmi = Vm
            endif
            if (i .lt. n) then
C             bni = bryd*bm*( 1.0/Re )**(n-i)
              bni = bryd*( 1.0/Re )**(n-i)
            else
              bni = b0
            endif
              if (Ny .eq. 7) tem = tem + dd*bni*(vri - vmi)
              if (Ny .eq. 8) tem = tem + dd*bni*(vmi - vri)
          enddo
c
              if (Ny .eq. 7) fryd = vrn + tem
              if (Ny .eq. 8) fryd = vmn + tem
        endif
c---
      endif
c--------------------------------------------------------
c   Add  d~n/(dR~n)[1/R**n] terms;  n = 4, 6, ..., 14.
c These terms are important for Wande-Waals molecules
c and quasi-stable molecules.
c f_n(long;R=Re) = (-1.0)**n *(k+n-1)!/[(k-1)!*R**(k+n)]
c--------------------------------------------------------
 50      R4n = 0.0
         R6n = 0.0
         R8n = 0.0
        R10n = 0.0
        R12n = 0.0
        R14n = 0.0
        temp = 0.0
        pref = (-1.0)**n
          e4 = 6.0
          e6 = 120.0
          e8 = 5040.0
         e10 = 362880.0
         e12 = 39916800.0
         e14 = 6227020800.0
      if ( br4 .gt. 0.0)  R4n = pref*facx(3+n)/( e4*R**(4+n) )    
      if ( br6 .gt. 0.0)  R6n = pref*facx(5+n)/( e6*R**(6+n) )    
      if ( br8 .gt. 0.0)  R8n = pref*facx(7+n)/( e8*R**(8+n) )    
      if (br10 .gt. 0.0) R10n = pref*facx(9+n)/( e10*R**(10+n) )    
      if (br12 .gt. 0.0) R12n = pref*facx(11+n)/( e12*R**(12+n) )    
      if (br14 .gt. 0.0) R14n = pref*facx(13+n)/( e14*R**(14+n) )    
          temp = br4*R4n + br6*R6n + br8*R8n + br10*R10n
          temp = temp + br12*R12n + br14*R14n
          fryd = fryd + bryd*temp
        return
      END
c
      function fpot(R,n,ns)
      implicit real*8(a-h,o-z)
      common /spectra/ amu,Re,De,Wee,We,WeXe,WeYe,WeZe,WeTe
      common /spectrb/ Be,alphae,gamae,Der,betae,bryd,dryd
      common /pgdata1/ bt,c0,c02,c03,c04,c05,c06,c07,c08,c09,c010
      common /pgdata2/ Re2,Re4,Re6,Re8,Re10,Re12,Re14,Re16,Re18,Re20
      common /vnumpot/ ff2, betap
      common /fms/ ms, Mryd, Mwe, Ny, nw0, nvd
      common /bdt/ beta,br4,br6,br8,br10,br12,br14
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
      ep = dexp( -a*beta*(R-Re) )
      Vr = - De*beta*( 1.0/beta + a*(R-Re) )*ep
      fpot = (-a*beta)**m * ( m*De*ep + Vr )
        goto  800
c----------------------------------------------------------
c   The nth derivative of V(Pseudo-Gaussian; R) ; 
c        2 =< n =< 11  ONLY .
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
      subroutine  calEvj(nv,nj,Lj,Ev,Evj)
      implicit real*8(a-h,o-z)
      common /spectra/ amu,Re,De,Wee,We,WeXe,WeYe,WeZe,WeTe
      common /spectrb/ Be,alphae,gamae,Der,betae,bryd,dryd
      common /spectr0/ w0,we0,wex,wey,wez,wet,wes,wer,v0,r0
      common /spectr1/ Bee,ale,gae,eta3,eta4,eta5,eta6,eta7
      common /spectr2/ Dee,bete,xsi2,xsi3,xsi4,xsi5,xsi6,xsi7
      common /Eswitch/ aye,aze,ate,ase,are
      common /Eswitc1/ abe,aae,age,ae3,ae4,ae5,ae6,ae7
      common /Eswitc2/ ade,abt,ax2,ax3,ax4,ax5,ax6,ax7
      common /fms/ ms, Mryd, Mwe, Ny, nw0, nvd
      complex*16  DisE3ca, DisE3c, DisE3ia, DisE3i 
      dimension  Em(200),Eu(200),Ew(200),Ex(200),Eh(200)
      dimension  e1(200),e2(200),e3(200),Ew0(200)
      dimension  Ev2(200),Ev3(200),Ev4(200),Ev5(200),Ev6(200)
      dimension  Ewj(200,200),Euj(200,200),Ej(200),Er(200)
      dimension  aj1(200,200),aj2(200,200)
      dimension  Evj(Lj,Lj), Ev(Lj), nj(Lj)
c----------------------------------------------------------
c  nv = the number of vibrational states used.
c         (nv-1) - The HIGHEST vibrational state.
c  nj - Number of rotational states in each v state.
c==========================================================
       rydev = 13.60569809d0
        auev = 27.21139618d0
        aucm = 219474.6306d0
      rinert = amu*Re*Re
      Brigid = 1.0/(2.0*rinert)
c----------------------------------------------------------
       if (nv .eq. 1 .and. nj(1) .eq. 1) goto 10
c----------------------------------------------------------
c  Write out vib-rot constants
c----------------------------------------------------------
        ij = 6
      do i=1,2
          write(ij,400)
        if (WeXe .ne. 0.) px = 100.0*(abs(wex) - abs(WeXe))/wex
        if (WeYe .ne. 0.) py = 100.0*(abs(wey) - abs(WeYe))/wey
        if (WeZe .ne. 0.) pz = 100.0*(abs(wez) - abs(WeZe))/wez
        if (WeTe .ne. 0.) pt = 100.0*(abs(wet) - abs(WeTe))/wet
        write(ij,410) w0,we0,We,Wee,wex,WeXe,px,wey,WeYe,py,wez,WeZe,
     #   pz,wet,WeTe,pt,wes,wer
        write(ij,420)
        write(ij,410) w0*aucm,we0*aucm,We*aucm,Wee*aucm,
     #   wex*aucm,WeXe*aucm,px,wey*aucm,WeYe*aucm,py,
     #   wez*aucm,WeZe*aucm,pz,wet*aucm,WeTe*aucm,pt,
     #   wes*aucm,wer*aucm
c
        write(ij,430) 
          if (Be .ne. 0.0)     rb = 100.0*(abs(Bee)- abs(Be))/Bee
          if (alphae .ne. 0.0) ra = 100.0*(abs(ale)- abs(alphae))/ale
          if (gamae .ne. 0.0)  rg = 100.0*(abs(gae)- abs(gamae))/gae
          if (Der .ne. 0.0)    sd = 100.0*(abs(Dee)- abs(Der))/Dee
          if (betae .ne. 0.0)  sb = 100.0*(abs(bete)-abs(betae))/bete
        write(ij,440) Bee,Be,rb,ale,alphae,ra,gae,gamae,rg,eta3,eta4,
     #   eta5,eta6,eta7,     Dee,Der,sd,bete,betae,sb,xsi2,xsi3,xsi4,
     #   xsi5,xsi6,xsi7
        write(ij,450) 
        write(ij,440) Bee*aucm,Be*aucm,rb,ale*aucm,alphae*aucm,
     #   ra,gae*aucm,gamae*aucm,rg,eta3*aucm,eta4*aucm,
     #   eta5*aucm,eta6*aucm,eta7*aucm,   
     #   Dee*aucm,Der*aucm,sd,bete*aucm,betae*aucm,sb,xsi2*aucm,
     #   xsi3*aucm,xsi4*aucm,xsi5*aucm,xsi6*aucm,xsi7*aucm  
           if (i .eq. 1) ij = 35
      enddo
c----------------------------------------------------------
c  Check if the vibrational constants reproduce De
c       c : calculated data;  i : input data.
c----------------------------------------------------------
c--- From quadratic form :
        DisE2c = We*We/(4.0*wex)
        DisE2i = Wee*Wee/(4.0*WeXe)
c--- From CUBIC form :
          cubcal = wex**2 - 3.0*We*wey
        IF (cubcal .ge. 0.0) THEN
          DisE3ca= (1.0, 0.0)*2.0*( dsqrt(cubcal) )**3
        ELSE
c--- 
c--- As cubcal < 0.0, dsqrt(cubcal) is complex !
c--- ( dsqrt(cubcal) )**3 = ( i * dsqrt(- cubcal) )**3
c--- ( i * dsqrt(cubcal) )**3 = -i * ( dsqrt(- cubcal) )**3
c--- 
          DisE3ca= -(0.0, 1.0)*2.0*( dsqrt(- cubcal) )**3
        ENDIF
          DisE3cb= wex*(2.0*wex**2 - 9.0*We*wey)
          DisE3c = DisE3ca - DisE3cb*(1.0, 0.0)
          DisE3c = DisE3c/(27.0*wey*wey)
c
          cubinp = WeXe**2 - 3.0*Wee*WeYe
        IF (cubinp .ge. 0.0) THEN
          DisE3ia= (1.0, 0.0)*2.0*( dsqrt(cubinp) )**3
        ELSE
          DisE3ia= -(0.0, 1.0)*2.0*( dsqrt(- cubinp) )**3
        ENDIF
          DisE3ib= WeXe*(2.0*WeXe**2 - 9.0*Wee*WeYe)
          DisE3i = DisE3ia - DisE3ib*(1.0, 0.0)
        if (abs(WeYe) .gt. 1.0E-50) then
          DisE3i = DisE3i/(27.0*WeYe*WeYe)
        else
          DisE3i = 0.0
        endif
c
          E2cp = 100.0*abs(DisE2c - De)/De
          E2ip = 100.0*abs(DisE2i - De)/De
c---
c Next 6 line produce Wrong results !
c           dr3c = real(DisE3c)
c           di3c = imag(DisE3c)
c         E3cp = 100.0*abs( dr3c**2 + di3c**2 - De )/De
c           dr3i = real( DisE3i - De )
c           di3i = imag( DisE3i - De )
c         E3ip = 100.0*abs( dr3i**2 + di3i**2 )/De
c---
          E3cp = 100.0*abs( real(DisE3c - De) )/De
          E3ip = 100.0*abs( real(DisE3i - De) )/De
c---
        if (abs(WeYe) .le. 1.0E-50) E3ip = 0.0
c----------------------------------------------------------
c   Print and compare the calculated De to see the quality
c of the constants (We, wex, wey, ...)
c----------------------------------------------------------
        write(6,460) De, DisE2c, DisE2i, real(DisE3c),
     # real(DisE3i), E2cp, E2ip, E3cp, E3ip
        write(35,460) De, DisE2c, DisE2i, real(DisE3c),
     # real(DisE3i), E2cp, E2ip, E3cp, E3ip
c---
          IF (cubcal .lt. 0.0) THEN
            write(6,470) wex**2, -3.0*We*wey, 
     # DisE3cb, DisE3ca, DisE3c
            write(35,470) wex**2, -3.0*We*wey, 
     # DisE3cb, DisE3ca, DisE3c
          ENDIF
          IF (cubinp .lt. 0.0) THEN
            write(6,480) WeXe**2, -3.0*Wee*WeYe, 
     # DisE3ib, DisE3ia, DisE3i
            write(35,480) WeXe**2, -3.0*Wee*WeYe, 
     # DisE3ib, DisE3ia, DisE3i
          ENDIF
c----------------------------------------------------------
c  Calculate Morse parameters :
c----------------------------------------------------------
  10       al0 = Re * dsqrt( 0.50*amu*Wee*Wee/De )
         wexem = al0*al0/(2.0*amu*Re*Re)
c----------------------------------------------------------
c  Calculate vib-rot energies using vib-rot constants
c----------------------------------------------------------
         nv1 = nv+1
         nv2 = nv+40
c---
         if (nv .eq. 1 .and. nj(1) .eq. 1) then
           nv1 = 1
           nv2 = 1
         endif
c---
      do 20 i=1,nv2
        Ev(i) = 0.0d0
           bv = 1.0d0*( i - 1 )
c          bv = dfloat( i - 1 )
             if (nv2 .eq. 1) bv = v0
          bv0 = bv + 0.5d0
         bv02 =  bv0*bv0
         bv03 = bv02*bv0
         bv04 = bv03*bv0
         bv05 = bv04*bv0
         bv06 = bv05*bv0
         bv07 = bv06*bv0
         Ev2(i) =  w0 + (We + we0)*bv0 - wex*bv02 
         Ev3(i) =  Ev2(i) + aye*wey*bv03
         Ev4(i) =  Ev3(i) + aze*wez*bv04 
         Ev5(i) =  Ev4(i) + ate*wet*bv05 
         Ev6(i) =  Ev5(i) + ase*wes*bv06 
c
c--- Vibrational energies :
c
        Ev(i) = Ev6(i) + are*wer*bv07
c--------------------------------------
c Wee -- Original input We;
c  We -- Original input We if Mwe = 0 ;
c  We -- We + bryd  if Mwe > 0 .
c--- Approximate VIBrational energies :
c--------------------------------------
        Eu(i) = We*bv0 -  wex*bv02
        Ex(i) = Wee*bv0 - WeXe*bv02
        Em(i) = Wee*bv0 - wexem*bv02
        Eh(i) = Wee*bv0 
c--- New terms of VIBrational energies :
        Ew(i) = w0 + we0*bv0
c--- Ev's without NEW terms :
        Ew0(i) = Ev(i) - Ew(i)
c
        if (nw0 .eq. 0) Ev(i) = Ew0(i)
c
            nj0 = nj(i)
        if (nj(1) .gt. 0 .and. i .le. nv1) then
          do j=1,nj0
              Evj(i,j) = 0.0d0
             bj = 1.0d0*( j - 1.0 )
               if (nv2 .eq. 1) bj = r0
            bj0 = bj*(bj + 1.0)
            bj2 = bj0*bj0
            ejb = abe*Bee*bj0 -  aae*ale*bj0*bv0 
            ejb = ejb + age* gae*bj0*bv02 
            ejb = ejb - ae3*eta3*bj0*bv03 - ae4*eta4*bj0*bv04
            ejb = ejb - ae5*eta5*bj0*bv05 - ae6*eta6*bj0*bv06
            ej1 = ejb - ae7*eta7*bj0*bv07
            ej1a= ej1 - abe*Bee*bj0
c
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
          if (nv2 .eq. 1) goto 800 
c----------------------------------------------------------
c  Write out vib-rot energies
c----------------------------------------------------------
        write( 6,500) 
        write(35,500) 
        write(36,500) 
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
        write(35,510) kv, Ev(i), Ev(i)*aucm, difv, difv*aucm
        write( 6,510) kv, Ev(i), Ev(i)*aucm, difv, difv*aucm
        write(36,510) kv, Ev(i)
          if ( Ev(i) .gt. Ev(i-1) )   Emax = Ev(i)
          if (difv .gt. 0.0) sumdif = sumdif + difv
  30  continue
        write( 6,530) sumdif, Emax
        write(35,530) sumdif, Emax
c       write(35,530) sumdif, Emax-Ev(1)
c
        write( 6,535) 
        write(35,535) 
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
        write( 6,510) kv, Ev(i), Ew(i), difv
        write(35,510) kv, Ev(i), Ew(i), difv
      enddo
c
        write( 6,560) nw0 
        write(35,560) nw0
          kk = 0
      do i=1,nv
            kv = i - 1
          difv = Ew0(i) - Ew0(i-1)
            if (i .eq. 1) difv = Ew0(1)
          if ( Ev(i) .lt. Ev(i-1) .and. kk .eq. 0) then
              kk = 1
            write( 6,*)
            write(35,*)
          endif
        write( 6,510) kv, Ev(i), Ew0(i), Ew(i), difv
        write(35,510) kv, Ev(i), Ew0(i), Ew(i), difv
      enddo
c
        write( 6,570)
        write(35,570)
          kk = 0
      do i=1,nv
            kv = i - 1
          difv = ( Ew0(i) - Ew0(i-1) )*aucm
            if (i .eq. 1) difv = Ew0(1)*aucm
          if ( Ev(i) .lt. Ev(i-1) .and. kk .eq. 0) then
              kk = 1
            write( 6,*)
            write(35,*)
          endif
            evcm = Ev(i)*aucm
            ewcm = Ew(i)*aucm
            e0cm = Ew0(i)*aucm
        write( 6,510) kv, evcm, e0cm, ewcm, difv
        write(35,510) kv, evcm, e0cm, ewcm, difv
      enddo
c
        write( 6,580)
	  write(35,580)
          Emaxa = 0.0
          Emaxb = 0.0
          sumdifa = 0.0
          sumdifb = 0.0
          kk = 0
      do i=1,nv
            kv = i - 1
          if ( Ev(i) .lt. Ev(i-1) .and. kk .eq. 0) then
              kk = 1
            write( 6,*)
            write(35,*)
          endif
              difa = Eu(i) - Eu(i-1)
              difb = Ex(i) - Ex(i-1)
            if (i .eq. 1) difa = Eu(i)
            if (i .eq. 1) difb = Ex(i)
          if (difa .gt. 0.0) sumdifa = sumdifa + difa
          if (difb .gt. 0.0) sumdifb = sumdifb + difb
            if ( Eu(i) .gt. Eu(i-1) ) Emaxa = Eu(i)
            if ( Ex(i) .gt. Ex(i-1) ) Emaxb = Ex(i)
        write(35,510) kv, Ev(i), Ew(i), Eu(i), Ex(i)
        write( 6,510) kv, Ev(i), Ew(i), Eu(i), Ex(i)
      enddo
        write( 6,540) sumdifa, sumdifb, Emaxa, Emaxb
        write(35,540) sumdifa, sumdifb, Emaxa, Emaxb
c
        write( 6,544)
        write(35,546)
          kk = 0
         difa = 0.0
         difb = 0.0
      do i=1,nv
            kv = i - 1
          if ( Ev(i) .lt. Ev(i-1) .and. kk .eq. 0) then
              kk = 1
            write( 6,*)
            write(35,*)
          endif
            evcm = Ev(i)*aucm
            e0cm = Ew0(i)*aucm
            eucm = Eu(i)*aucm
            excm = Ex(i)*aucm
            difau = Eu(i) - Ex(i)
            difcm = difau*aucm
            difa = difa + abs(difau)
            difb = difb + abs(difcm)
        write( 6,510) kv, evcm, e0cm, eucm, excm
        write(35,510) kv, eucm, excm, difcm, difau
      enddo
        write( 6,550) difb/nv ,  difa/nv
        write(35,550) difb/nv ,  difa/nv
c 
      do i=1,nv
c-- Read in the INPUT vibrational energies OR their differencies
        read(23,*) Ew0(i)
      enddo
c
C     if (Ew0(nv) .lt. Ew0(1)) then
c-- As Ew0(nv) < Ew0(1), you readed Ev differencies;
c--   you need to find Ev themselves.
C         Ew(1) = Ew0(1)
C         Ew(2) = Ew0(2) + Ew0(1)
C       do i=3,nv
C         Ew(i) = Ew(i-1) + Ew0(i)
C       enddo
c
      if (Ew0(nv) .gt. Ew0(1)) then
c-- As Ew0(nv) > Ew0(1), you readed Ev ;
c--   you need to find Ev differencies .
          Ew(1) = Ew0(1)
        do i=2,nv
          Ew(i) = Ew0(i) - Ew0(i-1)
        enddo
c  
        do i=2,nv
          Ew0(i) = Ew(i)
        enddo
      endif
c
      if (Ew0(1) .gt. Ev(1)*1000.0d0) then
c-- Change the unit of Ew0 from cm-1 to a.u.
        do i=1,nv
          Ew0(i) = Ew0(i)/aucm
        enddo
      endif
c 
        write( 6,590)
        write(35,590)
          kk = 0
         esum= 0.0
      do i=1,nv
            kv = i - 1
          if ( Ev(i) .lt. Ev(i-1) .and. kk .eq. 0) then
              kk = 1
            write( 6,*)
            write(35,*)
          endif
c
C           dvcm = ( Ev(i) - Ew0(i) )*aucm
C           ducm = ( Eu(i) - Ew0(i) )*aucm
C           dxcm = ( Ex(i) - Ew0(i) )*aucm
C           dvcm = ( ( Ev(i)-Ev(i-1) ) - Ew0(i) )*aucm
c 
            e1(i)= ( Eu(i)-Eu(i-1) )*aucm
            e2(i)= ( ( Eu(i)-Eu(i-1) ) - Ew0(i) )*aucm
            e3(i)= 100.0 * abs( e2(i) )/( Ew0(i)*aucm )
            dxcm = ( ( Ex(i)-Ex(i-1) ) - Ew0(i) )*aucm
              if (i .eq. 1) dvcm = ( Ev(1) - Ew0(1) )*aucm
              if (i .eq. 1) e2(i)= ( Eu(1) - Ew0(1) )*aucm
              if (i .eq. 1) dxcm = ( Ex(1) - Ew0(1) )*aucm
            if ( i .le. nvd ) esum = esum + e3(i)
        write( 6,510) kv, Ew0(i)*aucm, e1(i), e2(i), dxcm
        write(35,510) kv, Ew0(i)*aucm, e1(i), e2(i), dxcm
C       write(35,510) kv, Ew0(i)*aucm, dvcm,  e2(i), dxcm
      enddo
c 
        write( 6,594)
        write(35,594)
      do i=1,nvd
            kv = i - 1
        write( 6,510) kv, Ew0(i)*aucm, e1(i), e2(i), e3(i)
        write(35,510) kv, Ew0(i)*aucm, e1(i), e2(i), e3(i)
      enddo
        write( 6,596) esum/nvd
        write(35,596) esum/nvd
c
        write( 6,620) wex,WeXe,wexem,wex*aucm,WeXe*aucm,wexem*aucm 
        write(35,620) wex,WeXe,wexem,wex*aucm,WeXe*aucm,wexem*aucm 
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
        write(35,510) kv, ev0, ex0, em0, eh0
        write( 6,510) kv, ev0, ex0, em0, eh0
      enddo
C
        write( 6,610)
        write(35,610)
        write(38,642)
      do k=1,6
            if (k .eq. 1) write( 6,611) 
            if (k .eq. 1) write(35,611) 
            if (k .eq. 2) write( 6,612) 
            if (k .eq. 2) write(35,612) 
            if (k .eq. 2) write(38,644) 
            if (k .eq. 3) write( 6,613) 
            if (k .eq. 3) write(35,613) 
            if (k .eq. 3) write(38,645) 
            if (k .eq. 4) write( 6,614) 
            if (k .eq. 4) write(35,614) 
            if (k .eq. 4) write(38,646) 
            if (k .eq. 5) write( 6,615) 
            if (k .eq. 5) write(35,615) 
            if (k .eq. 5) write(38,647) 
            if (k .eq. 6) write(38,648) 
	  do i=1,nv
            kv = i-1
          if (k .eq. 1) then
              error = 100.0*( Ev3(i) - Ev2(i) )/Ev2(i)
            write( 6,510) kv, Ev2(i)*aucm, Ev3(i)*aucm, error
            write(35,510) kv, Ev2(i)*aucm, Ev3(i)*aucm, error
            write(38,510) kv, Ev2(i)
	    elseif(k .eq. 2) then
              error = 100.0*( Ev4(i) - Ev3(i) )/Ev3(i)
            write( 6,510) kv, Ev3(i)*aucm, Ev4(i)*aucm, error
            write(35,510) kv, Ev3(i)*aucm, Ev4(i)*aucm, error
            write(38,510) kv, Ev3(i)
	    elseif(k .eq. 3) then
              error = 100.0*( Ev5(i) - Ev4(i) )/Ev4(i)
            write( 6,510) kv, Ev4(i)*aucm, Ev5(i)*aucm, error
            write(35,510) kv, Ev4(i)*aucm, Ev5(i)*aucm, error
            write(38,510) kv, Ev4(i)
	    elseif(k .eq. 4) then
              error = 100.0*( Ev6(i) - Ev5(i) )/Ev5(i)
            write( 6,510) kv, Ev5(i)*aucm, Ev6(i)*aucm, error
            write(35,510) kv, Ev5(i)*aucm, Ev6(i)*aucm, error
            write(38,510) kv, Ev5(i)
	    elseif(k .eq. 5) then
              error = 100.0*(  Ev(i) - Ev6(i) )/Ev6(i)
            write( 6,510) kv, Ev6(i)*aucm, Ev(i)*aucm, error
            write(35,510) kv, Ev6(i)*aucm, Ev(i)*aucm, error
            write(38,510) kv, Ev6(i)
	    elseif(k .eq. 6) then
            write(38,510) kv, Ev(i)
	    endif
        enddo
      enddo
C
c====================================================
c-- bjj is classicle rotational ANGular momentum
c-- bvj is rotational ANGular velocity
c
        write( 6,710) rinert, Brigid
        write(35,710) rinert, Brigid
        write(36,710) rinert, Brigid
      do j=1,nj(1)
           bj = j*1.0 - 1.0
          bjj = dsqrt( bj*(bj + 1.0) )
          bvj = bjj/rinert
        write( 6,510) j-1, Ej(j), Er(j), bjj, bvj
        write(35,510) j-1, Ej(j), Er(j), bjj, bvj
        write(36,510) j-1, Ej(j)
      enddo
c
      if (nj(1) .gt. 0) then
          write( 6,625) ade 
          write(35,625) ade
          write(36,625) ade
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
             if ( Evj(i,j) .gt. Evj(i+1,1) .and. kk .eq. 0 ) then
                   difj0 = Evj(i+1,1) - Evj(i,j-1)
 	         if (Evj(i,j-1) .gt. Evjmax) then
		     if (Evj(i,j-1) .le. Emax)  Evjmax = Evj(i,j-1)
		   endif
                   kk = 1
                 write( 6,*) 
                 write(35,*) 
                 write(36,*) 
             endif
           write(35,650) kv,kj,Evj(i,j),Evj(i,j)*aucm,difvj,difvj*aucm
           write( 6,650) kv,kj,Evj(i,j),Evj(i,j)*aucm,difvj,difvj*aucm
           write(36,650) kv,kj,Evj(i,j)
	       if ( Evj(i,j) .eq. Emax)   jj = 1
 	       if ( Evj(i,j) .eq. Emax)   sumdif = sumdif + difvj
          enddo
	      if (jj .eq. 1) then
              write( 6,*) 
              write(35,*) 
              write(36,*) 
		else
              write( 6,630)
              write(35,630)
              write(36,630)
		endif
        enddo
          write(35,640) sumdif, Evjmax
          write( 6,640) sumdif, Evjmax
c
          write(35,660) 
          write( 6,665) 
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
            write(35,650) kv,kj,Evj(i,j),Ev(i),aj1(i,j)+aj2(i,j)
            write( 6,650) kv,kj,Evj(i,j),Ewj(i,j),Ev(i),Ej(j)
              if ( Evj(i,j) .eq. Emax) jj = 1
          enddo
            if (jj .eq. 1) then
              write( 6,*)
              write(35,*)
            else
              write( 6,630)
              write(35,630)
            endif
        enddo
c
          write(35,670) 
        do i=1,nv
            kv = i - 1
            nj0 = nj(i)
          do j=1,nj0
            kj = j - 1
            write(35,650) kv,kj,Evj(i,j),Ev(i),aj1(i,j),aj2(i,j)
          enddo
            write(35,*) 
        enddo
c
          write(35,680) 
        do i=1,nv
            kv = i - 1
            nj0 = nj(i)
          do j=1,nj0
            kj = j - 1
            write(35,650) kv,kj,Evj(i,j),Euj(i,j),aj1(i,j)
          enddo
            write(35,*) 
        enddo
c
      endif
c
c----------------------------------------------------------
 400  format(///6x,'--- Ro-vibrational constants & energies --- ',/
     #/2x,'All calculated constants are the functions of the input ', 
     #/2x,'"We", "Re", & "mu",  and are based on the perturbation ',
     #/2x,'theory,  except that "We" is the input value.',
     #///8x,'**  The vibrational constants (in a.u.) are  ** ',
     #/6x,'evaluated using force constants from ANAlytical deriv.', 
C    #/6x,'evaluated using force constants from NUMerical deriv.', 
     #//6x,'  { If mtyp = 1, Consts(calc.) == Consts(input) } ',
     #//10x,'[ Error% = 100.0*(|Calc.| - |Input|)/Calc. ] ')
 410  format(/5x,'          Calculated              Input ',
     #13x,'Error% ',
     #//3x,' W0  = ', 1PE20.12,/3x,' We0 = ', 1PE20.12,
     # /3x,' We  = ', 1PE20.12,2x,1PE20.12,
     # /3x,'WeXe = ', 1PE20.12,2x,1PE20.12,2x,1PE13.5,
     # /3x,'WeYe = ', 1PE20.12,2x,1PE20.12,2x,1PE13.5,
     # /3x,'WeZe = ', 1PE20.12,2x,1PE20.12,2x,1PE13.5,
     # /3x,'WeTe = ', 1PE20.12,2x,1PE20.12,2x,1PE13.5,
     # /3x,'WeSe = ', 1PE20.12,/3x,'WeRe = ', 1PE20.12 )
 420  format(//13x,'The vibrational constants (in cm-1) are : ')
 430  format(///12x,'## The rotational constants (in a.u.) are : ## ')
 440  format(/5x,'             Calculated               Input ',
     #13x,'Error% ',
     #//3x,'     Be = ', 1PE20.12,2x,1PE20.12,2x,1PE13.5,
     # /3x,'Alpha_e = ', 1PE20.12,2x,1PE20.12,2x,1PE13.5,
     # /3x,'Gamma_e = ', 1PE20.12,2x,1PE20.12,2x,1PE13.5,
     # /3x,' Eta_e3 = ', 1PE20.12, /3x,' Eta_e4 = ', 1PE20.12,
     # /3x,' Eta_e5 = ', 1PE20.12, /3x,' Eta_e6 = ', 1PE20.12,
     # /3x,' Eta_e7 = ', 1PE20.12,
     #//3x,'    Dee = ', 1PE20.12,2x,1PE20.12,2x,1PE13.5, 
     # /3x,' Beta_e = ', 1PE20.12,2x,1PE20.12,2x,1PE13.5,
     # /3x,' Xsi_e2 = ', 1PE20.12, /3x,' Xsi_e3 = ', 1PE20.12,
     # /3x,' Xsi_e4 = ', 1PE20.12, /3x,' Xsi_e5 = ', 1PE20.12,
     # /3x,' Xsi_e6 = ', 1PE20.12, /3x,' Xsi_e7 = ', 1PE20.12 )
 450  format(//15x,'The rotational constants (in cm-1) are : ')
 460  format(///15x,'    Check VIBRATIONAL constants : ',
     #//8x,"[ As V(R)=De, De can be calculated by Rees's Eq. ",
     # /8x,"            from  VIBrational constants.         ",
     #//8x,"    Quadratic potential form :   ",
     # /8x,"  De_2c -- De from calculated vibrational constants; ",
     # /8x,"  De_2i -- De from  inputted  vibrational constants. ",
     #//8x,"                De_2 = We*We/(4*Wexe)                ",
     #//8x,"      CUBIC   potential form (much accurate) :   ",
     # /8x,"  De_3c -- De from calculated vibrational constants; ",
     # /8x,"  De_3i -- De from  inputted  vibrational constants. ",
     #//8x,"     De3(a) = 2*[dsqrt(Wexe**2 - 3*We*Weye)]**3      ",
     # /8x,"     De3(b) =  Wexe*(2*Wexe**2 - 9*We*Weye)          ",
     # /8x,"       De_3 = [De3(a) - De3(b)]/(27*Weye**2)         ",
     #//8x,"     IF  term = Wexe**2 - 3*We*Weye  < 0.0           ",
     # /8x,"       dsqrt( term ) == complex number !             ",
     #//8x,"       De_2   =       De_2c  OR  De_2i               ",
     # /8x,"       De_3   =       De_3c  OR  De_3i               ",
     # /8x,"       De3(a) =       De_3ca OR  De_3ia              ",
     # /8x,"       De3(b) =       De_3cb OR  De_3ib              ",
     #//8x,"  Higher_power potential form is much more accurate. ",
     # /8x,"    Set De_3i & D3i% = 0.0 if inputted WeYe is 0.0 . ",
     #//8x,"       Dei = Input 'TRUE' dissociation energy De :   ",
     #//8x,"           D2c% = 100 * | De_2c - Dei |/Dei ;   ",
     # /8x,"           D2i% = 100 * | De_2i - Dei |/Dei ;   ",
     # /8x,"        D3c% = 100 * | real(De_3c - Dei) |/Dei ;   ",
     # /8x,"        D3i% = 100 * | real(De_3i - Dei) |/Dei .       ]",
     #//18x,"    Dei == De = ",f12.8,'  a.u.',
     #//4x," De_2c(a.u)      De_2i(a.u)      De_3c(a.u)     ",
     # " De_3i(a.u) ",/2x,4(1f12.8,4x),
     #//8x,"          Percent errors of generated De : ",
     #/4x,"   D2c%             D2i%            D3c%           D3i% ",
     #/2x,4(1f12.6,4x) )
 470  format(///15x,'    Check CUBIC form calculations : ',
     #//12x," Using the calculated constants Wexe and Weye ",
     #//11x,"       Wexe**2(a.u.)     -3*We*Weye(a.u.)   ",
     #/17x,14(1H-),5x,16(1H-)
     #/15x,2(1PE16.8,4x),
     #///4x,"De_3cb(a.u)",9x,"De_3ca(a.u)",16x,"De_3c(a.u)",
     #/3x,13(1H-),2x,24(1H-),2x,25(1H-)
     #/7x,"Real",10x,"Real",7x,"Imaginary",6x,"Real",8x,"Imaginary",
     #/2x,1PE14.6,2(1PE13.5),1x,2(1PE13.5) )
 480  format(///15x,'    Check CUBIC form calculations : ',
     #//15x," Using the INPUT vibrational constants ",
     #//11x,"       WeXe**2(a.u.)     -3*We*WeYe(a.u.)   ",
     #/17x,14(1H-),5x,16(1H-)
     #/15x,2(1PE16.8,4x),
     #///4x,"De_3ib(a.u)",9x,"De_3ia(a.u)",16x,"De_3i(a.u)",
     #/3x,13(1H-),2x,24(1H-),2x,25(1H-)
     #/7x,"Real",10x,"Real",7x,"Imaginary",6x,"Real",8x,"Imaginary",
     #/2x,1PE14.6,2(1PE13.5),1x,2(1PE13.5) )
 500  format(///17x,'=== The VIBrational energies are : === ',
     #//19x,'    [  Ev_dif = E(v) - E(v-1)  ] ',
     #//5x,'v      E(v; a.u.)        E(v; cm-1)     Ev_dif(a.u)',
     #'   Ev_dif(cm-1)',/)
 510  format(3x,i3,1PE18.9,1PE18.9,x,1PE13.5,x,1PE13.5)
 520  format(1PE18.9)
 530  format(/12x,'The SUM of  Ev_dif(a.u) = ',1PE16.8,
     #/12x,'Maximum energy  Ev(max) = ',1PE16.8,/)
C    #/12x,'Ev(maximum)  -  Ev(v=0) = ',1PE16.8,/)
 535  format(//5x,'v      E(v; a.u.)      Enew(v; a.u.)',
     #'    Ev_dif(a.u.)',/)
 540  format(/2x,'SUM of Ea_dif(a.u) =',1PE13.6,
     # 3x,'SUM of Eb_dif(a.u) =',1PE13.6,
     #/2x,'Maxi. Ea(v)=Ea(max)=',1PE13.6,
     # 3x,'Maxi. Eb(v)=Eb(max)=',1PE13.6,/)
 544  format(//5x,"v     E(v; cm-1)       E'(v; cm-1)",
     #"      Ea(v; cm-1)    Eb(cm-1)",/)
 546  format(//5x,"v     Ea(v; cm-1)       Eb(v; cm-1)",
     #"     Ea-Eb(cm-1)   Ea-Eb(a.u.)",/)
 550  format(/15x,'Average of |Ea(cm-1) - Eb(cm-1)| =',1PE13.6,
     #/15x,'Average of |Ea(a.u.) - Eb(a.u.)| =',1PE13.6,/)
 560  format(///8x,"[  E'(v) = E(v) - Enew(v);  ",
     #"E(v) =/= E'(v)  for nw0 = 1 ; ",
     #/12x,"E(v) = E(v) - Enew(v);  E(v) === E'(v)  for nw0 = 0  ]",
     #/31x,"nw0 =",i2,/
     #/5x,"v     E(v; a.u.)        E'(v; a.u.)    Enew(v; a.u) ",
     #"  E'v_dif(au)",/)
 570  format(//5x,"v     E(v; cm-1)        E'(v; cm-1)",
     #"    Enew(v;cm-1)  E'v_dif(cm-1)",/)
 580  format(//16x,'Comparing vibrational energies (I) : ',
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
C590  format(///25x,"  Einp(v) is the INPUT Ev's ",
C    #/27x,"Edifv(v) =  E(v) - Einp(v) ",
C    #/27x,"Edifa(v) = Ea(v) - Einp(v) ",
C    #/27x,"Edifb(v) = Eb(v) - Einp(v) ",/
C    #/5x,"v    Einp(v; cm-1)    Edifv(v; cm-1)",
C    #"    Edifa(cm-1)   Edifb(cm-1)",/)
 590  format(///18x,"DEin(v) is the INPUT differencies Einp(v) ",
C    #/18x,"Edelv(v) =  [  E(v) -  E(v-1) ] - DEin(v) ",
     #/18x,"Edifa(v) =  [ Ea(v) - Ea(v-1) ] ",
     #/18x,"Edela(v) =  [ Ea(v) - Ea(v-1) ] - DEin(v) ",
     #/18x,"Edelb(v) =  [ Eb(v) - Eb(v-1) ] - DEin(v) ",/
C    #/5x,"v    Einp(v; cm-1)    Edelv(v; cm-1)",
     #/5x,"v    Einp(v; cm-1)    Edifa(v; cm-1)",
     #"    Edela(cm-1)   Edelb(cm-1)",/)
 594  format(///17x,"ERRORa(v)% = 100.0*| Edela(v) |/Einp(v) : ",/
     #/5x,"v    Einp(v; cm-1)    Edifa(v; cm-1)",
     #"    Edela(cm-1)   ERRORa(cm-1)",/)
 596  format(/5x,'Average errors for quadratic Ev,',
     #'  ERRORa(v)% = ',1PE16.8,'%',/)
 610  format(///11x,'Comparing vibrational energies (III) : ',/
     #/12x,"Edifv(i) =  Ev_(n)[i] - Ev_(n-1)[i] ",
     #/8x,'Error_v(i)% = 100.0*{ Edifv(i) }/Ev_(n-1)[i] : ',)
 611  format(//11x,'Ev2(i) => Energy expanded to Wexe term,',
     #/11x,'Ev3(i) => Energy expanded to Weye term.',/
     #/5x,"v      Ev2(cm-1)         Ev3(cm-1)       Error_v%",/)
 612  format(//11x,'Ev3(i) => Energy expanded to Weye term,',
     #/11x,'Ev4(i) => Energy expanded to Weze term.',/
     #/5x,"v      Ev3(cm-1)         Ev4(cm-1)       Error_v%",/)
 613  format(//11x,'Ev4(i) => Energy expanded to Weze term,',
     #/11x,'Ev5(i) => Energy expanded to Wete term.',/
     #/5x,"v      Ev4(cm-1)         Ev5(cm-1)       Error_v%",/)
 614  format(//11x,'Ev5(i) => Energy expanded to Wete term,',
     #/11x,'Ev6(i) => Energy expanded to Wese term.',/
     #/5x,"v      Ev5(cm-1)         Ev6(cm-1)       Error_v%",/)
 615  format(//11x,'Ev6(i) => Energy expanded to Wese term,',
     #/11x,'Ev7(i) => Energy expanded to Were term.',/
     #/5x,"v      Ev6(cm-1)         Ev7(cm-1)       Error_v%",/)
 620  format(//17x,'Comparing vibrational energies (II) : ',
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
 625  format(///21x,'The VIB-ROTational energies are : ',
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
 630  format(16x,'-----',13x,'-----',12x,'-----',9x,'-----',/)
 640  format(/12x,'The SUM of Evj_dif(a.u) = ',1PE16.8,
     #/12x,'Maximum energy Evj(max) = ',1PE16.8,/
     #/6x,'If the above TWO are NOT equal, Check  nj  in each ',
     #'v state !',
     #/11x,'Set  nj >= 1 + j_max (of vib. state) = 1 + (nj-1) '/)
 642  format(//1x,"Vibrational energies Ev's in various form : ",/
     #/2x,'Ev2(i) => Energy expanded to Wexe term;',
     #/2x,'Ev3(i) => Energy expanded to Weye term;',
     #/2x,'Ev4(i) => Energy expanded to Weze term;',
     #/2x,'Ev5(i) => Energy expanded to Wete term;',
     #/2x,'Ev6(i) => Energy expanded to Wese term;',
     #/2x,'Ev7(i) => Energy expanded to Were term.',/
     #/5x,"v      Ev2(a.u.)   ",/)
 644  format(//5x,"v      Ev3(a.u.)   ",/)
 645  format(//5x,"v      Ev4(a.u.)   ",/)
 646  format(//5x,"v      Ev5(a.u.)   ",/)
 647  format(//5x,"v      Ev6(a.u.)   ",/)
 648  format(//5x,"v      Ev7(a.u.)   ",/)
 650  format(3x,2i3,1PE18.9,1PE18.9,x,1PE13.5,x,1PE13.5)
 660  format(///13x,'Contributions to E_vj are (Part I) : ',
     #//7x,'[   E(v,j) = E(v) + Ej ;     Ej = Ej1 + Ej2  ; ',
     # /7x,'  Ej1 = E{v;j*(j*1)} ;  Ej2 = E{v;j*j*(j*1)**2}  ] ',/
     #/5x,'v  j    E(v,j; a.u.)        E(v;a.u.)       Ej(a.u.) ',/)
 665  format(///18x,'Separate contributions to E_vj  : ',
     #//12x,'[   E(v,j) = E(v-j_coupling) + E(v) + E(j) ; ',
     #//12x,'    E(v-j_coup) = Vib-Rot_coupling energies; ',
     # /12x,'           E(v) =    Vibrational   energies; ',
     # /12x,'           E(j) =    Rotational    energies.  ] ',/
     #/5x,'v  j    E(v,j; a.u.)       E(v-j_coup)     E(v; a.u.) ',
     #'    E(j; a.u.) ',/)
 670  format(///17x,'Contributions to E_vj are (Part II) : ',
     #//19x,'[  E(v,j) = E(v) + Ej1 + Ej2  ] ',/
     #/5x,'v  j    E(v,j; a.u.)        E(v;a.u.)       Ej1(a.u.) ',
     #'    Ej2(a.u.) ',/)
 680  format(///12x,'Contributions to E_vj are (Part III) : ',
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
      common /spectra/ amu,Re,De,Wee,We,WeXe,WeYe,WeZe,WeTe
      common /spectrb/ Be,alphae,gamae,Der,betae,bryd,dryd
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
      common /spectra/ amu,Re,De,Wee,We,WeXe,WeYe,WeZe,WeTe
      common /spectrb/ Be,alphae,gamae,Der,betae,bryd,dryd
      common /spectr0/ w0,we0,wex,wey,wez,wet,wes,wer,v0,r0
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
C
      REAL*8 FUNCTION FACX(I)
      IMPLICIT REAL*8 (A-H,O-Z)
C---------------------------------------------------------------------
C     THIS IS A FACTORIAL FUNCTION  I!
C---------------------------------------------------------------------
      DIMENSION TABLE(15)
      DATA TABLE/1.0D+00,2.0D+00,6.0D+00,24.0D+00,120.0D+00,720.0D+00,
     1      5040.0D+00,40320.0D+00,362880.0D+00,
     2      36288.0D+02,399168.0D+02,4790016.0D+02,62270208.0D+02,
     3      871782912.0D+02,1307674368.0D+03/
C---------------------------------------------------------------------
      FACX=1.0
      IF (I)95,100,10
   10 IF (I-15)20,20,30
   20 FACX=TABLE(I)
      GO TO 200
C-----------------------
   30 FJ=16.0D+00
      FACX=TABLE(15)
      DO 40 J=16,I
      FACX=FACX*FJ
   40 FJ=FJ+1.0D+00
      GO TO 200
   95 FACX=0.0D+00
  100 CONTINUE
C-----------------------
  200 RETURN
      END
C
