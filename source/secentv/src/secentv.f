C     program  secen
      implicit real*8(a-h,o-z)
c===================================================================
c   This code uses the Numerov method (subroutine solvese) to solve
c the 2nd order differential Schrodinger equation (se) for a CENTRAL
c (SPHERICAL) potential (centv) for a given BOUND-STATE 
c (NOT scattering) energy E_v :

c           u''(r) - f(r)u(r) = 0 .     Here it becomes :

c  { - d^2/(dr^2) + l*(l+1)/r**2 + 2*mu*[V(r) - E_v] } u(r) = 0
c
c   The code will give :
c 1.)  The potentials V(r) of a specific central potential in unit 10.
c                                      numerical potential in unit 20.
c 2.)  a set of EIGENfunctions for the given (inputted/calculated)
c    energies E_v in unit 7.
c 
c  Modified & enlarged  by       Weiguo Sun         11/19/1993
c  Modified to include Sun-Murrell-Sorbie (SMS) potential
c           by Weiguo Sun        05/26/1995
c  Modified to converge SMS wavefunctions by Weiguo Sun  11/20/1996
c
c  Modified to include ECM and rewrite the codes by Hao Feng 11/14/2000
c     Now, it can cope with the following potential:
c         ============================
c            mtyp     ktyp    N
c                     1       1
c                     2       2
c            3        3       3
c            4        4       4
c                     5       5
c            5        6,7,8   6,7,8
c            9        9       9
c         ============================
c   1.) Gaussion type or of Slater type ;
c
c   2.) Spherical central potential V0*exp(-alpha*R) ;
c
c   3.) Simple harmonic potential   V(x)=0.5d0*ak*x**2 ;
c              where  x=R-Re
c
c   4.) Morse potential  V(x)=de[exp(-2*a*x) - 2*exp(-a*x)] + de ;
c              where  x=(R-Re)/Re
c
c   5.) Screened central potential  V(x)=-Z*dexp(-x/D)/x
c
c   6.) Sun-Murrell--Sorbie potential  :
c              temp = 1/beta + a1*x + a2*x**2 + a3*x**3 +
c                     a4*x**4 + a5*x**5
c              V_MS(x) = - De*beta*temp*exp(-a1*beta*x)
c              where x = R - Re
c
c   7.) Huxley-Murrell-Sorbie :
c              V_MS(x)=-De*(1 + a1*x + a2*x**2 + a3*x**3 + ...)*exp(-a1*x)
c              where x = R - Re
c
c   8.) SF (ECM) :
c              V_ecm(R) = V_MS(R) + Lamta(R)*delta_V(R)
c
c              V_MS:  Murrell & Sorbie potential in which the
c                 (a1,a2,a3, ...) are calculated using SF formulae.
c              Lamta(R)   = lamta*(x/R)*[1-exp(-lamta^2*x/R)]
c              delta_V(R) = V_MS - V_0
c                For  R < Re :
c              Nturn = 0, V_ecm(R) = V_MS   for ms = 3 ONLY ;
c                For  ms = 4, 5 :
c                = 1, V_ecm(R) = V_0,   V_0 = V_Morse   ;
c                = 2, V_ecm(R) = V_0,   V_0 = V_Rydberg ;
c                = 3, V_ecm(R) = V_0,   V_0 = V_PG      .
c
c   9.) Numerical potential:    read it from fort.20
c
c===================================================================
c  nd -- the dimension of r-mesh & wavefunction arrays;  nd >= ng.
c----------------------------------------------------------------
C       parameter(nd=120, nd1=2*nd, na=6000)
C       parameter(nd=120, nd1=2*nd, na=10000)
c----------------------------------------------------------------
      parameter(nd=120, nd1=2*nd, na=20000)
      common /pdata/ vc(10),xl(10),nv(10),Add,nvp,iel,ktyp,ms
      common /fdata/ kfre,nci,ncf
      common /rdata/ rmin,rmax,rmatch,h,ul0(nd1),ur0(nd1)
      common /vibe/  evib(nd1),evib1(nd1),eori(nd1),expr(nd1)
      common /vcon/ qeng(nd1),vtol(nd1),dev(nd1),kcon(nd1)
      common /kmon/ kstd(nd1)
      dimension wf(nd),rmesh(nd),u(20000),eigf(na,nd)
c----------------------------------------------------------------
      nbmax = 20
      call readin(rmesh,nbmax,ng,nr,nbi,nwv)
c----------------------------------------------------------------
c  These rmesh(j) aren't of course gaussian mesh points, but the 
c  magnitudes are about what we want for our problem.
c----------------------------------------------------------------
c     do j = 1, ng
c       rmesh(j) = 1.5d0 + 1.5011274d0 * j/ng
c     enddo
c----------------------------------------------------------------
      aucm = 219474.63067d0
      write(6,320) 
      write(21,350)
C---
      k1 = 5
      ku = 21
      kw = 29 + nbi - 1
c----------------------------------------------------------------
c  Redefine unit switch for writting purpose
c----------------------------------------------------------------
      if (nwv .ge. 61) kw = 29
      if (nwv .ge. 61) k1 = nbi + 5
c----------------------------------------------------------------
c  Loop over bound states
c----------------------------------------------------------------
      DO nu=nbi, nbmax
C
         if (nu .gt. k1 .and. (nu-k1) .gt. 10) then
            k1 = k1 + 10
            ku = ku + 1
            write(ku,350)
         endif
C
c----------------------------------------------------------------
c   boundv (call solvese -->) solves Schrodinger Equation & get 
c the numerical wavefunction u.
c----------------------------------------------------------------
         call boundv(nbi,ku,nu,eigv,u)
c----------------------------------------------------------------
         do i=1,nr
            eigf(i,nu) = u(i)
         enddo
      ENDDO
c----------------------------------------------------------------
c Loop bound states again
c      vibrational quantum number :   v = nu -1
c----------------------------------------------------------------
      DO 100 nu=nbi,nbmax
C
         eigv = evib(nu)
         if (nu .eq. 61) then
            write(6,435) 
            write(0,435)
         endif
         kw = kw + 1
c----------------------------------------------------------------
         do i=1,nr
            u(i) = eigf(i,nu)
         enddo
c----------------------------------------------------------------
c   Evaluate expectation value of r :  < u | r | u > = expr
c and wavefunctions (wf) at rmesh points.
c----------------------------------------------------------------
         call expectwf(kw,nr,nu,ng,nbi,nwv,h,rmin,eigv,u, rmesh,wf,expr)
c----------------------------------------------------------------
         if (nu .eq. nbi) write(3,*) nwv,ng
         if (nu .eq. nbi) write(7,*) nwv,ng
         if (nu .le. nwv+1) then
            write(7,420)  nu-1, evib(nu)
	    if (Add .gt. 0.0) then
               write(7,422)  evib(nu)+Add
            endif
            if (ktyp .eq. 9) goto 100
            do j = 1, ng
               write(3,430) rmesh(j), wf(j)
               write(7,430) rmesh(j), wf(j)
            enddo
         endif
 100  CONTINUE
c----------------------------------------------------------------
      do nu=nbi, nbmax
         if (nu .le. nwv+1) write(3,440) evib(nu)
      enddo
c----------------------------------------------------------------
c Print eigenvalues E  &  < u | r | u >
c----------------------------------------------------------------
      write(6,650) 
      do i=nbi, nbmax
         write(6,660) i-1,rmax,evib(i),expr(i)
      enddo
c----------------------------------------------------------------
c Print calculated eigenvalues E  &  "experiment" E's
c----------------------------------------------------------------
      IF (ktyp .eq. 9) GOTO 110
C---
      if (ktyp .eq. 4) then
         write(6,680) 
      else
         write(6,670) 
      endif
c-
      do i=nbi, nbmax
         dvi = eori(i)-evib(i)
         Edif = dabs( dvi )/eori(i)
         write(6,710) i-1,eori(i),evib(i),dvi,Edif
      enddo
c----------------------------------------------------------------
c Print eigenvalues &  "experiment" E's in  cm-1 .
c----------------------------------------------------------------
      if (ktyp .ge. 5) then
         write(6,720) 
      elseif (ktyp .eq. 4) then
         write(6,722) 
      endif
      do i=nbi, nbmax
         write(6,715) i-1,eori(i)*aucm,evib(i)*aucm
      enddo
c-
      if (ktyp .eq. 5 .and. Add .gt. 0.0) then
         write(6,820) Add*aucm
         do i=nbi, nbmax
            temp1 = eori(i) + Add
            temp2 = evib(i) + Add
            write(6,715) i-1,temp1*aucm,temp2*aucm
         enddo
      endif
c---
 110  write(6,725) 
      write(6,715) nbi-1,evib(nbi)*aucm
      do i=nbi+1, nbmax
         edif = evib(i) - evib(i-1) 
         if (i .gt. 2) then
            edif1 = edif0 - edif
            write(6,718) i-1,evib(i)*aucm,edif*aucm,edif1*aucm
         else
            write(6,715) i-1,evib(i)*aucm,edif*aucm
         endif
         edif0 = edif
      enddo
c-
      if (ktyp .eq. 5 .and. Add .gt. 0.0) then
         write(6,830) Add*aucm
         write(6,715) nbi-1,(evib(nbi)+Add)*aucm
         do i=nbi+1, nbmax
            edif = evib(i) - evib(i-1) 
            temp2 = evib(i) + Add
            if (i .gt. 2) then
               edif1 = edif0 - edif
               write(6,718) i-1,temp2*aucm,edif*aucm,edif1*aucm
            else
               write(6,715) i-1,temp2*aucm,edif*aucm
            endif
            edif0 = edif
         enddo
      endif
c----------------------------------------------------------------
c Calculate free wave if neccessary
c----------------------------------------------------------------
      if (kfre .eq. 1) then
         ek = dsqrt( 2.0d0*evib(nwv) )
         write(98,730) ek,evib(nwv)
         write(99,740) ek,evib(nwv)
         do i=1,nr 
            ri = rmin + (i-1)*h
            rk = ri*ek
            write(98,750) ri, cos(rk)
            write(99,750) ri, sin(rk)
         enddo
         write(6,*) " Free waves are in fort.98 & fort.99 "
      endif
c----------------------------------------------------------------
c Calculate we, wexe, weye from KNOWN E_v's if neccessary
c          v - u = i - (i-1) = 1; 
c   wexe is evaluated from E(i-1) & E(i);
c    we  is evaluated from E(i-1) & wexe;
c   weye is evaluated from Delta_E = E(i) - E(i-1);
c   weze is evaluated from E(i), we, wexe, & weye.
c----------------------------------------------------------------
      IF (kfre .eq. 2) THEN
         acm = 219474.63067
         awe = 0.0d0
         awx = 0.0d0
         awy = 0.0d0
         awz = 0.0d0
         bwe = 0.0d0
         bwx = 0.0d0
         bwy = 0.0d0
         bwz = 0.0d0
         nw1 = nwv - (nbi + 2) + 1
         nw2 = ncf - nci + 1
         write(6,760) 
c
         do i=nbi+2,nwv
            v = real(i - 1)
            z = v - 1.0d0
            dve = evib(i) - evib(i-1)
            evf = (2*v+1)/(2*z+1)
            ev1 = 0.25d0*(2*z+1)**2
            ev2 = v + 0.5d0
            wexe = 2.0d0*( evf*evib(i-1) - evib(i) )/(2*v+1)
            we  = 2.0d0*( evib(i-1) + ev1*wexe )/(2*z+1)
c--------------------------------------------------------------
c  Next line gives POOR result :
C          weye = (evib(i) - we*ev2 + wexe*ev2**2)/(ev2**3)
c---
c  Next THREE lines give POOR result :
c           ev3 = z + 0.5d0
c           ev4 = 2.0d0*(z + 1.0)
c           ev5 = 1.0 + 3.0d0*(z + 0.5d0)*(z + 1.5d0)
c---
c  Next THREE lines give POOR result :
c           ev6 = 1.0 + 4.0d0*ev3**3 + 6.0d0*ev3**2
c           ev6 = ev6 + 4.0d0*ev3
c          weze = (dve - we + ev4*wexe - ev5*weye)/ev6
c--------------------------------------------------------------
c
c-- Use energy difference to find weye :
            ev4 = 2.0d0*(v + 1.0)
            ev5 = 1.0 + 3.0d0*(v + 0.5d0)*(v + 1.5d0)
            weye = (dve - we + ev4*wexe)/ev5
c
c-- Use energy formula to find weze :
            weze = evib(i) - we*ev2 + wexe*ev2**2 - weye*ev2**3
            weze = weze/(ev2**4)
c
            awe = awe + we
            awx = awx + wexe
            awy = awy + weye
            awz = awz + weze
            write(6,765) i-1, we, wexe, weye, weze
c
            if (i .ge. nci .and. i .le. ncf) then
               bwe = bwe + we
               bwx = bwx + wexe
               bwy = bwy + weye
               bwz = bwz + weze
            endif  
         enddo
c
         awe = awe/nw1
         awx = awx/nw1
         awy = awy/nw1
         awz = awz/nw1
c
         bwe = bwe/nw2
         bwx = bwx/nw2
         bwy = bwy/nw2
         bwz = bwz/nw2
         write(6,770) nw1,awe, awx, awy, awz
         write(6,790) awe*acm, awx*acm, awy*acm, awz*acm
         write(6,780) nci-1,ncf-1,bwe, bwx, bwy, bwz
         write(6,790) bwe*acm, bwx*acm, bwy*acm, bwz*acm
      ENDIF
c----------------------------------------------------------------
      write(6,990) 
c----------------------------------------------------------------
320   format(//19x,'Information returned from SOLVESE :',/,
     #/9x'[      v -- the (v+1) state with quantum number v;     ',
     #/9x'  wronsk -- an wronskian of the  left and right waves; ',
     #/9x'   Ecorr -- estimated correction to the input energy E  ',
     #/9x'      kv -- number of iterations used for this v       ]',
     #//11x,'   (   wronsk = 0  means the derivatives of LEFT     ',
     #/11x,'     & RIGHT wavefunctions are equal and therefore   ',
     #/11x,'     the whole EIGENfunction is SMOOTH.              ',
     #/11x,'        When wronsk > 1.0, CHECK wavefunctions !   )',/,
     #/11x,'   { abs(kstd) : abs(0) == abs(Ecorr/E)         ',
     #/11x,'                 abs(1) == abs(qeng*Ecorr/E)    ',
     #/11x,'                 abs(2) == abs(E - E0 )       } ',/
     #/2x'v',4x'wronsk',7x,'Ecorr',10x,'E',11x,'E+Ecorr',
C    #/2x'v',4x'wronsk',7x,'Ecorr',10x,'E',11x,'  Q_v  ',
     #4x,'abs(kstd) kstd kv ',/)
350   format(/14x,'{ abs(kstd) : abs(0) == abs(Ecorr/E)    ',
     #/18x,'          abs(1) == abs(qeng*Ecorr/E)    ',
     #/18x,'          abs(2) == abs(E - E0 )       } ',//3x,'v',9x,
     #'E',13x,'Ecorr',8x,' abs(kstd)',4x,'kstd',6x,'vtol',)
420   format(/x,'For v = ',i3,2x,' &  eigenvalue E =',1PE18.9)
422   format(x,'After adding constant Add,  E =',1PE18.9,/)
430   format(6x,f10.5,3x,1pe16.8)
435   format(/4x,23(1H*),' NOTATION ',23(1H*),/
     #/8x,'Since Linux system admits 99 units for output ONLY,', 
     #/6x,'and we keep units 1--29 for other purpose,     all',
     #/6x,'states with quantum number v_initial >= 60 will be ',
     #/6x,'written (starting) from unit 30 & up.       So you ', 
     #/6x,'might perform your calculations in TWO stages :    ',
     #/11x,'  First,   run from v =  0 --> v = 59 ;   ',      
     #/11x,'  Second,  run from v = 60 --> v =120 .',//4x,56(1H*),/)      
440   format(3x,1pe16.8)
630   format(13x,i3,4x,i3,3x,i3,3x,i5,3x,i4)
650   format(//9x,'The energies & expectation values are :',/,
     #/5x,'v',6x,'  r  ',7x,'E_v(a.u.)',6x,'< v | r | v > ',/)
660   format(3x,i3,3x,f9.4,2x,2(1pe15.7,2x,))
670   format(//11x,'( Edif% = | Expt_v - Ecal_v |/Expt_v )',
     #/12x,'The "expt" & the calc. energies are :',/,
     #/5x,'v',5x,' "Expt_v"',6x,'Ecal_v(a.u.)',
     #6x,'Ex - Ec',10x,'Edif% ',/)
680   format(//11x,'( Edif% = | Emos_v - Ecal_v |/Emos_v )',
     #/12x,'The Morse & the calc. energies are :',/,
     #/5x,'v',5x,'"Morse_v"',6x,'Ecal_v(a.u.)',
     #6x,'Em - Ec',10x,'Edif% ',/)
c710  format(3x,i3,2x,4(1pe15.7,x,))
 710  format(3x,i3,x,2(f15.11,x),2(1pe15.7,x,))
 715  format(3x,i3,x,2(f15.6,x),2(1pe15.7,x,))
 718  format(3x,i3,x,2(f15.6,x),2x,f12.6)
720   format(//2x,'The "expt" & the calc. energies are  (cm-1) :',/, 
     #/5x,'v',5x,' "Expt_v"',6x,'Ecal_v(cm-1)',/)
722   format(//2x,'The MORSE & the calc. energies are  (cm-1) :',/, 
     #/5x,'v',5x,' "Morse_v"',5x,'Ecal_v(cm-1)',/)
725   format(//4x,'The calculated energies are  (cm-1) :',/, 
     #/5x,'      { edif = E(v) - E(v-1) } ',
     #/5x,'v',5x,'Ecal_v(cm-1)',4x,'    edif     ',2x,' Delta_edif ',/)
C    #/5x,'v',5x,'Ecal_v(cm-1)',4x,'E(v) - E(v-1)',/)
730   format(/3x,' Free wave  cos(k*r) with  k = sqrt(2E) = ',1pe15.7,
     #/41x,'E = ',1pe15.7,//3x,'  r(ao)          cos(k*r) ',/)
740   format(/3x,' Free wave  sin(k*r) with  k = sqrt(2E) = ',1pe15.7,
     #/41x,'E = ',1pe15.7,//3x,'  r(ao)          sin(k*r) ',/)
750   format( 2(1pe15.7,x) )
760   format(//12x,'Calculated sprctroscopic constants from known',
     #/13x,"        vibrational energies  E_v's : ",
     #/13x," ( we & wexe > 0; weye & weze MAY be < 0 ) ",/
     #/3x,'  v',7x,'we',12x,'wexe',11x,'weye',11x,'weze',/)
765   format(3x,i3,4(1pe15.7))
770   format(///7x,'Sprctroscopic constants AVERAGED over ',
     #i3,'  states : ',/
     #/7x,' we  = ',1pe15.7,';     wexe = ',1pe15.7,'  a.u.',
     #/7x,'weye = ',1pe15.7,';     weze = ',1pe15.7,'  a.u.',/)
780   format(/7x,'Sprctroscopic constants AVERAGED from ',
     #'state v = ',i3,/42x,"to state v'= ",i3,'  :',/
     #/7x,' we  = ',1pe15.7,';     wexe = ',1pe15.7,'  a.u.',
     #/7x,'weye = ',1pe15.7,';     weze = ',1pe15.7,'  a.u.',/)
790   format(7x,' we  = ',1pe15.7,';     wexe = ',1pe15.7,
     #'  cm-1 ',
     #/7x,'weye = ',1pe15.7,';     weze = ',1pe15.7,'  cm-1',/)
820   format(//2x,'The "expt" & the calc. energies are  (cm-1) :',/, 
     #/2x,'After adding constant Add,  Add = ',f16.5,/
     #/5x,'v',5x,' "Expt_v"',6x,'Ecal_v(cm-1)',/)
830   format(//3x,'After adding constant Add,  Add = ',f16.5,/
     #/5x,'v',5x,'Ecal_v(cm-1)',4x,'    edif     ',2x,' Delta_edif ',/)
990   format(/22x,'*****  You are done  *****',/)
c----------------------------------------------------------------
      stop 
      end
C
C=
       subroutine readin(rmesh,nbmax,ng,nr,nbi,nwv)
       implicit real*8(a-h,o-z)
c----------------------------------------------------------------
        parameter(nd=120, nd1=2*nd)
      common /rdata/ rmin,rmax,rmatch,h,ul0(nd1),ur0(nd1)
      common /pdata/ vc(10),xl(10),nv(10),Add,nvp,iel,ktyp,ms
      common /fdata/ kfre,nci,ncf
      common /shomos/ de, alpha, re, rmu, we
      common /murrell/ De0,a1,a2,a3,a4,a5,a6,a7,shift,beta
      common /SF/ alamta,Isf,amu,Nturn,an
      common /vibe/  evib(nd1),evib1(nd1),eori(nd1),expr(nd1)
      common /vcon/ qeng(nd1),vtol(nd1),dev(nd1),kcon(nd1)
      common /kmon/ kstd(nd1)
      common /vmon/ rv(20000),vnum(20000)
      dimension  rmesh(nd),ev1(nd1),ev2(nd1),fms(3500)
      dimension  ff(20), an(20)
c-----------------------------------------------------------------
c--- read in radius information
c  CAUTION about rmatch :  
c    rmatch is the matching boundary radius (e.g. rmatch~re). This
c  value should be in the classically ALLOWED region. For wave
c  functions which are ZERO at re, rmatch must be slightly off
c  re to get the converged wavefunctions !
c=================================================================
       read(5,*) kf
       read(5,*) rmin,rmax,rstep,rmatch
       read(5,*) re,rmu,iel,ktyp,Isf,kfre
       read(5,*) nvp,ng,nbmax,nbi,nwv,nci,ncf
C--- 
      write(0,320)
      write(6,350) rmin,rmax,rstep,rmatch,iel,ktyp, nvp,ng,nbmax,nbi,nwv
     $     ,nci-1,ncf-1
      write(6,360) re,re*0.529177249d0,rmu,kfre
C--- 
      write(6,390)
C--- Readin iteration #, E correction factor, & tolerences, etc. 
C---   for eigenvalue convergence
      do 10 i=1,nbmax
         read(5,*) kstd(i),kcon(i),qeng(i),vtol(i),dev(i), ul0(i),ur0(i)
         write(6,410) i-1,kstd(i),kcon(i),qeng(i),vtol(i),dev(i),ul0(i)
     $        ,ur0(i)
10    continue
C--- 
      h=rstep
      nr=int(rmax-rmin)/h
c------------------------------------------------------------
c--- read in SPHERICAL potential information
c------------------------------------------------------------
      aucm = 219474.63067d0
c-
      if (ktyp .lt. 5) then
c-----  For other spherical potential  ----
c-----  For SHO, nvp=1; Morse, nvp=2.  ----
         write(6,490) 
         do i=1,nvp
            read(5,*) nv(i),vc(i),xl(i)
            write(6,550) i,nv(i),vc(i),xl(i)
         enddo
      elseif (ktyp .ge. 6 .and. ktyp .le. 8) then
c------------------------------------------------------------
c-----  For SMS, HMS, SF potential  ----
c   The (corrected) eigenvalues of Morse potential
c are input as the first GUESS for MS potential;
c or use the experimental vibrational energies as
c the first guess.
c
c  naucm = 0, read we, wexe,... in Hartree; = 1, in cm-1.
c     ms = 3, 4, 5 : The highest power in the expansion of MS potential.
c   ntyp = 1, use Morse eigenvalues as 1st guess
c        = 2, use exp't vibrational energies as 1st guess
c        = 3, use Sun-Murrell-Sorbie energies as 1st guess
c   mall = 1, use the a1 from fort.5 (intermediate case);
c        > 1, use the a1 from fort.4 (continuous running case).
c    msv = v, the code'll generate approximate energies starting from v.
c  
c 1.) Sun's Modified Murrell-Sorbie (x=R-Re) :
c           V_SMMS(R)=-De*beta*(1/beta + a1*x + a2*x**2 
c                                 + a3*x**3 + ...)*exp(-a1*beta*x)
c
c 2.) Huxley-Murrell-Sorbie (x=R-Re) :
c           V_MS(R)=-De*(1 + a1*x + a2*x**2 + a3*x**3 + ...)*exp(-a1*x)
C
C 3.) SF (ECM) : 
C           V_ecm(R) = V_MS + Lamta(R)*delta_V(R)
C
c           V_MS:  Murrell & Sorbie potential in which the
c                 (a1,a2,a3, ...) are calculated using SF formulae.
c
c          Lamta(R)   = lamta*(x/R)*[1-exp(-lamta^2*x/R)]
c          delta_V(R) = V_MS - V_0
c                For  R < Re :
C      Nturn = 0, V_ecm(R) = V_MS  For ms = 3 ONLY ;
c                For  ms = 4, 5 :
C            = 1, V_ecm(R) = V_0,  V_0 = V_Morse   ;
C            = 2, V_ecm(R) = V_0,  V_0 = V_Rydberg ;
C            = 3, V_ecm(R) = V_0,  V_0 = V_PG      .  
c---------------------------------------------------------------
c  De0, we, wexe, weye, weze, wete, wese  are ALL in  Hartree.
c---------------------------------------------------------------
c  
         read(5,*) naucm,De0,beta,alamta,ms,ntyp,mall,msv,a1
         read(5,*) amu, Nturn
         read(5,*) we, wexe, weye, weze
         read(5,*) wete, wese
         read(5,*) Add
c  
         if (naucm .gt. 0) then
            we = we/aucm
            wexe = wexe/aucm
            weye = weye/aucm
            weze = weze/aucm
            wete = wete/aucm
            wese = wese/aucm
            Add =  Add/aucm
         endif
C---
         Dee = De0
         De1 = De0*beta
C---
         write(6,414) we,we*aucm,wexe,wexe*aucm,weye,weye*aucm, weze
     $        ,weze*aucm,wete,wete*aucm,wese,wese*aucm,Nturn
C---
         if (Add .gt. 0.0) then
            write(6,416) Add, Add*aucm
         endif
C---
C         if (mall .gt. 1) read(4,*) a1
C         an(1) = a1
C---
         do i=1,nbmax
	    amv=dfloat(i-1) 
	    bmv=amv + 0.5d0
            evib(i)=we*bmv - wexe*bmv**2 + weye*bmv**3 + weze*bmv**4
            evib(i)=evib(i) + wete*bmv**5 + wese*bmv**6 
C--- Herzberg, P. 69 :
C             temp =we*(amv+0.5d0) - wexe*(amv+0.5d0)**2
C           evib(i)=temp + weye*(amv+0.5d0)**3 + weze*(amv+0.5d0)**4
C
            eori(i)=evib(i)
         enddo
C---
         write(6,420)  ntyp
         write(6,440)  0,eori(1)
         write(19,445) eori(1)
         do i=2,nbmax
            dvi = eori(i) - eori(i-1)
            write(6,440) i-1,eori(i),dvi
            write(19,445) eori(i)
         enddo
         write(19,450)
C===
         if (ntyp .eq. 3) then
C--- Readin initial (numerical) guesses for energies
            do i=1,nbmax
               read(5,*) evib1(i)
            enddo
            write(6,470)
            write(6,440)  0,evib1(1)
            expr(1) = 1.0d0
C---
            do i=2,nbmax
               ev1(i) = evib1(i) - evib1(i-1)
               expr(i) = abs( ev1(i) )/evib1(i)
               perc = expr(i)/expr(i-1)
C-
C  If wanted, generate suggested (trial) energies ev2(i) :
               if (msv .gt. 0 .and. i .ge. msv) then
                  temp = ev1(i-1)*( 1.0d0 - expr(i-1) )
                  ev2(i) = evib1(i-1) + temp
               endif
C-
               write(6,440) i-1,evib1(i),ev1(i),expr(i),perc
            enddo
C---
            write(6,475)
            do i=1,nbmax
               dvi = evib(i) - evib1(i)
               write(6,440) i-1,evib(i),evib1(i),dvi
               evib(i)=evib1(i)
            enddo
c
            if (msv .gt. 0) then
               write(6,480)
               do i=msv,nbmax
                  write(6,440) i-1,eori(i),ev2(i)
                  write(19,445) ev2(i)
               enddo
            endif
C---
         endif
C===
         if (ntyp .eq. 4) then
c  Write out "experimental" constants :
            write(6,452) we, wexe, weye, weze, wete, wese
C---
c Read we, ... DETERMINED by code from previous job using
c calculated vibrational energies E_v's.
C---
            read(5,*) we, wexe, weye, weze
            read(5,*) wete, wese

            if (naucm .gt. 0) then
               we = we/aucm
               wexe = wexe/aucm
               weye = weye/aucm
               weze = weze/aucm
               wete = wete/aucm
               wese = wese/aucm
            endif
c
            do i=1,nbmax
               amv=dfloat(i-1) 
               bmv=amv + 0.5d0
               evib(i)=we*bmv - wexe*bmv**2 + weye*bmv**3 + weze*bmv**4
C           evib(i)=evib(i) + wete*bmv**5 + wese*bmv**6 
            enddo
            write(6,455)  we,wexe,weye,weze,0,evib(1)
            do i=2,nbmax
               dvi = evib(i) - evib(i-1)
               write(6,440) i-1,evib(i),dvi
               write(19,445) evib(i)
            enddo
         endif
C===
c------------------------------------------------------------
c   Prepare CONVERSION factors :
c------------------------------------------------------------
         utoh = 3.42317725E+07
         cmao = 5.29177249E-09
         aoA1 = 1.889725989d0
         aoA2 = aoA1*aoA1
         aoA3 = aoA2*aoA1
         aoA4 = aoA3*aoA1
         aoA5 = aoA4*aoA1
c--- Prepare Prefactors :
         ss0 = 0.021657164d0
c------------------------------------------------------------
C Evaluate Sun's modified MS expansion coefficients :
c------------------------------------------------------------
C-
         if (Isf .eq. 1) then
C---For ECM V_ecm(R) :
            do i=1,ms
               read(4,*) an(i)
            enddo
         else if (Isf .eq. 0) then
C--- For SMS
            f2=1.000004496d0*ss0*(utoh*rmu)*(aucm*we*cmao)**2
            ff(2) = f2
            an(2) = 0.5d0*(a1**2 - f2/De1)
C--- Sun's formulae :
            do k=3,ms+1
               temp = (- 1.0)**k * 2.0d0*(k-1)
               temp = temp*dsqrt(f2**k)/facx(k)
               ff(k) = temp/(De1**(k/2.0d0 - 1.0) )
            enddo
C
            if ( kf .eq. 1 ) then 
               do k = 2,ms+1
                  read(13,*)ff(k)
               enddo
               ff(2) = f2
               an(2) = 0.5d0*(a1**2 - f2/De1)
            endif
c---
            do k=3,ms
               temp = - 0.5d0*ff(k)/De1 + (-a1)**k *(k-1)/facx(k)
               do i=2,k-1
                  temp = temp - (-a1)**(k-i)*an(i)/facx(k-i)
               enddo
               an(k) = temp
            enddo  
c---
            write(6,460) De0,De0*aucm,beta,De1,De1*aucm
            do k=1,ms
               write(6,462) k, an(k), k, an(k)*aoA1**k , k
            enddo  
c-
            write(6,464) 
            do k=2,ms+1
               write(6,466) k, ff(k), k
            enddo
c---------------------------------------------------------------
c           f3=-2.0d0*dsqrt(f2**3/De1)/3.0d0
c           f4=0.25d0*f2**2/De1
c           f5=-dsqrt(f2**5)/(15.0d0*dsqrt(De1**3) )
c         a2=0.5d0*(a1**2 - f2/De1)
c         a3=a1*a2 - a1**3/3.0d0 - 0.5d0*f3/De1     
c         a4=0.0
c         a5=0.0
c         a6=0.0
c         a7=0.0
c           if (ms .gt. 3) then
c             a4=a1*a3 - 0.5d0*a1**2*a2 + a1**4/8.0d0 
c             a4=a4 - 0.5d0*f4/De1     
c             a5=a1*a4 - 0.5d0*a1**2*a3 + a1**3*a2/6.0d0
c             a5=a5 - a1**5/30.0d0 - 0.5d0*f5/De1     
c           endif
c---------------------------------------------------------------
c         write(6,465) De0,De0*aucm,beta,De1,De1*aucm,a1,a1*aoA1,
c    # a2,a2*aoA2,a3,a3*aoA3,a4,a4*aoA4,a5,a5*aoA5,we,we*aucm,
c    # wexe,wexe*aucm,weye,weye*aucm,weze,weze*aucm,
c    # wete,wete*aucm,wese,wese*aucm,
c    # f2,f3,f4,f5
c---------------------------------------------------------------
         endif
      endif
c------------------------------------------------------------
c  Calculate SHO energies if neccessary.
c------------------------------------------------------------
      if (ktyp .eq. 3) then
         we = vc(1)
         wexe = xl(1)
         write(6,560) we
         do m = 1,nbmax
            amv = dfloat(m-1)
            evib(m) = we*(amv+0.5d0)
            eori(m) = evib(m)
         enddo
         write(6,440) 0,evib(1)
         do i = 2,nbmax
            dvi = evib(i) - evib(i-1)
            write(6,440) i-1,evib(i),dvi
         enddo
      endif
c------------------------------------------------------------
c    Calculate Morse energies if neccessary.
c  When nv(1)=0, code calculates we, wexe for given de, alpha, rmu, & re;
c            =1, code calculates de, alpha for given we, wexe, rmu, & re;
c            =2, code calculates wexe, alpha for given de, we, rmu, & re;
c            =3, code calculates alpha (ONLY) for given wexe, rmu,  & re.
c  nv(1) = 3 may be UNphysical since  wexe is DETERMINED by De & we,
c  then wexe DETERMINES alpha.  wexe MUST be consistent with De, OR
c  you may get UNphysical (& unconverged) results.
c
c------------------------------------------------------------
      if (ktyp .eq. 4) then
         de=vc(1) 
         alpha=xl(1)
         we=vc(2)
         wexe=xl(2)
         Dee = de
c
         if (nv(1) .eq. 0) then
            we=(alpha/re)*dsqrt(2.0d0*de/rmu)
	    wexe=(alpha/re)**2/(2.0d0*rmu)
         elseif (nv(1) .eq. 1) then
            de=(we**2)/(4.0d0*wexe)
            alpha=re*dsqrt(2.0d0*rmu*wexe)
         elseif (nv(1) .eq. 2) then
            wexe=(we**2)/(4.0d0*de)
            alpha=re*dsqrt(2.0d0*rmu*wexe)
         elseif (nv(1) .eq. 3) then
            alpha=re*dsqrt(2.0d0*rmu*wexe)
         endif
c
         write(6,570) nv(1),de,alpha,we,wexe
         do m=1,nbmax
	    amv=dfloat(m-1)
            evib(m)=we*(amv+0.5d0) - wexe*(amv+0.5d0)**2
            eori(m)=evib(m)
         enddo

C--- Readin initial (numerical) guesses for energies
C *          do i=1,nbmax
C *             read(5,*) evib1(i)
C *             evib(i) = evib1(i)
C *             eori(i) = evib(i)
C *          enddo

         write(6,440) 0,evib(1)
         do i=2,nbmax
            dvi = evib(i) - evib(i-1)
            write(6,440) i-1,evib(i),dvi
         enddo
      endif
c-----------------------------------------
      if (nwv .lt. nbmax) nbmax = nwv
c---------------------------------------------------------------
c Calculate the "relative" dissociation energy  D_o
c---------------------------------------------------------------
      if (ktyp .ge. 4 .and. ktyp .lt. 9) then
         D00 = Dee - 0.50d0*we + 0.25d0*wexe - weye/8.0d0
         write(6,520) D00, D00*27.21139618
      endif
c---------------------------------------------------------------
c   Find shift constant of the Sun-Murrell-Sorbie/Morse
c potential such that   shift = - V_minimum  .
c---------------------------------------------------------------
      if (ktyp .lt. 9) then
c
         if (ktyp .ge. 6 .and. ktyp .le. 8) then
            write(8,572)
         elseif (ktyp .eq. 4) then
            write(8,574)
         endif
c---
         shift=0.0d0
         re1=re/2.0d0
         xstep=re/400
         R=re1 - xstep
         do 20 i=1,2000
            R=R + xstep
            fms(i)=potf(R)
 20      continue
         do i=2,3000
            if (fms(i) .lt. fms(i-1)) shift = fms(i)
         enddo
         if (shift .lt. 0.0) shift = - shift
         write(6,580) shift
c
         R=re1 - xstep
         do i=1,3000
            R=R + xstep
            if (ktyp .eq. 4) fms(i)=fms(i) - de
            write(8,576) R, fms(i)
         enddo
c
      endif
      rlast = rmin + (nr-1)*rstep
      rlas0 = rlast
c--------------------------------------------------------------
      write(6,600) 
      if (ktyp .ge. 4 ) then
         ng1 = ng
      else if (ktyp .lt. 4) then
         ng1 = 3
      endif
      do 100 i=1,ng1
         read(22,*) rmesh(i)
         write(6,610) i, rmesh(i)
 100  continue
c---------------------------------------------------------------
c If wanted, read in NUMERICAL potentials :
c
c    mpu -- 0, read POT in Hartree; = 1, in eV; = 2, in cm-1.
c     nr -- The # of potential points to be read
c    ri0 -- The first radial value of the numerical potential
c  rste1 -- The step length of the numerical potential
c---------------------------------------------------------------
      if (ktyp .eq. 9) then
         auev = 27.21139618d0
         read(5,*) mpu,mr,ri0,rste1
         do i=1,nbmax
            read(5,*) evib(i)
         enddo
         do i=1,mr
            read(20,*) rv(i), vnum(i)
            if (mpu .eq. 1) then
               vnum(i) = vnum(i)/auev
            elseif (mpu .eq. 2) then
               vnum(i) = vnum(i)/aucm
            endif
         enddo
         h = rste1
         rmin = ri0
         if (rv(mr) .lt. rmax) then
            rmax = rv(mr)
            rlast = rmax
         endif
         if (mr .lt. nr) nr = mr
         write(6,590) ri0,rste1
      endif
c---------------------------------------------------------------
c Wavefunctions will be interpolated on the following r-mesh 
c---------------------------------------------------------------
      write(6,595) nr, rlast
c-
      write(6,800) 
c============================================================
 200  format(3x,2i5)
 210  format(3x,1pe15.6)
 320  format(/10x,"*** Using Sun's energy-consistent method ***",
     #/8x,'  ( Chengdu Sichuan 610065  P.R.China;  1996 )',/
     #/6x,"ALWAYS CHECK the wavefunction of the HIGH  v states;",
     #/6x,"If E is NOT an EIGENvalue, the WAVE has WRONG nodes.",/
     #/6x,"When the  HIGHLY  excited states are  NOT converged,",
     #/6x,"try to  INCREASE  rmax,     OR change the values of ",
     #/6x,"parameters :         kstd,  kcon,  vtol,  &   dev ",/
     #/6x,"When get 'Floating exception',  CHANGE rmin or rmax.",
     #/6x,"   SMALL rmin may cause 'floating exception' ; ",
     #/6x," For LOW  v's, LARGE rmax may cause 'floating ...'. ",
c    #/6x,"  For HIGH v's, SMALL rmax may cause WRONG nodes .  ",
     #//2x,"If E_try > De, one gets 'Floating exception' ",
     #"too (CONTINUUM)",
     #//7x,"Chose 'ntyp=3' & modify E's in 'Evib' for HIGH v's"/)
 350  format(///10x,'*** Solving Schrodinger equation for ***',/,
     #13x,'   bound SPHERICAL potentials ',/,
     #/17x,'{ Using Numerov method }',/,
     #/3x,'The minimum radius of the r-mesh,      rmin = ',f9.4,
     #/3x,'The maximum radius of the r-mesh,      rmax = ',f9.4,
     #/3x,'The step lenth of radius r,           rstep = ',f9.4,
     #/3x,'The matching boundary radius (C.A.), rmatch = ',f9.4,
     #/3x,'The angular momentum quantum number L,  iel = ',i4,/
     #/3x,'[ ktyp=1, Gaussion or Slater; =2, Exponential; ',
     #/3x,'  ktyp=3, Simple Harmonic   ; =4, Morse        ',
     #/3x,'  ktyp=5, Screened central potential             ',
     #/3x,'  ktyp=6, SMS, Sun-Murrell-Sorbie potential     ',
     #/3x,'  ktyp=7,  MS, Huxley-Murrell-Sorbie potential  ',
     #/3x,'  ktyp=8,  SF, ECM potential                    ',
     #/3x,'  ktyp=9, Use NUMERICAL potentials            ] ',
     #/3x,'Type of central  SPHERICAL potentials, ktyp = ',i3,/
     #/3x,'The # of terms in potential expansion,  nvp = ',i3,
     #/3x,'    ( nvp is NOT used for ktyp = 5,6,7,8,9 )     ',/
     #/3x,'The # of r-mesh used for interpolation,  ng = ',i3, 
     #/3x,'(Read-in) maximum # of bound states,  nbmax = ',i3,
     #/3x,'The # of INITITAL bound state used,     nbi = ',i3,
     #/3x,'The # of  FINAL   bound state used,     nwv = ',i3,
     #/3x,'  [ If nbmax > nwv,  code sets nbmax=nwv ]    ',/
     #/3x,'The INITITAL state to AVERAGE we, ... , v_i = ',i3,
     #/3x,'The  FINAL   state to AVERAGE we, ... , v_f = ',i3,)
 360  format(/3x,'The equilibrium internuclear distance, re =',
     #f10.6,'  ao = ',f10.6,' A',
     #/3x,'The reduced mass of linear molecule,  rmu =',f14.5,
     #//3x,' ( kfre = 0, do NOT cal. FREE wave;',
     # /3x,'        = 1, calculate FREE wave  ;',
     # /3x,"        = 2, cal. we ... from E_v's )  kfre =",i3,/)
 390  format(/14x,'Parameters for eigenvalues & wavefunctions :',/
     #/11x,'      [ Switch for energy convergence :',
     #/11x,'          kstd = 0, eq1=abs(Ecorr/E)     ; ',
     #/11x,'               = 1, eq1=abs(qeng*Ecorr/E); ',
     #/11x,'               > 1, eq1=abs(E - E0 )     . ] ',//
     #10x,'      { kcon -- iteration numbers for E_v  ; ',/
     #10x,'        qeng is in  E_v = E_v + qeng*Ecorr ; ',/
     #10x,'        vtol -- tolerances for E_v         ; ',/
     #10x,'         dev -- in   E_v = E_v + Ecor*dev  ; ',/
     #10x,'         ul0 -- values of LEFT  functions  ; ',/
     #10x,'         ur0 -- values of RIGHT functions  ; ',/
     #10x,'           ( qeng is used for kstd >= 1 )    ',/
     #10x,'           (  dev is used for ALL kstd  )    ',/
     #10x,'        ( both qeng & dev may be negative ) }',//
     #'        Iteration Ecorrection  Tolerance',15x,'Left',
     #7x,'Right',/
     #'  v kstd  kcon(v)   qeng(v)     vtol(v)     dev(v)',
     #5x,'ul0(v)',5x,'ur0(v) ',/)
 410  format(i3,2x,i2,2x,i5,2x,1pe13.5,1pe12.4,3(1pe11.3))
 414  format(///3x,'The vibrational constants USED are : ',
     #/6x,'            Hartree          cm-1  ',
     #/6x,'    we =',2(1pe15.8,x),"  for both a's & E(v) ",
     #/6x,'  wexe =',2(1pe15.8,x),"  for E(v) ONLY ",
     #/6x,'  weye =',2(1pe15.8,x),"  for E(v) ONLY ",
     #/6x,'  weze =',2(1pe15.8,x),"  for E(v) ONLY ",
     #/6x,'  wete =',2(1pe15.8,x),"  for E(v) ONLY ",
     #/6x,'  wese =',2(1pe15.8,x),"  for E(v) ONLY ",/
     #/3x,'[           For  R < Re :               ',
     #/3x,'  Nturn = 0, V_ecm(R) = V_MS  for ms = 3 ONLY ; ',
     #/3x,'            For  ms = 4, 5 :                    ',
     #/3x,'        = 1, V_ecm(R) = V_0,  V_0 = V_Morse   ; ',
     #/3x,'        = 2, V_ecm(R) = V_0,  V_0 = V_Rydberg ; ',
     #/3x,'        = 3, V_ecm(R) = V_0,  V_0 = V_PG      . ] ', 
     #/3x,'                Nturn = ',i2,/)
 416  format(///3x,'The energy shifting constant for electronic',
     #' EXCITED state is : ',
     #/3x,'      Add = ',f16.9,' a.u.',
     #/3x,'      Add = ',f16.5,' cm-1',/)
 420  format(//5x,"{ Switches for INITIAL energy guesses : ",
     #/5x,"   ntyp = 1, use Morse eigenvalues  ; ",
     #/5x,"        = 2, use exp't vib. energies; ",
     #/5x,"        = 3, use SMS or modified exp't E; ",
     #/5x,"        = 4, use we, ... GIVEN by code.   }  ntyp =",i3,
     #///5x,"The 'experiment' energies of the molecule : ",
     #/5x,'( They are also written into fort.19 )',
     #//7x,'v',8x,'E(v)',9x,'E(v)-E(v-1)',/)
 440  format(5x,i3,2x,4(1pe15.7,x))
 445  format(1pe20.12)
 450  format(/x,'The ABOVE are the INITIAL (experiment) energies',
     #///x,'Below are the suggested (trial) energies (Es) :',/
     #'( You may shift Es around the suggested value a little )',/)
 452  format(//11x,"The 'experiment' vibrational constants : ",
     #/11x,'    we =',1pe14.6,"  for exp't E(v) ",
     #/11x,'  wexe =',1pe14.6,"  for exp't E(v) ",
     #/11x,'  weye =',1pe14.6,"  for exp't E(v) ",
     #/11x,'  weze =',1pe14.6,"  for exp't E(v) ",/)
 455  format(/11x,'The code-determined constants are :',
     #/11x,'    we =',1pe14.6,"  for both a's & E(v) ",
     #/11x,'  wexe =',1pe14.6,"  for E(v) ONLY ",
     #/11x,'  weye =',1pe14.6,"  for E(v) ONLY ",
     #/11x,'  weze =',1pe14.6,"  for E(v) ONLY ",/
     #/7x,'which generates following energies : ',
     #//7x,'v',8x,'E(v)',9x,'E(v)-E(v-1)',/
     #5x,i3,2x,4(1pe15.7,x))
 460  format(//3x,'The parameters in Sun-Murrell-Sorbie potential :',
     #/5x,'                ( ktyp=5 ) ',/
     #/5x,'V(r) = - De*beta*(1/beta + a1*r + a2*r**2 + a3*r**3 ',
     #/5x,'           + a4*r**4 + a5*r**5 + ... )*exp(-a1*beta*r)',
     #//5x,'      De =',f11.8,'   Hartree    = ',f12.4,' cm-1 ',
     #//5x,'    Beta =',f9.5,
     #//5x,' De*Beta =',f11.8,'   Hartree    = ',f12.4,' cm-1 ',/)
 462  format(9x,'a(',i1,') =',1pe14.6,' 1/(ao**',i1,') =',
     #1pe14.6,' 1/(A**',i1,')',)
 464  format(//2x,'The f coefficients used to solve for a(k) are :'/)
 466  format(9x,'f(',i1,') =',1pe14.6,'  Har/(ao**',i1,')',)
 470  format(//5x,'The inputted energies for ABOVE potential :',
     #/11x,'( Edif% = | E(v) - E(v-1) |/E(v) )',/
     #/7x,'v',8x,'E(v)',9x,'E(v)-E(v-1)',7x,'Edif%',
     #7x,'Edif_v/Edif_v-1',/)
C    #/7x,'v',8x,'E(v)',9x,'E(v)-E(v-1)',/)
 475  format(//20x,'The vibrational energies :',/
     #/12x,'"Experiment"',6x,'Inputted',
     #/7x,'v',8x,'Ee(v)',11x,'Ei(v)',8x,'Ee(v)-Ei(v)',/)
 480  format(//10x,'The suggested (trial) energies :',/
     #3x,'( You may shift Es around the suggested value a little )',/
     #/12x,'"Experiment"',6x,'Suggested',
     #/7x,'v',8x,'Ee(v)',11x,'Es(v)',/)
 490  format(/3x,'The INPUT parameters in potentials of :',/
     #/5x,'ktyp=1; V(r) => vc(i)*r**nv(i)*exp(-xl(i)*r**2) ; OR',
     #/5x,'        V(r) => vc(i)*r**nv(i)*exp(-xl(i)*r) ',
     #/5x,'    =2; V(r) => vc(i)*exp(-xl(i)*r) ',
     #/5x,'    =3; V(r) => 0.5*[rmu*vc(i)**2]*r**2; we=vc. ',
     #/5x,'    =4; V(r) => De*[exp(-2*a*r) - 2*exp(-a*r)] + De',
     #/5x,'            De = vc(1),   a  = xl(1) for ktyp = 4 ,',
     #/5x,'            we = vc(2), wexe = xl(2) for ktyp = 4 .',
     #//3x,'  i  nv(i)',7x,'vc(i)',11x,'xl(i)',/)
 520  format(//x,"The 'relative' dissociation energy D_o : ",
     #/7x,"D_o = D_e - we/2 + wexe/4 - weye/8 = ",
     #f10.6,' a.u. ',/42x,'= ',f10.6,' eV',/)
 550  format(3x,i3,i5,3x,2(e15.8,2x))
 560  format(//5x,'===  Using Simple Harmonic Potentials ===',/,
     #/3x,'The vibrational constant  we = ',D16.8,/
     #/3x,'The (starting energies) eigenvalues of the ',
     #'above potential :',//7x,'v',8x,'E(v)',9x,'E(v)-E(v-1)',/)
 570  format(//8x,' { nv(1) = 0, code calculates  we,  wexe ; ',/
     #8x,'         = 1, code calculates  De,  alpha; ',/
     #8x,'         = 2, code calculates wexe, alpha; ',/
     #8x,'         = 3, code calculates alpha  ONLY. }  nv(1) =',i3,/
     #/3x,'The parameters USED for MORSE potential : ',
     #/11x,'De, Alpha = ',2(f9.6,2x),/,
     #/3x,'The vibrational constants USED for Morse energies :',/,
     #8x,'   we,  wexe =',2(1PE16.8,x),/,
     #/3x,'The (starting energies) eigenvalues of the ',
     #'above potential :',//7x,'v',8x,'E(v)',9x,'E(v)-E(v-1)',/)
 572  format(//3x,'The MS type (SMS,HMS,ECM) potential',
     #' before shifting :',
     #/x,'( To find   shift = - V_minimum; here the potential ',
     #/x,'    has NO relations with inputted  r_min  & r_max  )',
     #//4x,'    R            V(R)  ',/)
 574  format(//3x,'The Morse potential AFTER shifting :',
     #/x,'( shift = - De;  here the potential has NO',
     #/x,'    relations with inputted  r_min  & r_max  )',
     #//4x,'    R            V(R)  ',/)
 576  format(2x,f10.5,2x,1pe16.8)
 580  format(/' Calculated shifting constant of the SMS potential',
     #' is, ',/34x,'shift =',1pe14.6)
 590  format(//3x,'--- Using inputed NUMERICAL potential ---',/
     #/3x,'The minimum radius of V_num.(r),   rmin  = ',f9.5,
     #/3x,'The r_step length  of V_num.(r),  r_step = ',f9.5,)
 595  format(/3x,'The total number of r-mesh points,  nr =',i6,/
     #/3x,'The LAST radial point is at,    r_last =',f9.5,//)
 600  format(' r-mesh points used to interpolate wavefunctions :',
     #/,'     i           r-mesh(i) ',/)
 610  format(4x,i2,6x,f15.9)
 800  format(///23x,' *****  THE OUTPUT  ***** ')
      return
      end
C
C=
      subroutine boundv(nbi,ku,nu,Q,u)
      implicit real*8(a-h,o-z)
c----------------------------------------------------------------
c   boundv (call solvese -->) solves Schrodinger Equation & get 
c the numerical wavefunction u.
c
c Input:
c   ku -- the unit to write convergence information.
c   nu -- is the nu_th (vibrational) state with quantum number (nu-1).
c
c Output:
c    Q -- the eigenvalue of the nu_th vibrational state.
c    u -- the nu_th eigenvector.
c----------------------------------------------------------------
      parameter(nd=120, nd1=2*nd)
      dimension u(20000)
      common /rdata/ rmin,rmax,rmatch,h,ul0(nd1),ur0(nd1)
      common /pdata/ vc(10),xl(10),nv(10),Add,nvp,iel,ktyp,ms
      common /shomos/ de, alpha, re, rmu, we
      common /vibe/  evib(nd1),evib1(nd1),eori(nd1),expr(nd1)
      common /vcon/ qeng(nd1),vtol(nd1),dev(nd1),kcon(nd1)
      common /kmon/ kstd(nd1)
      common /vmon/ rv(20000),vnum(20000)
c----------------------------------------------------------------
c  Get the bound state energies for a given state nu
c    Q = evib(nu) = E_v
c----------------------------------------------------------------
      kv = 0
      kp = 0
      Q0 = 0.0
c----------------------------------------------------------------
      if (nu .eq. nbi .and. ktyp .ge. 98) then
         write(6,310) 
         stop
      endif
c----------------------------------------------------------------
 5    Q = evib(nu)
c----------------------------------------------------------------
c parameters for the wavefunction solution
c----------------------------------------------------------------
 8    ul1 = ul0(nu)
      ur1 = ur0(nu)
c---
      ul2 = ul1 * ((rmin+h)/rmin)**(iel+1)
c
      if (ktyp .lt. 9) then
         ur2 = ur1 * dexp( dsqrt( h*h/2.0d0 * (potf(rmax) - Q) ) +
     $        dsqrt( h*h/2.0d0 *(potf(rmax-h) - Q) ) )
      elseif (ktyp .eq. 9) then
         ur2 = ur1 * dexp( dsqrt( h*h/2.0d0 * (vnum(rmax) - Q) ) +
     $        dsqrt( h*h/2.0d0 *(vnum(rmax-h) - Q) ) )
      endif
c----------------------------------------------------------------
c qeng -- the input correction factor for energy convergence;
c kcon -- the input iteration  number for energy convergence.
c   now solve SE & get the numerical wavefunction
c----------------------------------------------------------------
 10   call solvese(potf, Q, ktyp, iel, rmu, rmin, rmatch, rmax, h, ul1,
     $     ul2, ur1, ur2, wronsk, Ecorr, u )
c---
c=== Sun's modification ===
      if (kstd(nu) .eq. 0) then
C--- IMPORTANT :  Next line is ORIGINAL ---
         eq1=dabs(Ecorr/Q)
c---
      elseif (kstd(nu) .eq. 1) then
         eq1=dabs(qeng(nu)*Ecorr/Q)
      elseif (kstd(nu) .gt. 1) then
         eq1=dabs( Q - Q0 )
      endif
      Q0 = Q
c----------------------------------------------------------------
      kv = kv + 1
      Ecor = Ecorr
c--- 
      write(ku,25) nu-1,Q,Ecorr,eq1,kstd(nu),vtol(nu)
C     IF ( eq1 .gt.  tol ) THEN
      IF ( eq1 .gt. vtol(nu) .and. kv .le. kcon(nu) ) THEN
c---
         if (kstd(nu) .ge. 1) then
            Ecor = qeng(nu)*Ecorr 
         endif
         Q = Q + Ecor
         qdif = Q - Q0
c---
C             qdif = Q - evib(nu)
C               Q1 = Q
c---
         if (qdif .gt. 0.0 .and. Ecor .gt. 0.0) then
            Q = Q - Ecor + Ecor*dev(nu)
         elseif (qdif .lt. 0.0 .and. Ecor .lt. 0.0) then
            Q = Q - Ecor - Ecor*dev(nu)
C--- Next one produces degenerate states :
C               Q = Q - Ecor + Ecor*dev(nu)
C
C--- Next condition is unlikely TRUE :
         elseif (qdif .gt. 0.0 .and. Ecor .lt. 0.0) then
            Q = Q - Ecor - Ecor*dev(nu)
C--- Next condition is unlikely TRUE :
         elseif (qdif .lt. 0.0 .and. Ecor .gt. 0.0) then
            Q = Q - Ecor + Ecor*dev(nu)
C       write(28,*) "  v; Q1; Q; Ecor*dev = ",nu-1,Q1,Q,Ecor*dev(nu)
c---
         endif
c---
         go to 10
c---
      ENDIF
c---
      write(6,20) nu-1,wronsk,Ecor,evib(nu),Q,eq1,kstd(nu),kv-1
c=== End modification ===
c---
      evib(nu)=Q    
c----------------------------------------------------------------
 20   format(i3,1pe12.4,3(1pe14.6),1pe10.3,2x,i2,i4)
 25   format(x,i3,x,3(1pe16.8),2x,i2,x,1pe14.6)
 90   format(1pe20.12)
c---
310   format(////5x,55(1H*),/
     #/10x,"     I STEAL Dr. Weiguo Sun's program ",/
     #/10x,"        I realized this is ILLEGAL ",/
     #/10x,"I am willing to get any PENALTIES I deserve ! ",/
     #/5x,55(1H*),/)
c----------------------------------------------------------------
      return
      end
C
C
      subroutine expectwf(kw,nr,nu,ng,nbi,nwv,h,rmin,Q,u,
     # rmesh,wf,expr)
      implicit real*8(a-h,o-z)
c----------------------------------------------------------------
c   expectwf returns the vibrational wavefunction on a given
c r-mesh and the expectation value of r.
c Atomic units throughout (energies in hartrees, not in rydbergs!)
c
c Input:
c   kw -- specifies the unit to write wavefunctions.
c   nr -- is the total number of radius r points.
c   nu -- is the nu_th (vibrational) state with quantum number (nu-1).
c   ng -- is the number of r points on which the wavefunction is desired.
c  nwv -- write wavefunctions to unit 9  for v =< nwv.
c    Q -- the eigenvalue of the nu_th vibrational state.
c    h -- the step length of the radius.
c rmin -- the (input) minimum of the radius.
c    u -- the nu_th vibrational eigenvector.
c rmesh -- is the r (vector) points.
c
c Output:
c    wf -- is the vector of wavefunction on the rmesh
c  expr -- is the expectation value of r :  < u | r | u >
c----------------------------------------------------------------
        parameter(nd=120, nd1=2*nd)
      common /pdata/ vc(10),xl(10),nv(10),Add,nvp,iel,ktyp,ms
      dimension rmesh(nd),wf(nd),expr(nd1),u(20000),nbb(20)
      dimension potv(10000)
c----------------------------------------------------------------
c Find the expectation value of r :  < v | r | v >
c           ri = (i-1)*h + rmin
c----------------------------------------------------------------
          aucm = 219474.63067
        expect = 0.0d0
      nb = int( (nwv - nbi + 1)/10 ) + 2
          nbb(1) = nbi 
          nbb(2) = nbi + 9
        if (nb .ge. 3) then
          do i=3,nb
            nbb(i) = nbb(i-1) + 10
          enddo
        endif
      do i = 2, nr-1
        expect = expect + h*u(i)*u(i)*((i-1)*h + rmin)
      enddo
        expr(nu) = expect + h/2.0d0 * ( u(1)*u(1) + u(nr)*u(nr) )
c----------------------------------------------------------------
c If use NUMERICAL potential, then write out E & wavefunctions
c----------------------------------------------------------------
      IF (ktyp .eq. 9) GOTO 10
c----------------------------------------------------------------
	if (nu .eq. nbb(1)) then
            nbf = nbb(2)-2
            if (nbf .gt. nwv-1) nbf = nwv - 1
              write(9,60) nbb(1)-1, nbf, nr
	      write(17,40) ms
	      write(18,42) ms
	endif
c---
          nk = 8
      do i=2,nb
	if ( nu .eq. nbb(i) ) then
             nj = nk + i
            nbf = nbb(i+1)-2
            if (nbf .gt. nwv-1) nbf = nwv - 1
              write(nj,60) nbb(i)-1, nbf, nr
	endif
      enddo
c---
c Write out eigenvalue :
c---
  10    if (Add .eq. 0.0) then
          if (kw .le. 90)    write(kw,30) nu-1,Q
	else
            Q2 = Q + Add
          if (kw .le. 90) write(kw,32) nu-1,Q,Q2,Q2*aucm
	endif
c-
c Write out eigenfunction :
c-
      do 15 i = 1, nr
	ri=rmin+(i-1)*h
	  if (kw .le. 90)    write(kw,50)  ri, u(i)
c---
          IF (ktyp .eq. 9) GOTO 15
c---
        do k=1,nb-1
	  if (nu .ge. nbb(k) .and. nu .lt. nbb(k+1) ) then
                 nj = nk + k
            if (nu .le. nwv+1) write(nj,50)  ri, u(i)
	  endif
        enddo
c-
	if (nu .eq. nbi) then
	  write(17,50) ri, potf(ri)
	  write(18,52) ri, potf(ri)*aucm
	    potv(i) = potf(ri)
	endif
  15  continue
          IF (ktyp .eq. 9) GOTO 20
c----------------------------------------------------------------
c   Now using linear interpolation to get wavefunction at rmesh 
c points. You can get more sophisticated if needed.
c----------------------------------------------------------------
      do j=1,ng
	r = rmesh(j)
	i = (r-rmin)/h + 1
	ri = rmin +(i-1)*h
	wf(j) = u(i) + ( u(i+1)-u(i) ) * (r -ri)/h
      enddo
c----------------------------------------------------------------
      if (nu .eq. nbi) then
	write(15,44) ms, ms-2
	write(16,40) ms-2
	  ms = ms - 2
            if (Add .gt. 0.0) then
  	      write(17,70) Add
  	      write(18,80) Add*aucm
            endif  
c-
c Write out potential difference & potential :
c-
        do i = 1, nr
	  ri=rmin+(i-1)*h
	    temp0 = potv(i) - potf(ri)
  	  write(15,50) ri, temp0
  	  write(16,50) ri, potf(ri)
            if (Add .gt. 0.0) then
                tempv = potv(i) + Add
  	      write(17,50) ri, tempv
	      write(18,52) ri, tempv*aucm
            endif  
        enddo
	  ms = ms + 2
C       write(15,*) '  ms, ms-2 =',ms, ms-2
      endif
c----------------------------------------------------------------
 20   continue
c----------------------------------------------------------------
 30   format(/x,'For v = ',i3,2x,' &  eigenvalue E =',1d18.9,' a.u.',
     #//4x,'r(a0)  ',3x,'Wavefunction(r)',/)
 32   format(/x,'For v = ',i3,2x,' &  eigenvalue E =',1d18.9,' a.u.',
     #/x,'After adding constant,      E =',1d18.9,' a.u.',
     #/x,'                            E =',1f18.9,' cm-1',
     #/x,'Adding do NOT change wavefunctions ',/,
     #/4x,'r(a0)  ',3x,'Wavefunction(r)',/)
 35   format(x,'For v = ',i3,2x,' &  eigenvalue E =',1d18.9)
 40   format(/x,'The central SPHERICAL potentials are :',/
     #/2x,'The power of the SMS potential,  ms =',i3,/ 
     #/4x,'r(a0)  ',4x,' Pot(r; a.u.)',/)
 42   format(/x,'The central SPHERICAL potentials are :',/
     #/2x,'The power of the SMS potential,  ms =',i3,/ 
     #/4x,'r(a0)  ',4x,' Pot(r; cm-1)',/)
 44   format(/x,'The DIFFERENCE of SPHERICAL potentials are :',/
     #/2x,'The powers for TWO SMS potentials,  ms, ms-2 =',2i3,/ 
     #/4x,'r(a0)  ',4x,' Pot(r; a.u.)',/)
 50   format(f10.5,2x,1pe16.8)
 52   format(f10.5,2x,f16.5)
 60   format(x,'From v = ',i3,"  to v' = ",i3,'  with ',i6,
     #'  points for each state ',)
 70   format(/x,'The central SPHERICAL potentials are added ',/
     # 2x,'by the constant  Add = ',f12.9,' a.u.',
     #/6x,'[ Pot = Pot + Add ] ',/
     #/4x,'r(a0)  ',4x,' Pot(r; a.u.)',/)
 80   format(/x,'The central SPHERICAL potentials are added ',/
     # 2x,'by the constant  Add = ',f16.5,' cm-1',
     #/6x,'[ Pot = Pot + Add ] ',/
     #/4x,'r(a0)  ',4x,' Pot(r; cm-1)',/)
c----------------------------------------------------------------
        return
      end
c
C=
      subroutine solvese(V, Q, ktyp, iel, rmu, rmin, rmatch,
     &       rmax, h, uL1, uL2, uR1, uR2, wronsk, Ecorr, u )
C    &       rmax, h, ul1, ul2, ur1, ur2, wronsk, Ecorr, u )
c---------------------------------------------------------------------
c  Solves the radial Schrodinger Equation for a GIVEN ENERGY in a
c  given potential, returning the wavefunction and an energy 
c  correction term indicating how close the input energy is to an
c  eigenvalue.
c---------------------------------------------------------------------
c  Method:
c    Uses the Numerov method for propagating the wavefunction out
c    from small r and in from large r on a given (equally spaced)
c    mesh.  These waefunctions are matched together at some 
c    intermediate r.  If they match smoothly, i.e if their 
c    derivatives are the same at the matching radius, then the
c    input energy is an eigenvalue and the returned wavefunction
c    is the corresponding eigenfunction.
c---------------------------------------------------------------------
c  Input:
c
c    V          -  An EXTERNAL DOUBLE PRECISION FUNCTION taking one
c                  argument, the radius, which returns the potential,
c                  in Hartrees, at that radius.  If other parameters
c                  are needed for the potential, they should be
c                  provided by a common block.  (double precision)
c
c    Q          -  The INPUT energy, in Hartrees.  (double precision)
c
c    iel        -  The angular momentum quantum number.  (integer)
c
c    rmu        -  The reduced mass, in atomic units.  (double 
c                  precision)
c
c    rmin       -  The minimum radius for the mesh.  (double precision)
c
c    rmatch     -  The matching radius.  This value should be in the
c                  classically allowed region.  (double precision)
c
c    rmax       -  The maximum radius for the mesh.  (double precision)
c
c    h          -  The stepsize for the mesh.  (double precision)
c
c    ul1, ul2   -  The values of the "left" wavefunction (to be
c                  propagated outward) at the first and second rmesh
c                  points.  (double precision)
c
c    ur1, ur2   -  The values of the "right" wavefunction (to be
c                  propagated inward) at the last and next to last
c                  rmesh points.  (double precision)
c---------------------------------------------------------------------
c  Output:
c
c    wronsk     -  An un-normalized wronskian of the left and right 
c                  wavefunctions.  (double precision)
C                    wronsk = 0  means the derivatives of LEFT & 
C                  RIGHT wavefunctions are equal and therefore the
C                  whole EIGENfunction is SMOOTH.
c
c    Ecorr      -  The estimated correction to the input energy Q.
c                  (double precision)
c
c    u          -  The normalized wavefunction on the input rmesh.
c                  u(1) holds u at rmin, u(2) holds u at rmin+h,
c                  etc.  (double precision)
c---------------------------------------------------------------------
c  Notes:
c
c    -  All values are in atomic units:  radii in bohr, energies in
c       Hartrees.  also hbar=1.
c
c    -  The values of rmatch and rmax might be changed slightly to 
c       ensure that they are on the mesh.  rmin will not be changed.
c
c    -  Ecorr is actually the wronskian of the two propagated
c       *****                 *********
c       normalized wavefunctions scaled by some constants.  That 
c       this value may actually be interpreted as an energy correction
c       depends on the calculated wavefunction being "close enough"
c       to the correct eigenfunction.  This condition is not always
c       met in practice, so some care must be used.  It is true,
c                           **********************
c       however, that when the wronskian is zero, the input energy
c       is an eigenvalue.  Near an eigenvalue, (TRUE) wronsk varies 
c                          ****************************************
c       smoothly with the input energy, so you can bracket the
c       ******************************
c       eigenvalue with this information.
c
c    -  Take care not to set rmin exactly equal to zero!  The
c                 *************************************
c       centrifugal term WILL blow up.  Use a very small value of
c       r instead.
c
c    -  The notation used in the code is taken directly from the
c       excellent reference "Practical Points Concerning the Solution
c       of the Schrodinger Equation" by John M. Blatt, Journal of
c       Computational Physics, v.1  pp 382-396 (1967).  Not all of
c       the suggestions in the paper are implemented in this code.
c
c    -  This code was written with clarity more in mind than 
c       efficiency.  
c---------------------------------------------------------------------
c  nrmax -- the maximum number of rmesh points allowed.
c
      implicit real*8(a-h,o-z)
      parameter( nrmax = 20000 )
      dimension T(nrmax),uL(nrmax),uR(nrmax),u(*)
      common /vmon/ rv(20000),vnum(20000)
      data one,two,three,four,ten,twelve/1.0d0,2.0d0,3.0d0, 4.0d0,10.0d0
     $     ,12.0d0/
c---------------------------------------------------------------------
c        IMPORTANT variables:
c  u() holds the wavefunction on the rmesh
c  uL() holds the left wavefunction on a left mesh
c  mL is the index of the matching radius for uL
c  uR() holds the right wavefunction on a right mesh
c  mR is the index of the matching radius for uR
c  T() is used in the Numerov propagation; calculated on the rmesh
c---------------------------------------------------------------------
c  define the internal Numerov f function  f(r)==Tnumerov(r)
c---------------------------------------------------------------------
      Tnumerov(r) =  h*h/twelve * (two*rmu * (V(r) - Q) + (iel*(iel+1))
     $     /(r*r) )
c---------------------------------------------------------------------
c  make sure match will be clean.  "round up" rmax if necessary,
c  put nrmesh to an odd number to make integration convenient
c---------------------------------------------------------------------
c       nrmesh = (rmax - rmin)/h  + 1
c
      nrmesh = int(rmax - rmin)/h  + 1
      if( rmin + h*(nrmesh + 1).lt.rmax)then
         nrmesh = nrmesh + 1
      endif
      if(mod(nrmesh,2).eq.0) nrmesh = nrmesh +1
C     mL = (rmatch - rmin)/h + 1.5
      mL = int( (rmatch - rmin)/h + 1.5 )
      mR = nrmesh + 1 - mL
      rmatch = rmin +(mL-1)*h
c---------------------------------------------------------------------
      if( nrmesh.gt.nrmax)then
         write(6,20)
         stop
      endif
c---
 20   format("Need to increase parameter ''nrmax'' in",
     $     " subroutine solvese ")   
c---------------------------------------------------------------------
c  get T(r) on mesh
c---------------------------------------------------------------------
      if (ktyp .lt. 9) then
         do i = 1, nrmesh
            T(i) = Tnumerov( h*(i-1) + rmin)
         enddo
      elseif (ktyp .eq. 9) then
         do i = 1, nrmesh
            r = h*(i-1) + rmin
            T(i) = h*h/twelve *(two*rmu*(vnum(i)-Q)+(iel*(iel+1))/(r*r))
         enddo
      endif
c---------------------------------------------------------------------
c  propagate uL  (Numerov method)
c---------------------------------------------------------------------
 50   uL(1) = uL1
      uL(2) = uL2
      do i = 3, mL+2
         uL(i) = ((two+ten*T(i-1))*uL(i-1)-(one-T(i-2))*uL(i-2))/(one-
     $        T(i))
      enddo
c  same for uR
      uR(1) = uR1
      uR(2) = uR2
      do i = 3, mR+2
         uR(i) = ((two+ten*T(nrmesh-i+2))*uR(i-1)-(one-T(nrmesh-i+3))
     $        *uR(i-2))/(one-T(nrmesh-i+1))
      enddo
c---------------------------------------------------------------------
c  scale uR
c---------------------------------------------------------------------
      scale = uL(mL)/uR(mR)
      do i = 1,mR+2
         uR(i) = uR(i)*scale
      enddo
c  copy uL and uR onto u
      do i = 1,mL
         u(i) = uL(i)
      enddo
      do i = 1,mR-1
         u(nrmesh+1-i) = uR(i)
      enddo
c---------------------------------------------------------------------
c  normalize, using Simpson's rule for integration
c---------------------------------------------------------------------
      anormsq =  u(1)**2 + u(nrmesh)**2 
      do i = 2,nrmesh-1,2
C      write(0,*)i,u(i)
         anormsq = anormsq + four * u(i)**2
      enddo
      do i = 3,nrmesh-2,2
         anormsq = anormsq + two * u(i)**2
      enddo
      anormsq = anormsq * h/three 
      anorm = one/dsqrt(anormsq)
      do i = 1, nrmesh
         u(i) = u(i) * anorm
      enddo
c  will need these for derivatives
      do i = -2, 2
         uR(mR+i) = uR(mR+i) * anorm
         uL(mL+i) = uL(mL+i) * anorm
      enddo
c---------------------------------------------------------------------
c  get derivatives
c---------------------------------------------------------------------
      A1L = ( uL(mL+1) - uL(mL-1))/two
      A2L = ( uL(mL+2) - uL(mL-2))/two
      A1R = ( uR(mR-1) - uR(mR+1))/two
      A2R = ( uR(mR-2) - uR(mR+2))/two
      B1L = T(mL+1) * uL(mL+1) - T(mL-1) * uL(mL-1)
      B2L = T(mL+2) * uL(mL+2) - T(mL-2) * uL(mL-2)
      B1R = T(mL+1) * uR(mR-1) - T(mL-1) * uR(mR+1)
      B2R = T(mL+2) * uR(mR-2) - T(mL-2) * uR(mR+2)
      uLprime = 16.0d0/(21.0d0 * h)  * (-A1L + 37.0d0*A2L/32.0d0 -
     $     37.0d0*B1L/5.0d0 - 17.0d0*B2L/40.0d0 )
      uRprime = 16.0d0/(21.0d0 * h)  * (-A1R + 37.0d0*A2R/32.0d0 -
     $     37.0d0*B1R/5.0d0 - 17.0d0*B2R/40.0d0 )
      wronsk = uLprime - uRprime
      Ecorr = uL(mL) * ( uLprime - uRprime) /(two * rmu )
c---------------------------------------------------------------------
      return
      end
C
C
      REAL*8 FUNCTION FACX(I)
      IMPLICIT REAL*8 (A-H,O-Z)
C-----------------------------------------------------------------
C     THIS IS A FACTORIAL FUNCTION  I!
C-----------------------------------------------------------------
      DIMENSION TABLE(15)
      DATA TABLE/1.0D+00,2.0D+00,6.0D+00,24.0D+00,120.0D+00,720.0D+00,
     1      5040.0D+00,40320.0D+00,362880.0D+00,
     2      36288.0D+02,399168.0D+02,4790016.0D+02,62270208.0D+02,
     3      871782912.0D+02,1307674368.0D+03/
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
C
c
      real*8 function potf(R)
      implicit real*8(a-h,o-z) 
	real*8 an(20)
      common /pdata/ vc(10),xl(10),nv(10),Add,nvp,iel,ktyp,ms
      common /shomos/ de, alpha, re, rmu, we
      common /murrell/ De0,a1,a2,a3,a4,a5,a6,a7,shift,beta
      common /SF/ alamta,Isf,amu,Nturn,an
c-----------------------------------------------------------------------
c   This function is called to calculate a spherical potential of :
C
c  1.) (if k=1) Gaussion type or of Slater type ;
c  2.) (if k=2) Spherical central potential V0*exp(-alpha*x) ;
c  3.) (if k=3) Simple harmonic potential   V(r)=0.5d0*ak*r**2 ;
c                      where  r=x-re        for ktyp=3 .
c  4.) (if k=4) Morse potential  V(r)=de[exp(-2*a*r) - 2*exp(-a*r)] + de ;
C                      where  r=(x-re)/re   for ktyp=4 .
C  5.) (if k=5) Screened central potential  V(r)=-Z*dexp(-r/D)/r
C
C  6.) (if k=6) Sun-Murrell-Sorbie potential (ktyp=5;  r=x-re) :
C               temp = 1/beta + a1*r + a2*r**2 + a3*r**3 + 
C                               a4*r**4 + a5*r**5
C               V_MS(r) = - De*beta*temp*exp(-a1*beta*r)
C
C  7.) (if k=7) Huxley-Murrell-Sorbie
C               V_MS(x)=-De*(1 + a1*x + a2*x**2 + a3*x**3 + ...)*exp(-a1*x)
C               where x = R - Re
C
C  8.) (if k=8) (if Isf=1) SF potential . V = (lamta+1)*V_MS - lamta*V_Morse .
c-----------------------------------------------------------------------
           yy = 0.0d0
      if (ktyp .eq. 1) then
         do 15 n=1,nvp
            yy=yy+vc(n)*R**nv(n)*dexp(-xl(n)*R*R)
 15      continue

      else if (ktyp .eq. 2) then
         do 25 n=1,nvp
	    yy=yy+vc(n)*R**nv(n)*dexp(-xl(n)*R) 
 25      continue 

      else if (ktyp .eq. 3) then
         xx=R-re 
         ak=rmu*we**2
         yy=yy + 0.5d0*ak*xx**2

      else if (ktyp .eq. 4) then
         xx=(R-re)/re
         temp=dexp(-2.0d0*alpha*xx) - 2.0d0*dexp(-alpha*xx)
         yy=yy + de*temp + de

      else if (ktyp .eq. 5) then
         xx=r-re
         yy=yy - Z*dexp(-xx/D)/xx

      else if (ktyp .ge. 6 .and. ktyp .le. 8) then
         xx=r-re
         if (ktyp .eq. 6) then
            temp=1.0d0/beta + an(1)*xx + an(2)*xx**2 + an(3)*xx**3 
         else 
            temp=1.0d0 + an(1)*xx + an(2)*xx**2 + an(3)*xx**3 
         endif
         if (ms .ge. 4) then
            temp=temp  + an(4)*xx**4 
         elseif (ms .ge. 5) then
            temp=temp  + an(5)*xx**5
         endif
c-
         if (ktyp .eq. 6) then
             yy = yy - de0*beta*temp*dexp(-an(1)*beta*xx)+De0
             goto 200
         else
             yy0 = yy - de0*temp*dexp(-an(1)*xx)
             yy = yy0 + De0
             if (ktyp .eq. 7) goto 200
         endif
c-
  	 f2 = amu*we*we
c--- morse pot
        if (nturn .le. 1) then
          alph = dsqrt(0.5d0*f2/de0)
  	  vmorse = de0*(dexp(-2.0d0*alph*xx)-2.0d0*dexp(-alph*xx))
           v00 = vmorse 
c--- rydberg pot
        elseif (nturn .eq. 2) then
             ralph = dsqrt(f2/de0)
          vrydberg = -de0*(1.0d0+ralph*xx)*dexp(-ralph*xx)
               v00 = vrydberg
c--- pseudo-gaussion pot
        elseif (nturn .eq. 3) then
          pbeta = 0.5d0*dsqrt(4.0d0+f2*re*re/de0) - 1.0d0
            pf1 = 1.0d0-(re/r)*(re/r)
            pf2 = 1.0d0-(r/re)*(r/re)
            vpg = -de0*(1.0d0+pbeta*pf1)*dexp(pbeta*pf2)
            v00 = vpg
        endif
c
c--- fy is the force-field variational function  lamda(r)
  	 fy = xx/r
   	 fy = fy*(1.0d0-dexp(-xx/re*alamta**2))
  	 fy = fy*alamta
  	 yy = (fy + 1.0d0)*yy0 - fy*v00
c-
         if (r .lt. re) then
           if (nturn .eq. 0) then
             yy = yy0
           else
             yy = v00
           endif
         endif
         yy = yy + De0
       endif
C----------------------------------------------------
200     potf = yy
          return
       end
C

