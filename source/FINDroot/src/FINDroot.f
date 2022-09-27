C     program  FINDroot
      implicit real*8(a-h,o-z)
      external zbrent,func
c===================================================================
c   This code is to calculate :
c
C 1.) the ROOT of ONE-dimensional function 
c         --------------------------------
C     Using the Brent's method (through FUNCTION zbrent) to find
C   the ROOT of a user supplied ONE-dimensional function FUNC 
C   known to lie between x1 & x2.  The root, returned as zbrent,
C   will be refined until its accuracy is tol.
c
c       Next (=> broydn) is NOT used yet in the code !
c
C 2.) the ROOTS of MULTI-dimensional equations
c         ------------------------------------
c     Using the Broyden's method (through SUBROUTINE broydn) to
c   find the ROOTS of a user supplied MULTI-dimensional (n-D)
c   equations (in subroutine) FUNCV for a given initial guess 
c   X(1:n).
c
c===================================================================
c   For purpose 1.) :
c       -----------
c ktyp = 1, -->   General Modified Murrell-Sorbie potential, for 
c               example,  for n=4 :
c               f(a1)=de*a1**4 - 6*f2*a1**2 - 12*f3*a1 -12*f4=0.0     (1.)
c               where the ROOT, a1, is the first expansion coefficient
c               of the Murrell-Sorbie (modified Rydberg) potential.
c
c ktyp = 2, --> f(x)=de*a1**4 - 6*f2*a1**2 - 4*f3*a1 - f4=0.0         (2.)
c               for ms=3, Huxley-Murrell formulae for MS potential :
c                   J. Chem. Soc. Faraday Trans 2, 79, 323(1983)
C                 For MS potential [ ms=4; SF's (ECM) formulae ] :
c               f(x)=de*a1**5 - 10.0d0*f2*a1**3 - 10.0d0*f3*a1**2
c                             - 5.0d0*f4*a1 - f5 = 0.0
C                 For MS potential [ ms=5; SF's (ECM) formulae ] :
c               f(x)=de*a1**6 - 15.0d0*f2*a1**4 - 20.0d0*f3*a1**3
c                      - 15.0d0*f4*a1**2 - 6.0d0*f5*a1 - f6 = 0.0
c
c ktyp = 3, --> f(x)=Ev/De + exp(-a1*x)*f(a1,a2,a3;x)=0.0             (3.)
c               where   f(a1,a2,a3;x)=1 + a1*x + a2*x**2 + a3*x**3
c               and x = R - Re.
c               Eq.(2) is the equation of calculating the classical
c               turning point (CTP), x=x_ctp, for Murrel-Sorbie 
c               potential.     ***
c
c ktyp = 4, --> f(x)= -Ev/De + [1-exp(-a*x)]**2 + g(x) =0.0           (4.)
c               where   g(x)=d*a**3*x**3*exp(-2*a*x)*(1+b*a*x)
c               and x = R - Re.      Eq.(3) is the equation of
c               calculating the classical turning point (CTP), x=x_ctp,
c                                                        ***
c               for Hulburt & Hirschfelder (HH) diatomic potential
c               which is the modified Morse potential.
c
c ktyp = 5, --> f(x)
c
c-------------------------------------------------------------------
c   x1, x2 -- The left and right bounds of the ROOT of FUNC.
c               Usually, x1 < 0.0,  x2 > 0.0 .
c      tol -- The convergence criterion.
c      Ev  -- The vibrational eigenvalues.
c-----------------------------------------------------------------
c  For a1 used in Murrell-Sorbie potential (ktyp=1, 2) :
c              ms =  n-1, is the order of MS potential.
c     de,f2,f3,f4 -- parameters used in Eq.(1.)
c   mall = 1, use f2,...,f6 from fort.5 (intermediate case);
C        > 1, use f2,...,f6 from fort.2 (continuous running case).
C
c  For ktyp=4 :
c     we, wexe, be, & ae  are the experimental spectropic constants.
c===================================================================
c   For purpose 2.) :
c       -----------
c
c
c
c
c
c
c===================================================================
      common /murrell/ de,Dd,f2,f3,f4,f5,f6
      common /ctpms/ d1,a1,a2,a3,a4,a5
      common /ctphh/ a,b,d,d2
      dimension  xx1(100),xx2(100),Ev(100),xct(100)
c-------------------------------------------------------------------
      write(6,400) 
        read(5,*) ktyp
      if (ktyp .eq. 3 .or. ktyp .eq. 4) goto 20 
      if (ktyp .eq. 5) goto 30 
c-------------------------------------
c--- For Modified Murrell- Sorbie :
c
        read(5,*) x1,x2,tol,mall
        read(5,*) ms,de,beta,f2,f3,f4
        read(5,*) f5,f6
c---
        Dd = de*beta
      if (mall .gt. 1) then
  	 if(ms .eq. 3 ) then
          read(2,*) f2, f3, f4
  	 else if(ms .eq. 4) then
          read(2,*) f2, f3, f4, f5
  	 else if(ms .eq. 5) then
          read(2,*) f2,f3,f4,f5,f6
       endif
      endif
c---
      if (ktyp .eq. 1) then
        if (ms .eq. 3) then
          write(6,410) de,f2,f3,f4
        elseif (ms .eq. 5) then
          write(6,420) de,beta,Dd,f2,f3,f4,f5,f6
        endif
      elseif (ktyp .eq. 2) then
        if (ms .eq. 3) then
          write(6,450) de,beta,Dd,f2,f3,f4
        elseif (ms .eq. 4) then
          write(6,452) de,beta,Dd,f2,f3,f4,f5
        elseif (ms .eq. 5) then
          write(6,455) de,beta,Dd,f2,f3,f4,f5,f6
        endif
      endif
c---
c         call calaaf
c           Nff = ms - 1
c         call calff(ff,Nff,40,method)
c---
      temp=0.0d0
      a1=zbrent(func,temp,x1,x2,tol,ktyp,ms)
      fy=func(a1,temp,ktyp,ms)
        write(6,520) a1,fy
        write(4,460) a1
      goto 900
c-------------------------------------
c For ktyp=3 (CTP for MS):
c  d2 = De;  ua is the reduced mass;
c  we,wexe,ae,be are the spectropic constants.
c-------------------------------------
  20  read(5,*) n,tol
      if (ktyp .eq. 3) then
        read(5,*) Re,d1,a1,a2,a3
        write(6,530) Re,d1,a1,a2,a3
c
c--- CTP for HH :
      elseif(ktyp .eq. 4) then
        read(5,*) Re,d2,ua
        read(5,*) we,wexe,be,ae
        write(6,540) we,wexe,be,ae,ua
          a=1.2177d0*10**7 * we*dsqrt(ua/d2)
          d=1.0 - (1.0 + ae*we/(6*be**2))/(a*Re)
         b1=ae*we/(be**2)
         b2=5/4 + 5*b1/12 + 5*b1**2/144 - 2*wexe/(3*be)
         b2=b2/((a*Re)**2)
          b=2.0d0 + (7/12 - b2)/d
        write(6,550) Re,d2,a,b,d
      endif
c---
        do i=1,n
          read(5,*) Ev(i),xx1(i),xx2(i)
        enddo
          write(6,560)
      do i=1,n
        x1=xx1(i)
        x2=xx2(i)
        ee=Ev(i)
        xct(i)=zbrent(func,ee,x1,x2,tol,ktyp,ms)
        fy=func(xct(i),ee,ktyp,ms)
          write(6,570) i,Ev(i),xct(i),fy
      enddo
          write(6,580)
      do i=1,n
        xl=Re - xct(i)
        xr=Re + xct(i)
          write(6,590) i,xl,xr
      enddo
        goto 900
c-------------------------------------
  30  write(0,*)
c-------------------------------------

c--------------------------------------------------------------------
 400  format(//5x,'Finding the ROOT of a user supplied function'/)
c410  format(/5x,'0.0 = de*a1**4 - 6*f2*a1**2 - 12*f3*a1 -12*f4',
 410  format(/5x,'0.0 = Dd*a1**4 - 6*f2*a1**2 - 12*f3*a1 -12*f4',
     #//5x,'which is the formulae of Weiguo Sun.  f2, f3, & f4',
     # /5x,'are the functions of Dd & we with  Dd = de*beta .',
     #//5x,'where ',
     #//7x,'     de =',1pe16.8,'  Hartree ',
     # /7x,'     f2 =',1pe16.8,'  Har/(ao**2) ',
     # /7x,'     f3 =',1pe16.8,'  Har/(ao**3) ',
     # /7x,'     f4 =',1pe16.8,'  Har/(ao**4) ',/
     #/5x,'and the ROOT a1 will be used in Murrell-Sorbie',
     #' potential.')
c420  format(/5x,'0 = de*a1**6 - 15*f2*a1**4 - 60*f3*a1**3 ',
 420  format(/5x,'0 = Dd*a1**6 - 15*f2*a1**4 - 60*f3*a1**3 ',
     #/18x,'- 180*f4*a1**2 - 360*f5*a1 - 360*f6 ',
     #//5x,'which is the formulae of Weiguo Sun.  f2, f3, f4, f5',
     # /5x,'& f6 are the functions of de & we with  Dd = de*beta .',
     #//5x,'where ',
     #//7x,'     de =',1pe16.8,'  Hartree ',
     # /7x,'   Beta =',1pe16.8,
     # /7x,'de*Beta =',1pe16.8,
     # /7x,'     f2 =',1pe16.8,'  Har/(ao**2) ',
     # /7x,'     f3 =',1pe16.8,'  Har/(ao**3) ',
     # /7x,'     f4 =',1pe16.8,'  Har/(ao**4) ',
     # /7x,'     f5 =',1pe16.8,'  Har/(ao**5) ',
     # /7x,'     f6 =',1pe16.8,'  Har/(ao**6) ',/
     #/5x,'and the ROOT a1 will be used in Murrell-Sorbie',
     #' potential.')
 450  format(/5x,'0.0 = de*a1**4 - 6*f2*a1**2 - 4*f3*a1 - f4',
     #//5x,'which is the Huxley-Murrell-Sorbie or ',
     #'SF(ECM; 3rd) formulae.',//5x,'where ',
     #//7x,'     de =',1pe16.8,'  Hartree ',
     # /7x,'   Beta =',1pe16.8,
     # /7x,'de*Beta =',1pe16.8,
     # /7x,'     f2 =',1pe16.8,'  Har/(ao**2) ',
     # /7x,'     f3 =',1pe16.8,'  Har/(ao**3) ',
     # /7x,'     f4 =',1pe16.8,'  Har/(ao**4) ',/
     #/5x,'and the ROOT a1 will be used in Murrell-Sorbie',
     #' potential.')
 452  format(/5x,'0.0 = de*a1**5 - 10*f2*a1**3 - 10*f3*a1**2 - '
     #//11x, '5*f4*a1 - f5',
     #//5x,'which is the MS [ SF(ECM; 4th) ] formulae.',
     #//5x,'where ',
     #//7x,'     de =',1pe16.8,'  Hartree ',
     # /7x,'   Beta =',1pe16.8,
     # /7x,'de*Beta =',1pe16.8,
     # /7x,'     f2 =',1pe16.8,'  Har/(ao**2) ',
     # /7x,'     f3 =',1pe16.8,'  Har/(ao**3) ',
     # /7x,'     f4 =',1pe16.8,'  Har/(ao**4) ',
     # /7x,'     f5 =',1pe16.8,'  Har/(ao**5) ')
 455  format(/5x,'0.0 = de*a1**6 - 15*f2*a1**4 - 20*f3*a1**3 - '
     #//11x, '15*f4*a1**2 - 6*f5*a1 - f6',
     #//5x,'which is the MS [ SF(ECM; 5th) ] formulae.',
     #//5x,'where ',
     #//7x,'     de =',1pe16.8,'  Hartree ',
     # /7x,'   Beta =',1pe16.8,
     # /7x,'de*Beta =',1pe16.8,
     # /7x,'     f2 =',1pe16.8,'  Har/(ao**2) ',
     # /7x,'     f3 =',1pe16.8,'  Har/(ao**3) ',
     # /7x,'     f4 =',1pe16.8,'  Har/(ao**4) ',
     # /7x,'     f5 =',1pe16.8,'  Har/(ao**5) ',
     # /7x,'     f6 =',1pe16.8,'  Har/(ao**6) ')
 460  format(1pe24.16)
 520  format(//7x,'The ROOT & the value of the above function are :',
     #//6x,' The  ROOT of the function is, a1 =',1pe14.6,
     #/6x,' The value of the function at  a1 =',1pe14.6,/)
 530  format(/5x,'0=Ev/De + exp(-a1*x)*(1+a1*x+a2*x**2+a3*x**3)',
     #//5x,'where the root x=R-Re=x_ctp, is the classical turning ',
     #/5x,'point for Murrell-Sorbie (MS) potential.',
     # /7x,'     Re =',1pe16.8,'  ao ',
     # /7x,'     De =',1pe16.8,'  Hartree ',
     # /7x,'     a1 =',1pe16.8,'  1/ao ',
     # /7x,'     a2 =',1pe16.8,'  1/(ao**2) ',
     # /7x,'     a3 =',1pe16.8,'  1/(ao**3) ')
 540  format(/5x,'The experimental spectroscopic constants are :',
     # /7x,'     we =',1pe16.8,'  Hartree ',
     # /7x,'   WeXe =',1pe16.8,'  Hartree ',
     # /7x,'     Be =',1pe16.8,'  Hartree ',
     # /7x,'     Ae =',1pe16.8,'  Hartree ',
     # /7x,' Reduced mass  ua =',1pe16.8,'   amu ')
 550  format(/5x,'0=-Ev/De + [1-exp(-a*x)]**2 + g(x) ',
     #//5x,'where g(x)=d*a**3*x**3*exp(-2*a*x)*(1+b*a*x) ',
     #/5x,'and the root x=R-Re=x_ctp, is the classical turning ',
     #/5x,'point for Hulburt-Hirschfelder (HH) potential',
     #/5x,'which is the modified Morse potential.',
     # /7x,'     Re =',1pe16.8,
     # /7x,'     De =',1pe16.8,
     # /7x,'      a =',1pe16.8,
     # /7x,'      b =',1pe16.8,
     # /7x,'      d =',1pe16.8)
 560  format(//5x,'The ROOT & the value of the above function are :',
     #/7x,'        Vib-energies    Root     f(root) ',
     #/7x,'   i        Ev(i)       X(i)       f(X) ',/)
 570  format(7x,i4,2(1x,f14.8),1x,1pe14.6)
 580  format(//5x,'The Classical Turning Points of the potential :',
     #/7x,'           Left ctp     Right ctp  ',
     #/7x,'           Re-X(i)       Re+X(i)    ',
     #/7x,'   i      X_ctp,L(i)    X_ctp,R(i) ',/)
 590  format(7x,i4,2(1x,f14.8))
c--------------------------------------------------------------------
 900    stop 
      end
C===============================================================
C
      real*8 function  zbrent(func,tem,x1,x2,tol,ktyp,ms)
      implicit real*8(a-h,o-z)
      external func
      PARAMETER (ITMAX=1000,EPS=3.e-30)
c----------------------------------------------------------------
c   ITMAX = maximum allowed number of iterations.
c     EPS = machine floating-point precision.
c----------------------------------------------------------------
      a=x1
      b=x2
        fa=func(a,tem,ktyp,ms)
        fb=func(b,tem,ktyp,ms)
      if((fa.gt.0..and.fb.gt.0.).or.(fa.lt.0..and.fb.lt.0.)) then
         write(0,*) "   CHANGE the input x1 & x2  ! "
           pause ' # root must be bracketed for zbrent # '
      endif
         c=b
        fc=fb
      do 11 iter=1,ITMAX
        if((fb.gt.0..and.fc.gt.0.).or.(fb.lt.0..and.fc.lt.0.))then
          c=a
          fc=fa
          d=b-a
          e=d
        endif
        if(abs(fc).lt.abs(fb)) then
          a=b
          b=c
          c=a
          fa=fb
          fb=fc
          fc=fa
        endif
        tol1=2.*EPS*abs(b)+0.5*tol
        xm=.5*(c-b)
        if(abs(xm).le.tol1 .or. fb.eq.0.)then
          zbrent=b
          return
        endif
        if(abs(e).ge.tol1 .and. abs(fa).gt.abs(fb)) then
          s=fb/fa
          if(a.eq.c) then
            p=2.*xm*s
            q=1.-s
          else
            q=fa/fc
            r=fb/fc
            p=s*(2.*xm*q*(q-r)-(b-a)*(r-1.))
            q=(q-1.)*(r-1.)*(s-1.)
          endif
          if(p.gt.0.) q=-q
          p=abs(p)
          if(2.*p .lt. min(3.*xm*q-abs(tol1*q),abs(e*q))) then
            e=d
            d=p/q
          else
            d=xm
            e=d
          endif
        else
          d=xm
          e=d
        endif
        a=b
        fa=fb
        if(abs(d) .gt. tol1) then
          b=b+d
        else
          b=b+sign(tol1,xm)
        endif
        fb=func(b,tem,ktyp,ms)
11    continue
        pause 'zbrent exceeding maximum iterations'
      zbrent=b
        return
      END
C===============================================================
C
      real*8 function  func(x,tem,ktyp,ms)
      implicit real*8 (a-h,o-z)
      common /murrell/ de,Dd,f2,f3,f4,f5,f6
      common /ctpms/ d1,a1,a2,a3,a4,a5
      common /ctphh/ a,b,d,d2
C--------------------------------------------------------------
C   This is a user supplied ONE-dimensional function.
C
c ktyp = 1, -->   General Modified Murrell-Sorbie potential, for 
c               example,  for n=4 :
c               f(a1)=de*a1**4 - 6*f2*a1**2 - 12*f3*a1 -12*f4=0.0     (1.)
c               where a1 is the first expansion coefficient of
c               the Murrell-Sorbie potential.
c
c ktyp = 2, --> f(x)=de*a1**4 - 6*f2*a1**2 - 4*f3*a1 - f4=0.0         (2.)
c               for ms=3, Huxley-Murrell formulae for MS potential :
c                   J. Chem. Soc. Faraday Trans 2, 79, 323(1983)
C                 For MS potential [ ms=4; SF's (ECM) formulae ] :
c               f(x)=de*a1**5 - 10.0d0*f2*a1**3 - 10.0d0*f3*a1**2
c                             - 5.0d0*f4*a1 - f5 = 0.0
C                 For MS potential [ ms=5; SF's (ECM) formulae ] :
c               f(x)=de*a1**6 - 15.0d0*f2*a1**4 - 20.0d0*f3*a1**3
c                      - 15.0d0*f4*a1**2 - 6.0d0*f5*a1 - f6 = 0.0
c
c ktyp = 3, --> f(x)=Ev/De + exp(-a1*x)*f(a1,a2,a3;x)=0.0             (3.)
c                 where   f(a1,a2,a3;x)=1 + a1*x + a2*x**2 + a3*x**3
c               and x = R - Re. 
c               Eq.(2) is the equation of calculating the classical
c               turning point, x=x_ctp, for Murrel-Sorbie potential.
c 
c ktyp = 4, --> f(x)= -Ev/De + [1-exp(-a*x)]**2 + g(x) =0.0           (4.)
c               where   g(x)=d*a**3*x**3*exp(-2*a*x)*(1+b*a*x)
c               and x = R - Re.      Eq.(3) is the equation of
c               calculating the classical turning point, x=x_ctp,
c               for Hulburt & Hirschfelder (HH) diatomic potential
c               which is the modified Morse potential.
C--------------------------------------------------------------
        goto (10,20,30,40) ktyp
C--- For Sun's formulae :
  10  if (ms .eq. 3) then
C       fx=de*x**4 - 6.0d0*f2*x**2 - 12.0d0*f3*x - 12.0d0*f4
        fx=Dd*x**4 - 6.0d0*f2*x**2 - 12.0d0*f3*x - 12.0d0*f4
      elseif (ms .eq. 5) then
        fx=Dd*x**6 - 15.0d0*f2*x**4 - 60.0d0*f3*x**3
        fx=fx - 180.0d0*f4*x**2 - 360.0d0*f5*x - 360.0d0*f6
      endif
        goto 900
C---------------------------------------------
C--- For MS potential [ SF's (ECM) formulae ] :
  20  if (ms .eq. 3) then
        fx=de*x**4 - 6.0d0*f2*x**2 - 4.0d0*f3*x - f4
      else if (ms .eq. 4) then
        fx=de*x**5 - 10.0d0*f2*x**3 - 10.0d0*f3*x**2 - 
     # 5.0d0*f4*x - f5
      else if (ms .eq. 5) then
        fx=de*x**6 - 15.0d0*f2*x**4 - 20.0d0*f3*x**3 - 
     # 15.0d0*f4*x**2 - 6.0d0*f5*x - f6
      endif
        goto 900
C---------------------------------------------
  30  fx=1.0d0 + a1*x + a2*x**2 + a3*x**3
      fx=tem/d1 + dexp(-a1*x)*fx
        goto 900
C---------------------------------------------
  40  fx=d*(a*x)**3*dexp(-2*a*x)*(1+b*a*x)
      fx=-tem/d2 + (1.0d0 - dexp(-a*x))**2 + fx
C--------------------------------------------------------------
 900  func=fx
        return
      END
C
C================================================================
C  "broydn" is NOT used here !
C================================================================
C
      SUBROUTINE broydn(x,n,N1,check)
      implicit real*8 (a-h,o-z)
C----------------------------------------------------------------
C   Given an initial guess X(1:n) for a ROOT in n dimensions,
C find the root by Broyden's method embedded in a globally 
C convergent strategy.  The vector of functions to be zeroed,
C called FVEC(1:n) in the routine below, is returned by a user
C supplied subroutine that MUST be called FUNCV and have the
C declaration subroutine  FUNCV(n,x,fvec). The subroutine FDJAC
C and the function FMIN are used.  
C   The output quantity CHECK is false on a normal return and 
C true if the routine has converged to a local minimum of the 
C function FMIN or if Broyden's can make no further progress.
C In this case try RESTARTING from a different initial guess.
C
C       PARAMETERS :
C   NP   -- is the maximum expected value of n;
C MAXITS -- is the maximum number of iterations;
C   EPS  -- is close to the machine precision;
C  TOLF  -- sets the convergence criterion on function values;
C TOLMIN -- sets the criterion for deciding whether spurious
C             convergence to a minimum of FMIN has occurred;
C  TOLX  -- is the convergence criterion on delta_x;
C STPMX  -- is the scaled maximum step length allowed in line 
C             searches.
C----------------------------------------------------------------
      LOGICAL check,restrt,sing,skip
      PARAMETER (NP=40, MAXITS=200, EPS=1.e-7, TOLF=1.e-4)
      PARAMETER (TOLMIN=1.e-6, TOLX=EPS, STPMX=100.0)
      EXTERNAL fmin
      COMMON /newtv/ fvec(NP),nn
      dimension  x(n),c(NP),d(NP),fvcold(NP),g(NP),p(NP)
      dimension  qt(NP,NP),r(NP,NP),s(NP),t(NP),w(NP),xold(NP)
C----------------------------------------------------------------
CU  USES fdjac,fmin,lnsrch,qrdcmp,qrupdt,rsolv
C----------------------------------------------------------------
        nn=n
      f=fmin(N1,x)
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
      pause ' *  MAXITS exceeded in broydn  * '
      END
C
C================================================================
C
      SUBROUTINE fdjac(n,x,fvec,np,df)
      implicit real*8 (a-h,o-z)
      PARAMETER (NMAX=40,EPS=1.e-4)
      dimension  df(np,np),fvec(n),x(n),f(NMAX)
C----------------------------------------------------------------
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
C
C================================================================
C
      FUNCTION fmin(n1,x)
      implicit real*8 (a-h,o-z)
      PARAMETER (NP=40)
      COMMON /newtv/ fvec(NP),n
      dimension  x(n1)
      SAVE /newtv/
C     dimension  x(*)
c-------------------------------------------------------------------
      call funcv(NP,n,x,fvec)
      sum=0.
      do 11 i=1,n
        sum=sum+fvec(i)**2
11    continue
      fmin=0.5*sum
        return
      END
C
C================================================================
c     SUBROUTINE lnsrch(np,n,xold,fold,g,p,x,f,stpmax,check,func1)
C
      SUBROUTINE lnsrch(np,n,xold,fold,g,p,x,f,stpmax,check)
      implicit real*8 (a-h,o-z)
      LOGICAL check
      PARAMETER (ALF=1.e-4, TOLX=1.e-7)
c-----------------------
c     EXTERNAL func1
c-----------------------
      EXTERNAL fmin
      dimension  g(n),p(n),x(n),xold(n)
c-------------------------------------------------------------------
      check=.false.
        sum=0.
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
c       f=func1(x,n)
        f=fmin(np,x)
        if(alam.lt.alamin)then
          do 16 i=1,n
            x(i)=xold(i)
16        continue
          check=.true.
          return
        else if(f.le.fold+ALF*alam*slope)then
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
C
C================================================================
C
      SUBROUTINE qrdcmp(a,n,np,c,d,sing)
      implicit real*8 (a-h,o-z)
      LOGICAL sing
      dimension  a(np,np),c(n),d(n)
c-------------------------------------------------------------------
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
C
C================================================================
C
      SUBROUTINE qrupdt(r,qt,n,np,u,v)
      implicit real*8 (a-h,o-z)
      dimension  r(np,np),qt(np,np),u(np),v(np)
c-------------------------------------------------------------------
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
C
C================================================================
C
      SUBROUTINE rsolv(a,n,np,d,b)
      implicit real*8 (a-h,o-z)
      dimension  a(np,np),b(n),d(n)
c-------------------------------------------------------------------
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
C
C================================================================
C
      SUBROUTINE func1(x,n)
      implicit real*8 (a-h,o-z)
      dimension  x(n)
c-------------------------------------------------------------------
C
          return
      END
C
C================================================================
      subroutine calaaf
      implicit real*8(a-h,o-z)
      common /aafcom/ aaf(4,10)
      common /fms/ ms
      common /spectra/ amu,Re,We,WeXe,WeYe,WeZe,alphae,gamae  
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
10      continue
100     format(1x,5(1pe16.7))
C
          return
        end
C================================================================
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
10      continue
      if (method .eq. 1) then
          call broydn(x,N,NP,check)
c     else if (method .eq. 2) then
c         call newt(x,N,NP,check)
      endif
C
          call funcv(NP,N,x,fvec)
        do 20 k = 1,N
           write(25,*)"fvec(",k,") = ",fvec(k)
20      continue
        do 30 k = 1, N
           ff(k+2) = x(k) 
30      continue
        return
        end
C================================================================
C
      SUBROUTINE funcv(n1,n,x,fvec)
      implicit real*8 (a-h,o-z)
      dimension  fvec(n1),x(n1)
      common /aafcom/ aaf(4,10)
      common /fms/ ms
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
c-------------------------------------------------------------------
C
        return
      END
C
C================================================================
C
      SUBROUTINE rotate(r,qt,n,np,i,a,b)
      implicit real*8 (a-h,o-z)
      dimension  r(np,np),qt(np,np)
c-------------------------------------------------------------------
      if(a.eq.0.)then
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
C
C================================================================
