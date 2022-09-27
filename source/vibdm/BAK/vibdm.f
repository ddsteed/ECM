C    program vibdim
C========================================================================
C                Modified by Feng Hao.        07/10/1998
C                Modified by Weiguo  Sun.     11/08/1998
C    To calculate the bound vibrational energies & wavefunctions of 
C  diatomic potential :
C
C 1.) Sun's Modified Murrell-Sorbie (x=R-Re) :
c           V_SMMS(R)=-De*beta*(1/beta + a1*x + a2*x**2 
c                                 + a3*x**3 + ...)*exp(-a1*beta*x)
C    which is NOT used here.
c
c 2.) Huxley-Murrell-Sorbie (x=R-Re) :
c        V_MS(R)=-De*(1 + a1*x + a2*x**2 + a3*x**3 + ...)*exp(-a1*x)
C
C 3.) SF (ECM) : V_ecm(R) = V_MS + Lamta(R)*delta_V(R)
c          V_MS:  Murrell & Sorbie potential in which the
c                 (a1,a2,a3) are calculated using SF formulae.
c          Lamta(R)   = lamta*(x/R)*[1-exp(-lamta^2*x/R)]
c          delta_V(R) = V_MS - V_0
c                                For  R < Re :
c   Nturn = 0, V_0 = V_Morse   , V_ecm(R) = V_ecm;
c         = 1, V_0 = V_Morse   , V_ecm(R) = V_MS ;
c         = 2, V_0 = V_Morse   , V_ecm(R) = V_0  ;
c         = 3, V_0 = V_Rydberg , V_ecm(R) = V_0  ;
c         = 4, V_0 = V_PG      , V_ecm(R) = V_0  .
C
C    If you want to use the other potential , please modified the
C  corresponding cofficients and the FUNCTION POT.
C========================================================================
      IMPLICIT  REAL*8(A-H,O-Z)
      PARAMETER (MRN=20000,MHEGR=100)
      DIMENSION EV(MHEGR),ST1(MRN),Z(MRN,MHEGR)
      DIMENSION a(10),ax(96,96),ex(30,96),sv(30)
      DIMENSION xmean(30),xadev(30),xsdev(30)
      DIMENSION xvar0(30),xskew(30),xcurt(30)
      COMMON /MS1/ a
      COMMON /MS2/ De,beta,alamta,Re,MSF,ims,Nturn
      COMMON /BDT/ AMU,RBOHR,CONST,PI
      COMMON /MOL/ We,DALE,ST1I,ST1F,NTOT,NCC1
      COMMON /LA1/ mset,mcst,mv,nvs(30)
C-------------------------------------------------
C Read data and calculate reduced mass  AMU
C-------------------------------------------------
          aucm = 219474.63067d0
        CALL READATA
C-------------------------------------------------
C Prepare head for output energies
C-------------------------------------------------
      write(10,300)
C-------------------------------------------------------------    
C   VIBDIM calls RNORM which uses renormalized Numerov method 
C to solve a 2nd order radial differential (eigenvalue)
C equation for a given potential array V(R).
C   st1 -- 1-D array containning radial points R's.  
C    Z  -- 2-D array containning vibrational eigenfunctions.
C    EV -- 1-D array containning vibrational eigenvalues.
C-------------------------------------------------------------    
      CALL VIBDIM(NTOT,DALE,NCC1,ST1I,ST1F,IFAIL,EV,Z,st1,De)
C-------------------------------------------------------------    
C     These are the wavefunctions. v = 0   ---> fort.30  
C                                  v = 1   ---> fort.31
C                                  ...     ...  ...
C-------------------------------------------------------------    
      do 50 i = 1, NTOT
          write(i+29,310) i-1, EV(i)
         do 40 j = 1,ncc1
          write(i+29,320) st1(j) ,Z(j,i)
 40      continue
 50   continue
C-------------------------------------------------------------    
C Sun's modification
C
C   Generate the coefficients matrix ax(nv,nv) according to 
C Ev = We(v+1/2) + WeXe(v+1/2)**2 + WeYe(v+1/2)**3 + ...
C for (nv = nve+1) states :
c         v = 0, 1, 2, 3, 4, 5, ..., 10, ..., mv
C  mv =<  96 ;  ==> v_max = mv - 1 = 95.
C-------------------------------------------------------------  
      do 60 i=1, mv
          iv = i-1
        do k=1, mv
          ax(i,k) = (iv + 0.5d0)**k
        enddo
 60   continue
C-------------------------------------------------------------    
C   Solve linear equation    ax * X = bx   to find the vector
C X = (We, WeXe, WeYe, WeZe, WeSe, WeTe, ...)
C   When returns,  ex contains mset bx (solution vector X).
C-------------------------------------------------------------    
        call linersolv(ax,96,ex,30,96,EV)
C-------------------------------------------------------------    
C Cal. statistical data for every element of vector X :
C    X = ( We, WeXe, WeYe, WeZe, WeSe, WeTe, ... )
C    i =    1,   2,    3,    4,    5,    6,  ...
C ex(k,i) -- the ith element generated from the kth data set.
C
C   Given an array of sv(n), moment returns its mean ave,
C average deviation adev, standard deviation sdev, variance
C var, skewness skew, and kurtosis curt.
C-------------------------------------------------------------    
      do 70 i=1, mcst
        do k=1, mset
          sv(k) = ex(k,i)
        enddo
          call moment(sv,mset,30,ave,adev,sdev,var,skew,curt)
        xmean(i) = ave
        xadev(i) = adev
        xsdev(i) = sdev
        xvar0(i) = var
        xskew(i) = skew
        xcurt(i) = curt
 70   continue
C---
        write(10,330) 
      do 80 i=1, mcst
        write(10,340) i, xmean(i), xadev(i), xsdev(i), xvar0(i),
     # xskew(i), xcurt(i)
 80   continue
C---
        write(10,350) 
      do 90 i=1, mcst
        write(10,380) i, xmean(i), xmean(i)*aucm
 90   continue
C---
      do 100 k=1, mset
          write(10,370) k
        do i=1, mcst
          write(10,380) i, ex(k,i), ex(k,i)*aucm
        enddo
 100  continue
C
C End modification
C------------------------------------------------------------- 
 300  format(//26x,'=====  Output energies  =====',//
     #5x,'i',9x,'Ev(i)',9x,'Ev(i)-Ev(1)',7x,'Ev(i)-Ev(i-1)',
     #6x,'Ev(i)+De',/)
 310  format(/3x,'Wavefunctions for v =',i3,5x,'E_v = ',f16.8,/
     #/9x,'R(ao)',12x,'Phi_v(R)',/)
 320  format(f15.8,3x,e20.10)
 330  format(///11x,'***  Statistical calculations on molecular',
     #' constants  ***  : ',//
     #20x,' Ave -- mean (average) value of data; ',/
     #20x,'Adev -- average deviation of data; ',/
     #20x,'Sdev -- standard deviation of data; ',/
     #20x,' Var -- variance of data; ',/
     #20x,'Skew -- skewness of data; ',/
     #20x,'Curt -- kurtosis of data; ',//
     #16x,'{     We, WeXe, WeYe, WeZe, WeSe, WeTe, ...  }',/
     #16x,'{ i =  1,   2,    3,    4,    5,    6,  ...  }',//
     #/2x,' i',8x,'Ave',11x,'Adev',8x,'Sdev',9x,'Var',8x,
     #'Skew',8x,'Curt',/)
 340  format(2x,i2,1E17.9,5E12.5)
 350  format(//5x,'===  Average value of vibrational constants',
     #' ===',//6x,'i',6x,'Const(a.u.)',6x,'Const(cm-1)'/)
 370  format(//5x,'***  Vibrational constants from data set',
     #i3,'  ***',//6x,'i',6x,'Const(a.u.)',6x,'Const(cm-1)'/)
 380  format(5x,i2,2(1PE18.9) )
C-------------------------------------------------------------    
      END

      SUBROUTINE  VIBDIM(NTOT,DALE,NCC1,ST1I,ST1F,IFAIL,EV,Z,st1,De)
      IMPLICIT REAL*8(A-H,O-Z)
C---
      PARAMETER (MRN=20000,MHEGR=100)
      DIMENSION ST1(MRN),V1(MRN)
C     DIMENSION EV(MHEGR),D(MHEGR),E(MHEGR),Z(MRN,MHEGR)
      DIMENSION EV(MHEGR),D(MHEGR),Z(MRN,MHEGR)
      COMMON /BDT/ AMU,RBOHR,CONST,PI
C-------------------------------------------------------------    
      IF (MOD(NCC1,2).EQ.0) NCC1=NCC1+1
        IFAIL=0
      AL1=1/AMU
      HH1=(ST1F-ST1I)/(NCC1-1)
        ST1(1)=ST1I
      DO 1 I=2,NCC1
  1   ST1(I)=ST1(I-1)+HH1
      DO 3 I=1,NCC1
      X=ST1(I)
  3   V1(I)=POT(X)
C----------------------------------------
C-- CONST =1 Hartree = 219474.63067 cm-1 
C--   Change DALE into Hartree :
C----------------------------------------
        DALE=DALE/CONST
C-------------------------------------------------------------
C  RNORM - Using renormalized Numerov method to solve a 2nd
C          order radial differential (eigenvalue) equation 
C          for a given potential array V1.
C-------------------------------------------------------------
      CALL RNORM(NCC1,NTOT,HH1,EV,V1,EPS,DALE,0,ST1,AL1,A,D,Z,IFAIL)
C----------------------------------------
        IF (IFAIL.EQ.-1) THEN
          write(10,*)'   rnorm  failed !'
          RETURN
        endif
C     write(10,*)'   ev',' NTOT=',NTOT
      WRITE(8)NCC1,NTOT,ST1,Z
C
      DO 5 I=1,NTOT
        write(7,8)  Ev(i) + De
        write(23,8) Ev(i) + De
        WRITE(10,7) I,EV(I),EV(I)-EV(1),EV(I)-EV(I-1),Ev(I)+De
 5    continue
C----------------------------------------
 6     FORMAT(1X,I5,2F15.8)
 7     FORMAT(1X,I5,4E18.10)
C7     FORMAT(1X,I5,4F16.10)
 8     FORMAT(1X,F16.10)
C----------------------------------------
        return
      END


      SUBROUTINE RNORM(N,MROOT,HH,EV,V,EPS,DALE,IMARK,ST,DTAMU,A,DD,
     &                  Z,IFAIL)
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (MRN=20000,MHEGR=100)      
      DIMENSION EV(MHEGR),T(MRN),U(MRN),F(MRN),RL(MRN),RR(MRN)
      DIMENSION V(MRN),ST(MRN),A(MHEGR,MHEGR),DD(MHEGR),Z(MRN,MHEGR)
C================================
C     IMARK=0     NO SYMMETRY
C     IMARK=1     SYMMETRY
C--------------------------------
      EPS=1.0D-10
        EMIN=1000000
      DO 90 I=1,N
C      WRITE(10,*)I,V(I)
      IF (V(I) .LT. EMIN) EMIN=V(I)
  90  CONTINUE
        CC=-HH**2/(6*DTAMU)
        EL=EMIN
        EE=EL
      DO 50 IR=0,MROOT-1
 100  EE=EE+DALE
C---    DETERMINING EH,EL
      DO 1 I=1,N
        T(I)=CC*(EE-V(I))
 1    U(I)=(2+10*T(I))/(1-T(I))
        RR(N)=U(N)
        RL(1)=U(1)
        MP=0
      DO 3 I=N-1,1,-1
        RR(I)=U(I)-1/RR(I+1)
      IF (RR(I).LT.1) THEN
      MP=I
      GOTO 2
      ENDIF
  3   CONTINUE
  2   CONTINUE
        IE=0
      DO 6 I=2,MP
      RL(I)=U(I)-1/RL(I-1)
      IF (RL(I).LT.0) IE=IE+1
  6   CONTINUE
      IF (IE.EQ.IR+1) THEN
      EH=EE
C      WRITE(0,*)'  EH=',EH, '  EL=',EL
      GOTO 25
      ENDIF
      IF (IE.GT.IR+1) THEN
      WRITE(0,*)'  PLEASE GIVE A SMALL VALUE OF dale (= delta_E) '
      WRITE(0,*)" IE = ",IE," IR = ",IR
      STOP
      ENDIF
        GOTO 100
  25  E=(EH+EL)/2
        IF (EH-EL.LT.EPS) GOTO 30
      DO 28 I=1,N
      T(I)=CC*(E-V(I))
 28   U(I)=(2+10*T(I))/(1-T(I))
        RR(N)=U(N)
      RL(1)=U(1)
      DO 26 I=N-1,MP+1,-1
  26  RR(I)=U(I)-1/RR(I+1)
      DO 27 I=2,MP
  27  RL(I)=U(I)-1/RL(I-1)
        DAE=1/RR(MP+1)-RL(MP)
      IF (DAE.LT.0) THEN
      EL=E
      ELSE
      EH=E
      ENDIF
        GOTO 25
  30  CONTINUE
C      WRITE(10,*)IR+1,E,'  ROOT'
      EL=E
      EV(IR+1)=E
        F(MP)=1
      DO 31 I=MP-1,1,-1
  31  F(I)=F(I+1)/RL(I)
      DO 32 I=MP+1,N
  32  F(I)=F(I-1)/RR(I)
      DO 35 I=1,N
  35  F(I)=F(I)/(1-T(I))
c--------------------------------------------
c Normalize wavefunctions in array F.
c--------------------------------------------
        CALL NORM(N,HH,F,IMARK)
      DO 37 I=1,N
  37  Z(I,IR+1)=F(I)
        EE=E
  50  CONTINUE
c--------------------------------------------
        return
      END
C
      SUBROUTINE NORM(N,HH,F,IMARK)
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (MRN=20000,MHEGR=100)      
      DIMENSION F(MRN)
c--------------------------------------------
      NM=N/2
      SUM1=0
      SUM2=0
      DO 5 K=1,NM-1
      K1=2*K+1
  5   SUM1=SUM1+F(K1)**2
      DO 6 K=1,NM
      K2=2*K
  6   SUM2=SUM2+F(K2)**2
      SUM=(2*SUM1+4*SUM2+F(1)**2+F(N)**2)*HH/3
      IF (IMARK.EQ.1) SUM=2*SUM
      SUM=1/DSQRT(SUM)
      DO 10 K=1,N
 10   F(K)=F(K)*SUM
c--------------------------------------------
        return
      END

      SUBROUTINE READATA
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER *80 TITLE
      DIMENSION a(10)
      COMMON /MS1/ a
      COMMON /BDT/ AMU,RBOHR,CONST,PI
      COMMON /MS2/ De,beta,alamta,Re,MSF,ims,Nturn
      COMMON /MOL/ We,DALE,ST1I,ST1F,NTOT,NCC1
      COMMON /LA1/ mset,mcst,mv,nvs(30)
C-------------------------------------------------
      RBOHR=0.529177249D0
      AMAE=1.6605655D-27/9.1093897D-31
C-------------------------------------------------
C     AMAE=mass_unit/mass_e
C     AMAE=1.6605655D-27/9.1093897D-31
C     AMAE=1822.9163
C     CONST=2.0D0*109737.32D0
C     1 Rydberg = 109737.32D0
C     CONST=27.211396181D0*8065.541D0
C     CONST=1 Hartree = 219474.63067 cm-1
C-------------------------------------------------
      CONST=219474.63067D0
      PI=DACOS(-1.0D0)
C-------------------------------------------------
C Imal == 1, read coeff. a(i) from fort.5;
C       = 2, read a(i) from fort.4.
C  ims -- number of terms in V(R) expansion.
C-------------------------------------------------
      READ(5,*) Imal
      READ(5,*) ims, Nturn 
C-------------------------------------------------
C Read title & atomic mass; cal. reduced mass
C-------------------------------------------------
      READ(5,350) TITLE
      READ(5,*)  Imu
      READ(5,*)  AMA,AMB
      READ(5,*)  AMU1
C---
      write(10,*)
      WRITE(10,350) TITLE
      WRITE(10,370) AMA,AMB
C-------------------------------------------------
C NTOT -- number of roots (energies) wanted;
C DALE -- E_step (cm-1) used in renormalization.
C-------------------------------------------------
      READ(5,*) NTOT,DALE
C-------------------------------------------------
C NCC1 -- number of potential points;
C ST1I -- R_min for V(R);
C ST1F -- R_max used to cal. R_step :
C     R_step = (ST1F - ST1I)/(NCC1-1) = HH1
C-------------------------------------------------
      READ(5,*) NCC1,ST1I,ST1F
C---
      WRITE(10,380) NTOT
      WRITE(10,390) NCC1,ST1I,ST1F
C-------------------------------------------------
C      De -- molecular dissociation energy.
C    beta  = 1.0, for SF potential function.
C  alamta  = variational parameter in SF V(R).
C     MSF  = 1, use SF V(R); = 0, use MMS.
C      Re -- equilibrium internuclear distance.
C-------------------------------------------------
        READ(5,*) De,beta,alamta,MSF,Re
C-------------------------------------------------
C We -- vibrational constant
C-------------------------------------------------
        READ(5,*) We
C-------------------------------------------------
C mset -- number of set of solution vectors  X;
C           usually, set  mset =< 20.
C mcst -- number of vib. constants in a vector X;
C           usually, set  mcst =< 6.
C             A(mcst, mcst);  b(mcst, 1)
C   mv -- number of vib. states in A used to 
C           solve linear equation    A * X = b
C             set  mv =< 95.
C nvs(i) -- value of initial vib. quantum state 
C           v+1 in each set of mset vectors, e.g.
C               i  = 1, 2, 3, 4, 5   (mset=5)
C           nvs(i) = 1, 3, 6, 8, 9
C            v_ini = 0, 2, 5, 7, 8
C
C    When mcst=6, the states used in a set are :
C  nvs(1)=1 :  v = 0, 1, 2, 3, 4, 5.
C  nvs(3)=6 :  v = 5, 6, 7, 8, 9,10.
C
C    X = ( We, WeXe, WeYe, WeZe, WeSe, WeTe, ...)
C-------------------------------------------------
        READ(5,*) mset, mcst, mv
        READ(5,*) ( nvs(i), i=1,mset )
C-------------------------------------------------
C Read potential expansion coefficients  a(i)
C-------------------------------------------------
      IF(Imal .EQ. 2 ) THEN
        do 10 i = 1,ims
          READ(4,*) a(i)
 10     continue
      ELSE IF( Imal .EQ. 1 ) THEN
        do 20 i = 1,ims
          READ(5,*) a(i)
 20     continue
      ENDIF
C-------------------------------------------------
      if ( Imu .eq. 1 ) then
        AMU=( AMB*AMA/(AMA+AMB) )*AMAE
      else if ( Imu .eq. 2 ) then
        AMU = AMU1*AMAE
      endif
c
      WRITE(10,375) AMU1,AMU
C-------------------------------------------------
350   format(a)
370   format(//5x,'THE ATOMIC MASSES ARE (IN ATOMIC MASS):',
     #/14x,'A_mass',9x,'B_mass',/10x,2f14.10)
375   format(/5x,'The reduced molecular mass is : '
     #/4x,f14.10,' (IN ATOMIC MASS)',/5x,'or',/4x,f18.10,
     #1x,'(IN ATOMIC MASS UNIT)')
380   format(//5x,'The number of states used, NTOT =',i5)
390   format(/10x,'R-pts',5x,'R_ini',5x,'R_end',
     #/9x,'(NCC1)',4x,'(ST1I)',4x,'(ST1F)',
     #/8x,i6,3x,f8.3,3x,f8.3,/)
C-------------------------------------------------
        return
      END

      DOUBLE PRECISION FUNCTION POT(R)
      IMPLICIT REAL*8(A-H,O-Z)
      real*8  a(10)
      COMMON /MS1/ a
      COMMON /MS2/ De,beta,alamta,Re,MSF,ims,Nturn
      COMMON /BDT/ AMU,RBOHR,CONST,PI
      COMMON /MOL/ We,DALE,ST1I,ST1F,NTOT,NCC1
C-------------------------------------------------
      RR=R-Re
      x =RR
      d = De*beta
        func = 0.0
      do 10  i = 1, ims
         func = func + a(i)*RR**i
10    continue
c
c--- Prepare Murrell-Sorbie (MS) POT
         func = -d*(func + 1.0d0/beta)*dexp(-a(1)*beta*RR)
c===
      if ( MSF .eq. 1 ) then
          f2 = AMU*We*We
c--- Morse POT
        if (Nturn .le. 2) then
          alph = dsqrt(0.5d0*f2/De)
          Vmorse = De*(dexp(-2.0d0*alph*RR) - 2.0d0*dexp(-alph*RR))
           V00 = Vmorse 
c--- Rydberg POT
        elseif (Nturn .eq. 3) then
             Ralph = dsqrt(f2/De)
          Vrydberg = -De*(1.0d0+Ralph*x)*dexp(-Ralph*x)
               V00 = Vrydberg
c--- Pseudo-Gaussion POT
        elseif (Nturn .eq. 4) then
          Pbeta = 0.5d0*dsqrt(4.0d0+f2*Re*Re/De) - 1.0d0
            Pf1 = 1.0d0-(Re/R)*(Re/R)
            Pf2 = 1.0d0-(R/Re)*(R/Re)
            Vpg = -De*(1.0d0+Pbeta*Pf1)*dexp(Pbeta*Pf2)
            V00 = Vpg
        endif
c
c--- fy is the force-field variational function  LAMDA(R)
  	  fy = x/R
	  if (ims .eq. 3) then
	     fy = fy
	  else if (ims .eq. 4) then
	     fy = (dabs(fy))**(1.0001d0)*x/dabs(x)
	  else if (ims .eq. 5) then
	     fy = (dabs(fy))**(2.0001d0)*x/dabs(x)
	  endif
          fy = fy*(1.0d0 - dexp(-x/Re * alamta**2.0d0))
          fy = fy*alamta
          fp = (fy + 1.0d0)*func - fy*V00
      endif
c===
        IF (R .lt. Re) THEN
          if (Nturn .eq. 0) then
            POT = fp
	    else if (Nturn .eq. 1) then
            POT = func
          else
            POT = V00
          endif
        ELSE
            POT = fp
        ENDIF
C-------------------------------------------------
      END

C---
      subroutine linersolv(ax,ka,ex,ke,kf,bb)
      implicit real*8 (a-h,o-z)
      PARAMETER (MHEGR=100)
c     dimension ax(96,96),ex(30,96),bb(mhegr)
      dimension ax(ka,ka),ex(ke,kf),bb(mhegr)
      dimension aa(96,96),ab(96,96),ac(96,96),bx(96,1)
      COMMON /MS2/ De,beta,alamta,Re,MSF,ims,Nturn
      COMMON /LA1/ mset,mcst,mv,nvs(30)
C-------------------------------------------------
C Loop over (solve) the mset sets linear equa.
C       mset =< 30
C-------------------------------------------------
      do 30 j = 1, mset
          m1 = nvs(j)
          m2 = m1 + mcst - 1
C-------------------------------------------------
C For i=m2 :  m2-m1+1 = m1+mcst-1 - m1+1 = mcst
C   The physical size of arrays :
C        aa(mcst,mcst),   bx(mcst,1)
C Define aa & bx arrays in the jth set
C
C   The shifted energy  E(v) = Ev + De
C-------------------------------------------------
        do 20 i = m1, m2
             i1 = i - m1 + 1
            bx(i1,1) = bb(i) + De
C-------------------------------------------------
C  Next line generates wrong  We,WeXe,WeYe, ... :
C           bx(i1,1) = bb(i)
C-------------------------------------------------
          do k = 1, mcst
            aa(i1,k)=ax(i,k)
            ab(i1,k)=ax(i,k)
          enddo
 20     continue
C-------------------------------------------------
C Solve   aa*X=bx    for the jth set
C-------------------------------------------------
        call gaussj(aa,mcst,96,bx,1,1)
C-------------------------------------------------
C   When gaussj returns, aa = 1/ab , bx == X .
C Save the jth solution vector bx onto ex .
C-------------------------------------------------
            do k = 1, mcst
              ex(j,k) = bx(k,1)
            enddo
C-------------------------------------------------
C Print ab and its inverse matrix aa == 1/ab
C-------------------------------------------------
            kt = 18
          write(kt,301) j
        call mprint(ab,mcst,mcst,96,96,kt,1)
        call mprint(aa,mcst,mcst,96,96,kt,2)
C-------------------------------------------------
C Check & print  if  ab *aa = ab * 1/ab = 1
C-------------------------------------------------
        call multp(mcst,mcst,mcst,aa,ab,ac,96,96,96)
        call mprint(ac,mcst,mcst,96,96,kt,3)
 30   continue
C-------------------------------------------------
C Print the solution vector array  ex
C-------------------------------------------------
        call mprint(ex,mset,mcst,30,96,kt,4)
C-------------------------------------------------
 301  format(//13x,'*** Solved A*X=b for data set i =',
     #i3,'  ***')
C-------------------------------------------------
c       return
      END
C
C==============================================================================
C  GAUSSJ solves for the linear equations 
C                          A*X=b.  
C using the Gauss-Jordan elimination.    
C   Input:  a(n,n) is the coefficient matrix A.  b(n,m) is an inputting 
C           matrix containning the m right-hand side vectors.
C   Output: a(n,n) is the inverse matrix A-1 of A.  b(n,m) is the solution
c           vectors X; and b == a = A-1 if b is inputted as an unit matrix.
C==============================================================================
      SUBROUTINE gaussj(a,n,np,b,m,mp)
      implicit real*8 (a-h,o-z)
        PARAMETER (NMAX=50)
C------------------------------------------------------------------------------
c     INTEGER m,mp,n,np,NMAX
c     INTEGER i,icol,irow,j,k,l,ll,indxc(NMAX),indxr(NMAX),ipiv(NMAX)
c     REAL a(np,np),b(np,mp)
c     REAL big,dum,pivinv
C------------------------------------------------------------------------------
        dimension  a(np,np),b(np,mp)
        INTEGER indxc(NMAX),indxr(NMAX),ipiv(NMAX)
C------------------------------------------------------------------------------
C
      do 11 j=1,n
        ipiv(j)=0
11    continue
      do 22 i=1,n
        big=0.
        do 13 j=1,n
          if(ipiv(j).ne.1)then
            do 12 k=1,n
              if (ipiv(k).eq.0) then
                if (abs(a(j,k)).ge.big)then
                  big=abs(a(j,k))
                  irow=j
                  icol=k
                endif
              else if (ipiv(k).gt.1) then
                pause 'singular matrix in gaussj'
              endif
12          continue
          endif
13      continue
        ipiv(icol)=ipiv(icol)+1
        if (irow.ne.icol) then
          do 14 l=1,n
            dum=a(irow,l)
            a(irow,l)=a(icol,l)
            a(icol,l)=dum
14        continue
          do 15 l=1,m
            dum=b(irow,l)
            b(irow,l)=b(icol,l)
            b(icol,l)=dum
15        continue
        endif
        indxr(i)=irow
        indxc(i)=icol
        if (a(icol,icol).eq.0.) pause 'singular matrix in gaussj'
        pivinv=1./a(icol,icol)
        a(icol,icol)=1.
        do 16 l=1,n
          a(icol,l)=a(icol,l)*pivinv
16      continue
        do 17 l=1,m
          b(icol,l)=b(icol,l)*pivinv
17      continue
        do 21 ll=1,n
          if(ll.ne.icol)then
            dum=a(ll,icol)
            a(ll,icol)=0.
            do 18 l=1,n
              a(ll,l)=a(ll,l)-a(icol,l)*dum
18          continue
            do 19 l=1,m
              b(ll,l)=b(ll,l)-b(icol,l)*dum
19          continue
          endif
21      continue
22    continue
      do 24 l=n,1,-1
        if(indxr(l).ne.indxc(l))then
          do 23 k=1,n
            dum=a(k,indxr(l))
            a(k,indxr(l))=a(k,indxc(l))
            a(k,indxc(l))=dum
23        continue
        endif
24    continue
C--------------------------------------------
        return
      END
C
      SUBROUTINE MULTP(N,L,M,A,B,C,N1,L1,M1)
	IMPLICIT REAL*8 (A-H,O-Z)
	dimension A(N1,L1),B(L1,M1),C(N1,M1)
C MATRIX MULTIPLICATION     C(N-M) = A(N-L) * B(L-M)
C
       DO 20 I=1,N
       DO 20 J=1,M
	C(I,J)=0.0d0 
       DO 20 K=1,L
	C(I,J) = C(I,J) + A(I,K)*B(K,J)
 20    CONTINUE
c------------------------------------------
	RETURN
	END
C
      subroutine mprint(g,m,n,ni,nj,kt,kg)
      implicit real*8 (a-h,o-z)
      dimension g(ni,nj)
c-------------------------------------------------------------------------
c  This subroutine is written to print a matrix g.  Weiguo Sun  06/25/1993
c On entry :
c   g(m,n)  -- The matrix to be printed.
c     m,n   -- Integers to specify the actual size of g.
c    ni,nj  -- Integers to specify the dimension of g.
c       kt  -- Index for printing unit.
c       kg  -- Switch to print the tittle (you may change it) of g. 
c              kg=0, no tittle is printed.
c-------------------------------------------------------------------------
      iout=kt
c     iout=6
c
        if (kg .eq. 0) go to 10
      if (kg .eq. 1) then
        write(iout,100)
      elseif (kg .eq. 2) then
        write(iout,110)
      elseif (kg .eq. 3) then
        write(iout,120)
      elseif (kg .eq. 4) then
        write(iout,130)
      endif
c
  10  do 70 k=1,n
        n1=k*5
      if(n1.ge.n) goto 75
        n2=n1-4
      write(iout,170) (j,j=n2,n1)
        write(iout,180)
C
      do 50 i=1,m
 50   write(iout,190) i,(g(i,j),j=n2,n1)
        write(iout,200)
 70   continue
C
 75     n2=n1-4
      write(iout,170) (j,j=n2,n)
      write(iout,180)
      do 80 i=1,m
 80   write(iout,190) i,(g(i,j),j=n2,n)
      write(iout,220)
c-------------------------------------------------------
 100  format(//23x,'The input matrix A :'/)
 110  format(//23x,'The inverse of A (=> 1/A) :'/)
 120  format(//23x,'The multiplication of A * (1/A) :'/)
 130  format(///23x,'The solution vector array  X : '/)
 170  format(4x,1hj,8x,i2,7(12x,i2))
 180  format(3x,1hi)
 190  format(2x,i2,2x,6(1pe14.6))
 200  format(1h*,33x,9hContinue./)
 220  format(1h!,35x,4hEND.)
c-------------------------------------------------------
        return
      end
C
      SUBROUTINE moment(data,n,nd,ave,adev,sdev,var,skew,curt)
C---------------------------------------------------------------------
C   Given an array of data(1:n), this routine returns its mean ave,
C average deviation adev, standard deviation sdev, variance var,
C skewness skew, and kurtosis curt.
C---------------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      dimension  data(nd)
C     REAL adev,ave,curt,sdev,skew,var,data(n)
C---------------------------------------------------------------------
      if(n.le.1)pause 'n must be at least 2 in moment'
      s=0.
      do 11 j=1,n
        s=s+data(j)
11    continue
      ave=s/n
      adev=0.
      var=0.
      skew=0.
      curt=0.
      ep=0.
      do 12 j=1,n
        s=data(j)-ave
        ep=ep+s
        adev=adev+abs(s)
        p=s*s
        var=var+p
        p=p*s
        skew=skew+p
        p=p*s
        curt=curt+p
12    continue
      adev=adev/n
      var=(var-ep**2/n)/(n-1)
      sdev=sqrt(var)
      if(var.ne.0.)then
        skew=skew/(n*sdev**3)
        curt=curt/(n*var**2)-3.
      else
        pause 'no skew or kurtosis when zero variance in moment'
      endif
        return
      END
C
