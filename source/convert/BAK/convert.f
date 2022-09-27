C     program convert
c 
c 1.)    This program is to convert cross sections from square
c      Angstrom (A**2) to square Bohr (a0**2); or to convert ENERGIES.
c 2.)    To convert y's from Hartree to cm-1.
c
c----------------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      dimension x(7000),y(7000),y1(1500),y2(1500),y3(1500),y4(1500)
c----------------------------------------------------------------------
c
c     Read the data in one symmetry.
c     M  --> the type of conversion :
c            =1, convert XSCs from Angstrom**2 to Bohr**2;
c            =2, convert XSC & its error from Angstrom**2 to Bohr**2;
c            =3, convert energy from Hartree to eV;
c            =4, convert energy from Rydberg to eV;
c            =5, convert energy from Rydberg to eV, & 
c                XSCs from Angstrom**2 to Bohr**2;
c            =6, convert energy from cm-1 to Hartree;
c            =7, convert energy from Hartree to cm-1 ;
c            =8, convert XSCs from Bohr**2 to Angstrom**2 (A**2).
c
c            =11, convert y(i) from Hartree to cm-1.
c
c     N  --> the total number of lines of the data
c
c     ny --> the number of sets of y arrays 
c
c     ky --> the kth set to be used in the ny y arrays
c
c     a  --> scaling constant for the XSCs. a=1.0 for unit converting.
c
c                    Angstrom**2 = 1D-16 (cm)**2
c              Convert  1D-16 cm2/sr  to  (ao)**2 ,  a = 1.0      ;
c              Convert  1D-18 cm2/sr  to  (ao)**2 ,  a = 0.01     ;
c              Convert  1D-20 cm2/sr  to  (ao)**2 ,  a = 0.0001   ;
c              Convert  1D-22 cm2/sr  to  (ao)**2 ,  a = 0.000001 .
c
c              Convert  (a0)**2 to  1D-16 cm2/sr ,   a = 1.0      ;
c              Convert  (a0)**2 to  1D-18 cm2/sr ,   a = 100      ;
c              Convert  (a0)**2 to  1D-20 cm2/sr ,   a = 10000    ;
c              Convert  (a0)**2 to  1D-22 cm2/sr ,   a = 1000000  .
c
c---------------------------------------------------------------------------
        conv=0.280028561d0
        aucm=219474.63067d0
c
c     read (5,700) M,N
      read (5,*) M,N,ny,ky,a
c----------------------------------
        if (M .eq. 6) goto 80
        if (M .eq. 7) goto 90
c----------------------------------
        if (M .eq. 11) goto 200
c----------------------------------
c
      do 5 i=1,N 
      if (ny .eq. 0) then
          read (5,*) x(i)
	elseif (ny .eq. 1) then
          read (5,*) x(i),y1(i)
	elseif (ny .eq. 2) then
          read (5,*) x(i),y1(i),y2(i)
	elseif (ny .eq. 3) then
          read (5,*) x(i),y1(i),y2(i),y3(i)
	elseif (ny .eq. 4) then
          read (5,*) x(i),y1(i),y2(i),y3(i),y4(i)
	endif
          if (ky .eq. 1) y(i)=y1(i)
          if (ky .eq. 2) y(i)=y2(i)
          if (ky .eq. 3) y(i)=y3(i)
          if (ky .eq. 4) y(i)=y4(i)
   5  continue
c----------------------------------
      if (M .eq. 2) goto 20
      if (M .eq. 3) goto 30
      if (M .eq. 4) goto 50
      if (M .eq. 5) goto 70
      if (M .eq. 8) goto 150
c----------------------------------
      do 10 i=1,N 
	y(i) =a*y(i)/conv
        write(6,720) x(i),y(i)
   10 continue
	go to 990
c----------------------------------
C
   20 do 25 i=1,N
c       read (5,*) x(i),y(i),y2(i)
	y(i) =a*y(i)/conv
        y2(i)=a*y2(i)/conv
        write(6,720) x(i),y(i),y2(i)
   25 continue
	go to 990
C
c----------------------------------
   30 do 40 i=1,N   
      x(i)=x(i)*27.21139618
      write(6,720) x(i),y(i)
   40 continue
	go to 990
c----------------------------------
c
   50 do 60 i=1,N      
      x(i)=x(i)*27.21139618/2.0 
      write(6,720) x(i),y(i)
   60 continue
	go to 990
c----------------------------------
c
   70 do 75 i=1,N
      x(i)=x(i)*27.21139618/2.0 
      y(i)=a*y(i)/conv
      write(6,720) x(i),y(i)
   75 continue
	go to 990
c----------------------------------
c
   80 do i=1,N
        read (5,*) x(i)
        x(i)=x(i)/219474.63067
        write(6,720) x(i)
      enddo
	go to 990
c----------------------------------------------------
c
   90 do i=1,N
        read (5,*) x(i)
        x(i)=x(i)*219474.63067
        write(6,720) x(i)
      enddo
	go to 990
c----------------------------------------------------
  150 do 155 i=1,N
        if (ny .eq. 1) then
          y1(i)=a*conv*y1(i)
          write(6,720) x(i),y1(i)
        elseif (ny .eq. 2) then
          y1(i)=a*conv*y1(i)
          y2(i)=a*conv*y2(i)
          write(6,720) x(i),y1(i),y2(i)
        elseif (ny .eq. 3) then
          y1(i)=a*conv*y1(i)
          y2(i)=a*conv*y2(i)
          y3(i)=a*conv*y3(i)
          write(6,720) x(i),y1(i),y2(i),y3(i)
        elseif (ny .eq. 4) then
          y1(i)=a*conv*y1(i)
          y2(i)=a*conv*y2(i)
          y3(i)=a*conv*y3(i)
          y4(i)=a*conv*y4(i)
          write(6,730) x(i),y1(i),y2(i),y3(i),y4(i)
        endif
  155 continue
	go to 990
c----------------------------------------------------
c
  200 do i=1,N
        read (5,*) x(i), y(i)
          y(i)=y(i)*aucm
        write(6,740) x(i), y(i)
      enddo
C
c----------------------------------------------------
  700 format(2i4)
C 720 format(f10.5,2x,f15.10,2x,f15.10)
C 720 format(F10.5,2x,1PE18.10,2x,1PE18.10)
  720 format(F10.5,3(2x,1PE18.10))
  730 format(F10.5,4(2x,1PE14.6))
  740 format(F10.5,1x,F19.8)
c----------------------------------------------------
  990 stop
      end

