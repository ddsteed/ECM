C             calVIB.f
C  The program is to calculate the vibrational spectrum .
C
C  number ---  the number of the bound state used;
C
  	program main
  	parameter ( NMAX = 100)
  	implicit real*8(a-h,o-z)
  	real*8 vibspec(NMAX),Evib(NMAX,NMAX)
  	real*8 FG(NMAX),d1FG(NMAX),d2FG(NMAX),d3FG(NMAX)
  	read(5,*) number
  	read(5,*) we, wexe,weye

    	do 10 i = 1, number
  	   read(23,*) Evib(i,1)
  	   FG(i) = Evib(i,1)
10 	continue
   	do 20 i = 1, number-1 
  	   d1FG(i) = FG(i+1) - FG(i)
  	   write(21,*)"d1FG(",i,") = ",d1FG(i)
20  	continue
  	do 30 i = 1, number-2
  	   d2FG(i) = d1FG(i+1) - d1FG(i)
30  	continue
  	do 40 i = 1, number-3
  	   d3FG(i) = d2FG(i+1) - d2FG(i)
40 	continue

  	vibspec(3) = 0.0d0
   	do 50   i = 1, number-3
  	   vibspec(3) = vibspec(3) + d3FG(i)
50 	continue
  	vibspec(3) = vibspec(3)/(6.0d0*(number-3))

  	vibspec(2) = 0.0d0
  	do 60 i = 1, number-2
  	   vibspec(2) = vibspec(2) + 6.0d0*vibspec(3)*(i+0.5d0) - d2FG(i)
60 	continue
  	vibspec(2) = vibspec(2)/(2.0d0*(number-2))

  	vibspec(1) = 0.0d0
  	do 70 i = 1, number-1
  	   vibspec(1) = vibspec(1) + d1FG(i) + 2.0d0*vibspec(2)*(i-0.5d0)
  	   vibspec(1) = vibspec(1) - vibspec(3)*(3.0d0*(i-0.5d0)**2+1.0d0/12.0d0)
70  	continue
  	vibspec(1) = vibspec(1)/(number-1)

  	write(14,*)
        write(14,*) " The value of spectra as follows : "
  	write(14,*)
  	do 80 i = 1, number
           write(14,100)"spec(",i,") = ",vibspec(i)
80 	continue
  	write(14,*)
  	write(14,200)"We  =",We,"  We_cal  =",vibspec(1),"  Edif%=",(We-vibspec(1))/We
  	write(14,200)"Wexe=",Wexe,"  Wexe_cal=",vibspec(2),"  Edif%=",(Wexe-vibspec(2))/Wexe
  	write(14,200)"Weye=",Weye,"  Weye_cal=",vibspec(3),"  Edif%=",(Weye-vibspec(3))/Weye
100 	format(1x,a,i2,a,1pe15.6)
200 	format(1x,a,1pe15.6,a,1pe15.6,a,1pe15.6)
  	end
