C                    compdG.f
C The program is to compare the Evib and the Evib_exp 
C 
C num -------  the number of compared deltaG .
C naucm -----  = 0 , read Evib_cal in a.u.
C              = 1 , read Evib_cal in cm_1

C 1 a.u. = 219474.63067 cm_1

  	program main
  	parameter ( autocm = 219474.63067 )
  	parameter ( NMAX = 100)
C 	implicit real*8 (a-h,o-z)
  	real*8 Evib(NMAX),Evib_exp(NMAX),dg_exp(NMAX)
 	read(5,*)num, naucm1,naucm2

 	do 10 i = 1, num
  	   read(23,*)Evib(i)
  	   read(21,*)dg_exp(i)
10 	continue
  
  	Evib_exp(1) = dg_exp(1)
  	do 20 i = 2, num
  	   Evib_exp(i) = dg_exp(i) + Evib_exp(i-1)
20 	continue
  	if ( naucm1 .eq. 0 ) then
 	   do 30 i = 1, num
  	      Evib(i) = Evib(i)*autocm
30 	   continue
  	endif
  	if ( naucm2 .eq. 0 ) then
 	   do 35 i = 1, num
  	      Evib_exp(i) = Evib_exp(i)*autocm
35 	   continue
  	endif

   	write(14,100) 
   	write(14,*) 
  	do 40 i = 1, num
  	   write(14,200)Evib_exp(i), Evib(i), Evib_exp(i)-Evib(i),
     *       dabs(Evib_exp(i)-Evib(i))/Evib_exp(i)
40 	continue
100 	format(2x,"   Evib_exp       Evib_cal       Evib_e-Evib_c   |deltaE|/Evib_exp")
200 	format(2x,4(1pe15.6))
  	end  
