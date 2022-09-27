C                    compdG.f
C   The program is to compare the (energies) deltaG_cal with
c the deltaG_theory or the deltaG_exp .
C 
C num -------  the number of compared deltaG .
C naucm -----  = 0 , read Evib_cal in a.u.    from fort.23
C              = 1 , read Evib_cal in cm_1
C nexpt -----  = 0 , read experimental energies from fort.21 in a.u.
C              = 1 , read in cm_1 from fort.21
C alam ------  = Lambda,  is the variational adjustable 
C                             parameter in V_ecm(R).
C aver1 -----  the arithmetical average of error
C aver2 -----  the     weighted average of error
C
C============================================================== 
c
C 1 a.u. = 219474.63067 cm_1
c
  	program main
  	implicit real*8(a-h,o-z)
  	parameter ( NMAX = 100, autocm = 219474.63067 )
  	dimension  Evib(NMAX),dg_cal(NMAX,NMAX),dg_exp(NMAX,NMAX)
  	dimension  dgdif(NMAX),dgave(NMAX)
c
  	  read(5,*) num, naucm1,naucm2, nexpt, alam

 	do 10 i = 1, num
  	   read(23,*) Evib(i)
  	   read(21,*) dg_exp(1,i)
10 	continue
  
 	  dg_cal(1,1) = Evib(1)
  	do 20 i = 2, num
  	   dg_cal(1,i) = Evib(i) - Evib(i-1)
20 	continue
c---
  	if ( nexpt .eq. 0 ) then
 	   do i = 1, num
  	      dg_exp(1,i) = dg_exp(1,i)*autocm
           enddo
  	endif
c---
             write(14,200)
  	if ( naucm1 .eq. 0 ) then
             write(14,250)
 	   do 30 i = 1, num
  	      dg_cal(1,i) = dg_cal(1,i)*autocm
30 	   continue
  	else
             write(14,300)
  	endif

  	if ( naucm2 .eq. 0 ) then
 	   do 35 i = 1, num
  	      dg_exp(1,i) = dg_exp(1,i)*autocm
35 	   continue
  	endif

  	   aver1 = 0.0d0
  	   aver2 = 0.0d0
  	do 70 i = 1, num
  	   dgdif(i) = dg_exp(1,i)-dg_cal(1,i)
  	   dgave(i) = dabs( dgdif(i)/dg_exp(1,i) )
  	   aver1 = aver1 + dgave(i)
  	   aver2 = aver2 + dgave(i)*i
70 	continue
  	   aver1 = aver1/num
  	   aver2 = 2.0d0*aver2/(num*(num+1))
c
        do 40 j = 1, num
           write(14,800) dg_exp(1,j), dg_cal(1,j), 
     *                   dgdif(j), dgave(j)
40      continue
           write(14,350) aver1,aver2
c-----
C          write(15,320) 
        if (naucm .eq. 0) then
            write(14,300)
          do j = 1, num
            write(14,800) dg_exp(1,j)/autocm, dg_cal(1,j)/autocm, 
     *                   dgdif(j)/autocm, dgave(j)
          enddo
            write(14,350) aver1,aver2
            write(15,360) alam, aver1,aver2
        else
c-
            write(14,250)
          do j = 1, num
            write(14,800) dg_exp(1,j)*autocm, dg_cal(1,j)*autocm, 
     *                   dgdif(j)*autocm, dgave(j)
          enddo
            write(14,350) aver1,aver2
            write(15,360) alam, aver1,aver2
        endif
C
c--------------------------------------------------------------------
200     format(///14x,'The comparision of the energies delta_G :',/)
250     format(//9x,"E_exp",9x,"E_cal",6x,"E_exp - E_cal",
     #2x,"|E_e - E_c|/E_e",/25x,'( in cm-1 unit )')
300     format(//9x,"E_exp",9x,"E_cal",6x,"E_exp - E_cal",
     #2x,"|E_e - E_c|/E_e",/25x,'( in a.u. unit )')
C320    format(//4x,"DifE_ave = |E_e-E_c|/E_e  is the average of ',
c    #'energy percentage errors ',
c    #/4x,'Lambda is the variational adjustable parameter in V(R)',/
c    #/2x,'DifE_ave(cm-1)',3x,'DifE_ave(a.u.)',5x,'Lambda',/)
350     format(/7x,"The arithmetical average of |E_e-E_c|/E_e  is  :",
     #  1pe16.8,/7x," The  weighted average of |E_e-E_c|/E_e is  :",
     #  1pe16.8)
360     format(1x,f9.5,4x,2(1pe16.8,1x),/)
800 	format(2x,4(1pe15.6))
  	END  
