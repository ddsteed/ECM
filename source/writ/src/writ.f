C     program writ.f
c 
c     This program is to do one of the following :
c 1.)   read n line m column data and then write them separately to n
c     sets (fort.11, ..., fort.1n).
c 2.)   read in MN=N1*N2 data pairs, and then write them to N1 files
c 3.)   read in MN=N1*N2 data pairs, sum y(i) of the N1 sets, then
C     write the x(i) and the summed y(i) out.
c 4.)   read in MN=N1*N2 data pairs, write x and N1 sets of y's :
c           x(i)   y1(i)   y2(i)   y3(i)   y4(i) 
c 5.)   read in MN=N1*N2 data pairs, write x and N1 sets of y's & SUMMED y's:
c           x(i)   y1(i)   y2(i)   y3(i)   y4(i)    Y-sum
c  .......................
c 6.)   read in x & y, then write out the pair which satisfies x=a .
c 7.)   read in x(i) & y(i) for every z(k), then write out z(k) & y(k)
c     pairs for every x(i).
c 8.)   read in  x(i), y1(i), y2(i), then write out y1(i) & y2(i) pairs.
c 
c 9.) For N3 < 10 :
c       read in from fort.5 :  nx(i),y1(i),y2(i)              if N4=2
c       read in from fort.5 :  nx(i),y1(i),y2(i),y3(i)        if N4=3
c       read in from fort.5 :  nx(i),y1(i),y2(i),y3(i),y4(i)  if N4=4
c             & read in nx(i), y5(i)  from fort.3 (i=1,N2;  N2 >= N1),
c       then write out :
c                   nx(i), y1(i), y2(i), y5(i), dy5,  if N3 = 1;
c                   nx(i), y1(i), y5(i), y2(i), dy2,  if N3 = 2;
c                   nx(i), y5(i), y1(i), y2(i), dy2,  if N3 = 3;
c
c       nx(i), y15(i), y1(i), y2(i), y5(i), Dy5(i), y52(i),  if N3 = 4;
c       nx(i), y12(i), y1(i), y5(i), y2(i), Dy2(i), y25(i),  if N3 = 5;
c       nx(i), y52(i), y5(i), y1(i), y2(i), Dy2(i), y21(i),  if N3 = 6.
c
c             [ dy# = y#(i) - y#(i-1) ;  y52(i) = y5(i) - y2(i) ]
c
c     For N3 > 10 :
c       read in  nx(i), y1(i), y2(i), y3(i), then write out :
c                   nx(i), y1(i), dy1(i)  if N3 = 11;
c                   nx(i), y2(i), dy2(i)  if N3 = 12;
c                   nx(i), y3(i), dy3(i)  if N3 = 13;
c
c     For N3 > 20 :
c       read in  nx(i), y1(i), y2(i), y3(i), then write out :
c                   nx(i), dy1(i)  if N3 = 21;
c                   nx(i), dy2(i)  if N3 = 22;
c                   nx(i), dy3(i)  if N3 = 23.
c
c
c 10.)  M=10:  (Fill ZEROs to some array elements)
c               N1 -- the number of lines of the data.
c              For N2=0,  read in x(i) & y2(i), then ZERO y2(i) & 
c                                                    ----
c                         write x(i) & y2(i) in fort.8 ; 
c              For N2=1,  read in y1(i)[==x(i)] & y2(i) from fort.8 &
c                         read in  xx(i) &  yy(i) from fort.5 ;
c                         if y1(i) .lt. xx(i)  {fill ZEROs},
c                                               ----------
c                             put [ y1(i),y2(i) ] into [ x(i), y3(i) ];
c                         then 
c                             paste [ xx(i),yy(i) ] to [ x, y3 ]
c                         and  write out (x, y3).
c
c 11.)  M=11:   To add up static & MAP polarization potentials :
c                   V_stmap = V_static + V_map
c
c 12.)  M=12:   read in x(i), y1(i);   i=1,N1.   Then write out :
c               x(i), y1(i)-a1     for i =< N2     
c               x(i), y1(i)-a2     for i  > N2
c               
c 13.)  M=13:   read in nx(i), y1(i), y2(i), y3(i); then write out :
c                 y1(i), y3(i)    i=1,N1
c                 y2(i), y3(i)    i=N1,1,-1
c               Change y1 & y2 from Anstrom to ao  if N2 = 1.
c
c          read in  y1(i), y3(i) &  Change  y3 = y3 - a  if N2 = 2.
c
c          read in  y1(i), y3(i) &  Change y1 from Anstrom to ao,
c              and     Change  y3 = y3 - a  if N2 = 3.
c
c=========================================================================
      implicit real*8 (a-h,o-z)
      parameter (nmax=800,kmax=2000)
c The next line is for purpose 1.)
      dimension x(nmax), x1(nmax),y1(nmax),y2(nmax),y3(nmax),y4(nmax)
C     dimension y5(nmax),y6(nmax),y7(nmax),y8(nmax)
      dimension y5(nmax),y6(nmax),y7(nmax),y8(nmax),nx(nmax)
c The next line is for purpose 2.)
      dimension xx(kmax),yy(kmax)
c The next two lines are for purpose 11.)
      dimension ri(20),rf(20),rs(20),c1(10),c2(10),lab(30)
      dimension title(72), titl0(72)
c-------------------------------------------------------------------------
c     Read the data in one symmetry.
c     M = 1, for purpose 1.); =2 for 2.); =3 for 3.); =4 for 4.);
c       = 6, for purpose 6.); =7 for 7.); =8 for 8.), ...
c
c  For 1.), 8.) :
c      read (5,*) N
c     N  --> the total number of lines of the data
c
c  For 2.), 3.), 4.), 5.), 7.) :
c     N1 --> The # of data sets in the input data
c     N2 --> The # of data pairs in each data set
c
c  For 6.) :
c      read (5,*) N1,N2,en,a
c     en --> Energy in Rydberg;   a --> Angle in degree.
c
c  For 9.) :
c      read (5,*) N1,N2,N3
c     N1 --> The # of data pairs in fort.5;
c     N2 --> The # of data pairs in fort.3;
c     N3 --> A switch number for printting (see above).
c
c
c-------------------------------------------------------------------------
        read (5,*) M
c
c--------------------------------
        anstoao = 0.529177249
c--------------------------------
      if (M .eq. 6)  go to 150
      if (M .eq. 7)  go to 170
      if (M .eq. 8)  go to 200
      if (M .eq. 9)  go to 220
      if (M .eq. 10) go to 240
      if (M .eq. 11) go to 260
      if (M .eq. 12) go to 280
      if (M .eq. 13) go to 300
c--------------------------------
      if (M .eq. 1) then
c For purpose 1.)
c       read (5,600) N
        read (5,*) N
        do 10 i=1,N 
          read (5,*) x(i),y1(i),y2(i),y3(i),y4(i),y5(i),y6(i),
     # y7(i),y8(i)
c         read (5,*) x(i),y1(i),y2(i),y3(i),y4(i),y5(i)
c         read (5,*) x(i),y1(i),y2(i),y3(i),y4(i)
   10   continue
c-------------------------------------------------------------------------
C         a=1.0d0
          a=27.21139618
        do 20 i=1,N 
        write(11,620) x(i),y1(i)*a
        write(12,620) x(i),y2(i)*a
        write(13,620) x(i),y3(i)*a
        write(14,620) x(i),y4(i)*a
C
        write(15,620) x(i),y5(i)*a
        write(16,620) x(i),y6(i)*a
        write(17,620) x(i),y7(i)*a
        write(18,620) x(i),y8(i)*a
   20   continue
	  go to 990
c
      endif
c-------------------------------------------------------------------------
c For purpose 2.)
      if (M .eq. 2) then
C       read (5,600) N1,N2
        read (5,*) N1,N2
	  N3=N1*N2
          write(6,*)'  M,N1,N2,N3= ',M,N1,N2,N3
        do 40 i=1,N3
          read (5,*) xx(i),yy(i)
c         read (5,610) xx(i),yy(i)
   40   continue
c
	    i1=18
	do 60 i=1,N1
	    i1=i1 + 2
	  do 50 k=1,N2
	    k1=(i-1)*N2 + k
            write(i1,630) xx(k1),yy(k1)
   50     continue
   60   continue
	  go to 990
      endif
C
c-------------------------------------------------------------------------
c For purpose 3.)
      if (M .eq. 3) then
c       read (5,600) N1,N2
        read (5,*) N1,N2
	  MN=N1*N2
	do 65 i=1,MN
          read (5,*) xx(i),yy(i)
C         read (5,610) xx(i),yy(i)
  65   continue
c
	do 80 i=1,N2
  	  y1(i)=0.0d0
	 do 75 k=1,N1
	  k1=(k-1)*N2 + i
	  y1(i)=y1(i) + yy(k1)
  75     continue
	  write(6,640) xx(i),y1(i)
  80   continue
	   go to 990
      endif
C
c-------------------------------------------------------------------------
c For purpose 4.) & 5.)
      if (M .eq. 4 .or. M .eq. 5) then
        read (5,*) N1,N2
	  MN=N1*N2
	    m2=2*N2
	    m3=3*N2
	    m4=4*N2
        if (M .eq. 4) then
	  if (N1 .eq. 2) write(6,660) 
	  if (N1 .eq. 3) write(6,665) 
	  if (N1 .eq. 4) write(6,670) 
        else
	  if (N1 .eq. 2) write(6,675) 
	  if (N1 .eq. 3) write(6,680) 
	  if (N1 .eq. 4) write(6,685) 
	endif
C
        do 90 i=1,MN
	    read (5,*) xx(i),yy(i)
	  if (i .le. N2) y1(i)=yy(i)
	  if (i .gt. N2 .and. i .le. m2) y2(i-N2)=yy(i)
	  if (i .gt. m2 .and. i .le. m3) y3(i-m2)=yy(i)
	  if (i .gt. m3 .and. i .le. m4) y4(i-m3)=yy(i)
   90   continue
c
        if (M .eq. 5) then
	  do 95 i=1,N2
            y8(i)=0.0d0
          do 95 k=1,N1
            k1=(k-1)*N2 + i
            y8(i)=y8(i) + yy(k1)
   95     continue
        endif
C
        if (M .eq. 4) then
         do 105 i=1,N2
	  if (N1 .eq. 2) write(6,690) xx(i),y1(i),y2(i)
	  if (N1 .eq. 3) write(6,690) xx(i),y1(i),y2(i),y3(i)
	  if (N1 .eq. 4) write(6,690) xx(i),y1(i),y2(i),y3(i),y4(i)
  105    continue
        else
         do 110 i=1,N2
          if (N1 .eq. 2) write(6,700)xx(i),y1(i),y2(i),y8(i)
          if (N1 .eq. 3) write(6,700)xx(i),y1(i),y2(i),y3(i),y8(i)
          if (N1 .eq. 4)write(6,700)xx(i),y1(i),y2(i),y3(i),y4(i),y8(i)
  110    continue
        endif
	 go to 990
      endif
c-------------------------------------------------------------------------
c For purpose 6.)
  150    read (5,*) N1,N2,en,a
             N3=1
	     ee=13.60569809*en
C     Next 8 reads corresponds to the format of my dcs files ONLY !!
  155      read (5,*) 
           read (5,650) title
           read (5,650) title
           read (5,*) 
           read (5,650) title
           read (5,*) 
           read (5,650) title
           read (5,*) 
       do 160 i=1,N2
	 read (5,*) xx(i),yy(i)
	   if (N3 .eq. N1 .and. xx(i) .eq. a) then
	     write(6,625) ee,yy(i)
	   endif
  160   continue
	   if (N3 .lt. N1) then
	     N3=N3+1
	       go to 155
           endif
	go to 990
c-------------------------------------------------------------------------
c For purpose 7.)
  170    read(5,*) N1,N2
c ---- y1(k) -> the internuclear distance; x(i) -> the radius r;
c ---- yy(j) -> the potential or polarizability or any data you input.
       do 175 k=1,N1
           read (5,*) y1(k)
	 do i=1,N2
	   read (5,*) x(i),yy(i+N2*(k-1))
	 enddo
  175  continue          
             write(6,710) 
         do 180 i=1,N2
             write(6,720) x(i)
           do k=1,N1
             write(6,610) y1(k),yy(i+N2*(k-1))
           enddo
             write(6,725) 
             ay=0.0d0
           do k=1,N1
             dy=0.0d0
             if (k .gt. 1) dy=yy(i+N2*(k-1)) - yy(i+N2*(k-2))
             write(6,730) y1(k),yy(i+N2*(k-1)),dy
             ay=ay + dy
           enddo
             write(6,740) ay/(N1-1)
  180    continue
	   go to 990
c-------------------------------------------------------------------------
c For purpose 8.)
  200    read(5,*) N
c
         do i=1,N
           read (5,*)    x(i), y1(i), y2(i)
C          read (5,*)   nx(i), y1(i), y2(i)
	   write(6,750) y1(i),y2(i)
         enddo
c
	   go to 990
c-------------------------------------------------------------------------
c For purpose 9.)
  220    read(5,*) N1, N2, N3, N4
             write(6,810) N1, N2, N3
           do i=1,N1
             if (N4 .le. 2) then
               read(5,*) nx(i),y1(i),y2(i)
             elseif (N4 .eq. 3) then
               read(5,*) nx(i),y1(i),y2(i),y3(i)
             elseif (N4 .eq. 4) then
               read(5,*) nx(i),y1(i),y2(i),y3(i),y4(i)
             endif
           enddo
             write(6,712) 
           if (N3 .eq. 1) write(6,812) 
           if (N3 .eq. 2) write(6,814) 
           if (N3 .eq. 3) write(6,816) 
           if (N3 .eq. 4) write(6,820) 
           if (N3 .eq. 5) write(6,822) 
           if (N3 .eq. 6) write(6,824) 
c
           if (N3 .gt. 10 .and. N3 .lt. 20)  goto 225
           if (N3 .gt. 20)  goto 230
c---
         do i=1,N2
             read(3,*) nx(i),y5(i)
           if (N3 .eq. 1) then
             if (i .eq. 1) then
               write(6,830) nx(i),y1(i),y2(i),y5(i)
             elseif (i .le. N1) then
               dy = y5(i) - y5(i-1)
               write(6,830) nx(i),y1(i),y2(i),y5(i),dy
             elseif (i .gt. N1) then
               write(6,832) nx(i),y2(i)
             endif
           endif
c-
           if (N3 .eq. 2) then
             if (i .eq. 1) then
               write(6,830) nx(i),y1(i),y5(i),y2(i)
             elseif (i .le. N1) then
               dy = y2(i) - y2(i-1)
               write(6,830) nx(i),y1(i),y5(i),y2(i),dy
             elseif (i .gt. N1) then
               write(6,832) nx(i),y5(i)
             endif
           endif
c-
           if (N3 .eq. 3) then
             if (i .eq. 1) then
               write(6,830) nx(i),y5(i),y1(i),y2(i)
             elseif (i .le. N1) then
               dy = y2(i) - y2(i-1)
               write(6,830) nx(i),y5(i),y1(i),y2(i),dy
             elseif (i .gt. N1) then
               write(6,832) nx(i),y1(i)
             endif
           endif
c--
           if (N3 .eq. 4) then
             if (i .le. N1) then
                  dy = y5(i) - y5(i-1)
                 dyL = y5(i) - y1(i)
                 dyR = y5(i) - y2(i)
               if (N4 .eq. 1) then
                 write(6,834) nx(i),dyL,y1(i),y2(i),y5(i),dy,dyR
               elseif (N4 .eq. 2) then
c  Use format 835 for PERCENT error
C                dyL = 100.0*abs(dyL)/y5(i)
C                dyR = 100.0*abs(dyR)/y5(i)
C                write(6,835) nx(i),dyL,y1(i),y2(i),y5(i),dy,dyR
               endif
             elseif (i .gt. N1) then
               write(6,836) nx(i),y2(i)
             endif
           endif
c-
           if (N3 .eq. 5) then
             if (i .le. N1) then
                  dy = y2(i) - y2(i-1)
                 dyL = y2(i) - y1(i)
                 dyR = y2(i) - y5(i)
               if (N4 .eq. 1) then
                 write(6,834) nx(i),dyL,y1(i),y5(i),y2(i),dy,dyR
               elseif (N4 .eq. 2) then
c  Use format 835 for PERCENT error
                 dyL = 100.0*abs(dyL)/y2(i)
                 dyR = 100.0*abs(dyR)/y2(i)
                 write(6,835) nx(i),dyL,y1(i),y5(i),y2(i),dy,dyR
               endif
             elseif (i .gt. N1) then
                 write(6,836) nx(i),y5(i)
             endif
           endif
c-
           if (N3 .eq. 6) then
             if (i .le. N1) then
                  dy = y2(i) - y2(i-1)
                 dyL = y2(i) - y5(i)
                 dyR = y2(i) - y1(i)
               if (N4 .eq. 1) then
                 write(6,834) nx(i),dyL,y5(i),y1(i),y2(i),dy,dyR
               elseif (N4 .eq. 2) then
c  Use format 835 for PERCENT error
                 dyL = 100.0*abs(dyL)/y2(i)
                 dyR = 100.0*abs(dyR)/y2(i)
                 write(6,835) nx(i),dyL,y5(i),y1(i),y2(i),dy,dyR
               endif
             elseif (i .gt. N1) then
               write(6,836) nx(i),y1(i)
             endif
           endif
c
         enddo
	   go to 990
c-------------------------------------------
  225      write(6,632) 
       IF (N3 .eq. 11) THEN
           write(6,834) nx(1), y1(1)
         do i=2,N1
             dy = y1(i) - y1(i-1)
           write(6,834) nx(i), y1(i), dy
         enddo
       ELSEIF (N3 .eq. 12) THEN 
           write(6,834) nx(1), y2(1)
         do i=2,N1
             dy = y2(i) - y2(i-1)
           write(6,834) nx(i), y2(i), dy
         enddo
       ELSEIF (N3 .eq. 13) THEN 
           write(6,834) nx(1), y3(1)
         do i=2,N1
             dy = y3(i) - y3(i-1)
           write(6,834) nx(i), y3(i), dy
         enddo
       ENDIF
c
	   go to 990
c----
  230      write(6,634) 
       IF (N3 .eq. 21) THEN
           write(6,834) nx(1)
         do i=2,N1
             dy = y1(i) - y1(i-1)
           write(6,834) nx(i), dy
         enddo
       ELSEIF (N3 .eq. 22) THEN 
           write(6,834) nx(1)
         do i=2,N1
             dy = y2(i) - y2(i-1)
           write(6,834) nx(i), dy
         enddo
       ELSEIF (N3 .eq. 23) THEN 
           write(6,834) nx(1)
         do i=2,N1
             dy = y3(i) - y3(i-1)
           write(6,834) nx(i), dy
         enddo
       ENDIF
c
	   go to 990
c-------------------------------------------------------------------------
c For purpose 10.)
  240      read(5,*) N1,N2
        if (N2 .eq. 0) then
             write(8,600) N1
           do i=1,N1
             read(5,*) x(i),y2(i)
               y2(i)=0.0d0
             write(8,620) x(i),y2(i)
           enddo
        else
           do i=1,N1
             read(5,*) xx(i),yy(i)
           enddo
             read(8,*) NN
           do i=1,NN
             read(8,*) y1(i),y2(i)
           enddo
             N3=NN-N1
            do i=1,N3
              x(i)=y1(i)
             y3(i)=y2(i)
            enddo
          do i=1,N1
              j = i + N3
            x(j)=xx(i)
           y3(j)=yy(i)
          enddo
             write(6,600) NN
           do i=1,NN
             write(6,620) x(i),y3(i)
           enddo
        endif
	   go to 990
c-------------------------------------------------------------------------
c For purpose 11.)
c
  260   read(3,770) title
        read(4,770) title
        read(3,*)   nrg
        read(4,*)   nrg
        read(5,770) titl0
        read(5,*)   labst, labmap
          labda = labst - labmap
        write(8,770) titl0
        write(8,775) nrg
      do i=1,nrg
         read(3,780)  ri(i), rf(i), rs(i)
         read(4,780)  ri(i), rf(i), rs(i)
        write(8,780)  ri(i), rf(i), rs(i)
      enddo
         read(3,*)   natom
         read(4,*)   natom
        write(8,775) natom
      do i=1,natom
           read(3,*)   c1(i), c2(i)
           read(4,*)   c1(i), c2(i)
          write(8,790) c1(i), c2(i)
      enddo
c
      do k=1,labmap
           read(3,*)   lab(k), ndata
           read(4,*)   lab(k), ndata
          write(8,775) lab(k), ndata
        do i=1,ndata
          read(3,800)  x(i), y1(i)
          read(4,800)  x(i), y2(i)
            y3(i) = y1(i) + y2(i)
          write(8,800) x(i), y3(i) 
        enddo
      enddo
c---
      do k=1,labda
           read(3,*)   lab(k+labmap), ndata
          write(8,775) lab(k+labmap), ndata
        do i=1,ndata
          read(3,800)  x(i), y1(i)
          write(8,800) x(i), y1(i) 
        enddo
      enddo
c
	   go to 990
c-------------------------------------------------------------------------
c For purpose 12.)
  280      read(5,*) N1,N2,a1,a2
          write(6,850) N2,N2,a1,a2
        do i=1,N1
C           read(5,*)   x(i), y1(i)
            read(5,*)  nx(i), y1(i)
          if (nx(i) .le. N2) then
            write(6,860) nx(i), y1(i)-a1 
C           write(6,860)  x(i), y1(i)-a1 
          else
            write(6,860) nx(i), y1(i)-a2 
C           write(6,860)  x(i), y1(i)-a2 
          endif
        enddo
c
	   go to 990
c-------------------------------------------------------------------------
c For purpose 13.)
  300      read(5,*) N1, N2, a
        do i=1,N1
          if (N2 .lt. 2) then
            read(5,*) nx(i), y1(i), y2(i), y3(i)
          elseif (N2 .gt. 1 .and. N2 .le. 3) then
            read(5,*) y1(i), y3(i)
              y3(i) = y3(i) - a
            if (N2 .eq. 3) then
              y1(i) = y1(i)/anstoao
            endif
          endif
          if (N2 .eq. 1) then
            y1(i) = y1(i)/anstoao
            y2(i) = y2(i)/anstoao
          endif
          if (y1(i) .ne. y1(i-1)) then
            write(6,865)  y1(i), y3(i)
          endif
        enddo
        if (N2 .lt. 2) then
          do i=N1,1,-1
            if (y2(i) .ne. y2(i-1)) then
              write(6,865)  y2(i), y3(i)
            endif
          enddo
        endif
c-------------------------------------------------------------------------
  600 format(2i4)
  610 format(f10.5,1PE23.15)
  620 format(f10.5,2x,f16.10)
  625 format(2x,f10.5,1x,E16.8)
c 630 format(f10.5,2x,f22.16)
  630 format(f10.5,2x,E14.6)
  632 format(/'nx(i)  y1(i)',3x,'y(i)-y(i-1)',//)
  634 format(/'nx(i) y(i)-y(i-1)',//)
  640 format(f10.5,2x,1PE20.13)
  650 format(A72)
  660 format(/3x,'  x(i)        y1(i)         y2(i)',/)
  665 format(/3x,'  x(i)        y1(i)         y2(i)          y3(i)',/)
  670 format(/3x,'  x(i)        y1(i)         y2(i)          y3(i)
     #         y4(i)',/)
  675 format(/3x,' x(i)       y1(i)        y2(i)        Y-SUM',/)
  680 format(/3x,' x(i)       y1(i)        y2(i)         y3(i)'
     #,'        Y-SUM',/)
  685 format(/3x,' x(i)       y1(i)        y2(i)         y3(i)'
     #,'        y4(i)        Y-SUM',/)
  690 format(f10.5,4(1PE15.6))
  700 format( f8.4,5(1PE14.6))
  710 format(//3x,' The rearranged data are : ')
  712 format(//10x,' The reprinted data are : ')
  720 format(//3x,'    For radius  r = ',f10.6/)
  725 format(/3x,'  x(k)          y(k)',15x,'dif[y(k)-y(k-1)]',/)
  730 format(f10.5,2(1PE23.15))
  740 format(/3x,'  The average difference of Y is,  AVEdy =',1PE23.15)
  750 format(2(1PE18.10,2x))
c
  770 format(72a1)
  775 format(2i5)
  780 format(3(f10.5))
  790 format(2(e15.6))
  800 format(f10.5,e23.16)
c
  810 format(//4x,'Read in N1(=',i3,') rows of data from unit 5; ',/
     #4x,'Read in N2(=',i3,') rows of data from unit 3; ',/
     #4x,'           [ N2 >= N1 ]     ',//
     #4x,'( Print  nx(i) y1(i) y2(i) y5(i) if N3 = 1; ',/
     #4x,'         nx(i) y1(i) y5(i) y2(i) if N3 = 2; ',/
     #4x,'         nx(i) y5(i) y1(i) y2(i) if N3 = 3; ',//
     #4x,'  nx(i) y51 y1(i) y2(i) y5(i) Dy5 y52 if N3 = 4; ',/
     #4x,'  nx(i) y21 y1(i) y5(i) y2(i) Dy2 y25 if N3 = 5; ',/
     #4x,'  nx(i) y25 y5(i) y1(i) y2(i) Dy2 y21 if N3 = 6; ',//
     #4x,'         nx(i) y1(i) dy1(i)      if N3 = 11; ',/
     #4x,'         nx(i) y2(i) dy2(i)      if N3 = 12; ',/
     #4x,'         nx(i) y3(i) dy3(i)      if N3 = 13; ',//
     #4x,'         nx(i) dy1(i)            if N3 = 21; ',/
     #4x,'         nx(i) dy2(i)            if N3 = 22; ',/
     #4x,'         nx(i) dy3(i)            if N3 = 23 );    N3 =',i3,
     #//4x,'[  y21 = y2(i) - y1(i) ;  Dy2 = y2(i) - y2(i-1)  ]',/)  
  812 format(/2x,'nx(i)   y1(i)',6x,'y2(i)',6x,'y5(i)    Dy5(i)',//)
  814 format(/2x,'nx(i)   y1(i)',6x,'y5(i)',6x,'y2(i)    Dy2(i)',//)
  816 format(/2x,'nx(i)   y5(i)',6x,'y1(i)',6x,'y2(i)    Dy2(i)',//)
  820 format(/'nx(i)',3x,'y51(i)',7x,'y1(i)',4x,'y2(i)',4x,
     #'y5(i)   Dy5(i)   y52(i)',//)
  822 format(/'nx(i)',3x,'y21(i)',7x,'y1(i)',4x,'y5(i)',4x,
     #'y2(i)   Dy2(i)   y25(i)',//)
  824 format(/'nx(i)',3x,'y25(i)',7x,'y5(i)',4x,'y1(i)',4x,
     #'y2(i)   Dy2(i)   y21(i)',//)
  830 format(2x,i3,1x,f10.1,1x,f10.1,1x,f10.1,2x,f7.1)
  832 format(2x,i3,12x,f10.1)
  834 format(i3,f9.1,6x,f9.1,f9.1,f9.1,f8.1,f8.1)
  835 format(i3,f9.4,6x,f9.1,f9.1,f9.1,f8.1,f8.4)
  836 format(i3,23x,f10.1)
  850 format(//4x,"Shifting y's by a1 if i =< N2 (=",i3,' )',/
     #4x,"Shifting y's by a2 if i  > N2 (=",i3,' )',//
     #4x,'   a1 = ',f12.3,3x,'   a2 = ',f12.3,//
     #4x,'nx(i)     y(i) ',/)
C    #4x,' x(i)       y(i) ',/)
  860 format(3x,i3,3x,f11.2)
C 860 format(3x,f8.3,3x,f11.2)
  865 format(f10.5,1x,f16.5)
c-------------------------------------------------------------------------
  990   stop
      end
