      program where_in_the_world_is_Carmen_Chloridiago
c I'm funny

      double precision dz,x,y,z,Rx,Ry,Rz,scl,clx,cly,clz
      double precision,allocatable:: bin(:)
      integer iatoms,ires,clock,nbins,waters,CLspec,lows,charform
      integer,allocatable:: prob(:)
      character adummy,atsym*4,header*60
c this program finds the distance between CL_spec and the water center of mass and then sorts these distances into sections to find probability
      k = 0
      clock = 0
      open(7,file='CLspec.dat') 
      read(7,*) CLspec
c CLspec is written by readndx
      allocate(prob(-100:100),bin(-101:100))
      do j=-100,100
       prob(j) = 0
      enddo
      read(5,'(a60)') header
      do f=1,60
      if(header(f:f+1).eq.'t=') charform = f+1
      enddo
      rewind(5)
      do
      k = k + 1
      if(charform.eq.50) then
      read(5,'(a50,f8.2)',iostat=iostatus) adummy,timestep
      elseif(charform.eq.45) then
      read(5,'(a45,f8.2)',iostat=iostatus) adummy,timestep
      endif
      if(iostatus.ne.0) exit
      read(5,*) iatoms
      Rx = 0.
      Ry = 0.
      Rz = 0.
      waters = 0
      lows = 0
      clz = -41000
      do i=1,iatoms
        read(5,99) ires,ares,atsym,iatom,x,y,z
        atsym = adjustl(atsym)
        if(atsym(1:2).eq.'OW') then 
!        Rx = Rx + x
!        Ry = Ry + y
        Rz = Rz + z
        waters = waters + 1 
        endif
        if(iatom.eq.CLspec) then
!        clx = x
!        cly = y
        clz = z
c storing the coords of our favorite chloride
        endif
      enddo
      read(5,*) xbox, ybox, zbox
      clock = clock + 1
!      Rx = Rx/real(waters)
!      Ry = Ry/real(waters)
      Rz = Rz/real(waters)
c R* makes video games
c Rx, Ry, and Rz are the x, y, and z coordinates of the water COM
      dz = zbox/100.
      bin(-101) = -zbox
      scl = Rz - clz
      do j=-100,100
      bin(j) = bin(j-1) + dz
      if(scl.ge.bin(j-1).and.scl.lt.bin(j)) prob(j) = prob(j) + 1
      enddo
      enddo
      do j=-100,100
      if(prob(j).ne.0) write(6,*) bin(j)-dz/2.,real(prob(j))/real(clock)
      enddo 
      deallocate(prob,bin)
      close(7)
99    format(i5,a4,2x,a4,i5,2x,f6.3,2x,f6.3,2x,f6.3)
      end  
        
