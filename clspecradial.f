      program salt_water_distance

      !ifort clspecradial.f -Ofast -o clspecradial.e 

      integer waters,chlorides,sodiums,iatoms,nbins,nrings,itime,
     &clock,nr,charform,f,cats,clspec
      double precision,allocatable::co(:),ccat(:),ring(:),bin(:),x(:),
     +y(:),z(:),vol(:)
      integer,allocatable:: ires(:)
      double precision dclo,clox,cloy,cloz,pie,dr,clcatx,clcaty,clcatz
      character adummy*160,header*160,ares*4,agroup*11
      character*4,allocatable:: atsym(:)

      open(49,file='clspecradial.dat') 
      open(59,file='index.ndx')
      
      do
      read(59,'(a11)',iostat=iostatus) agroup
      if(iostatus.ne.0) exit
      if(agroup(1:1).eq.'[') groups = groups + 1
      if(agroup(1:11).eq.'[ CL_spec ]') then
      read(59,*) clspec
      endif
      enddo
    
      close(59)

      open(9,file='input.dat')
      read(9,*) nbins
      read(9,*) itime
      read(9,*) nrings
      allocate(ring(0:nrings),bin(0:nbins),vol(nrings))
      q = 0
      cats = 0
      clock = 0
      waters = 0
      sodiums = 0
      chlorides = 0
      pie=2.*acos(0.)
c pie is pi 
      dr=1./(real(nrings))
c dr is the width of each shell
      allocate(co(nrings),ccat(nrings))
        do l=1,nrings
         co(l) = 0.
         ccat(l) = 0.
        enddo
      read(5,'(a160)') header
      do f=1,160
      if(header(f:f+1).eq.'t=') charform = f+1
      enddo
      rewind(5)
c how we read what timestep we're at depends on how long the first line
c is
      do
      q = q + 1
      read(5,'(a,f8.2)',iostat=iostatus) adummy(1:charform),timestep
      if(iostatus.ne.0) exit
c do stuff until there's nothing left to do
      if(mod(timestep,100.).eq.0) write(*,*) timestep
      read(5,*) iatoms
      if(q.eq.1) allocate(ires(iatoms),x(iatoms),y(iatoms),z(iatoms),
     +atsym(iatoms))
      do i=1,iatoms
        read(5,99) ires(i),ares,atsym(i),iatom,x(i),y(i),z(i)
        atsym(i)=adjustl(atsym(i))
        if(q.eq.1) then
      endif
c remember all atoms and their coordinates
      enddo
      read(5,*,iostat=iostatus) xbox,ybox,zbox
      if(timestep.lt.itime) cycle
      clock = clock + 1
c start doing math at the desired time
      ring(0) = 0.
      do l=1,nrings
c find the radius and volume of each shell
        ring(l)=ring(l-1)+dr
        vol(l) = (4./3.)*pie*(ring(l)**3-ring(l-1)**3)
      enddo

          k = 0
c look at all the atoms and look at all the atoms
          do
           k = k + 1
           if(k.gt.iatoms) exit
c periodic boundry conditions
c overkill because signed distance doesn't matter here, but done to be
c consistant with H-O-z angle treatment of pbc
           if(atsym(k).eq.'OW') then
           if(abs(x(clspec)-x(k)).gt.abs(xbox-abs(x(clspec)-x(k)))) then
            if(x(clspec).gt.x(k)) then
            clox = xbox - abs(x(k) - x(clspec))
            else
            clox = (x(k)-x(clspec))-xbox
            endif
           else
            clox = (x(k) - x(clspec))
           endif
           if(clox.gt.1.) cycle
           if(abs(y(clspec)-y(k)).gt.abs(ybox-abs(y(clspec)-y(k)))) then
            if(y(clspec).gt.y(k)) then
            cloy = ybox-abs(y(k) - y(clspec))
            else
            cloy = (y(k)-y(clspec))-ybox
            endif
           else
            cloy = abs(y(clspec) - y(k))
           endif
           if(cloy.gt.1.) cycle
           if(abs(z(clspec)-z(k)).gt.abs(zbox-abs(z(clspec)-z(k)))) then
            if(z(clspec).gt.z(k)) then
            cloz = zbox - abs(z(clspec) - z(k))
            else
            cloz = (z(k)-z(clspec))-zbox
            endif
           else
            cloz = abs(z(clspec) - z(k))
           endif
           if(cloz.gt.1.) cycle
           dclo=sqrt(clox**2+cloy**2+cloz**2)
           if(dclo.le.(dr*real(nrings))) then
           do l=1,nrings
            if(dclo.ge.ring(l-1).and.dclo.lt.ring(l)) then
            co(l) = co(l) + 1./vol(l)
            waters=waters+1
            exit
            endif
           enddo
            endif
          endif
           if(atsym(k).eq.'NAI'.or.atsym(k).eq.'NAA') then 
           if(abs(x(clspec)-x(k)).gt.abs(xbox-abs(x(clspec)-x(k)))) then
            if(x(clspec).gt.x(k)) then
            clcatx = xbox - abs(x(k) - x(clspec))
            else
            clcatx = (x(k)-x(clspec))-xbox
            endif
           else
            clcatx = (x(k) - x(clspec))
           endif
           if(clcatx.gt.1.) cycle
           if(abs(y(clspec)-y(k)).gt.abs(ybox-abs(y(clspec)-y(k)))) then
            if(y(clspec).gt.y(k)) then
            clcaty = ybox-abs(y(k) - y(clspec))
            else
            clcaty = (y(k)-y(clspec))-ybox
            endif
           else
            clcaty = abs(y(clspec) - y(k))
           endif
           if(clcaty.gt.1.) cycle
           if(abs(z(clspec)-z(k)).gt.abs(zbox-abs(z(clspec)-z(k)))) then
            if(z(clspec).gt.z(k)) then
            clcatz = zbox - abs(z(clspec) - z(k))
            else
            clcatz = (z(k)-z(clspec))-zbox
            endif
           else
            clcatz = abs(z(clspec) - z(k))
           endif
           if(clcatz.gt.1.) cycle
           dclcat=sqrt(clcatx**2+clcaty**2+clcatz**2)
           if(dclcat.le.(dr*real(nrings))) then
           do l=1,nrings
            if(dclcat.ge.ring(l-1).and.dclcat.lt.ring(l)) then
            ccat(l) = ccat(l) + 1./vol(l)
            cats=cats+1
            exit
            endif
           enddo
            endif
            endif
          enddo
          
         enddo
c fancy shmancy xmrgace formatting to get labels              
        write(49,'(a)') '@    title "radial distance"'
        write(49,'(a)') '@    xaxis label "distance (nm)"'
        write(49,'(a)') 
     +'@    yaxis label "particles per unit volume (u/nm^3)"'
        write(49,'(a)') '@TYPE xy'
      write(49,'(a)') '@ view 0.15, 0.15, 0.95, 0.80'
      write(49,'(a)') '@ legend on'
      write(49,'(a)') '@ legend box on'
      write(49,'(a)') '@ legend loctype view'
      write(49,'(a)') '@ legend 1.01, 0.6'
      if(cats.gt.0) write(49,'(a)') '@ legend length 7'
      if(cats.eq.0) write(49,'(a)') '@ legend length 5'
      write(49,'(a)') '@ s0 legend "Cl-water"'
      if(cats.gt.0) write(49,'(a)') '@ s1 legend "Cl-N+"'
        do l=1,nrings
       if(cats.eq.0) then
       write(49,10) ring(l)-dr/2.,co(l)/real(waters)
      endif 
      if(cats.gt.0) then
       write(49,11) ring(l)-dr/2.,co(l)/real(waters),ccat(l)/real(cats)
       endif
       enddo
      close(9)
      close(49)
      deallocate(bin,ring,x,y,z,atsym,vol,co,ccat,ires)
10    format(f20.16,5x,f20.16)
11    format(f20.16,2(5x,f20.16))
99    format(i5,a4,2x,a4,i5,2x,f6.3,2x,f6.3,2x,f6.3)
      end
