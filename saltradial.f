      program salt_water_distance

      integer waters,chlorides,sodiums,iatoms,ires,nbins,nrings,itime,
     &clock,nr,charform,f,cats
      double precision,allocatable::snc(:,:),snn(:,:),scc(:,:),sno(:,:),
     &sco(:,:),nc(:),nn(:),cc(:),no(:),co(:),ccat(:),ncat(:)
      double precision dnacl,naclx,nacly,naclz,nanax,nanay,nanaz,dnana,
     &  dnao,naox,naoy,naoz,dclo,clox,cloy,cloz,dclcl,clclx,clcly,clclz,
     &  pie,dr,clcatx,clcaty,clcatz,nacatx,nacaty,nacatz
      double precision,allocatable::ring(:),bin(:),x(:),y(:),z(:),vol(:)
      character adummy*160,header*160,ares*4
      character*4,allocatable:: atsym(:)

      open(49,file='saltradial.dat') 
      open(9,file='input.dat')
      read(9,*) nbins
!!!c Most lines invloving bins have been commented out because they are 
c not useful. Graphs for radial distance at different places on the
c z-axis look like rainbow spaghetti. 
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
      allocate(snc(nrings,nbins),snn(nrings,nbins),scc(nrings,nbins
     &),sno(nrings,nbins),sco(nrings,nbins),nc(nrings),nn(nrings),
     &cc(nrings),no(nrings),co(nrings),ccat(nrings),ncat(nrings))
        do l=1,nrings
!         do j=1,nbins
!         snc(l,j) = 0.
!         snn(l,j) = 0.
!         scc(l,j) = 0.
!         sno(l,j) = 0.
!         sco(l,j) = 0.
!         enddo
         nc(l) = 0.
         nn(l) = 0.
         cc(l) = 0.
         no(l) = 0.
         co(l) = 0.
         ccat(l) = 0.
         ncat(l) = 0.
c nc is the number of sodiums or chlorides that are a certain distance
c away from chlorides or sodiums 
c nn " " sodiums " " sodiums
c cc " " chlorides " " chlorides
c no " " sodiums " " oxygens
c co " " chlorides " " oxygens
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
      if(mod(timestep,50.).eq.0) write(*,*) timestep
      read(5,*) iatoms
      if(q.eq.1) allocate(x(iatoms),y(iatoms),z(iatoms),atsym(iatoms))
      do i=1,iatoms
        read(5,99) ires,ares,atsym(i),iatom,x(i),y(i),z(i)
        atsym(i)=adjustl(atsym(i))
        if(q.eq.1) then
        if(atsym(i).eq.'NA'.and.atsym(i).ne.'NAI') sodiums = sodiums + 1
        if(atsym(i).eq.'CL') chlorides = chlorides + 1
        if(atsym(i).eq.'OW') waters  = waters + 1
        if(atsym(i).eq.'NAI'.or.atsym(i).eq.'NAA') cats = cats + 1
      endif
c remember all atoms and their coordinates
c count the number of sodiums, chlorides, and waters
      enddo
      read(5,*,iostat=iostatus) xbox,ybox,zbox
      if(timestep.lt.itime) cycle
      clock = clock + 1
c start doing math at the desired time
!      dz=zbox/real(nbins)
!c finding the height of each bin
      ring(0) = 0.
!      bin(0) = 0.
!      do j=1,nbins
!        bin(j)=bin(j-1)+dz
!      enddo
      do l=1,nrings
c find the radius and volume of each shell
        ring(l)=ring(l-1)+dr
        vol(l) = (4./3.)*pie*(ring(l)**3-ring(l-1)**3)
      enddo
!      do j=1,nbins
        do i=1,iatoms
        if((atsym(i).eq.'NA'.or.atsym(i).eq.'CL').and.atsym(i).ne.
     +'NAI') then
          k = 0
c look at all the atoms and look at all the atoms
          do
           k = k + 1
           if(k.gt.iatoms) exit
           if((atsym(k).eq.'NA'.and.atsym(i).eq.'CL').or.
     &        (atsym(k).eq.'CL'.and.atsym(i).eq.'NA').and.atsym(i)
     +.ne.'NAI'.and.atsym(k).ne.'NAI') then 
c periodic boundry conditions
c overkill because signed distance doesn't matter here, but done to be
c consistant with H-O-z angle treatment of pbc
           if(abs(x(i)-x(k)).gt.abs(xbox-abs(x(i)-x(k)))) then
            if(x(i).gt.x(k)) then
            naclx = xbox - abs(x(k) - x(i))
            else
            naclx = (x(k)-x(i))-xbox
            endif
           else
            naclx = (x(k) - x(i))
           endif
           if(naclx.gt.1.) cycle
           if(abs(y(i)-y(k)).gt.abs(ybox-abs(y(i)-y(k)))) then
            if(y(i).gt.y(k)) then
            nacly = ybox-abs(y(k) - y(i))
            else
            nacly = (y(k)-y(i))-ybox
            endif
           else
            nacly = abs(y(i) - y(k))
           endif
           if(nacly.gt.1.) cycle
           if(abs(z(i)-z(k)).gt.abs(zbox-abs(z(i)-z(k)))) then
            if(z(i).gt.z(k)) then
            naclz = zbox - abs(z(i) - z(k))
            else
            naclz = (z(k)-z(i))-zbox
            endif
           if(naclz.gt.1.) cycle
           else
            naclz = abs(z(i) - z(k))
           endif
           dnacl=sqrt(naclx**2+nacly**2+naclz**2)
            do l=1,nrings
            if(dnacl.ge.ring(l-1).and.dnacl.lt.ring(l)) then
!            snc(l,j) = snc(l,j) + 1./vol(l)
            nc(l) = nc(l) + 1./vol(l)
            exit
            endif
            enddo
           endif
           if(atsym(k).eq.'NA'.and.k.ne.i.and.atsym(i).eq.'NA'.and.
     +atsym(i).ne.'NAI'.and.atsym(k).ne.'NAI') then 
           if(abs(x(i)-x(k)).gt.abs(xbox-abs(x(i)-x(k)))) then
            if(x(i).gt.x(k)) then
            nanax = xbox - abs(x(k) - x(i))
            else
            nanax = (x(k)-x(i))-xbox
            endif
           else
            nanax = (x(k) - x(i))
           endif
           if(nanax.gt.1.) cycle
           if(abs(y(i)-y(k)).gt.abs(ybox-abs(y(i)-y(k)))) then
            if(y(i).gt.y(k)) then
            nanay = ybox-abs(y(k) - y(i))
            else
            nanay = (y(k)-y(i))-ybox
            endif
           else
            nanay = abs(y(i) - y(k))
           endif
           if(nanay.gt.1.) cycle
           if(abs(z(i)-z(k)).gt.abs(zbox-abs(z(i)-z(k)))) then
            if(z(i).gt.z(k)) then
            nanaz = zbox - abs(z(i) - z(k))
            else
            nanaz = (z(k)-z(i))-zbox
            endif
           else
            nanaz = abs(z(i) - z(k))
           endif
           if(nanaz.gt.1.) cycle
           dnana=sqrt(nanax**2+nanay**2+nanaz**2)
           if(dnana.le.(dr*real(nrings))) then
           do l=1,nrings
            if(dnana.ge.ring(l-1).and.dnana.lt.ring(l)) then
!            snn(l,j) = snn(l,j) + 1./vol(l)
            nn(l) = nn(l) + 1./vol(l)
            exit
            endif
            enddo
            endif
           endif
           if(atsym(k).eq.'CL'.and.k.ne.i.and.atsym(i).eq.'CL') then 
           if(abs(x(i)-x(k)).gt.abs(xbox-abs(x(i)-x(k)))) then
            if(x(i).gt.x(k)) then
            clclx = xbox - abs(x(k) - x(i))
            else
            clclx = (x(k)-x(i))-xbox
            endif
           else
            clclx = (x(k) - x(i))
           endif
           if(clclx.gt.1.) cycle
           if(abs(y(i)-y(k)).gt.abs(ybox-abs(y(i)-y(k)))) then
            if(y(i).gt.y(k)) then
            clcly = ybox-abs(y(k) - y(i))
            else
            clcly = (y(k)-y(i))-ybox
            endif
           else
            clcly = abs(y(i) - y(k))
           endif
           if(clcly.gt.1.) cycle
           if(abs(z(i)-z(k)).gt.abs(zbox-abs(z(i)-z(k)))) then
            if(z(i).gt.z(k)) then
            clclz = zbox - abs(z(i) - z(k))
            else
            clclz = (z(k)-z(i))-zbox
            endif
           else
            clclz = abs(z(i) - z(k))
           endif
           if(clclz.gt.1.) cycle
           dclcl=sqrt(clclx**2+clcly**2+clclz**2)
           if(dclcl.le.(dr*real(nrings))) then
           do l=1,nrings
            if(dclcl.ge.ring(l-1).and.dclcl.lt.ring(l)) then
!            scc(l,j) = scc(l,j) + 1./vol(l)
            cc(l) = cc(l) + 1./vol(l)
            exit
            endif
            enddo
            endif
           endif
           if(atsym(k).eq.'OW'.and.atsym(i).eq.'NA'.and.atsym(i).ne.
     +'NAI') then 
           if(abs(x(i)-x(k)).gt.abs(xbox-abs(x(i)-x(k)))) then
            if(x(i).gt.x(k)) then
            naox = xbox - abs(x(k) - x(i))
            else
            naox = (x(k)-x(i))-xbox
            endif
           else
            naox = (x(k) - x(i))
           endif
           if(naox.gt.1.) cycle
           if(abs(y(i)-y(k)).gt.abs(ybox-abs(y(i)-y(k)))) then
            if(y(i).gt.y(k)) then
            naoy = ybox-abs(y(k) - y(i))
            else
            naoy = (y(k)-y(i))-ybox
            endif
           else
            naoy = abs(y(i) - y(k))
           endif
           if(naoy.gt.1.) cycle
           if(abs(z(i)-z(k)).gt.abs(zbox-abs(z(i)-z(k)))) then
            if(z(i).gt.z(k)) then
            naoz = zbox - abs(z(i) - z(k))
            else
            naoz = (z(k)-z(i))-zbox
            endif
           else
            naoz = abs(z(i) - z(k))
           endif
           if(naoz.gt.1.) cycle
           dnao=sqrt(naox**2+naoy**2+naoz**2)
           if(dnao.le.(dr*real(nrings))) then
           do l=1,nrings
            if(dnao.ge.ring(l-1).and.dnao.lt.ring(l)) then
!            sno(l,j) = sno(l,j) + 1./vol(l)
            no(l) = no(l) + 1./vol(l)
            exit
            endif
            enddo
            endif
           endif
           if(atsym(k).eq.'OW'.and.atsym(i).eq.'CL') then 
           if(abs(x(i)-x(k)).gt.abs(xbox-abs(x(i)-x(k)))) then
            if(x(i).gt.x(k)) then
            clox = xbox - abs(x(k) - x(i))
            else
            clox = (x(k)-x(i))-xbox
            endif
           else
            clox = (x(k) - x(i))
           endif
           if(clox.gt.1.) cycle
           if(abs(y(i)-y(k)).gt.abs(ybox-abs(y(i)-y(k)))) then
            if(y(i).gt.y(k)) then
            cloy = ybox-abs(y(k) - y(i))
            else
            cloy = (y(k)-y(i))-ybox
            endif
           else
            cloy = abs(y(i) - y(k))
           endif
           if(cloy.gt.1.) cycle
           if(abs(z(i)-z(k)).gt.abs(zbox-abs(z(i)-z(k)))) then
            if(z(i).gt.z(k)) then
            cloz = zbox - abs(z(i) - z(k))
            else
            cloz = (z(k)-z(i))-zbox
            endif
           else
            cloz = abs(z(i) - z(k))
           endif
           if(cloz.gt.1.) cycle
           dclo=sqrt(clox**2+cloy**2+cloz**2)
           if(dclo.le.(dr*real(nrings))) then
           do l=1,nrings
            if(dclo.ge.ring(l-1).and.dclo.lt.ring(l)) then
!            sco(l,j) = sco(l,j) + 1./vol(l)
            co(l) = co(l) + 1./vol(l)
            exit
            endif
            enddo
            endif
          endif
           if(atsym(k).eq.'NAI'.and.k.ne.i.and.atsym(i).eq.'CL') then 
           if(abs(x(i)-x(k)).gt.abs(xbox-abs(x(i)-x(k)))) then
            if(x(i).gt.x(k)) then
            clcatx = xbox - abs(x(k) - x(i))
            else
            clcatx = (x(k)-x(i))-xbox
            endif
           else
            clcatx = (x(k) - x(i))
           endif
           if(clcatx.gt.1.) cycle
           if(abs(y(i)-y(k)).gt.abs(ybox-abs(y(i)-y(k)))) then
            if(y(i).gt.y(k)) then
            clcaty = ybox-abs(y(k) - y(i))
            else
            clcaty = (y(k)-y(i))-ybox
            endif
           else
            clcaty = abs(y(i) - y(k))
           endif
           if(clcaty.gt.1.) cycle
           if(abs(z(i)-z(k)).gt.abs(zbox-abs(z(i)-z(k)))) then
            if(z(i).gt.z(k)) then
            clcatz = zbox - abs(z(i) - z(k))
            else
            clcatz = (z(k)-z(i))-zbox
            endif
           else
            clcatz = abs(z(i) - z(k))
           endif
           if(clcatz.gt.1.) cycle
           dclcat=sqrt(clcatx**2+clcaty**2+clcatz**2)
           if(dclcat.le.(dr*real(nrings))) then
           do l=1,nrings
            if(dclcat.ge.ring(l-1).and.dclcat.lt.ring(l)) then
            ccat(l) = ccat(l) + 1./vol(l)
            exit
            endif
            enddo
            endif
           endif
           if(atsym(k).eq.'NAI'.and.k.ne.i.and.atsym(i).eq.'NA'.and.
     +atsym(i).ne.'NAI') then 
           if(abs(x(i)-x(k)).gt.abs(xbox-abs(x(i)-x(k)))) then
            if(x(i).gt.x(k)) then
            nacatx = xbox - abs(x(k) - x(i))
            else
            nacatx = (x(k)-x(i))-xbox
            endif
           else
            nacatx = (x(k) - x(i))
           endif
           if(nacatx.gt.1.) cycle
           if(abs(y(i)-y(k)).gt.abs(ybox-abs(y(i)-y(k)))) then
            if(y(i).gt.y(k)) then
            nacaty = ybox-abs(y(k) - y(i))
            else
            nacaty = (y(k)-y(i))-ybox
            endif
           else
            nacaty = abs(y(i) - y(k))
           endif
           if(nacaty.gt.1.) cycle
           if(abs(z(i)-z(k)).gt.abs(zbox-abs(z(i)-z(k)))) then
            if(z(i).gt.z(k)) then
            nacatz = zbox - abs(z(i) - z(k))
            else
            nacatz = (z(k)-z(i))-zbox
            endif
           else
            nacatz = abs(z(i) - z(k))
           endif
           if(nacatz.gt.1.) cycle
           dnacat=sqrt(nacatx**2+nacaty**2+nacatz**2)
           if(dnacat.le.(dr*real(nrings))) then
           do l=1,nrings
            if(dnacat.ge.ring(l-1).and.dnacat.lt.ring(l)) then
            ncat(l) = ncat(l) + 1./vol(l)
            exit
            endif
            enddo
            endif
           endif
          enddo
          
         endif
         enddo
!         enddo
       enddo
c fancy shmancy xmrgace formatting to get labels              
        write(49,'(a)') '@    title "radial distance"'
        write(49,'(a)') '@    xaxis label "distance (nm)"'
        write(49,'(a)') 
     +'@    yaxis label "particles per unit volume (u/nm^3)"'
        write(49,*) '@TYPE xy'
      write(49,'(a)') '@ view 0.15, 0.15, 0.95, 0.80'
      write(49,'(a)') '@ legend on'
      write(49,'(a)') '@ legend box on'
      write(49,'(a)') '@ legend loctype view'
      write(49,'(a)') '@ legend 1.01, 0.6'
      if(cats.gt.0) write(49,'(a)') '@ legend length 7'
      if(cats.eq.0) write(49,'(a)') '@ legend length 5'
      write(49,'(a)') '@ s0 legend "Na-Cl"'
      write(49,'(a)') '@ s1 legend "Na-Na"'
      write(49,'(a)') '@ s2 legend "Cl-Cl"'
      write(49,'(a)') '@ s3 legend "Na-water"'
      write(49,'(a)') '@ s4 legend "Cl-water"'
      if(cats.gt.0) write(49,'(a)') '@ s5 legend "Na-N+"'
      if(cats.gt.0) write(49,'(a)') '@ s6 legend "Cl-N+"'
      if(sodiums.eq.0) sodiums = 1
      if(chlorides.eq.0) chlorides = 1
        do l=1,nrings
!       if(snc(l,j).ne.0) write(44,*) ring(l)-dr/2.,(real(snc(l,j))/
!     +(2.*real(clock)),j=1,nbins)
!       if(snn(l,j).ne.0) write(45,*) ring(l)-dr/2.,(real(snn(l,j))/
!     +(real(clock)),j=1,nbins)
!       if(scc(l,j).ne.0) write(46,*) ring(l)-dr/2.,(real(scc(l,j))/
!     +(real(clock)),j=1,nbins)
!       if(sno(l,j).ne.0) write(47,*) ring(l)-dr/2.,(real(sno(l,j))/
!     +(real(clock)),j=1,nbins)
!       if(sco(l,j).ne.0) write(48,*) ring(l)-dr/2.,(real(sco(l,j))/
!     +(real(clock)),j=1,nbins)
       if(cats.eq.0) then
       write(49,10) ring(l)-dr/2.,nc(l)/real(clock*(sodiums+chlorides)),
     &nn(l)/real(clock*(sodiums-1)),cc(l)/real(clock*(chlorides-1)),
     &no(l)/real(clock*sodiums),co(l)/real(clock*chlorides)
      endif 
      if(cats.gt.0) then
       write(49,11) ring(l)-dr/2.,nc(l)/real(clock*(sodiums+chlorides)),
     &nn(l)/real(clock*(sodiums-1)),cc(l)/real(clock*(chlorides-1)),
     &no(l)/real(clock*sodiums),co(l)/real(clock*chlorides),
     &ncat(l)/real(clock*sodiums),ccat(l)/real(clock*chlorides)
       endif
       enddo
      close(9)
      close(49)
      deallocate(bin,ring,x,y,z,atsym,snc,snn,scc,sno,sco,vol,ncat,ccat)
10    format(f20.16,5(5x,f20.16))
11    format(f20.16,7(5x,f20.16))
99    format(i5,a4,2x,a4,i5,2x,f6.3,2x,f6.3,2x,f6.3)
      end
