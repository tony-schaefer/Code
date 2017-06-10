      program water_tail

      integer ires,iatoms,clspec,itime,charform,f,nbins
      integer,allocatable:: solshell(:),secshell(:),chl(:)
      double precision wcomx,wcomy,wcomz,oscomx,oscomy,oscomz,wmass,
     +osmass,clos,clw,xd,yd,zd,dclo
      double precision,allocatable:: x(:),y(:),z(:),amass(:),bin(:)
      character adummy*150,header*150,model*6,charcheat*4
      character,allocatable:: ares(:)*4,atsym(:)*4
      real timestep


      open(14,file='solvshell.out')
      open(9,file='input.dat')

      read(9,*) nbins
      read(9,*) itime
      read(9,*) adummy
      read(9,*) model
c read time to start calcs and what model (all or united)
      close(9)

      open(10,file='index.ndx')
      do
      read(10,'(a150)',iostat=iostatus) adummy
      if(iostatus.ne.0) exit
      if(adummy(1:11).eq.'[ CL_spec ]') then
      read(10,'(i5)') clspec
      exit
      endif
      enddo
c figure out which Cl is being pulled

      close(10)

      read(5,'(a150)') header
      do f=1,150
      if(header(f:f+1).eq.'t=') charform = f+1
      enddo
      rewind(5)
c figure out how to read what timestep we are on      

      k=0
      waters=0
      os=0
      clock=0

      do
      k=k+1
      read(5,'(a,f10.5)',iostat=iostatus) adummy(1:charform),timestep
      if(iostatus.ne.0) exit
      if(mod(timestep,100.).eq.0) write(*,*) timestep
      read(5,*) iatoms
      if(k.eq.1) then
      allocate(x(iatoms),y(iatoms),z(iatoms),amass(iatoms),
     +ares(iatoms),atsym(iatoms),solshell(nbins),secshell(nbins),
     +bin(0:nbins),chl(nbins))
      do j=1,nbins
      solshell(j)=0
      secshell(j)=0
      enddo
      endif
        

      wcomx=0.
      wcomy=0.
      wcomz=0.
      oscomx=0.
      oscomy=0.
      oscomz=0.
      wmass=0.
      osmass=0.

      do i=1,iatoms
       read(5,99) ires,ares(i),atsym(i),iatom,x(i),y(i),z(i)

       atsym(i) = adjustl(atsym(i))
       charcheat=atsym(i)

       if(itime.le.timestep) then

       if(charcheat(1:2).eq.'CA') amass(i)=12.0110
       if(charcheat(1:1).eq.'O') amass(i) = 16.
       if(charcheat(1:1).eq.'H') amass(i) = 1.008
       if(charcheat(1:2).eq.'CL') amass(i) = 35.453
       if(charcheat(1:2).eq.'NA') amass(i) = 22.98977 
       if(charcheat(1:3).eq.'NAI') amass(i) = 14.0067
c charcheat is the unallocated version of atsym, so masses for something
c like CAA, CAB, CAC, etc. don't need to be added
         if(model.eq.'united') then
          if(charcheat(1:2).eq.'CA') amass(i) = 14.027
          if(charcheat(1:3).eq.'CAA'.and.ares(i).eq.'DRG') amass(i) = 15.0350
          if(charcheat(1:3).eq.'CAF'.and.ares(i).eq.'DRG') amass(i) = 15.0350
          if(charcheat(1:3).eq.'CAA'.and.ares(i).eq.'TBA') amass(i) = 15.0350
          if(charcheat(1:3).eq.'CAE'.and.ares(i).eq.'TBA') amass(i) = 15.0350
          if(charcheat(1:3).eq.'CAQ'.and.ares(i).eq.'TBA') amass(i) = 15.0350
          if(charcheat(1:3).eq.'CAM'.and.ares(i).eq.'TBA') amass(i) = 15.0350
          if(charcheat(1:3).eq.'CA'.and.ares(i).eq.'TMA') amass(i) = 15.0350
          if(charcheat(1:3).eq.'CAB'.and.ares(i).eq.'OEDM') amass(i) = 15.0350
          if(charcheat(1:3).eq.'CAK'.and.ares(i).eq.'OEDM') amass(i) = 15.0350
          if(charcheat(1:3).eq.'CAA'.and.ares(i).eq.'OEDM') amass(i) = 15.0350
          if(charcheat(1:3).eq.'CAM'.and.ares(i).eq.'OEDM') amass(i) = 15.0350
          if(charcheat(1:3).eq.'CAA'.and.ares(i).eq.'BTEA') amass(i) = 15.0350
          if(charcheat(1:3).eq.'CAB'.and.ares(i).eq.'BTEA') amass(i) = 15.0350
          if(charcheat(1:3).eq.'CAC'.and.ares(i).eq.'BTEA') amass(i) = 15.0350
          if(charcheat(1:3).eq.'CAH'.and.ares(i).eq.'BTEA') amass(i) = 12.0110
          if(charcheat(1:3).eq.'CAF'.and.ares(i).eq.'BTEA') amass(i) = 12.0110
          if(charcheat(1:3).eq.'CAD'.and.ares(i).eq.'BTEA') amass(i) = 12.0110
          if(charcheat(1:3).eq.'CAE'.and.ares(i).eq.'BTEA') amass(i) = 12.0110
          if(charcheat(1:3).eq.'CAG'.and.ares(i).eq.'BTEA') amass(i) = 12.0110
          if(charcheat(1:3).eq.'CAM'.and.ares(i).eq.'BTEA') amass(i) = 12.0110
          if(charcheat(1:3).eq.'CAA'.and.ares(i).eq.'MTC') amass(i) = 15.0350
          if(charcheat(1:3).eq.'CAC'.and.ares(i).eq.'MTC') amass(i) = 15.0350
          if(charcheat(1:3).eq.'CAD'.and.ares(i).eq.'MTC') amass(i) = 15.0350
          if(charcheat(1:3).eq.'CAB'.and.ares(i).eq.'MTC') amass(i) = 15.0350
          if(charcheat(1:3).eq.'CAA'.and.ares(i).eq.'ACH') amass(i) = 15.0350
          if(charcheat(1:3).eq.'CAI'.and.ares(i).eq.'ACH') amass(i) = 12.0110
          if(charcheat(1:3).eq.'CAC'.and.ares(i).eq.'ACH') amass(i) = 15.0350
          if(charcheat(1:3).eq.'CAD'.and.ares(i).eq.'ACH') amass(i) = 15.0350
          if(charcheat(1:3).eq.'CAB'.and.ares(i).eq.'ACH') amass(i) = 15.0350
         endif
        endif

      if(ares(i).eq.'SOL') then
       wcomx=wcomx+(amass(i)*x(i))
       wcomy=wcomy+(amass(i)*y(i))
       wcomz=wcomz+(amass(i)*z(i))
       wmass=wmass+amass(i)
      endif
       
      if(ares(i).eq.'CHX'.or.ares(i).eq.'DRG') then
       oscomx=oscomx+(amass(i)*x(i))
       oscomy=oscomy+(amass(i)*y(i))
       oscomz=oscomz+(amass(i)*z(i))
       osmass=osmass+amass(i)
      endif
      enddo

      read(5,*,iostat=iostatus) xbox,ybox,zbox
      
      if(k.eq.1) then
      dz=zbox/real(nbins)
      bin(0)=0
      do j=1,nbins
      bin(j)=bin(j-1)+dz
      enddo
      endif

       if(timestep.lt.itime) then
       cycle
       endif

      clock=clock+1

       wcomx=wcomx/wmass
       wcomy=wcomy/wmass
       wcomz=wcomz/wmass
c find COM of water

       oscomz=oscomz/osmass
c find COM of organic layer

        do i=1,iatoms

        if(atsym(i).eq.'OW'.or.i.eq.clspec) then
c by 'waters' I mean the oxygen of the water molecule
         if(abs(x(i)-x(clspec)).gt.abs(xbox-abs(x(i)-x(clspec)))) then
          xd=xbox-abs(x(clspec)-x(i))
          else
          xd=abs(x(clspec)-x(i))
         endif
         if(xd.gt.0.7) cycle
         if(abs(y(i)-y(clspec)).gt.abs(ybox-abs(y(i)-y(clspec)))) then
          yd=ybox-abs(y(clspec)-y(i))
          else
          yd=abs(y(clspec)-y(i))
         endif
         if(yd.gt.0.7) cycle
         if(abs(z(i)-z(clspec)).gt.abs(zbox-abs(z(i)-z(clspec)))) then
          zd=zbox-abs(z(clspec)-z(i))
          else
          zd=abs(z(clspec)-z(i))
         endif
         if(zd.gt.0.7) cycle

       dclo=sqrt(xd**2+yd**2+zd**2)
!       dclo=zd 

       do j=1,nbins
       if((z(clspec).gt.bin(j-1).and.z(clspec).le.bin(j)).or.(j.eq.1
     +.and.z(clspec).le.0).or.(j.eq.nbins.and.z(clspec).gt.zbox)) then
       if(dclo.lt.0.4.and.i.ne.clspec) solshell(j)=solshell(j)+1
       if(dclo.lt.0.7.and.i.ne.clspec) secshell(j)=secshell(j)+1
       if(i.eq.clspec) chl(j)=chl(j)+1
       endif
       enddo
c count up waters within that radius

       endif

       enddo

       enddo
       
      write(14,'(a)') '@    title "1\Sst\N and 2\Snd\N Solvation Shell"'
      write(14,'(a)') '@    xaxis  label "\xx\f{} (nm)"'
      write(14,'(a)') '@    yaxis  label "avg waters"'
      write(14,'(a)') '@TYPE xy'

       do j=1,nbins
       if(chl(j).gt.0) then
       write(14,*) wcomz-(bin(j)-dz/2.),real(solshell(j))/real(chl(j)),
     +real(secshell(j))/real(chl(j))
       endif
       enddo

       close(14)

99     format(i5,a4,2x,a4,i5,2x,f6.3,2x,f6.3,2x,f6.3)
 
       deallocate(chl,x,y,z,amass,ares,atsym,bin,solshell,secshell)
         
       end






