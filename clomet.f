      program water_tail

      !ifort clomet.f -Ofast -o clomet.e

      integer ires,iatoms,clspec,itime,charform,f,solshell,secshell
      double precision wcomx,wcomy,wcomz,oscomx,oscomy,oscomz,wmass,
     +osmass,clos,clw,xd,yd,zd,dclo
      double precision,allocatable:: x(:),y(:),z(:),amass(:)
      character adummy*150,header*150,model*6,charcheat*4
      character,allocatable:: ares(:)*4,atsym(:)*4
      real timestep


      open(14,file='solvshell.out')
      open(9,file='input.dat')

      read(9,*) adummy
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
      if(k.eq.1) allocate(x(iatoms),y(iatoms),z(iatoms),amass(iatoms),
     +ares(iatoms),atsym(iatoms))
       
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

       if(timestep.lt.itime) then
       cycle
       endif

       atsym(i) = adjustl(atsym(i))
       charcheat=atsym(i)

       if(itime.le.timestep) then

       if(charcheat(1:2).eq.'CA'.and.model(1:3).eq.'all') amass(i)=12.0110
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

      if(timestep.lt.itime) cycle

       clock=clock+1

       wcomx=wcomx/wmass
       wcomy=wcomy/wmass
       wcomz=wcomz/wmass
c find COM of water

       clw=sqrt((x(clspec)-wcomx)**2+(y(clspec)-wcomy)**2+(z(clspec)-
     + wcomz)**2)
c find distance from Cl to water COM

       oscomx=oscomx/osmass
       oscomy=oscomy/osmass
       oscomz=oscomz/osmass
c find COM of organic layer

       clos=sqrt((x(clspec)-oscomx)**2+(y(clspec)-oscomy)**2+(z(clspec)-
     + oscomz)**2)
c find distance from Cl to organic COM

       if(clw.gt.(1.+clos)) then
c if the the distance from Cl to water COM is significantly larger than the distance
c from the Cl to the organic COM, find water molecules within a certain radius
        solshell = 0
        secshell = 0

        do i=1,iatoms

        if(atsym(i).eq.'OW') then
c by 'waters' I mean the oxygen of the water molecule
         if(abs(x(i)-x(clspec)).gt.abs(xbox-abs(x(i)-x(clspec)))) then
          if(x(i).gt.x(clspec)) then
          xd=xbox-abs(x(clspec)-x(i))
          else
          xd=(x(clspec)-x(i))-xbox
          endif
          else
          xd=abs(x(clspec)-x(i))
         endif
         if(abs(y(i)-y(clspec)).gt.abs(ybox-abs(y(i)-y(clspec)))) then
          if(y(i).gt.y(clspec)) then
          yd=ybox-abs(y(clspec)-y(i))
          else
          yd=(y(clspec)-y(i))-ybox
          endif
          else
          yd=abs(y(clspec)-y(i))
         endif
         if(abs(z(i)-z(clspec)).gt.abs(zbox-abs(z(i)-z(clspec)))) then
          if(z(i).gt.z(clspec)) then
          zd=zbox-abs(z(clspec)-z(i))
          else
          zd=(z(clspec)-z(i))-zbox
          endif
          else
          zd=abs(z(clspec)-z(i))
         endif

!       dclo=sqrt(xd**2+yd**2+zd**2)
       dclo=zd 
      
       if(dclo.lt.0.4) solshell=solshell+1
       if(dclo.lt.0.7) secshell=secshell+1
c count up waters within that radius

       endif

       enddo

       write(14,'(f11.5,2x,i4,2x,i4)') timestep,solshell,secshell

       endif

       enddo
       
       close(14)

99     format(i5,a4,2x,a4,i5,2x,f6.3,2x,f6.3,2x,f6.3)
 
       deallocate(x,y,z,amass,ares,atsym)
         
       end









!1 - find number of water molecules within a certain cutoff
!    * find solvent molecules within 1 nm, similar to radial
!    distribution program
!    * only do this if the Cl is closer to the center of mass of
!    the organic layer than to the center of mass of water
!2 - find number of water molecules attached to Cl and not to bulk water
!    * find closest water molecule to Cl, then find closest water
!    molceule to that water molecules, etc. until the distance between
!    closest waters exceeds some threshold, or too many waters have been
!    found and it is probably finding waters in the bulk water layer
!    * only perform this if " "
!  - not doing 2 b/c oxygens in the shell might not be the closest to
!    other waters and would get left out
