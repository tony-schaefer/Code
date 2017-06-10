      program water_tail_properties


      integer ires,iatoms,bins,itime,charform,f,clspec
      double precision,allocatable:: x(:),y(:),z(:),xbin(:),
     +ybin(:),zbin(:),vtail(:),amass(:),xy(:,:,:)
      double precision wcomz,oscomz,wmass,osmass,dx,dy,dz,minibox
      character adummy*150,header*150,model*6,atsym*4
      character,allocatable:: ares(:)*4

      open(9,file='input.dat')
      read(9,*) bins
      read(9,*) itime
      read(9,*) idummy
      read(9,*) model
      close(9)      

      read(5,'(a150)') header
      do f=1,150
      if(header(f:f+1).eq.'t=') charform = f+1
      enddo
      rewind(5)

      allocate(xbin(0:bins),ybin(0:bins),zbin(0:bins),vtail(bins),
     +xy(bins,bins,bins))

      xbin(0) = 0.
      ybin(0) = 0.
      zbin(0) = 0.

      do j=1,bins
      vtail(j)=0.
      do i=1,bins
      do k=1,bins
      xy(j,i,k)=0.
      enddo
      enddo
      enddo

      clock=0.
      k=0
      sol=0
      wcomz = 0
      wmass=0

      do
      k=k+1
      read(5,'(a,f13.5)',iostat=iostatus) adummy(1:charform),timestep
      if(iostatus.ne.0) exit
      if(mod(timestep,100.).eq.0) write(*,*) timestep
      read(5,*) iatoms
      if(k.eq.1) allocate(x(iatoms),y(iatoms),z(iatoms),ares(iatoms),
     +amass(iatoms))
      
      do i=1,iatoms
       read(5,99) ires,ares(i),atsym,iatom,x(i),y(i),z(i)
       if(timestep.lt.itime) cycle
        atsym=adjustl(atsym)
       if(atsym(1:2).eq.'OW') amass(i)=15.9994
       if(atsym(1:2).eq.'HW') amass(i)=1.008
        if(ares(i).eq.'SOL') then
        wmass=wmass+amass(i)
        wcomz=wcomz+(amass(i)*z(i))
        endif
       if(atsym(1:2).eq.'OW') then
       sol=sol+1.
       endif
      enddo

      read(5,*) xbox,ybox,zbox

      if(timestep.lt.itime) cycle

      clock=clock+1.

      if(clock.eq.1) then

      wcomsol=wcomz/wmass
 
      dx=xbox/real(bins)
      dy=ybox/real(bins)
      dz=zbox/real(bins)

      minibox=dx*dy*dz
      histvol=dz*xbox*ybox

      do j=1,bins
       xbin(j)=xbin(j-1)+dx
       ybin(j)=ybin(j-1)+dy
       zbin(j)=zbin(j-1)+dz
      enddo
      endif

      do i=1,iatoms
      if(ares(i).eq.'SOL') then
       do j=1,bins
       if(((z(i).ge.zbin(j-1).and.z(i).lt.zbin(j)).or.(zbin(j).eq.
     + zbin(1).and.z(i).lt.0).or.(zbin(j).eq.zbin(bins).and.z(i).gt.
     + zbox))!.and.z(i).lt.wcomz
     + )then
        do l=1,bins
        if((y(i).ge.ybin(l-1).and.y(i).lt.ybin(l)).or.(ybin(l).eq.
     +  ybin(1).and.y(i).lt.0).or.(ybin(l).eq.ybin(bins).and.y(i).gt.
     +  ybox)) then
         do n=1,bins
         if((x(i).ge.xbin(n-1).and.x(i).lt.xbin(n)).or.(xbin(n).eq.
     +   xbin(1).and.x(i).lt.0).or.(xbin(n).eq.xbin(bins).and.x(i).gt.
     +   xbox)) then
         
         vtail(j)=vtail(j)+1
         xy(n,l,j)=xy(n,l,j)+1
         cycle
         
         endif
         enddo
        endif
        enddo
       endif
       enddo
      endif
      enddo
      
      enddo

      open(16,file='watertail.dat')
      write(16,*) '@    title "water volume"'
      write(16,*) '@    xaxis label "z position"'
      write(16,*) '@    yaxis label "count per iter"'
      do j=1,bins
      vtail(j)=vtail(j)*real(bins)/(clock)
      zbin(j)=-((zbin(j)-dz/2.)-wcomsol)
      write(16,*) zbin(j),vtail(j)*xbox*ybox*dz
         xy(n,l,j)=xy(n,l,j)*real(bins)/clock
      enddo
      close(16)

      deallocate(x,y,z,xbin,ybin,zbin,ares,vtail,amass,xy)

99    format(i5,a4,2x,a4,i5,2x,f6.3,2x,f6.3,2x,f6.3)

      end



! 1. find zbin between water and CHX where CHX density is 0 and bin+1 is > 0
! 2. find CHX density and water density below this
! 3. if water density is 0 in zbin and zbin+1 but there is water density in the organic
! layer, the tail is broken
! for the width of the water tail, look at density on x and y at
! different z positions
! split dz up into smaller boxes, if there is water density in the box,
! add it to the water volume or density or something
! alt
! find places in bulk CHX that have space for water by checking phase
! space that is a certain distance away from CHX molecules
! probably won't do alt - checking phase space takes time
!
!write x and y volume stuff for all z bins and just superimpose to find
!the shape of the water tail - ua/ben/slow/24384 goes through the middle
!- can use that as proof of concept
