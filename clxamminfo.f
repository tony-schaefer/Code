      program where_in_the_world_is_carmen_chloridiago

      !ifort self.f -Ofast -o self.e

      real xd,yd,zd,md,sd,xbox,ybox,zbox,pie
      real,allocatable::x(:),y(:),z(:),thetr(:),dclr(:),dnr(:),im(:),
     +bclr(:,:),bnr(:,:),thetb(:,:),bin(:),countcount(:),rat(:)
      integer clspec,iatoms,i,k,cat,charform,f,bins
      integer,allocatable:: ires(:),ik(:)
      character agroup*11,header*160,adummy*160
      character,allocatable:: atsym(:)*4

c all catalysts have the same general stucture
c     R
c     |
c   R-N-R
c     |
c     R
c so where does the chloride like to hang out relative to the R's?
c this program finds the R-N-Cl angles

      open(8,file='input.dat')
      open(9,file='index.ndx')
      open(16,file='clrn.angles')

      pie=acos(-1.)

      do
      read(9,'(a11)',iostat=iostatus) agroup
      if(iostatus.ne.0) then
      print*,"no CL_spec group was found in index.ndx"
      exit
      endif
      if(agroup(1:11).eq.'[ CL_spec ]') then
      read(9,*) clspec
      exit
      endif
      enddo
c read the index and find which chloride we care about
      close(9)

      read(8,*) bins
      read(8,*) itime

      allocate(im(4),ik(4),thetr(0:4),dnr(4),dclr(4),bclr(4,bins),
     +thetb(4,bins),bnr(4,bins),bin(0:bins),countcount(bins),rat(bins))

      do j=1,bins
      bin(j)=0.
      countcount(j)=0.
      rat(j)=0.
      do i=1,4
      bclr(i,j)=0.
      bnr(i,j)=0
      thetb(i,j)=0
      enddo
      enddo
c initialize variables

      read(5,'(a160)') header
      do f=1,159
      if(header(f:f+1).eq.'t=') charform = f+1
      enddo
      rewind(5)
c figure out how to read what timestep it's at

      q=0
      clock=0
      do
      q=q+1
      read(5,'(a,f8.2)',iostat=iostatus) adummy(1:charform),timestep
      
      if(iostatus.ne.0) exit
c do stuff until there's nothing left to do
      
      if(mod(timestep,100.).eq.0) write(*,*) nint(timestep)
      
      read(5,*) iatoms
      
      if(q.eq.1) allocate(ires(iatoms),x(iatoms),y(iatoms),z(iatoms),
     +atsym(iatoms))
      
      do i=1,iatoms
        read(5,99) ires(i),ares,atsym(i),iatom,x(i),y(i),z(i)
        atsym(i)=adjustl(atsym(i))
c remember all atoms and their coordinates
      enddo

      read(5,*,iostat=iostatus) xbox,ybox,zbox

      if(timestep.lt.itime) cycle
c do stuff only when we want

      clock=clock+1

      if(clock.eq.1.) then
      bin(0)=0
      do j=1,bins
      bin(j)=bin(j-1)+(zbox/real(bins))
      enddo
      endif
      
      md=xbox**2+ybox**2+zbox**2    
      thetr(0)=0.
      do i=1,4
      im(i)=md
      ik(i)=0
      thetr(i)=360.
      enddo
c initialize variables 

      do i=1,iatoms

      if(atsym(i).eq."NAA".or.atsym(i).eq."NAI".or.atsym(i).eq."N22")
     &then
c catalyst nitrogens are all NAA or NAI      
      if(xbox-abs(x(clspec)-x(i)).gt.abs(x(clspec)-x(i))) then
      xd=x(clspec)-x(i)
      else
      xd=xbox-abs(x(clspec)-x(i))
      endif

      if(ybox-abs(y(clspec)-y(i)).gt.abs(y(clspec)-y(i))) then
      yd=y(clspec)-y(i)
      else
      yd=ybox-abs(y(clspec)-y(i))
      endif

      if(zbox-abs(z(clspec)-z(i)).gt.abs(z(clspec)-z(i))) then
      zd=z(clspec)-z(i)
      else
      zd=zbox-abs(z(clspec)-z(i))
      endif

      sd=xd**2+yd**2+zd**2

      if(sd.lt.md) then
      md=sd
      cat=i
      endif
      
      endif

      enddo
c find the closest nitrogen to the chloride

      do i=1,iatoms

      if(ires(i).eq.ires(cat)) then

      if(xbox-abs(x(cat)-x(i)).gt.abs(x(cat)-x(i))) then
      xd=x(cat)-x(i)
      else
      xd=xbox-abs(x(cat)-x(i))
      endif

      if(ybox-abs(y(cat)-y(i)).gt.abs(y(cat)-y(i))) then
      yd=y(cat)-y(i)
      else
      yd=ybox-abs(y(cat)-y(i))
      endif

      if(zbox-abs(z(cat)-z(i)).gt.abs(z(cat)-z(i))) then
      zd=z(cat)-z(i)
      else
      zd=zbox-abs(z(cat)-z(i))
      endif

      sd=xd**2+yd**2+zd**2

        if(sd.lt.im(4).and.sd.ge.im(3)) then
         im(4) = sd
         ik(4) = i
        endif
        if(sd.lt.im(3).and.sd.ge.im(2)) then
         im(4) = im(3)
         ik(4) = ik(3)
         im(3) = sd
         ik(3) = i
        endif
        if(sd.lt.im(2).and.sd.ge.im(1)) then
         im(4) = im(3)
         ik(4) = ik(3)
         im(3) = im(2)
         ik(3) = ik(2)
         im(2) = sd
         ik(2) = i
        endif
        if(sd.lt.im(1).and.sd.ne.0.) then
         im(4) = im(3)
         ik(4) = ik(3)
         im(3) = im(2)
         ik(3) = ik(2)
         im(2) = im(1)
         ik(2) = ik(1)
         im(1) = sd
         ik(1) = i
        endif
       endif
             
       enddo
c find the four closest whatevers to the nitrogen that's closest the the
c chloride 
 
      do k=1,4

      if(xbox-abs(x(cat)-x(ik(k))).gt.abs(x(cat)-x(ik(k)))) then
      if(x(ik(k)).gt.x(cat)) then
      xd=-abs(x(cat)-x(ik(k)))
      else
      xd=abs(x(cat)-x(ik(k)))
      endif
      else
      if(x(ik(k)).lt.x(cat)) then
      xd=-abs(xbox-abs(x(cat)-x(ik(k))))
      else
      xd=abs(xbox-abs(x(cat)-x(ik(k))))
      endif
      endif

      if(ybox-abs(y(cat)-y(ik(k))).gt.abs(y(cat)-y(ik(k)))) then
      if(y(ik(k)).gt.y(cat)) then
      yd=-abs(y(cat)-y(ik(k)))
      else
      yd=abs(y(cat)-y(ik(k)))
      endif
      else
      if(y(ik(k)).lt.y(cat)) then
      yd=-abs(ybox-abs(y(cat)-y(ik(k))))
      else
      yd=abs(ybox-abs(y(cat)-y(ik(k))))
      endif
      endif

      if(zbox-abs(z(cat)-z(ik(k))).gt.abs(z(cat)-z(ik(k)))) then
      if(z(ik(k)).gt.z(cat)) then
      zd=-abs(z(cat)-z(ik(k)))
      else
      zd=abs(z(cat)-z(ik(k)))
      endif
      else
      if(z(ik(k)).lt.z(cat)) then
      zd=-abs(zbox-abs(z(cat)-z(ik(k))))
      else
      zd=abs(zbox-abs(z(cat)-z(ik(k))))
      endif
      endif

      dnr(k)=sqrt(xd**2+yd**2+zd**2)
c distance between N and R
     
      if(xbox-abs(x(clspec)-x(ik(k))).gt.abs(x(clspec)-x(ik(k)))) then
      if(x(ik(k)).gt.x(clspec)) then
      xd=-abs(x(clspec)-x(ik(k)))
      else
      xd=abs(x(clspec)-x(ik(k)))
      endif
      else
      if(x(ik(k)).lt.x(clspec)) then
      xd=-abs(xbox-abs(x(clspec)-x(ik(k))))
      else
      xd=abs(xbox-abs(x(clspec)-x(ik(k))))
      endif
      endif

      if(ybox-abs(y(clspec)-y(ik(k))).gt.abs(y(clspec)-y(ik(k)))) then
      if(y(ik(k)).gt.y(clspec)) then
      yd=-abs(y(clspec)-y(ik(k)))
      else
      yd=abs(y(clspec)-y(ik(k)))
      endif
      else
      if(y(ik(k)).lt.y(clspec)) then
      yd=-abs(ybox-abs(y(clspec)-y(ik(k))))
      else
      yd=abs(ybox-abs(y(clspec)-y(ik(k))))
      endif
      endif

      if(zbox-abs(z(clspec)-z(ik(k))).gt.abs(z(clspec)-z(ik(k)))) then
      if(z(k).gt.z(clspec)) then
      zd=-abs(z(clspec)-z(ik(k)))
      else
      zd=abs(z(clspec)-z(ik(k)))
      endif
      else
      if(z(ik(k)).lt.z(clspec)) then
      zd=-abs(zbox-abs(z(clspec)-z(ik(k))))
      else
      zd=abs(zbox-abs(z(clspec)-z(ik(k))))
      endif
      endif
c periodic boundry conditions are fun

      dclr(k)=sqrt(xd**2+yd**2+zd**2)
c distance between Cl and R      

      angle=acos((dclr(k)**2-md-dnr(k)**2)/(-2.*dnr(k)*sqrt(md)))
c law of cosines, md is the squared distance between Cl and N

      do i=4,1,-1
      if(angle.lt.thetr(i).and.angle.ge.thetr(i-1)) then
      do j=4,i+1,-1
      thetr(j)=thetr(j-1)
      enddo
      thetr(i)=angle
      endif
      enddo
      
      enddo
c find the N-R-Cl angles and sort them smallest to largest

!      write(12,*)timestep,(thetr(k)*180./pie,k=1,4)
c convert to degrees, pie is pi

      do k=1,4
      do j=1,bins
      if(z(clspec).gt.bin(j-1).and.z(clspec).le.bin(j).or.j.eq.1.and.
     +z(clspec).le.0.or.j.eq.bins.and.z(clspec).gt.zbox) then
      thetb(k,j)=thetb(k,j)+thetr(k)
      bnr(k,j)=bnr(k,j)+dnr(k)
      bclr(k,j)=bclr(k,j)+dclr(k)
      if(k.eq.1) rat(j)=rat(j)+sqrt(md)
      if(k.eq.1) countcount(j)=countcount(j)+1.
c countcount counts how many times things are found in the jth bin
      endif
      enddo
      enddo

      enddo
! out of things to do

      write(16,*) '@ title "Cl-N-R angle"'
      write(16,*) '@ xaxis label "z"'
      write(16,*) '@ yaxis label "degrees"'

      do j=1,bins
      if(countcount(j).gt.0) then
      do k=1,4
      thetb(k,j)=thetb(k,j)/countcount(j)
      enddo
      write(16,*) bin(j)-zbox/real(2.*bins),(thetb(k,j)*180./pie,k=1,4)
      rat(j)=rat(j)/countcount(j)
!      write(19,*) bin(j)-zbox/real(2.*bins),rat(j) 
      endif
      enddo
c write stuff
      
      close(8)
!      close(10)
!      close(11)
      close(16)

      deallocate(ires,atsym,x,y,z,dnr,dclr,thetr,im,ik,bin,bnr,bclr,
     +thetb,countcount,rat)

99    format(i5,a4,2x,a4,i5,2x,f6.3,2x,f6.3,2x,f6.3)

      end
