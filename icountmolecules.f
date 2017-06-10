      program icountmolecules

      integer clspec,charform,f,waters
      real,allocatable:: x(:),y(:),z(:),d(:)
      character adummy*150
      character,allocatable:: atsym(:)*4

      open(10,file='index.ndx')
      do
       read(10,'(a150)',iostat=iostatus) adummy
       if(iostatus.ne.0) exit
       if(adummy(1:11).eq.'[ CL_spec ]') then
        read(10,'(i5)') clspec
        exit
       endif
      enddo
      close(10)
c find which Cl is special by reading the index

      read(5,'(a150)') adummy
      
      do f=1,149
       if(adummy(f:f+1).eq.'t=') charform = f+1
      enddo
c figure out how to read what timestep it is
      
      rewind(5)

      k=0
      comz=0.
      sols=0.

      do
c do stuff

       read(5,'(a,f10.5)',iostat=iostatus) adummy(1:charform),timestep
       if(iostatus.ne.0) exit
c if there's nothing more to do, don't try to keep doing things

       read(5,*) natoms
      
       if(mod(timestep,100.).eq.0) write(*,*) nint(timestep)
c spit out progress so I don't think the program is borked

      if(k.eq.0) allocate(atsym(natoms),x(natoms),y(natoms),z(natoms),
     + d(natoms))
c allocate stuff once

       do i=1,natoms       
        read(5,99)  ires,mres,atsym(i),iatom,x(i),y(i),z(i)
        atsym(i)=adjustl(atsym(i))
c read atom names and positions

        if(atsym(i).eq.'OW'.and.k.eq.0) then
         comz=comz+z(i)
         sols=sols+1.
        endif

       enddo
       read(5,*) xbox,ybox,zbox
c box size is useless here

       if(k.eq.0) then
        comz=comz/sols
       endif

       k=1

       do i=1,natoms
      
        if(atsym(i).eq.'OW') then

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
 
         d(i)=sqrt(xd**2+yd**2+zd**2)
c find the distance between the special Cl and water oxygens

        endif
        
       enddo

       waters=0
       one=0

       do

        do i=1,natoms
         if(d(i).gt.one.and.d(i).le.two.and.atsym(i).eq.'OW') then
          two=d(i)
         endif
        enddo
c find the next closest water molecule to the special chloride

        if((two-one).gt.0.7.or.waters.ge.100) exit
c having a trail over 100 water molecules large would be silly for this
c system, and the water trail breaks around the point when the closest
c water molecule is 0.7 nm farther away form the chloride than the next
c closest

        one=two
        two=zbox
        waters=waters+1
c count how many water molecules are in the trail

       enddo
       
       if(waters.lt.100) then
        open(16,file="trail.dat")
        write(16,*) "waters in trail:",waters
        write(16,*) "time the trail broke:",timestep
        write(16,*) "distance from water COM:",abs(z(clspec)-comz)
c spit out how many water molecules are in the trail after it breaks and
c when it broke, then stop doing things
        close(16)
        exit
       endif

      enddo


      deallocate(atsym,x,y,z,d)

99    format(i5,a4,2x,a4,i5,2x,f6.3,2x,f6.3,2x,f6.3) 

      end
