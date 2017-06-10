      program pick_CL

      integer iatoms,deleted
      character adummy*32
      character,allocatable::atsym(:)*4,ares(:)*4
      integer,allocatable:: ires(:),iatom(:)
      double precision,allocatable:: x(:),y(:),z(:),vx(:),vy(:),vz(:)
      double precision xbox,ybox,zbox
      real cutoff
      open(7,file='removed.gro')
      cutoff = 9.
      read(5,'(a32)') adummy
      read(5,'(i6)') iatoms
      write(*,*) iatoms
      allocate(x(iatoms),y(iatoms),z(iatoms),vx(iatoms),vy(iatoms),
     +vz(iatoms),ires(iatoms),iatom(iatoms),ares(iatoms),atsym(iatoms))
      deleted = 0
      k=0
      do i=1,iatoms
       read(5,99) ires(i),ares(i),atsym(i),iatom(i),x(i),y(i),z(i),vx(i)
     +,vy(i),vz(i)
       atsym(i) = adjustl(atsym(i))
       if(atsym(i).eq.'HW2'.and.z(i).lt.cutoff.and.z(i-1).lt.cutoff.and.
     +z(i-2).lt.cutoff) then
       k = k + 3
       endif
      enddo
      read(5,*) xbox,ybox,zbox
      write(7,'(a32)') adummy
      write(7,'(i6)') iatoms - k
      do i=1,iatoms
       if(atsym(i).eq.'OW'.and.z(i).lt.cutoff.and.z(i+1).lt.cutoff.and.
     +z(i+2).lt.cutoff) then
       deleted = deleted + 1
       elseif(atsym(i).eq.'HW1'.and.z(i).lt.cutoff.and.z(i+1).lt.cutoff
     +.and.z(i-1).lt.cutoff) then
        deleted = deleted + 1
       elseif(atsym(i).eq.'HW2'.and.z(i).lt.cutoff.and.z(i-1).lt.cutoff
     +.and.z(i-2).lt.cutoff) then
        deleted = deleted + 1
       else
       write(7,99) ires(i),ares(i),atsym(i),iatom(i),x(i),y(i),z(i),
     +vx(i),vy(i),vz(i)
       endif
      enddo
      write(7,'(2x,3(f8.5,2x))')xbox,ybox,zbox
      deallocate(x,y,z,vx,vy,vz,ires,ares,atsym,iatom)
      close(7)
      write(*,*) deleted,k
99    format(i5,a4,2x,a4,i5,2x,f6.3,2x,f6.3,2x,f6.3,2x,f6.3,2x,f6.3,2x,
     +f6.3)
      end 
