      program pick_CL

      character adummy,atsym*4
      integer iatoms,ires,cl,iatom

c this program reads nvt_equib.gro and tells you the atomic number of the last CL and how many chlorides there are
      read(5,*) adummy
      read(5,*) iatoms
      cl = 0
      do i=1,iatoms
       read(5,99) ires,ares,atsym,iatom,x,y,z
       atsym(1:4) = adjustl(atsym(1:4))
       if(atsym(1:3).eq.'CL ') then
       lastcl = iatom
       cl = cl + 1
       endif
      enddo
      write(*,'(a14,1x,i5)') "the last Cl is", lastcl
      write(*,'(a9,1x,i3,1x,a9)') 'there are',cl,'chlorides'
99    format(i5,a4,2x,a4,i5,2x,f6.3,2x,f6.3,2x,f6.3)
      end 
