      program test

      implicit none

      integer:: num(10),n,i,clock(8)
      real:: random

      num=(/1,1,1,2,5,6,7,8,9,10/)
      i=1

      n=count(i.eq.num(:))

      write(*,*) n

      end
