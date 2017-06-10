      program test

      implicit none

      integer:: i,alloca
      integer:: num
      integer, dimension(:), allocatable:: list
      integer, dimension(:), allocatable:: templist

      do

      read(*,*) num

      if(allocated(list)) then
        alloca=size(list)
        allocate(templist(alloca+1))
        do i=1,alloca
          templist(i)=list(i)
        enddo
        templist(alloca+1)=num
        deallocate(list)
        allocate(list(alloca+1))
        list=templist
        deallocate(templist)
      else
        allocate(list(1))
        list(1)=num
      endif

      print*,list

      enddo

      end 



