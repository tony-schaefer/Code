      program primes

      use IFPORT

      implicit none

      integer, allocatable:: primelist(:),templist(:)
      integer:: n,k,kmax,j,m
      character:: arg*90

      call getarg(1,arg)
      read(arg,'(i90)') kmax

      n=2
      k=0

      do
        j=0
        if(k.lt.kmax) exit
        do m=0,size(primelist)-1
          if(mod(n,primelist(m)).eq.0) then
             exit
           endif
           j=m+1
        enddo
        if(j.eq.size(primelist)) then
          if(.not.allocated(primelist)) then
            allocate(primelist(0:0))
            primelist(0)=n
          else
            allocate(templist(0:size(primelist)))
            do m=0,size(primelist)-1
              templist(m)=primelist(m)
            enddo
            templist(size(primelist))=n
            deallocate(primelist)
            call move_alloc(templist,primelist)
          endif
          print*, n
          k=k+1
        endif
        n=n+1
        if(mod(n,2).eq.0) n=n+1
      enddo

      print*, primelist

      end program primes
