      program primes

      implicit none

      integer(kind=selected_int_kind(16))::primelist(0:2**31-2),n,k,m,a

      m=0

      primelist(0)=2

      open(7,file='j5-prime.file')

      print*, size(primelist)

      n=3

      do 
        a=sqrt(real(n))
        do k=1,m
          if(mod(n,primelist(k)).eq.0) goto 7 
          if(primelist(k).gt.a) exit
        enddo
        m=m+1
        primelist(m)=n
        write(7,*) n
7       n=n+2
        if(m.gt.(2**31-2)) exit
      enddo

      do
        a=sqrt(real(n))+1
        do k=1,(2**31-2)
          if(mod(n,primelist(k)).eq.0) goto 8
          if(primelist(k).ge.a) exit
        enddo
        do k=primelist(2**31-2),a,2
          if(mod(n,k).eq.0) goto 8
        enddo
        write(7,*) n
8       n=n+2
      enddo

      close(7)

      end program primes
