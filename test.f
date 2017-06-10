      program test

      character*90,allocatable:: files(:)
      integer i,sz,ms,m
      character*99 d

      d='../nvt/'

      call numfiles(i,d)

      allocate(files(i))

      call listdir(files,d,i)

      m=1
      ms=-1

      do k=1,i
        inquire(file='../nvt/'//files(k), size=sz)
        if(sz.gt.ms) then
          m=k
          ms=sz
        endif
      enddo

      print*, trim(files(m)), 'is the largest'

      deallocate(files)

      end
