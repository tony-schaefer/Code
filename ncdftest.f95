      program teststuff

      use netcdf

      implicit none

      integer:: frame,atom,ncid,varid
      real:: cell_spatial(3),cell_angular(3),spatial(3)
      character:: filename*90

      call getarg(1,filename)
      
      call check(nf90_open(filename,NF90_NOWRITE,ncid))

      end program teststuff
