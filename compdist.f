      program compile_distdotout

      integer d,lastfort
      double precision time,distance

      open(7,file='dist.out')
      read(5,*) lastfort
      do d=10,lastfort
      open(d)
      do
      read(d,*,iostat=iostatus) time, distance
      if(iostatus.ne.0) exit
      write(7,*) time, distance
      enddo
      enddo
      close(7)
      end
