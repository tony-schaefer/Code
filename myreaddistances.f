      program read_distances

      integer window
      double precision dist,lastdist
      
      space = 0.3
      
      read(5,*) window,dist
      write(6,*) window,dist
      lastdist = dist
      do
      read(5,*,iostat=iostatus) window,dist
      if(iostatus.ne.0) exit
      if(abs(dist-lastdist).gt.space.and.((window+1.).ne.dist)) then
      write(6,*) window,dist
      lastdist = dist
      endif
      enddo
    
      end
