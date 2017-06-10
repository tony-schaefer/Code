      program reflect
      
      character*100 header
      double precision x,y,dy,baseline,start
 
      flag = 2
      baseline = 0
      first=1
      start=0
9     do
      read(5,'(a100)') header
      write(6,*) header
      if(header(7:10).eq.'xydy') exit
      enddo
      do
      read(5,*,iostat=iostatus) x,y,dy
      if(first.eq.1) then
      start=x
      first=0
      endif
      if(abs(y).gt.baseline.and.flag.ne.1) baseline = y
      if(iostatus.ne.0.and.flag.ne.1) then
      flag = 1
      rewind(5)
      goto 9
      endif
      if(iostatus.ne.0.and.flag.eq.1) exit
      if(flag.eq.1) write(6,*) -x,y-baseline,dy
      enddo
10    format(f13.10,1x,f13.10,1x,f13.10)
      end
