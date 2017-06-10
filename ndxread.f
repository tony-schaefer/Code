      program read_index
      character*11 agroup
      integer groups,clgroup

      groups = 0
      do
      read(5,'(a11)',iostat=iostatus) agroup
      if(iostatus.ne.0) exit
      if(agroup(1:1).eq.'[') groups = groups + 1
      if(agroup(1:7).eq.'[ CL ]') then
      clgroup = groups
      write(*,'(a,1x,i2,1x,a)') 'group',clgroup,'is chlorides'
      endif
      enddo
      write(*,'(a,1x,i2,1x,a)') 'there are',groups,'groups'
      end
