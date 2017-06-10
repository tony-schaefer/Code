      program find_interface_atoms

      character res*4,tag*4,conf*80,g1*4,g2*4
      real,allocatable:: z(:)
      integer,allocatable:: atom(:),list(:)

      
      read(5,*) conf,g1,g2

      open(12,file=conf)
      read(12,*) res
      read(12,*) natoms

      allocate(z(natoms),atom(natoms),list(natoms))

      saz=0.
      sol=0.
      oaz=0.
      os=0.
      k=0

      do i=1,natoms
      list(i)=0
      read(12,99) mol,res,tag,atom(i),x,y,z(i)
      res=adjustl(res)
      if(res.eq.g1) then
      saz=saz+z(i)
      sol=sol+1
      endif
      if(res.eq.g2) then
      oaz=oaz+z(i)
      os=os+1
      endif
      enddo

      read(12,*) xbox,ybox,zbox

      if(sol.gt.0) saz=saz/sol
      if(os.gt.0) oaz=oaz/os

      if(g1.eq."ceil") saz=zbox
      if(g2.eq."ceil") oaz=zbox

      if(saz.gt.oaz) then
      top=saz
      bot=oaz
      else
      top=oaz
      bot=saz
      endif

      do i=1,natoms
      if(z(i).gt.bot.and.z(i).lt.top) then
      k=k+1
      list(k)=atom(i)
      endif
      enddo
      print*,k,top,bot

      open(7,file="index.ndx")
      
      do
      read(7,*,iostat=ios) res
      if(ios.ne.0) exit
      enddo
      write(7,'(a)') "[ interface ]"
      write(7,'(15(i5,1x))') (list(i),i=1,k)

      close(7)
      close(12)

      deallocate(z,atom,list)

99    format(i5,a4,2x,a4,i5,3(2x,f6.3))

      end
