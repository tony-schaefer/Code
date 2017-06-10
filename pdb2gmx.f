      program gmx2amb

      logical help
      integer n,natoms,nmol,nat,i,j,onmol
      character str*80,gmxconf*80,archivepath*80,atomgro*80,mol*4,at*3
      
      atomgro="out.gro"

      help=.false.

      if(iargc().eq.0) help=.true.

      do i=1,iargc()

       call getarg(i,str)

       str=trim(str)

       if(str.eq."-o") then
        call getarg(i+1,gmxconf)
        gmxconf=trim(gmxconf)
        write(*,*) gmxconf(1:len(gmxconf))
       endif

       if(str.eq."-a") then
        call getarg(i+1,archivepath)
        archivepath=trim(archivepath)
        write(*,*) archivepath(1:len(archivepath))
       endif

       if(str.eq."-c") then
        call getarg(i+1,atomgro)
        atomgro=trim(atomgro)
        write(*,*) atomgro
       endif

       if(str.eq."-h") help=.true.

      enddo

      if(help) then
        print*, "convert .gro's to .pdb's"
        print*, "-c : input .pdb"
        print*, "-o : output .gro"
        print*, "-h help"
        goto 97
      endif

      open(6,file=gmxconf)
      open(7,file=atomgro)

      do 
       read(7,'(a)',iostat=ios) str
       print*, str(1:6)
       if(str(1:6).eq.'ATOM'.or.str(1:6).eq.'HETATM') then
        print*, 'hi'
        read(str,99) nat,at,mol,nmol,x,y,z
        x=.1*x
        y=.1*y
        z=.1*z
        write(6,98) nmol,mol,at,nat,x,y,z
       endif
       if(ios.ne.0) exit
      
      enddo

      write(7,'(a)') "TER"
      write(7,'(a)') "END"

      close(6)
      close(7)

97    help=.true.

99    format(6x,i5,2x,a3,1x,a4,1x,i4,4x,3(1x,f7.3))
98    format(i5,a4,3x,a3,i5,3(2x,f6.3))

      end
