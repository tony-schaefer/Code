      program gmx2amb

      logical help
      integer n,natoms,nmol,nat,i,j,onmol
      character str*80,gmxconf*80,archivepath*80,atompdb*80,mol*4,at*3
      
      atompdb="out.pdb"

      help=.false.

      if(iargc().eq.0) help=.true.

      do i=1,iargc()

       call getarg(i,str)

       str=trim(str)

       if(str.eq."-c") then
        call getarg(i+1,gmxconf)
        gmxconf=trim(gmxconf)
        write(*,*) gmxconf(1:len(gmxconf))
       endif

       if(str.eq."-a") then
        call getarg(i+1,archivepath)
        archivepath=trim(archivepath)
        write(*,*) archivepath(1:len(archivepath))
       endif

       if(str.eq."-o") then
        call getarg(i+1,atompdb)
        atompdb=trim(atompdb)
        write(*,*) atompdb
       endif

       if(str.eq."-h") help=.true.

      enddo

      if(help) then
        print*, "convert .gro's to .pdb's"
        print*, "-c : input .gro"
        print*, "-o : output .pdb"
        print*, "-h help"
        goto 97
      endif

      open(6,file=gmxconf)
      open(7,file=atompdb)

      read(6,*) str
      read(6,*) natoms

      do j=1,natoms

       read(6,98) nmol,mol,at,nat,x,y,z
       if(j.eq.1) onmol=nmol
       x=10.*x
       y=10.*y
       z=10.*z
       if(onmol.ne.nmol) write(7,'(a)') "TER"
       onmol=nmol
       write(7,99) "HETATM",nat,at,mol,nmol,x,y,z,1.00,0.00
      
      enddo

      write(7,'(a)') "TER"
      write(7,'(a)') "END"

      close(6)
      close(7)

97    help=.true.

99    format(a6,i5,2x,a3,1x,a4,1x,i4,4x,3(1x,f7.3),2x,f4.2,1x,f4.2)
98    format(i5,a4,3x,a3,i5,3(2x,f6.3))

      end
