      program how_dense_something_is

!     ifort -Ofast gromacs-things.f90 -o densiness.e densiness.f -ldl -lstdc++ libxtc.a 

      use bromacs

      implicit none

      integer charform,natom,bins,rezidoos,itime,ftime,idumm,ax,
     +handle(4),status, ires,i,j,k,l,m,n,o,ios
      integer, allocatable:: g(:),iatom(:)
      character header*150,adummy*150,str*80,axis*1,formut*3,
     +outfile*80,infile*300,framefile*300,specform*3,ndx*90,gr*90
      character,allocatable:: mol(:)*4,ares(:)*4,atsym(:)*4
      real box(6),timestep,clock,sols,comxyz
      real,allocatable:: amass(:),axyz(:,:),xyz(:)
      double precision vol,dxyz
      double precision,allocatable:: bin(:),rho(:,:)

c input.dat has how many sections to split the box into and when to
c start finding densities
      bins=100
      itime=0
      outfile="densities.dat"
      ftime=-1
      axis="z"
      ios=0
      framefile=" "
      gr='-1'
      ndx='-1'

      if(iargc().eq.0) then
       print*,'use -f to specify filename'
       print*,'use -h for help'
       goto 97
      endif

      do i=1,iargc()

       call getarg(i,str)

       if(str.eq."-bins") then
        call getarg(i+1,str)
        read(str,'(i80)') bins
       endif

       if(str.eq."-itime") then
        call getarg(i+1,str)
        read(str,'(i80)') itime
       endif

       if(str.eq."-ftime") then
        call getarg(i+1,str)
        read(str,'(i80)') ftime
       endif

       if(str.eq."-o") then
        call getarg(i+1,outfile)
        outfile=adjustl(outfile)
       endif

       if(str.eq."-h") then
        print*," "
        print*,"finds densities of residues in MD trajectories"
        print*," -bins  : (100) number of histograms"
        print*," -itime : (0) initial time"
        print*," -ftime : (-1) final time"
        print*," -f     : input .gro/.pdb/.xtc trajectory; non-ASCII
     + must use -s option"
        print*," -s     : input .gro/.pdb system frame"
        print*," -o     : (densities.dat) output densities"
        print*," -axis  : (z) find density on x, y, or z axis"
        print*," -g     : additional group from index file (hella slow)"
        print*,"-ndx    : index file"
        goto 97
       endif

       if(str.eq."-axis") then
        call getarg(i+1,axis)
       endif

       if(str.eq."-f") then
        call getarg(i+1,infile)
       endif

       if(str.eq."-s") then
        call getarg(i+1,framefile)
       endif

       if(str.eq.'-ndx') then
         call getarg(i+1,ndx)
       endif

       if(str.eq.'-g') then
         call getarg(i+1,gr)
       endif
      
      enddo

      infile=adjustr(infile)
      formut=infile(298:300)
      infile=adjustl(infile)
      if(formut.ne.'gro'.and.formut.ne.'pdb'.and.formut.ne.'xtc') then
       print*,"use -f to specify input file"
       print*,"use -h for help"
       goto 97
      endif
      print*, "reading ",trim(infile)," in ",formut," format"

      if(formut.eq.'gro'.or.formut.eq.'pdb') then
       open(9,file=infile)
      endif

      allocate(bin(0:bins),rho(99,bins),mol(99))
c bin marks the boundry of each section; rho is the density of each type
c of molecule found for each of the sections; mol is the name of each
c type of molecule
      
      do n=1,99
       mol(n)="    "
       do j=1,bins
       rho(n,j)=0. 
       enddo
      enddo
     
      if(formut.eq.'xtc') then
       if(framefile.eq.' ') then
        print*,'must use -s with an xtc file'
        print*,'use -h for help'
        goto 97
       endif
       framefile=adjustr(framefile)
       specform=framefile(298:300)
       framefile=adjustl(framefile)
       open(10,file=framefile)
       if(specform.eq.'gro') then
         read(10,*) header
         read(10,*) natom
         allocate(iatom(0:natom),ares(0:natom),atsym(0:natom),
     +            xyz(3*natom))
         do i=1,natom
          read(10,96) ares(i),atsym(i),iatom(i)
         enddo
       elseif(specform.eq.'pdb') then
         natom=0
         do
           read(10,*,iostat=ios) str
           if(ios.ne.0) exit
           if(str(1:4).eq.'ATOM'.or.str(1:6).eq.'HETATM') then
             natom=natom+1
           endif
         enddo
         allocate(iatom(0:natom),ares(0:natom),atsym(0:natom),
     +            xyz(3*natom))
         rewind(10)
         i=0
         do   
          read(10,'(a80)',iostat=ios) str
          if(ios.ne.0) exit
          if(str(1:4).eq.'ATOM'.or.str(1:6).eq.'HETATM') then
           i=i+1
           if(i.gt.natom) exit
           read(str,98) iatom(i),atsym(i),ares(i),ires,(xyz(j),j=1,3)
          endif
         enddo
       endif
       call f77_molfile_init
       call f77_molfile_open_read(handle(1),natom,infile,'xtc')
       close(10)
      endif


      rezidoos=0
      if(gr.ne.'-1') then
        if(ndx.eq.'-1') then
          print*, "specify an index file if you're going to use a group"
          goto 97
        endif
        gr=adjustl(gr)
        call readndx(ndx,gr,g)
        ! read group from index file
        o=size(g)
        if(o.eq.0) goto 97
        mol(1)=adjustl(gr)
        rezidoos=1
      endif

      if(axis.eq.'x') ax=1
      if(axis.eq.'y') ax=2
      if(axis.eq.'z') ax=3

c initialize variables

      clock=0.
      k=0
      comxyz=0.
      sols=0.
c clock counts the number of times densities are found so they can be
c averaged
c k is a flag
c rezidoos is how many different types of molecules are found

      if(formut.eq.'pdb') then
       read(9,*) str,box
      endif

      if(formut.eq.'gro') then
       read(9,'(a150)') header
       do n=1,149
        if(header(n:n+1).eq.'t=') charform=n+1
       enddo
      elseif(formut.eq.'pdb') then
       natom=0
       do
        read(9,*) str
        if(str.eq.'ATOM'.or.str.eq.'HETATM') natom=natom+1
        if(str.eq.'ENDMDL') exit
       enddo
      endif
c figure out how to read the timestep

      rewind(9)
      ios=0

      do
       k=k+1
       if(formut.eq.'gro') then
        read(9,'(a,f10.5)',iostat=ios) adummy(1:charform),timestep
        if(timestep.gt.ftime.and.ftime.gt.0) exit
        if(ios.ne.0) exit
        if(mod(timestep,100.).eq.0.) print*,nint(timestep) 
       endif

       if(formut.eq.'gro') then
        read(9,*) natom
       endif
c read how many atoms their are       

       if(k.eq.1) allocate(axyz(0:natom,3),amass(0:natom))
       if(k.eq.1.and.formut.ne.'xtc') then
        allocate (ares(0:natom),atsym(0:natom),iatom(0:natom))
        timestep=0.
       endif
       
       i=0
        
       if(ios.ne.0) exit

       do 
        if(timestep.gt.ftime.and.ftime.ge.0) goto 95
         if(formut.eq.'gro') then
          i=i+1
          if(i.gt.natom) exit
          read(9,99) ires,ares(i),atsym(i),iatom(i),(axyz(i,j),j=1,3)
         elseif(formut.eq.'pdb') then
          read(9,'(a80)',iostat=ios) str
          if(i.gt.natom) exit
          if(str(1:5).eq.'MODEL') then
           backspace(9)
           read(9,*) str,timestep
           if(mod(timestep,100.).eq.0.) print*,nint(timestep) 
          endif
          if(str(1:4).eq.'END ') then
           ios=1
           exit
          endif
          if(str(1:4).eq.'ATOM') then
           i=i+1
           if(i.gt.natom) exit
            read(str,98) iatom(:),atsym(i),ares(i),ires,
     +                   (axyz(i,j),j=1,3)
          endif
         elseif(formut.eq.'xtc') then
           i=i+1
           if(i.gt.natom) exit
           if(i.eq.1) then
            timestep=timestep+1.
            status = 1 ! status=1 on entry means read
            call f77_molfile_read_next(
     +          handle(1),natom,xyz(1),box,status)
            if(mod(timestep,100.).eq.0.) print*, nint(timestep) 
            if (status.eq.0) goto 95
           endif
           axyz(i,1)=xyz(i*3-2)
           axyz(i,2)=xyz(i*3-1)
           axyz(i,3)=xyz(i*3)
         endif

         if(k.eq.1) then
           amass(i)=1.00800
           atsym(i)=adjustl(atsym(i))
           ares(i)=adjustl(ares(i))
           if(atsym(i)(1:1).eq.'O') amass(i)=15.99940
           if(atsym(i)(1:1).eq.'H') amass(i)=1.00800
           if(atsym(i)(1:1).eq.'C') amass(i)=12.01100
           if(atsym(i)(1:1).eq.'N') amass(i)=14.00670
           if(atsym(i)(1:1).eq.'F') amass(i)=18.9984
           if(atsym(i)(1:2).eq.'CL') amass(i)=35.45300
           if(ares(i)(1:2).eq.'CL') amass(i)=35.45300
           if(ares(i)(1:2).eq.'NA') amass(i)=22.98977
         endif
c read atoms and their positions and figure out the masses
       
         if(k.eq.1.and.i.gt.0) then
          do n=1,99
           if(ares(i).eq.mol(n)) then
            exit
           elseif(mol(n).eq.'    ') then
            mol(n)=ares(i)
            print*,'found ',trim(mol(n)),' residue'
            rezidoos=rezidoos+1
            exit
           endif
          enddo
         endif
c figure out what types of atoms are in the thing
        if(k.eq.1.and.ares(i).eq.'SOL') then
         comxyz=comxyz+axyz(i,ax)*amass(i)
         sols=sols+amass(i)
        endif
       enddo
       
       if(k.eq.1) then
        comxyz=comxyz/sols
       endif
       
       if(formut.eq.'gro') then
         read(9,*) (box(j),j=1,3) 
       endif
c read box size
      
c print out progress
       
       if(timestep.lt.itime) cycle
c do density stuff when it's time
       
       clock=clock+1.
c count how many times density stuff is calculated
       if(clock.eq.1.) print*,"averaging densities"
     
       dxyz=box(ax)/real(bins)
c how wide each section is
      
       bin(0)=0.
       do j=1,bins
        bin(j)=bin(j-1)+dxyz
       enddo
c section boundries
      
       do j=1,bins
        do i=1,natom
         if(axyz(i,ax).le.0.) axyz(i,ax)=axyz(i,ax)+box(ax)
         if(axyz(i,ax).ge.box(ax)) axyz(i,ax)=axyz(i,ax)-box(ax)
         do n=1,rezidoos
         if(gr(1:4).eq.mol(n)) then
          ! find density of special group
          do m=1,o
           if(g(m).eq.iatom(i)) then
            if(axyz(i,ax).gt.bin(j-1).and.axyz(i,ax).le.bin(j)) then
             rho(n,j)=rho(n,j)+amass(i)
            endif
           endif
          enddo
         endif
          if(ares(i).eq.mol(n)) then
           if(axyz(i,ax).gt.bin(j-1).and.axyz(i,ax).le.bin(j)) then
            rho(n,j)=rho(n,j)+amass(i)
           endif
          endif
         enddo
        enddo
       enddo
c sort things into the sections and account for pbc
      
      enddo

95    vol=dxyz
      do j=1,3
       if(j.ne.ax) vol=vol*box(j)
      enddo

c the volume of each section
      do j=1,bins
       do n=1,rezidoos
        if(formut.eq.'gro') rho(n,j)=rho(n,j)/(vol*.602214*clock)
        if(formut.eq.'pdb') rho(n,j)=1000.*rho(n,j)/(vol*.602214*clock)
        if(formut.eq.'xtc') rho(n,j)=1000.*rho(n,j)/(vol*.602214*clock)
       enddo
      enddo
c convert units and average

      if(comxyz.gt.0.) then 
      open(8,file='densities.com')
      write(8,'(a)') '@    title "density"'
      if(formut.eq.'gro')
     +write(8,'(a,a,a)') '@    xaxis label "',axis,' position (nm)"'
      if(formut.eq.'pdb')
     +write(8,'(a,a,a)') '@    xaxis label "',axis,' position (\cE\C)"'
      if(formut.eq.'xtc')
     +write(8,'(a,a,a)') '@    xaxis label "',axis,' position (\cE\C)"'
      write(8,'(a)') '@    yaxis label "density (g/L)"'
      write(8,'(a)') '@TYPE xy'
      write(8,'(a)') '@ view 0.15, 0.15, 1.00, 0.80'
      write(8,'(a)') '@ legend on'
      write(8,'(a)') '@ legend box on'
      write(8,'(a)') '@ legend loctype view'
      write(8,'(a)') '@ legend 1.1, 0.6'
      write(8,'(a,i2)') '@ legend length ',rezidoos
      
      do n=1,rezidoos
       idumm=n-1
c reuse old variable
       write(adummy,'(i2)') idumm
       adummy=adjustl(adummy)
       write(8,'(a,a2,a,a,a)') '@s',adummy(1:2),' legend "'
     + ,mol(n),'"'
      enddo
c fancy-shmancy xmgrace formatting

      do j=1,bins
       write(8,'(100(f20.14,5x))') bin(j)-dxyz/2.-comxyz,
     + (rho(n,j),n=1,rezidoos)
      enddo
      close(8)
      endif
c write density stuff

      open(7,file=outfile)
      write(7,'(a)') '@    title "density"'
      if(formut.eq.'gro')
     +write(7,'(a,a,a)') '@    xaxis label "',axis,' position (nm)"'
      if(formut.eq.'pdb')
     +write(7,'(a,a,a)') '@    xaxis label "',axis,' position (\cE\C)"'
      if(formut.eq.'xtc')
     +write(7,'(a,a,a)') '@    xaxis label "',axis,' position (\cE\C)"'
      write(7,'(a)') '@    yaxis label "density (g/L)"'
      write(7,'(a)') '@TYPE xy'
      write(7,'(a)') '@ view 0.15, 0.15, 1.00, 0.80'
      write(7,'(a)') '@ legend on'
      write(7,'(a)') '@ legend box on'
      write(7,'(a)') '@ legend loctype view'
      write(7,'(a)') '@ legend 1.1, 0.6'
      write(7,'(a,i2)') '@ legend length ',rezidoos

      do n=1,rezidoos
       idumm=n-1
c reuse old variable
       write(adummy,'(i2)') idumm
       adummy=adjustl(adummy)
       write(7,'(a,a2,a,a,a)') '@s',adummy(1:2),' legend "'
     + ,mol(n),'"'
      enddo
c fancy-shmancy xmgrace formatting

      do j=1,bins
       write(7,'(100(f20.14,5x))') bin(j)-dxyz/2.,
     + (rho(n,j),n=1,rezidoos)
      enddo
      close(7)
c write density stuff

      if(formut.eq.'xtc') call f77_molfile_finish

      deallocate(bin,rho,mol,axyz,amass,ares,atsym,iatom)
      if(allocated(g)) deallocate(g)

96    format(5x,a4,2x,a4,i5)
97    i=1
98    format(7x,i5,1x,a4,a4,1x,i5,4x,3(1x,f7.3))
99    format(i5,a4,2x,a4,i5,3(2x,f6.3))

      end
