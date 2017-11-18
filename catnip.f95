      program catnip

      use IFPORT
! comment ^ out if not using ifort
! need IFPORT to make system calls
! i think system calls work differently with gfortran, but i have not
! tested
! ifort gromacs-things.f90 -Tf catnip.f95 -free -Ofast -o catnip.e -fpe0
! gfortran gromacs-things.f90 catnip.f95 -Ofast -o catnip.e
     
! known bugs
! 1. molecules can be deleted even if they are not flagged as deletable
!      this happens because the input coordinate file might not have the
!      molecules numbered off like I'd expect
!      it happens more with packmol files because those double down on
!      molecule numbers, and assumes the sequence letter will be enough
!      to differentiate them, but I don't pay much attention to the
!      sequence
!      I'll fix it eventually
!      maybe

      use bromacs
! bromacs is a custom module that has a subroutine for reading index
! files; it's in gromacs-things.f90

      implicit none

      logical:: help,ential,grid,mute,warn,found,boxguessed,wrap
! get real
      real, allocatable:: xyz(:,:),nxyz(:,:),tryxyz(:,:)
      real:: dist,c1,c2,com1,com2,box(3),bot,top,cx,cy,cz,randumb,psi,&
      theta,dxyz(3),a1,a2,a3,b1,b2,b3,nb,whattimeisit,a,b,c,&
      gridbox(1000000,3),cent(3)
! get really real
      double precision:: R(3,3),dx,dy,dz,pie,vx,vy,vz
      integer:: i,j,k,l,m,n,o,tries,attempt,boxatoms,newatoms,lim,&
      ilist(8),deleted,groups,molsofthat,nextatom,nextmol,igroup,&
      rngesus,ios,intnum,moldel,lugicul,brexit,limit,alloc
      integer, allocatable:: groupies(:),molnum(:),atnum(:),nmolnum(:)& 
      ,natnum(:),catdel(:),tempint(:),group1(:),group2(:)
      character:: str*150,agroup*90,g1*90,g2*90,outbox*90,outtop*90,&
      molfile*90,boxfile*90,dlist*90,ndxfile*90,axis,topfile*90,fi*4&
      ,fo*4,fn*4
      character, allocatable:: molres(:)*4,atres(:)*4,nmolres(:)*4,&
      natres(:)*4,delgroups(:)*4,s_chain(:),n_chain(:)

      help=.False.
      grid=.False.
      mute=.False.
      warn=.False.
      found=.False.
      boxguessed=.False.
      brexit=0
      wrap=.True.

      if(iargc().lt.3) help=.True.
! help if there is no info at runtime

      i=0
      igroup=-1
      boxfile="a1"
      topfile="a1"
      molfile="a1"
      outbox="catnip.gro"
      outtop="catnip.top"
! default outs
      dlist=" "
      ndxfile="-1"
! default index file
      agroup="-1"
      tries=-1
      g1="6.02"
      g2="3.14"
      axis='z'

      do i=1,iargc()
! read command line stuff 
        call getarg(i,str)
        if(str.eq."-c") then
          call getarg(i+1,boxfile)
          ! system to which the new molecules are added
        endif
        if(str.eq."-oc") then
         call getarg(i+1,outbox)
         ! output system
        endif
        if(str.eq."-p") then
         call getarg(i+1,topfile)
         ! input topology file; not necessary, only copies all the
         ! params, includes, etc.
        endif
        if(str.eq."-op") then
          call getarg(i+1,outtop)
          ! output topology file with updated molecule counts
        endif
        if(str.eq."-ci") then
          call getarg(i+1,molfile)
          ! molecule(s) to add
          ! if more that one molecule is in this file, the relative
          ! positions of these molecules will not be changed when they
          ! are added
          ! I would like to eventually give the option to independently
          ! move the new molecules
        endif
        if(str.eq."-dl") then
          call getarg(i+1,dlist)
          ! string or file of molecule residues that can be deleted
          ! string should be a comma-seperated list without spaces
          ! file should have one name per line
        endif
        if(str.eq."-n") then
         call getarg(i+1,str)
         read(str,'(i90)') tries
         ! number of new things to add
         ! if there are M molecules in the ci file, M*tries new
         ! molecules will be added because i cba to count your molecules
         ! for you
        endif
        if(str.eq."-g") then
        ! group
        ! can be group name or group number in index file
        !  -group number counting starts at 0 (i.e. -g 0 will get the
        !   first group in the index file)
        !  -group name should not include spaces or the brackets around
        !   the name that appear in the index file
        !  -the program will try to place the new molecule on top of an
        !   atom in this group
        ! can be interface group_1 group_2 axis
        !  -the program will try to place the new molecule on top of an
        !   atom that is between the centers of molecules named RES1 and
        !   RES2 along the specified axis
        !  -specifying a group that is not in the system will result in
        !   the bottom of the box being used as one of the bounds
        !  -specifying ceil will result in the top of the box being used
        !   as one of the bounds
        !  -cannot do group names for this, but it I would like to add
        !   that feature at some point
        !  -axis should be x, y, or z
        ! can be grid
        !  -the program will generate a grid of points inside the system
        !   box and try to place a new molecule at one of these points
          call getarg(i+1,agroup)
          if(agroup.eq."interface") then
            found=.True.
            call getarg(i+2,g1)
            call getarg(i+3,g2)
            if(i+4.le.iargc()) call getarg(i+4,axis)
          endif
          if(agroup.eq.'grid') then
            grid=.True.
          endif 
        endif
        if(str.eq."-ndx") then
          call getarg(i+1,ndxfile)
         ! index file
         ! can be useful for custom groups
        endif
        if(str.eq."-pbc") then
          call getarg(i+1,str)
          if(str.eq.'no') then
            wrap=.False.
          else
            wrap=.True.
          endif
        endif
        if(str.eq.'-mute') then
          mute=.True.
          ! supress most output to terminal that isn't an error message
        endif
        if(str.eq."-h") then
          help=.True.
          ! print help page to terminal
          exit
        endif
      enddo

      open(1,file="/dev/stdout")
! unit 1 is the default out

      if(help) then
      ! help page
        str=adjustl("")
        write(1,'(a)') trim(str)
        str=adjustr("catnip is used to put residues from one &
          &file into another")
        write(1,'(a)') trim(str)
        str=adjustl("")
        write(1,'(a)') trim(str)
        str=adjustl("-c   : input system coordinate file (gro/pdb) to &
          &which molecules are added")
        write(1,'(a)') trim(str)
        str=adjustl("-oc  : [ optional | catnip.gro ] output system &
          &coordinate file (gro/pdb)")
        write(1,'(a)') trim(str)
        str=adjustl("-p   : input topology file (.top) which &
          &corresponds to the system coordinate file")
        write(1,'(a)') trim(str)
        str=adjustl("-op  : [ optional | catnip.top ] output topology &
          &file (.top)")
        write(1,'(a)') trim(str)
        str=adjustl("-ci  : input molecule file (gro/pdb) which is put &
          &in the system file")
        write(1,'(a)') trim(str)
        str=adjustl("-dl  : [ optional ] comma-separated list or file &
          &containing removable residues (e.g. LIG,DRG)")
        write(1,'(a)') trim(str)
        str=adjustl("-n   : [ optional | prompted ] number of new &
          &molecules to put into the system")
        write(1,'(a)') trim(str)
        str=adjustl("-g   : [ optional | prompted ] group name, number,&
          & or 'interface group_1 (or ceil) group_2 x||y||z' or 'grid'")
        write(1,'(a)') trim(str)
        str=adjustl("-ndx : [ optional | index.file ] specify index &
          &file")
        write(1,'(a)') trim(str)
        str=adjustl("-pbc : (yes/no) wrap periodic boundary, if one is &
        &definined in the system coordinate file")
        write(1,'(a)') str
        str=adjustl("-mute: suppress output")
        write(1,'(a)') trim(str)
        str=adjustl("-h   : help")
        write(1,'(a)') trim(str)
        goto 97
      endif 
! useful info ^
! gets printed if the user needs help on how to use the program
      
      pie=dacos(dble(-1.))
! i like pie

      do while(tries.lt.0) 
        write(1,*) "How many to add?"
        read(5,*) tries
      enddo
! if not given on the command line, it will ask how many to add

      inquire(file=dlist,exist=ential)
! get it? existential...
      if(ential) then
! if it was given a file for the deletable molecules list, read the file
! and add the stuff to the list of removable molecule names
        n=0
        open(12,file=dlist)
        do
          read(12,*,iostat=ios) str
          if(ios.ne.0) exit
          n=n+1
        enddo

        allocate(delgroups(n))

        rewind(12)

        do i=1,n
          read(12,*) delgroups(i)
        enddo

        close(12)
! read file for removable molecules
! should have one molecule per line
      else

        n=1
        do k=1,90
          if(dlist(k:k).eq.",") then
            n=n+1
          endif
        enddo
! split list on commas  
        allocate(delgroups(n))

        n=1
        l=1
        do i=1,90
          if(dlist(i:i).eq.",") then
          !add the string between this comma and the last comma to the
          !list
            delgroups(n)=dlist(l:i-1)
            n=n+1
            l=i+1
          endif
        enddo

        delgroups(n)=dlist(l:90)
      ! catch the last thing in the list
      endif
! read commandline delete stuff stuff

      outbox=adjustr(outbox)
      fo=outbox(87:90)
      !format of output file is assumed by the file extension
      ! should be .pdb or .gro
      outbox=adjustl(outbox)
     
      if(fo.ne.'.gro'.and.fo.ne.'.pdb') then
        write(1,*) "output coordinates should be in PDB or &
        &Gromacs format"
        write(1,*) "please use the proper file extension (pdb/gro)"
        brexit=1
        ! yell at the user for not requesting a pdb or gro output file
        goto 97
      endif

      if(.not.mute)&
            write(1,*) "removable molecules:", &
            (trim(delgroups(k))//' ',k=1,n)
      ! tell the user what molecule types are deletable so they can make
      ! sure they are using catnip correctly

      inquire(file=ndxfile,exist=ential)
      !check to see if index file exists

      if(.not.ential.and..not.grid) then
      ! if the index file does not exist, check to see if index.ndx does
      ! index.ndx is the default output of gmx make_ndx
        inquire(file="index.ndx",exist=ential)
        if(ential.and.ndxfile.eq."-1") then
        !if index.ndx does exist and the user has not specified an
        !index file, delete index.ndx and generate a new one
          if(.not.mute)&
            write(1,*) "removing old index file"
          lugicul=system("rm index.ndx")
! want to remove old indexes because if the program is run multiple
! times in a row, the old index might not correspond to the current
! system
          endif
          if(.not.mute)&
            write(1,*)"generating index file"
          str="echo q | gmx -quiet make_ndx -f "//trim(boxfile)//"&
            &&>/dev/null"
          lugicul=system(str)
          ! generate new index file
          ndxfile="index.ndx"
      else
      ! use the specified index file
        if(.not.mute)&
            write(1,*) "using pre-existing index file"
      endif
      
      inquire(file=boxfile,exist=ential)
      if(.not.ential) then
       if(boxfile.ne.'a1') write(1,*) 'tried to open ',trim(boxfile),&
       ', but it did not exist'
       write(1,*) "box file not properly specified, use -h for help"
       write(1,*) "-c is the proper flag for the box file"
       ! yell at the user for trying to use a file that does not exist
       brexit=1
       goto 97
      endif
! goto 97 ends the program

      boxfile=adjustr(boxfile)
      fi=boxfile(87:90)
      ! input file format is assumed based on the file extension
      ! should be .gro or .pdb
      boxfile=adjustl(boxfile)

      if(.not.grid) then
        if(.not.mute)&
            write(1,*) "reading index file..."

        open(6,file=ndxfile)

! I've put an index reader thing in a subroutine in gromacs-things.f90,
! but I don't want to replace this with it because I don't want to break
! things

37      if(verify(trim(agroup),'0123456789').ne.0.and.agroup.ne."-1") then
          agroup='[ '//trim(agroup)//' ]'
        else
          read(agroup,'(i90)') igroup
        endif
        
        groups=0

! check if the user gave a number or a name for the group, and search
! according to their input
        l=0
        do
          read(6,'(a)',iostat=ios) str
          if(ios.ne.0) exit
          str=adjustl(str)
          if(str(1:1).eq.'[') then
            if(groups.eq.igroup.or.str.eq.agroup) then
              ! check if either the group name or group number match
              ! where we're at in the index file
              found=.True.
              do
              ! if it does, read the index file by splitting index
              ! file's lines on whitespace
              ! this ensures that the array of atoms in the group is the
              ! same size as the group 
              ! it's probably not necessary, but heck it's fun
                read(6,'(a)',iostat=ios) str
                str=adjustl(str)
                if(ios.ne.0.or.str(1:1).eq.'[') exit
                n=90
                str=' '//str
                do k=89,1,-1
                  if(str(k:k).eq.' '.and.str(k+1:k+1).ne.' ') then
                  !this column is blank, but the one to its right is not
                  !that means it's cutting time
                    read(str(k:n),'(i6)') m
                    n=k
                    if(allocated(groupies)) then
                      intnum=size(groupies)
                      allocate(tempint(intnum+1))
                      do o=1,intnum
                        tempint(o)=groupies(o)
                      enddo
                      tempint(intnum+1)=m
                      deallocate(groupies)
                      call move_alloc(tempint,groupies)
                    else
                      allocate(groupies(1))
                      groupies(1)=m
                    endif
                    ! as shown by the various prime number programs,
                    ! this dynamic allocation method is slower than
                    ! static arrays
                    ! it might also be faster to allocate this array all
                    ! at once, but that has not been tested
                    ! this really doesn't take too much time, I just
                    ! wanted to talk about prime numbers
! append each number to the list
! random item in this list will be chosen later
                  endif
                enddo
              enddo
            else
              if(agroup.eq.'-1'.and.igroup.eq.-1) then
                if(.not.mute)&
                  write(1,*) groups,trim(str)
! write groups if the group hasn't been changed from the initial
              endif
              groups=groups+1
! count the groups
            endif
          endif
        enddo
! read index file and members of the group specified

        if(.not.allocated(groupies).and.agroup.ne."-1") then
          if(.not.mute)&
            write(1,*) trim(agroup)," not found in index"
        endif
! say if the selected group wasn't found
        if(.not.found) then
          write(1,*) "Which group?"
! ask for the group if it is not found in index (or specified at the
! start)
          read(*,*) agroup
          if(agroup.eq.'grid') then
            grid=.True.
            found=.True.
          endif
          if(agroup.eq."interface") then
! ask the user about the groups and axes if the user would like to use
! an interfacial group
            write(1,*) "interface group 1:"
            read(*,*) g1
            write(1,*) "interface group 2:"
            read(*,*) g2
            write(1,*) "axis: (x, y, or z)"
            read(*,*) axis
            agroup="[ interface ]"
            found=.True.
          else
            groups=0
            rewind(6)
            goto 37
! go back and read index
          endif
        endif
! ask about group is not specified or not in index

        close(6)

      endif

      open(7,file=boxfile)
      
      if(fi.eq.'.gro') then
        read(7,'(a)') str
        read(7,*) boxatoms
        ! read number of atoms currently in system
      elseif(fi.eq.'.pdb') then
        boxatoms=0
        do
          read(7,'(a)',iostat=ios) str
          if(ios.ne.0) exit
          if(str(1:4).eq.'ATOM'.or.str(1:6).eq.'HETATM') then
            boxatoms=boxatoms+1
          ! use the number of lines starting with ATOM or HETATM as the
          ! number of atoms if we're dealing with a pdb file because not
          ! all pdb files will have a MASTER entry
          endif
        enddo
        rewind(7)
      else
        write(1,*) "input coordinate files should have a pdb or gro &
        &extension"
        write(1,*) "this file does not: ",trim(boxfile)
        brexit=1
        ! yell at the user for not using a pdb or gro file
        goto 97
      endif
      
      inquire(file=molfile,exist=ential)
      if(.not.ential) then
        if (molfile.ne.'a1') write(1,*) 'tried to open ',trim(molfile),&
        ', but it did not exist'
        write(1,*) "input file not properly specified, use -h for help"
        write(1,*) "-ci is the proper flag to specify the input file"
        brexit=1
        goto 97
      endif

      close(7)

      molfile=adjustr(molfile)
      fn=molfile(87:90)
      ! assume the format of the file for the new molecule(s) based on
      ! the extension, which should be .pdb or .gro
      molfile=adjustl(molfile)

      open(8,file=molfile)
      
      if(fn.eq.'.gro') then
        read(8,*) str
        read(8,*) newatoms
      elseif(fn.eq.'.pdb') then
        newatoms=0
        do
          read(8,'(a)',iostat=ios) str
          if(ios.ne.0) exit
          if(str(1:4).eq.'ATOM'.or.str(1:6).eq.'HETATM') then
            newatoms=newatoms+1
          endif
        enddo
        rewind(8)
      else
        write(1,*) "input coordinate files should have a gro or pdb &
          &extension"
        write(1,*) "this file did not: ",trim(molfile)
        brexit=1
        goto 97
      endif

      alloc=boxatoms+tries*newatoms
      
      allocate(molnum(alloc),molres(alloc),atres(alloc),atnum(alloc),&
       xyz(alloc,3),catdel(alloc),s_chain(alloc))

      catdel(:)=-1
! catdel is the list of deleted molecules
      
      s_chain='A'

      open(7,file=boxfile)

      if(fi.eq.'.gro') then
        read(7,*) str
        read(7,*) str
        do i=1,boxatoms
          read(7,99) molnum(i),molres(i),atres(i),atnum(i),&
            (xyz(i,j),j=1,3)
! read box file for atoms and box size
        enddo
      read(7,*) box
      elseif(fi.eq.'.pdb') then
        box=(/0.,0.,0./)
        i=0
        do
          read(7,'(a)',iostat=ios) str
          if(ios.ne.0) exit
          if(str(1:4).eq.'ATOM'.or.str(1:6).eq.'HETATM') then
            i=i+1
            read(str,96) atnum(i),atres(i),molres(i),s_chain(i),&
              molnum(i),(xyz(i,j),j=1,3)
            do j=1,3
              xyz(i,j)=xyz(i,j)/10.
            enddo
          endif
        if(str(1:6).eq.'CRYST1') then
          backspace(7)
          read(7,'(a6,3(f9.3))') str,box
          ! this really only works with a rectangular box
          box=box/10.
        endif
        enddo
      else
        write(1,*) "input files should have a .gro or .pdb extension"
        goto 97
      endif
      close(7)

      if(any(box.le.0.)) then
      ! we don't know how big the box is, so we guess based on the
      ! largest and smallest coordinates
      ! not all pdb files have a CRYST1 entry
        boxguessed=.True.
        box(1)=1.01*(maxval(xyz(:,1))-minval(xyz(:,1)))
        box(2)=1.01*(maxval(xyz(:,2))-minval(xyz(:,2)))
        box(3)=1.01*(maxval(xyz(:,3))-minval(xyz(:,3)))
      endif

      if(grid) then
        if(.not.mute)&
            write(1,*) 'generating grid...'
        o=1
        do i=1,100
          do j=1,100
            do k=1,100
              gridbox(o,:)=(/box(1)*real(i)/100.,box(2)*real(j)/100.,&
                  box(3)*real(k)/100./)
              o=o+1
            enddo
          enddo
        enddo
      endif
! create a grid of points, 100 along each axis, across the entire box

      if(verify(axis,'xyz').ne.0) then
        write(1,*) "enter axis orthogonal to interface (x, y, or z)"
        read(*,*) axis
! ask about the axis if it isn't x, y, or z
      endif

      if(agroup.eq."[ interface ]".and..not.grid) then
        if(.not.mute)&
            write(1,*) "determining interface..."
        if(axis.eq."x") lim=1
        if(axis.eq."y") lim=2
        if(axis.eq."z") lim=3
    
        c1=0.
        c2=0.
        com1=0.
        com2=0.

        call readndx(ndxfile,g1,group1)
        ! read atoms in group from index file
        call readndx(ndxfile,g2,group2)
        ! ditto

        if(size(group1).gt.0) then
          do i=1,size(group1)
            com1=com1+xyz(group1(i),lim)
          enddo
          com1=com1/real(size(group1))
        else
          com1=0.0
        endif
        ! com is ther average position of atoms in the group along the
        ! specified axis

        if(size(group2).gt.0) then
          do i=1,size(group2)
            com2=com2+xyz(group2(i),lim)
          enddo
          com2=com2/real(size(group2))
        else
          com2=0.0
        endif
        ! ditto

        if(g1.eq.'ceil') com1=box(lim)
        if(g2.eq.'ceil') com2=box(lim)
        ! use the box size if ceil (ceiling) is given
        top=max(com1,com2)
        bot=min(com1,com2)
        ! find the upper and lower bounds
  
        if(com1.eq.com2) then
          write(1,'(a)') 'The specified groups ('//trim(g1)//', '//&
            trim(g2)//&
            ") have the same center. This probably won't work well."
        endif
  
        do i=1,boxatoms
          if(xyz(i,lim).lt.top.and.xyz(i,lim).gt.bot) then
            ! add atoms between the bounds to the group
            if(allocated(groupies)) then
              intnum=size(groupies)
              allocate(tempint(intnum+1))
              do o=1,intnum
                tempint(o)=groupies(o)
              enddo
              tempint(intnum+1)=atnum(i)
              deallocate(groupies)
              call move_alloc(tempint,groupies)
            else
              allocate(groupies(1))
              groupies(1)=atnum(i)
           endif
          endif
        enddo
      endif
      
      if(size(groupies).eq.0.and..not.grid) then
        write(1,*) 'No interface could be found'
        goto 97
      endif
! find interface if needed
! 'interface' is the region between the middles of the selected groups
! ceil is the top of the box
  
      allocate(nmolnum(newatoms),nmolres(newatoms),natres(newatoms),& 
        natnum(newatoms),nxyz(newatoms,3),tryxyz(newatoms,3),&
        n_chain(newatoms))

      n_chain='A'

      cx=0.
      cy=0.
      cz=0.

      if(fn.eq.'.gro') then
        do l=1,newatoms
          read(8,99) nmolnum(l),nmolres(l),natres(l),natnum(l), &
            (nxyz(l,j),j=1,3)
          cx=cx+nxyz(l,1)/real(newatoms)
          cy=cy+nxyz(l,2)/real(newatoms)
          cz=cz+nxyz(l,3)/real(newatoms)
! read molecules to add and find its center
        enddo
      else
        l=0
        do
          read(8,'(a)',iostat=ios) str
          if(ios.ne.0) exit
          if(str(1:4).eq.'ATOM'.or.str(1:6).eq.'HETATM') then
            l=l+1
            read(str,96) natnum(l),natres(l),nmolres(l),n_chain(l),&
              nmolnum(l),(nxyz(l,j),j=1,3)
              do j=1,3
                nxyz(l,j)=nxyz(l,j)/10.
              enddo
            cx=cx+nxyz(l,1)/real(newatoms)
            cy=cy+nxyz(l,2)/real(newatoms)
            cz=cz+nxyz(l,3)/real(newatoms)
          endif
        enddo
      endif

      close(8)

      do l=1,newatoms
        nxyz(l,1)=nxyz(l,1)-cx
        nxyz(l,2)=nxyz(l,2)-cy
        nxyz(l,3)=nxyz(l,3)-cz
      enddo
! move new molecules to the center(ish)
      
      moldel=0
      deleted=0

      limit=nint(10*log(real(newatoms))+50)
      ! number of attempts to move the new molecule
      ! use a log scale because the moving stuff takes a while, and it
      ! takes a long while longer if there's a lot more atoms

      do k=1,tries

        call date_and_time(VALUES=ilist)

        call srand(ilist(8)+ilist(7)+deleted+k)
! use current time to be more randumb

        randumb=rand()
        randumb=rand()
! the 1st randumb number is randumb, always low

        if(grid) then
          rngesus=floor(1000000*randumb)
          if(.not.mute)&
            write(1,'(a,2x,i5,2x,a,3(2x,f6.3))') 'place new molecule',k&
                  ,'around',gridbox(rngesus,:)
          do l=1,newatoms
            tryxyz(l,:)=nxyz(l,:)+gridbox(rngesus,:)
            ! try putting the new molecule at the place we randomly
            ! picked
          enddo
        else
          rngesus=groupies(floor((size(groupies)*randumb)+1.))
! pick the chosen one

          if(.not.mute)&
            write(1,*) "placing new molecule",k,"by",rngesus

          do l=1,newatoms
            tryxyz(l,:)=nxyz(l,:)+xyz(rngesus,:)
            ! try putting the new molecule on top of the atom we
            ! randomly picked
          enddo
          gridbox(rngesus,:)=xyz(rngesus,:)
          ! use gridbox to store this point to avoid more if-statements
          ! later
        endif

        attempt=0

        if(.not.mute)&
            write(1,*) "adjusting orientation..."
        
        i=0
        outer: do
          i=i+1
          if(i.gt.boxatoms) exit
          if(.not.any(delgroups.eq.molres(i))) then
            l=0
            do 
              l=l+1
              if(l.gt.newatoms) exit
              call sqdist(xyz(i,:),tryxyz(l,:),box,dist) 
              if(dist.lt.0.04) then
! if the molecules that are being added are too close to something that
! isn't allowed to be removed, it will be translated or rotated to try
! to get them farther apart
                call sqdist(xyz(i,:),gridbox(rngesus,:),box,a)
                call sqdist(tryxyz(l,:),gridbox(rngesus,:),box,b)
                call sqdist(xyz(i,:),tryxyz(l,:),box,c)
  
                if(a.gt.1..and.b.gt.1.) then
                  psi=acos(sqrt((c-a-b)/(-2.*a*b)))
                else
                  psi=0.
                endif
 
                if(c.le.0.1.or.psi.eq.0..or.newatoms.eq.1) then
! if they are less than 0.3 apart, translate. rotate otherwise.
                  do o=1,3
                    if(wrap) then
                      if(abs(xyz(i,o)-tryxyz(l,o)).gt.abs(box(o)-&
                        abs(xyz(i,o)-tryxyz(l,o)))) then
                        if(xyz(i,o).gt.tryxyz(l,o)) then
                          dxyz(o)=box(o)-abs(xyz(i,o)-tryxyz(l,o))
                        else
                          dxyz(o)=(tryxyz(l,o)-xyz(i,o))-box(o)
                        endif
                      else
                        dxyz(o)=tryxyz(l,o)-xyz(i,o)
                      endif
                    else
                      dxyz(o)=tryxyz(l,o)-xyz(i,o)
                   endif 
                    ! get a vector pointing from the atom we can't delete to the atom in the new molecule
                  enddo
                  nb=0
                  do j=1,3
                    nb=nb+dxyz(j)**2
                  enddo
                  nb=sqrt(nb)
                  if(nb.gt.0.0001) then
! can sometimes get floating point error here that will throw the new
! molecule very far away from the box if i only check if nb != 0.0
                    do j=1,3
                     dxyz(j)=dxyz(j)/(nb)
                    enddo
                    ! normalize the vector
                  else
                    dxyz=(/1.0,0.0,0.0/)
                    ! if the atoms are literally on top of each other,
                    ! just move the molecule along the x-axis
                    ! this prevents dividing by 0ish numbers
                  endif
          
                  nb=abs(sqrt(0.11)-abs(sqrt(c)))

                  dxyz=nb*dxyz
                  ! change the length of the vector so that the
                  ! translation will move the new molecule just far
                  ! enough

                  do j=1,newatoms
                    tryxyz(j,:)=tryxyz(j,:)+dxyz
! translate it away
                  enddo

                  gridbox(rngesus,:)=gridbox(rngesus,:)+dxyz
                  ! move the center coordinates of the new molecule as well
                  ! this may help the rotations be better
                else
                  a1=tryxyz(l,1)-gridbox(rngesus,1)
                  a2=tryxyz(l,2)-gridbox(rngesus,2)
                  a3=tryxyz(l,3)-gridbox(rngesus,3)
                  ! a vector pointing from the center of the new
                  ! molecule to the atom in the new molecule
                  b1=xyz(i,1)-gridbox(rngesus,1)
                  b2=xyz(i,2)-gridbox(rngesus,2)
                  b3=xyz(i,3)-gridbox(rngesus,3)
                  ! b vector pointing from the center of the new
                  ! molecule to the atom we can't remove
        
                  vx=-(a2*b3)-(a3*b2)
                  vy=(a1*b3)+(a3*b1)
                  vz=-(a1*b2)-(a2*b1)
                  ! cross to get a vector orthogonal to a and b
                  ! we will rotate the molecule about this vector
                  ! negative give the right rotation direction (away
                  ! instead of towards)
                  nb=sqrt(vx**2+vy**2+vz**2)
                  if(nb.gt.0.001) then
                    vx=vx/nb
                    vy=vy/nb
                    vz=vz/nb
                    ! normalize it
                  else
                    vx=1.0
                    vy=0.0
                    vz=0.0
                    ! i don't think we should ever go in this part of
                    ! the if statement, but idk
                  endif
! find unit vector parallel to axis of rotation
! if the atoms are on top of each other, use the x axis

                  if(a.gt.0.001.and.b.gt.0.001) then
                    theta=sqrt((0.21**2-a-b)/(-2.*sqrt(a*b)))
                    ! rotate just enough
                    if(theta.gt.1.) theta=0.88
                    theta=acos(theta)
                  else
                    theta=0.5
                  endif

! rotation angle theta; strange things happen for small a and b
! also, the angle might not be possible for some a and b combinations
! i'll fix that later maybe 
! maybe not
! we'll see

                  R(1,1)=cos(theta)+vx*vx*(1-cos(theta))
                  R(1,2)=vx*vy*(1-cos(theta))-vz*sin(theta)
                  R(1,3)=vx*vz*(1-cos(theta))+vy*sin(theta)
                  R(2,1)=vx*vy*(1-cos(theta))+vz*sin(theta)
                  R(2,2)=cos(theta)+vy*vy*(1-cos(theta))
                  R(2,3)=vy*vz*(1-cos(theta))-vx*sin(theta)
                  R(3,1)=vx*vz*(1-cos(theta))-vy*sin(theta)
                  R(3,2)=vy*vz*(1-cos(theta))+vx*sin(theta)
                  R(3,3)=cos(theta)+vz*vz*(1-cos(theta))
! rotation matrix for rotation about vector v by theta degrees

                  do j=1,newatoms
                    dx=nxyz(j,1)*R(1,1)+nxyz(j,2)*R(1,2)+nxyz(j,3)*&
                      R(1,3)
                    dy=nxyz(j,1)*R(2,1)+nxyz(j,2)*R(2,2)+nxyz(j,3)*&
                      R(2,3)
                    dz=nxyz(j,1)*R(3,1)+nxyz(j,2)*R(3,2)+nxyz(j,3)*&
                      R(3,3)
                    nxyz(j,:)=(/dx,dy,dz/)
                    ! move the atoms based on the rotation 
                  enddo    

                  do j=1,newatoms
                    tryxyz(j,:)=nxyz(j,:)+gridbox(rngesus,:)
! move the new molecule back to the chose spot
                  enddo
                endif
                i=0
                l=0
                attempt=attempt+1
              
                cent=0.
  
                do j=1,newatoms
!find the center of the new molecule
                  do o=1,3
                    cent(o)=cent(o)+tryxyz(j,o)/float(newatoms)
                  enddo
                enddo
          
                if(wrap) then
                  do j=1,newatoms
!move the new molecule back inside the box if the center
!is outside of it
                    do o=1,3
                      if(cent(o).lt.0.) then
                        tryxyz(j,o)=tryxyz(j,o)+float(max(1,1-&
                          int(tryxyz(j,o)/box(o))))*box(o)
                          ! use max because an atom can be inside the box,
                          ! but the center might not be
                      endif
                      if(cent(o).gt.box(o)) then
                        tryxyz(j,o)=tryxyz(j,o)-float(max(1,&
                          int(tryxyz(j,o)/box(o))))*box(o)
                      endif
                    enddo
                  enddo
                endif
              endif
              if(attempt.ge.limit) then
                warn=.true.
                exit outer
              endif
            enddo
          endif
        enddo outer
! move molecules away from ones that are too close, but can't be removed
! it's got x actions (rotations and/or translations) to move the molecule away

        if(.not.mute)&
            write(1,*) "removing molecules..."

        do i=1,boxatoms
          if(any(delgroups.eq.molres(i)).and.&
            .not.any(catdel.eq.molnum(i))) then
! user-specified molecules can be removed if they are too close to the
! new molecules
            do l=1,newatoms
              call sqdist(xyz(i,:),tryxyz(l,:),box,dist)
              if(dist.lt.0.04) then
                ! this is too close, get rid of this molecule
                ! keep in mind, this is squared distance, so ~0.2 nm
                moldel=moldel+1
                ! number of deleted molecules
                catdel(moldel)=molnum(i)
                ! molecule number of this molecule
                deleted=deleted+count(molnum(i).eq.molnum(1:boxatoms))
                ! total number of atoms that have been removed
                exit
              endif
            enddo
          endif
        enddo
! delete molecules that are too close and can be removed

        do l=1,newatoms
          molnum(boxatoms+l)=molnum(boxatoms)+nmolnum(l)
          molres(boxatoms+l)=nmolres(l)
          atres(boxatoms+l)=natres(l)
          xyz(boxatoms+l,:)=tryxyz(l,:)
          s_chain(boxatoms+l)=n_chain(l)
        enddo
! add the new stuff to the list
     
        boxatoms=boxatoms+newatoms         
! update number of atoms in the box (ignoring removed ones for now)
      
      enddo
  
      if(.not.mute)&
            write(1,*) "writing output files..."

      inquire(file=outbox,exist=ential)

      if(ential) then
! make a backup of the output coordinate file if it already exists
! backup will be called #outbox.n#
        boxfile=outbox
        i=0
        do
          i=i+1
          if(i.gt.99) then
            if(.not.mute) &
              write(1,*) "will not keep more than 99 backups"
              exit
            endif
          write(str,'(i2)') i
          str=adjustl(str)
          boxfile="#"//trim(outbox)//"."//trim(str)//"#"
          inquire(file=boxfile,exist=ential)
          if(.not.ential) then
            if(.not.mute) &
              write(1,*) "backing up ",trim(outbox)," to ",&
                trim(boxfile)
            str="echo 'cp "//trim(outbox)//" \"//trim(boxfile)//&
            " ; exit' | bash"
            lugicul=system(str)
            exit
          endif
        enddo
      endif

      open(9,file=outbox)
! fo is the output file format, guesses as the extention of the output
! file's name
      if(fo.eq.'.pdb') then
        box=box*10.
        xyz=xyz*10.
! convert from nm to A
        atres=adjustl(atres)
        molres=adjustl(molres)
      endif
      open(11,file=outtop)
       
      inquire(file=topfile,exist=ential)
      if(ential) then
        if(outtop.eq.topfile) then
! if the input and output topologies are the same, copy to old one to
! #topology.n#
          i=0
          do
            i=i+1
            if(i.gt.99) then
              if(.not.mute) &
                write(1,*) "will not keep more than 99 backups"
                exit
              endif
            write(str,'(i2)') i
            str=adjustl(str)
            topfile="#"//trim(topfile)//"."//trim(str)//"#"
            inquire(file=topfile,exist=ential)
            if(.not.ential) then
              if(.not.mute) &
                write(1,*) "backing up ",trim(outtop)," to ",&
                  trim(topfile)
              str="echo 'cp "//trim(outtop)//" \"//trim(topfile)//&
              " ; exit' | bash"
              lugicul=system(str)

              exit
            else
              topfile=outtop
            endif
          enddo
        endif

        open(10,file=topfile)
            
        do
          read(10,'(a)',iostat=ios) str
          write(11,'(a)') trim(str)
          str=adjustl(str)
          if(str.eq."[ molecules ]".or.ios.ne.0) exit
        enddo

! copy everything from the old topology up until [ molecules ]

        close(10)
      else
        write(11,'(a)') '[ molecules ]'
      endif
      
      if(fo.eq.'.gro') then
        write(9,'(a)') "catnip conffile"
        write(str,'(i99)') boxatoms-deleted
        str=adjustl(str)
        write(9,'(a)') str
      endif

      if(fo.eq.'.pdb') then
        write(9,'(a)') "REMARK catnip conffile"
        write(9,'(a,3(f9.3),3(f7.2),2x,a10,i4)') &
            'CRYST1',box,90.,90.,90.,'P 1',1
      endif

      if(any(catdel.eq.molnum(1))) then
        nextmol=0
      else
        nextmol=1
      endif
      nextatom=0
      molsofthat=0

      do i=1,boxatoms
        if(.not.boxguessed.and.wrap) then
          !put atoms back inside the box if they are outside the box,
          !but only if we know how big the box actually is
          do j=1,3
            if(xyz(i,j).lt.0.) then
              xyz(i,j)=xyz(i,j)+float(max(1,1-int(xyz(i,j)/&
                       box(j))))*box(j)
            endif
            if(xyz(i,j).gt.box(j)) then
              xyz(i,j)=xyz(i,j)-float(max(1,int(xyz(i,j)/&
                       box(j))))*box(j)
            endif
          enddo
        endif
        if(.not.any(catdel.eq.molnum(i))) then
          nextatom=nextatom+1
          if(i.gt.1) then 
            if(molnum(i).ne.molnum(i-1).or.s_chain(i).ne.s_chain(i-1))&
              then
              nextmol=nextmol+1
              molsofthat=molsofthat+1
! renumber atoms and molecules so they are consecutive
              if(fo.eq.'.pdb') then
                write(9,'(a3,3x,i5,6x,a4)') 'TER',nextmol-1,molres(i-1)
              endif
            endif
          endif
! count the new number of things new number of things to topology
! write new box file
          if(fo.eq.'.gro') then
! write gromacs file format
            write(9,99) & 
              nextmol,molres(i),atres(i),mod(nextatom,100000),&
                (xyz(i,j),j=1,3)
! gromacs formatting starts the atom numbers over if the number goes
! over 99,999
          elseif(fo.eq.'.pdb') then
! write pdb file
            write(9,98) "HETATM",nextatom,atres(i),molres(i),s_chain(i)&
            ,nextmol,(xyz(i,j),j=1,3),1.00,0.00 
          endif
        endif
        if(molres(i).ne.molres(i-1).and.i.gt.1) then
          write(11,'(a4,2x,i5)') molres(i-1),molsofthat
          molsofthat=0
        endif
      enddo
      if(.not.any(catdel.eq.molnum(boxatoms)).and.nextmol.gt.1) &
        molsofthat=molsofthat+1
      ! the molecule counting above would miss the last molecule, so we
      ! catch it here
      write(11,'(a4,2x,i5)') molres(size(molres)),molsofthat
      if(fo.eq.'.gro') write(9,'(3(2x,f8.5))') box
      if(fo.eq.'.pdb') then
        write(9,'(a3,3x,i5,6x,a4)') 'TER',nextmol,molres(i-1)
        write(9,'(a)') "END"
      endif
! write all atoms to the new files

      close(9)
      close(11)

97    call cpu_time(whattimeisit)
! 97 is the go-to for exiting the program early for errors and stuff

      if(allocated(groupies)) deallocate(groupies)
      if(allocated(molnum)) deallocate(molnum,molres,atres,atnum,xyz,&
        &catdel)
      if(allocated(nmolnum)) deallocate(nmolnum,nmolres,natres,natnum,&
        nxyz,tryxyz)
      if(allocated(delgroups)) deallocate(delgroups)
      if(allocated(group1)) deallocate(group1,group2)
! make sure things are deallocated, even if things are messed up
      
      do i=6,12
        inquire(i,opened=ential)
        if(ential) close(i)
      enddo

      if(.not.mute) then
        if(warn)&
          write(1,*) 'check output files; ',&
            'some molecules may have extremely close contacts'
      endif

      if(.not.mute) &
            write(1,'(a4,1x,f10.4,a1)') 'took',whattimeisit,'s'

96    format(6x,i5,2x,a4,a4,a1,i4,4x,3(1x,f7.3))
98    format(a6,i5,2x,a4,a4,a1,i4,4x,3(1x,f7.3),2x,f4.2,1x,f4.2)
99    format(i5,a4,2x,a4,i5,3(2x,f6.3))

      close(1)

      if(brexit.ne.0) then
        stop 1
      ! somebody messed up
      else
        stop 0
      ! exit gracefully
      endif

      end program catnip

      subroutine sqdist(xyz1,xyz2,box,dist2)
! squared distance between atoms
      implicit none

      real, intent(in):: xyz1(3),xyz2(3),box(3)
      real, intent(out):: dist2
      real:: r(3), x1(3), x2(3)
      integer:: i

      do i=1,3
        if(xyz2(i).lt.0.) then
          x2(i)=xyz2(i)+float(max(1,1-int(xyz2(i)/box(i))))*box(i)
        endif
        if(xyz2(i).gt.box(i)) then
          x2(i)=xyz2(i)-float(max(1,int(xyz2(i)/box(i))))*box(i)
        else
          x2(i)=xyz2(i)
        endif
        if(xyz1(i).lt.0.) then
          x1(i)=xyz1(i)+float(max(1,1-int(xyz1(i)/box(i))))*box(i)
        endif
        if(xyz1(i).gt.box(i)) then
          x1(i)=xyz1(i)-float(max(1,int(xyz1(i)/box(i))))*box(i)
        else
          x1(i)=xyz1(i)
        endif
        ! move the atoms to the position inside the actual box
        if(abs(x1(i)-x2(i)).lt.abs(box(i)-abs(x1(i)-x2(i)))) &
          then
          r(i)=x1(i)-x2(i)
        else
          r(i)=box(i)-abs(x1(i)-x2(i))
        endif
        r(i)=r(i)**2
        ! find the squared distance between the closest images of these atoms
      enddo

      dist2=sum(r)

      end subroutine sqdist
