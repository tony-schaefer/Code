      program cl_process
      
      !ifort dirutil.f gromacs-things.f90 wit-process.f90 -Ofast -o wit-process.e -ldl -lstdc++ libxtc.a 
      !gfortran gro-dirutil.f gromacs-things.f90 wit-process.f90 -Ofast -o wit-process.e -ldl -lstdc++ libxtc.a
      !!! might need to change system functions to calls for gfortran
      ! this has not been tested with gfortran
     
      ! this is intented to be run in the same directory as the thing
      ! was pulled

      use IFPORT
      !comment out if not using ifort
      use bromacs
      !bromacs is a module that contains a subroutine that can read the
      !atoms numbers of a given group in an index file
      !it is in gromacs-things

      implicit none

      integer:: trashint,atnum,clspec,i,j,iwin,natoms,handle(4),status&
      ,k,m, ios
      integer,allocatable:: molnum(:), group1(:), group2(:)
      character::trashstr*15,windir*13,aclspec*5,equfile*99,str*90,d*99&
      ,filename*99, ag1*90, ag2*90, ndx*90
      character,allocatable:: molres(:)*4,atres(:)*4,files(:)*99
      real:: axyz(3),dist,old,box(6),com1, com2
      real,allocatable:: xyz(:)
      logical:: direx,filex,ential
      
      d='.'

      open(15,file='index.ndx')

      i=0

      do
        read(15,'(a)',iostat=ios) str
        if(ios.ne.0) exit
        str=adjustl(str)
        if(str(1:1).eq.'[') then
          print*, i, trim(str)
          i=i+1
        endif
      enddo
      ! print each group in the index file

      close(15)

      ndx='index.ndx'

      print*,'group 1:'
      read(*,'(a)') ag1
      call readndx(ndx,ag1,group1)
      !reads the index file and returns a list of the atom numbers in
      !the ag2 group

      print*, 'group 2'
      read(*,*) ag2
      call readndx(ndx,ag2,group2)

      d='.'

      call numfiles(i,d)
      !dirutils call to get the number of files in the current directory
      allocate(files(i))

      call listdir(files,d,i)
      !dirutils call to get the list of files in the current directory

      do k=1,i
        files(k)=adjustr(files(k))
        print*, files(k)(97:99)
        if(files(k)(97:99).eq.'gro') filename=adjustl(files(k))
        ! take whatever gro file is found in the current directory
      enddo

      print*, 'using',trim(filename)

      open(6,file=filename)
      ! open this gro file

      read(6,'(a)') trashstr
      read(6,*) natoms
      ! use this gro file to approximate the location of the center of
      ! mass of the water layer
      allocate(molnum(natoms),molres(natoms),atres(natoms),&
        xyz(3*natoms))
      do i=1,natoms
        read(6,99) molnum(i),molres(i),atres(i),atnum,(axyz(j),j=1,3)
      enddo

      ag1=molres(group1(1))
      ag2=molres(group2(1))
      !make sure ag1 and ag2 are set to the molecule residue
      ! i don't think i refernce these after this point, so this is
      ! probably useless

      trashint=1
      do 
      write(trashstr,'(i2)') trashint
      trashstr=adjustl(trashstr)
      inquire(directory='../'//trim(trashstr)//'-windows',exist=direx)
      !check to see if n-windows exists; n=1,2,3,5,6,7...
      ! can only get to 99
      ! rip #4 & #15
      if(.not.direx) then
        windir='../'//trim(trashstr)//'-windows'
        print*, 'mkdir '//trim(windir)
        trashint=system('mkdir '//trim(windir))
        exit
      endif
      trashint=trashint+1
      if(trashint.eq.4) trashint=trashint+1
      if(trashint.eq.15) trashint=trashint+1
      enddo
      !make new n-windows directory

      call f77_molfile_init
      str='traj_comp.xtc'
      call f77_molfile_open_read(handle(1),natoms,str,'xtc')
      
      dist=1.5
      !space windows 0.15 nm (1.5 A) apart (xtc file is angstroms)
      iwin=1
      ! window number
      k=-1

!warning: wall of strings follows

      do
        k=k+1
        status = 1 ! status=1 on entry means read
        call f77_molfile_read_next(handle(1),natoms,xyz(1),box,status)
        !read xtc file
        
        com1=0.0
        do i=1, size(group1)
          com1=com1+xyz(3*group1(i))
        enddo
        com1=com1/real(size(group1))
      !find com of group 1

        com2=0.0
        do i=1, size(group2)
          com2=com2+xyz(3*group2(i))
        enddo
        com2=com2/real(size(group2))
      !find com of group 2

        if(status.eq.0) goto 97
        if(k.eq.0) old=abs(com1-com2)
        if(abs(abs(com1-com2)-old).gt.dist) then
          print*,'making window',iwin, abs(com1-com2)
          old=abs(com1-com2) 
          ! new old position of thing
          write(trashstr,'(i2)') iwin
          trashstr=adjustl(trashstr)
          trashstr='window-'//trim(trashstr)
          trashstr=adjustl(trashstr)
          ! name of new window subdirectory
          print*,'mkdir '//trim(windir)//'/'//trim(trashstr)
          trashint=system('mkdir '//trim(windir)//'/'//&
          &trim(trashstr))
          !make new directory in ../n-windows 
          print*, 'writing',trim(windir)//'/'//&
            trim(trashstr)//'/'//'input.gro'
          open(7,file=trim(windir)//'/'//&
            trim(trashstr)//'/'//'input.gro')
          write(7,'(a,i5)') 'Input t=',k
          ! put the frame number in the header
          write(7,'(i5)') natoms
          do i=1,natoms
            write(7,99) &
            molnum(i),molres(i),atres(i),i,(xyz(3*i-j)/10.,j=2,0,-1)
          enddo
          write(7,'(3(2x,f8.5))') box(1:3)/10.
          close(7)
          !write input coordinate file
          print*, &
            'cp index.ndx '//trim(windir)//'/'//trim(trashstr)//'/'
          print*, 'cp /home/scha0275/backups/nvt-*.mdp '&
          //trim(windir)//'/'//trim(trashstr)//'/'
          print*, 'cp ../pull/pull-'//trim(aclspec)//&
          '/*.top '//trim(windir)//'/'//trim(trashstr)//'/'
          trashint=system('cp index.ndx '//trim(windir)//&
            '/'//trim(trashstr)//'/')
          trashint=system('cp /home/scha0275/backups/nvt-*.mdp '&
          //trim(windir)//'/'//trim(trashstr)//'/')
          trashint=system('cp *.top '//trim(windir)//&
            '/'//trim(trashstr)//'/')
          ! copy necessary stuff to new window directory
          iwin=iwin+1
        endif
        if(iwin.gt.99) exit
      enddo
      
      deallocate(group1,group2)
97    if(allocated(molnum)) deallocate(molnum,molres,atres,xyz)
      
      call f77_molfile_finish
      
99    format(i5,a4,2x,a4,i5,3(2x,f6.3))
      
      end
