      program cl_process
      
      !ifort dirutil.f -Tf cl-process.f95 -free -Ofast -o cl-process.e -ldl -lstdc++ libxtc.a 
      !gfortran gro-dirutil.f cl-process.f95 -Ofast -o cl-process.e -ldl -lstdc++ libxtc.a
      !!! might need to change system functions to calls for gfortran
      
      !dirutil is a custom thing that can give you a list of files in a
      !directory

      !this is intented to be run the directory above the thing that was
      !pulled

      use IFPORT
      !comment out if not using ifort
      implicit none

      integer:: trashint,atnum,clspec,i,j,iwin,natoms,handle(4),status&
      ,k,wats,m,ms,sz
      integer,allocatable:: molnum(:)
      character::trashstr*15,windir*13,aclspec*5,equfile*99,str*90,d*99&
      ,filename*99
      character,allocatable:: molres(:)*4,atres(:)*4,files(:)*99
      real:: axyz(3),dist,old,box(6),com
      real,allocatable:: xyz(:)
      logical:: direx,filex,ential
      
      d='../pull/'

      call numfiles(i,d)
      !dirutils call to get the number of files in ../pull
      allocate(files(i))

      call listdir(files,d,i)
      !dirutils call to get the list of files in '../pull'
      m=1
      ms=-1

      do k=1,i
        filename='../pull/'//trim(files(k))//'/confout.gro'
        inquire(file=filename, exist=ential)
        ! check to see if ../pull/pull-n/confout.gro exists
        if(ential) then
          print*, trim(filename)
        endif
      enddo

      deallocate(files)

      print*,'What CL?'
      read(*,*) clspec
      write(aclspec,'(i5)') clspec
      aclspec=adjustl(aclspec)
      !ask what cl to process

      filename='../pull/pull-'//trim(aclspec)//'/confout.gro'

      open(6,file=filename)
      ! open this gro file

      wats=0.
      com=0.
      read(6,'(a)') trashstr
      read(6,*) natoms
      ! use this gro file to approximate the location of the center of
      ! mass of the water layer
      allocate(molnum(natoms),molres(natoms),atres(natoms),&
        xyz(3*natoms))
      do i=1,natoms
        read(6,99) molnum(i),molres(i),atres(i),atnum,(axyz(j),j=1,3)
        trashstr=adjustl(molres(i))
        if(trashstr.eq.'SOL') then
          wats=wats+1
          com=com+axyz(3)
        endif
      enddo
      com=10.0*com/real(wats)
      ! convert to A from nm
      !read molecule names and stuff, print if there's a CL

      trashint=1
      do 
      write(trashstr,'(i2)') trashint
      trashstr=adjustl(trashstr)
      inquire(directory='../'//trim(trashstr)//'-windows',exist=direx)
      !check to see if n-windows exists; n=1,2,3,5,6,7...
      ! can only get to 99
      ! rip #4
      if(.not.direx) then
        windir='../'//trim(trashstr)//'-windows'
        print*, 'mkdir '//trim(windir)
        trashint=system('mkdir '//trim(windir))
        exit
      endif
      trashint=trashint+1
      enddo
      !make new n-windows directory

      !print*, 'cp ../pull/pull-'//aclspec//'/Run.tpr .'
      ! i don't need no tpr files no more cuz i read right from the xtc
      ! file
      print*, 'cp ../pull/pull-'//aclspec//'/traj_comp.xtc .'
      trashint=system('cp ../pull/pull-'//aclspec//'/traj_comp.xtc .')
      !copy stuff here from the pull directory
      
      call f77_molfile_init
      str='../pull/pull-'//aclspec//'/traj_comp.xtc'
      call f77_molfile_open_read(handle(1),natoms,str,'xtc')
      
      dist=2.15
      !space windows 0.215 nm (2.15 A) apart (xtc file is angstroms)
      iwin=1
      ! window number
      k=-1

!warning: wall of strings follows

      do
        k=k+1
        status = 1 ! status=1 on entry means read
        call f77_molfile_read_next(handle(1),natoms,xyz(1),box,status)
        !read xtc file
        if(status.eq.0) goto 97
        if(k.eq.0) old=abs(xyz(3*clspec)-2.0*dist)
        if(old.gt.(com+65.0)) then
          cycle
        endif
        if(old.lt.abs(com-65.0)) then
          cycle
          ! cycle if the chloride is more than 6.5 nm from the
          ! water com
        endif
        if(abs(xyz(3*clspec)-old).gt.dist) then
        ! if the cl has moved far enough ...
          if(abs(xyz(3*clspec)-old).gt.(box(3)/2.)) then
            exit
          !moving across half the box in just a few steps indicates
          !crossing pbc
          endif
          ! if the cl crosses the periodic boundary, exit
          ! WHAM has trouble with data that crosses the boundary
          ! anyways, don't need it
          print*,'making window',iwin,xyz(3*clspec)
          old=xyz(3*clspec) 
          ! new old position of chloride
          write(trashstr,'(i2,a1,i5)') iwin,'-',clspec
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
          ! put the fram number in the header
          write(7,'(i5)') natoms
          do i=1,natoms
            write(7,99) &
            molnum(i),molres(i),atres(i),i,(xyz(3*i-j)/10.,j=2,0,-1)
          enddo
          write(7,'(3(2x,f8.5))') box(1:3)/10.
          close(7)
          !write input coordinate file
          print*, 'cp ../pull/pull-'//trim(aclspec)//&
          '/index.ndx '//trim(windir)//'/'//trim(trashstr)//'/'
          print*, 'cp /home/scha0275/backups/nvt-*.mdp '&
          //trim(windir)//'/'//trim(trashstr)//'/'
          print*, 'cp ../pull/pull-'//trim(aclspec)//&
          '/*.top '//trim(windir)//'/'//trim(trashstr)//'/'
          trashint=system('cp ../pull/pull-'//trim(aclspec)//&
          '/index.ndx '//trim(windir)//'/'//trim(trashstr)//'/')
          trashint=system('cp /home/scha0275/backups/nvt-equil.mdp '&
          //trim(windir)//'/'//trim(trashstr)//'/')
          trashint=system('cp ../pull/pull-'//trim(aclspec)//&
          '/*.top '//trim(windir)//'/'//trim(trashstr)//'/')
          ! copy necessary stuff to new window directory
          iwin=iwin+1
        endif
        if(iwin.gt.99) exit
      enddo
      
97    if(allocated(molnum)) deallocate(molnum,molres,atres,xyz)
      
      call f77_molfile_finish
      
99    format(i5,a4,2x,a4,i5,3(2x,f6.3))
      
      end
