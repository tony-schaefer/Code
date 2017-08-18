      module bromacs 

      contains
        subroutine readgro(phil,frame,natoms,molnum,molres,atres,atnum,&
          xyz,box,worked)
        
        !          readgro(file name, frame #, number of atoms, list of
        !          molecule #, list of molecule names, list of atom
        !          names, list of atom #, coordinate vector, box
        !          dimensions, whether or not the file was read
        !          successfully)

        implicit none

        character*90, intent(in):: phil
        integer, intent(in):: frame
        character*4, allocatable, intent(out):: molres(:), atres(:)
        integer, allocatable, intent(out):: molnum(:), atnum(:)
        integer, intent(out):: natoms
        real, allocatable, intent(out):: xyz(:,:)
        real, intent(out):: box(3)
        logical, intent(out):: worked
        integer:: i, j, ios
        character*90:: header

        worked=.True.

        open(6,file=phil)

        read(6,*,iostat=ios) header ! read header
        if(ios.ne.0) then
          worked=.False.
          return
        endif
        read(6,*,iostat=ios) natoms ! read number of atoms
        if(ios.ne.0) then
          worked=.False.
          return
        endif

        if(.not.allocated(molres)) then
          allocate(molnum(natoms),molres(natoms),atres(natoms),&
          atnum(natoms),xyz(natoms,3)) ! allocate output things
        endif

        rewind(6)

        do i=1,(frame*(natoms+3))
          read(6,*,iostat=ios) header
          if(ios.ne.0) then
            worked=.False.
            return
          endif
        enddo
        ! get to the right starting place

        read(6,*,iostat=ios) header
        if(ios.ne.0) then
          worked=.False.
          return
        endif
        read(6,*,iostat=ios) natoms
        if(ios.ne.0) then
          worked=.False.
          return
        endif

        do i=1,natoms
          read(6,99,iostat=ios) & ! read atom info
            molnum(i),molres(i),atres(i),atnum(i),(xyz(i,j),j=1,3)
          if(ios.ne.0) then
            worked=.False.
            return
          endif
        enddo

        read(6,*,iostat=ios) box ! rad box size
        if(ios.ne.0) then
          worked=.False.
          return
        endif

        close(6)

99      format(i5,a4,2x,a4,i5,3(2x,f6.3))

        return

        end subroutine readgro

        subroutine readndx(phil,group_name,list)
!                  readndx(file name, group name or number, output list
!                  of atom numbers)
        implicit none

        character*90, intent(in):: group_name, phil
        integer, allocatable, intent(out):: list(:)
        character*150:: str, agroup*99
        integer:: ios, igroup, groups, i, k , m, n, o, intnum
        integer, allocatable:: tempint(:)

        igroup=-1

        open(6,file=phil)
        !phil is the index file name

        agroup=group_name

        if(verify(trim(group_name),'0123456789').ne.0) then
          agroup='[ '//trim(group_name)//' ]'
        else
          read(group_name,'(i90)') igroup
        endif
        
        groups=0

! check if the user gave a number or a name for the group, and search
! according to their input
        do
          read(6,'(a)',iostat=ios) str
          if(ios.ne.0) exit
          str=adjustl(str)
          if(str(1:1).eq.'[') then
            if(groups.eq.igroup.or.str.eq.agroup) then
              ! check if either the group name or group number match
              ! where we're at in the index file
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
                    read(str(k:n),'(i6)') m
                    n=k
                    if(allocated(list)) then
                      intnum=size(list)
                      allocate(tempint(intnum+1))
                      do o=1,intnum
                        tempint(o)=list(o)
                      enddo
                      tempint(intnum+1)=m
                      deallocate(list)
                      call move_alloc(tempint,list)
                    else
                      allocate(list(1))
                      list(1)=m
                    endif
! append each number to the list
! random item in this list will be chosen later
                  endif
                enddo
              enddo
            else
! write groups if the group hasn't been changed from the initial
              groups=groups+1
! count the groups
            endif
          endif
        enddo
! read index file and members of the group specified

        if(.not.allocated(list)) then
            write(1,*) trim(group_name)," not found in index"
        endif

        close(6)

        return

        end subroutine readndx
      end module bromacs
