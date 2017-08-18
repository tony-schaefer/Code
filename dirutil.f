      subroutine listdir(stuff,d,i)
      !given a directory, d, and a number, i, return a list containing i
      !files in d
      use IFPORT
      ! there exists a dirutil for gfortran called gfo-dirutil.f90
      ! gfortran uses system as a subroutine as opposed to a function
      ! it also doesn't expand the ~
      implicit none

      integer,intent(in):: i
      character,intent(in):: d*99
      character*99,intent(out):: stuff(i)
      integer:: j,ios,sz,maxsz,filenum
      character:: a,s*500
     
      s=trim("echo 'a=`ls ")//" "//trim(d)//trim("` ; for thing in ${a} 
     &;do echo $thing >> ~/.list ; done ; exit ' | bash")
      ! use system call to put the files in the directory in a file in
      ! the users home directory - one filename per line
      j=system(s) 

      open(1157,file='~/.list')
      ! the .list file is put in the home directory to make sure the
      ! user will have read/write permissions for it, no matter where
      ! they are

      maxsz=0
      filenum=1

      ! read in each file name (or directory name)
      do j=1,i
        read(1157,'(a)') stuff(j)
        ! read the name of the jth file
        inquire(file=stuff(j), size=sz)
        if(sz.gt.maxsz) then
          filenum=j
          maxsz=sz
          ! file size doesn't really matter anymore, but i wanted it for
          ! something
        endif
      enddo

      close(1157)

      j=system("echo ' rm ~/.list ' | bash ")
      !remove the list of file names

      return
      
      end subroutine listdir
      
      subroutine numfiles(i,d)
      ! given a directory, d, returns the number of files or
      ! subdirectories in d
      ! this should be run before the  listdir subroutine so an
      ! appropriately-sized array can be given to listdir
      use IFPORT
      implicit none

      integer,intent(out):: i
      integer:: j,ios
      character:: a,s*500
      character,intent(in):: d*99
      
      ! use system call to write a list of file names
      s=trim("echo 'a=`ls ")//" "//trim(d)//trim("` ; for thing in ${a} 
     &;do echo $thing >> ~/.list ; done ; exit ' | bash")
      j=system(s)

      open(1157,file='~/.list')
      i=0
      do
        ! read how many things are in the lsit of files
        read(1157,'(a)',iostat=ios) a
        if(ios.ne.0) exit
        i=i+1
      enddo

      close(1157)
      
      ! remove the list of files
      j=system("echo ' rm ~/.list ' | bash ")

      return

      end subroutine numfiles
