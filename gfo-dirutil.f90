      subroutine listdir(stuff,d,i)
      !given a directory, d, and a number, i, return a list containing i
      !files in d
      !gfortran doesn't expand ~ in the open function, so i made a
      !version that uses the gen_environment_variable call
      implicit none

      integer,intent(in):: i
      character,intent(in):: d*99
      character*99,intent(out):: stuff(i)
      integer:: j,ios,sz,maxsz,filenum
      character:: a,s*500, home*40
     
      s=trim("echo 'a=`ls ")//" "//trim(d)//trim("` ; for thing in ${a};do echo $thing >> ~/.list ; done ; exit ' | bash")
      ! use system call to put the files in the directory in a file in
      ! the users home directory - one filename per line
      j=system(s) 

      call get_environment_variable("HOME",home)

      open(1157,file=trim(home)//'/.list')

      maxsz=0
      filenum=1

      ! read in each file name (or directory name)
      do j=1,i
        read(1157,'(a)') stuff(j)
        inquire(file=stuff(j), size=sz)
        if(sz.gt.maxsz) then
          filenum=j
          maxsz=sz
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
      implicit none

      integer,intent(out):: i
      integer:: j,ios
      character:: a,s*500,home*40
      character,intent(in):: d*99
      
      call get_environment_variable("HOME",home)

      ! use system call to write a list of file names
      s=trim("echo 'a=`ls ")//" "//trim(d)//trim("` ; for thing in ${a}&
     &;do echo $thing >> ~/.list ; done ; exit ' | bash")
      j=system(s)

      open(1157,file=trim(home)//'/.list')
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
