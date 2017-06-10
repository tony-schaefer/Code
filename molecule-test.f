module class_molecule
  implicit none
  double precision:: pi=acos(-1.),e=exp(1.)

  type, public:: molecule
    integer:: natoms
    double precision,dimension(:),allocatable:: x,y,z
    real,dimension(:),allocatable:: mass
    integer,dimension(:),allocatable:: element
  contains
 
    procedure :: com => molecule_center_of_mass

  end type molecule

contains 
  subroutine molecule_center_of_mass(mol,com)
     
  implicit none
  
  class(molecule),intent(in):: mol
  real,dimension(3),intent(out):: com
  integer:: i,j
 
  com=0.
  do i=1,mol%natoms
    com(1)=com(1)+mol%mass(i)*mol%x(i)
    com(2)=com(2)+mol%mass(i)*mol%y(i)
    com(3)=com(3)+mol%mass(i)*mol%z(i)
  enddo
  com=com/sum(mol%mass(:))

  end subroutine molecule_center_of_mass

end module class_molecule

program dosomething
  use class_molecule
  implicit none

  integer:: i,j,k,natoms
  integer,allocatable:: molnum(:),atnum(:),ele(:)
  double precision,allocatable:: xyz(:,:)
  real,allocatable:: mass(:)
  real,dimension(3):: point,box
  character:: str,molres,atres*4,sysfile*90
  type(molecule),allocatable:: mol(:)

  call getarg(1,sysfile)

  open(7,file=sysfile)
  read(7,*) str
  read(7,*) natoms
  
  allocate(xyz(natoms,3),mass(natoms),molnum(natoms),atnum(natoms),ele(natoms))
 
  mass=1.008
  ele=1

  do i=1,natoms
    read(7,99) molnum(i),molres,atres,atnum(i),(xyz(i,j),j=1,3)
    atres=adjustl(atres)
    if(atres(1:1).eq.'C') then
      mass(i)=12.011
      ele(i)=6
    endif
    if(atres(1:1).eq.'N') then
      mass(i)=14.0067
      ele(i)=7
    endif
    if(atres(1:3).eq.'NA ') then
      mass(i)=22.99
      ele(i)=1
    endif
    if(atres(1:1).eq.'O') then
      mass(i)=15.999
      ele(i)=8
    endif
    if(atres(1:3).eq.'CL ') then
      mass(i)=35.453
      ele(i)=17
    endif
  enddo

  read(7,*) (box(j),j=1,3)

  allocate(mol(molnum(natoms)))

  open(8,file='test.gro')
  write(8,'(a)') "test"
  write(8,*) molnum(natoms)

  k=0
  do i=1,natoms
    if(i.gt.1) then
      if(molnum(i-1).ne.molnum(i)) then
        mol(molnum(i-1))=molecule(k,xyz(i-k:i-1,1),xyz(i-k:i-1,2),xyz(i-k:i-1,3),mass(i-k:i-1),ele(i-k:i-1))
        call mol(molnum(i-1))%com(point)
        write(8,99) molnum(i-1),'D','D',molnum(i-1),point
        k=0
      endif
    endif
    if(i.eq.natoms) then
      if(molnum(i).eq.molnum(i-1)) then
        k=k+1
        mol(molnum(i-1))=molecule(k,xyz(i-k+1:i,1),xyz(i-k+1:i,2),xyz(i-k+1:i,3),mass(i-k+1:i),ele(i-k+1:i))
        call mol(molnum(i))%com(point)
        write(8,99) molnum(i),'D','D',molnum(i),point
      endif
    endif
    k=k+1
  enddo

  write(8,*) box


  deallocate(xyz,mass,mol,molnum,atnum,ele)
  close(7)
  close(8)

99    format(i5,a4,2x,a4,i5,3(2x,f6.3))

end program dosomething
