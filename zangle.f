      program H_O_z_angles

      double precision aa,ab,ac,ba,bb,bc,aangle,bangle,dz,haowx
     +,haowy,haowz,hbowx,hbowy,hbowz,avg
      character adummy*150,header*150
      character*4,allocatable:: atsym(:)
      double precision,allocatable::bin(:),z(:),y(:),x(:),xa(:)
     + ,ran(:)
      integer iatoms,ires,nbins,itime,clock,wata,gradN,b,inter,charform
     +,f
      integer,allocatable:: protons(:),prob(:,:),ta(:)
      real rati,dN
      
      open(7,file='zangle.dat')
!      open(18,file='probdist_HOz.dat')
!      open(19,file='interface_HOz.dat')
      open(9,file='input.dat')   
      read(9,*) nbins     
      read(9,*) itime
      read(9,*) gradN
      pi = 2*acos(0.) 
      allocate(bin(0:nbins),xa(nbins),
     + protons(nbins),ran(0:gradN)
     + ,prob(nbins,gradN),ta(nbins))
      wata = 0
      k = 0
      clock = 0
      dN = real(pi/gradN)
      ran(0) = 0.
      do b=1,gradN             !do 1
        ran(b) = ran(b-1) + dN
      enddo                    !enddo 1
      do j=1,nbins             !do 2
       protons(j) = 0
       xa(j) = 0.
       ta(j) = 0
       do b=1,gradN           !do 3
        prob(j,b) = 0
       enddo                  !enddo 3
      enddo                   !enddo 4
      read(5,'(a150)') header
      do f=1,150
      if(header(f:f+1).eq.'t=') charform = f+1
      enddo
c how we read what timestep we are at depends on how long the first line is
      rewind(5)   
      do                      !do 5
      k=k+1 
      read(5,'(a,f8.2)',iostat=iostatus) adummy(1:charform),timestep
      if(iostatus.ne.0) exit
c do things until the end of the document
      if(mod(timestep,100.).eq.0) write(*,*) timestep
      read(5,*) iatoms
      if(k.eq.1) then !if 1
      allocate(z(iatoms),y(iatoms),x(iatoms),atsym(iatoms))
      endif           !endif 1
      do i=1,iatoms           !do 6
        read(5,99) ires, ares, atsym(i), iatom,x(i),y(i),z(i)
        atsym(i) = adjustl(atsym(i))
        if(atsym(i).eq.'OW'.and.k.eq.1) wata = wata + 1
c remember the type and coordinates of all atoms
        enddo                 !enddo 6
      read(5,*,iostat=iostatus) xbox, ybox, zbox
      if(timestep.lt.itime) cycle
      clock = clock + 1
c start calculations at the desired initial time
      dz = zbox/real(nbins) 
c finding the height of each bin
      bin(0) = 0.
      do j=1,nbins           !do 7
       bin(j)=bin(j-1)+dz 
      enddo                  !enddo 7
      do j=1,nbins           !do 8
       do i=1,iatoms         !do 9
c account for periodic boundry conditions
        if(atsym(i).eq.'HW1'.and.((z(i).ge.bin(j-1).and.z(i).lt.bin(j))
     +    .or.(bin(j).eq.bin(1).and.z(i).lt.0.).or.
     +    (bin(j).eq.bin(nbins).and.z(i).ge.zbox))) then !if 25
           if(abs(x(i-1)-x(i)).gt.abs(xbox-abs(x(i-1)-x(i)))) then !if 3
            if(x(i-1).gt.x(i)) then !if 4
             haowx = xbox-abs(x(i-1) - x(i))
            else
             haowx = (x(i)-x(i-1))-xbox
            endif                   !endif 4
           else
            haowx = x(i) - x(i-1)
           endif                    !endif 3
           if(abs(y(i-1)-y(i)).gt.abs(ybox-abs(y(i-1)-y(i)))) then !if 5
            if(y(i-1).gt.y(i)) then !if 6
             haowy = ybox-(abs(y(i-1) - y(i)))
            else
             haowy = (y(i)-y(i-1))-ybox
            endif                   !endif 6
           else
            haowy = (y(i) - y(i-1))
           endif                    !endif 5
           if(abs(z(i-1)-z(i)).gt.abs(zbox-abs(z(i-1)-z(i)))) then !if 7
            if(z(i-1).gt.z(i)) then !if 8
             haowz = zbox-(abs(z(i-1) - z(i)))
            else
             haowz = ((z(i)-z(i-1)))-zbox
            endif                   !endif 8
           else
             haowz = (z(i) - z(i-1))
           endif                    !endif 7
          aa = sqrt(haowx**2+haowy**2+haowz**2)
          ab = haowz
          protons(j) = protons(j) + 1 
          aangle = acos(ab/aa)
          xa(j) = xa(j) + aangle
        endif !endif 25
        if(atsym(i).eq.'HW2'.and.((z(i).ge.bin(j-1).and.z(i).lt.bin(j))
     +    .or.(bin(j).eq.bin(1).and.z(i).lt.0.).or.
     +    (bin(j).eq.bin(nbins).and.z(i).ge.zbox))) then !if 24
           if(abs(x(i-2)-x(i)).gt.abs(xbox-abs(x(i-2)-x(i)))) then !if 9
            if(x(i-2).gt.x(i)) then !if 10
             hbowx = xbox-(abs(x(i-2) - x(i)))
            else
             hbowx = (x(i)-x(i-2))-xbox
            endif                   !endif 10
           else
            hbowx = (x(i) - x(i-2))
           endif                    !endif 9
           if(abs(y(i-2)-y(i)).gt.abs(ybox-abs(y(i-2)-y(i)))) then!if 11
            if(y(i-2).gt.y(i)) then !if 12
             hbowy = ybox-abs((y(i-2) - y(i)))
            else
             hbowy = (y(i)-y(i-2))-ybox
            endif                   !endif 12
           else
            hbowy = (y(i) - y(i-2))
           endif                    !endif 11
           if(abs(z(i-2)-z(i)).gt.abs(zbox-abs(z(i-2)-z(i)))) then!if 13
            if(z(i-2).gt.z(i)) then !if 14
             hbowz = zbox-(abs(z(i-2) - z(i)))
            else
             hbowz = ((z(i)-z(i-2)))-zbox
            endif                   !endif 14
           else
            hbowz = (z(i) - z(i-2))
           endif                 !endif 13
          ba = sqrt(hbowx**2+hbowy**2+hbowz**2)
          bb = hbowz
          protons(j) = protons(j) + 1 
          bangle = acos(bb/ba)
          xa(j) = xa(j) + bangle
          do b=1,gradN    !do 10
           if(aangle.ge.ran(b-1).and.aangle.lt.ran(b))then !if 15
           prob(j,b) = prob(j,b) + 1
           exit
           endif !endif 15
           if(bangle.ge.ran(b-1).and.bangle.lt.ran(b))then !if 16
           prob(j,b) = prob(j,b) + 1
           exit
           endif !endif 16
          enddo           !enddo 10
        endif !endif 24
        enddo             !enddo 9
      enddo               !enddo 8
      enddo               !enddo 7
c fancy shmancy xmgace formatting to get labels
      write(7,*) '@    title "H-O-z axis angle"'
      write(7,*) '@    xaxis label "z position (nm)"'
      write(7,*) '@    yaxis label "average angle (rad)"'
      write(7,*) '@TYPE xy'
      do j=1,nbins        !do 13
      avg = (xa(j))/(real(protons(j)))
      rati = real(nbins*(protons(j)))/real(clock*wata)
      if(rati.gt.0.06)write(7,*) bin(j)-dz/2.,avg 
      enddo               !enddo 13
!      write(18,*) '@    title "H-O-z axis Angle Probability"'
!      write(18,*) '@    xaxis label "angle (rad)"'
!      write(18,*) '@    yaxis label "probability"'
!      write(18,*) '@TYPE xy'
!       do b=1,gradN       !do 11
!       write(18,98)ran(b)-dN/2.,
!     +(real(prob(j,b))/real(wata*clock),j=1,nbins)
!       enddo              !enddo 11
!      open(63,file='interface.dat')
!      iostatus=0
!      do                  !do 12
!      read(63,*,iostat=iostatus) inter
!      if(iostatus.ne.0) exit
!      avg = (xa(inter))/(2.*real(protons(inter)))
!      write(19,*) bin(inter)-dz/2.,avg
!      enddo               !enddo 12
          
      
      deallocate(z,bin,x,y,atsym,xa,protons,ran,prob,ta)
      close(9)
      close(6)
      close(7)
!      close(18)
!      close(19)
!      close(63)
98    format(200(f18.16,2x))
99    format(i5,a4,2x,a4,i5,2x,f6.3,2x,f6.3,2x,f6.3)
 
      end 
