      program twod_density

!     ifort -Ofast -o 2DDensity.e 2DDensity.f -ldl -lstdc++ libxtc.a 

      integer itime,ftime,charform,natom,clspec,anum,mnum,status,
     +handle(4)
      real dxy,maxi(3),box(6)
      real,allocatable:: x(:),y(:),z(:),xyz(:)
      double precision cxy,cz
      double precision,allocatable:: zbin(:),xybin(:), color(:,:,:),
     +vol(:)
      character agroup*60,str*80,infile*300,outfile*80,header*300,ares*4
     +,ndxfile*300,framefile*300,group*60,formut*3,hex*7,catres(100)*4
     +,mol(3)*4,os(20)*4
      character,allocatable:: mres(:)*4

      outfile="2ddensities.dat"
      dxy=0.1
      itime=0
      ftime=-1
      ndxfile="index.ndx"
      group="CL_spec"
      clspec=-1
      catres='-1'
      os='-1'
      catres(1)='ACH'
      catres(2)='AMM'
      catres(3)='ATTS'
      catres(4)='BTEA'
      catres(5)='TMA'
      catres(6)='TEA'
      catres(7)='TPRO'
      catres(8)='TBA'
      catres(9)='TPEN'
      catres(10)='THEX'
      catres(11)='THEP'
      catres(12)='TOA'
      catres(13)='TONE'
      catres(14)='Na'
      catres(15)='Q10'
      catres(16)='DMDP'
      catres(17)='DADM'
      catres(18)='DMDO'
      catres(19)='TMO'
      catres(20)='Q10'
      catres(21)='BTBA'
      os(1)='CHX'
      os(2)='CHCL'
      os(3)='CCL2'
      os(4)='ETAC'
      mol(1)='N/A'
      mol(2)='N/A'
      mol(3)='N/A'
c set defaults

      if(iargc().eq.0) then
       print*,'use -f to specify filename'
       print*,'use -h for help'
       goto 97
      endif

      do i=1,iargc()

       call getarg(i,str)

       if(str.eq."-dxy") then
        call getarg(i+1,str)
        read(str,'(f80.70)') dxy
       endif

       if(str.eq."-ndx") then
        call getarg(i+1,ndxfile)
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
       endif

       if(str.eq."-h") then
        print*," "
        print*,"finds densities of residues in .gro trajectories"
        print*," -dxy   : (0.1) max histogram size"
        print*," -itime : (0) initial time"
        print*," -ftime : (-1) final time"
        print*," -f     : input .gro/.xtc trajectory; use -s with xtc"
        print*," -s     : input .gro frame"
        print*," -ndx   : (index.ndx) index file with CL_spec group"
        print*," -o     : (2ddensities.dat) output densities"
        print*," -g     : (CL_spec)  group name of single atom in index 
     +file"
        goto 97
       endif

       if(str.eq."-s") then
        call getarg(i+1,framefile)
       endif

       if(str.eq."-g") then
        call getarg(i+1,group)
       endif

       if(str.eq."-f") then
        call getarg(i+1,infile)
        infile=adjustr(infile)
        formut=trim(infile(298:300))
        infile=adjustl(infile)
       endif
      
      enddo
c parse commandline 

      group="[ "//trim(group)//" ]"
    
      open(9,file=ndxfile)
      
      do
       read(9,'(a)',iostat=iostatus) agroup
       if(iostatus.ne.0) then
        print*,trim(group),' not found in index'
        exit
       endif
       if(agroup.eq.group) then
        read(9,*) clspec
        exit
       endif
      enddo
c find clspec

      close(9)
      if(clspec.eq.-1) goto 97

      if(formut.eq.'xtc') then
       open(11,file=framefile)
       read(11,*) header
       read(11,*) natom
       allocate(xyz(3*natom),mres(natom))
       do i=1,natom
        read(11,96) mres(i)
        mres(i)=adjustl(mres(i))
       enddo
       call f77_molfile_init
       call f77_molfile_open_read(handle(1),natom,infile,'xtc')
      endif
c read residues for xtc because xtc doesn't contain residues

      if(formut.eq.'gro') then
       open(8,file=infile)
       read(8,'(a300)') header
       do i=1,299
        if(header(i:i+1).eq.'t=') then
         charform=i+1
         exit
        endif
       enddo
       rewind(8)
      endif
c figure out how to read the timestep for gro files

      clock=0.
      k=0
      timestep=0.

c start doing things
      do
       k=k+1
       if(formut.eq.'gro') then
        read(8,'(a,f10.5)',iostat=ios) header(1:charform),timestep
c read timestep
        if(ios.ne.0) exit
        read(8,*) natom
       endif
       if(k.eq.1) then
        allocate(x(natom),y(natom),z(natom))
        if(formut.ne.'xtc') allocate(mres(natom))
       endif
       do i=1,natom
        if(formut.eq.'gro') then
          read(8,99) mnum,mres(i),ares,anum,x(i),y(i),z(i)
         elseif(formut.eq.'xtc') then
          if(i.eq.1) then
           timestep=timestep+1.
c timestep is simply interated for xtc
           status = 1 ! status=1 on entry means read
           call f77_molfile_read_next(
     +         handle(1),natom,xyz(1),box,status)
c read stuff about each frame
           if(status.eq.0) goto 95
c continue when done
          endif
          x(i)=xyz(i*3-2)
          y(i)=xyz(i*3-1)
          z(i)=xyz(i*3)
c put xyz into x, y, and z
        endif
c read atoms and stuff
       enddo

       if(formut.eq.'gro') read(8,*) (box(j),j=1,3)
c read box size

       if(timestep.ge.ftime.and.ftime.ge.0) exit
       if(timestep.lt.itime) cycle
c quitting time

       clock=clock+1
       
       if(clock.eq.1) then
c find bin sizes 
        sols=0.
        do i=1,natom
         if(mres(i).eq.'SOL'.or.mres(i).eq.'WAT') then
          souls=souls+z(i)
          sols=sols+1.
         endif
        enddo

       comz=souls/sols
c crude way to define water CoM

        xy=sqrt(box(1)**2+box(2)**2)/1.
        
        n=1
        do
         n=n+1
         if((xy/real(n)).le.dxy) then
          nxybins=n
          exit
         endif
        enddo
 
        m=1
        do
         m=m+1
         if((box(3)/real(m)).le.dxy) then
          nzbins=m
          exit
         endif
        enddo

        allocate(zbin(0:nzbins),xybin(0:nxybins),
     +   color(nzbins,nxybins,3),vol(nxybins))

        cxy=xy/real(nxybins)
        cz=box(3)/real(nzbins)
        pie=acos(-1.)

        xybin(0)=0.
        pie=acos(-1.)
        do n=1,nxybins
         xybin(n)=xybin(n-1)+cxy
         vol(n)=(xybin(n)**2-xybin(n-1)**2)*pie*cz
         do m=1,nzbins
          do j=1,3
           color(m,n,j)=0.
          enddo
         enddo
        enddo

       
        zbin(0)=0.
        do n=1,nzbins
         zbin(n)=zbin(n-1)+cz
        enddo
       

       endif

       do i=1,natom
        dx=abs(x(i)-x(clspec))
        dx2=box(1)-abs(x(i)-x(clspec))
        if(dx2.lt.dx) dx=dx2
        dy=abs(y(i)-y(clspec))
        dy2=box(2)-abs(y(i)-y(clspec))
        if(dy2.lt.dy) dy=dy2
        dz=z(i)

        rxy=sqrt(dx**2+dy**2)

        if(rxy.gt.xy) cycle

        do n=1,nxybins
         if(rxy.ge.xybin(n-1).and.rxy.lt.xybin(n)) then
          do m=1,nzbins
           if(dz.ge.zbin(m-1).and.dz.lt.zbin(m)) then
            if(mres(i).eq.'SOL'.or.mres(i).eq.'WAT') then
c water is blue, ammonium is green
             color(m,n,1)=color(m,n,1)+dble(1)
             color(m,n,2)=color(m,n,2)+dble(1)
             color(m,n,3)=color(m,n,3)+dble(0)
             if(mol(3).eq.'N/A') mol(3)=mres(i)
             exit
            endif
            do l=1,20
              if(mres(i).eq.os(l)) then
               color(m,n,1)=color(m,n,1)+dble(0)
               color(m,n,2)=color(m,n,2)+dble(1)
               color(m,n,3)=color(m,n,3)+dble(1)
               if(mol(1).eq.'N/A') mol(1)=mres(i)
               exit
              endif
            enddo
            if(any(mres(i).eq.catres)) then
             color(m,n,1)=color(m,n,1)+dble(1)
             color(m,n,2)=color(m,n,2)+dble(0)
             color(m,n,3)=color(m,n,3)+dble(1)
             if(mol(2).eq.'N/A') mol(2)=mres(i)
            endif
            exit
           endif
          enddo
          exit
         endif
        enddo
       enddo
      enddo

95    if(formut.eq.'gro') close(8)

      do j=1,3
       maxi(j)=0.
      enddo

      do m=1,nzbins
       do n=2,nxybins
        do j=1,3
         color(m,n,j)=(color(m,n,j)/(clock*vol(n)))
         if(color(m,n,j).gt.maxi(j)) then
          maxi(j)=color(m,n,j)
         endif
        enddo
       enddo
      enddo

      do j=1,3
c adjust the color intensity relative to the max of each
       if(maxi(j).gt.0.) maxi(j)=255./maxi(j)
      enddo

      open(10,file=outfile)
      write(10,*) "hexscatter()"
      write(10,*) "plot.title(r'Density Rel. to Cl$^-$',fontsize=20)"
      if(formut.eq.'xtc') then
       write(10,*) "plot.ylabel(r'$R_{xy}$ ($\AA$)',fontsize=15)"
       write(10,*) "plot.xlabel(r'$\mathrm{\xi}$ ($\AA$)',fontsize=15)"
      elseif(formut.eq.'gro') then
       write(10,*) "plot.ylabel(r'$R_xy$ (nm)')"
       write(10,*) "plot.xlabel(r'$\xi$ (nm)')"
      endif
      write(10,*) "import matplotlib.patches as trusty"
      write(10,*) "r=trusty.Patch(color='red',label='",mol(1),"')"
      write(10,*) "g=trusty.Patch(color='green',label='",mol(2),"')"
      write(10,*) "b=trusty.Patch(color='blue',label='",mol(3),"')"
      write(10,*) "plot.legend(handles=[r,g,b])"
c      write(10,*) "plot.legend(handles=[g,b])"
      write(10,*) "plot.scatter(x,y,c=color,edgecolors=color)"
c fancy-shmancy rtw formatting stuff      
      do m=1,nzbins
       do n=2,nxybins
        do j=1,3
         color(m,n,j)=255.-1.0*maxi(j)*color(m,n,j)
         if(color(m,n,j).lt.0.) color(m,n,j)=dble(0)
         if(color(m,n,j).gt.255.) color(m,n,j)=dble(255)
        enddo
        if(any(color(m,n,:).lt.0).or.any(color(m,n,:).gt.255)) then
          print*, 'hi'
        endif
        call rgbhex(color(m,n,1),color(m,n,2),color(m,n,3),hex)
        write(10,*) comz-zbin(m)-cz/2.,xybin(n)-cxy/2.,hex
c write stuff
       enddo
      enddo
      close(10)

      if(formut.eq.'xtc') call f77_molfile_finish

      deallocate(mres,x,y,z,zbin,xybin,color,vol)
      if(formut.eq.'xtc') deallocate(xyz)

97    i=1

96    format(5x,a4)
98    format(2(f15.10,2x),3(i3))
99    format(i5,a4,2x,a4,i5,3(2x,f6.3))

      end

      subroutine rgbhex(r,g,b,hexagon)

      double precision r,g,b
      character base16(0:15)*1,hexagon*7

      base16(0)='0'
      base16(1)='1'
      base16(2)='2'
      base16(3)='3'
      base16(4)='4'
      base16(5)='5'
      base16(6)='6'
      base16(7)='7'
      base16(8)='8'
      base16(9)='9'
      base16(10)='A'
      base16(11)='B'
      base16(12)='C'
      base16(13)='D'
      base16(14)='E'
      base16(15)='F'

      hexagon='#'//base16(floor(mod((r/16),16.)))//base16(floor(
     +mod(r,16.)))//base16(floor(mod((g/16),16.)))//base16(floor(
     +mod(g,16.)))//base16(floor(mod((b/16),16.)))//base16(floor(
     +mod(b,16.)))

      return

      end subroutine
