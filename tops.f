      program tetrahedral_order
      
      integer fk,sk,tk,vk,nbins,iatoms,ires,gradN,b,inter,fails,charform
     +,f
      integer,allocatable:: prob(:,:)
      double precision wata
      real,allocatable:: waters(:)
      double precision dz,theta,thetb,thetc,thetd,thete,thetf,d,fm,sm,
     +tm,vm,cab,cac,cad,cbc,cbd,ccd,xd,yd,zd,dfsx,dfsy,dfsz,dftx,dfty,
     +dftz,dstx,dsty,dstz,dsvx,dsvy,dsvz,dtvx,dtvy,dtvz,q,rati
      double precision,allocatable:: x(:),y(:),z(:),bin(:),tq(:),ran(:)
      character adummy*150,header*150
      character,allocatable:: atsym(:)*4,alk(:)*2,ares(:)*4

      open(12,file='tops.dat')
      open(16,file='probdist_tops.dat')
!      open(23,file='interface_top.dat')
      open(9,file='input.dat')
      read(9,*) nbins
c nbins is the number of sections the box is divided in to
      read(9,*) itime
c itime is the time calculations start
      read(9,*) gradN
      allocate(bin(0:nbins),tq(nbins),waters(nbins),ran(0:gradN),
     + prob(nbins,gradN))
      do j=1,nbins
       tq(j) = 0.
       waters(j) = 0.
       do b=1,gradN
        prob(j,b) = 0
c tq is the sum of all q's in each bin
c waters is the number of waters in each bin at all timesteps
      enddo
      enddo
      dN = 1./real(gradN)
      ran(0) = 0.
      do b=1,gradN
       ran(b) = ran(b-1)+dN
      enddo
      h = 0
!      cutoff = 0
      clock = 0
      wata = 0.
c wata eez da numba of watas mon
      fails = 0
      read(5,'(a150)') header
      do f=1,150
      if(header(f:f+1).eq.'t=')charform = f+1
      enddo
      rewind(5)
c how we read what timestep we are at depends on how long the fisrt line
c is
      do
      h=h+1
      read(5,'(a,f8.2)',iostat=iostatus) adummy(1:charform),timestep
      if(iostatus.ne.0) exit
c do stuff until the end of the input *.gro
      if(mod(timestep,100.).eq.0.) write (*,*) timestep
      read(5,*) iatoms
      if(h.eq.1) allocate(x(iatoms),y(iatoms),z(iatoms),atsym(iatoms),
     +alk(iatoms),ares(iatoms))
       
      do i=1,iatoms
       read(5,99) ires,ares(i),atsym(i),iatom,x(i),y(i),z(i)
       atsym(i) = adjustl(atsym(i))
       if(atsym(i).eq.'OW'.and.h.eq.1) wata = wata + 1
       alk(i) = atsym(i)
      enddo

      read(5,*) xbox,ybox,zbox
      if(timestep.lt.itime) cycle
      dz = zbox/real(nbins)
      clock = clock + 1
      bin(0) = 0.
      do j = 1,nbins
       bin(j)=bin(j-1)+dz
      enddo

      do j = 1,nbins
      
       do i = 1,iatoms
       if(atsym(i).eq.'OW'.and.((z(i).lt.bin(j).and.z(i).ge.bin(j-1))
     +   .or.(bin(j).eq.bin(1).and.z(i).lt.0.).or.
     +   (bin(j).eq.bin(nbins).and.z(i).ge.zbox)))then 
       k = 0
       fm = zbox
       sm = zbox
       tm = zbox
       vm = zbox
       fk = 0
       sk = 0
       tk = 0
       vk = 0
c f,s,t, and v are the 1st,2nd,3rd, and 4th closest heavy atoms 
c *k is the number of the closest heavy atoms
c *m is the distance from the closest heavy atoms
        do
        k = k + 1
        if(k.gt.iatoms) exit
c accounting for periodic boundry conditions
        if(atsym(k).eq.'OW'.and.k.ne.i.or.atsym(k).eq.'CL'.or.atsym(k)
     +.eq.'NA'.or.atsym(k).eq.'NAI'.or.alk(k).eq.'CA'.and.ares(k).ne.
     +'CHX'.and.ares(k).ne.'DRG') then
        if(abs(x(i)-x(k)).gt.abs(xbox-abs(x(i)-x(k)))) then
          if(x(i).gt.x(k)) then
          xd=xbox-abs(x(k)-x(i))
          else
          xd=(x(k)-x(i))-xbox
          endif
          else
          xd=abs(x(k)-x(i))
        endif
        if(xd.gt.1.) cycle
        if(abs(y(i)-y(k)).gt.abs(ybox-abs(y(i)-y(k)))) then
          if(y(i).gt.y(k)) then
          yd=ybox-abs(y(k)-y(i))
          else
          yd=(y(k)-y(i))-ybox
          endif
          else
          yd=abs(y(k)-y(i))
        endif
        if(yd.gt.1.) cycle
        if(abs(z(i)-z(k)).gt.abs(zbox-abs(z(i)-z(k)))) then
          if(z(i).gt.z(k)) then
          zd=zbox-abs(z(k)-z(i))
          else
          zd=(z(k)-z(i))-zbox
          endif
          else
          zd=abs(z(k)-z(i))
        endif
        if(zd.gt.1.) cycle
         d=sqrt(xd**2+yd**2+zd**2)
c finding the four closest waters
c things are done in this order because it has to be done in this order
        if(d.lt.vm.and.d.ge.tm) then
c if 4th > distance >= 3rd, change 4th to distance
         vm = d
         vk = k
        endif
        if(d.lt.tm.and.d.ge.sm) then
c if 3rd > distance >= 2nd, change 4th to 3rd and 3rd to distance
         vm = tm
         vk = tk
         tm = d
         tk = k
        endif
        if(d.lt.sm.and.d.ge.fm) then
c if 2nd > distance >= 1st, change 4th to 3rd, 3rd to 2nd, and 2nd to
c distance
         vm = tm
         vk = tk
         tm = sm
         tk = sk
         sm = d
         sk = k
        endif
        if(d.lt.fm.and.d.ne.0.) then
c you get the idea
         vm = tm
         vk = tk
         tm = sm
         tk = sk
         sm = fm
         sk = fk
         fm = d
         fk = k
        endif
       endif
       enddo
!       if(vm.gt.cutoff) cutoff = vm c I wanted to find what distance
!       is safe to cycle through
        if(abs(x(fk)-x(sk)).gt.abs(xbox-abs(x(fk)-x(sk)))) then
          if(x(fk).gt.x(sk)) then
          dfsx = xbox - abs(x(fk)-x(sk))
          else
          dfsx = (x(sk)-x(fk))-xbox
          endif
          else
          dfsx = abs(x(fk)-x(sk))
        endif
        if(abs(y(fk)-y(sk)).gt.abs(ybox-abs(y(fk)-y(sk)))) then
          if(y(fk).gt.y(sk)) then
          dfsy = ybox - abs(y(fk)-y(sk))
          else
          dfsy = (y(sk)-y(fk))-ybox
          endif
          else
          dfsy = abs(y(fk)-y(sk))
        endif
        if(abs(z(fk)-z(sk)).gt.abs(zbox-abs(z(fk)-z(sk)))) then
          if(z(fk).gt.z(sk)) then
          dfsz = zbox - abs(z(fk)-z(sk))
          else
          dfsz = (z(sk)-z(fk))-zbox
          endif
          else
          dfsz = abs(z(fk)-z(sk))
        endif
        if(abs(x(fk)-x(tk)).gt.abs(xbox-abs(x(fk)-x(tk)))) then
          if(x(fk).gt.x(tk)) then
          dftx = xbox - abs(x(fk)-x(tk))
          else
          dftx = (x(tk)-x(fk))-xbox
          endif
          else
          dftx = abs(x(fk)-x(tk))
        endif
        if(abs(y(fk)-y(tk)).gt.abs(ybox-abs(y(fk)-y(tk)))) then
          if(y(fk).gt.y(tk)) then
          dfty = ybox - abs(y(fk)-y(tk))
          else
          dfty = (y(tk)-y(fk))-ybox
          endif
          else
          dfty = abs(y(fk)-y(tk))
        endif
        if(abs(z(fk)-z(tk)).gt.abs(zbox-abs(z(fk)-z(tk)))) then
          if(z(fk).gt.z(tk)) then
          dftz = zbox - abs(z(fk)-z(tk))
          else
          dftz = (z(tk)-z(fk))-zbox
          endif
          else
          dftz = abs(z(fk)-z(tk))
         endif
        if(abs(x(fk)-x(vk)).gt.abs(xbox-abs(x(fk)-x(vk)))) then
          if(x(fk).gt.x(vk)) then
          dfvx = xbox - abs(x(fk)-x(vk))
          else
          dfvx = (x(vk)-x(fk))-xbox
          endif
          else
          dfvx = abs(x(fk)-x(vk))
        endif
        if(abs(y(fk)-y(vk)).gt.abs(ybox-abs(y(fk)-y(vk)))) then
          if(y(fk).gt.y(vk)) then
          dfvy = ybox - abs(y(fk)-y(vk))
          else
          dfvy = (y(vk)-y(fk))-ybox
          endif
          else
          dfvy = abs(y(fk)-y(vk))
        endif
        if(abs(z(fk)-z(vk)).gt.abs(zbox-abs(z(fk)-z(vk)))) then
          if(z(fk).gt.z(vk)) then
          dfvz = zbox - abs(z(fk)-z(vk))
          else
          dfvz = (z(vk)-z(fk))-zbox
          endif
          else
          dfvz = abs(z(fk)-z(vk))
         endif
        if(abs(x(sk)-x(tk)).gt.abs(xbox-abs(x(sk)-x(tk)))) then
          if(x(sk).gt.x(tk)) then
          dstx = xbox - abs(x(sk)-x(tk))
          else
          dstx = (x(tk)-x(sk))-xbox
          endif
          else
          dstx = abs(x(sk)-x(tk))
        endif
        if(abs(y(sk)-y(tk)).gt.abs(ybox-abs(y(sk)-y(tk)))) then
          if(y(sk).gt.y(tk)) then
          dsty = ybox - abs(y(sk)-y(tk))
          else
          dsty = (y(tk)-y(sk))-ybox
          endif
          else
          dsty = abs(y(sk)-y(tk))
        endif
        if(abs(z(sk)-z(tk)).gt.abs(zbox-abs(z(sk)-z(tk)))) then
          if(z(sk).gt.z(tk)) then
          dstz = zbox - abs(z(sk)-z(tk))
          else
          dstz = (z(tk)-z(sk))-zbox
          endif
          else
          dstz = abs(z(sk)-z(tk))
         endif
        if(abs(x(sk)-x(vk)).gt.abs(xbox-abs(x(sk)-x(vk)))) then
          if(x(sk).gt.x(vk)) then
          dsvx = xbox - abs(x(sk)-x(vk))
          else
          dsvx = (x(vk)-x(sk))-xbox
          endif
          else
          dsvx = abs(x(sk)-x(vk))
        endif
        if(abs(y(sk)-y(vk)).gt.abs(ybox-abs(y(sk)-y(vk)))) then
          if(y(sk).gt.y(vk)) then
          dsvy = ybox - abs(y(sk)-y(vk))
          else
          dsvy = (y(vk)-y(sk))-ybox
          endif
          else
          dsvy = abs(y(sk)-y(vk))
        endif
        if(abs(z(sk)-z(vk)).gt.abs(zbox-abs(z(sk)-z(vk)))) then
          if(z(sk).gt.z(vk)) then
          dsvz = zbox - abs(z(sk)-z(vk))
          else
          dsvz = (z(vk)-z(sk))-zbox
          endif
          else
          dsvz = abs(z(sk)-z(vk))
         endif
        if(abs(x(tk)-x(vk)).gt.abs(xbox-abs(x(tk)-x(vk)))) then
          if(x(tk).gt.x(vk)) then
          dtvx = xbox - abs(x(tk)-x(vk))
          else
          dtvx = (x(vk)-x(tk))-xbox
          endif
          else
          dtvx = abs(x(tk)-x(vk))
        endif
        if(abs(y(tk)-y(vk)).gt.abs(ybox-abs(y(tk)-y(vk)))) then
          if(y(tk).gt.y(vk)) then
          dtvy = ybox - abs(y(tk)-y(vk))
          else
          dtvy = (y(vk)-y(tk))-ybox
          endif
          else
          dtvy = abs(y(tk)-y(vk))
        endif
        if(abs(z(tk)-z(vk)).gt.abs(zbox-abs(z(tk)-z(vk)))) then
          if(z(tk).gt.z(vk)) then
          dtvz = zbox - abs(z(tk)-z(vk))
          else
          dtvz = (z(vk)-z(tk))-zbox
          endif
          else
          dtvz = abs(z(tk)-z(vk))
         endif
c finding the distance between the 1st and 2nd,1st and 3rd, etc.
       cab=sqrt(dfsx**2+dfsy**2+dfsz**2)
       cac=sqrt(dftx**2+dfty**2+dftz**2)
       cad=sqrt(dfvx**2+dfvy**2+dfvz**2)
       cbc=sqrt(dstx**2+dsty**2+dstz**2)
       cbd=sqrt(dsvx**2+dsvy**2+dsvz**2)
       ccd=sqrt(dtvx**2+dtvy**2+dtvz**2)
       theta = -5.
       thetb = -5.
       thetc = -5.
       thetd = -5.
       thete = -5.
       thetf = -5.
!       write(32,*) i,cab,cac,cad,cbc,cbd,ccd 
c finding cosines of angles
       theta=(cab**2-fm**2-sm**2)/(-2.*fm*sm)
       thetb=(cac**2-fm**2-tm**2)/(-2.*fm*tm)
       thetc=(cad**2-fm**2-vm**2)/(-2.*fm*vm)
       thetd=(cbc**2-sm**2-tm**2)/(-2.*sm*tm)
       thete=(cbd**2-sm**2-vm**2)/(-2.*sm*vm)
       thetf=(ccd**2-tm**2-vm**2)/(-2.*tm*vm)
       if(theta.ge.-4..and.thetb.ge.-4..and.thetc.ge.-4..and.thetd.ge.
     +-4..and.thete.ge.-4..and.thetf.ge.-4.) then
!       write(33,*) i,theta,thetb,thetc,thetd,thete,thetf  
       q=1.-3.*((theta+1./3.)**2+(thetb+1./3.)**2+
     +   (thetc+1./3.)**2+(thetd+1./3.)**2+(thete+1./3.)
     +   **2+(thetf+1./3.)**2)/8.
c calculating the tetrahedral order parameter, q
!       write(34,*) i,q
       tq(j)=tq(j) + q
c adding all the q's in the bin up       
       waters(j) = waters(j) + 1.
c counting all the waters in the bin
       do b=1,gradN 
c some probability stuff 
        if(q.ge.ran(b-1).and.q.lt.ran(b)) then
        prob(j,b) = prob(j,b) + 1
        exit
        endif
       enddo
       else
       fails = fails + 1
c if the cosine of the angles aren't reasonable (>= -4), it counts as a
c fail. If it fails, it'll tell you how often at the end. I haven't ever
c seen it fail.
       endif
       endif
        
      enddo
      enddo
      enddo
c fancy shmancy xmgrace formatting stuff to get labels
      write(12,*) '@    title "Tetrahedral Order"'
      write(12,*) '@    xaxis label "z position (nm)"'
      write(12,*) '@    yaxis label "average tetrahedral order"'
      write(12,*) '@TYPE xy'
      do j=1,nbins
      rati = real(nbins*waters(j))/real(wata*clock)
c rati helps determine if data points are reasonable based on what
c percent of the waters are in that bin
      if(rati.gt.0.06)write(12,*) bin(j)-dz/2.,tq(j)/real(waters(j))
      enddo
      write(16,*) '@    title "Tetrahedral Order Parameter probability"'
      write(16,*) '@    xaxis label "tetrahedral order"'
      write(16,*) '@    yaxis label "probability"'
      write(16,*) '@TYPE xy'
      do b=1,gradN
        write(16,98)ran(b)-dN/2.,(real(prob(j,b))/real(wata),j=1,nbins)
      enddo
!      write(*,*) cutoff
!      open(63,file='interface.dat')
!c interface.dat is a file put out by the density program with a list of 
!c bins that are in the interfacial region
!      iostatus = 0
!      do
!      read(63,*,iostat=iostatus) inter
!      if(iostatus.ne.0) exit
!      write(23,*) bin(inter)-dz/2., tq(inter)/real(waters(inter))
!      enddo
      if(fails.ne.0) write(*,*) 'unable to calculate for',fails,'of',
     + wata*clock,'waters','(',real(fails)/real(wata*clock),')'
c I haven't ever seen it write the fails
c try increasing the cycling distance for the x/y/z compenents
c those were added to speed things up
      deallocate(bin,x,y,z,atsym,tq,waters,ran,prob,alk,ares)
      close(9)
      close(12)
      close(16)
!      close(23) 
!      close(63)
98    format(200(f18.16,2x))
99    format(i5,a4,2x,a4,i5,2x,f6.3,2x,f6.3,2x,f6.3)
      end
