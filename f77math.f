      subroutine f77sqdist(x1,y1,z1,x2,y2,z2,xbox,ybox,zbox,sqdist)

! built for f2py
Cf2py intent(out) sqdist
Cf2py intent(in) x1,y1,z1,x2,y2,z2,xbox,ybox,zbox
Cf2py depend(sqdist) f77sqdist(x1,y1,z1,x2,y2,z2,xbox,ybox,zbox)

      dx1=x1-x2
      dx2=x2-x1
      if(dx1.gt.dx2) then
          xdb=(dx1-xbox)**2
      else
          xdb=(dx2-xbox)**2
      endif
      xd=(dx1)**2
      if(xdb.lt.xd) then
          xd=xdb
      endif
      dy1=y1-y2
      dy2=y2-y1
      if(dy1.gt.dy2) then
          ydb=(dy1-ybox)**2
      else
          ydb=(dy2-ybox)**2
      endif
      yd=(dy1)**2
      if(ydb.lt.yd) then
          yd=ydb
      endif
      dz1=z1-z2
      dz2=z2-z1
      if(dz1.gt.dz2) then
          zdb=(dz1-zbox)**2
      else
          zdb=(dz2-zbox)**2
      endif
      zd=(dz1)**2
      if(zdb.lt.zd) then
          zd=zdb
      endif
      sqdist=xd+yd+zd 
     
      return

      end subroutine

      subroutine f77vectorize(x1,y1,z1,x2,y2,z2,xbox,ybox,zbox,xd,yd,zd)

Cf2py intent(out) xd,yd,zd
Cf2py intent(in) x1,y1,z1,x2,y2,z2,xbox,ybox,zbox
Cf2py depend(xd) f77vectorize(x1,x2,xbox)
Cf2py depend(yd) f77vectorize(y1,y2,ybox)
Cf2py depend(zd) f77vectorize(z1,z2,zbox)

      if(abs(x2-x1).gt.abs(xbox-abs(x2-x1))) then
        if(x2.gt.x1) then
            xd = xbox-abs(x2-x1)
        else
            xd = (x1-x2)-xbox
        endif
      else
        xd = x1 - x2
      endif
      if(abs(y2-y1).gt.abs(ybox-abs(y2-y1))) then
        if(y2.gt.y1) then
            yd = ybox-abs(y2-y1)
        else
            yd = (y1-y2)-ybox
        endif
      else
        yd = y1 - y2
      endif
      if(abs(z2-z1).gt.abs(zbox-abs(z2-z1))) then
        if(z2.gt.z1) then
            zd = zbox-abs(z2-z1)
        else
            zd = (z1-z2)-zbox
        endif
      else
        zd = z1 - z2
      endif

      return 

      end subroutine
