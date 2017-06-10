      program dostuff
      
      !gfortran -fdefault-real-8 metropolis.f95 -o metropolis.e

      implicit none

      double precision,dimension(1:1000):: Ea,Ca,Sa
      double precision:: temp
      integer:: atoms,num,k

      k=1000
      
      do num=1,k,1
        temp=dble(num)*10./(dble(k))
        call Metropolis(500,temp,Ca(num),Ea(num),Sa(num)) 
      enddo

      open(6,file='metropolis.rtw')
      write(6,'(a)') "pointscatter()"
      write(6,'(a)') "import matplotlib.lines as ml"
      write(6,'(a)') "plot.title(r'Ising Model',fontsize=20)"
      write(6,'(a)') "plot.ylabel(r'Value',fontsize=15)"
      write(6,'(a)') "plot.xlabel(r'Temperature (K)',fontsize=15)"
      write(6,'(a)') "plot.plot(x,[val[0] for val in y],label=r'$\frac{&
      &C_v}{n}$ $(J/K\cdot spin)$',color='green')"
      write(6,'(a)') "plot.plot(x,[val[1] for val in y],label=r'$\frac{&
      &\langle E\rangle}{n}$ $(J/spin)$',color='blue')"
      write(6,'(a)') "plot.plot(x,[val[2] for val in y],label=r'&
      &$\frac{S}{k_b\cdot n}$ $(spin^{-1})$',color='red')"
      write(6,'(a)') "a=ml.Line2D([],[],label=r'$\frac{C_v}{n}$ &
      &$(J/K\cdot spin)$',color='green')"
      write(6,'(a)') "b=ml.Line2D([],[],label=r'$\frac{\langle &
      &E\rangle}{n}$ $(J/spin)$',color='blue')"
      write(6,'(a)') "c=ml.Line2D([],[],label=r'$\frac{S}{k_b\cdot n}$&
      & $(spin^{-1})$',color='red')"
      write(6,'(a)') "plot.legend(handles=[a,b,c])"
      write(6,'(a)') "plot.legend(bbox_to_anchor=(0.9,0.4))"

      do num=1,k,1
        temp=dble(num)*10./(dble(k))
        write(6,'(4(f9.6,2x))') temp,Ca(num),Ea(num),Sa(num)
      enddo
      close(6)

      end program dostuff

      subroutine Metropolis(n,T,Cv,E,Se)  
      
      !use IFPORT
      implicit none
      
      integer,intent(in)::n                         
      double precision,intent(in)::T
      double precision,intent(out)::Cv
      double precision,intent(out)::E
      double precision,intent(out)::Se
      integer,dimension(1:n):: SpinArray          
      integer:: indx,indx_left,sampling,s,q,indx_right,i
      integer,dimension(1:8):: ilist
      double precision:: J,probability,energy,total_energy,&
     &total_energy_sqr,k,avg_energy,avg_energy_sqr,delta_energy    
      
      J=2.0
      total_energy=0.0
      total_energy_sqr=0.0
      delta_energy=0.0                
      probability=exp(-4.0*J/T)   
      energy=-J*n                     
      sampling=n*4000
      SpinArray(:)=-1

      call date_and_time(VALUES=ilist)

      call srand(ilist(8)+ilist(7))
      do i=1,sampling,1
            indx=floor(1.+dble(n-1)*rand())
            indx_left=indx-1                         
                  if(indx_left.eq.0) then
                        indx_left=n
                  endif
            indx_right=indx+1                       
                  if(indx_right.gt.n) then
                        indx_right=1
                  endif
            delta_energy=2.*J*dble(SpinArray(indx)*(SpinArray(indx_left)&
     &          +SpinArray(indx_right)))
      k=rand()
                  if((delta_energy.le.0.).or.(k.lt.probability))& 
     &then
                        SpinArray(indx)=-SpinArray(indx)  
                        energy=energy+delta_energy           
                  endif
            total_energy=total_energy+energy
            total_energy_sqr=total_energy_sqr+energy**2
      enddo
      
      avg_energy=total_energy/dble(sampling)
      avg_energy_sqr=total_energy_sqr/dble(sampling)
      
      Cv=(avg_energy_sqr-avg_energy**2)/(T**2)
      Cv=Cv/dble(n)                                                 
      E=avg_energy/dble(n)                                           
      s=0 
            do q=1,n-1,1
                  if(SpinArray(q+1).ne.SpinArray(q)) then
                        s=s+1
                  endif
            enddo
      if(s.gt.0) then
      Se=(dble(n)*log(dble(n))-dble(s)*log(dble(s))-dble(n-s)*&
     &log(dble(n-s)))/dble(n)     
      else
      Se=0.
      endif
      
      End subroutine Metropolis
     
