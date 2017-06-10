      program make_inputdotdat

      integer nbins,itime,gradN

c this program will be pretty useless after we test all boxes under the same conditions
      write(*,*) "how big of a slice do you want?"
      read(5,*) nbins
      write(*,*) "when do I start?"
      read(5,*) itime
      write(*,*) 'type "15"'
      read(5,*) gradN
      open(7,file='input.dat')
      write(7,*) nbins
      write(7,*) itime
      write(7,*) gradN
      close(7)
      end 
