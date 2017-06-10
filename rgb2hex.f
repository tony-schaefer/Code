      program rgbhex

      integer r,g,b
      character*1 base16(0:16),hexagon*7

      print*,'enter r g b'

      read(5,*) r,g,b

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

      hexagon='#'//base16(mod((r/16),16))//base16(mod(r,16))//base16(
     +mod((g/16),16))//base16(mod(g,16))//base16(mod((b/16),16))//
     +base16(mod(b,16))

      print*,hexagon

      end 
