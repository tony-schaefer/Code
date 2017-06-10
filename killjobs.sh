#!/bin/tcsh

set g = $1
set h = $2

@ h = $h + 1

set pin=( `ls /home/scha0275/.pinning/` )

while ($g < $h)
  qdel $g
  foreach fil ( $pin )
    sed -i "s/'$g'/'unpinned'/g" /home/scha0275/.pinning/$fil
  end
  @ g = $g + 1
end
