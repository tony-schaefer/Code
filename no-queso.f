      program node_que_sumbission_ordering
      ! NO-QueSO, for short

      character adummy*81,cdummy*2,tens,aslot,nodelist*350
      integer,allocatable:: slots(:),maxslot(:),space(:)
      integer ios,minslot,procs,que,node,totspace,reserved

      allocate(slots(0:12),maxslot(0:12),space(0:12))

      do i=1,12
      !making sure no funny business happens
      !only boring business
      slots(i)=97
      maxslot(i)=0
      space(i)=0
      enddo
 
      reserved=0

      open(7,file='queso.test')
      !"qstat -f > queso.test" is what the program will read

      read(5,*) procs
      ! procs is the number of slots the job being submitted will take

      flag=0
      con=0
      
1     totspace=0
      minslot=-99
      ! minslot is the most slots available on any one node
      ! negative because negative node space is possible with cheese
      do
      ! read the number of filled slots on each node
      
      read(7,'(a81)',iostat=ios) adummy
      
      if(ios.ne.0) then
      !when the reading is done and there isn't space for the next job
      !to leave some slots open, be considerate
      if((space(que).le.procs.or.(totspace-procs).le.48).and.
     +con.eq.0.and.totspace.gt.0) then
      slots(que)=99
      flag=1
      reserved=reserved+space(que)
      if(reserved.lt.48) then
      con=0
      else
      con=1
      endif
      write(*,'(i2,a,i2)') space(que),' will be saved on node ',que
      rewind(7)
      goto 1
      else
      ! otherwise quit
      exit
      endif
      endif
      

      if(adummy(1:3).eq.'all') then
      read(adummy(18:18),'(a1)') tens
      if(tens.eq.'.') then
      length=17
      else
      length=18
      endif
      read(adummy(17:length),'(i2)') node
      ! don't want the '.' of compute-0-n.local to be read
      read(adummy(41:41),'(a1)') aslot
      ! don't want to read a '/'
      if(aslot.eq.'/') then
      nslot=40
      else
      nslot=41
      endif
      if(slots(node).lt.98) then
      read(adummy(40:nslot),'(i2)') slots(node)
      ! how many slots are used on the node
      read(adummy(nslot+2:nslot+3),'(i2)') maxslot(node)
      ! how many slots can the node have filled
      endif

      !how much space is open on the node
      space(node)=maxslot(node)-slots(node)
      
      if(slots(node).eq.99.and.con.eq.0.and.reserved.eq.0) then
      con=1
      endif
      !if this is the first time we are reading queso.test, there may
      !have already been space reserved that we don't know about
      
      if(slots(node).eq.98) then
      flag=1
      endif

      if(space(node).eq.maxslot(node).and.flag.eq.0) then
      slots(node)=98
      flag=1
      write(*,'(i2,a,i2)') space(node),' will be saved on',node
      space(node)=-99
      endif
      !keep a whole node free

      if(adummy(77:78).ne.'  ') then
      maxslot(node)=0
      slots(node)=97
      endif
      !check to see if the node is down

      if(space(node).ge.0) totspace=totspace+space(node)
      !negative space should not be counted; node 4 would mess stuff up
      if(space(node).ge.minslot.and.maxslot(node).ne.0) then
      !finding the emptiest node
      que=node
      minslot=space(node)
      endif
      endif
      enddo
    
      rewind(7)

      open(123,file='salsa')
      !Submission Argument for the Largest Space Available
      if(space(que).ge.procs) then
      write(nodelist(1:2),'(i2)') que
      nodelist(1:2)=adjustl(nodelist(1:2))
      if(que.lt.10) then
      write(123,'(a13,a1,a6)') 'q@compute-0-',nodelist(1:1),'.local'
      else
      write(123,'(a13,a2,a6)') 'q@compute-0-',nodelist(1:2),'.local'
      endif
      !write the emptiest node to salsa so the shells can cat it
      slots(que)=slots(que)+procs
      !update the emptiest node with what was just added because the
      !shells are faster than qstat updates
      else
      !or Submission Argument for the List of Spaces Acceptable
      !if there's no more space, make lists of nodes that aren't being
      !saved for anything
      do k=1,350
      nodelist(k:k)=' '
      ! no funny business with the nodelist
      enddo
      k=0
      do i=0,12
      if(slots(i).lt.97) then
      ! 97+ are special numbers
      nodelist(k:k+15)="all.q@compute-0-"
      if(i.lt.10) then
      !different formatting if there's a tens spot or not
      write(nodelist(k+16:k+16),'(i1)') i
      nodelist(k+17:k+22)='.local'
      nodelist(k+23:k+23)=","
      k=k+24
      else
      write(nodelist(k+16:k+17),'(i2)') i
      nodelist(k+18:k+23)='.local'
      nodelist(k+24:k+24)=","
      k=k+25
      endif
      endif
      enddo
      nodelist(k-1:k-1)=' '
      !last one in the list shouldn't end with a comma
      nodelist(1:346)=nodelist(4:350)
      nodelist(347:350)='    '
      ! make it so we so qsub -q all.$NODE and not qsub -q $NODE because
      ! there's a difference
      write(123,'(a350)') nodelist(1:350)
      endif

      close(123)
      
      do i=0,12
      !sooper dooper formated writes that put everything (that matters)
      !back where it was in queso.test so the next time the program is
      !used, queso.test can be read

      if(i.ge.10) write(7,'(a3,3x,a10,i2,a1,19x,i3,a1,i2)')
     +'all','compute-0-',i,'.',slots(i),'/',maxslot(i)
      if(i.lt.10) write(7,'(a3,3x,a10,i1,a1,20x,i3,a1,i2)')
     +'all','compute-0-',i,'.',slots(i),'/',maxslot(i)

      enddo

      close(7)

      deallocate(slots,space,maxslot)

      end
