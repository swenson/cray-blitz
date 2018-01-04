      program bookp
      implicit integer (a-z)
      integer card(72), head(50), digits(10)
      character*30 namein, nameout
c
c
      include 'common.f'
c
c
      include 'global.f'
c
c
c
c
      common /record/ record(100,4000)
      data digits/'0','1','2','3','4','5','6','7','8','9'/
      data alphay / 'y' /
      data pcnt / 0 /
      data paged / 0 /
      data postns / 0 /
      data page / 0 /
      data head / 50*'    ' /
      data astrsk/'*'/, blank/' '/, zero/'0'/, nine/'9'/
      data quest /'?'/, alphab /'b'/, comma /','/, dollar /'$' /
      data bmask /o'17777777777777' /
c
c------------------------------< initialization
c
c     tdata(1)=setb60(loc(tdata(1)))
      call mpcopy
      call copyg(1)
      print 1013
1013  format(1x,'enter name of binary (output) data file: ')
      read 1012, nameout
      print 1011
1011  format(1x,'enter name of source (input) data file: ')
      read 1012, namein
1012  format(a)
      print 1014
1014  format(1x,'enter size of book (i4 format): ')
      read 1015, sizebook
1015  format(i4)
      open (unit=3,file=nameout,status='NEW',access='DIRECT',
     *      form='UNFORMATTED',recl=800)

      open (unit=5,file=namein,status='OLD')
      do 43 i=1,4000
          do 42 j=1,100
              record(j,i)=0
42        continue
43    continue
      record(1,1)=sizebook
c     call lockasgn(lockmp)
c     call lockasgn(lockht)
c     call lockasgn(lockpt)
      mchtyp=tst124(0)
      hsize=32768
      hsizm1=hsize-1
      psize=ishft(hsize,-2)
      psizm1=psize-1
      ksize=ishft(hsize,-3)
      ksizm1=ksize-1
      htbl1=7168+hsize
      htbl2=htbl1*2
      ktbl1=htbl2+hsize+7168+1
      ktbl2=ktbl1+ksize
      ptbl1=ktbl2+ksize+1
      ptbl2=ptbl1+psize
      do 622 i=1,50
          moveds(i)=3
622   continue
      color=1
      call setup
      call setgb(1)
      next=1
      level=0
      plevel=0
      side=1
      line=0
c
c------------------------------< read and print next record
c
100   continue
      line=line+1
      read(5,101,end=9999)card
      do 198 i=1,72
          if(card(i) .ne. blank) goto 197
198   continue
197   continue
      if(card(i) .ne. nine) go to 199
          if(mscore .ne. 0) print 301, mscore
301       format(30('*'),'material score = ',i6,20('*'))
199   continue
      if(card(1) .eq. dollar) pcnt=100
101   format(72a1)
103   format(1x,i5,1x,72a1)
104   format('1')
      do 108 char=1,72
          if(card(char).ne.blank) go to 109
108   continue
      go to 100
109   continue
      if(card(char) .ne. dollar) go to 541
          do 542 i=1,50
              head(i)=card(char+i)
542       continue
541   continue
          if(pcnt.lt.55) go to 5844
              page=page+1
              print 5845, head, page
5845          format('1',50a1,t72,'page',i5//)
              pcnt=0
5844      continue
105   continue
          pcnt=pcnt+1
          print 103, line, (card(lll),lll=1,72)
          if(card(1).eq.dollar .or. card(1).eq.astrsk) go to 100
c
c------------------------------< scan input record and pick up
c------------------------------< level and move text
c
200   continue
      plevel=level
      if(char .ge. 72) go to 100
      do 201 char=char,72
          if(card(char) .ne. blank) go to 203
201   continue
      go to 100
203   continue
      level=0
202   do 250 j=1,10
          if(card(char).eq.digits(j)) go to 300
250   continue
      go to 320
300   continue
      level=level*10+j-1
      char=char+1
      if(card(char).ge.zero.and.card(char).le.nine) go to 202
          do 350 char=char,72
              if(card(char).ne.blank) go to 375
350       continue
      go to 8888
320   continue
      level=plevel+1
375   continue
      if(level .gt. 50) go to 100
      do 376 l=1,30
          atext(l)=blank
376   continue
      do 380 l=1,30
          sub=char+l-1
          if(card(sub).eq.blank .or. card(sub).eq.comma) go to 390
          atext(l)=card(sub)
          if(sub .eq. 72) go to 390
380   continue
390   continue
      char=sub+1
400   continue
      if(level .le. plevel) go to 600
c
c------------------------------< this level is deeper than the previous
c------------------------------< level. advance the pointers and proceed
c------------------------------< to the next level.
c
500   continue
      ply=level
      player=2-mod(ply,2)
      side=cvside(player)
      if(ply .le. 2) go to 722
          moveds(ply)=moveds(ply-2)
722   continue
      call input(1)
      if(return .ne. 0) go to 8888
      postns=postns+1
      from(ply)=mfrom
      to(ply)=mto
      type(ply)=mtype
      call make
      prevmv(1+3*mod(ply+1,2))=mfrom
      prevmv(2+3*mod(ply+1,2))=mto
      go to 200
c
c------------------------------< this level is lower than last
c------------------------------< level, start backing out of tree
c------------------------------< writing records as we go
c
600   continue
      if(plevel .gt. 50) go to 650
      ply=plevel
      player=2-mod(ply,2)
      side=cvside(player)
      mfrom=from(ply)
      mto=to(ply)
      mtype=type(ply)
      ktemp=and(hash,65535)
      key=mod(ktemp,sizebook/2)+(player-1)*sizebook/2+1
      do 610 i=1,100,2
          if(and(hash,bmask) .eq. record(i,key)) go to 620
          if(record(i,key) .eq. 0) go to 620
610   continue
      print 611
611   format(1x,'enlarge book database')
620   continue
          record(i,key)=and(hash,bmask)
          record(i+1,key)=131072
640   continue
      if(plevel .le. 0) go to 1776
      call unmake
650   continue
      plevel=plevel-1
      go to 400
c
c------------------------------< an error was detected, print the move
c------------------------------< error, display the board and continue.
c
8888  print 8889,char
8889  format(1x,'error in column ',i2,' skipping to next record.')
      print 8890,line,level,(atext(i),i=1,30)
8890  format(1x,'line:',i5,2x,i2,1x,30a1)
      call dcmnd
      stop
9999  level=0
          ply=plevel
      go to 400
1776  continue
      print 1777,postns
1777  format(/////1x,'the book contains',i7,' positions.')
      do 1778 key=1,sizebook
          write(3,rec=key) (record(i,key),i=1,100)
1778  continue
      stop
      end
