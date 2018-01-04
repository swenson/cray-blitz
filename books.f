      program books
      implicit integer (a-z)
c
c
      character*30 namein, nameout
c
c
      include 'common.f'
c
c
      include 'global.f'
c
c
      common /record/ record(100,4000)
      data bmask / o'17777777777777' /
c
c------------------------------< initialization
c
c     tdata(1)=setb60(loc(tdata(1)))
      print 1013
1013  format(1x,'enter name of file to score: ')
      read 1012, nameout
1012  format(a)
      open (unit=3,file=nameout,status='OLD',access='DIRECT',
     *      form='UNFORMATTED',recl=800)
      call mpcopy
      call copyg(1)
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
      color=1
      call set gb(1)
      call setup
      call locate
      do 188 i=1,50
          inchk(i)=0
188   continue
      ply=0
      side=-1
      first(1)=1
      read(unit=3,rec=1) sizebook
      print 1884, sizebook
1884  format(1x,'size of book is ',i4)
      do 199 key=1,sizebook
          read(unit=3,rec=key) (record(j,key),j=1,100)
199   continue
      depth=100
      rdepth=100
c
c------------------------------< advance to the next level.
c
1000  continue
          ply=ply+1
          side=-side
          player=2-and(ply,1)
          if(ply .gt. 50) go to 5000
          if(ply .gt. 1) first(ply)=last(ply-1)+1
          value(ply)=-99999999
          if(ply .le. 2) go to 8160
              moveds(ply)=moveds(ply-2)
              go to 8180
8160      continue
          moveds(ply)=movedr(player)
8180      continue
          call movgen
          which(ply)=first(ply)-1
c
c------------------------------< select and make next move
c
2000      continue
              which(ply)=which(ply)+1
              if(which(ply) .gt. last(ply)) go to 4000
              mfrom=extrct(moves(which(ply)))
              call make
              from(ply)=mfrom
              to(ply)=mto
              type(ply)=mtype
              ktemp=and(hash,65535)
              key=mod(ktemp,sizebook/2)+(player-1)*sizebook/2+1
              do 2010 i=1,100,2
                  if(and(hash,bmask) .eq. record(i,key)) go to 6000
2010          continue
2020          continue
              call unmake
              go to 2000
c
c------------------------------< level is completed. update the
c------------------------------< book and back up to previous level.
c
4000      continue
              if(value(ply) .ne. -99999999) go to 4005
                  call score
                  if(tscore .gt. 0) tscore=tscore-100
                  value(ply)=side*tscore
4005          continue
              ktemp=and(hash,65535)
              key=mod(ktemp,sizebook/2)+(2-player)*sizebook/2+1
              do 4010 i=1,100,2
                  if(and(hash,bmask) .eq. record(i,key)) go to 4100
4010          continue
              print 4020
4020          format(10(1x,'*'/),1x,'internal program error',
     *               10(1x,'*'/))
              go to 3000
4100          continue
                  record(i+1,key)=-value(ply)
c
c------------------------------< back up and try next move at the
c------------------------------< previous level.
c
3000      continue
              value(ply-1)=max0(value(ply-1),-value(ply))
3010          continue
              ply=ply-1
              if(ply .le. 0) go to 7000
              side=-side
              player=2-and(ply,1)
              mfrom=extrct(moves(which(ply)))
              call unmake
              go to 2000
c
c------------------------------< at maximum depth, previous node
c------------------------------< must have been terminal
c
5000      continue
              call score
              if(tscore .gt. 0) tscore=tscore-100
              value(ply-1)=max0(value(ply-1),-side*tscore)
              go to 3010
c
c
c------------------------------< this position was found in the book
c------------------------------< data base. if there is also a score
c------------------------------< for it, no further searching is
c------------------------------< necessary.
c
6000      continue
              if(record(i+1,key) .eq. 131072) go to 1000
              value(ply)=max0(value(ply),record(i+1,key))
              go to 2020
c
c------------------------------< finished.  write the book out and
c------------------------------< exit.
c
7000  continue
      do 7100 key=1,sizebook
          write(3,rec=key) (record(i,key),i=1,100)
7100  continue
      stop
      end
