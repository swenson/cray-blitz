      subroutine pgcmnd
crev  last revision 12/09/90
c
c     ******************************************************************
c     *                                                                *
c     *      pgcmnd is used to output a list of the game moves that    *
c     *  it kept by the program.  it prints the moves in a compressed  *
c     *  form for speed and so that the output is convenient to give   *
c     *  to an opponent after the game.                                *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      integer buf1(30), buf(80), digits(10)
c
c
      include 'global.f'
c
c
      include 'common.f'
c
c
      equivalence (buf1(1),moves(100)),(buf(1),moves(200))
      equivalence (blank,alpha(70)),(digits(1),alpha(53))
c
c
c------------------------------< initialize.
c
      m=1
      i=0
      col=4
      size=scan(col)
      if(size .le. 10) size=30
      do 10 j=1,80
          buf(j)=blank
10    continue
c
c------------------------------< output heading
c
      if(color .eq. 2) print 100, name
      if(color .eq. 1) print 200, name
100   format(1x,a20,' - cray blitz')
200   format(1x,'cray blitz - ',a20)
c
c------------------------------< read moves from history file
c
300   continue
          i=i+1
          if(i .gt. npmovs+nomovs) go to 9999
          read(unit=1,fmt=301,rec=i+1) buf1, elapp
301       format(30a1,i6)
c
c------------------------------< insert move number in text
c
          mnum=(i+1)/2
          if(mod(i,2) .eq. 1) then
              if(mnum .ge. 100) then
                  buf(m)=digits(mnum/100+1)
                  m=m+1
              endif
              if(mnum .ge. 10) then
                  buf(m)=digits((mnum-mnum/100*100)/10+1)
                  m=m+1
              endif
              buf(m)=digits(mod(mnum,10)+1)
              m=m+2
          endif
          do 700 j=1,10
              if(buf1(j) .eq. blank) go to 800
              buf(m)=buf1(j)
              m=m+1
700       continue
800       continue
          m=m+1
          if(m .lt. size) go to 300
          print 900, (buf(j),j=1,m)
900       format(1x,80a1)
          do 1000 j=1,80
              buf(j)=blank
1000      continue
          m=1
      go to 300
c
c-----------------------------< done, print remaining text in
c-----------------------------< buffer (if any) and return.
c
9999  continue
          if(m .gt. 1) print 900, (buf(j),j=1,m)
          return
      end
