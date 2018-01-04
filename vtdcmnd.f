      subroutine vtdcmnd
crev  last revision 10/23/91
c
c     ******************************************************************
c     *                                                                *
c     *      dcmnd is used to display the game board in response to    *
c     *  'd' or 'do' commands.                                         *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      integer bdchar(64)
      integer char(13)
c
c
      include 'global.f'
c
c
      include 'common.f'
c
c
      equivalence (blank,alpha(70)),(aster,alpha(66))
      data char / ' K', ' Q', ' R', ' B', ' N', ' P', '  ',
     *            '(P)', '(N)', '(B)', '(R)', '(Q)', '(K)' /
      data escape / ''/
c
c------------------------------< set up the display board
c
      wrong=0
      if((text(2).ne.blank).and.(text(2).ne.aster)) wrong=1
      ocolor=color
      if(wrong .ne. 0) color=3-color
      sq=21
      if(wrong .ne. 0) sq=100
      do 200 i=1,8
          do 100 j=1,8
              if(wrong .eq. 0) sq=sq+1
              if(wrong .ne. 0) sq=sq-1
              sub=(i-1)*8+j
              bdchar(sub)=char(board(sq)+7)
100       continue
          if(wrong .eq. 0) sq=sq+2
          if(wrong .ne. 0) sq=sq-2
200   continue
c
c------------------------------< output board
c
      if(color .eq. 1) print 300
300   format(15x,' h  g  f  e  d  c  b  a'/)
      if(vttype .ne. 3) go to 500
          print 400, escape, '(0'
400       format(14x,a1,a2,
     *           'lqqqqqqqqqqqqqqqqqqqqqqqqk')
500   continue
      do 800 i=1,8
          f=i
          if(color .ne. 1) f=9-i
          j=(i-1)*8+1
          k=j+7
          if(vttype .eq. 2) print 600, (escape, '[',7*mod(l+i-1,2),'m',
     *                bdchar(l), l=j,k), escape, '[0m', f
600       format(15x,8(a1,a1,i1,a1,a3),a1,a3,i3)
          if(vttype .eq. 3) print 700, 'x', (escape, '[',7*mod(l+i-1,2),
     *                'm', bdchar(l), l=j,k), escape, '[0mx', f
700       format(14x,a1,8(a1,a1,i1,a1,a3),a1,a4,i3)
800   continue
      if(vttype .ne. 3) go to 1000
          print 900, escape
900       format(14x,'mqqqqqqqqqqqqqqqqqqqqqqqqj',a1,'(B')
1000  continue
      if(color .eq. 2) print 1100
1100  format(/15x,' a  b  c  d  e  f  g  h')
      color=ocolor
      return
      end
