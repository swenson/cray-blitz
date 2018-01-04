      subroutine dcmnd
crev  last revision 04/05/90
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
      integer char(-6:6)
c
c
      include 'global.f'
c
c
      include 'common.f'
c
c
      equivalence (blank,alpha(70)),(aster,alpha(66))
      data char / ' K', ' Q', ' R', ' B', ' N', ' P', ' -',
     *            'PP', 'NN', 'BB', 'RR', 'QQ', 'KK' /
      data wsq / ' +' /
c
c
c------------------------------< set up the display board
c
      wrong=0
      if((atext(2).ne.blank) .and. (atext(2).ne.aster)) wrong=1
      ocolor=color
      if(wrong .eq. 0) color=3-color
      sq=21
      if(wrong .eq. 0) sq=100
      do 200 i=1,8
          do 100 j=1,8
              if(wrong .ne. 0) sq=sq+1
              if(wrong .eq. 0) sq=sq-1
              sub=(i-1)*8+j
              bdchar(sub)=char(board(sq))
100       continue
          if(wrong .ne. 0)sq=sq+2
          if(wrong .eq. 0) sq=sq-2
200   continue
      fudge=0
      do 400 i=1,8
          do 300 j=1,7,2
              if(bdchar((i-1)*8+j+fudge) .eq. char(0))
     *               bdchar((i-1)*8+j+fudge)=wsq
300       continue
          fudge=and(fudge+1,1)
400   continue
c
c------------------------------< output board
c
      if(color .eq. 1) print 500
      if(color.eq.1 .and. atext(1).eq.aster) write(3,500)
500   format(15x,' h  g  f  e  d  c  b  a'/)
      do 700 i=1,8
          f=i
          if(color .ne. 1) f=9-i
          j=(i-1)*8+1
          k=j+7
          print 600,(bdchar(l),l=j,k), f
          if(atext(2) .eq. aster) write(3,600)
     *                               (bdchar(l),l=j,k), f
600       format(15x,8(a2,1x),i3)
700   continue
      if(color .eq. 2) print 800
      if(color.eq.2 .and. atext(2).eq.aster) write(3,800)
800   format(/15x,' a  b  c  d  e  f  g  h')
      color=ocolor
      return
      end
