      subroutine ccmnd
crev  last revision 10/16/91
c
c     ******************************************************************
c     *                                                                *
c     *      ccmnd handles all 'chess clock' commands.  there          *
c     *  are five basic operations that can be performed by this       *
c     *  routine:                                                      *
c     *                                                                *
c     *      1)  the chess clock can be set to some specific           *
c     *          value along with a move counter to force the          *
c     *          program and human to make a certain number of         *
c     *          moves in a specified time period, for example         *
c     *          40 moves in 10 minutes for high-speed chess.          *
c     *                                                                *
c     *      2)  the chess clock can be displayed, telling how         *
c     *          much time each side has on it's clock to see          *
c     *          how each side is doing with respect to time.          *
c     *                                                                *
c     *      3)  the computer's time per move is usually in            *
c     *          computer time which may be much slower than           *
c     *          real or wall clock time.  the computer may be         *
c     *          forced to use elapsed time just as the human          *
c     *          always does if desired.  this may get the prog-       *
c     *          ram into time trouble, but may be required for        *
c     *          tournament play where computer time is not            *
c     *          understood by everyone.                               *
c     *                                                                *
c     *      4)  the program can be instructed to periodically         *
c     *          ask the terminal operator how much time is            *
c     *          left on it's clock since computer timing may          *
c     *          not exactly agree with the chess clock.  see          *
c     *          module 'query' for further details.                   *
c     *                                                                *
c     *      5)  the program can be instructed to complete the time    *
c     *          control with a certain amount of time left over as    *
c     *          a surplus or 'pad'.  this is usually 10 minutes for   *
c     *          tournament mode.                                      *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
c
c
      include 'global.f'
c
c
      include 'common.f'
c
c
      equivalence
     *   (b,alpha(28)),  (c,alpha(29)),  (d,alpha(30)),
     *   (e,alpha(31)),  (l,alpha(38)),  (o,alpha(41)),
     *   (p,alpha(42)),  (q,alpha(43)),  (s,alpha(45)),
     *   (t,alpha(46)),  (blank,alpha(70)),  (quest,alpha(69)),
     *   (zero,alpha(53)),  (aster,alpha(66)),  (equal,alpha(67))
c
c
c------------------------------< determine which option was
c------------------------------< requested.
c
      if(atext(3) .eq. b) go to 150
      if(atext(3) .eq. c) go to 200
      if(atext(3) .eq. d) go to 500
      if(atext(3) .eq. s) go to 1300
      if(atext(3) .eq. t) go to 1600
      if(atext(3) .eq. q) go to 1000
c
c------------------------------< cl : set chess clock
c
          if(atext(3) .eq. equal) then
              col=4
              do 50 i=1,5
                  temp=scan(col)
                  timlim(i)=temp
                  col=col+1
50            continue
          endif
          if(timlim(5) .eq. 0) then
              gelap1=timlim(2)/60
              selap1=timlim(4)/60
              print 100, timlim(1), gelap1, timlim(3), selap1
              write(3,100) timlim(1), gelap1, timlim(3), selap1
100           format(1x,i2,' moves in ',i3,' minutes'/
     *           1x,'then ',i2,' moves in ',i3,' minutes.')
          else
              print 101, hhmmss(timlim(1),5), hhmmss(timlim(2),8),
     *                   hhmmss(timlim(3),5), hhmmss(timlim(4),8),
     *                   hhmmss(timlim(5),5)
              write(3,101) hhmmss(timlim(1),5), hhmmss(timlim(2),8),
     *                     hhmmss(timlim(3),5), hhmmss(timlim(4),8),
     *                     hhmmss(timlim(5),5)
101           format('     average ',a5,' per move for ',a8,/
     *               'then average ',a5,' per move for ',a8,/
     *               'then average ',a5,' for the rest of the game'//)
              return
          endif
c
c------------------------------< clb : set beginning clock time
c
150   continue
          col=5
          temp=scan(col)
          if(temp.ne.0 .and. atext(4).eq.equal) then
              cbegin=temp
              if(cbegin .lt. 0) then
                  print 160
160               format(1x,'beginning time cannot be negative.')
                  cbegin=0
                  return
              endif
          endif
          print 170, hhmmss(cbegin,5)
          write(3,170) hhmmss(cbegin,5)
170       format(1x,'beginning time is ',a5)
          return
c
c
c------------------------------< clc : correct chess clock
c
200   continue
          code=atext(4)
          if(code.eq.p .or. code.eq.blank) then
              print 300
300           format(1x,'what time is it on my clock ? (hh:mm)')
              call command(eof)
              if(eof .eq. 0) then
                  temp=1
                  utime=scan(temp)
                  if(utime .ne. 0) pelap=utime*60-cbegin*60
              endif
          endif
          if(code.eq.o .or. code.eq.blank) then
              print 400
400           format(1x,'what time is it on my opponent''s'
     *               ' clock ? (hh:mm)')
              call command(eof)
              if(eof .eq. 0) then
                  temp=1
                  utime=scan(temp)
                  if(utime .eq. 0) oelap=utime*60-cbegin*60
              endif
          endif
          return
c
c------------------------------< cld : display chess clock
c
500   continue
          d1=pelap+cbegin*60
          d2=oelap+cbegin*60
          if(d1 .lt. 0) d1=0
          if(d2 .lt. 0) d2=0
          print 600, hhmmss(pelap,8),hhmmss(oelap,8),npmovs,nomovs
          write(3,600) hhmmss(pelap,8),hhmmss(oelap,8),npmovs,nomovs
600       format(/8x,'blitz',6x,'opponent',
     *        /6x,a8,5x,a8,/5x,i3,' moves',4x,i3,' moves'/)
          return
c
c------------------------------< clq : set clock query flag
c
1000  continue
          cquery=xor(cquery,1)
          if(cquery .ne. 0) print 1100
          if(cquery .eq. 0) print 1200
          return
1100      format(1x,'clock query on.')
1200      format(1x,'clock query off.')
c
c------------------------------< cls : set surplus time
c
1300  continue
          col=5
          temp=scan(col)
          if(atext(4) .eq. equal) then
              surpls=temp
              if(surpls .lt. 0) then
                  print 1400
1400              format(1x,'time surplus cannot be negative.')
                  surpls=0
                  return
              endif
          endif
          print 1500, hhmmss(surpls,5)
          write(3,1500) hhmmss(surpls,5)
1500      format(1x,'time surplus is ',a5)
          return
c
c------------------------------< clt : set clock type (elapsed/cpu)
c
1600  continue
          col=5
          temp=scan(col)
          if(temp .ne. 0) then
              if(temp.ne.1 .and. temp.ne.2) then
                  print 1700
1700              format(1x,'enter (1) for cpu and (2) for elapsed.')
                  return
              endif
              cputim=0
              if(temp .eq. 1) cputim=1
          endif
          if(cputim .ne. 0) print 1800
          if(cputim .eq. 0) print 1900
          if(cputim .ne. 0) write(3,1800)
          if(cputim .eq. 0) write(3,1900)
          return
1800      format(1x,'clock is using cpu time.')
1900      format(1x,'clock is using elapsed time.')
      end
