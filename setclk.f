      subroutine setclk
crev  last revision 10/27/91
c
c     ******************************************************************
c     *                                                                *
c     *      setclk is used to inform the operator whenever a new      *
c     *  phase of the game is reached, resulting in a reduced time     *
c     *  per move.  if the time per move is 5 seconds or less, the     *
c     *  verbosity of the status report is also reduced.               *
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
      equivalence (alphad,alpha(30)),(aster,alpha(66))
      data mm / 0 /
c
c
c------------------------------< if not at the preset time limit,
c------------------------------< return without resetting the clocks.
c
      if(timlim(5) .eq. 0) then
          if(npmovs .eq. timlim(1)) then
              timlim(1)=timlim(1)+timlim(3)
              timlim(2)=timlim(2)+timlim(4)
              cntrls=cntrls+1
              print 100, cntrls
100           format(/1x,'time control',i3,' reached.'/)
              write(3,100) cntrls
              atext(3)=alphad
              atext(4)=aster
              call ccmnd
          endif
      else
          if(pelap.lt.timlim(2)-3*timlim(1)) then
              if(mm .eq. 0) then
                  tlim=timlim(1)
                  print 101, hhmmss(timlim(1),5)
101               format(1x,'time control phase one reached (',
     *                   a5,' per move).')
                  mm=1
              endif
          elseif(pelap.lt.timlim(2)+timlim(4)-5*timlim(3)) then
              if(mm .eq. 1) then
                  mm=2
                  tlim=timlim(3)
                  print 200, hhmmss(timlim(3),5)
200               format(1x,'time control phase two reached (',
     *                   a5,' per move).')
              endif
          elseif(mm .eq. 2) then
              mm=3
              tlim=timlim(5)
              print 300, hhmmss(timlim(5),5)
300           format(1x,'time control phase three reached (',
     *                   a5,' per move).')
          endif
      endif
      return
      end
