      subroutine iterate
crev  last revision 03/30/91
c
c     ******************************************************************
c     *                                                                *
c     *      iterate is used to perform the iterated search.  it       *
c     *  uses the ply one move list already produced and executes the  *
c     *  iterative search process.  it does a complete one ply ex-     *
c     *  haustive search, then a complete two ply exhaustive search,   *
c     *  and so forth until it has used all available time.            *
c     *      each new iteration uses the same ply one move list which  *
c     *  is re-ordered by 'backup' each time a new best move is backed *
c     *  up to ply one.  in this way, the program 'learns' more about  *
c     *  the position with each iteration and also gets maximum        *
c     *  benefits from the alpha/beta algorithm due to looking at      *
c     *  the best moves first.  it should be noted that the killer     *
c     *  moves and transposition table are kept across iterations      *
c     *  (and even across searches) so that continuity from one depth  *
c     *  to the next is high.                                          *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      real running, factor, npr
c
c
      include 'global.f'
c
c
      include 'common.f'
c
c
      integer boardx(120)
c
c
c------------------------------< initialize.
c
      burp=0
      broke=0
      timeup=0
      xnodes=dnodes
c
c------------------------------< evaluate the material on the board
c------------------------------< for the current position.
c
      call locate
c
c------------------------------< set the piece/square scoring
c------------------------------< tables now.
c
      call prescore
c
c------------------------------< initialize for the minimax search
c------------------------------< procedure.
c
      window(1)=-1999999
      window(2)= 1999999
c
c------------------------------< insert the principle variation
c------------------------------< in the transposition table so that
c------------------------------< phase0 will find them first.
c
300   continue
          if(trueval .eq. 0) then
              tdebug=debug
              debug=and(debug,65531)
              side=1
              player=1
              ply=1
              value(-1)=-1999999
              value(0) = 1999999
              if(and(trace(51,1),65535) .ge. 2) then
                  do 400 i=1,and(trace(51,1),65535)
                      trace(i,i)=trace(i,1)
                      trace(51,i)=trace(51,1)
                      trace(52,i)=trace(52,1)
400               continue
                  if(iabs(value(1)).ge.900000 .and. 
     *               iabs(value(1)).le.1500000) 
     *                     value(1)=value(1)+isign(2,value(1))
                  v1=value(1)
500               continue
                      rdepth=depth
                      if(ply .le. and(trace(51,1),65535)-1) then
                          mfrom=extrct(trace(ply,1))
                          to(ply)=mto
                          from(ply)=mfrom
                          type(ply)=mtype
                          cappc(ply)=mcappc
                          call make
                          value(ply)=value(ply-2)
                          ply=ply+1
                          player=2-and(ply,1)
                          side=-side
                          value(ply)=v1
                          call store1
                          go to 500
                      endif
600                   continue
                      ply=ply-1
                      if(ply .gt. 0) then
                          player=2-and(ply,1)
                          side=-side
                          mfrom=extrct(trace(ply,1))
                          call unmake
                          go to 600
                      endif
              endif
              debug=tdebug
          endif
c
c------------------------------< zero the window adjustment values
c------------------------------< to enable the multiple-tiered
c------------------------------< window expansion.
c
          raised=0
          lowered=0
c
c------------------------------< increment the basic search depth for
c------------------------------< this iteration.
c
          depth=depth+1
c
c------------------------------< if we have done a two ply search,
c------------------------------< re-enable the hash table so that
c------------------------------< it will help move ordering.  it was
c------------------------------< turned off to produce a predicted
c------------------------------< move for pondering.
c
          if(depth .gt. maxply-2) go to 2000
          if(tflag.gt.0 .and. trueval.eq.0) print 700, depth
700       format(1x,'*'/1x,'* search depth ',i2/1x,'*')
          if(tflag.gt.0 .and. trueval.ne.0) print 800, depth
800       format(1x,'*'/1x,'* true value search depth ',i2/1x,'*')
c
c------------------------------< call search to analyze the current
c------------------------------< board position.
c
          if(depth.ne.1 .and. trueval.eq.0) then
              window(1)=eval-250
              window(2)=eval+250
              if(eval.ge.drawsc .and. eval.lt.drawsc+100) then
                  window(1)=drawsc-1
                  window(2)=drawsc+101
              endif
              if(eval.gt.drawsc+100 .and.
     *           window(1).lt.drawsc+100) window(1)=window(1)-100
              if(eval.le.drawsc .and.
     *           window(2).gt.drawsc)     window(2)=window(2)+100
          endif
          mated(1)=1
c
c------------------------------< initialize for search.
c
900       continue
          which(1)=0
          do 1000 i=1,200
              tried(i)=0
              order(i)=0
1000      continue
1100      continue
              ply=0
              side=-1
              player=2
              do 1300 i=1,16
                  do 1200 j=1,50
                      dlevel(j,i)=0
1200              continue
                  alloc(i)=0
                  stopping(i)=0
                  stop(i)=0
                  nbusy(i)=0
1300          continue
              xnodes=dnodes
              call control(1)
c
c------------------------------< determine if the window was too
c------------------------------< narrow. if so, extend the edge
c------------------------------< that caused the failure.
c
              if(abort.eq.0 .and. timeup.eq.0 .and. trueval.eq.0) then
                  if(value(1) .le. window(1)) then
                      if(raised .eq. 0) then
                          call faillo
                          go to 900
                      endif
                  else if(value(1) .ge. window(2)) then
                      call failhi
                      if(return .eq. 0) go to 1100
                  endif
              endif
c
c------------------------------< search returned a value within the
c------------------------------< proper bounds.
c
          if(value(1) .gt. window(1)) eval=value(1)
          ply=1
          player=1
          side=1
          mfrom=extrct(trace(1,1))
          call make
          if(abort.eq.0 .and. timeup.eq.0 .and. autos.gt.2) then
              if(trueval .eq. 0) then
                  call inform('->')
              else
                  if(rnodes+nodes .ge. snodes) then
                      call cptime(psec2,msec2)
                      if(cputim .ne. 0) psec2=msec2
                      itime=(psec2-fsec1)/100
                      print 1400, depth,
     *                           hhmmss(itime,5)
1400                  format(17x,i3,'->',2x,a5)
                  endif
              endif
          endif
          ply=1
          player=1
          side=1
          mfrom=extrct(trace(1,1))
          call unmake
          call cptime(psec2, msec2)
          if(cputim .ne. 0) psec2=msec2
          if(abort.ne.0 .or. timeup.ne.0) go to 2000
          itime=(psec2-fsec1)/100
          dnodes=20000
c
c------------------------------< if checkmate has been found, there is
c------------------------------< no need to continue the search since
c------------------------------< the mate is forced at the current
c------------------------------< depth.
c
          if(value(1).lt.-900000
     *       .and. value(1).gt.window(1)) go to 2000
          if(vterm.lt.4000000 .and. value(1).ge.vterm) go to 2000
          if(vterm .ne. 4000000) go to 1900
          if(sortp1.ne.0 .and. depth.eq.2) go to 2000
          if(value(1) .lt. 900000) go to 1900
              if(value(1) .gt. peval) go to 2000
1900      continue
c
c------------------------------< another iteration can be started
c------------------------------< if the total time used so far
c------------------------------< does not exceed the amount of time
c------------------------------< the program must average.
c
          if(sortp1.ne.0 .and. smode.ne.0) go to 2000
          if(fdepth .lt. 60) then
              if(fdepth .gt. depth)  go to 300
          else
              if(psec2-psec1 .lt. fdepth) go to 300
          endif
          if(pndrng.ne.0 .and. matchd.eq.0) go to 300
          if(fdepth .ne. 0) go to 2000
          tavgtim=avgtim
          if(easy .ne. 0) tavgtim=tavgtim/3
          if(psec2-psec1 .lt. tavgtim) go to 300
      go to 300
2000  continue
      return
      end
