      subroutine select
crev  last revision 05/11/91
c
c     ******************************************************************
c     *                                                                *
c     *      select is used to select the next move to be examined by  *
c     *  'search'.  it is also called for and must properly handle the *
c     *  case where ply = divide.  these phase modules have to do some *
c     *  additional work to protect the multiple processors from each  *
c     *  other at critical times.  the move selection process is       *
c     *  executed in several phases:                                   *
c     *                                                                *
c     *      phase0 : try the null move and search one ply less than   *
c     *              normal.  If this doesn't let the opponent do      *
c     *              something bad, then he has no threats at all.     *
c     *              note that certain constraints apply that might    *
c     *              not allow the null move to be introduced.  see    *
c     *              phase0 for details.                               *
c     *                                                                *
c     *      phase1 : if this position was found in the transposition  *
c     *              hash table, try the suggested move that was       *
c     *              stored with it.  this move was found to be the    *
c     *              best in this position either during this          *
c     *              iteration or during a previous iteration.  it is  *
c     *              similar to a killer move but is a killer for      *
c     *              this particular position, rather than for this    *
c     *              particular ply.                                   *
c     *                                                                *
c     *      phase2 : try all captures that do not seem to be out-     *
c     *              right material loses. ie, pxn is safe, but qxr    *
c     *              is safe only if the rook is undefended.           *
c     *                                                                *
c     *      phase3 : try the 'killer' moves to see if any of them     *
c     *              are legal.  these moves have been found to be     *
c     *              good at other descendants of the same parent      *
c     *              node that this position has and should be tried   *
c     *              first to hopefully force an alpha/beta cutoff.    *
c     *                                                                *
c     *      phase4 : try the rest of the moves in more or less        *
c     *              random order.                                     *
c     *                                                                *
c     *      phase5 : try the level 1 move list moves in the order in  *
c     *              which they appear in the list.                    *
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
c------------------------------< ply 1 moves require special
c------------------------------< handling.
c
      if(ply .eq. 1) go to 1200
c
c------------------------------< if this is the first 'select' call
c------------------------------< for this node, set up for phase0;
c------------------------------< otherwise, zero the last move examined
c------------------------------< so that is will not be considered
c------------------------------< again later.
c
      if(which(ply) .ne. 0) then
          if(phase(ply) .eq. 4) then
              call phase4
              if(return .eq. 0) go to 9999
          else
              if(phase(ply) .eq. 3) go to 1000
              if(phase(ply) .eq. 2) go to 800
              if(phase(ply) .eq. 1) go to 300
              if(phase(ply) .eq. 0) go to 200
          endif
          go to 9998
      endif
c
c------------------------------< this is the first call to select
c------------------------------< from this level.  fall into phase0.
c------------------------------< unless we are re-searching without
c------------------------------< the null move (phase(ply)=1).
c------------------------------< first, determine if it is time to 
c------------------------------< check the clock.
c
      if(nodes .ge. xnodes) then
          call select1
          if(return .ne. 0) then
              curmvs(ply,taskid)=0
              return=2
              return
          endif
      endif
c
c******************************<
c******************************< phase 0
c******************************<
c
200   continue
          if(phase(ply).eq.0 .and. xnullm.ne.0) then
              if(ply.le.rdepth-xnullm .and. status(ply).eq.0) then
                  first(ply)=last(ply-1)+1
                  last(ply)=first(ply)
                  call phase0
                  if(return .eq. 0) go to 9999
              endif
              phase(ply)=1
              status(ply)=0
          endif
c
c******************************<
c******************************< phase 1
c******************************<
c
300   continue
          if(status(ply) .eq. 0) then
              first(ply)=last(ply-1)+1
              last(ply)=first(ply)
              if(hmove(ply) .ne. 0) then
                  call phase1
                  if(return .eq. 0) go to 9999
                  hmove(ply)=0
              endif
          endif
c
c------------------------------< if the side on move is already in
c------------------------------< check, then the search for this level
c------------------------------< is exhaustive anyway, skip mater.
c
          if(ply.gt.rdepth .and. inchk(ply).eq.0) call mater
          call movgen
          if(return .ne. 0) go to 9997
              mated(ply-1)=0
              if(first(ply) .gt. last(ply)) go to 9998
                  if(hmove(ply) .ne. 0) then
                      do 500  i=first(ply),last(ply)
                          if(moves(i) .eq. hmove(ply)) then
                              moves(i)=0
                              go to 600
                          endif
500                   continue
600                   continue
                  endif
                  phase(ply)=2
                  status(ply)=0
c
c******************************<
c******************************< phase 2
c******************************<
c
800   continue
          if(status(ply) .eq. 0) call phase2
850   continue
          status(ply)=status(ply)-1
          if(status(ply) .gt. 0) then
              which(ply)=which(ply)+1
              if(which(ply) .gt. last(ply)) go to 900
              if(moves(which(ply)) .eq. 0) go to 850
              mfrom=extrct(moves(which(ply)))
              return=0
              go to 9999
          endif
900       continue
              phase(ply)=3
              status(ply)=ishft(ply,16)
              if(ply .le. rdepth) go to 1000
              if(inchk(ply) .ne. 0) go to 1000
              if(givchk(ply) .ne. 0) go to 1100
                  go to 9998
c
c******************************<
c******************************< phase 3
c******************************<
c
1000  continue
          call phase3
          if(return .eq. 0) go to 9999
1100      continue
              phase(ply)=4
              status(ply)=0
c
c******************************<
c******************************< phase 4
c******************************<
c
          call phase4
          if(return .eq. 0) go to 9999
          go to 9998
c
c******************************<
c******************************< phase 5
c******************************<
c
1200  continue
          call phase5
          if(return .ne. 0) go to 9998
c
c------------------------------< a phase 5 move was found, return 
c------------------------------< to examine it.
c
          curmvs(ply,taskid)=moves(which(ply))
          return=0
          return
c
c------------------------------< move was rejected by movgen as
c------------------------------< illegal. return to select another
c------------------------------< at the previous level.
c
9997  continue
          curmvs(ply,taskid)=0
          return=3
          return
c
c------------------------------< no moves were found, return to
c------------------------------< indicate search is complete.
c
9998  continue
          phase(ply)=9
          curmvs(ply,taskid)=0
          return=1
          return
c
c------------------------------< a move was found, return to examine
c------------------------------< it.
c
9999  continue
          curmvs(ply,taskid)=moves(which(ply))
          moves(which(ply))=0
          return
      end
      subroutine select1
crev  last revision 06/10/90
c
c     ******************************************************************
c     *                                                                *
c     *      select1 performs the time check every so often to         *
c     *  determine if it is time to stop searching.                    *
c     *                                                                *
c     *      select1 measures two distinct intervals:  (1) the time    *
c     *  since the search started and (2) the time since the opponent  *
c     *  made a move.  if not pondering, these are the same.           *
c     *                                                                *
c     *      if the program does not see itself in trouble (noted by   *
c     *  an evaluation that is lower by a significant amount over the  *
c     *  previous iteration, and no fail lows have occurred, then it   *
c     *  will use the total search time and compare it against the     *
c     *  target.  this will allow pondering to save time and build up  *
c     *  a surpls.  this interval is psec2-fsec1.                      *
c     *                                                                *
c     *      if the program believes that it is in some type of        *
c     *  trouble (either losing material or the positional score has   *
c     *  dropped significantly) then it only measures the time it has  *
c     *  spent searching "while it's clock is running" to give it a    *
c     *  little more time to solve the problem.  this interval is      *
c     *  psec2-psec1.                                                  *
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
      data cursup / x'1b5b410000000000' /
c
c------------------------------< determine if another ply n move
c------------------------------< can be considered without blowing
c------------------------------< the clock time.  if a pawn or piece
c------------------------------< is being lost, additional time can
c------------------------------< be used to attempt to save it. if
c------------------------------< checkmate is forced, a lot of time
c------------------------------< will be used to determine if there
c------------------------------< is any way out.
c
      xnodes=nodes+dnodes
      call cptime(psec2,msec2)
      if(cputim .ne. 0) psec2=msec2
      if(rnodes+nodes.ge.snodes .and. autos.ne.0 .and.
     *   vttype.eq.2 .and. taskid.eq.1) then
          mtime=(psec2-fsec1)/100
          if(burp+15 .le. mtime) then
c             call lock22
              if(burp+15 .le. mtime) then
                  burp=mtime
                  print 100, depth, hhmmss(mtime,5), cursup
100               format(17x,i3,4x,a5,a4)
              endif
c             call unlock22
          endif
      endif
c
c------------------------------< if searching to a forced depth,
c------------------------------< ignore timing controls.
c
      if(fdepth .ne. 0) go to 9999
c
c------------------------------< atlim is the absolute upper
c------------------------------< time limit that we can spend
c------------------------------< if we have to "overflow" to
c------------------------------< solve some problem.
c
      if(timlim(5) .ne. 0) then
          if(avgtim .le. timlim(1)) then
              atlim=(timlim(2)-pelap)*100
          else if(avgtim .le. timlim(3)) then
              atlim=(timlim(2)+timlim(4)-pelap)*100
          else
              atlim=5*avgtim
          endif
      else
          atlim=timlim(2)-pelap-surpls
          atlim=atlim*100
      endif
c
c------------------------------< if doing a true valued search,
c------------------------------< don't extend on time as some
c------------------------------< moves will be "real bad"
c
      if(trueval .ne. 0) then
          if(psec2-fsec1 .lt. avgtim) go to 9999
          if(depth .gt. 2) go to 9998
c
c------------------------------< if we are pondering and our
c------------------------------< opponent hasn't made a move,
c------------------------------< continue searching.
c
      else if(smode .eq. 0) then
          if(pndrng.ne.0 .and. matchd.eq.0) go to 9999
c
c------------------------------< if there is only one legal move,
c------------------------------< only spend a few seconds to get
c------------------------------< something to ponder.  otherwise,
c------------------------------< continue until using at least
c------------------------------< the target time.
c
          if(last(1).eq.1 .and. psec2-fsec1.gt.300) go to 9998
c
c------------------------------< if "easy" is set, then the first
c------------------------------< move is a pawn better than the
c------------------------------< rest of the ply-1 moves.  use
c------------------------------< only 1/3 of the target time and
c------------------------------< quit as the move is "obvious"
c------------------------------< if "easy" is not set, then use
c------------------------------< at least the regular target time.
c
          tavgtim=avgtim
          if(easy .ne. 0) tavgtim=tavgtim/3
          if(psec2-fsec1 .lt. tavgtim) go to 9999
c
c------------------------------< if we are "stuck" on the first
c------------------------------< move of an iteration, and we
c------------------------------< haven't failed low, then we can
c------------------------------< stop and report our move.
c
          if(value(1).eq.window(1) .and.
     *       (window(1).eq.eval-250 .or.
     *        (window(1).eq.drawsc-1 .and. 
     *         window(2).eq.drawsc+101))) go to 9998           
c
c------------------------------< if the eval has dropped significantly
c------------------------------< since the last iteration, use more
c------------------------------< time.  often, the first (best) move
c------------------------------< will be worse, but another move
c------------------------------< will regain the lost positional
c------------------------------< score, but only if we take the time
c------------------------------< to search it.
c
          if((value(1).ne.window(1) .or. raised.ne.0)
     *        .and. value(1)+200 .le. eval) then
              if(psec2-fsec1 .lt. min0(avgtim*2,atlim)) go to 9999
          endif
c
c------------------------------< if the search has failed low during
c------------------------------< the current iteration, and the value
c------------------------------< is still unknown, double the target
c------------------------------< time so that we don't make a really
c------------------------------< poor positional move that still will
c------------------------------< avoid losing material.  try to find a
c------------------------------< more that saves the material and the
c------------------------------< position.
c
          if(value(1).eq.window(1) .and. lowered.ne.0 .and.
     *       value(1).lt.eval) then
              if(psec2-fsec1 .lt. min0(avgtim*2,atlim)) go to 9999
          endif
c
c------------------------------< check to see if a lower bound
c------------------------------< cutoff has occurred.  if so,
c------------------------------< more time should be used to
c------------------------------< try to find a better move if
c------------------------------< possible.
c
          if(value(1).eq.window(1) .and. window(1).lt.eval-250) then
              if(window(1).le.eval-500 .and.
     *            psec2-psec1 .lt. min0(avgtim*3,atlim)) go to 9999
              if(window(1).le.eval-1500 .and.
     *            psec2-psec1 .lt. min0(avgtim*5,atlim)) go to 9999
              if(window(1).le.eval-8500 .and.
     *            psec2-psec1 .lt. min0(avgtim*6,atlim)) go to 9999
                  go to 9998
          endif
c
c------------------------------< if getting mated, much more time
c------------------------------< should be used to avoid it if
c------------------------------< possible.
c
          if(value(1).le.-900000 .and. value(1).gt.-1500000) then
              if(psec2-psec1 .gt. min0(avgtim*10,atlim)) go to 9998
              go to 9999
          endif
c
c------------------------------< if losing a pawn, additional time
c------------------------------< should be used to save it.
c
          if(value(1)+500 .gt. eval) go to 9998
          if(psec2-psec1 .lt. min0(avgtim*3,atlim)) go to 9999
c
c------------------------------< if losing a piece, even more time
c------------------------------< should be used to save it.
c
          if(value(1)+2000  .gt. eval) go to 9998
              if(psec2-psec1 .lt. min0(avgtim*5,atlim)) go to 9999
c
c------------------------------< out of time, abort the current
c------------------------------< search.
c
9998      continue
              if(trueval .eq. 0) then
                  write(3,99981) value(1), window(1), eval,
     *                           lowered, raised, easy
99981             format(1x,'value(1), window(1), eval, lowered,',
     *                   ' raised, easy=',6i9)
              endif
              timeup=1
              return=2
              return
      endif
c
c------------------------------< time remains, continue the search.
c
9999  continue
          return=0
          return
      end
      subroutine selget
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      selget is used to copy moves from the permanent common    *
c     *  to the local or private sub-task common.                      *
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
      include 'perm.f'
c
c
      blockid=block(taskid)
c     call lock23
      first(ply)=qdata(1149+ply,blockid)
      last(ply)=qdata(1399+ply,blockid)
      hmove(ply)=qdata(1299+ply,blockid)
      phase(ply)=qdata(2151+ply,blockid)
      status(ply)=qdata(2201+ply,blockid)
      which(ply)=qdata(5003+ply,blockid)
      inchk(ply)=qdata(1349+ply,blockid)
      givchk(ply)=qdata(1249+ply,blockid)
      do 300 i=qdata(1149+ply,blockid),qdata(1399+ply,blockid)
          moves(i)=qdata(5599+i,blockid)
300   continue
      return
      end
      subroutine selput
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      selput is used to copy moves to the permanent common      *
c     *  from the local or private sub-task common.                    *
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
      include 'perm.f'
c
c
      blockid=block(taskid)
      qdata(1149+ply,blockid)=first(ply)
      qdata(1399+ply,blockid)=last(ply)
      qdata(1299+ply,blockid)=hmove(ply)
      qdata(2151+ply,blockid)=phase(ply)
      qdata(2201+ply,blockid)=status(ply)
      qdata(5003+ply,blockid)=which(ply)
      qdata(1349+ply,blockid)=inchk(ply)
      qdata(1249+ply,blockid)=givchk(ply)
      do 100 i=first(ply),last(ply)
          qdata(5599+i,blockid)=moves(i)
100   continue
c     call unlock23
      return
      end
