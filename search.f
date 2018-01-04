      subroutine search
crev  last revision 03/30/91
c
c     ******************************************************************
c     *                                                                *
c     *      search is the driver for the basic look ahead procedure   *
c     *  used to select a move.  it is called by 'driver' after the    *
c     *  depth has been set to control the search.  search uses the    *
c     *  variable depth minimax search with alpha/beta backward        *
c     *  pruning as proposed by shannon.  the search is exhaustive     *
c     *  rather than selective with captures and certain types of      *
c     *  checking moves carried out to whatever depth is necessary     *
c     *  to obtain a quiesced position so that the terminal node       *
c     *  state scoring routine will not return bogus scores.           *
c     *      the basic search is iterated; that is, a complete search  *
c     *  is done with the basic depth at one, then a complete search   *
c     *  is done with the basic depth at two, three, ... until the     *
c     *  allotted move time is used or it becomes apparent that an     *
c     *  additional search will use too much time based on previous    *
c     *  search times.  each time a score/move is backed up to ply     *
c     *  one, that move is shuffled to the top of the move list to     *
c     *  consider first for the next iteration.                        *
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
c------------------------------< initialize multiprocessing stuff
c
      hashes=0
      phashs=0
      khashs=0
      pcalls=0
      nodes=0
      xnodes=dnodes
      cnodes=0
      value(-1)=window(1)
      value(0)=window(2)
      if(ply .gt. 0) go to 3000
      rdepth=depth
      status(1)=1
c
c------------------------------< advance to the next level.
c
100   continue
          ply=ply+1
          odepth(ply)=rdepth
          curmvs(ply,taskid)=0
          cutmvs(ply)=0
          side=-side
          player=3-player
          moveds(ply)=moveds(ply-2)
          value(ply)=value(ply-2)
          if(abort.ne.0 .or. timeup.ne.0) go to 7000
          if(stop(taskid) .ne. 0) go to 7100
          inchk(ply)=0
          givchk(ply)=0
          if(ply .ge. maxply) go to 5200
          which(ply)=0
          which(ply+1)=0
          phase(ply)=0
          status(ply)=0
          if(ply .gt. 1) then
              call lookup
              if(return .eq. 1) then
                  if(ply .le. tflag) print 101, taskid
101               format(1x,'pid',i3,' lookup found good score, done.')
                  go to 5200
              else if(return .eq. 2) then
                  if(ply .le. tflag) print 102, taskid
102               format(1x,'pid',i3,' lookup found a cutoff, done.')
                  go to 4300
              endif
          endif
          if(side .eq. 1) then
              call searcho
          else
              call searche
          endif
          if(return .ne. 0) go to 4000
3000      continue
          if(player.eq.1 .and. broke .ne. 0) call stats
          if(shareit(taskid) .ne. 0) then
               call share
          endif
          if(usplit(taskid) .ne. 0) then
              result=0
c             call lock22
              call unsplit
c             call unlock22
              if(result .eq. 1) go to 4000
          endif
          if(help(taskid) .ne. 0) then
              if(stop(taskid) .eq. 0) then
                  call split
              else
c                 call lock22
                  helpid=taskid+1
                  if(helpid .gt. ncpus) helpid=1
                  help(helpid)=20
                  help(taskid)=0
c                 call unlock22
              endif
          endif
          if(stop(taskid) .ne. 0) go to 7100
c
c------------------------------< call 'select' to select the next
c------------------------------< move for consideration. 'select'
c------------------------------< will return 1 when no further moves
c------------------------------< should be considered, 2 when out
c------------------------------< of time and 3 when no moves found.
c
          if(ply .eq. splitl(taskid)) then
              call selget
              call select
              call selput
          else
              call select
          endif
          if(return .eq. 1) go to 5000
          if(return .eq. 2) go to 7000
          if(return .eq. 3) go to 4300
          to(ply)=mto
          from(ply)=mfrom
          type(ply)=mtype
c
c------------------------------< perform trace analysis output if the
c------------------------------< tflag has been set.
c
          nodes=nodes+1
          if(ply .le. tflag) call searchp
          call make
c
c------------------------------< determine if this position has occurred
c------------------------------< earlier in this or previous searches.
c------------------------------< if so, consider it a drawn position
c------------------------------< and evaluate no deeper.
c
          if(repeat(0) .eq. 0) go to 100
              count=2
              mated(ply)=0
              ply=ply+1
              odepth(ply)=rdepth
              curmvs(ply,taskid)=0
              cutmvs(ply)=0
              player=3-player
              side=-side
              value(ply)=value(ply-2)
              go to 6100
c
c------------------------------< the move at the current level is
c------------------------------< so good that the move at the previous
c------------------------------< level would never be made. this
c------------------------------< is an alpha/beta cutoff. remember
c------------------------------< this move as a 'killer' if it is
c------------------------------< worthwhile and return to the previous
c------------------------------< level to try another move discarding
c------------------------------< the current move at that level.
c
4000  continue
          mated(ply-1)=0
4100  continue
          call killer2
4200  continue
          if(ply .eq. splitl(taskid)) then
              result=1
              call merge
              if(return .eq. 0) go to 7000
          endif
          rdepth=odepth(ply)
          call store2
4300  continue
          if(ply .eq. splitl(taskid)) then
              result=1
              call merge
              if(return .eq. 0) go to 7000
          endif
          rdepth=odepth(ply)
          ply=ply-1
          player=3-player
          side=-side
          mfrom=from(ply)
          mto=to(ply)
          mtype=type(ply)
          call unmake
          go to 3000
c
c------------------------------< the current ply has been completed
c------------------------------< without detecting a cutoff.  back up
c------------------------------< the best score/variation to the
c------------------------------< previous ply, clean up, and try
c------------------------------< the next move at the previous level.
c
5000  continue
          if(mated(ply) .ne. 0) go to 6000
          call killer1
5100  continue
          if(ply .eq. splitl(taskid)) then
              result=0
              call merge
              if(return .eq. 0) go to 7000
              if(result .eq. 1) go to 4100
          endif
          rdepth=odepth(ply)
          call store1
5200  continue
          if(ply .le. 1) return
          if(abort.ne.0 .or. timeup.ne.0) go to 7000
          call backup
          rdepth=odepth(ply)
          ply=ply-1
          player=3-player
          side=-side
          mfrom=from(ply)
          mto=to(ply)
          mtype=type(ply)
          call unmake
c
c------------------------------< check to see if the alpha/beta
c------------------------------< algorithm will cause a cutoff on
c------------------------------< here. briefly stated, if this move
c------------------------------< is better for the moving side than
c------------------------------< moves already found at previous nodes
c------------------------------< in the tree for the opposite side,
c------------------------------< then this is a 'refutation' type move
c------------------------------< which forces the move at the previous
c------------------------------< level to be discarded with no further
c------------------------------< analysis.
c
          if(ply .gt. 1) then
              if(side .gt. 0) then
                  if(value(ply) .ge. value(ply-1)) go to 4000
              else
                  if(value(ply) .le. value(ply-1)) go to 4000
              endif
          endif
          go to 3000
c
c------------------------------< no legal moves were generated for
c------------------------------< the current level.  it is either
c------------------------------< checkmate or stalemate.  set the
c------------------------------< correct score and return.
c
6000  continue
          count=0
6100  continue
          if(count.le.1 .and.
     *       attack(-side,kloc(player)).ne.0) go to 6200
c
c------------------------------< make a longer draw more
c------------------------------< attractive to the program.
c
          if(ply.le.tflag .and. count.eq.0) print 6110, taskid, ply
6110      format(20x,'stalemate, processor id ',i2,'  ply=',i2)
          if(ply.le.tflag .and. count.ne.0) print 6120, taskid, ply
6120      format(20x,'perpetual draw, processor id ',i2,'  ply=',i2)
          value(ply)=drawsc+ply
          go to 6300
c
c------------------------------< checkmate.
c
6200  continue
          if(ply.le.tflag) print 6210, taskid, ply
6210      format(20x,'checkmate, processor id ',i2,'  ply=',i2)
          value(ply)=-side*(1000000-ply)
c
c------------------------------< now determine if this score will
c------------------------------< cause a cutoff which refutes the
c------------------------------< move at the previous level.
c
6300  continue
          if(ply .gt. 1) then
              if(side .gt. 0) then
                  if(value(ply) .ge. value(ply-1)) go to 4200
              else
                  if(value(ply) .le. value(ply-1)) go to 4200
              endif
          endif
c
c------------------------------< current mate/draw score is
c------------------------------< acceptable. set the variation
c------------------------------< and continue.
c
          do 6800 level=1,ply-1
              trace(level,ply)=curmvs(level,taskid)
6800      continue
          trace(51,ply)=ply-1
          trace(52,ply)=depth
          go to 5100
c
c------------------------------< search was aborted. back out
c------------------------------< gracefully and return a.s.a.p.
c
7000  continue
              ply=ply-1
              if(ply .le. 0) return
              side=-side
              player=3-player
              mfrom=from(ply)
              mto=to(ply)
              mtype=type(ply)
              call unmake
              go to 7000
c
c------------------------------< this processor received a stop
c------------------------------< signal, back out gracefully.
c
7100  continue
      call searchmp
      if(return .ne. 0) go to 4000
      return
      end
      subroutine searche
crev  last revision 03/22/91
c
c     ******************************************************************
c     *                                                                *
c     *      searche is the body of the search used when the           *
c     *  opponent is on move.  it can be rewritten in assembly to      *
c     *  speed things up if desired.                                   *
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
 
c
c------------------------------< remember if the side on move is in
c------------------------------< check to speed up move rejection and
c------------------------------< allow for additional checks later.
c
      if(ply .gt. rdepth) then
c
c------------------------------< if the move made at the previous
c------------------------------< ply pushed a pawn to the 6th,
c------------------------------< 7th, or 8th rank.
c
          if(ply.le.2*rdepth .and. (type(ply-1).eq.0 .or.
     *       (type(ply-1).gt.3 .and. type(ply-1).lt.8))) then
              if(to(ply-1).gt.70 .and.
     *           board(to(ply-1)).eq.1) then
                  torank=cranks(to(ply-1))
                  tofile=cfiles(to(ply-1))
                  if(ofirst(tofile-1).le.torank .and.
     *               ofirst(tofile  ).le.torank .and.
     *               ofirst(tofile+1).le.torank) then
                      inchk(ply)=1
                      go to 1400
                  endif
              endif
          endif
c
c------------------------------< if beyond rdepth, inchk only needs
c------------------------------< be set if we are following a series
c------------------------------< of checks against the opposing side.
c
          start=rdepth+1
          if(2-and(start,1) .ne. 2) start=start+1
          lastply=ply-2
          do 700 level=start,lastply,2
              if(inchk(level) .eq. 0) go to 900
700       continue
          inchk(ply)=attack(1,kloc(2))
          if(inchk(ply) .ne. 0) go to 1400
          go to 900
      endif
c
c------------------------------< if the move at the previous ply
c------------------------------< captured a piece next to the
c------------------------------< king, treat that the same as a
c------------------------------< check.  if the move at the
c------------------------------< previous ply was a simple re-
c------------------------------< capture that restored the
c------------------------------< material balance, extend rdepth
c------------------------------< also.
c
      if(ply.gt.1 .and. cappc(ply-1).ne.0 .and.
     *   (iabs(cfiles(to(ply-1))-kfile(2)).le.1 .and.
     *    iabs(cranks(to(ply-1))-krank(2)).le.1)) then
          inchk(ply)=1
          rdepth=rdepth+1
      else
          inchk(ply)=attack(1,kloc(2))
          if(inchk(ply) .ne. 0) then
              rdepth=rdepth+1
          else if(ply.gt.2 .and.
     *            cappc(ply-2).ne.0 .and. cappc(ply-1).ne.0
     *            .and. to(ply-2).eq.to(ply-1) .and.
     *            mscore.eq.msave11(ply-2)) then
                   rdepth=rdepth+1
          endif
      endif
c
c------------------------------< zero the killer move counts so that
c------------------------------< killers found will be placed first
c------------------------------< although older ones will be kept.
c
      killmv(ply+1,3)=0
      killmv(ply+1,4)=0
      mated(ply)=1
      go to 3000
c
c------------------------------< if this is a terminal node, then the
c------------------------------< scoring function may need to be called
c------------------------------< to evaluate the positional/material
c------------------------------< advantages for each one.
c
900   continue
c
c------------------------------< now determine if the material score
c------------------------------< will cause an alpha/beta cutoff without
c------------------------------< calculating the positional score
c
          if(nopwns*nppwns*nppcs*nopcs .ne. 0) then
              tscore=mscore+minmax(2)
              if(tscore .le. value(ply-1)) go to 4000
          endif
          call score
c
c------------------------------< now determine if this score is
c------------------------------< good enough for the one to move
c------------------------------< so that the opposing one would
c------------------------------< never make the move leading to
c------------------------------< this position.
c
          if(tscore .le. value(ply-1)) go to 4000
c
c------------------------------< if this score is worse for the one
c------------------------------< to move than others already found,
c------------------------------< there is no need to remember the
c------------------------------< score or variation unless it is
c------------------------------< backed up later.
c
          if(tscore .lt. value(ply)) then
              value(ply)=tscore
              do 1300 level=1,ply-1
                  trace(level,ply)=curmvs(level,taskid)
1300          continue
              trace(51,ply)=ply-1
              trace(52,ply)=depth
          endif
1400      continue
c
c------------------------------< zero the killer move counts so that
c------------------------------< killers found will be placed first
c------------------------------< although older ones will be kept.
c
          killmv(ply+1,3)=0
          killmv(ply+1,4)=0
          mated(ply)=inchk(ply)
3000  continue
      return=0
      return
4000  continue
      return=4000
      return
      end
      subroutine searchmp
crev  last revision 03/22/91
c
c     ******************************************************************
c     *                                                                *
c     *      searchmp is called when parallel processing is being      *
c     *  used in the tree search and a processor receives a 'stop'     *
c     *  order from some other processor that is sharing a split       *
c     *  point with it.  the 'stop' order forces the current processor *
c     *  to stop, back up to the split point where the stop order      *
c     *  originated, and then either exit or process the cutoff if     *
c     *  this processor is the -last- one to make it back.             *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
c
c
      include 'common.f'
c
c
      include 'global.f'
c
c------------------------------< this processor received a stop
c------------------------------< request.  exit gracefully.
c
c     call lock22
      nstops(splitl(taskid))=nstops(splitl(taskid))+1
      exnodes(splitl(taskid))=exnodes(splitl(taskid))+nodes
      if(and(debug,512) .ne. 0) print 7105, taskid, stop(taskid), ply
7105  format(1x,'search   -  pid ',i2,'  stop=',i2,'  ply=',i2)
      splitl(taskid)=0
      block(taskid)=0
      if(stop(taskid) .eq. 1) tried(which(1))=0
7150  continue
          if(dlevel(ply,taskid) .ne. 0) then
              blockid=dlevel(ply,taskid)
              nbusy(blockid)=nbusy(blockid)-1
              dlevel(ply,taskid)=0
              if(nbusy(blockid) .gt. 0) then
                  do 7175 i=1,ply-1
                      if(dlevel(i,taskid) .ne. 0) then
                          blockid=dlevel(i,taskid)
                          dlevel(i,taskid)=0
                          nbusy(blockid)=nbusy(blockid)-1
                          if(nbusy(blockid) .eq. 0) then
                              alloc(blockid)=0
                              atply(blockid)=0
                          endif
                      endif
7175              continue
c                 call unlock22
                  return=0
                  return
              endif
              if(ply .eq. stop(taskid)) then
                  call copyg(blockid)
                  curmvs(ply,taskid)=cutmvs(ply)
              endif
              alloc(blockid)=0
              atply(blockid)=0
          endif
          if(ply .le. stop(taskid)) go to 7180
              ply=ply-1
              player=3-player
              side=-side
              mfrom=from(ply)
              mto=to(ply)
              mtype=type(ply)
              call unmake
      go to 7150
7180  continue
      block(taskid)=0
      splitl(taskid)=0
      do 7190 i=1,ply
           if(dlevel(i,taskid) .ne. 0) then
               splitl(taskid)=i
               block(taskid)=dlevel(i,taskid)
           endif
7190  continue
      stop(taskid)=0
c     call unlock22
      if(ply .gt. 1) then
          return=4000
      else
          return=0
      endif
      return
      end
      subroutine searcho
crev  last revision 03/22/91
c
c     ******************************************************************
c     *                                                                *
c     *      searcho is the body of the search used when the           *
c     *  program is on move.  it can be rewritten in assembly to       *
c     *  speed things up if desired.                                   *
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
 
c
c------------------------------< remember if the side on move is in
c------------------------------< check to speed up move rejection and
c------------------------------< allow for additional checks later.
c
      if(ply .gt. rdepth) then
c
c------------------------------< if the move made at the previous
c------------------------------< ply pushed a pawn to the 7th or 8th
c------------------------------< rank.
c
          if(ply.le.2*rdepth .and. (type(ply-1).eq.0 .or.
     *       (type(ply-1).gt.3 .and. type(ply-1).lt.8))) then
              if(to(ply-1).lt.50 .and.
     *           board(to(ply-1)).eq.-1) then
                  torank=cranks(to(ply-1))
                  tofile=cfiles(to(ply-1))
                  if(pfirst(tofile-1).ge.torank .and.
     *               pfirst(tofile  ).ge.torank .and.
     *               pfirst(tofile+1).ge.torank) then
                      inchk(ply)=1
                      go to 1400
                  endif
              endif
          endif
c
c------------------------------< if beyond rdepth, inchk only needs
c------------------------------< be set if we are following a series
c------------------------------< of checks against the opposing side.
c
          start=rdepth+1
          if(2-and(start,1) .ne. 1) start=start+1
          lastply=ply-2
          do 700 level=start,lastply,2
              if(inchk(level) .eq. 0) go to 900
700       continue
          inchk(ply)=attack(-1,kloc(1))
          if(inchk(ply) .ne. 0) go to 1400
          go to 900
      endif
c
c------------------------------< if the move at the previous ply
c------------------------------< captured a piece next to the
c------------------------------< king, treat that the same as a
c------------------------------< check.  if the move at the
c------------------------------< previous ply was a simple re-
c------------------------------< capture that restored the
c------------------------------< material balance, extend rdepth
c------------------------------< also.
c
c     if(ply.gt.1 .and. cappc(ply-1).ne.0 .and.
      if(ply .gt. 1) then
          if(cappc(ply-1).ne.0 .and.
     *       (iabs(cfiles(to(ply-1))-kfile(1)).le.1 .and.
     *        iabs(cranks(to(ply-1))-krank(1)).le.1)) then
              inchk(ply)=1
              rdepth=rdepth+1
          else
              inchk(ply)=attack(-1,kloc(1))
              if(inchk(ply) .ne. 0) then
                  rdepth=rdepth+1
              else if(ply.gt.2 .and.
     *                cappc(ply-2).ne.0 .and. cappc(ply-1).ne.0
     *                .and. to(ply-2).eq.to(ply-1) .and.
     *                mscore.eq.msave11(ply-2)) then
                       rdepth=rdepth+1
              endif
          endif
      endif
c
c------------------------------< zero the killer move counts so that
c------------------------------< killers found will be placed first
c------------------------------< although older ones will be kept.
c
      killmv(ply+1,3)=0
      killmv(ply+1,4)=0
      mated(ply)=1
      go to 3000
c
c------------------------------< if this is a terminal node, then the
c------------------------------< scoring function may need to be called
c------------------------------< to evaluate the positional/material
c------------------------------< advantages for each one.
c
900   continue
c
c------------------------------< now determine if the material score
c------------------------------< will cause an alpha/beta cutoff without
c------------------------------< calculating the positional score
c
          if(nopwns*nppwns*nppcs*nopcs .ne. 0) then
              tscore=mscore+minmax(1)
              if(tscore .ge. value(ply-1)) go to 4000
          endif
          call score
c
c------------------------------< now determine if this score is
c------------------------------< good enough for the one to move
c------------------------------< so that the opposing one would
c------------------------------< never make the move leading to
c------------------------------< this position.
c
          if(tscore .ge. value(ply-1)) go to 4000
c
c------------------------------< if this score is worse for the one
c------------------------------< to move than others already found,
c------------------------------< there is no need to remember the
c------------------------------< score or variation unless it is
c------------------------------< backed up later.
c
          if(tscore .gt. value(ply)) then
              value(ply)=tscore
              do 1300 level=1,ply-1
                  trace(level,ply)=curmvs(level,taskid)
1300          continue
              trace(51,ply)=ply-1
              trace(52,ply)=depth
          endif
1400      continue
c
c------------------------------< zero the killer move counts so that
c------------------------------< killers found will be placed first
c------------------------------< although older ones will be kept.
c
          killmv(ply+1,3)=0
          killmv(ply+1,4)=0
          mated(ply)=inchk(ply)
3000  continue
      return=0
      return
4000  continue
      return=4000
      return
      end
      subroutine searchp
crev  last revision 05/19/89
c
c     ******************************************************************
c     *                                                                *
c     *        searchp does a debug printout when tflag is set         *
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
      integer tab(30), fmt(12)
      data tab /'3','6','9','12','15','18','21','24','27','30',
     * '33','36','39','42','45','48','51','54','57','60','63','66',
     * '69','72','75','78','81','84','87','90'/
      data fmt /'(      t', '10', ',i2,1x,', '8a1,2x,', '''phase'',',
     *'i2,''('',', 'i2,'')'',', '3x,i8,i9', ',2x,''pid', ''',i3,2x,''',
     *'''rd='',i2', ',i3)'/
c     call lock22
      call output(0,0)
      fmt(2)=tab(ply)
      statply=status(ply)
      if(statply .gt. 99) statply=ishft(statply,-16)
      print fmt, ply,(text(ix),ix=1,8), phase(ply), statply,
     *             value(ply), nodes, taskid, rdepth, onerep(ply)
c     call unlock22
      return
      end
