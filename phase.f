      subroutine phase0
crev  last revision 02/11/91
c
c     ******************************************************************
c     *                                                                *
c     *     phase0 is used to select the null move for the null-move   *
c     *  heuristic.  this operates as follows.  instead of making a    *
c     *  legal move, the side on move 'passes' and does nothing.  the  *
c     *  resulting position is searched to a shallower depth than      *
c     *  normal (usually one ply less but settable by the operator)    *
c     *  this should result in a cutoff or at least should set the     *
c     *  lower bound better since anything should be better than not   *
c     *  doing anything.                                               *
c     *                                                                *
c     *     this is skipped for any of the following reasons:          *
c     *                                                                *
c     *     1.  the side on move is in check.  the null move results   *
c     *         in an illegal position.                                *
c     *     2.  ply=1 since not moving is not a real choice that the   *
c     *         program can choose to make!                            *
c     *     3.  two null moves can not appear consecutively in any     *
c     *         sequence of moves.                                     *
c     *     4.  the side on move has little material left making       *
c     *         zugzwang positions more likely.                        *
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
c------------------------------< if we should try the null move,
c------------------------------< stuff it in the move list and return
c------------------------------< to examine it.
c
      if(inchk(ply) .lt. 0) go to 9998
      if(phase(ply-1) .eq. 0) go to 9998
      if(nppcs.le.18 .or. nopcs.le.18) go to 9998
      moves(first(ply))=16383
      which(ply)=first(ply)
      mtype=8
      mcappc=0
      movgpc=0
      mpropc=0
      mfrom=0
      mto=0
      status(ply)=1
      return=0
      return
c
c------------------------------< no null move here
c
9998  continue
      return=1
      return
      end
      subroutine phase1
crev  last revision 09/14/91
c
c     ******************************************************************
c     *                                                                *
c     *     phase1 is used to select the move retrieved from the       *
c     *  transposition table.  this move was the best move for this    *
c     *  position, stored from an earlier analysis of this position.   *
c     *  the principle advantage of using this move is that no move    *
c     *  generation is required, saving a fair amount of time.  if     *
c     *  the suggested move is any good, a cutoff will occur which     *
c     *  allows the search to examine this branch without investing    *
c     *  the time to generate all moves, and using only one.           *
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
      data bad / 0 /
c
c
c------------------------------< unpack the table move.
c
      status(ply)=1
c     mfrom=extrct(hmove(ply))
      mtype=and(ishft(hmove(ply),-14),7)
      mcappc=and(ishft(hmove(ply),-17),7)
      mto=and(ishft(hmove(ply),-7),127)
      movgpc=ishft(hmove(ply),-20)
      mpropc=mtype-2
      if(mtype .lt. 4) mpropc=0
      mfrom=and(hmove(ply),127)
      if(mto .gt. 99) then
          mtype=8
          mcappc=0
          mto=0
          movgpc=0
          mpropc=0
          mfrom=0
      endif
c
c------------------------------< if in the quiescence search, don't
c------------------------------< look at a non-capturing move from
c------------------------------< the table as it will waste a lot
c------------------------------< of time.  some non-capturing moves
c------------------------------< should be considered, but they will
c------------------------------< be picked up later anyway. an ex-
c------------------------------< ception occurs when following forcing
c------------------------------< lines.  if in check or giving check,
c------------------------------< non-captures are ok to try.
c
      if(ply .gt. rdepth) then
          if(mcappc+mpropc .eq. 0) then
              if(movgpc .eq. 1) then
                  if(player.eq.1 .and. mto.gt.80) go to 200
                  if(player.eq.2 .and. mto.lt.40) go to 200
              endif
              if(inchk(ply).eq.0 .and. givchk(ply).eq.0) go to 9998
          endif
      endif
200   continue
      moves(first(ply))=hmove(ply)
      which(ply)=first(ply)
c
c------------------------------< make sure that the move suggested
c------------------------------< by the table is legal.
c
      go to (300,400,700,1000,300,300,300,300),mtype+1
c
c------------------------------< normal/pawn promotion moves.
c
300   continue
          if(board(mfrom) .ne. side*movgpc) go to 1100
          if(iabs(board(mto)) .ne. mcappc) go to 1100
              go to 9999
c
c------------------------------< castle king-side moves.
c
400   continue
          if(movedr(player) .eq. 0) go to 1100
          if(moveds(ply) .eq. 0) go to 1100
          bias=cbias(player)
          if(color .eq. 1) then
              if(board(bias+23) .ne. 0) go to 1100
              if(board(bias+24) .ne. 0) go to 1100
              if(and(movedr(player),1) .eq. 0) go to 1100
              if(and(moveds(ply),1) .eq. 0) go to 1100
          else
              if(board(bias+28) .ne. 0) go to 1100
              if(board(bias+27) .ne. 0) go to 1100
              if(and(movedr(player),2) .eq. 0) go to 1100
              if(and(moveds(ply),2) .eq. 0) go to 1100
          endif
          go to 9999
c
c------------------------------< castle queen-side moves.
c
700   continue
          if(movedr(player) .eq. 0) go to 1100
          if(moveds(ply) .eq. 0) go to 1100
          bias=cbias(player)
          if(color .eq. 1) then
              if(board(bias+26) .ne. 0) go to 1100
              if(board(bias+27) .ne. 0) go to 1100
              if(and(movedr(player),2) .eq. 0) go to 1100
              if(and(moveds(ply),2) .eq. 0) go to 1100
          else
              if(board(bias+24) .ne. 0) go to 1100
              if(board(bias+25) .ne. 0) go to 1100
              if(and(movedr(player),1) .eq. 0) go to 1100
              if(and(moveds(ply),1) .eq. 0) go to 1100
          endif
          go to 9999
c
c------------------------------< en passant pawn captures
c
1000  continue
          if(board(mfrom) .ne. side) go to 1100
          if(board(mto) .ne. 0) go to 1100
          if(board(mto-10*side) .ne. -side) go to 1100
              go to 9999
c
c------------------------------< move is illegal which indicates
c------------------------------< that the hashing algorithm allowed
c------------------------------< two different positions to hash to
c------------------------------< the same hash key.  inform the
c------------------------------< operator and ignore the move.
c
1100  continue
          bad=bad+1
          if(bad .lt. 25) then
              print 1200, taskid, bad, ply
1200          format(/1x,'pid',i3,' bad move hashed',i4,' ply=',i2)
              if(and(debug,2) .ne. 0) then
                  text(2)=' '
                  call dcmnd
                  call output(0,0)
                  print 1300, (text(i),i=1,7)
1300              format(1x,'bad move is ',7a1)
              endif
          endif
c
c------------------------------< now zap the bad entry
c
          htable(rkey1)=0
c
c------------------------------< return indicating that no move
c------------------------------< was found, or if one was found,
c------------------------------< it was not legal.
c
9998  continue
          return=1
          return
c
c------------------------------< move retrieved from the hash
c------------------------------< table was legal and should be
c------------------------------< tried. return.
c
9999  continue
          mated(ply-1)=0
          return=0
          return
      end
      subroutine phase2
crev  last revision 04/06/91
c
c     ******************************************************************
c     *                                                                *
c     *      phase2 is called to select captures which seem to gain    *
c     *  material.  the captures are ordered based on the value of     *
c     *  the captured piece.  'ripoff' is called to analyze the        *
c     *  attackers and defenders of each piece that can be captured    *
c     *  to determine the expected gain (or loss).                     *
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
c------------------------------< first we wish to scan the move
c------------------------------< list to determine if there are any
c------------------------------< captures which seem to win material.
c
      legals=0
      count=0
      where=first(ply)-1
100   continue
          where=where+1
          if(where .gt. last(ply)) go to 500
          if(moves(where) .eq. 0) then
              legals=legals+1
              go to 100
          endif
          pushpp=0
c
c------------------------------< if in check, determine if there
c------------------------------< are at least two legal moves to
c------------------------------< escape which avoids an extension.
c
c         mfrom=extrct(moves(where))
          mtype=and(ishft(moves(where),-14),7)
          mcappc=and(ishft(moves(where),-17),7)
          mto=and(ishft(moves(where),-7),127)
          movgpc=ishft(moves(where),-20)
          mpropc=mtype-2
          if(mtype .lt. 4) mpropc=0
          mfrom=and(moves(where),127)
          if(mto .gt. 99) then
              mtype=8
              mcappc=0
              mto=0
              movgpc=0
              mpropc=0
              mfrom=0
          endif
          if(inchk(ply) .lt. 0) then
              ctemp=checki(player)
              if(ctemp .ne. 0) then
                   moves(where)=0
                   go to 100
              endif
              legals=legals+1
          endif
c
c------------------------------< determine if this move is a capture
c------------------------------< or promotion. if not, skip it.
c
          if(mtype.ge.4 .or. mcappc.ne.0) go to 300
              if(movgpc .ne. 1) go to 100
              if(side .ge. 0) then
                  if(mto .lt. 60) go to 100
              else
                  if(mto .gt. 60) go to 100
              endif
              pushpp=500
300       continue
c
c------------------------------< the score is equal to the value
c------------------------------< of the piece being captured.  if
c------------------------------< the piece is defended, subtract
c------------------------------< the value of the capturing piece.
c
          call mover
          score=-ripoff(mto)+pushpp
          if(mcappc .ne. 0) score=score+pieces(mcappc)
          if(mpropc .ne. 0) score=score+pieces(mpropc)-1000
          call umover
c
c------------------------------< if the score is less than zero
c------------------------------< leave the move where it is and do
c------------------------------< not consider it a phase2 capture.
c
          if(score .lt. 0) go to 100
          if(score.eq.0 .and. ply.gt.rdepth+4) go to 100
          temp=moves(first(ply)+count)
          moves(first(ply)+count)=moves(where)
          moves(where)=temp
          count=count+1
          val(count)=score
          go to 100
500   continue
c
c------------------------------< if there is only one legal move
c------------------------------< then set onerep(ply) true (1)
c
          onerep(ply)=0
          if(inchk(ply).lt.0 .and. legals.eq.1) then
              onerep(ply)=1
              if(ply .le. tflag) print 550, ply
550           format(1x,'one legal response to check at ply',i3)
          endif
c
c------------------------------< if there is one or fewer captures
c------------------------------< that seem to win, there is no need
c------------------------------< to sort the list.
c
          if(count .eq. 0) go to 9998
          if(count .eq. 1) go to 900
c
c------------------------------< now order the captures based on
c------------------------------< the expected gain of material
c------------------------------< computed above.
c
600   continue
          done=1
          where=0
          move=first(ply)-1
700       continue
              where=where+1
              if(where .ge. count) go to 800
              move=move+1
              if(val(where) .ge. val(where+1)) go to 700
                  temp=val(where)
                  val(where)=val(where+1)
                  val(where+1)=temp
                  temp=moves(move)
                  moves(move)=moves(move+1)
                  moves(move+1)=temp
                  done=0
          go to 700
800       continue
      if(done .eq. 0) go to 600
c
c------------------------------< remember how many captures were
c------------------------------< selected so that phase2 can exit
c------------------------------< when they have been examined.
c
900   continue
          status(ply)=count+1
          which(ply)=first(ply)-1
9998  continue
      return
      end
      subroutine phase3
crev  last revision 09/14/91
c
c     ******************************************************************
c     *                                                                *
c     *      phase3 is used to select the 'killer' moves for this      *
c     *  ply from the 'killer' move list as the next move(s) to be     *
c     *  considered.  the 'killer' moves are examined in order of      *
c     *  their frequency of use in an attempt to try the best one      *
c     *  first.  these 'killer' moves are simply moves that were       *
c     *  found to be best at this level from the same previous         *
c     *  parent position.  by hopefully looking at the best move       *
c     *  first, the search will optimize the number of alpha/beta      *
c     *  cutoffs that occur.                                           *
c     *      an additional feature added for version seven is to try   *
c     *  all killers for the same side that have been found.  that is, *
c     *  after examining all killers for ply 3 of a 5 ply search,      *
c     *  try the ply 1 killers and the ply 5 killers as well.  the     *
c     *  reason for this is that phase3 is more or less random in      *
c     *  selection of moves, therefore, any reasonable choices that    *
c     *  can be tried should be searched first.                        *
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
c------------------------------< advance to the next killer for
c------------------------------< this level.
c
      level=ishft(status(ply),-16)
      whichk=and(status(ply),65535)+1
      if(whichk .gt. 2) go to 700
c
c------------------------------< sort through the killer lists
c------------------------------< to determine if a match can be
c------------------------------< be found.
c
100   continue
          do 600 kill=whichk,2
              move=killmv(level,kill)
              if(move .ne. 0) then
                  do 200 where=first(ply),last(ply)
                      if(move .eq. moves(where)) go to 300
200               continue
                  go to 600
300               continue
c                 mfrom=extrct(move)
                  mtype=and(ishft(move,-14),7)
                  mcappc=and(ishft(move,-17),7)
                  mto=and(ishft(move,-7),127)
                  movgpc=ishft(move,-20)
                  mpropc=mtype-2
                  if(mtype .lt. 4) mpropc=0
                  mfrom=and(move,127)
                  if(mto .gt. 99) then
                      mtype=8
                      mcappc=0
                      mto=0
                      movgpc=0
                      mpropc=0
                      mfrom=0
                  endif
                  if(givchk(ply) .eq. 0) then
                          go to 9999
                  endif
                  ctemp=checkg(player)
                  if(ctemp .ne. 0) go to 9999
                  moves(where)=0
                  go to 600
              endif
600       continue
c
c------------------------------< this level of killers has been
c------------------------------< exhausted, it is now time to try
c------------------------------< killers from another ply.
c
700   continue
          if(level .eq. ply) level=player-2
          level=level+2
          whichk=1
          if(level .eq. ply) level=level+2
          if(level .gt. rdepth) go to 9998
          go to 100
c
c----------------------------------< killer search complete
c
9998  continue
          return=1
          return
c
c------------------------------< a killer move was found, select
c------------------------------< it and return to examine it to
c------------------------------< see if it causes an alpha/beta
c------------------------------< cutoff in this position.
c
9999  continue
          status(ply)=ishft(level,16)+kill
          which(ply)=where
          return=0
          return
      end
      subroutine phase4
crev  last revision 03/30/91
c
c     ******************************************************************
c     *                                                                *
c     *      phase4 is used to select all moves that are left in       *
c     *  the move list after phases 0, 1, 2 and 3 have been executed.  *
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
c------------------------------< if this is the first entry for this
c------------------------------< level, initialize.
c
      if(status(ply) .eq. 0) then
          which(ply)=first(ply)-1
          status(ply)=1
      endif
c
c------------------------------< advance to the next move and
c------------------------------< determine if it should be ex-
c------------------------------< amined.
c
200   continue
          which(ply)=which(ply)+1
          if(which(ply) .gt. last(ply)) go to 9998
          if(moves(which(ply)) .eq. 0) go to 200
c
c------------------------------< if the 'givchk' flag is set, only
c------------------------------< select moves that put the opponent
c------------------------------< in check. this is used to run down
c------------------------------< an attack on the king to see what
c------------------------------< happens.
c
c     mfrom=extrct(moves(which(ply)))
      mtype=and(ishft(moves(which(ply)),-14),7)
      mcappc=and(ishft(moves(which(ply)),-17),7)
      mto=and(ishft(moves(which(ply)),-7),127)
      movgpc=ishft(moves(which(ply)),-20)
      mpropc=mtype-2
      if(mtype .lt. 4) mpropc=0
      mfrom=and(moves(which(ply)),127)
      if(mto .gt. 99) then
          mtype=8
          mcappc=0
          mto=0
          movgpc=0
          mpropc=0
          mfrom=0
      endif
      if(givchk(ply) .ne. 0) then
          ctemp=checkg(player)
          if(ctemp .eq. 0) go to 200
      endif
c
c------------------------------< this move should be examined.
c------------------------------< select it and return to begin
c------------------------------< the search.
c
9999  continue
          return=0
          return
c
c------------------------------< no moves were found, return 1 to
c------------------------------< indicate search is complete.
c
9998  continue
          return=1
          return
      end
      subroutine phase5
crev  last revision 03/24/91
c
c     ******************************************************************
c     *                                                                *
c     *      phase5 is used to select the next ply=1 move for the      *
c     *  tree search.  the moves are selected in the order they were   *
c     *  sorted by 'base' and reordered by driver based on the node    *
c     *  counts for each move.                                         *
c     *      phase5 examines moves in three phases.  (1) take the      *
c     *  move list in order until the evaluation is approximately as   *
c     *  good as the previous iteration or search.  (2) then extract   *
c     *  tactical moves (captures/pawn pushes) for examination next in *
c     *  order to be sure that these moves are examined before time    *
c     *  runs out.  (3) examine the rest of the move list.             *
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
c
c------------------------------< determine if the search should be
c------------------------------< aborted due to a very good/very
c------------------------------< bad move.
c
      if(trueval .gt. 0) value(1)=value(-1)
      phase(1)=6
      if(which(1) .eq. 0) status(1)=1
      if(which(1) .le. 0) go to 300
      if(mated(1) .ne. 0) go to 100
      if(trueval .eq. 0) then
          if(value(1) .gt. window(1)) go to 100
          if(lowered.ge.1000 .and. minply.gt.1) go to 100
          if(raised .ne. 0) go to 100
              if(window(2) .ne. 1999999) go to 9998
100       continue
          if(value(1) .ge. window(2)) go to 9998
          if(value(1).gt.900000 .and.
     *       value(1) .lt. 1500000) then
              if(vterm.lt.300000 .and.
     *           value(1).lt.vterm)    go to 300
              if(value(1) .gt. peval) go to 9998
              go to 300
          endif
          if(value(1) .ge. vterm) go to 9998
      endif
300   continue
      go to (400,700,1000), status(1)
c
c------------------------------< if the best score found so far is
c------------------------------< within 1/10 of a pawn of the best
c------------------------------< score from the previous search,
c------------------------------< then enter the tactical search.
c------------------------------< otherwise, this loop will try the
c------------------------------< next sequential move in the list.
c
400   continue
          tcomp=100
          if(eval.ge.100 .and. value(1).lt.0) tcomp=200
          if(value(1).lt.eval-tcomp .or. which(1).le.0) then
500           continue
                  which(1)=which(1)+1
                  if(which(1) .gt. last(1)) go to 9998
                  if(tried(which(1)) .ne. 0) go to 500
c                     mfrom=extrct(moves(which(1)))
                      mtype=and(ishft(moves(which(1)),-14),7)
                      mcappc=and(ishft(moves(which(1)),-17),7)
                      mto=and(ishft(moves(which(1)),-7),127)
                      movgpc=ishft(moves(which(1)),-20)
                      mpropc=mtype-2
                      if(mtype .lt. 4) mpropc=0
                      mfrom=and(moves(which(1)),127)
                      if(mto .gt. 99) then
                          mtype=8
                          mcappc=0
                          mto=0
                          movgpc=0
                          mpropc=0
                          mfrom=0
                      endif
                      tried(which(1))=1
                      go to 2000
          endif
          status(1)=2
c
c-----------------------------< the best score found is close to
c-----------------------------< the score from the previous search.
c-----------------------------< before examining every move left,
c-----------------------------< select captures and pawn pushes to
c-----------------------------< try to find a tactical brilliancy
c-----------------------------< since the last iteration will not
c-----------------------------< be fully completed.
c
700   continue
          which(1)=which(1)+1
          if(which(1) .le. last(1)) then
          if(tried(which(1)) .ne. 0) go to 700
c             mfrom=extrct(moves(which(1)))
              mtype=and(ishft(moves(which(1)),-14),7)
              mcappc=and(ishft(moves(which(1)),-17),7)
              mto=and(ishft(moves(which(1)),-7),127)
              movgpc=ishft(moves(which(1)),-20)
              mpropc=mtype-2
              if(mtype .lt. 4) mpropc=0
              mfrom=and(moves(which(1)),127)
              if(mto .gt. 99) then
                  mtype=8
                  mcappc=0
                  mto=0
                  movgpc=0
                  mpropc=0
                  mfrom=0
              endif
              if(mcappc .eq. 0) then
                  if(movgpc.ne.1 .or. mto.lt.60) go to 700
              endif
              tried(which(1))=1
              go to 2000
          endif
          status(1)=3
          which(1)=0
c
c-----------------------------< now search the remainder of the
c-----------------------------< move list examining any move that
c-----------------------------< has not been examined by the two
c-----------------------------< preceeding steps.
c
1000  continue
          which(1)=which(1)+1
          if(which(1) .gt. last(1)) go to 9998
          if(tried(which(1)) .ne. 0) go to 1000
c             mfrom=extrct(moves(which(1)))
              mtype=and(ishft(moves(which(1)),-14),7)
              mcappc=and(ishft(moves(which(1)),-17),7)
              mto=and(ishft(moves(which(1)),-7),127)
              movgpc=ishft(moves(which(1)),-20)
              mpropc=mtype-2
              if(mtype .lt. 4) mpropc=0
              mfrom=and(moves(which(1)),127)
              if(mto .gt. 99) then
                  mtype=8
                  mcappc=0
                  mto=0
                  movgpc=0
                  mpropc=0
                  mfrom=0
              endif
              tried(which(1))=1
              go to 2000
c
c------------------------------< output the selected move to let
c------------------------------< the operators know what's going on.
c
2000  continue
          if(rnodes+nodes.ge.snodes .and. autos.ne.0 .and.
     *       vttype.eq.2) then
              otype=0
              ctemp=checkg(1)
              if(ctemp .ne. 0) otype=1
              call output(otype,1)
              call cptime(psec2,msec2)
              if(cputim .ne. 0) psec2=msec2
              mtime=(psec2-fsec1)/100
              print 2100, depth, hhmmss(mtime,5), 
     *                   which(1), last(1), (text(i),i=1,10), cursup
2100          format(17x,i3,4x,a5,4x,i2,'/',i2,t41,10a1,a3)
          endif
          go to 9999
c
c------------------------------< no moves were found, return to
c------------------------------< indicate search is complete.
c
9998  continue
          return=1
          return
c
c------------------------------< a move was found, return to examine
c------------------------------< it.
c
9999  continue
          return=0
          return
      end
