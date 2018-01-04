      subroutine failhi
crev  last revision 09/23/90
c
c     ******************************************************************
c     *                                                                *
c     *      failhi is called whenever the search encounters a move at *
c     *  at ply one that generates a value above the current value of  *
c     *  beta.  these moves are better than the best found so far.     *
c     *      after the first move is examined to fix the lower and     *
c     *  upper search bounds, if any moves generate a value above the  *
c     *  current upper bound failhi is called to remember it as the    *
c     *  new best move.  if no additional cutoffs occur, this move     *
c     *  is the move that will be passed to the next iteration and/or  *
c     *  is the move that will actually be made otherwise.             *
c     *      an additional feature of failhi is that upon encountering *
c     *  a beta cutoff condition, the upper search bound is not        *
c     *  immediately relaxed to plus infinity, but is incrementally    *
c     *  increased for each successive cutoff on the same move.  the   *
c     *  first cutoff causes the window to be raised one pawn, the     *
c     *  next one causes the window to be raised one queen, and the    *
c     *  final cutoff will cause the window to reach plus infinity.    *
c     *      this is done in an effort to speed up the re-search that  *
c     *  must be done when the position allows the search to win a     *
c     *  pawn or piece, and the position also contains lots of mates   *
c     *  that are unforced.  with this scheme, the mates will still    *
c     *  cause cutoffs and not cause the search to blow up.            *
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
c------------------------------< first, print the move if necessary.
c
      easy=0
      return=1
      do 100 where=1,last(1)
          if(trace(1,1) .eq. moves(where)) go to 200
100   continue
200   continue
      if(where .gt. last(1)) return
      tried(where)=0
      if(rnodes+nodes .ge. snodes) then
          mfrom=extrct(moves(where))
          ply=1
          otype=0
          ctemp=checkg(1)
          if(ctemp .ne. 0) otype=1
          call output(otype,1)
          call cptime(psec2,msec2)
          if(cputim .ne. 0) psec2=msec2
          mtime=(psec2-fsec1)/100
          if(autos .gt. 0)
     *    print 300, depth, names(taskid), hhmmss(mtime,5), 
     *               raised/1000, (text(i),i=1,30)
300       format(17x,i3,a2,2x,a5,2x,'   ++',i1,t41,30a1)
      endif
c
c------------------------------< if a cutoff is occuring, then the
c------------------------------< window should be increased slowly
c------------------------------< rather than in one jump.  Also,
c------------------------------< lowered should be zero since nothing
c------------------------------< is being lost.
c
      if(raised.lt.1000 .and. window(2).lt.900000) then
          window(1)=window(2)-1
          window(2)=window(2)+1000
          raised=1000
      elseif(raised.lt.9000 .and. window(2).lt.900000) then
          window(1)=window(2)-1
          window(2)=window(2)+8000
          raised=9000
      else
          window(2)=1999999
          raised=1999999
      endif
c
c------------------------------< done, return
c
      which(1)=0
      return=0
      return
      end
      subroutine faillo
crev  last revision 09/23/90
c
c     ******************************************************************
c     *                                                                *
c     *      faillo is called whenever the search finds that the first *
c     *  move at ply one generates a value below the current value of  *
c     *  alpha.  this means that the lower bound was set too           *
c     *  optimistically so that the search did not find a true score.  *
c     *      an additional feature of faillo is that upon encountering *
c     *  an alpha cutoff condition, the lower search bound is not      *
c     *  immediately relaxed to minus infinity, but is incrementally   *
c     *  decreased for each successive cutoff on the same move.  the   *
c     *  first cutoff causes the window to be lowered one pawn, the    *
c     *  next one causes the window to be lowered one queen, and the   *
c     *  final cutoff will cause the window to reach minus infinity.   *
c     *      this is done in an effort to speed up the re-search that  *
c     *  must be done when the position forces the search to lose a    *
c     *  pawn or piece, and the position also contains lots of mates   *
c     *  that are unforced.  with this scheme, the mates will still    *
c     *  cause cutoffs and not cause the search to blow up.            *
c     *      after lowering the lower search bound by the value of a   *
c     *  queen (9000), the search will not abort if the first move     *
c     *  fails low.  the remainder of the move list will be examined   *
c     *  to determine if all moves will fail low.  the reason for      *
c     *  this is that lowering the bound to include mates can slow     *
c     *  things down dramatically.  we have to hope that the program   *
c     *  has not gotten itself into a position where it can be mated   *
c     *  with no escape.                                               *
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
c------------------------------< first, print the move if necessary.
c
      easy=0
      if(rnodes+nodes .ge. snodes) then
          mfrom=extrct(moves(1))
          ply=1
          ctemp=checkg(1)
          call output(ctemp,1)
          call cptime(psec2,msec2)
          if(cputim .ne. 0) psec2=msec2
          fltime=(psec2-fsec1)/100
          if(autos .gt. 0) then
              if(smode.eq.0 .and. fdepth.eq.0) then
                  if(timlim(5) .eq. 0) then
                      atlim=timlim(2)-pelap-surpls
                      atlim=atlim*100
                  else
                      atlim=avgtim*6*100
                  endif
              endif
c
c------------------------------< determine exactly how much
c------------------------------< more time should be used to
c------------------------------< solve the problem if possible.
c
              if(lowered.lt.1000) then
                  targ1=min0(avgtim*3,atlim)
              else if(lowered.lt.9000) then
                  targ1=min0(avgtim*5,atlim)
              else if(lowered.ge.9000) then
                  targ1=min0(avgtim*6,atlim)
              endif
              mtime=targ1/100
              if(autos .gt. 0) 
     *        print 100, depth, names(taskid), 
     *                   hhmmss(fltime,5), lowered/1000,
     *                   (text(i),i=1,7), hhmmss(mtime,5)
100           format(17x,i3,a2,2x,a5,2x,'   --',i1,
     *               t41,7a1,a5,' limit')
          else
              if(autos .gt. 0) 
     *        print 200, depth, names(taskid), 
     *                   hhmmss(fltime,5), lowered/1000,
     *                   (text(i),i=1,7)
200           format(17x,i3,a2,2x,a5,2x,'   --',i1,
     *               t41,30a1)
          endif
      endif
c
c-----------------------------< reduce the lower search bound
c-----------------------------< incrementally.
c
      if(lowered.lt.1000 .and. window(1).gt.-900000) then
          window(1)=window(1)-1000
          lowered=1000
      elseif(lowered.lt.9000 .and. window(1).gt.-900000) then
          window(1)=window(1)-8000
          lowered=9000
      else
          window(1)=-1999999
          lowered=1999999
      endif
      return
      end
