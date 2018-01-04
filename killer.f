      subroutine killer1
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      killer is used to maintain the 'killer' move list which   *
c     *  is used by phase4 move selection.  basically, a killer        *
c     *  move is either one of two possibilities:                      *
c     *                                                                *
c     *      1)  it is the best move found at this level after the     *
c     *          other side made some move from the same parent        *
c     *          position;                                             *
c     *                                                                *
c     *      2)  or it is a move that refutes the move made by the     *
c     *          other side from the same parent position.             *
c     *                                                                *
c     *      the purpose of 'killer' moves is to provide a good move   *
c     *  early in the move list in order to maximize the number of     *
c     *  alpha/beta cutoffs.  it is based on the principle that most   *
c     *  moves from a parent position can be refuted by the same       *
c     *  move; whether the move is a capture, a fork, or simply a      *
c     *  strong positional move such as claiming an open file.  this   *
c     *  list will have up to two such moves to help the program       *
c     *  'learn' as it progresses through the search.                  *
c     *      killer1 is called when a move is to be backed up to the   *
c     *  previous ply as a new best move.  the killer move is the one  *
c     *  being backed up and comes from trace(ply,ply).                *
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
c
c------------------------------< remember the current best move
c------------------------------< as a 'killer' move.  these moves
c------------------------------< will be tried first each time a move
c------------------------------< is examined from the curent ply.
c------------------------------< don't enter a move that captures the
c------------------------------< last piece moved since it probably
c------------------------------< only refutes that move.
c
      if(mcappc.eq.0 .and.
     *   (ply.le.rdepth .or. inchk(ply).ne.0
     *                 .or. givchk(ply).ne.0)) then
c
c------------------------------< first, check to see if the current
c------------------------------< move is already in the 'killer' list.
c------------------------------< if it is, bump it's popularity count
c------------------------------< and adjust the position in the list.
c
          if(trace(ply,ply) .eq. killmv(ply,1)) then
              killmv(ply,3)=killmv(ply,3)+1
          else if(trace(ply,ply) .eq. killmv(ply,2)) then
              killmv(ply,4)=killmv(ply,4)+1
              if(killmv(ply,3) .lt. killmv(ply,4)) then
                  temp=killmv(ply,1)
                  killmv(ply,1)=killmv(ply,2)
                  killmv(ply,2)=temp
                  temp=killmv(ply,3)
                  killmv(ply,3)=killmv(ply,4)
                  killmv(ply,4)=temp
              endif
c
c------------------------------< the move is not in the list. enter
c------------------------------< it in the last position.
c
          else
              killmv(ply,2)=trace(ply,ply)
              killmv(ply,4)=1
              if(killmv(ply,3) .lt. killmv(ply,4)) then
                  temp=killmv(ply,1)
                  killmv(ply,1)=killmv(ply,2)
                  killmv(ply,2)=temp
                  temp=killmv(ply,3)
                  killmv(ply,3)=killmv(ply,4)
                  killmv(ply,4)=temp
              endif
          endif
      endif
      return
      end
      subroutine killer2
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      killer is used to maintain the 'killer' move list which   *
c     *  is used by phase4 move selection.  basically, a killer        *
c     *  move is either one of two possibilities:                      *
c     *                                                                *
c     *      1)  it is the best move found at this level after the     *
c     *          other side made some move from the same parent        *
c     *          position;                                             *
c     *                                                                *
c     *      2)  or it is a move that refutes the move made by the     *
c     *          other side from the same parent position.             *
c     *                                                                *
c     *      the purpose of 'killer' moves is to provide a good move   *
c     *  early in the move list in order to maximize the number of     *
c     *  alpha/beta cutoffs.  it is based on the principle that most   *
c     *  moves from a parent position can be refuted by the same       *
c     *  move; whether the move is a capture, a fork, or simply a      *
c     *  strong positional move such as claiming an open file.  this   *
c     *  list will have up to two such moves to help the program       *
c     *  'learn' as it progresses through the search.                  *
c     *      killer2 is called when a move causes a cutoff.  the       *
c     *  killer move is the current move under analysis and comes      *
c     *  from curmvs(ply,taskid).                                      *
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
c
c------------------------------< remember the move causing the current
c------------------------------< cutoff as a 'killer' move.  these moves
c------------------------------< will be tried first each time a move
c------------------------------< is examined from the curent ply.
c------------------------------< don't enter a move that captures the
c------------------------------< last piece moved since it probably
c------------------------------< only refutes that move.
c
      if(mcappc.eq.0 .and.
     *   (ply.le.rdepth .or. inchk(ply).ne.0
     *                  .or. givchk(ply).ne.0)) then
c
c------------------------------< first, check to see if the current
c------------------------------< move is already in the 'killer' list.
c------------------------------< if it is, bump it's popularity count
c------------------------------< and adjust the position in the list.
c
          if(curmvs(ply,taskid) .eq. killmv(ply,1)) then
              killmv(ply,3)=killmv(ply,3)+1
          else if(curmvs(ply,taskid) .eq. killmv(ply,2)) then
              killmv(ply,4)=killmv(ply,4)+1
              if(killmv(ply,3) .lt. killmv(ply,4)) then
                  temp=killmv(ply,1)
                  killmv(ply,1)=killmv(ply,2)
                  killmv(ply,2)=temp
                  temp=killmv(ply,3)
                  killmv(ply,3)=killmv(ply,4)
                  killmv(ply,4)=temp
              endif
          else
c
c------------------------------< the move is not in the list. enter
c------------------------------< it in the last position.
c
              killmv(ply,2)=curmvs(ply,taskid)
              killmv(ply,4)=1
              if(killmv(ply,3) .lt. killmv(ply,4)) then
                  temp=killmv(ply,1)
                  killmv(ply,1)=killmv(ply,2)
                  killmv(ply,2)=temp
                  temp=killmv(ply,3)
                  killmv(ply,3)=killmv(ply,4)
                  killmv(ply,4)=temp
              endif
          endif
      endif
      return
      end
