      subroutine pmvmkr
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      pmvmkr is used to make moves on the game board and to     *
c     *  set the permanent castling status if the king or rooks are    *
c     *  moved.  it is only called to make moves immediatly after      *
c     *  they are actually chosen, not during the minimax search.      *
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
c------------------------------< permanently unmake a move. first,
c------------------------------< restore the castling status.
c
      entry pumver
          do 100 i=1,2
              movedr(i)=psave(i)
100       continue
          do 200 i=1,150
              bdsave(i)=pbsave(i)
200       continue
c
c------------------------------< then, unmake the move.
c
          call unmake
          call locate
          point=pointsv
          return
c
c------------------------------< permanently make a move. first,
c------------------------------< save the castling status.
c
      entry pmover
          do 300 i=1,2
              psave(i)=movedr(i)
300       continue
          do 400 i=1,150
              pbsave(i)=bdsave(i)
400       continue
          moveds(ply)=movedr(ply)
c
c------------------------------< make the move,
c
          call make
          call locate
c
c------------------------------< then determine what castling status
c------------------------------< has changed and set the appropriate
c------------------------------< status flags.
c
          movedr(ply)=moveds(ply)
c
c------------------------------< if this is an irreversable move, then
c------------------------------< restart the repeated position table
c------------------------------< to speed up the table search.
c
      entry reverse
          pointsv=point
          if(mcappc .ne. 0) go to 500
          if(movgpc .eq. 1) go to 500
          if(mpropc .ne. 0) go to 500
          if(movedr(ply) .eq. psave(ply)) return
500           continue
                  point=0
                  frmove=npmovs+nomovs
                  return
      end
