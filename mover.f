      subroutine mover
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      mover is used to make normal moves on the game board.  it *
c     *  saves the value of the 'to' square before making a move so    *
c     *  that the move can be 'unmade' later.  the database used by    *
c     *  the search to generate moves is not updated, therefore,       *
c     *  mover/umover are used only to quickly make/unmake a move for  *
c     *  testing, not searching. mover1 is called for unusual moves.   *
c     *                                                                *
c     ******************************************************************
c
c
c    mtype:        0 = normal move
c                  1 = castle king-side
c                  2 = castle queen-side
c                  3 = en passant pawn capture
c                  4 = pawn promotion to knight
c                  5 = pawn promotion to bishop
c                  6 = pawn promotion to rook
c                  7 = pawn promotion to queen
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
c------------------------------< normal moves
c
      if(mtype .eq. 0) then
          cappc(ply)=board(mto)
          board(mto)=board(mfrom)
          board(mfrom)=0
c
c------------------------------< if the king is being moved, update
c------------------------------< it's location
c
          if(iabs(board(mto)) .eq. 6) kloc(player)=mto
          return
c
c------------------------------< if not a normal move, call mover 2.
c
      else
          call mover1
      endif
      return
      end
      subroutine mover1
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      mover1 is used to make unusual moves on the game board.   *
c     *                                                                *
c     ******************************************************************
c
c    mtype:        0 = normal move (handled by cal routine)
c                  1 = castle king-side
c                  2 = castle queen-side
c                  3 = en passant pawn capture
c                  4 = pawn promotion to knight
c                  5 = pawn promotion to bishop
c                  6 = pawn promotion to rook
c                  7 = pawn promotion to queen
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
c------------------------------< new entry point
c
      go to (100,300,500,600,600,600,600),mtype
c
c------------------------------< castle king-side
c
100   continue
          bias=cbias(player)
          cappc(ply)=0
          if(color .eq. 1) then
              board(bias+22)=0
              board(bias+23)=6*side
              board(bias+24)=4*side
              board(bias+25)=0
              kloc(player)=bias+23
          else
              board(bias+29)=0
              board(bias+28)=6*side
              board(bias+27)=4*side
              board(bias+26)=0
              kloc(player)=bias+28
          endif
          return
c
c------------------------------< castle queen-side
c
300   continue
          bias=cbias(player)
          cappc(ply)=0
          if(color .eq. 1) then
              board(bias+29)=0
              board(bias+27)=6*side
              board(bias+26)=4*side
              board(bias+25)=0
              kloc(player)=bias+27
          else
              board(bias+22)=0
              board(bias+24)=6*side
              board(bias+25)=4*side
              board(bias+26)=0
              kloc(player)=bias+24
          endif
              return
c
c------------------------------< en passant pawn capture
c
500   continue
          board(mto)=board(mfrom)
          board(mfrom)=0
          board(mto-10*side)=0
          cappc(ply)=-1*side
          return
c
c------------------------------< pawn promotion
c
600   continue
          cappc(ply)=board(mto)
          board(mto)=mpropc*side
          board(mfrom)=0
          return
      end
