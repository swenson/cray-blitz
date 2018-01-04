      subroutine umover
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      umover is used to unmake normal moves on the game board.  *
c     *  it restores the board to the position before the current      *
c     *  move was made.  umover is caled to undo a move that was made  *
c     *  by mover.  the database used by the search to generate moves  *
c     *  not updated,  therefore, mover/umover are used only to        *
c     *  quickly make/unmake a move for testing, not searching.        *
c     *  umover1 is called to process unusual moves.                   *
c     *                                                                *
c     ******************************************************************
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
c------------------------------< retract normal move
c
      if(mtype .eq. 0) then
          board(mfrom)=board(mto)
          board(mto)=cappc(ply)
          if(iabs(board(mfrom)) .eq. 6) kloc(player)=mfrom
      else
          call umover1
      endif
      return
      end
      subroutine umover1
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *     umover1 is used to unmake unusual moves on the game board. *
c     *                                                                *
c     ******************************************************************
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
c------------------------------< new entry point
c
      go to (100,300,500,600,600,600,600),mtype
c
c------------------------------< retract castle king-side
c
100   continue
          bias=cbias(player)
          if(color .eq. 1) then
              board(bias+22)=4*side
              board(bias+23)=0
              board(bias+24)=0
              board(bias+25)=6*side
              kloc(player)=bias+25
          else
              board(bias+29)=4*side
              board(bias+28)=0
              board(bias+27)=0
              board(bias+26)=6*side
              kloc(player)=bias+26
          endif
          return
c
c------------------------------< retract castle queen-side
c
300   continue
          bias=cbias(player)
          if(color .eq. 1) then
              board(bias+29)=4*side
              board(bias+27)=0
              board(bias+26)=0
              board(bias+25)=6*side
              kloc(player)=bias+25
          else
              board(bias+22)=4*side
              board(bias+24)=0
              board(bias+25)=0
              board(bias+26)=6*side
              kloc(player)=bias+26
          endif
          return
c
c------------------------------< retract en passant pawn capture
c
500   continue
          board(mfrom)=board(mto)
          board(mto)=0
          board(mto-10*side)=-1*side
          return
c
c------------------------------< retract pawn promotion
c
600   continue
          board(mfrom)=side
          board(mto)=cappc(ply)
          return
      end
