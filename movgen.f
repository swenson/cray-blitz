      subroutine movgen
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      movgen is used to generate all moves.  it acts as a       *
c     *  driver for the individual move generators.  the piece list    *
c     *  is scanned and as a piece for the side on move is detected,   *
c     *  the correct generator is called.                              *
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
c------------------------------< initialize.
c
      last(ply)=first(ply)-1
      gencap=0
      if(ply.gt.rdepth
     *          .and. inchk(ply).eq.0
     *                .and. givchk(ply).eq.0) gencap=1
c
c------------------------------< generate castling moves.
c
      if(ply.le.rdepth .and. moveds(ply).ne.0) call castle
c
c------------------------------< generate en passant pawn captures
c------------------------------< if any exist.  pawnep will call piece
c------------------------------< which will call pawn to finish up.
c
      call pawnep
c
      return
      end
