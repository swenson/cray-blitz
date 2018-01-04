      subroutine castle
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      castle is used to generate all castle type moves.         *
c     *  castle only generates legal moves, unlike the regular move    *
c     *  generators which may generate moves leaving the king in       *
c     *  check.  function 'attack' is called to make sure that the     *
c     *  king will not pass over a square attacked by the enemy when   *
c     *  performing the castle move.  it also makes sure that the      *
c     *  king and rook(s) haven't moved rendering castling illegal.    *
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
c
c------------------------------< if in check, castling is illegal.
c
      if(inchk(ply) .ne. 0) return
c
c------------------------------< initialize.
c
      bias=cbias(player)
c
c------------------------------< program is black, generate queen-side
c------------------------------< castling moves.
c
      if(color .eq. 2) then
          if(and(moveds(ply),1).ne.0   .and.
     *       board(bias+22).eq.4*side   .and.
     *       board(bias+23).eq.0        .and.
     *       board(bias+24).eq.0        .and.
     *       board(bias+25).eq.0        .and.
     *       board(bias+26).eq.6*side   .and.
     *       attack(-side,bias+24).eq.0 .and.
     *       attack(-side,bias+25).eq.0) then
              last(ply)=last(ply)+1
              moves(last(ply))=ishft(castqn,14)
          endif
c
c------------------------------< program is black, generate king-side
c------------------------------< castling moves.
c
          if(and(moveds(ply),2).ne.0   .and.
     *       board(bias+26).eq.6*side   .and.
     *       board(bias+27).eq.0        .and.
     *       board(bias+28).eq.0        .and.
     *       board(bias+29).eq.4*side   .and.
     *       attack(-side,bias+27).eq.0 .and.
     *       attack(-side,bias+28).eq.0) then
              last(ply)=last(ply)+1
              moves(last(ply))=ishft(castkg,14)
          endif
          return
c
c------------------------------< program is white, generate king-side
c------------------------------< castling moves.
c
      else
          if(and(moveds(ply),1).ne.0   .and.
     *       board(bias+22).eq.4*side   .and.
     *       board(bias+23).eq.0        .and.
     *       board(bias+24).eq.0        .and.
     *       board(bias+25).eq.6*side   .and.
     *       attack(-side,bias+23).eq.0 .and.
     *       attack(-side,bias+24).eq.0) then
              last(ply)=last(ply)+1
              moves(last(ply))=ishft(castkg,14)
          endif
c
c------------------------------< program is white, generate queen-side
c------------------------------< castling moves.
c
          if(and(moveds(ply),2).ne.0   .and.
     *       board(bias+25).eq.6*side   .and.
     *       board(bias+26).eq.0        .and.
     *       board(bias+27).eq.0        .and.
     *       board(bias+28).eq.0        .and.
     *       board(bias+29).eq.4*side   .and.
     *       attack(-side,bias+26).eq.0 .and.
     *       attack(-side,bias+27).eq.0) then
              last(ply)=last(ply)+1
              moves(last(ply))=ishft(castqn,14)
          endif
          return
      endif
      end
