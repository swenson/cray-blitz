      subroutine piece
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      piece is used to generate all piece moves (excluding      *
c     *  pawn and castling moves which are special cases handled       *
c     *  elsewhere).  piece calls pawn to generate pawn moves.         *
c     *      if a move leaves the king in check, this will be found    *
c     *  as the search progresses deeper when the king is captured.    *
c     *  at this point the previous move will be rejected as illegal.  *
c     *  since the underlying idea of this program is speed, this      *
c     *  seems to be faster than checking for 'in check' after each    *
c     *  move is generated since only a small percentage of moves      *
c     *  actually leave the king in check.                             *
c     *                                                                *
c     *      see extrct for form of move storage                       *
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
c------------------------------< find squares with pieces of side
c
      do 300 where=pstart(player),pcend(player)
          mpiece=side*vlist(where)
          square=plist(where)
          if(mpiece .gt. 1) then
c
c------------------------------< begin iteration to generate all legal
c------------------------------< moves from 'square'.
c
              do 200 i=begin(mpiece),end(mpiece)
                  tosq=square
100               continue
                      tosq=tosq+movdir(i)
c
c------------------------------< make sure the piece is not capturing a
c------------------------------< friendly piece or moving off the edge
c------------------------------< of the board.
c
                      cpiece=-side*board(tosq)
                      if(cpiece.ge.0 .and. cpiece.lt.6) then
                          if(gencap.eq.0 .or. cpiece.ne.0) then
c
c------------------------------< this move is legal. enter it into
c------------------------------< the move list for consideration.
c
                              last(ply)=last(ply)+1
                              moves(last(ply))=square+ishft(tosq,7)
     *                                         +ishft(cpiece,17)
     *                                         +ishft(mpiece,20)
                          endif
                          if(begin(mpiece) .gt. 8) go to 200
                          if(cpiece .eq. 0) go to 100
                      else
                          if(cpiece .eq. 6) then
                              return=1
                              return
                          endif
                      endif
200           continue
          endif
300   continue
c
c------------------------------< move generation for all pieces is
c------------------------------< complete.  now generate pawn moves.
c
      return=0
      call pawn
      return
      end
