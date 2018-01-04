      subroutine pawn
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      pawn is used to generate all pawn moves.  the array       *
c     *  containing the move directions is used to generate the        *
c     *  destination squares.  the moves are checked to make sure that *
c     *  a friendly piece is not being captured.                       *
c     *      en passant captures are handled by a separate routine.    *
c     *      pawn promotion moves cause four (4) moves to be           *
c     *  generated:  promotion to queen, rook, bishop and knight.      *
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
c------------------------------< find squares with pawns of side
c
      do 900 where=pwnstrt(player),pend(player)
          square=plist(where)
          if(vlist(where) .eq. side) then
c
c------------------------------< initialize.
c
              lim=27
              if(side.ge.0 .and. square.lt.40) lim=28
              if(side.lt.0 .and. square.gt.80) lim=28
c
c------------------------------< allow passed pawn moves to 6th 
c------------------------------< thru 8th rank, even if gencap is true
c
              if(gencap .ne. 0) then
                  lim=26
                  if(side.ge.0 .and. square.gt.60) then
                      if(board(square+19).ne.-1 .and.
     *                   board(square+20).ne.-1 .and.
     *                   board(square+21).ne.-1) lim=27
                  else if(side.lt.0 .and. square.lt.60) then
                      if(board(square-19).ne.1 .and.
     *                   board(square-20).ne.1 .and.
     *                   board(square-21).ne.1) lim=27
                  endif
              endif
c
c------------------------------< begin iteration to generate all legal
c------------------------------< moves from 'square'.
c
              do 800 i=25,lim
                  tosq=square+movdir(i)*side
                  cpiece=-side*board(tosq)
                  if(cpiece.ge.0 .and. cpiece.le.6) then
c
c------------------------------< process regular/capturing moves.
c
                      if((i.le.26 .and. cpiece.ne.0) .or.
     *                   (i.gt.26 .and. cpiece.eq.0)) then
                          if(cpiece .eq. 6) then
                              return=1
                              return
                          endif
c
c------------------------------< move is legal, now determine if it
c------------------------------< is a promotion type move.
c
                          if((side.gt.0 .and. tosq.gt.90) .or.
     *                       (side.lt.0 .and. tosq.lt.30)) then
c
c------------------------------< move is a promotion type move, enter
c------------------------------< all four (4) promotions into the move
c------------------------------< list for further consideration.
c
                              temp=square+ishft(tosq,7)
     *                                   +ishft(cpiece,17)
     *                                   +ishft(1,20)
                              moves(last(ply)+1)=temp+ishft(7,14)
                              moves(last(ply)+2)=temp+ishft(4,14)
                              moves(last(ply)+3)=temp+ishft(6,14)
                              moves(last(ply)+4)=temp+ishft(5,14)
                              last(ply)=last(ply)+4
c
c------------------------------< now allow only queen and knight
c------------------------------< promotions in quiescence
c
                              if(gencap .ne. 0) last(ply)=last(ply)-2
c
c------------------------------< move is a normal type move, enter it
c------------------------------< into the move list for further
c------------------------------< consideration.
c
                          else
                              last(ply)=last(ply)+1
                              moves(last(ply))=square+ishft(tosq,7)
     *                                         +ishft(cpiece,17)
     *                                         +ishft(1,20)
                          endif
                      else
                          if(cpiece.gt.0 .and. i.gt.26) go to 900
                      endif
                  else
                      if(i.ge.27 .and. cpiece.ne.0) go to 900
                  endif
800           continue
          endif
900   continue
c
c------------------------------< done with pawns, return
c
          return=0
          return
      end
      subroutine pawnep
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      pawnep is called to generate en passant pawn captures by  *
c     *  the move generator.  it is only called once for each move.    *
c     *  in the new scheme, pawnep calls piece which calls pawn.       *
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
c------------------------------< initialize.
c
      if(ply .le. 1) then
          tlast=prevmv(5)
          flast=prevmv(4)
      else
          tlast=to(ply-1)
          flast=from(ply-1)
      endif
c
c------------------------------< determine if an en passant capture
c------------------------------< is possible.
c
      if(iabs(tlast-flast).eq.20 .and.
     *   board(tlast).eq.-side)   then
          if(board(tlast-1) .eq. side) then
              last(ply)=last(ply)+1
              moves(last(ply))=tlast-1+ishft(tlast+side*10,7)
     *                        +ishft(3,14)+ishft(1,17)
     *                        +ishft(1,20)
          endif
          if(board(tlast+1) .eq. side) then
                  last(ply)=last(ply)+1
                  moves(last(ply))=tlast+1+ishft(tlast+side*10,7)
     *                            +ishft(3,14)+ishft(1,17)
     *                            +ishft(1,20)
          endif
      endif
      call piece
      return
      end
