      integer function illegal(testmove)
crev  last revision 06/21/89
c
c     ******************************************************************
c     *                                                                *
c     *     illegal() is used to test the legality of a move,          *
c     *  normally one that has been retrieved from the hash table.     *
c     *  since these moves might be applied in the wrong position due  *
c     *  to hashing collisions, they are put through a simple sanity   *
c     *  check to make sure we don't wipe out the game board.          *
c     *                                                                *
c     *     these "sanity" checks are quite simple and simply verify   *
c     *  that the moving piece is currently on the from square, the    *
c     *  captured piece is on the to square, or that castling/ep       *
c     *  moves have pieces on the correct squares so that even if the  *
c     *  move is technically illegal (psuedo-legal) it won't "create"  *
c     *  a piece on the board out of nowhere                           *
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
c------------------------------< extract the move (unpack it)
c
      mtype=and(ishft(testmove,-14),7)
      mcappc=and(ishft(testmove,-17),7)
      mto=and(ishft(testmove,-7),127)
      movgpc=ishft(testmove,-20)
      mpropc=mtype-2
      if(mtype .lt. 4) mpropc=0
      mfrom=and(testmove,127)
      if(mto .gt. 99) then
          mtype=8
          mcappc=0
          mto=0
          movgpc=0
          mpropc=0
          mfrom=0
      endif
c
c------------------------------< make sure that the move suggested
c------------------------------< by the table is legal.
c
      go to (100,200,300,400,100,100,100,100),mtype+1
c
c------------------------------< normal/pawn promotion moves.
c
100   continue
          if(board(mfrom) .ne. side*movgpc) go to 9998
          if(iabs(board(mto)) .ne. mcappc) go to 9998
              go to 9999
c
c------------------------------< castle king-side moves.
c
200   continue
          if(movedr(player) .eq. 0) go to 9998
          if(moveds(ply) .eq. 0) go to 9998
          bias=cbias(player)
          if(color .eq. 1) then
              if(board(bias+23) .ne. 0) go to 9998
              if(board(bias+24) .ne. 0) go to 9998
              if(and(movedr(player),1) .eq. 0) go to 9998
              if(and(moveds(ply),1) .eq. 0) go to 9998
          else
              if(board(bias+28) .ne. 0) go to 9998
              if(board(bias+27) .ne. 0) go to 9998
              if(and(movedr(player),2) .eq. 0) go to 9998
              if(and(moveds(ply),2) .eq. 0) go to 9998
          endif
          go to 9999
c
c------------------------------< castle queen-side moves.
c
300   continue
          if(movedr(player) .eq. 0) go to 9998
          if(moveds(ply) .eq. 0) go to 9998
          bias=cbias(player)
          if(color .eq. 1) then
              if(board(bias+26) .ne. 0) go to 9998
              if(board(bias+27) .ne. 0) go to 9998
              if(and(movedr(player),2) .eq. 0) go to 9998
              if(and(moveds(ply),2) .eq. 0) go to 9998
          else
              if(board(bias+24) .ne. 0) go to 9998
              if(board(bias+25) .ne. 0) go to 9998
              if(and(movedr(player),1) .eq. 0) go to 9998
              if(and(moveds(ply),1) .eq. 0) go to 9998
          endif
          go to 9999
c
c------------------------------< en passant pawn captures
c
400   continue
          if(board(mfrom) .ne. side) go to 9998
          if(board(mto) .ne. 0) go to 9998
          if(board(mto-10*side) .ne. -side) go to 9998
              go to 9999
c
c------------------------------< return indicating that the move
c------------------------------< was illegal.
c
9998  continue
          illegal=1
          return
c
c------------------------------< return indicating that the move
c------------------------------< was legal.
c
9999  continue
          illegal=0
          return
      end

