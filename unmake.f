      subroutine unmake
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      unmake is used to unmake all moves on the game board.     *
c     *  it restores the board to the position before the current      *
c     *  move was made.                                                *
c     *      unmake also updates the various data bases that are used  *
c     *  in move generation and position scoring.  it incrementally    *
c     *  updates the hash table key as pieces are moved, taking into   *
c     *  consideration en passant status.  it updates the pawn         *
c     *  structure tables for position evaluation and maintains the    *
c     *  material score and piece counts as well.                      *
c     *      it calls unmake1, unmake2, and/or unmake3 to handle       *
c     *  special moves and captures.                                   *
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
c------------------------------< initialize.
c
      hash=msave7(ply)
      phash=msave8(ply)
      nppcs=msave9(ply)
      nopcs=msave10(ply)
      mscore=msave11(ply)
c
c------------------------------< check castling status
c
      if(movedr(player) .ne. 0) moveds(ply)=moveds(ply-2)
c
c------------------------------<    mtype: 0 = normal move
c
      if(mtype .ne. 0) then
          call unmake2
          return
c
c------------------------------< retract normal move
c
      else
          board(mfrom)=board(mto)
          board(mto)=cappc(ply)
          bitbd=or(and(or(bitbd,bit0(mfrom)),cbit0(mto)),
     *              cappb(ply))
          pboard(mfrom)=pboard(mto)
          pboard(mto)=cappt(ply)
          plist(pboard(mfrom))=mfrom
          plist(pboard(mto))=mto
          vlist(pboard(mfrom))=board(mfrom)
          vlist(pboard(mto))=board(mto)
c
c------------------------------< now update the pawn structure if a pawn
c------------------------------< is being moved.  if the move is a push,
c------------------------------< only pawn locations are being changed;
c------------------------------< however, if a pawn is capturing some-
c------------------------------< thing it is moving to a new file and
c------------------------------< comprehensive updating is required.
c
          if(iabs(board(mfrom)) .eq. 1) then
              call unmake1
              return
          endif
c
c------------------------------< if the king is being moved, update
c------------------------------< it's location.
c
          if(iabs(board(mfrom)) .eq. 6) then
              kloc(player)=mfrom
              krank(player)=cranks(mfrom)
              kfile(player)=cfiles(mfrom)
          endif
c
c------------------------------< if a piece is being captured, then the
c------------------------------< material score must be adjusted.  if
c------------------------------< the piece being captured is a pawn,
c------------------------------< then the pawn structure must be ad-
c------------------------------< justed as well as the pawn location.
c
          if(cappc(ply) .ne. 0) call unmake3
          return
      endif
      end
      subroutine unmake1
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *     unmake1 handles normal pawn moves                          *
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
c------------------------------< restore the average rank and file
c------------------------------< values.
c
      frank=cranks(mfrom)
      ffile=cfiles(mfrom)
      arank=arank-cranks(mto)+frank
      afile=afile-cfiles(mto)+ffile
c
c------------------------------< program pawn move.
c
      if(side .gt. 0) then
          if(cfiles(mto) .eq. ffile) then
              pfirst(ffile)=msave1(ply)
              plast(ffile)=msave2(ply)
              return
          endif
c
c------------------------------< undo a pawn capture which affects
c------------------------------< two files.
c
          pfirst(ffile)=msave3(ply)
          plast(ffile)=msave4(ply)
          file=cfiles(mto)
          pcount(file)=pcount(file)-1
          pcount(ffile)=pcount(ffile)+1
          pfirst(file)=msave1(ply)
          plast(file)=msave2(ply)
c
c------------------------------< opponent's pawn move.
c
      else
          if(cfiles(mto) .eq. ffile) then
              ofirst(ffile)=msave1(ply)
              olast(ffile)=msave2(ply)
              return
          endif
c
c------------------------------< undo a pawn capture which affects
c------------------------------< two files.
c
          ofirst(ffile)=msave3(ply)
          olast(ffile)=msave4(ply)
          file=cfiles(mto)
          ocount(file)=ocount(file)-1
          ocount(ffile)=ocount(ffile)+1
          ofirst(file)=msave1(ply)
          olast(file)=msave2(ply)
      endif
c
c------------------------------< if a piece is being captured, then the
c------------------------------< material score must be adjusted.  if
c------------------------------< the piece being captured is a pawn,
c------------------------------< then the pawn structure must be ad-
c------------------------------< justed as well as the pawn location.
c
      if(cappc(ply) .ne. 0) call unmake3
      return
      end
      subroutine unmake2
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *  unmake2 handles unusual moves                                 *
c     *                                                                *
c     ******************************************************************
c
c       mtype=     1 = castle king-side
c                  2 = castle queen-side
c                  3 = en passant pawn capture
c                  4 = pawn promotion to knight
c                  5 = pawn promotion to bishop
c                  6 = pawn promotion to rook
c                  7 = pawn promotion to queen
c                  8 = null move
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
c------------------------------< start here.
c
      go to (100,300,500,800,800,800,800,200),mtype
c
c------------------------------< retract null move
c
200   continue
          rdepth=rdepth+xnullm
          return
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
              bitbd=or(bitbd,bit0(bias+22))
              bitbd=and(bitbd,cbit0(bias+23))
              bitbd=and(bitbd,cbit0(bias+24))
              bitbd=or(bitbd,bit0(bias+25))
              pboard(bias+22)=pboard(bias+24)
              pboard(bias+25)=pboard(bias+23)
              pboard(bias+23)=33
              pboard(bias+24)=33
              plist(pboard(bias+22))=bias+22
              plist(pboard(bias+25))=bias+25
              vlist(pboard(bias+22))=board(bias+22)
              vlist(pboard(bias+25))=board(bias+25)
              kloc(player)=bias+25
              krank(player)=cranks(bias+1)+2
              kfile(player)=5
          else
              board(bias+29)=4*side
              board(bias+28)=0
              board(bias+27)=0
              board(bias+26)=6*side
              bitbd=or(bitbd,bit0(bias+29))
              bitbd=and(bitbd,cbit0(bias+28))
              bitbd=and(bitbd,cbit0(bias+27))
              bitbd=or(bitbd,bit0(bias+26))
              pboard(bias+29)=pboard(bias+27)
              pboard(bias+26)=pboard(bias+28)
              pboard(bias+28)=33
              pboard(bias+27)=33
              plist(pboard(bias+29))=bias+29
              plist(pboard(bias+26))=bias+26
              vlist(pboard(bias+29))=board(bias+29)
              vlist(pboard(bias+26))=board(bias+26)
              kloc(player)=bias+26
              krank(player)=cranks(bias+1)+2
              kfile(player)=6
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
              bitbd=or(bitbd,bit0(bias+29))
              bitbd=and(bitbd,cbit0(bias+27))
              bitbd=and(bitbd,cbit0(bias+26))
              bitbd=or(bitbd,bit0(bias+25))
              pboard(bias+29)=pboard(bias+26)
              pboard(bias+25)=pboard(bias+27)
              pboard(bias+27)=33
              pboard(bias+26)=33
              plist(pboard(bias+29))=bias+29
              plist(pboard(bias+25))=bias+25
              vlist(pboard(bias+29))=board(bias+29)
              vlist(pboard(bias+25))=board(bias+25)
              kloc(player)=bias+25
              krank(player)=cranks(bias+1)+2
              kfile(player)=5
          else
              board(bias+22)=4*side
              board(bias+24)=0
              board(bias+25)=0
              board(bias+26)=6*side
              bitbd=or(bitbd,bit0(bias+22))
              bitbd=and(bitbd,cbit0(bias+24))
              bitbd=and(bitbd,cbit0(bias+25))
              bitbd=or(bitbd,bit0(bias+26))
              pboard(bias+22)=pboard(bias+25)
              pboard(bias+26)=pboard(bias+24)
              pboard(bias+24)=33
              pboard(bias+25)=33
              plist(pboard(bias+22))=bias+22
              plist(pboard(bias+26))=bias+26
              vlist(pboard(bias+22))=board(bias+22)
              vlist(pboard(bias+26))=board(bias+26)
              kloc(player)=bias+26
              krank(player)=cranks(bias+1)+2
              kfile(player)=6
          endif
          return
c
c------------------------------< retract en passant pawn capture
c
500   continue
          board(mfrom)=board(mto)
          board(mto)=0
          board(mto-10*side)=-1*side
          bitbd=or(bitbd,bit0(mfrom))
          bitbd=and(bitbd,cbit0(mto))
          bitbd=or(bitbd,bit0(mto-10*side))
          pboard(mfrom)=pboard(mto)
          pboard(mto)=33
          pboard(mto-10*side)=cappt(ply)
          plist(pboard(mfrom))=mfrom
          plist(pboard(mto-10*side))=mto-10*side
          vlist(pboard(mfrom))=board(mfrom)
          vlist(pboard(mto-10*side))=board(mto-10*side)
c
c------------------------------< a pawn is capturing and a pawn is being
c------------------------------< removed.  update the pawn structure and
c------------------------------< pawn location tables.
c
      if(side .gt. 0) then
          frank=cranks(mfrom)
          ffile=cfiles(mfrom)
          arank=arank+frank-cranks(mto)
          afile=afile+ffile-cfiles(mto)
          pfirst(ffile)=msave3(ply)
          plast(ffile)=msave4(ply)
          file=cfiles(mto)
          pcount(file)=pcount(file)-1
          pcount(ffile)=pcount(ffile)+1
          pfirst(file)=msave1(ply)
          plast(file)=msave2(ply)
          arank=arank+cranks(mto)-1
          afile=afile+file
          ocount(file)=ocount(file)+1
          ofirst(file)=msave5(ply)
          olast(file)=msave6(ply)
          nopwns=nopwns+1
          npawns=npawns+1
      else
          frank=cranks(mfrom)
          ffile=cfiles(mfrom)
          arank=arank+frank-cranks(mto)
          afile=afile+ffile-cfiles(mto)
          ofirst(ffile)=msave3(ply)
          olast(ffile)=msave4(ply)
          file=cfiles(mto)
          ocount(file)=ocount(file)-1
          ocount(ffile)=ocount(ffile)+1
          ofirst(file)=msave1(ply)
          olast(file)=msave2(ply)
          arank=arank+cranks(mto)+1
          afile=afile+file
          pcount(file)=pcount(file)+1
          pfirst(file)=msave5(ply)
          plast(file)=msave6(ply)
          nppwns=nppwns+1
          npawns=npawns+1
      endif
      return
c
c------------------------------< retract pawn promotion
c
800   continue
      mpropc=iabs(board(mto))
      board(mfrom)=side
      board(mto)=cappc(ply)
      bitbd=or(bitbd,bit0(mfrom))
      bitbd=or(and(bitbd,cbit0(mto)),cappb(ply))
      pboard(mfrom)=pboard(mto)
      pboard(mto)=cappt(ply)
      plist(pboard(mfrom))=mfrom
      plist(pboard(mto))=mto
      vlist(pboard(mfrom))=board(mfrom)
      vlist(pboard(mto))=board(mto)
      ffile=cfiles(mfrom)
      afile=afile+ffile
      if(side .gt. 0) then
          arank=arank+8
          pcount(ffile)=pcount(ffile)+1
          pfirst(ffile)=msave1(ply)
          plast(ffile)=8
          nppwns=nppwns+1
          npawns=npawns+1
      else
          arank=arank+3
          ocount(ffile)=ocount(ffile)+1
          ofirst(ffile)=msave1(ply)
          olast(ffile)=3
          nopwns=nopwns+1
          npawns=npawns+1
      endif
c
c------------------------------< reset pwnstrt to new position.
c
      ipp=2
      if(side .ge. 0) ipp=1
      pwnstrt(ipp)=min0(pboard(mfrom),pwnstrt(ipp))
      if(cappc(ply) .ne. 0) call unmake3
      return
      end
      subroutine unmake3
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *  unmake3 handles all captures                                  *
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
c------------------------------< start here.
c
      mcappc=iabs(cappc(ply))
      if(cappc(ply) .eq. 0) return
      if(cappc(ply) .eq. 1) then
          rank=cranks(mto)
          file=cfiles(mto)
          arank=arank+rank
          afile=afile+file
          pcount(file)=pcount(file)+1
          pfirst(file)=msave5(ply)
          plast(file)=msave6(ply)
          nppwns=nppwns+1
          npawns=npawns+1
      else if(cappc(ply) .eq. -1) then
          rank=cranks(mto)
          file=cfiles(mto)
          arank=arank+rank
          afile=afile+file
          ocount(file)=ocount(file)+1
          ofirst(file)=msave5(ply)
          olast(file)=msave6(ply)
          nopwns=nopwns+1
          npawns=npawns+1
      endif
      return
      end
