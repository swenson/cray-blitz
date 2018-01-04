      subroutine make
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      make is used to make all moves on the game board.  it     *
c     *  saves the value of the 'to' square before making a move so    *
c     *  that the move can be 'unmade' later.                          *
c     *      make also updates the various data bases that are used    *
c     *  in move generation and position scoring.  it incrementally    *
c     *  updates the hash table key as pieces are moved, taking into   *
c     *  consideration en passant status.  it updates the pawn         *
c     *  structure tables for position evaluation and maintains the    *
c     *  material score and piece counts as well.                      *
c     *      it calls make1, make2 and/or make3 to make special        *
c     *  moves and captures.                                           *
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
c-----------------------------< initialize.
c
      msave7(ply)=hash
      msave8(ply)=phash
      msave9(ply)=nppcs
      msave10(ply)=nopcs
      msave11(ply)=mscore
      if(mtype .ne. 0) then
          call make2
          return
c
c------------------------------< normal moves
c
      else
          cappc(ply)=board(mto)
          board(mto)=board(mfrom)
          board(mfrom)=0
          cappb(ply)=and(bitbd,bit0(mto))
          bitbd=and(or(bitbd,bit0(mto)),cbit0(mfrom))
          cappt(ply)=pboard(mto)
          pboard(mto)=pboard(mfrom)
          pboard(mfrom)=33
          plist(pboard(mto))=mto
          plist(cappt(ply))=0
          vlist(pboard(mto))=board(mto)
          vlist(cappt(ply))=0
          hash=xor(random(mfrom,board(mto)+7),
     *         xor(random(mto,board(mto)+7),
     *         xor(random(mto,cappc(ply)+7),hash)))
c
c------------------------------< now update the pawn structure if a pawn
c------------------------------< is being moved.  if the move is a push,
c------------------------------< only pawn locations are being changed;
c------------------------------< however, if a pawn is capturing some-
c------------------------------< thing, it is moving to a new file
c------------------------------< and more updating is required.
c
          if(iabs(board(mto)) .eq. 1) then
              call make1
              return
          endif
c
c------------------------------< if the king is being moved, update
c------------------------------< it's location and castling status.
c
          if(iabs(board(mto)) .eq. 6) then
              moveds(ply)=0
              kloc(player)=mto
              krank(player)=cranks(mto)
              kfile(player)=cfiles(mto)
              if(mcappc .ne. 0) call make3
              return
          endif
          if(movedr(player) .eq. 0) then
              if(mcappc .ne. 0) call make3
              return
          endif
          bias=cbias(player)
          if(mfrom .eq. bias+22) moveds(ply)=and(moveds(ply),2)
          if(mfrom .eq. bias+29) moveds(ply)=and(moveds(ply),1)
          if(mto .eq. bias+22) moveds(ply)=and(moveds(ply),2)
          if(mto .eq. bias+29) moveds(ply)=and(moveds(ply),1)
          if(mcappc .ne. 0) call make3
          return
      endif
      end
      subroutine make1
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *     make1 handles normal pawn moves                            *
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
c------------------------------< update hash key and average rank/
c------------------------------< file the pawns stand on.
c
      phash=xor(random(mfrom,board(mto)+7),
     *      xor(random(mto,board(mto)+7),phash))
      rank=cranks(mto)
      file=cfiles(mto)
      arank=arank+rank-cranks(mfrom)
      afile=afile+file-cfiles(mfrom)
c
c------------------------------< program pawn is being moved.
c
      if(side .gt. 0) then
          msave1(ply)=pfirst(file)
          msave2(ply)=plast(file)
          if(cfiles(mfrom) .eq. file) then
              if(pfirst(file) .eq. cranks(mfrom)) pfirst(file)=rank
              plast(file)=max0(plast(file),rank)
              return
          endif
c
c------------------------------< the pawn is capturing something and
c------------------------------< moving to a new file.  update the
c------------------------------< status of both the old and new
c------------------------------< files accordingly.
c
              pfirst(file)=min0(pfirst(file),rank)
              plast(file)=max0(plast(file),rank)
              ffile=cfiles(mfrom)
              pcount(ffile)=pcount(ffile)-1
              pcount(file)=pcount(file)+1
              msave3(ply)=pfirst(ffile)
              msave4(ply)=plast(ffile)
              pfirst(ffile)=100
              plast(ffile)=0
              if(pcount(ffile) .ne. 0) then
                  start=ffile+30
                  do 200 sq=start,90,10
                      if(board(sq) .eq. 1) then
                          pfirst(ffile)=min0(pfirst(ffile),cranks(sq))
                          plast(ffile)=cranks(sq)
                      endif
200               continue
              endif
              if(mcappc .ne. 0) call make3
              return
c
c------------------------------< opponent's pawn is being moved.
c
      else
          msave1(ply)=ofirst(file)
          msave2(ply)=olast(file)
          if(cfiles(mfrom) .eq. file) then
              if(ofirst(file) .eq. cranks(mfrom)) ofirst(file)=rank
              olast(file)=min0(olast(file),rank)
              return
          endif
c
c------------------------------< the pawn is capturing something and
c------------------------------< moving to a new file.  update the
c------------------------------< status of both the old and new
c------------------------------< files accordingly.
c
          ofirst(file)=max0(ofirst(file),rank)
          olast(file)=min0(olast(file),rank)
          ffile=cfiles(mfrom)
          ocount(ffile)=ocount(ffile)-1
          ocount(file)=ocount(file)+1
          msave3(ply)=ofirst(ffile)
          msave4(ply)=olast(ffile)
          ofirst(ffile)=0
          olast(ffile)=100
          if(ocount(ffile) .ne. 0) then
              start=ffile+30
              do 500 sq=start,90,10
                  if(board(sq) .eq. -1) then
                      ofirst(ffile)=cranks(sq)
                      olast(ffile)=min0(olast(ffile),cranks(sq))
                  endif
500           continue
          endif
          if(mcappc .ne. 0) call make3
          return
      endif
      end
      subroutine make2
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *     make2 handles unusual moves                                *
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
      go to (100,300,500,1600,1600,1600,1600,200),mtype
c
c------------------------------< null move
c
200   continue
          rdepth=rdepth-xnullm
          return
c
c------------------------------< castle king-side
c
100   continue
          bias=cbias(player)
          cappc(ply)=0
          moveds(ply)=0
          if(color .eq. 1) then
              board(bias+22)=0
              board(bias+23)=6*side
              board(bias+24)=4*side
              board(bias+25)=0
              bitbd=and(bitbd,cbit0(bias+22))
              bitbd=or(bitbd,bit0(bias+23))
              bitbd=or(bitbd,bit0(bias+24))
              bitbd=and(bitbd,cbit0(bias+25))
              pboard(bias+23)=pboard(bias+25)
              pboard(bias+24)=pboard(bias+22)
              pboard(bias+22)=33
              pboard(bias+25)=33
              plist(pboard(bias+23))=bias+23
              plist(pboard(bias+24))=bias+24
              vlist(pboard(bias+23))=board(bias+23)
              vlist(pboard(bias+24))=board(bias+24)
              hash=xor(random(bias+22,4*side+7),
     *             xor(random(bias+23,6*side+7),
     *             xor(random(bias+24,4*side+7),
     *             xor(random(bias+25,6*side+7),hash))))
              kloc(player)=bias+23
              krank(player)=cranks(bias+1)+2
              kfile(player)=3
          else
              board(bias+29)=0
              board(bias+28)=6*side
              board(bias+27)=4*side
              board(bias+26)=0
              bitbd=and(bitbd,cbit0(bias+29))
              bitbd=or(bitbd,bit0(bias+28))
              bitbd=or(bitbd,bit0(bias+27))
              bitbd=and(bitbd,cbit0(bias+26))
              pboard(bias+28)=pboard(bias+26)
              pboard(bias+27)=pboard(bias+29)
              pboard(bias+29)=33
              pboard(bias+26)=33
              plist(pboard(bias+28))=bias+28
              plist(pboard(bias+27))=bias+27
              vlist(pboard(bias+28))=board(bias+28)
              vlist(pboard(bias+27))=board(bias+27)
              hash=xor(random(bias+29,4*side+7),
     *             xor(random(bias+28,6*side+7),
     *             xor(random(bias+27,4*side+7),
     *             xor(random(bias+26,6*side+7),hash))))
              kloc(player)=bias+28
              krank(player)=cranks(bias+1)+2
              kfile(player)=8
          endif
          return
c
c------------------------------< castle queen-side
c
300   continue
          bias=cbias(player)
          cappc(ply)=0
          moveds(ply)=0
          if(color .eq. 1) then
              board(bias+29)=0
              board(bias+27)=6*side
              board(bias+26)=4*side
              board(bias+25)=0
              bitbd=and(bitbd,cbit0(bias+29))
              bitbd=or(bitbd,bit0(bias+27))
              bitbd=or(bitbd,bit0(bias+26))
              bitbd=and(bitbd,cbit0(bias+25))
              pboard(bias+27)=pboard(bias+25)
              pboard(bias+26)=pboard(bias+29)
              pboard(bias+29)=33
              pboard(bias+25)=33
              plist(pboard(bias+27))=bias+27
              plist(pboard(bias+26))=bias+26
              vlist(pboard(bias+27))=board(bias+27)
              vlist(pboard(bias+26))=board(bias+26)
              hash=xor(random(bias+29,4*side+7),
     *             xor(random(bias+27,6*side+7),
     *             xor(random(bias+26,4*side+7),
     *             xor(random(bias+25,6*side+7),hash))))
              kloc(player)=bias+27
              krank(player)=cranks(bias+1)+2
              kfile(player)=7
          else
              board(bias+22)=0
              board(bias+24)=6*side
              board(bias+25)=4*side
              board(bias+26)=0
              bitbd=and(bitbd,cbit0(bias+22))
              bitbd=or(bitbd,bit0(bias+24))
              bitbd=or(bitbd,bit0(bias+25))
              bitbd=and(bitbd,cbit0(bias+26))
              pboard(bias+24)=pboard(bias+26)
              pboard(bias+25)=pboard(bias+22)
              pboard(bias+22)=33
              pboard(bias+26)=33
              plist(pboard(bias+24))=bias+24
              plist(pboard(bias+25))=bias+25
              vlist(pboard(bias+24))=board(bias+24)
              vlist(pboard(bias+25))=board(bias+25)
              hash=xor(random(bias+22,4*side+7),
     *             xor(random(bias+24,6*side+7),
     *             xor(random(bias+25,4*side+7),
     *             xor(random(bias+26,6*side+7),hash))))
              kloc(player)=bias+24
              krank(player)=cranks(bias+1)+2
              kfile(player)=4
          endif
          return
c
c------------------------------< en passant pawn capture
c
500   continue
          cappc(ply)=-1*side
          board(mto)=board(mfrom)
          board(mfrom)=0
          board(mto-10*side)=0
          cappb(ply)=bit0(mto-10*side)
          bitbd=or(bitbd,bit0(mto))
          bitbd=and(bitbd,cbit0(mfrom))
          bitbd=and(bitbd,cbit0(mto-10*side))
          pboard(mto)=pboard(mfrom)
          pboard(mfrom)=33
          cappt(ply)=pboard(mto-10*side)
          pboard(mto-10*side)=33
          plist(pboard(mto))=mto
          plist(cappt(ply))=0
          vlist(pboard(mto))=board(mto)
          vlist(cappt(ply))=0
          hash=xor(random(mfrom,board(mto)+7),
     *         xor(random(mto,board(mto)+7),
     *         xor(random(mto-10*side,7-side),hash)))
          phash=xor(random(mfrom,board(mto)+7),
     *          xor(random(mto,board(mto)+7),
     *          xor(random(mto-10*side,7-side),phash)))
c
c------------------------------< a pawn is capturing and a pawn is being
c------------------------------< removed.  update the pawn structure and
c------------------------------< pawn location tables.
c
          if(side .gt. 0) then
              rank=cranks(mto)
              file=cfiles(mto)
              ffile=cfiles(mfrom)
              arank=arank-cranks(mfrom)+1
              afile=afile-ffile
              msave1(ply)=pfirst(file)
              msave2(ply)=plast(file)
              msave3(ply)=pfirst(ffile)
              msave4(ply)=plast(ffile)
              msave5(ply)=ofirst(file)
              msave6(ply)=olast(file)
              pfirst(file)=min0(pfirst(file),rank)
              plast(file)=max0(plast(file),rank)
              pcount(ffile)=pcount(ffile)-1
              pcount(file)=pcount(file)+1
              pfirst(ffile)=100
              plast(ffile)=0
              if(pcount(ffile) .ne. 0) then
                  start=ffile+30
                  do 600 sq=start,90,10
                      if(board(sq) .eq. 1) then
                          pfirst(ffile)=min0(pfirst(ffile),cranks(sq))
                          plast(ffile)=cranks(sq)
                      endif
600               continue
              endif
              ocount(file)=ocount(file)-1
              ofirst(file)=0
              olast(file)=100
              if(ocount(file) .ne. 0) then
                  start=file+30
                  do 800 sq=start,90,10
                      if(board(sq) .eq. -1) then
                          ofirst(file)=cranks(sq)
                          olast(file)=min0(olast(file),cranks(sq))
                      endif
800               continue
              endif
              nopwns=nopwns-1
              npawns=npawns-1
          else
              rank=cranks(mto)
              file=cfiles(mto)
              ffile=cfiles(mfrom)
              arank=arank-cranks(mfrom)-1
              afile=afile-ffile
              msave1(ply)=ofirst(file)
              msave2(ply)=olast(file)
              msave3(ply)=ofirst(ffile)
              msave4(ply)=olast(ffile)
              msave5(ply)=pfirst(file)
              msave6(ply)=plast(file)
              ofirst(file)=max0(ofirst(file),rank)
              olast(file)=min0(olast(file),rank)
              ocount(ffile)=ocount(ffile)-1
              ocount(file)=ocount(file)+1
              ofirst(ffile)=0
              olast(ffile)=100
              if(ocount(ffile) .ne. 0) then
                  start=ffile+30
                  do 1100 sq=start,90,10
                      if(board(sq) .eq. -1) then
                          ofirst(ffile)=cranks(sq)
                          olast(ffile)=min0(olast(ffile),cranks(sq))
                      endif
1100              continue
              endif
              pcount(file)=pcount(file)-1
              pfirst(file)=100
              plast(file)=0
              if(pcount(file) .ne. 0) then
                  start=file+30
                  do 1300 sq=start,90,10
                      if(board(sq) .eq. 1) then
                          pfirst(file)=min0(pfirst(file),cranks(sq))
                          plast(file)=cranks(sq)
                      endif
1300              continue
              endif
              nppwns=nppwns-1
              npawns=npawns-1
          endif
          mscore=mscore+side*pieces(1)
          return
c
c------------------------------< pawn promotion
c
1600  continue
          cappc(ply)=board(mto)
          board(mto)=mpropc*side
          board(mfrom)=0
          cappb(ply)=and(bitbd,bit0(mto))
          bitbd=or(bitbd,bit0(mto))
          bitbd=and(bitbd,cbit0(mfrom))
          cappt(ply)=pboard(mto)
          pboard(mto)=pboard(mfrom)
          pboard(mfrom)=33
          plist(pboard(mto))=mto
          plist(cappt(ply))=0
          vlist(pboard(mto))=board(mto)
          vlist(cappt(ply))=0
          hash=xor(random(mfrom,7+side),
     *         xor(random(mto,board(mto)+7),
     *         xor(random(mto,cappc(ply)+7),hash)))
          phash=xor(random(mfrom,side+7),phash)
c
c------------------------------< a new piece is being made and
c------------------------------< a pawn is being removed. update
c------------------------------< the pieces/pawn locations.
c
          ffile=cfiles(mfrom)
          afile=afile-ffile
          if(side .gt. 0) then
              arank=arank-8
              pcount(ffile)=pcount(ffile)-1
              msave1(ply)=pfirst(ffile)
              pfirst(ffile)=100
              plast(ffile)=0
              if(pcount(ffile) .ne. 0) then
                  start=ffile+30
                  do 1700 sq=start,90,10
                      if(board(sq) .eq. 1) then
                          pfirst(ffile)=min0(pfirst(ffile),cranks(sq))
                          plast(ffile)=cranks(sq)
                      endif
1700              continue
              endif
              nppwns=nppwns-1
              npawns=npawns-1
              nppcs=nppcs+3
              if(mpropc .eq. 2) nppcs=nppcs-1
              if(mpropc .eq. 4) nppcs=nppcs+2
              if(mpropc .eq. 5) nppcs=nppcs+6
          else
              arank=arank-3
              ocount(ffile)=ocount(ffile)-1
              msave1(ply)=ofirst(ffile)
              ofirst(ffile)=0
              olast(ffile)=100
              if(ocount(ffile) .ne. 0) then
                  start=ffile+30
                  do 2000 sq=start,90,10
                      if(board(sq) .eq. -1) then
                          ofirst(ffile)=cranks(sq)
                          olast(ffile)=min0(olast(ffile),cranks(sq))
                      endif
2000              continue
              endif
              nopwns=nopwns-1
              npawns=npawns-1
              nopcs=nopcs+3
              if(mpropc .eq. 2) nopcs=nopcs-1
              if(mpropc .eq. 4) nopcs=nopcs+2
              if(mpropc .eq. 5) nopcs=nopcs+6
          endif
c
c------------------------------< reset pcend to the new position.
c
      ipp=2
      if(side .ge. 0) ipp=1
      pcend(ipp)=max0(pcend(ipp),pboard(mto))
      mscore=mscore+side*(pieces(mpropc)-pieces(1))
c
c------------------------------< if a piece is being captured, then the
c------------------------------< material score must be adjusted.
c------------------------------< if the piece being captured is a
c------------------------------< pawn, then the pawn structure must
c------------------------------< be adjusted as well as the location.
c
      if(mcappc .ne. 0) call make3
      return
      end
      subroutine make3
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *     make3 handles capture moves                                *
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
c------------------------------< adjust piece counts for the piece
c------------------------------< being removed.  if it is a pawn,
c------------------------------< adjust the average rank and file
c------------------------------< values also.
c
      go to (1500,1400,1300,1200,1100,800,1500,
     *       100,400,500,600,700,1500),cappc(ply)+7
100   continue
          phash=xor(random(mto,cappc(ply)+7),phash)
          rank=cranks(mto)
          file=cfiles(mto)
          arank=arank-rank
          afile=afile-file
          pcount(file)=pcount(file)-1
          msave5(ply)=pfirst(file)
          msave6(ply)=plast(file)
          pfirst(file)=100
          plast(file)=0
          if(pcount(file) .ne. 0) then
              start=file+30
              do 200 sq=start,90,10
                  if(board(sq) .eq. 1) then
                      pfirst(file)=min0(pfirst(file),cranks(sq))
                      plast(file)=cranks(sq)
                  endif
200           continue
          endif
          nppwns=nppwns-1
          npawns=npawns-1
          go to 1500
400   continue
          nppcs=nppcs-2
          go to 1500
500   continue
          nppcs=nppcs-3
          go to 1500
600   continue
          nppcs=nppcs-5
          go to 1500
700   continue
          nppcs=nppcs-9
          go to 1500
800   continue
          phash=xor(random(mto,cappc(ply)+7),phash)
          rank=cranks(mto)
          file=cfiles(mto)
          arank=arank-rank
          afile=afile-file
          ocount(file)=ocount(file)-1
          msave5(ply)=ofirst(file)
          msave6(ply)=olast(file)
          ofirst(file)=0
          olast(file)=100
          if(ocount(file) .ne. 0) then
              start=file+30
              do 900 sq=start,90,10
                  if(board(sq) .eq. -1) then
                      ofirst(file)=cranks(sq)
                      olast(file)=min0(olast(file),cranks(sq))
                  endif
900           continue
          endif
          nopwns=nopwns-1
          npawns=npawns-1
          go to 1500
1100  continue
          nopcs=nopcs-2
          go to 1500
1200  continue
          nopcs=nopcs-3
          go to 1500
1300  continue
          nopcs=nopcs-5
          go to 1500
1400  continue
          nopcs=nopcs-9
1500  continue
          pcval=pieces(mcappc)
          mscore=mscore+side*pcval
          return
      end
