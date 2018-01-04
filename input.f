      subroutine input(error)
crev  last revision 08/29/90
c
c     ******************************************************************
c     *                                                                *
c     *      input is used to translate all chess moves from text      *
c     *  strings to the internal form used by the program.  it will    *
c     *  accept standard or reduced algebraic notation.  standard      *
c     *  form includes ne2-c3 and simply e2-c3.  reduced form will     *
c     *  cover the following cases: e4 (pawns only), exf5 (pawns only) *
c     *  nc3, nbc3, nxc7, ndxc7.  standard castling moves are o-o and  *
c     *  o-o-o.  too much information never hurts, however, too        *
c     *  little may leave the move ambiguous.                          *
c     *      pieces and pawns are always represented by a capital      *
c     *  letter on input/output.  ranks and other letters such as      *
c     *  x for captures must be lower case.                            *
c     *      the 'error' parameter is a flag which can inhibit error   *
c     *  messages if true.  this is used in reducing moves to the      *
c     *  simplified form for output.                                   *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      integer xmoves(400)
c
c
      include 'global.f'
c
c
      include 'common.f'
c
c
      equivalence (xmoves(1),moves(2000))
c
c------------------------------< generate all legal moves
c------------------------------< and load for elimination
c
      tempf=first(ply)
      templ=last(ply)
      first(ply)=1001
      player=2-and(ply,1)
      side=cvside(player)
      tempd=rdepth
      rdepth=ply+1
      inchk(ply)=attack(-side,kloc(player))
      givchk(ply)=0
      if(ply .eq. 2) then
          f1=from(ply-1)
          t1=to(ply-1)
          from(ply-1)=prevmv(3*(2-player)+1)
          to(ply-1)=prevmv(3*(2-player)+2)
      endif
      moveds(ply)=moveds(ply-2)
      call movgen
      if(ply .eq. 2) then
          from(ply-1)=f1
          to(ply-1)=t1
      endif
      istop=last(ply)-1000
      first(ply)=tempf
      last(ply)=templ
      rdepth=tempd
      do 100 i=1,istop
          xmoves(i)=moves(i+1000)
100   continue
c
c------------------------------< elimate all illegal moves
c
      do 200 i=1,istop
          mfrom=extrctf(xmoves(i))
          ctemp=checki(player)
          if(ctemp .ne. 0) xmoves(i)=0
          xmoves(i+200)=checkg(player)
200   continue
      xmoves(200)=istop
      call inputx(xmoves)
      if(return .eq. 0) return
      if(return .eq. 2) go to 400
      if(return .eq. 3) go to 600
c
c------------------------------< error messages
c
          if(error .ne. 0) print 300, bell
300       format(1x,'that''s ambiguous.',a1)
          return=1
          return
400   continue
          if(error .ne. 0) print 500, bell
500       format(1x,'illegal move.',a1)
          return=1
          return
600   continue
          if(error .ne. 0) print 700, bell
700       format(1x,'unrecognizable move.',a1)
          return=1
          return
      end
      subroutine inputx(xmoves)
crev  last revision 08/29/90
c
c     ******************************************************************
c     *                                                                *
c     *      inputx is used to compare the input move (algebraic       *
c     *  notation) with the list of legal moves.  it eliminates the    *
c     *  moves that somehow don't match the input move.  if, after     *
c     *  this is done, only one move is remaining in the list, then    *
c     *  this move must be the internal representation of the move     *
c     *  entered.                                                      *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      integer piecnm(12)
      integer xmoves(400),sqfrom(200),sqto(200),fpiece(200),tpiece(200)
      integer digits(9)
c
c
      include 'global.f'
c
c
      include 'common.f'
c
c
      equivalence (dash,alpha(64)),(equal,alpha(67)),
     *(alphao,alpha(41)),(plus,alpha(63)),(alphax,alpha(50)),
     *(blank,alpha(70)),(digits(1),alpha(54))
      data piecnm/'p','n','b','r','q','k','P','N','B','R','Q','K'/
c
c------------------------------< initialize.
c
50    continue
      frank=0
      ffile=0
      trank=0
      tfile=0
      piece=0
      ppiece=0
      pnprom=0
      chk=0
c
c------------------------------< load for testing
c
      nmoves=xmoves(200)
      do 100 i=1,nmoves
          mfrom=extrctf(xmoves(i))
          sqfrom(i)=mfrom
          sqto(i)=mto
          fpiece(i)=movgpc*side
          tpiece(i)=mcappc
100   continue
      if(atext(1) .eq. alphao) go to 2500
c
c------------------------------< take care of the bb4 meaning move the
c------------------------------< bishop and not the b pawn
c
      if(atext(1).eq.alpha(28) .and. atext(2).eq.alpha(28))
     *             atext(1)=alpha(2)
c
c------------------------------< first, find out whether the move is a
c------------------------------< capture or regular move.
c
      do 200 stype=1,80
          if(atext(stype) .eq. blank) go to 300
          if(atext(stype) .eq. alphax) go to 300
          if(atext(stype) .eq. dash) go to 300
200   continue
300   continue
c
c------------------------------< now break out the destination square
c------------------------------< and extract the rank/file if either
c------------------------------< is present.
c
      do 400 ch=1,80
          if(ch .ne. stype) then
              if(atext(ch).eq.blank .or. atext(ch).eq.equal .or.
     *           atext(ch).eq.plus)                       go to 500
              if(atext(ch).ge.alpha(53) .and.
     *                  atext(ch).le.alpha(62)) then
                  frank=trank
                  trank=atext(ch)
              else
                  if(atext(ch).ge.alpha(27) .and.
     *                  atext(ch).le.alpha(34)) then
                      ffile=tfile
                      tfile=atext(ch)
                  else
                      piece=atext(ch)
                  endif
              endif
          endif
400   continue
500   continue
c
c------------------------------< process promotions '=q' and checks '+'
c
      do 600 char=1,79
          if(atext(char) .eq. plus) chk=1
          if(atext(char) .eq. equal) then
              ppiece=atext(char+1)
              pnprom=1
          endif
600   continue
c
c------------------------------< convert character strings to
c------------------------------< integers for processing
c
      do 700 i=1,8
          if(frank .eq. digits(i)) frank=i+1
          if(trank .eq. digits(i)) trank=i+1
          if(ffile .eq. alpha(i+26)) ffile=10-i
          if(tfile .eq. alpha(i+26)) tfile=10-i
700   continue
      if(trank.lt.0 .or. frank.lt.0) go to 3200
      if(tfile.lt.0 .or. ffile.lt.0) go to 3200
      do 800 i=1,6
          if(ppiece.eq.piecnm(i) .or.
     *       ppiece.eq.piecnm(i+6))     ppiece=i
          if(piece.eq.piecnm(i) .or.
     *       piece.eq.piecnm(i+6))      piece=i
800   continue
      if(piece.eq.0 .and. (frank.eq.0 .or. ffile.eq.0
     *              .or.   trank.eq.0 .or. tfile.eq.0)) piece=1
      if(piece.lt.0 .or. piece.gt.6) go to 3200
      if(ppiece.lt.0 .or. ppiece.gt.6) go to 3200
      piece=piece*side
      if(color .ne. 1) then
          if(ffile .ne. 0) ffile=11-ffile
          if(frank .ne. 0) frank=11-frank
          if(tfile .ne. 0) tfile=11-tfile
          if(trank .ne. 0) trank=11-trank
      endif
c
c------------------------------< eliminate castling moves,
c------------------------------< en passant, promotion, etc.
c
      do 900 i=1,nmoves
          mfrom=extrctf(xmoves(i))
          if(mtype.eq.castkg .or. mtype.eq.castqn) sqfrom(i)=0
          if(pnprom.ne.0 .and.
     *        (mtype.lt.promot .or. ppiece.ne.mpropc)) sqfrom(i)=0
900   continue
c
c------------------------------< eliminate all moves except
c------------------------------< moves of correct piece
c
      stype=atext(stype)
      do 1000 i=1,nmoves
          if(piece.ne.0 .and. piece.ne.fpiece(i)) sqfrom(i)=0
          if(stype.eq.alphax .and. tpiece(i).eq.0) sqfrom(i)=0
1000  continue
c
c------------------------------< eliminate all moves except
c------------------------------< moves on correct files
c
      do 1100 i=1,nmoves
          if(ffile .ne. 0) then
              t=cfiles(sqfrom(i))
              if(t .ne. ffile) sqfrom(i)=0
          endif
          if(tfile .ne. 0) then
              t=cfiles(sqto(i))
              if(t .ne. tfile) sqfrom(i)=0
          endif
1100  continue
c
c------------------------------< eliminate all moves except
c------------------------------< moves on correct rank
c
      do 1200 i=1,nmoves
          if(frank .ne. 0) then
              t=cranks(sqfrom(i))
              if(t .ne. frank) sqfrom(i)=0
          endif
          if(trank .ne. 0) then
              t=cranks(sqto(i))
              if(t .ne. trank) sqfrom(i)=0
          endif
1200  continue
c
c------------------------------< eliminate all moves but checks
c------------------------------< if so indicated.
c
      if(chk .ne. 0) then
          do 1300 i=1,nmoves
              if(xmoves(i+200) .eq. 0) sqfrom(i)=0
1300      continue
      endif
c
c------------------------------< check to make sure only
c------------------------------< one move is left
c
      ic=0
      do 1400 i=1,nmoves
          if(sqfrom(i) .ne. 0) then
              ic=ic+1
              imove=i
          endif
1400  continue
      if(ic .gt. 1) go to 1600
      if(ic .eq. 0) go to 3100
1500  continue
      mfrom=extrctf(xmoves(imove))
      return=0
      return
c
c------------------------------< more than one move is left. if the
c------------------------------< moves are not all pawn promotions, then
c------------------------------< the move is ambiguous beyond hope and
c------------------------------< is rejected. if the move is a promotion
c------------------------------< the operator is asked for the promotion
c------------------------------< piece to clarify his intentions.
c
1600  continue
      do 1700 i=1,nmoves
          if(sqfrom(i) .ne. 0) then
              mfrom=extrctf(xmoves(i))
              if(mtype .lt. promot) go to 3000
          endif
1700  continue
1800  continue
      print 1900, bell, bell, bell
1900  format(1x,'promote to which piece? (q/r/b/n)',3(a1,15(' ')))
      call command(eof)
      do 2000 i=1,26
          if(atext(1) .eq. alpha(i+26)) atext(1)=alpha(i)
2000  continue
      do 2100 i=1,5
          if(atext(1) .eq. piecnm(i+6)) go to 2200
2100  continue
      go to 1800
2200  continue
      ppiece=i
      do 2300 i=1,nmoves
          if(sqfrom(i) .ne. 0) then
              mfrom=extrctf(xmoves(i))
              if(ppiece .ne. mpropc) sqfrom(i)=0
          endif
2300  continue
      ic=0
      do 2400 i=1,nmoves
          if(sqfrom(i) .ne. 0) then
              ic=ic+1
              imove=i
          endif
2400  continue
      if(ic .eq. 0) go to 3100
      if(ic .gt. 1) go to 3000
      go to 1500
c
c------------------------------< check castling moves
c
2500  continue
      if(atext(2).ne.dash .or. atext(3).ne.alphao) go to 3100
      if(atext(4) .ne. blank) go to 2600
      typemv=castkg
      go to 2700
2600  continue
      if(atext(4).ne.dash .or. atext(5).ne.alphao) go to 3100
      typemv=castqn
2700  continue
      do 2800 i=1,nmoves
          mfrom=extrctf(xmoves(i))
          if(typemv .eq. mtype) go to 2900
2800  continue
      go to 3100
2900  continue
      return=0
      return
c
c------------------------------< error messages
c
3000  continue
          if(atext(1) .eq. alpha(28)) then
              atext(1)=alpha(2)
              go to 50
          endif
          return=1
          return
3100  continue
          if(atext(1) .eq. alpha(28)) then
              atext(1)=alpha(2)
              go to 50
          endif
          return=2
          return
3200  continue
          if(atext(1) .eq. alpha(28)) then
              atext(1)=alpha(2)
              go to 50
          endif
          return=3
          return
      end
