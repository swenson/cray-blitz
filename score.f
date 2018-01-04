      subroutine score
crev  last revision 03/31/90
c
c     ******************************************************************
c     *                                                                *
c     *     score is the scoring driver.  if both sides have no pawns, *
c     *  the special draw and mate routines are used to evaluate what  *
c     *  is going on.  otherwise, the regular scoring functions scorep,*
c     *  scorekp, scorekm and score1 are called to evaluate the        *
c     *  current position.                                             *
c     *     a special case of rook pawns with bishop of the right/     *
c     *  wrong color has been added.  it enhances play when only rook  *
c     *  pawns and bishops are on the board.                           *
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
      equivalence
     * (pkingl, kloc(1)), (okingl, kloc(2)),
     * (prank, krank(1)), (orank, krank(2)),
     * (pfile, kfile(1)), (ofile, kfile(2))
c
c
c------------------------------< initialize.
c
      pscore=0
c
c------------------------------< does this position require special
c------------------------------< scoring?
c
      if(npawns .ne. 0) then
c
c------------------------------< score minus and plus pawns
c
          call scorep
c
c------------------------------< evaluate trading pieces.  
c
          call scoretrd
c
c------------------------------< if move 10 hasn't been made, then
c------------------------------< call scoredv to evaluate pieces
c------------------------------< that are being moved twice before
c------------------------------< all pieces have moved once.
c
          if(devdone .eq. 0) call scoredv
c
c------------------------------< check for special rook pawn case
c------------------------------< and handle appropriately.
c
          if(rpflag.ne.0 .and. rpflag.ne.3) then
              return=0
              if(nppcs+nopcs.eq.3 .or. nppcs+nopcs.eq.0) call scorebrp
              if(return .eq. 1) return
          endif
c
c------------------------------< score minus and plus kings
c
          call scorekp
          call scorekm
c
c------------------------------< score rest of pieces
c
          call score1
c
c------------------------------< now add in the positional score
c------------------------------< computed to the material balance
c------------------------------< score to compute the net worth
c------------------------------< of the current position.
c
          if(nppcs.eq.3 .and. nopcs.eq.3) call scoreboc
          tscore=mscore+pscore
          if(pscore .lt. minmax(1)) minmax(1)=pscore
          if(pscore .gt. minmax(2)) minmax(2)=pscore
          if(nppwns .eq. 0) then
              if(nppcs.lt.5 .and. tscore.ge.0) tscore=pscore
              if(nppcs-nopcs-nopwns .le. -5) tscore=tscore-10000
          else if(nopwns .eq. 0) then
              if(nopcs.lt.5 .and. tscore.lt.0) tscore=pscore
              if(nopcs-nppcs-nppwns .le. -5) tscore=tscore+10000
          endif
          if(tscore .ge. drawsc) tscore=tscore+100
          return
      endif
c
c------------------------------< determine if this position is drawn due
c------------------------------< to lack of sufficient material by
c------------------------------< either side to force checkmate.
c
      if(nppcs.lt.5 .and. nopcs.lt.5) then
          if(eval .lt. 900000) then
              tscore=drawsc+ply
              return
          endif
      else
c
c------------------------------< if there are no pawns, and the
c------------------------------< position is not special, then use
c------------------------------< 'scorem' to force or avoid check-
c------------------------------< mate.
c
          call scorem
          if(tscore .ge. drawsc) tscore=tscore+100
          return
      endif
      end
      subroutine score1
crev  last revision 02/13/90
c
c     ******************************************************************
c     *                                                                *
c     *      score is called to evaluate the material/positional       *
c     *  status for the current terminal position.  the board is       *
c     *  scanned and various positional scoring functions are called   *
c     *  to compute a positional evaluation.                           *
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
      equivalence
     * (pkingl, kloc(1)), (okingl, kloc(2)),
     * (prank, krank(1)), (orank, krank(2)),
     * (pfile, kfile(1)), (ofile, kfile(2))
c
c
c------------------------------< initialize.
c
      pbish=0
      obish=0
c
c******************************<
c******************************< score minus pieces (opponent)
c******************************<
c
      do 1000 where=pstart(2),pcend(2)
          piece=vlist(where)
          square=plist(where)
c
c******************************<
c******************************< score minus knights
c******************************<
c
          if(piece .eq. -2) then
              file=cfiles(square)
              rank=cranks(square)
c
c------------------------------< evaluate center/king tropism.
c
              pscore=pscore-oknightb(square)
c
c------------------------------< evaluate outpost knights. the knight
c------------------------------< must be on the 5th or 6th rank and
c------------------------------< must be in a 'hole' in the opponent's
c------------------------------< pawns to get a bonus. a larger bonus
c------------------------------< can be had if the knight is supported
c------------------------------< by a pawn. an additional bonus is
c------------------------------< given if a pawn sitting on the knight's
c------------------------------< square would be passed making it a
c------------------------------< difficult decision to capture the
c------------------------------< knight.
c
              if(oopost(square).ne.0 .and.
     *           pfirst(file-1).ge.rank .and.
     *           pfirst(file+1).ge.rank) pscore=pscore-noutpost
c
c******************************<
c******************************< score minus bishops
c******************************<
c
          else if(piece .eq. -3) then
c
c------------------------------< add a bonus if this side has two
c------------------------------< bishops as they are normally much
c------------------------------< stronger together.
c
              if(obish .eq. 0) then
                  obish=1
              else
                  pscore=pscore-bbonus
              endif
c
c------------------------------< evaluate center/king tropism.
c
              pscore=pscore-obishopb(square)
c
c******************************<
c******************************< score minus rooks
c******************************<
c
          else if(piece .eq. -4) then
              file=cfiles(square)
              rank=cranks(square)
c
c------------------------------< evaluate bad rook position.
c
              pscore=pscore-orookb(square)
c
c------------------------------< evaluate half/open and open files
c------------------------------< by checking the file's pawn count.
c
              if(pcount(file)+ocount(file) .eq. 0) then
                  pscore=pscore-ropen
              else if(ocount(file) .eq. 0) then
                  pscore=pscore-rhalfopn
                  if(pweakp(file) .ne. 0) pscore=pscore-atkweakp
              endif
c
c------------------------------< evaluate the usefullness of
c------------------------------< having a rook behind a passed
c------------------------------< pawn to support it.
c
              if(ppass(file) .ne. 0) then
                  if(rank.le.plast(file) .and.
     *               pfirst(file).eq.plast(file)) then
                      pscore=pscore-rbehindp*(plast(file)-2)
                  endif
              else
                  if(opass(file).ne.0 .and.
     *               rank.ge.olast(file) .and.
     *               ofirst(file).eq.olast(file)) then
                      pscore=pscore-rbehindp*(9-olast(file))
                  endif
              endif
c
c------------------------------< evaluate a rook that is psuedo
c------------------------------< trapped by having no horizontal
c------------------------------< mobility due to friendly pieces.
c
              if((board(square-1).eq.99.or.board(square-1).lt.0) .and.
     *           (board(square+1).eq.99.or.board(square+1).lt.0)) 
     *                                           pscore=pscore+rtrapped
c
c******************************<
c******************************< score minus queens
c******************************<
c
          else if(piece .eq. -5) then
c
c------------------------------< evaluate center/king tropism.
c
              pscore=pscore-oqueenb(square)+psafety
          endif
1000  continue
c
c******************************<
c******************************< score plus pieces (program)
c******************************<
c
      do 2000 where=1,pcend(1)
          piece=vlist(where)
          square=plist(where)
c
c******************************<
c******************************< score plus knights
c******************************<
c
          if(piece .eq. 2) then
              file=cfiles(square)
              rank=cranks(square)
c
c------------------------------< evaluate center/king tropism.
c
              pscore=pscore+pknightb(square)
c
c------------------------------< evaluate outpost knights. the knight
c------------------------------< must be on the 5th or 6th rank and
c------------------------------< must be in a 'hole' in the opponent's
c------------------------------< pawns to get a bonus. a larger bonus
c------------------------------< can be had if the knight is supported
c------------------------------< by a pawn. an additional bonus is
c------------------------------< given if a pawn sitting on the knight's
c------------------------------< square would be passed making it a
c------------------------------< difficult decision to capture the
c------------------------------< knight.
c
              if(popost(square).ne.0 .and.
     *           ofirst(file-1).le.rank .and.
     *           ofirst(file+1).le.rank) pscore=pscore+noutpost
c
c******************************<
c******************************< score plus bishops
c******************************<
c
          else if(piece .eq. 3) then
c
c------------------------------< add a bonus if this side has two
c------------------------------< bishops as they are normally much
c------------------------------< stronger together.
c
              if(pbish .eq. 0) then
                  pbish=1
              else
                  pscore=pscore+bbonus
              endif
c
c------------------------------< evaluate center/king tropism.
c
              pscore=pscore+pbishopb(square)
c
c******************************<
c******************************< score plus rooks
c******************************<
c
          else if(piece .eq. 4) then
              file=cfiles(square)
              rank=cranks(square)
c
c------------------------------< evaluate bad rook position.
c
              pscore=pscore+prookb(square)
c
c------------------------------< evaluate half/open and open files
c------------------------------< by checking the file's pawn count.
c
              if(pcount(file)+ocount(file) .eq. 0) then
                  pscore=pscore+ropen
              else if(pcount(file) .eq. 0) then
                  pscore=pscore+rhalfopn
                  if(oweakp(file) .ne. 0) pscore=pscore+atkweakp
              endif
c
c------------------------------< evaluate the usefullness of
c------------------------------< having a rook behind a passed
c------------------------------< pawn to support it.
c
              if(ppass(file) .ne. 0) then
                  if(rank.le.plast(file) .and.
     *               pfirst(file).eq.plast(file)) then
                      pscore=pscore+rbehindp*(plast(file)-2)
                  endif
              else
                  if(opass(file) .ne. 0) then
                      if(rank.ge.olast(file) .and.
     *                   ofirst(file).eq.olast(file)) then
                          pscore=pscore+rbehindp*(9-olast(file))
                      endif
                  endif
              endif
c
c------------------------------< evaluate a rook that is psuedo
c------------------------------< trapped by having no horizontal
c------------------------------< mobility due to friendly pieces.
c
              if(board(square-1).gt.0 .and. board(square+1).gt.0)
     *                                           pscore=pscore-rtrapped
c
c******************************<
c******************************< score plus queens
c******************************<
c
          else if(piece .eq. 5) then
c
c------------------------------< evaluate center/king tropism.
c
              pscore=pscore+pqueenb(square)-osafety
          endif
2000  continue
      return
      end
      subroutine scoreboc
crev  last revision 03/27/91
c
c     ******************************************************************
c     *                                                                *
c     *     scoreboc is used to evaluate the special case where one    *
c     *  side thinks it is winning, but has to win with bishops of     *
c     *  opposite color, a normally drawn ending unless one side has   *
c     *  more than a two pawn advantage, or else has a peculiar pawn   *
c     *  configuration which includes connected passed pawns with one  *
c     *  of the pawns doubled as well so it can drive the bishop       *
c     *  back when it advances.                                        *
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
c------------------------------< check to see if both sides only have
c------------------------------< bishops.
c
      if(nppcs.eq.3 .and. nopcs.eq.3) then
c
c------------------------------< determine if one side is far enoughte
c------------------------------< ahead that it is no problem.
c
          if(iabs(mscore) .gt. 2000) return
c
c------------------------------< locate the bishops.
c
          do 100 i=pstart(1),pend(1)
              if(vlist(i) .eq. 3) go to 125
100       continue
125       continue
          do 150 j=pstart(2),pend(2)
              if(vlist(j) .eq. 3) go to 175
150       continue
175       continue
c
c------------------------------< determine if the bishops are the same
c------------------------------< color.
c
          if(scolor(plist(i)) .eq. scolor(plist(j))) return
c
c------------------------------< bishops are of the opposite color.
c------------------------------< divide the positional score by two and
c------------------------------< return.
c
              pscore=pscore/bocdiv
      endif
      return
      end
      subroutine scorebrp
crev  last revision 03/27/91
c
c     ******************************************************************
c     *                                                                *
c     *     scorebrp is used to evaluate the special case where one    *
c     *  side has a rook pawn and the other side has a bishop.  it     *
c     *  also handles the case of a rook pawn by itself.               *
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
c------------------------------< check to see if program only has a
c------------------------------< bishop and pawns.
c
      if(nppcs .eq. 3) then
          if(nppwns .eq. 0) return
c
c------------------------------< make sure it is a bishop and not a
c------------------------------< knight.
c
          do 100 i=pstart(1),pend(1)
              if(vlist(i) .eq. 3) go to 200
100       continue
          stop 101
c
c------------------------------< determine if the bishop is the wrong
c------------------------------< color (doesn't attack the queening
c------------------------------< square of the rook pawn) or if it is
c------------------------------< the right color.
c
200       continue
              bc=and(cranks(plist(i))+cfiles(plist(i)),1)
              if(bc.eq.1 .and. and(rpflag,2).ne.0) go to 500
              if(bc.eq.0 .and. and(rpflag,1).ne.0) go to 300
              return
c
c------------------------------< wrong color bishop.  if the opponent's
c------------------------------< king controls the queening square, the
c------------------------------< game is a draw with no further ado.
c------------------------------< otherwise, with a wrong color bishop,
c------------------------------< the pawn is not particularly good and
c------------------------------< will probably only draw so penalize
c------------------------------< the score to avoid this position.
c
300       continue
              if(krank(2).lt.8 .or. kfile(2).gt.3) then
                  pscore=pscore-brpscore
              else
                  tscore=drawsc+ply
                  return=1
              endif
              return
500       continue
              if(krank(2).lt.8 .or. kfile(2).lt.8) then
                  pscore=pscore-brpscore
              else
                  tscore=drawsc+ply
                  return=1
              endif
              return
c
c------------------------------< check to see if opponent only has a
c------------------------------< bishop and rook pawns.
c
      else if(nopcs .eq. 3) then
          if(nopwns .eq. 0) return
c
c------------------------------< make sure it is a bishop and not a
c------------------------------< knight.
c
              do 800 i=pstart(2),pend(2)
                  if(vlist(i) .eq. -3) go to 900
800           continue
              stop 102
c
c------------------------------< determine if the bishop is the wrong
c------------------------------< color (doesn't attack the queening
c------------------------------< square of the rook pawn) or if it is
c------------------------------< the right color.
c
900           continue
                  bc=and(cranks(plist(i))+cfiles(plist(i)),1)
                  if(bc.eq.0 .and. and(rpflag,2).ne.0) go to 1200
                  if(bc.eq.1 .and. and(rpflag,1).ne.0) go to 1000
                  return
c
c------------------------------< wrong color bishop.  if the opponent's
c------------------------------< king controls the queening square, the
c------------------------------< game is a draw with no further ado.
c------------------------------< otherwise, with a wrong color bishop,
c------------------------------< the pawn is not particularly good and
c------------------------------< will probably only draw so penalize
c------------------------------< the score to avoid this position.
c
1000          continue
                  if(krank(1).gt.3 .or. kfile(1).gt.3) then
                      pscore=pscore+brpscore
                  else
                      tscore=drawsc+ply
                      return=1
                  endif
                  return
1200          continue
                  if(krank(1).gt.3 .or. kfile(1).lt.8) then
                      pscore=pscore+brpscore
                  else
                      tscore=drawsc+ply
                      return=1
                  endif
                  return
      else
c
c------------------------------< if neither side has a bishop, then
c------------------------------< determine if the queening square of
c------------------------------< the rook pawn is controlled by the
c------------------------------< opponent's king.  if so, the game
c------------------------------< is immediately drawn, otherwise the
c------------------------------< score should be penalized to avoid
c------------------------------< this probably drawn position.
c
          if(nppwns .ne. 0) then
              if(and(rpflag,2) .eq. 0) then
                  if(krank(2).lt.8 .or. kfile(2).gt.4) then
                      pscore=pscore-rpmaydrw
                  else
                      tscore=drawsc+ply
                      return=1
                  endif
                  return
              endif
              if(krank(2).lt.8 .or. kfile(2).lt.7) then
                  pscore=pscore-rpmaydrw
              else
                  tscore=drawsc+ply
                  return=1
              endif
              return
          else if(nopwns .ne. 0) then
              if(and(rpflag,2) .eq. 0) then
                  if(krank(1).gt.4 .or. kfile(1).gt.4) then
                      pscore=pscore+rpmaydrw
                  else
                      tscore=drawsc+ply
                      return=1
                  endif
                  return
              endif
              if(krank(1).gt.3 .or. kfile(1).lt.7) then
                  pscore=pscore+rpmaydrw
              else
                  tscore=drawsc+ply
                  return=1
              endif
              return
          endif
      endif
      end
      subroutine scoredv
crev  last revision 06/21/90
c
c     ******************************************************************
c     *                                                                *
c     *    scoredv is called until move 10 has been made on the game   *
c     *  board.  it assesses a penalty for each piece that hase moved  *
c     *  twice as long as either knight or bishop is still on it's     *
c     *  original square.  it also assesses a penalty if the queen     *
c     *  has moved under the same circumstances.                       *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
c
c
      include 'common.f'
c
c
      include 'global.f'
c
c
c
c
c------------------------------< initialize.
c
      tpscor=0
c
c------------------------------< determine if both bishops and knights
c------------------------------< have moved once.
c
      if(board(23).eq.2 .or. board(24).eq.3 .or.
     *   board(28).eq.2 .or. board(27).eq.3) then
c
c------------------------------< nope, if the queen has moved, then
c------------------------------< assess a penalty
c
          if(board(25).ne.5 .and.
     *       board(26).ne.5) tpscor=tpscor-qearly
c
c------------------------------< penalize each unmoved bishop.
c
          if(board(24).eq.3) tpscor=tpscor-develop
          if(board(27).eq.3) tpscor=tpscor-develop
c
c------------------------------< penalize each unmoved knight.
c
          if(board(23).eq.2) tpscor=tpscor-develop
          if(board(28).eq.2) tpscor=tpscor-develop
      endif
c
c------------------------------< determine if the first piece in the
c------------------------------< current variation is "developing."
c------------------------------< if so, add in a developing bonus to
c------------------------------< to encourage early development rather
c------------------------------< than fooling around.
c
      mpc=ishft(curmvs(1,taskid),-20)
      mfr=and(curmvs(1,taskid),127)
      mt=and(ishft(curmvs(1,taskid),-7),127)
      if((mpc.eq.2 .and. (mfr.eq.23 .or. mfr.eq.28) .and.
     *                    mt.ne.42 .and. mt.ne.49) .or.
     *   (mpc.eq.3 .and. (mfr.eq.24 .or. mfr.eq.27))) then
          pscore=pscore+develop*5
      endif
c
c------------------------------< penalize blocked center pawns.
c
      if(board(35).eq.1 .and. board(45).gt.1) tpscor=tpscor-pblocked
      if(board(36).eq.1 .and. board(46).gt.1) tpscor=tpscor-pblocked
      if(d4 .ne. 0) then
          if(color .eq. 1) then
              if(board(37).eq.1 .and. board(47).gt.1)
     *                             tpscor=tpscor-pblocked*2
          else
              if(board(34).eq.1 .and. board(44).gt.1)
     *                             tpscor=tpscor-pblocked*2
          endif
      endif
c
c------------------------------< penalize unmoved center pawns.
c
      if(board(35).eq.1 .and. board(24).eq.3) 
     *                                   tpscor=tpscor-develop*10
      if(board(36).eq.1 .and. board(27).eq.3) 
     *                                   tpscor=tpscor-develop*10
      if(d4 .ne. 0) then
          if(color .eq. 1) then
              if(board(37) .eq. 1) tpscor=tpscor-develop*10
          else
              if(board(34) .eq. 1) tpscor=tpscor-develop*10
          endif
      endif
      pscore=pscore+tpscor
      return
      end
      subroutine scorekm
crev  last revision 03/02/90
c
c     ******************************************************************
c     *                                                                *
c     *     scorekm is used to determine if this king/pawn position    *
c     *  has occurred before.  if so, the king safety is known and no  *
c     *  further king scoring is needed.                               *
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
      equivalence
     * (pkingl, kloc(1)), (okingl, kloc(2)),
     * (prank, krank(1)), (orank, krank(2)),
     * (pfile, kfile(1)), (ofile, kfile(2))
      data bmask / o'17777777777777' /
c
c
c------------------------------< determine if this king position has
c------------------------------< already been evaluated.  if so, save
c------------------------------< the time and return.
c
      khash=xor(xor(phash,random(okingl,1)),
     *           random(moveds(even(ply))+1,2))
      if(nppcs .le. 15) khash=xor(khash,random(1,3))
      kkey1=ktbl2+and(khash,ksizm1)
      if(and(htable(kkey1),bmask) .eq. ishft(khash,-24)) then
          khashs=khashs+1
          osafety=ishft(htable(kkey1),-48)-32767
          tsafety=osafety
          if(nppcs .gt. 15) tsafety=tsafety*ishft(nppcs,-1)
          pscore=pscore-tsafety
      else
          call scorekm1
      endif
      if(movedr(2) .ne. 0) call scorekm2
      return
      end
      subroutine scorekm1
crev  last revision 09/22/90
c
c     ******************************************************************
c     *                                                                *
c     *     scorekm is used to score the opponent's king safety.  it   *
c     *  analyzes the pawn structure around the king to determine if   *
c     *  it is in a safe or dangerous position.                        *
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
      equivalence
     * (pkingl, kloc(1)), (okingl, kloc(2)),
     * (prank, krank(1)), (orank, krank(2)),
     * (pfile, kfile(1)), (ofile, kfile(2))
c
c
c
c
c------------------------------< determine if the king should
c------------------------------< be concerned about safety by
c------------------------------< counting enemy pieces and the queen.
c
      if(nppcs .gt. 15) then
c
c------------------------------< first, determine which files are
c------------------------------< important to king safety.  the files
c------------------------------< immediately to either side plus
c------------------------------< the file the king is on is important.
c
          ffile=min0(max0(ofile-1,2),7)
          lfile=max0(min0(ofile+1,9),4)
c
c------------------------------< determine if the king's position is
c------------------------------< weak due to weak squares (holes) in
c------------------------------< the king's pawn structure.
c
          if(orank .ge. 8) then
              osafety=ksafety(ofile)*2
          else
              osafety=ksafety(ofile)*(orank-7)
          endif
          do 100 i=ffile,lfile
              if(board(i+79).ne.-1 .and. 
     *           board(i+81).ne.-1) osafety=osafety-khole
100       continue
c
c------------------------------< determine if the king's position is
c------------------------------< weak due to too many pawns being
c------------------------------< advanced in front of the king.
c
          if((board(ffile+80).ne.-1.and.board(ffile+81).ne.-1) .or.
     *       (board(lfile+80).ne.-1.and.board(lfile+79).ne.-1))
     *                     osafety=osafety-kpawns
c
c------------------------------< determine if there are open files
c------------------------------< around the king.  also, open files
c------------------------------< closer to the edge of the board are
c------------------------------< more dangerous than central ones.
c------------------------------< in addition, if the opponent has
c------------------------------< a half-open file, the file is
c------------------------------< dangerous since it is a route for
c------------------------------< rook attacks.  if the program has
c------------------------------< a half-open file, it is dangerous
c------------------------------< since the opponent can fully open
c------------------------------< it very easily.
c
          do 200 i=ffile,lfile
              if(pcount(i)+ocount(i) .eq. 0) then
                  osafety=osafety-ishft(atkfil(i),1)
              else if(ocount(i) .eq. 0) then
                  osafety=osafety-atkfil(i)
              else if(pcount(i) .eq. 0) then
                  osafety=osafety-atkfil(i)
              endif
              if(ofirst(i) .lt. 7) osafety=osafety-ishft(atkfil(i),-1)
200       continue
c
c------------------------------< penalize a king that is trapped on
c------------------------------< the back rank with no 'breathing
c------------------------------< space' on the rank in front of it.
c------------------------------< that is, avoid back rank mating
c------------------------------< situations whenever possible.
c
          do 300 i=ffile,lfile
              if(board(i+80) .ne. -1) go to 400
300       continue
          osafety=osafety-kluft
400       continue
c
c------------------------------< now compute the positional score for
c------------------------------< the king safety term. also, remember
c------------------------------< how exposed the king is so that other
c------------------------------< pieces will be attracted to an exposed
c------------------------------< king.
c
          pscore=pscore-osafety*ishft(nppcs,-1)
      else
c
c------------------------------< king safety is not important,
c------------------------------< attract the king to the center.
c------------------------------< also, attract the king toward the
c------------------------------< important pawns on the board.  if
c------------------------------< passed pawns exist, the king will
c------------------------------< move toward them in an effort to
c------------------------------< capture/defend them.  if none are
c------------------------------< on the board, then attract the king
c------------------------------< toward the center of all pawns.
c
          osafety=kcenter*center(okingl)
          pscore=pscore-osafety
      endif
      htable(kkey1)=ishft(khash,-24)+ishft(osafety+32767,48)
      return
      end
      subroutine scorekm2
crev  last revision 09/16/90
c
c     ******************************************************************
c     *                                                                *
c     *     scorekm2 is used to score the opponent's king safety.  it  *
c     *  determines if the opponent has given up the right to castle   *
c     *  by moving rooks/king, which traps the king in the center of   *
c     *  the board.                                                    *
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
c------------------------------< evaluate development. until the king
c------------------------------< has safely castled, penalize moves
c------------------------------< that move the king or rooks.
c
      if(nppcs .gt. 15) then
          limit=even(ply)
          if(limit .ge. 1) then
              if(moveds(limit) .ne. 0) then
                  if(and(moveds(limit),color) .eq. 0) 
     *                                 pscore=pscore+knocastk*nppcs
              else
                  do 100 temp=2,ply-1,2
                      if(type(temp) .eq. castkg) go to 200
                      if(type(temp) .eq. castqn) go to 200
100               continue
                  pscore=pscore+knocast*nppcs
              endif
          endif
200       continue
      endif
      return
      end
      subroutine scorekp
crev  last revision 03/02/90
c
c     ******************************************************************
c     *                                                                *
c     *     scorekp is used to determine if this king/pawn position    *
c     *  has occurred before.  if so, the king safety is known and no  *
c     *  further king scoring is needed.                               *
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
      equivalence
     * (pkingl, kloc(1)), (okingl, kloc(2)),
     * (prank, krank(1)), (orank, krank(2)),
     * (pfile, kfile(1)), (ofile, kfile(2))
      data bmask / o'17777777777777' /
c
c
c
c
c------------------------------< determine if this king position has
c------------------------------< already been evaluated.  if so, save
c------------------------------< the time and return.
c
      khash=xor(xor(phash,random(pkingl,1)),
     *           random(moveds(odd(ply))+1,2))
      if(nopcs .le. 15) khash=xor(khash,random(1,3))
      kkey1=ktbl1+and(khash,ksizm1)
      if(and(htable(kkey1),bmask) .eq. ishft(khash,-24)) then
          khashs=khashs+1
          psafety=ishft(htable(kkey1),-48)-32767
          tsafety=psafety
          if(nopcs .gt. 15) tsafety=tsafety*ishft(nopcs,-1)
          pscore=pscore+tsafety
      else
          call scorekp1
      endif
      if(movedr(1) .ne. 0) call scorekp2
      return
      end
      subroutine scorekp1
crev  last revision 09/22/90
c
c     ******************************************************************
c     *                                                                *
c     *     scorekp is used to score the program's king safety.  it    *
c     *  analyzes the pawn structure around the king to determine if   *
c     *  it is in a safe or dangerous position.                        *
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
      equivalence
     * (pkingl, kloc(1)), (okingl, kloc(2)),
     * (prank, krank(1)), (orank, krank(2)),
     * (pfile, kfile(1)), (ofile, kfile(2))
c
c
c
c
c------------------------------< determine if the king should
c------------------------------< be concerned about safety by
c------------------------------< counting enemy pieces and the queen.
c
      if(nopcs .gt. 15) then
c
c------------------------------< first, determine which files are
c------------------------------< important to king safety.  the files
c------------------------------< immediately to either side plus
c------------------------------< the file the king is on is important.
c
          ffile=min0(max0(pfile-1,2),7)
          lfile=max0(min0(pfile+1,9),4)
c
c------------------------------< determine if the king's position is
c------------------------------< weak due to weak squares (holes) in
c------------------------------< the king's pawn structure.
c
          if(prank .le. 3) then
              psafety=ksafety(pfile)*2
          else
              psafety=ksafety(pfile)*(4-prank)
          endif
          do 100 i=ffile,lfile
              if(board(i+29).ne.1 .and. 
     *           board(i+31).ne.1) psafety=psafety-khole
100       continue
c
c------------------------------< determine if the king's position is
c------------------------------< weak due to too many pawns being
c------------------------------< advanced in front of the king.
c
          if((board(ffile+30).ne.1.and.board(ffile+31).ne.1) .or.
     *       (board(lfile+30).ne.1.and.board(lfile+29).ne.1))
     *                     psafety=psafety-kpawns
c
c------------------------------< determine if there are open files
c------------------------------< around the king.  also, open files
c------------------------------< closer to the edge of the board are
c------------------------------< more dangerous than central ones.
c------------------------------< in addition, if the opponent has
c------------------------------< a half-open file, the file is
c------------------------------< dangerous since it is a route for
c------------------------------< rook attacks.  if the program has
c------------------------------< a half-open file, it is dangerous
c------------------------------< since the opponent can fully open
c------------------------------< it very easily.
c
          do 200 i=ffile,lfile
              if(pcount(i)+ocount(i) .eq. 0) then
                  psafety=psafety-ishft(atkfil(i),1)
              else if(pcount(i) .eq. 0) then
                  psafety=psafety-atkfil(i)
              else if(ocount(i) .eq. 0) then
                  psafety=psafety-atkfil(i)
              endif
              if(pfirst(i) .gt. 4) psafety=psafety-ishft(atkfil(i),-1)
200       continue
c
c------------------------------< penalize a king that is trapped on
c------------------------------< the back rank with no 'breathing
c------------------------------< space' on the rank in front of it.
c------------------------------< that is, avoid back rank mating
c------------------------------< situations whenever possible.
c
          do 300 i=ffile,lfile
              if(board(i+30) .ne. 1) go to 400
300       continue
          psafety=psafety-kluft
400       continue
c
c------------------------------< now compute the positional score for
c------------------------------< the king safety term. also, remember
c------------------------------< how exposed the king is so that other
c------------------------------< pieces will be attracted to an exposed
c------------------------------< king.
c
          pscore=pscore+psafety*ishft(nopcs,-1)
      else
c
c------------------------------< king safety is not important,
c------------------------------< attract the king to the center.
c------------------------------< also, attract the king toward the
c------------------------------< important pawns on the board.  if
c------------------------------< passed pawns exist, the king will
c------------------------------< move toward them in an effort to
c------------------------------< capture/defend them.  if none are
c------------------------------< on the board, then attract the king
c------------------------------< toward the center of all pawns.
c
          psafety=kcenter*center(pkingl)
          pscore=pscore+psafety
      endif
      htable(kkey1)=ishft(khash,-24)+ishft(psafety+32767,48)
      return
      end
      subroutine scorekp2
crev  last revision 09/16/90
c
c     ******************************************************************
c     *                                                                *
c     *     scorekp2 is used to score the program's king safety.  it   *
c     *  determines if the program has given up the right to castle    *
c     *  by moving rooks/king, which traps the king in the center of   *
c     *  the board.                                                    *
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
c------------------------------< evaluate development. until the king
c------------------------------< has safely castled, penalize moves
c------------------------------< that move the king or rooks.
c
      if(nopcs .gt. 15) then
          limit=odd(ply)
          if(limit .ge. 1) then
              if(moveds(limit) .ne. 0) then
                  if(and(moveds(limit),color) .eq. 0) 
     *                                 pscore=pscore-knocastk*nopcs
              else
                  do 100 temp=1,ply-1,2
                      if(type(temp) .eq. castkg) go to 200
                      if(type(temp) .eq. castqn) go to 200
100               continue
                  pscore=pscore-knocast*nppcs
              endif
          endif
200       continue
      endif
      return
      end
      subroutine scorem
crev  last revision 03/27/91
c
c     ******************************************************************
c     *                                                                *
c     *      scorem is a special scoring function used to force check- *
c     *  mate.  it is called whenever there are no pawns left to       *
c     *  promote.  scorem tries to accomplish three goals:             *
c     *                                                                *
c     *      1)  drive the losing king to the edge of the board and    *
c     *          eventually to a corner.                               *
c     *                                                                *
c     *      2)  attract all pieces toward the kings for offense       *
c     *          or defense.                                           *
c     *                                                                *
c     *      3)  keep the winning king two squares away from the       *
c     *          edge of the board to make checkmating easier.         *
c     *                                                                *
c     *      the reason for having no pawns is simplicity.  it is much *
c     *  easier to deliver mate with one or more queens than with only *
c     *  a rook and bishop when both sides have material.  it is also  *
c     *  easy to blunder and let the losing side queen a pawn and      *
c     *  clutch defeat from the jaws of victory.                       *
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
      equivalence
     * (pkingl, kloc(1)), (okingl, kloc(2)),
     * (prank, krank(1)), (orank, krank(2)),
     * (pfile, kfile(1)), (ofile, kfile(2))
c
c
c------------------------------< determine the rank and file of
c------------------------------< the winning and losing side and
c------------------------------< the color (sign1) of the winning
c------------------------------< side.
c
          if(mscore .ge. 0) then
              sign1=1
              sign5=5
              wrank=prank
              wfile=pfile
              lrank=orank
              lfile=ofile
              lsquare=okingl
          else
              sign1=-1
              sign5=-5
              wrank=orank
              wfile=ofile
              lrank=prank
              lfile=pfile
              lsquare=pkingl
          endif
c
c------------------------------< the king on the side with less material
c------------------------------< should try to stay in the center of the
c------------------------------< board and avoid the edges and
c------------------------------< particularly avoid the corners.
c
          pscore=pscore+sign1*matebd(lsquare)
c
c------------------------------< all pieces should move toward the
c------------------------------< losing king for attack or defense.
c
          do 300 where=1,pend(2)
              if(plist(where).eq.0) go to 300
              rank=cranks(plist(where))
              file=cfiles(plist(where))
              pscore=pscore-sign5*(iabs(lfile-file)+iabs(lrank-rank))
300       continue
c
c------------------------------< the king should try to stay on
c------------------------------< a rank or file two squares away
c------------------------------< from the edge of the board so
c------------------------------< that checkmate is easier to find.
c
          if(wfile.eq.4 .or. wfile.eq.7) pscore=pscore+sign5
          if(wrank.eq.4 .or. wrank.eq.7) pscore=pscore+sign5
c
c------------------------------< now add in the positional score
c------------------------------< computed to the material balance
c------------------------------< score to compute the net worth
c------------------------------< of the current position.
c
          tscore=mscore+pscore
          if(pscore .lt. minmax(1)) minmax(1)=pscore
          if(pscore .gt. minmax(2)) minmax(2)=pscore
          if(nppcs-nopcs .le. -5) tscore=tscore-10000
          if(nopcs-nppcs .le. -5) tscore=tscore+10000
          return
      end
      subroutine scorep
crev  last revision 02/13/91
c
c     ******************************************************************
c     *                                                                *
c     *      scorep is called to determine if this pawn position has   *
c     *  been evaluated earlier.  if so, and certain special cases     *
c     *  don't cause otherwise, scorep will return the score found in  *
c     *  the pawn position hash table.                                 *
c     *      sometimes, even if the position is found, some additional *
c     *  testing must be done by scorep2 to correctly evaluate the     *
c     *  position.  if there are no pieces, scorep2 is always called   *
c     *  to check for pawn races, etc.                                 *
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
      data bmask / o'17777777777777' /
      equivalence
     * (pkingl, kloc(1)), (okingl, kloc(2)),
     * (prank, krank(1)), (orank, krank(2)),
     * (pfile, kfile(1)), (ofile, kfile(2))
c
c
c
c
c------------------------------< determine if this pawn configuration
c------------------------------< has already been evaluated.  if so,
c------------------------------< the pawn structure hash table already
c------------------------------< has the correct evaluation.
c
      pcalls=pcalls+1
      tradepsc=0
      pkey1=ptbl1+and(phash,psizm1)
      if(and(htable(pkey1),bmask) .eq. ishft(phash,-24)) then
          rpflag=and(ishft(htable(pkey1),-62),3)
          pscore=pscore+and(ishft(htable(pkey1),-45),65535)-32767
          pkey2=ptbl2+ishft(and(phash,psizm1),-1)
          pshft=ishft(and(phash,1),5)
          ptemp=and(ishft(htable(pkey2),-pshft),2**32-1)
          ppassed=and(ptemp,255)
          opassed=and(ptemp,ishft(255,8))
          do 100 i=2,9
              ppass(i)=and(ptemp,1)
              ptemp=ishft(ptemp,-1)
100       continue
          do 200 i=2,9
              opass(i)=and(ptemp,1)
              ptemp=ishft(ptemp,-1)
200       continue
          do 300 i=2,9
              pweakp(i)=and(ptemp,1)
              ptemp=ishft(ptemp,-1)
300       continue
          do 400 i=2,9
              oweakp(i)=and(ptemp,1)
              ptemp=ishft(ptemp,-1)
400       continue
          phashs=phashs+1
      if(ppassed.ne.0 .and. nopcs.eq.0 .or.
     *   opassed.ne.0 .and. nppcs.eq.0) then
          call scorep2
      endif
      if(ppassed+opassed .ne. 0) then
          call scorep3
      endif
              return
      endif
          call scorep1
      call scorep1
      if(ppassed.ne.0 .and. nopcs.eq.0 .or.
     *   opassed.ne.0 .and. nppcs.eq.0) then
          call scorep2
      endif
      if(ppassed+opassed .ne. 0) then
          call scorep3
      endif
      return
      end
      subroutine scorep1
crev  last revision 02/13/91
c
c     ******************************************************************
c     *                                                                *
c     *      scorep1 is called to evaluate the pawn structure for      *
c     *  each side.  It uses the dynamic data updated by make/unmake   *
c     *  to analyze the overall pawn structure for both sides.  It     *
c     *  analyzes advancement, isolated, doubled, backward, connected, *
c     *  passed, and other types of pawns according to well known      *
c     *  principles.                                                   *
c     *      for speed, anything calculated by scorep1 is stored in    *
c     *  a pawn structure hash table since nothing in here is based on *
c     *  the position of any piece, only pawns.  Since some of the     *
c     *  endgame logic depends on king location, etc.  these are not   *
c     *  included in this routine but are included in scorep2.         *
c     *                                                                *
c     *                       W A R N I N G                            *
c     *      since the hashing key is only based on the position of    *
c     *  pawns, NOTHING but pawn positions can be used in this module  *
c     *  to compute evaluations.  Using ANYTHING else will result in   *
c     *  incorrect scores from the hash table!                         *
c     *      if some new scoring idea is being implemented, it should  *
c     *  be put in scorep2, or in a new module called scorep3 if       *
c     *  it is necessary to violate the above rule.                    *
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
      equivalence
     * (pkingl, kloc(1)), (okingl, kloc(2)),
     * (prank, krank(1)), (orank, krank(2)),
     * (pfile, kfile(1)), (ofile, kfile(2))
c
c
c
c
c------------------------------< initialize.
c
      pcount(1)=0
      ocount(1)=0
      pcount(10)=0
      ocount(10)=0
      npass=0
      rams=0
      ppassed=0
      opassed=0
      piso=0
      pisoof=0
      oiso=0
      oisoof=0
c
c------------------------------< determine if either side has a king
c------------------------------< or queen pawn that hasn't been pushed.
c------------------------------< if so, give a penalty since this has
c------------------------------< a negative effect on development.
c
      if(board(35).eq.1 .and. board(36).eq.1) pscore=pscore-kaqmov
      if(board(35).eq.1 .or. board(36).eq.1) pscore=pscore-koqmov
      if(board(85).eq.-1 .and. board(86).eq.-1) pscore=pscore+kaqmov
      if(board(85).eq.-1 .or. board(86).eq.-1) pscore=pscore+koqmov
c
c------------------------------< count the number of pawns the program
c------------------------------< has.  if there are eight (8), then
c------------------------------< penalize the score to encourage the
c------------------------------< program to make some sort of break to
c------------------------------< open things up.
c
      if(nppwns .eq. 8) pscore=pscore-pen8p
c
c------------------------------< analyze the relationships between
c------------------------------< pawns to compute a relative score 
c------------------------------< for the pawn structure.
c
      do 1400 file=2,9
          ppass(file)=0
          pweakp(file)=0
          opass(file)=0
          oweakp(file)=0
          countp=pcount(file)
          counto=ocount(file)
          lastp=plast(file)
          lasto=olast(file)
c
c------------------------------< score pawns based on how many ranks
c------------------------------< they have been advanced. only score
c------------------------------< the first and last on a file..ignore
c------------------------------< the others (only present when tripled,
c------------------------------< a severe weakness anyway).
c
          if(countp .ne. 0) then
              pscore=pscore+ppawnb(lastp*10+file)
c
c------------------------------< since the program plays open positions
c------------------------------< better than closed ones, penalize any
c------------------------------< 'locked' pawns (sometimes called pawn
c------------------------------< rams) to favor open positions.
c
              if(lastp .eq. lasto-1) rams=rams+1
c
c------------------------------< determine if more than one pawn
c------------------------------< is on this file. if so, penalize
c------------------------------< the score for doubled/tripled pawns.
c
              if(countp .eq. 2) pscore=pscore-pdouble
              if(countp .ge. 3) pscore=pscore-ptriple
c
c------------------------------< determine if the pawn on this file is
c------------------------------< isolated. if so, the pawn is weak and
c------------------------------< receives a penalty. if the pawn is on
c------------------------------< a half-open file, it is still weaker
c------------------------------< and receives a larger penalty.
c
              if(pcount(file-1).eq.0 .and. pcount(file+1).eq.0) then
                  piso=piso+countp
                  pweakp(file)=1
                  if(counto .eq. 0) then
                      pisoof=pisoof+countp
                  else
                      if(lastp .gt. ofirst(file)) pisoof=pisoof+3
                  endif
                  go to 300
              endif
c
c------------------------------< recognize a pawn that has been advanced
c------------------------------< far enough that no neighboring pawn can
c------------------------------< advance far enough to protect it in one
c------------------------------< move.  Therefore, it is 'psuedo' iso-
c------------------------------< lated.
c
              if(plast(file)-plast(file-1).gt.2 .and.
     *           plast(file)-plast(file+1).gt.2) then
                  piso=piso+1
                  pweakp(file)=1
                  if(counto .eq. 0) then
                      pisoof=pisoof+1
                  else
                      if(lastp .gt. ofirst(file)) pisoof=pisoof+3
                  endif
              else if((plast(file)-plast(file-1).eq.2 .and.
     *           olast(file-2).eq.plast(file)) .or.
     *           (plast(file)-plast(file+1).eq.2 .and.
     *           olast(file+2).eq.plast(file))) then
                  piso=piso+1
                  pweakp(file)=1
                  if(counto .eq. 0) then
                      pisoof=pisoof+1
                  else
                      if(lastp .gt. ofirst(file)) pisoof=pisoof+3
                  endif
              endif
c
c------------------------------< determine if the most advanced
c------------------------------< pawn on this file is backward and
c------------------------------< on an open file. if so, it is
c------------------------------< very weak and receives a penalty.
c
              if(pfirst(file-1).gt.lastp .and.
     *           pfirst(file+1).gt.lastp .and.
     *           (board(lastp*10+file+19).eq.-1 .or.
     *            board(lastp*10+file+21).eq.-1)) then
                  pweakp(file)=1
                  piso=piso+1
                  if(counto .eq. 0) then
                      pisoof=pisoof+1
                  endif
              endif
300           continue
c
c------------------------------< determine if the most advanced pawn
c------------------------------< on this file is passed. if no enemy
c------------------------------< pawns are in front of it either on
c------------------------------< this or an adjacent file, it is passed
c------------------------------< and valuable.
c
              if(lastp.ge.ofirst(file) .and.
     *           lastp.ge.ofirst(file-1) .and.
     *           lastp.ge.ofirst(file+1)) then
                  ppass(file)=1
                  ppassed=1
                  npass=npass+1
              endif
          endif
c
c------------------------------< score pawns based on how many ranks
c------------------------------< they have been advanced. only score
c------------------------------< the first and last on a file..ignore
c------------------------------< the others (only present when tripled,
c------------------------------< a severe weakness anyway).
c
          if(counto .ne. 0) then
              pscore=pscore-opawnb(lasto*10+file)
c
c------------------------------< determine if more than one pawn
c------------------------------< is on this file. if so, penalize
c------------------------------< the score for doubled/tripled pawns.
c
              if(counto .eq. 2) pscore=pscore+pdouble
              if(counto .ge. 3) pscore=pscore+ptriple
c
c------------------------------< determine if the pawn on this file is
c------------------------------< isolated. if so, the pawn is weak and
c------------------------------< receives a penalty. if the pawn is on
c------------------------------< a half-open file, it is still weaker
c------------------------------< and receives a larger penalty.
c
              if(ocount(file-1).eq.0 .and. ocount(file+1).eq.0) then
                  oweakp(file)=1
                  oiso=oiso+counto
                  if(countp .eq. 0) then
                      oisoof=oisoof+counto
                  else
                      if(lasto .lt. pfirst(file)) oisoof=oisoof+3
                  endif
                  go to 1000
              endif
c
c------------------------------< recognize a pawn that has been advanced
c------------------------------< far enough that no neighboring pawn can
c------------------------------< advance far enough to protect it in one
c------------------------------< move.  Therefore, it is 'psuedo' iso-
c------------------------------< lated.
c
              if(olast(file-1)-olast(file).gt.2 .and.
     *           olast(file+1)-olast(file).gt.2) then
                  oweakp(file)=1
                  oiso=oiso+1
                  if(countp .eq. 0) then
                      oisoof=oisoof+counto
                  else
                      if(lasto .lt. pfirst(file)) oisoof=oisoof+3
                  endif
              else if((olast(file-1)-olast(file).eq.2 .and.
     *            plast(file-2).eq.olast(file)) .or.
     *           (olast(file+1)-olast(file).eq.2 .and.
     *            plast(file+2).eq.olast(file))) then
                  oweakp(file)=1
                  oiso=oiso+1
                  if(countp .eq. 0) then
                      oisoof=oisoof+counto
                  else
                      if(lasto .lt. pfirst(file)) oisoof=oisoof+3
                  endif
              endif
c
c------------------------------< determine if the most advanced
c------------------------------< pawn on this file is backward and
c------------------------------< on an open file. if so, it is
c------------------------------< very weak and receives a penalty.
c
              if(ofirst(file-1).lt.lasto .and.
     *           ofirst(file+1).lt.lasto .and.
     *           (board(lasto*10+file-19).eq.1 .or.
     *            board(lasto*10+file-21).eq.1)) then
                  oweakp(file)=1
                  oiso=oiso+1
                  if(countp .eq. 0) then
                      oisoof=oisoof+1
                  endif
              endif
1000          continue
c
c------------------------------< determine if the most advanced pawn
c------------------------------< on this file is passed. if no enemy
c------------------------------< pawns are in front of it either on
c------------------------------< this or an adjacent file, it is passed
c------------------------------< and valuable.
c
              if(lasto.le.pfirst(file) .and.
     *           lasto.le.pfirst(file-1) .and.
     *           lasto.le.pfirst(file+1)) then
                  opass(file)=1
                  opassed=1
                  npass=npass+1
              endif
          endif
1400  continue
c
c------------------------------< now compute the penalty for isolated
c------------------------------< pawns.  it is more or less exponential
c------------------------------< based on the number of them. note 
c------------------------------< "psuedo-isolated" pawns only count
c------------------------------< as 1/2 of a regular isolated pawn.
c
      piso=min(piso,8)
      oiso=min(oiso,8)
      pisoof=min(pisoof,8)
      oisoof=min(oisoof,8)
      pscore=pscore-isolat(piso)-isolatof(pisoof)
     *             +isolat(oiso)+isolatof(oisoof)
c
c------------------------------< evaluate a position to determine if
c------------------------------< one side has won by the rule of having
c------------------------------< the 'most distant' passed pawn.
c
      avfile=ifix((float(afile)+.5)/float(npawns))
      pmax=-100
      omax=-100
      pprot=0
      oprot=0
      do 1600 file=2,9
          if(ppass(file) .ne. 0) then
              if(pmax .le. iabs(file-avfile)) then
                  pmax=iabs(file-avfile)
                  sq=plast(file)*10+file
                  pprot=0
                  if(board(sq-9).eq.1 .or. board(sq-11).eq.1) pprot=1
              endif
          endif
          if(opass(file) .ne. 0) then
              if(omax .le. iabs(file-avfile)) then
                  omax=iabs(file-avfile)
                  sq=olast(file)*10+file
                  oprot=0
                  if(board(sq+9).eq.-1 .or. board(sq+11).eq.-1) oprot=1
              endif
          endif
1600  continue
      if(pmax .gt. omax) then
          if(pmax.ge.2 .and. oprot.eq.0) pscore=pscore+poutside
      else if(omax .gt. pmax) then
          if(omax.ge.2 .and. pprot.eq.0) pscore=pscore-poutside
      endif
      pscore=pscore-max0(rams-1,0)*prams
      temp=0
      flag1=ppass(2)
      flag2=ishft(opass(2),8)
      flag3=ishft(pweakp(2),16)
      flag4=ishft(oweakp(2),24)
      do 1900 i=2,9
          temp=temp+flag1+flag2+flag3+flag4
          flag1=ishft(ppass(i+1),i-1)
          flag2=ishft(opass(i+1),i+7)
          flag3=ishft(pweakp(i+1),16+i-1)
          flag4=ishft(oweakp(i+1),16+i+7)
1900  continue
      rpflag=0
      if(nppwns.eq.0 .or. nopwns.eq.0) then
          if(and(temp,32382) .eq. 0) then
              rpflag=ishft(and(temp,32768),-12)+
     *               ishft(and(temp,384),-6)+and(temp,1)
              rpflag=and(rpflag,3)+ishft(rpflag,-2)
          endif
      endif
      pkey2=ptbl1+psize+ishft(and(phash,psizm1),-1)
      pshft=ishft(and(phash,1),5)
c     call lock21
      htable(pkey1)=ishft(phash,-24)+ishft(pscore+32767,45)
     *             +ishft(rpflag,62)
      htable(pkey2)=and(htable(pkey2),xor(ishft(2**32-1,pshft),-1))
     *             +ishft(temp,pshft)
c     call unlock21
      return
      end
      subroutine scorep2
crev  last revision 03/09/91
c
c     ******************************************************************
c     *                                                                *
c     *      scorep2 is called to evaluate the passed pawns whenever   *
c     *  it is determined that one side has a passed pawn(s) and the   *
c     *  other side has no pieces.  This is a case that cannot be      *
c     *  hashed since the king position is all-important in stopping   *
c     *  the pawn.                                                     *
c     *                                                                *
c     *      this routine depends on ppass(i) and opass(i) being set   *
c     *  upon entry, either from the hash table for the part of the    *
c     *  pawn scoring that can be hashed, or from scorep1 if it was    *
c     *  called.                                                       *
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
      equivalence
     * (pkingl, kloc(1)), (okingl, kloc(2)),
     * (prank, krank(1)), (orank, krank(2)),
     * (pfile, kfile(1)), (ofile, kfile(2))
c
c
c------------------------------< initialize.
c
      pcount(1)=0
      ocount(1)=0
      pcount(10)=0
      ocount(10)=0
      pqdist=100
      oqdist=100
c
c------------------------------< now analyze each pawn to see how
c------------------------------< far it has to go to become a new
c------------------------------< queen.
c
      do 300 file=2,9
          lastp=plast(file)
          lasto=olast(file)
c
c------------------------------< determine if any passed pawns exist. if
c------------------------------< so and the opposing side has no pieces
c------------------------------< other than the king, determine if the
c------------------------------< passed pawn can queen before the king
c------------------------------< can get to it. if so, remember the
c------------------------------< distance to the queening square.
c
          if(ppass(file).ne.0 .and. nopcs.eq.0) then
              pdist=9-lastp
              if(pfile.eq.file .and. prank.gt.lastp) then
                  pdist=pdist+1
                  if(file.eq.2 .or. file.eq.9) then
                      if(((prank.eq.8 .or. prank.eq.9) .and.
     *                    (orank.eq.8 .or. orank.eq.9)) .and.
     *                   iabs(ofile-pfile).eq.2) pdist=100
                  endif
              endif
              if(lastp .eq. 3) pdist=pdist-1
              kdist=max0(9-prank,iabs(pfile-file))
              odist=max0(9-orank,iabs(ofile-file))
              if(player .eq. 2) odist=odist-1
              if(odist .le. pdist) then
                  if((file.eq.2 .or. file.eq.9) .and.
     *               pfile.eq.file .and. prank.ge.lastp .and.
     *               iabs(prank-orank).le.player+1 .and.
     *               iabs(pfile-ofile).gt.player+1) go to 100
                  distk=max0(iabs(pfile-file),iabs(prank-lastp))
                  disto=max0(iabs(ofile-file),iabs(orank-lastp))
                  if(player .eq. 2) disto=disto-1
                  if(distk.gt.disto+1 .or.
     *               (kdist.ne.1 .and. kdist.gt.odist-2)) go to 100
              endif
              if(pqdist.gt.pdist .and. pdist.lt.10) then
                  pqdist=pdist
                  ppnsq=lastp*10+file
              endif
          endif
100       continue
          if(opass(file).ne.0 .and. nppcs.eq.0) then
              pdist=lasto-2
              if(ofile.eq.file .and. orank.lt.lasto) then
                  pdist=pdist+1
                  if(file.eq.2 .or. file.eq.9) then
                      if(((orank.eq.2 .or. orank.eq.3) .and.
     *                    (prank.eq.2 .or. prank.eq.3)) .and.
     *                   iabs(ofile-pfile).eq.2) pdist=100
                  endif
              endif
              if(lasto .eq. 8) pdist=pdist-1
              kdist=max0(orank-2,iabs(ofile-file))
              odist=max0(prank-2,iabs(pfile-file))
              if(player .eq. 1) odist=odist-1
              if(odist .le. pdist) then
                  if((file.eq.2 .or. file.eq.9) .and.
     *               ofile.eq.file .and. orank.gt.lasto .and.
     *               iabs(orank-prank).le.4-player .and.
     *               iabs(ofile-pfile).le.4-player) go to 200
                  distk=max0(iabs(ofile-file),iabs(orank-lasto))
                  disto=max0(iabs(pfile-file),iabs(prank-lasto))
                  if(player .eq. 1) disto=disto-1
                  if(distk.gt.disto+1 .or.
     *               (kdist.ne.1 .and. kdist.gt.odist-2)) go to 200
              endif
              if(oqdist.gt.pdist .and. pdist.lt.10) then
                  oqdist=pdist
                  opnsq=lasto*10+file
              endif
          endif
200       continue
300   continue
c
c------------------------------< now evaluate any passed pawns that can
c------------------------------< queen with no interference. if one pawn
c------------------------------< queens two or more moves before the
c------------------------------< opponent, give that side credit for a
c------------------------------< new queen. if they queen together, give
c------------------------------< no credit unless the first one queens
c------------------------------< with check.
c
      if(pqdist+oqdist .ne. 200) then
          ptemp=pqdist
          otemp=oqdist
          if(player .eq. 1) ptemp=ptemp-1
          if(player .eq. 2) otemp=otemp-1
          if(pqdist .lt. otemp) then
              pscore=pscore+pqueen
              return
          else if(pqdist .eq. otemp) then
              qnsq=90+cfiles(ppnsq)
              board(ppnsq)=0
              temp=board(qnsq)
              board(qnsq)=5
              if(ripoff(okingl) .ne. 0) then
                  board(ppnsq)=1
                  board(qnsq)=temp
                  pscore=pscore+pqueen
                  return
              else
                  oqnsq=20+cfiles(opnsq)
                  board(opnsq)=0
                  temp1=board(oqnsq)
                  board(oqnsq)=-5
                  if(ripoff(oqnsq) .ne. 0) then
                      pscore=pscore+pqueen
                  endif
                  board(opnsq)=-1
                  board(ppnsq)=1
                  board(qnsq)=temp
                  board(oqnsq)=temp1
              endif
          else if(oqdist .lt. ptemp) then
              pscore=pscore-pqueen
              return
          else if(oqdist .eq. ptemp) then
              qnsq=20+cfiles(opnsq)
              board(opnsq)=0
              temp=board(qnsq)
              board(qnsq)=-5
              if(ripoff(pkingl) .ne. 0) then
                  board(opnsq)=-1
                  board(qnsq)=temp
                  pscore=pscore-pqueen
                  return
              else
                  pqnsq=90+cfiles(ppnsq)
                  board(ppnsq)=0
                  temp1=board(pqnsq)
                  board(pqnsq)=5
                  if(ripoff(pqnsq) .ne. 0) then
                      pscore=pscore-pqueen
                  endif
                  board(opnsq)=-1
                  board(ppnsq)=1
                  board(qnsq)=temp
                  board(pqnsq)=temp1
              endif
          endif
      endif
c
c------------------------------< evaluate an ending with only one pawn.
c------------------------------< if the winning side's king is two ranks
c------------------------------< in front of the pawn (not rook pawn),
c------------------------------< the game is won. if the king is one
c------------------------------< square in front of the pawn with the
c------------------------------< opposition, the game is won, all others
c------------------------------< are evaluated as draws.
c
      if(nppcs+nopcs .ne. 0) return
      if(nopwns .eq. 0) then
          ppfile=afile
          if(ppfile.eq.2 .or. ppfile.eq.9) then
              if(pfile.ne.ppfile-1.and.pfile.ne.ppfile+1) return
              if(prank.ne.8 .and. prank.ne.9) return
          endif
          pprank=arank
          if(iabs(pfile-ppfile) .gt. 1) return
          if(orank-prank .lt. 2) go to 900
          if(pprank-orank .ge. 2) go to 900
          if(prank-pprank .ge. 2) go to 900
          if(prank-pprank .ne. 1) return
          if(player .ne. 2) then
              do 700 i=17,24
                  tkingl=pkingl+movdir(i)
                  if(board(tkingl) .gt. 0) go to 700
                  tfile=cfiles(tkingl)
                  trank=cranks(tkingl)
                  if(tfile .ne. ofile) go to 700
                  if(and(iabs(trank-orank),1).eq.0) go to 800
700           continue
              return
          endif
          if(iabs(pfile-ofile).ne.2 .or.
     *       iabs(prank-orank).ne.2) then
              if(pfile.ne.ofile .or.
     *           and(iabs(prank-orank),1).ne.0) return
          endif
800       continue
              if(ppfile .ne. pfile) pscore=pscore-popose
900       continue
          if(iabs(ofile-ppfile) .le. 1) then
              if(orank.lt.prank .and. orank.gt.pprank) return
          endif
          pscore=pscore+pwonkp
          if(prank .lt. 8) return
          if(iabs(pfile-ppfile) .le. 1) pscore=pscore+pkclose
          return
      endif
      opfile=afile
      if(opfile.eq.2 .or. opfile.eq.9) then
          if(ofile.ne.opfile-1.and.ofile.ne.opfile+1) return
          if(orank.ne.3 .and. orank.ne.2) return
      endif
      oprank=arank
      if(iabs(ofile-opfile) .gt. 1) return
      if(orank-prank.ge.2 .and. prank-oprank.lt.2 .and.
     *   oprank-orank.lt.2) then
          if(oprank-orank .ne. 1) return
          if(player .ne. 1) then
              do 1000 i=17,24
                  tkingl=okingl+movdir(i)
                  if(board(tkingl) .lt. 0) go to 1000
                  tfile=cfiles(tkingl)
                  trank=cranks(tkingl)
                  if(tfile .ne. pfile) go to 1000
                  if(and(iabs(trank-prank),1) .eq. 0)
     *                                        go to 1100
1000          continue
              return
          endif
          if(iabs(ofile-pfile).ne.2 .or.
     *       iabs(orank-prank).ne.2) then
              if(ofile.ne.pfile .or.
     *           and(iabs(orank-prank),1).ne.0) return
          endif
1100      continue
          if(opfile .ne. ofile) pscore=pscore+popose
      endif
      if(iabs(pfile-opfile) .le. 1) then
          if(prank.gt.orank .and. prank.lt.oprank) return
      endif
      pscore=pscore-pwonkp
      if(orank .gt. 3) return
      if(iabs(ofile-opfile) .le. 1) pscore=pscore-pkclose
      return
      end
      subroutine scorep3
crev  last revision 09/21/90
c
c     ******************************************************************
c     *                                                                *
c     *      scorep3 is called to determine if this pawn position has  *
c     *  passed pawns.  if so, it determines how valuable they are     *
c     *  relative to their advancement.                                *
c     *      the major consideration here is that if a pawn is both    *
c     *  passed and isolated, then it is not very valuable until the   *
c     *  number of opponent pieces drops below the point where the     *
c     *  pawn can be easily lost.                                      *
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
      do 100 file=2,9
          lastp=plast(file)
          lasto=olast(file)
          padvance=lastp*10+file
          oadvance=lasto*10+file
          if(ppass(file) .ne. 0) then
c
c------------------------------< determine if the pawn on this file is
c------------------------------< isolated. if so, the pawn is weak and
c------------------------------< receives a penalty. if the pawn is on
c------------------------------< a half-open file, it is still weaker
c------------------------------< and receives a larger penalty.
c
              pisolat=0
              if(pcount(file-1).eq.0 .and. pcount(file+1).eq.0) then
                  pisolat=1
              endif
c
c------------------------------< recognize a pawn that has been advanced
c------------------------------< far enough that no neighboring pawn can
c------------------------------< advance far enough to protect it in one
c------------------------------< move.  Therefore, it is 'psuedo' iso-
c------------------------------< lated.
c
              if(plast(file)-plast(file-1).gt.2 .and.
     *           plast(file)-plast(file+1).gt.2) then
                  pisolat=1
              endif
c
c------------------------------< determine if the passed pawn on this
c------------------------------< file is valuable.  if it is isolated
c------------------------------< and too many enemy pieces are still
c------------------------------< on the board, then it is not so
c------------------------------< dependable.
c
              passed=ppasspn
              if(iabs(plast(file-1)-lastp).lt.2 .or.
     *           iabs(plast(file+1)-lastp).lt.2) passed=passed+ppawncon
              ppassc=passed*(lastp-2)*ppawnrnk
              if(pisolat .eq. 0) then
                  pscore=pscore+ppassc
              else if(nopcs .le. 16) then
                  pscore=pscore+ppassc
              else
                  tradepsc=tradepsc+ppassc
              endif
c
c------------------------------< determine if the passed pawn is
c------------------------------< blockaded.  if it is isolated, then
c------------------------------< any blockader is good.  If it is not
c------------------------------< isolated, a bishop, queen or king is
c------------------------------< good but a rook or knight are not as
c------------------------------< good since a pawn can drive them away.
c
              if(board(padvance+10) .ne. 0) then
                  if(pcount(file-1).eq.0 .and. pcount(file+1).eq.0) then
                      pscore=pscore-iblocker(board(padvance+10))
                  else
                      pscore=pscore-blocker(board(padvance+10))
                  endif
              endif
          endif
          if(opass(file) .ne. 0) then
c
c------------------------------< determine if the pawn on this file is
c------------------------------< isolated. if so, the pawn is weak and
c------------------------------< receives a penalty. if the pawn is on
c------------------------------< a half-open file, it is still weaker
c------------------------------< and receives a larger penalty.
c
              oisolat=0
              if(ocount(file-1).eq.0 .and. ocount(file+1).eq.0) then
                  oisolat=1
              endif
c
c------------------------------< recognize a pawn that has been advanced
c------------------------------< far enough that no neighboring pawn can
c------------------------------< advance far enough to protect it in one
c------------------------------< move.  Therefore, it is 'psuedo' iso-
c------------------------------< lated.
c
              if(olast(file-1)-olast(file).gt.2 .and.
     *           olast(file+1)-olast(file).gt.2) then
                  oisolat=1
              endif
c
c------------------------------< determine if the passed pawn on this
c------------------------------< file is valuable.  if it is isolated
c------------------------------< and too many enemy pieces are still
c------------------------------< on the board, then it is not so
c------------------------------< dependable.
c
              passed=ppasspn
              if(iabs(olast(file-1)-lasto).lt.2 .or.
     *           iabs(olast(file+1)-lasto).lt.2) passed=passed+ppawncon
              opassc=passed*(9-lasto)*ppawnrnk
              if(oisolat .eq. 0) then
                  pscore=pscore-opassc
              else if(nppcs .le. 16) then
                  pscore=pscore-opassc
              else
                  tradepsc=tradepsc-opassc
              endif
c
c------------------------------< determine if the passed pawn is
c------------------------------< blockaded.  if it is isolated, then
c------------------------------< any blockader is good.  If it is not
c------------------------------< isolated, a bishop, queen or king is
c------------------------------< good but a rook or knight are not as
c------------------------------< good since a pawn can drive them away.
c
              if(board(oadvance-10) .ne. 0) then
                  if(ocount(file-1).eq.0 .and. ocount(file+1).eq.0) then
                      pscore=pscore+iblocker(board(oadvance-10))
                  else
                      pscore=pscore+blocker(board(oadvance-10))
                  endif
              endif
          endif
100   continue
      return
      end
      subroutine scoretrd
crev  last revision 03/02/91
c
c     ******************************************************************
c     *                                                                *
c     *      scoretrd is called to determine if it is beneficial to    *
c     *  trade (exchange) pieces and pawns (or not.)  it uses the      *
c     *  following logic:                                              *
c     *                                                                *
c     *   1.  if one side is a piece or more ahead, then that side     *
c     *     should trade pieces whenever possible.  If a piece behind, *
c     *     then trades should be avoided if possible.                 *
c     *                                                                *
c     *   2.  If neither side is a piece ahead, then use the pawn      *
c     *     scoring logic (scorep, etc.) to compute the pawn score as  *
c     *     if there were no pieces on the board.  If this score is    *
c     *     above the threshold (tradpwin), then that side should try  *
c     *     to trade pieces.  if the pawn score is below -tradpwin,    *
c     *     then that side should try to avoid trading pieces.         *
c     *                                                                *
c     *   3.  The inverse of this logic is applied to pawn trades,     *
c     *     effectively implementing the chess praxis "when ahead      *
c     *     trade pieces but not pawns, and when behind trade pawns    *
c     *     but not pieces."                                           *
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
c------------------------------< evaluate trading pieces.  if we
c------------------------------< are a piece ahead/behind, trading
c------------------------------< is good/bad.  If not, check the
c------------------------------< pawn score.  If it is above the
c------------------------------< tradpwin value, encourage trading
c------------------------------< since the resulting endgame looks bad.
c------------------------------< if it is below -tradpwin then
c------------------------------< discourage trading since the
c------------------------------< resulting endgame looks bad.
c
          tradeb=0
          tradepsc=tradepsc+pscore
          if(mscore .ge. 3000) then
              tradeb=tradown
          else if(mscore .le. -3000) then
              tradeb=-tradown
          else if(tradepsc .gt. tradpwin) then 
              tradeb=tradown
          else if(tradepsc .lt. -tradpwin) then
              tradeb=-tradown
          endif
c
c------------------------------< scan the move list to analyze trades
c------------------------------< of both pieces and pawns.  the value
c------------------------------< of "tradeb" indicates "trading pieces
c------------------------------< is good, pawns is bad" (positive) and
c------------------------------< "trading pieces is bad, pawns is good"
c------------------------------< (negative).
c
      if(tradeb .gt. 0) then
          do 100 i=1,ply-1,2
              if(cappc(i) .lt. -1) then
                  pscore=pscore+tradeb
              else if(cappc(i) .eq. -1) then
                  pscore=pscore-tradeb
              endif
100       continue
      else if(tradeb .lt. 0) then
          do 200 i=2,ply-1,2
              if(cappc(i) .gt. 1) then
                  pscore=pscore+tradeb
              else if(cappc(i) .eq. 1) then
                  pscore=pscore-tradeb
              endif
200       continue
      endif
      return
      end
