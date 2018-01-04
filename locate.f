      subroutine locate
crev  last revision 10/08/90
c
c     ******************************************************************
c     *                                                                *
c     *      locate is called to set up various tables for use by the  *
c     *  scoring functions.  it scans the board to locate pawns and    *
c     *  pieces and stores information about piece/pawn location in    *
c     *  tables for later use.  the kings are located so that tropism  *
c     *  can be evaluated later also.  pieces are counted for use in   *
c     *  evaluating king safety later.                                 *
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
      data zero, one / 0, 1 /
c
c
c------------------------------< first, analyze the pawn structure to
c------------------------------< determine the following factors:
c------------------------------< (1) how many pawns are on each file,
c------------------------------< (2) the rank of the first pawn on each
c------------------------------< file, (3) the rank of the last pawn on
c------------------------------< each file. these will be used to
c------------------------------< determine the pawn scoring.
c------------------------------< also, count pieces and pawns
c------------------------------< for each side so that king
c------------------------------< safety can be evaluated later.
c
      nppwns=0
      nppcs=0
      nopwns=0
      nopcs=0
      arank=0
      afile=0
c
c------------------------------< process each rank.
c
      do 1400 sq=22,29
          file=sq-20
          pfirst(file)=100
          plast(file)=0
          pcount(file)=0
          ofirst(file)=0
          olast(file)=100
          ocount(file)=0
c
c------------------------------< process each file
c
          do 1300 square=sq,100,10
              rank=cranks(square)
              go to (1200,1100,1000,900,800,700,1300,
     *               100,200,300,400,500,600),board(square)+7
c
c------------------------------< program pieces are counted by
c------------------------------< this routine.
c
100           continue
                  nppwns=nppwns+1
                  pcount(file)=pcount(file)+1
                  plast(file)=rank
                  if(pfirst(file) .eq. 100) pfirst(file)=rank
                  arank=arank+rank
                  afile=afile+file
                  go to 1300
200           continue
                  nppcs=nppcs+2
                  go to 1300
300           continue
                  nppcs=nppcs+3
                  go to 1300
400           continue
                  nppcs=nppcs+5
                  go to 1300
500           continue
                  nppcs=nppcs+9
                  go to 1300
600           continue
                  kloc(1)=square
                  krank(1)=rank
                  kfile(1)=file
                  go to 1300
c
c------------------------------< opponent pieces are counted by
c------------------------------< this routine
c
700           continue
                  nopwns=nopwns+1
                  ocount(file)=ocount(file)+1
                  ofirst(file)=rank
                  if(olast(file) .eq. 100) olast(file)=rank
                  arank=arank+rank
                  afile=afile+file
                  go to 1300
800           continue
                  nopcs=nopcs+2
                  go to 1300
900           continue
                  nopcs=nopcs+3
                  go to 1300
1000          continue
                  nopcs=nopcs+5
                  go to 1300
1100          continue
                  nopcs=nopcs+9
                  go to 1300
1200          continue
                  kloc(2)=square
                  krank(2)=rank
                  kfile(2)=file
1300      continue
1400  continue
      npawns=nppwns+nopwns
c
c------------------------------< set the movgen boards up so that squares
c------------------------------< adjacent to the target area get the
c------------------------------< highest value. squares one rank or
c------------------------------< file away get the next highest values,
c------------------------------< etc.
c
      do 1450 sq=22,99
          xpmovgen(sq)=9-max0(iabs(cranks(sq)-krank(1))+1,
     *                          iabs(cfiles(sq)-kfile(1))+1)
          xomovgen(sq)=9-max0(iabs(cranks(sq)-krank(2))+1,
     *                          iabs(cfiles(sq)-kfile(2))+1)
1450  continue
c
c------------------------------< now set up the piece lists so that they
c------------------------------< will point to all pieces on the
c------------------------------< board to avoid searching the entire
c------------------------------< board to locate the pieces.
c
      do 1500 i=22,99
          pboard(i)=33
1500  continue
c
c------------------------------< now set up the piece location lists
c------------------------------< so that pieces come first for both
c------------------------------< sides, followed by pawns.  also, the
c------------------------------< pieces nearest the opponent's king
c------------------------------< and most advanced pawns are first.
c
      pstart(1)=1
      pend(1)=0
      do 1700 kp=7,1,-1
          do 1600 where=99,22,-1
              if(board(where).gt.1 .and. board(where).le.6 .and.
     *           xomovgen(where).eq.kp) then
                  pend(1)=pend(1)+1
                  plist(pend(1))=where
                  vlist(pend(1))=board(where)
                  pboard(where)=pend(1)
              endif
1600      continue
1700  continue
      pcend(1)=pend(1)
      pwnstrt(1)=pend(1)+1
      do 1800 where=89,32,-1
          if(board(where) .eq. 1) then
              pend(1)=pend(1)+1
              plist(pend(1))=where
              vlist(pend(1))=board(where)
              pboard(where)=pend(1)
          endif
1800  continue
      if(pend(1) .ne. 16) then
          do 1900 i=pend(1)+1,16
              plist(i)=0
              vlist(i)=0
1900      continue
      endif
      pstart(2)=17
      pend(2)=16
      do 2200 kp=7,1,-1
          do 2100 where=22,99
              if(board(where).lt.-1 .and. xpmovgen(where).eq.kp) then
                  pend(2)=pend(2)+1
                  plist(pend(2))=where
                  vlist(pend(2))=board(where)
                  pboard(where)=pend(2)
              endif
2100      continue
2200  continue
      pcend(2)=pend(2)
      pwnstrt(2)=pend(2)+1
      do 2300 where=32,89
          if(board(where) .eq. -1) then
              pend(2)=pend(2)+1
              plist(pend(2))=where
              vlist(pend(2))=board(where)
              pboard(where)=pend(2)
          endif
2300  continue
      if(pend(2).eq.32) go to 2500
          do 2400 i=pend(2)+1,32
              plist(i)=0
              vlist(i)=0
2400      continue
2500  continue
c
c------------------------------< now set up the hashed board value
c------------------------------< so that everyone will know the
c------------------------------< correct encoded board value.
c
      hash=0
      phash=0
      do 2600 where=1,pend(2)
          square=plist(where)
          if(square .ne. 0) then
              hash=xor(hash,random(square,board(square)+7))
              if(iabs(board(plist(where))) .eq. 1)
     *            phash=xor(phash,random(square,board(square)+7))
          endif
2600  continue
c
c------------------------------< set mate board for special case of
c------------------------------< bishop & knight vs king if needed.
c
      if(npawns .ne. 0) go to 3400
      if(nppcs.ne.0 .and. nopcs.ne.0) go to 3400
      if(nppcs .eq. 5) go to 2800
      if(nopcs .ne. 5) go to 3400
c
c------------------------------< find bishop to determine which
c------------------------------< color it is.
c
      do 2700 i=pstart(2),pcend(2)
          if(vlist(i) .eq. -3) go to 3000
2700  continue
      go to 3400
2800  continue
      do 2900 i=pstart(1),pcend(1)
          if(vlist(i) .eq. 3) go to 3000
2900  continue
      go to 3400
c
c------------------------------< now determine the color of the
c------------------------------< bishop and set the matebd to
c------------------------------< drive the king into the right
c------------------------------< corner (color of bishop)
c
3000  continue
      bc=and(cranks(plist(i))+cfiles(plist(i)),1)
      if(bc .ne. 1) then
          do 3100 i=1,100
              matebd(i)=evbnbd(i)
3100      continue
      else
          do 3300 i=1,100
              matebd(i)=odbnbd(i)
3300      continue
      endif
3400  continue
c
c------------------------------< build bit board
c
      bitbd=zero
      do 3500 i=22,29
          if(board(i).ne.0) bitbd=or(bitbd,ishft(one,85-i))
3500  continue
      do 3600 i=32,39
          if(board(i).ne.0) bitbd=or(bitbd,ishft(one,87-i))
3600  continue
      do 3700 i=42,49
          if(board(i).ne.0) bitbd=or(bitbd,ishft(one,89-i))
3700  continue
      do 3800 i=52,59
          if(board(i).ne.0) bitbd=or(bitbd,ishft(one,91-i))
3800  continue
      do 3900 i=62,69
          if(board(i).ne.0) bitbd=or(bitbd,ishft(one,93-i))
3900  continue
      do 4000 i=72,79
          if(board(i).ne.0) bitbd=or(bitbd,ishft(one,95-i))
4000  continue
      do 4100 i=82,89
          if(board(i).ne.0) bitbd=or(bitbd,ishft(one,97-i))
4100  continue
      do 4200 i=92,99
          if(board(i).ne.0) bitbd=or(bitbd,ishft(one,99-i))
4200  continue
c
c------------------------------< set the flags d4 and e4 to indicate
c------------------------------< the opening type.  these will be used
c------------------------------< to slightly alter development.  an 
c------------------------------< example is the need to play c4/c5 in
c------------------------------< a d4 opening.  blocking the c-pawn 
c------------------------------< makes this more difficult.
c
      if(color .eq. 1) then
          kp=55
          qp=56
      else
          kp=56
          qp=55
      endif
      if(board(qp).eq.1 .and. e4.eq.0) then
          d4=d4+1
          if(d4 .eq. 1) then
              if(autos .gt. 3) print 4400
              write(3,4300)
4300          format(1x,'d4 opening recognized.')
          endif
      endif
      if(board(kp).eq.1 .and. d4.le.3) then
          e4=e4+1
          d4=0
          if(e4 .eq. 1) then
              if(autos .gt. 3) print 4300
              write(3,4400)
4400          format(1x,'e4 opening recognized.')
          endif
      endif
c
c------------------------------< now call scoredv to see if it finds
c------------------------------< anything wrong with development.  if
c------------------------------< not, set the "devdone" flag so it won't
c------------------------------< be called any more.
c
      pscore=0
      tply=ply
      ply=0
      call scoredv
      ply=tply
      if(tpscor .eq. 0) devdone=devdone+1
      if(devdone .eq. 1) then
          write(3,4500)
4500      format(/1x,'development completed.'/)
          if(autos .gt. 3) print 4500
      endif
c
c------------------------------< add up the pieces to compute the
c------------------------------< current material score.
c
      mscore=0
      do 4600 sq=1,pend(2)
          temp=vlist(sq)
          if(temp.ne.0 .and. temp.le.6) 
     *        mscore=mscore+isign(1,temp)*pieces(iabs(temp))
4600  continue
      return
      end
