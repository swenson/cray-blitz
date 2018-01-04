      subroutine prescore
crev  last revision 03/09/91
c
c     ******************************************************************
c     *                                                                *
c     *       prescore is called to set up the "static scoring"        *
c     *  tables.  these tables are used to produce scoring results     *
c     *  that are considered too time-consuming for the dynamic        *
c     *  scoring routines.  this routine therefore looks at the        *
c     *  board position at the root of the tree and sets the various   *
c     *  "piece/square" tables so that simple array loads can be used  *
c     *  to handle a piece on a particular square.                     *
c     *                                                                *
c     *       all is not free here, of course.  these "piece/square"   *
c     *  are set from the position at the root of the tree.  if some   *
c     *  of the board characteristics change (files open, etc.) these  *
c     *  tables won't change also, until the next search starts.  as   *
c     *  a result, these tables should contain primarily "static"      *
c     *  data to control this problem.                                 *
c     *                                                                *
c     ******************************************************************
c
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
c------------------------------< if little material remains, make it
c------------------------------< easier to advance pawns.  while lots of
c------------------------------< pieces are on the board, moving pawns
c------------------------------< often hurts more than it helps.
c
      if(nppcs.le.16 .or. nopcs.le.16) then
          oranks(3)=10
          oranks(4)=8
          oranks(5)=6
          oranks(6)=4
          oranks(7)=2
          pranks(4)=2
          pranks(5)=4
          pranks(6)=6
          pranks(7)=8
          pranks(8)=10
      else
          oranks(3)=-5
          oranks(4)=-4
          oranks(5)=-3
          oranks(6)=-2
          oranks(7)=-1
          pranks(4)=-1
          pranks(5)=-2
          pranks(6)=-3
          pranks(7)=-4
          pranks(8)=-5
      endif
c
c------------------------------< set the pawn scoring board so that
c------------------------------< pawns are advanced where appropriate
c
      do 1000 sq=22,99
          file=cfiles(sq)
          rank=cranks(sq)
          ppawnb(sq)=pfiles(file)*pranks(rank)
          opawnb(sq)=pfiles(file)*oranks(rank)
1000  continue
      if(devdone .eq. 0) then
          ppawnb(55)=50
          ppawnb(56)=50
          opawnb(65)=25
          opawnb(66)=25
      endif
c
c------------------------------< now set up to score knight/king
c------------------------------< tropism.
c
      do 2000 sq=22,99
          pknightb(sq)=ncenter*centern(sq)+nkingtrp*xomovgen(sq)
          oknightb(sq)=ncenter*centern(sq)+nkingtrp*xpmovgen(sq)
2000  continue
c
c------------------------------< now set up to score bishop/king
c------------------------------< tropism.
c
      do 3000 sq=22,99
          pbishopb(sq)=bcenter*center(sq)+bkingtrp*xomovgen(sq)
          obishopb(sq)=bcenter*center(sq)+bkingtrp*xpmovgen(sq)
3000  continue
c
c------------------------------< this block of code sets the piece/
c------------------------------< square values for rooks.  it looks
c------------------------------< at the pawn structure and tries to
c------------------------------< embody the following rules:
c------------------------------< 1.  don't advance rooks on files so 
c------------------------------<  that they become "hemmed" in behind
c------------------------------<  friendly pawns and get cut off from
c------------------------------<  open and 1/2 open files.
c------------------------------< 2.  don't do the above penalties for
c------------------------------<  files that are open.
c------------------------------< 3.  don't be concerned if the rook
c------------------------------<  moves in front of friendly pawns,
c------------------------------<  just behind them.
c
      do 4000 sq=22,99
          orookb(sq)=0
          prookb(sq)=0
4000  continue
      do 4160 rank=3,8
          do 4150 file=2,9
              if(pcount(file) .eq. 0) go to 4150
              sq=rank*10+file
              do 4110 i=file-1,2,-1
                  if(pcount(i) .eq. 0) go to 4150
                  if(board(rank*10+i) .eq. 1) go to 4120
4110          continue
4120          continue
              do 4130 i=file+1,9
                  if(pcount(i) .eq. 0) go to 4150
                  if(board(rank*10+i) .eq. 1) go to 4140
4130          continue
4140          continue
                  prookb(sq)=prookb(sq)-rbad
4150      continue
4160   continue
      do 4260 rank=3,8
          do 4250 file=2,9
              if(ocount(file) .eq. 0) go to 4250
              sq=rank*10+file
              do 4210 i=file-1,2,-1
                  if(ocount(i) .eq. 0) go to 4250
                  if(board(rank*10+i) .eq. -1) go to 4220
4210          continue
4220          continue
              do 4230 i=file+1,9
                  if(ocount(i) .eq. 0) go to 4250
                  if(board(rank*10+i) .eq. -1) go to 4240
4230          continue
4240          continue
                  orookb(sq)=orookb(sq)-rbad
4250      continue
4260  continue
c
c------------------------------< now set up to score rooks on 
c------------------------------< the 7th rank.
c
      pc=0
      do 4300 sq=82,89
          if(board(sq) .eq. -1) pc=pc+1
4300  continue
      if(pc.ge.2 .or. krank(2).eq.9) then 
          do 4310 sq=82,89
              prookb(sq)=prookb(sq)+ron7th
4310      continue
      endif
      pc=0
      do 4320 sq=32,39
          if(board(sq) .eq. 1) pc=pc+1
4320  continue
      if(pc.ge.2 .or. krank(1).eq.9) then 
          do 4330 sq=32,39
              orookb(sq)=orookb(sq)+ron7th
4330      continue
      endif
c
c------------------------------< now set up to score rook/king
c------------------------------< tropism.
c
      do 4400 sq=22,99
          prookb(sq)=prookb(sq)+rkingtrp*xomovgen(sq)
          orookb(sq)=orookb(sq)+rkingtrp*xpmovgen(sq)
4400  continue
c
c------------------------------< now set up to score queen/king
c------------------------------< tropism.
c
      do 5000 sq=22,99
          pqueenb(sq)=qcenter*center(sq)+qkingtrp*xomovgen(sq)
          oqueenb(sq)=qcenter*center(sq)+qkingtrp*xpmovgen(sq)
5000  continue
      return
      end
