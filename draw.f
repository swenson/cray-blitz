      subroutine draw
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      draw is used to detect those situations that are a draw   *
c     *  due to the lack of sufficient material by either side to be   *
c     *  able to force a mate.  generally, if either side has a queen, *
c     *  rook, or pawn then the position is assumed winnable.  also,   *
c     *  if either side has 2 or more bishops, 3 or more knights, or   *
c     *  a knight and bishop, then the position is assumed winnable.   *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
c
c
      include 'common.f'
c
c
c------------------------------< determine if either side has enough
c------------------------------< material to force checkmate. if not,
c------------------------------< consider the position a draw and
c------------------------------< return.
c
      return=0
      pbish=0
      obish=0
      pnight=0
      onight=0
      do 500  sq=1,pend(2)
          temp=vlist(sq)+7
          go to (500,9999,9999,100,200,9999,500,
     *                      9999,300,400,9999,9999,500),temp
100       continue
              if(onight .gt. 0) return
              obish=obish+1
              go to 500
200       continue
              if(obish .gt. 0) return
              onight=onight+1
              go to 500
300       continue
              if(pbish .gt. 0) return
              pnight=pnight+1
              go to 500
400       continue
              if(pnight .gt. 0) return
              pbish=pbish+1
500   continue
      if(pbish.ge.2 .or. obish.ge.2) return
      if(pnight.ge.3 .or. onight.ge.3) return
c
c------------------------------< the game is drawn due to lack of
c------------------------------< sufficient mating material
c
          return=1
          return
c
c------------------------------< not a draw, return
c
9999  continue
          return
      end
