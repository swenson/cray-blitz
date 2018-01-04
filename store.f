      subroutine store1
crev last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      store1&2 are used to enter positions into the hash table  *
c     *  for later use by 'lookup'. if a collision occurs, the current *
c     *  position will overlay the table  position if the table        *
c     *  position is from an earlier iteration or if the current       *
c     *  position is at a lower (numeric) depth than the table         *
c     *  position. store2 is used when the position value is unknown.  *
c     *      store uses the double hash algorihm to resolve all        *
c     *  collisions.  it tries to resolve collisions in two passes;    *
c     *  pass one attempts to find a slot that is empty or that was    *
c     *  stored from an earlier move.  pass two tries to find a        *
c     *  position stored during the current move that was stored       *
c     *  from a greater depth than the current search depth.  the      *
c     *  entire table  is not searched, just n random entries (n=7).   *
c     *                                                                *
c     *      table format for cray-1 is as follows (64 bit word):      *
c     *                                                                *
c     *         word 1   1 bit  : position age (0=current, 1=old)      *
c     *                  1 bit  : onerep(ply)                          *
c     *                 22 bits : position's value + 2000000           *
c     *                 40 bits : hashed board position                *
c     *                                                                *
c     *         word 2   7 bits : level of search from this position   *
c     *                           plus a bias of 64                    *
c     *                  2 bits : table entry type (good, bound, etc.) *
c     *                 23 bits : suggested move for this position     *
c     *                                                                *
c     *    96-bits are used.  two word 2's are packed together.        *
c     *    storage is laid out as follows:                             *
c     *      hsize+7168 words  of  player1's word 1's                  *
c     *      hsize+7168 words with player1's word 2's in right half    *
c     *                        and player2's word 2's in left half     *
c     *      hsize+7168 words  of  player2's word 1's                  *
c     *                                                                *
c     *     the 7168 comes from rindex size, currently 1024,           *
c     *     times the number of colsns, currently 6.                   *
c     *     in addition a pawn hash table is tacked on at the end      *
c     *     of the regular hash table. its size is hsize/4.            *
c     *                                                                *
c     *    total hash space=3*hsize+3*7168+hsize/8+hsize/2+hsize/32    *
c     *    the current hsize default is 32768, htable has 138240 words.*
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
      data mmask / 3 /
      data hbsiz / 40 /
c
c
c------------------------------< hash the current position to get the
c------------------------------< pointer to the hash table.
c
      if(and(debug,4) .ne. 0) return
      hashbd=xor(hash,random(or(moveds(ply-2),
     *                            ishft(moveds(ply-1),2))+1,2))
      if((from(ply-1)-to(ply-1)) .ne. 20*side) go to 100
      if(board(to(ply-1)) .ne. -side) go to 100
      if(board(to(ply-1)-1) .ne. side .and.
     *   board(to(ply-1)+1) .ne. side)       go to 100
          hashbd=xor(hashbd,random(cfiles(to(ply-1)),1))
100   continue
c
c------------------------------< now determine if this position is
c------------------------------< in the table.  for collisions, use
c------------------------------< the random probe technique to search
c------------------------------< 7 additional entries in an attempt
c------------------------------< to locate the correct one.
c
      rindex=or(and(ishft(hashbd,-24),1023),1)
      hkey=and(hashbd,hsizm1)
      rkey1=hkey+1
      rkey2=hkey+1+htbl1
      hshft=ishft(player-1,5)
      hmask=ishft(o'37777777777',32-hshft)
      if(player.eq.2) rkey1=rkey1+htbl2
c
      if(ishft(hashbd,-24) .eq. and(htable(rkey1),bmask)) go to 900
      age=ishft(htable(rkey1),-63)
      if(age .ne. 0) go to 1000
c
c------------------------------< a collision has occurred. determine
c------------------------------< if another probe should be made. if
c------------------------------< so, compute the next hash key and try
c------------------------------< again for a match. (3 times)
c
          best=and(ishft(htable(rkey2),-(25+hshft)),127)-64
          bestp=rkey1
          rkey1=rkey1+rindex
          rkey2=rkey2+rindex
          if(ishft(hashbd,-24) .eq. and(htable(rkey1),bmask)) go to 900
          age=ishft(htable(rkey1),-63)
          if(age .ne. 0) go to 1000
              mlevel=and(ishft(htable(rkey2),-(25+hshft)),127)-64
              if(best .lt. mlevel) go to 200
                  best=mlevel
                  bestp=rkey1
200           continue
          rkey1=rkey1+rindex
          rkey2=rkey2+rindex
          if(ishft(hashbd,-24) .eq. and(htable(rkey1),bmask)) go to 900
          age=ishft(htable(rkey1),-63)
          if(age .ne. 0) go to 1000
              mlevel=and(ishft(htable(rkey2),-(25+hshft)),127)-64
              if(best .lt. mlevel) go to 300
                  best=mlevel
                  bestp=rkey1
300           continue
          rkey1=rkey1+rindex
          rkey2=rkey2+rindex
          if(ishft(hashbd,-24) .eq. and(htable(rkey1),bmask)) go to 900
          age=ishft(htable(rkey1),-63)
          if(age .ne. 0) go to 1000
              mlevel=and(ishft(htable(rkey2),-(25+hshft)),127)-64
              if(best .lt. mlevel) go to 400
                  best=mlevel
                  bestp=rkey1
400           continue
          rkey1=rkey1+rindex
          rkey2=rkey2+rindex
          if(ishft(hashbd,-24) .eq. and(htable(rkey1),bmask)) go to 900
          age=ishft(htable(rkey1),-63)
          if(age .ne. 0) go to 1000
              mlevel=and(ishft(htable(rkey2),-(25+hshft)),127)-64
              if(best .lt. mlevel) go to 500
                  best=mlevel
                  bestp=rkey1
500           continue
          rkey1=rkey1+rindex
          rkey2=rkey2+rindex
          if(ishft(hashbd,-24) .eq. and(htable(rkey1),bmask)) go to 900
          age=ishft(htable(rkey1),-63)
          if(age .ne. 0) go to 1000
              mlevel=and(ishft(htable(rkey2),-(25+hshft)),127)-64
              if(best .lt. mlevel) go to 600
                  best=mlevel
                  bestp=rkey1
600           continue
          rkey1=rkey1+rindex
          rkey2=rkey2+rindex
          if(ishft(hashbd,-24) .eq. and(htable(rkey1),bmask)) go to 900
          age=ishft(htable(rkey1),-63)
          if(age .ne. 0) go to 1000
              mlevel=and(ishft(htable(rkey2),-(25+hshft)),127)-64
              if(best .lt. mlevel) go to 700
                  best=mlevel
                  bestp=rkey1
700           continue
          rkey1=rkey1+rindex
          rkey2=rkey2+rindex
          if(ishft(hashbd,-24) .eq. and(htable(rkey1),bmask)) go to 900
          age=ishft(htable(rkey1),-63)
          if(age .ne. 0) go to 1000
              mlevel=and(ishft(htable(rkey2),-(25+hshft)),127)-64
              if(best .lt. mlevel) go to 800
                  best=mlevel
                  bestp=rkey1
800           continue
c
c------------------------------< there was no position that was stored
c------------------------------< during the last search. try the most
c------------------------------< useless one we found and determine if
c------------------------------< it should be replaced.
c
      if(best .gt. rdepth-ply) return
      rkey1=bestp
      rkey2=bestp+htbl1
      if(player.eq.2) rkey2=bestp-htbl1
c
c------------------------------< this position is already in the table,
c------------------------------< only store the one from the lowest
c------------------------------< ply as it is the most useful.
c
900   continue
      mlevel=and(ishft(htable(rkey2),-(25+hshft)),127)-64
      if(mlevel .le. rdepth-ply) go to 1000
          if(and(ishft(htable(rkey2),-(23+hshft)),3) .eq. 1) return
1000  continue
c
c------------------------------< this position should be replaced by
c------------------------------< the current board position.
c
c     call packer(rkey1)
      w1=value(ply-3+player)
      w2=value(ply-player)
      if(value(ply).le.w1 .or. value(ply).ge.w2) go to 1100
c         call lock20
          call store5
c         call unlock20
          return
c
c------------------------------< this level has been completely searched
c------------------------------< without finding a move.  remember that
c------------------------------< this level is too bad to accept,
c------------------------------< so that all moves were cutoff.
c
1100  continue
c         call lock20
          htable(rkey1)=ishft(hashbd,-24)
     *                  +ishft(value(ply)+2000000,hbsiz)
     *                  +ishft(onerep(ply),62)
          htable(rkey2)=ishft(2,23+hshft)
     *                  +ishft(rdepth-ply+64,25+hshft)
     *                  +and(htable(rkey2),hmask)
c         call unlock20
          return
      end
      subroutine store2
crev last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      store1&2 are used to enter positions into the hash table  *
c     *  for later use by 'lookup'. if a collision occurs, the current *
c     *  position will overlay the table  position if the table        *
c     *  position is from an earlier iteration or if the current       *
c     *  position is at a lower (numeric) depth than the table         *
c     *  position. store2 is used when the position value is unknown.  *
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
      data mmask / 3 /
      data hbsiz / 40 /
c
c
c------------------------------< hash the current position to get the
c------------------------------< pointer to the hash table.
c
      if(and(debug,4) .ne. 0) return
      hashbd=xor(hash,random(or(moveds(ply-2),
     *                            ishft(moveds(ply-1),2))+1,2))
      if((from(ply-1)-to(ply-1)) .ne. 20*side) go to 100
      if(board(to(ply-1)) .ne. -side) go to 100
      if(board(to(ply-1)-1) .ne. side .and.
     *   board(to(ply-1)+1) .ne. side)       go to 100
          hashbd=xor(hashbd,random(cfiles(to(ply-1)),1))
100   continue
c
c------------------------------< now determine if this position is
c------------------------------< in the table.  for collisions, use
c------------------------------< the random probe technique to search
c------------------------------< 7 additional entries in an attempt
c------------------------------< to locate the correct one.
c
      rindex=or(and(ishft(hashbd,-24),1023),1)
      hkey=and(hashbd,hsizm1)
      rkey1=hkey+1
      rkey2=hkey+1+htbl1
      hshft=ishft(player-1,5)
      hmask=ishft(o'37777777777',32-hshft)
      if(player.eq.2) rkey1=rkey1+htbl2
c
      if(ishft(hashbd,-24) .eq. and(htable(rkey1),bmask)) go to 900
      age=ishft(htable(rkey1),-63)
      if(age .ne. 0) go to 1000
c
c------------------------------< a collision has occurred. determine
c------------------------------< if another probe should be made. if
c------------------------------< so, compute the next hash key and try
c------------------------------< again for a match. (3 times)
c
          best=and(ishft(htable(rkey2),-(25+hshft)),127)-64
          bestp=rkey1
          rkey1=rkey1+rindex
          rkey2=rkey2+rindex
          if(ishft(hashbd,-24) .eq. and(htable(rkey1),bmask)) go to 900
          age=ishft(htable(rkey1),-63)
          if(age .ne. 0) go to 1000
              mlevel=and(ishft(htable(rkey2),-(25+hshft)),127)-64
              if(best .lt. mlevel) go to 200
                  best=mlevel
                  bestp=rkey1
200           continue
          rkey1=rkey1+rindex
          rkey2=rkey2+rindex
          if(ishft(hashbd,-24) .eq. and(htable(rkey1),bmask)) go to 900
          age=ishft(htable(rkey1),-63)
          if(age .ne. 0) go to 1000
              mlevel=and(ishft(htable(rkey2),-(25+hshft)),127)-64
              if(best .lt. mlevel) go to 300
                  best=mlevel
                  bestp=rkey1
300           continue
          rkey1=rkey1+rindex
          rkey2=rkey2+rindex
          if(ishft(hashbd,-24) .eq. and(htable(rkey1),bmask)) go to 900
          age=ishft(htable(rkey1),-63)
          if(age .ne. 0) go to 1000
              mlevel=and(ishft(htable(rkey2),-(25+hshft)),127)-64
              if(best .lt. mlevel) go to 400
                  best=mlevel
                  bestp=rkey1
400           continue
          rkey1=rkey1+rindex
          rkey2=rkey2+rindex
          if(ishft(hashbd,-24) .eq. and(htable(rkey1),bmask)) go to 900
          age=ishft(htable(rkey1),-63)
          if(age .ne. 0) go to 1000
              mlevel=and(ishft(htable(rkey2),-(25+hshft)),127)-64
              if(best .lt. mlevel) go to 500
                  best=mlevel
                  bestp=rkey1
500           continue
          rkey1=rkey1+rindex
          rkey2=rkey2+rindex
          if(ishft(hashbd,-24) .eq. and(htable(rkey1),bmask)) go to 900
          age=ishft(htable(rkey1),-63)
          if(age .ne. 0) go to 1000
              mlevel=and(ishft(htable(rkey2),-(25+hshft)),127)-64
              if(best .lt. mlevel) go to 600
                  best=mlevel
                  bestp=rkey1
600           continue
          rkey1=rkey1+rindex
          rkey2=rkey2+rindex
          if(ishft(hashbd,-24) .eq. and(htable(rkey1),bmask)) go to 900
          age=ishft(htable(rkey1),-63)
          if(age .ne. 0) go to 1000
              mlevel=and(ishft(htable(rkey2),-(25+hshft)),127)-64
              if(best .lt. mlevel) go to 700
                  best=mlevel
                  bestp=rkey1
700           continue
          rkey1=rkey1+rindex
          rkey2=rkey2+rindex
          if(ishft(hashbd,-24) .eq. and(htable(rkey1),bmask)) go to 900
          age=ishft(htable(rkey1),-63)
          if(age .ne. 0) go to 1000
              mlevel=and(ishft(htable(rkey2),-(25+hshft)),127)-64
              if(best .lt. mlevel) go to 800
                  best=mlevel
                  bestp=rkey1
800           continue
c
c------------------------------< there was no position that was stored
c------------------------------< during the last search. try the most
c------------------------------< useless one we found and determine if
c------------------------------< it should be replaced.
c
      if(best .gt. rdepth-ply) return
      rkey1=bestp
      rkey2=bestp+htbl1
      if(player.eq.2) rkey2=bestp-htbl1
c
c------------------------------< this position is already in the table,
c------------------------------< only store the one from the lowest
c------------------------------< ply as it is the most useful.
c
900   continue
      mlevel=and(ishft(htable(rkey2),-(25+hshft)),127)-64
      if(mlevel .le. rdepth-ply) go to 1000
          if(and(ishft(htable(rkey2),-(23+hshft)),3) .eq. 1) return
1000  continue
c
c------------------------------< this position should be replaced by
c------------------------------< the current board position.
c------------------------------< the value of this position in unknown
c------------------------------< since it is causing an alpha/beta
c------------------------------< cutoff.  remember the correct search
c------------------------------< bound to determine if it might cause
c------------------------------< a cutoff again.
c
c     call packer(rkey1)
c         call lock20
          htable(rkey1)=ishft(hashbd,-24)
     *                  +ishft(value(ply-1)+2000000,hbsiz)
     *                  +ishft(onerep(ply),62)
          htable(rkey2)=ishft(3,23+hshft)
     *                  +ishft(rdepth-ply+64,25+hshft)
     *                  +and(htable(rkey2),hmask)
     *                  +ishft(curmvs(ply,taskid),hshft)
c         call unlock20
          return
      end
      subroutine store5
crev last revision 04/26/89
c
c     *****************************************************************
c     *                                                               *
c     *    store5 completes the work of store1 and is rarely called.  *
c     *                                                               *
c     *****************************************************************
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
      data hvmask / o'17777777' /
      data hbsiz / 40 /
c
c
c------------------------------< the backed up score is usable as is,
c------------------------------< enter it in the hash table along with
c------------------------------< a 'completely useful' flag.
c
      actval=value(ply)
      if(actval.ge.drawsc .and. actval.le.drawsc+100)
     *                                        actval=actval-ply
      htable(rkey1)=ishft(hashbd,-24)
     *              +ishft(actval+2000000,hbsiz)
     *              +ishft(onerep(ply),62)
      htable(rkey2)=ishft(1,23+hshft)
     *              +ishft(rdepth-ply+64,25+hshft)
     *              +and(htable(rkey2),hmask)
      if(and(trace(51,ply),65535) .ge. ply)
     *   htable(rkey2)=htable(rkey2)+ishft(trace(ply,ply),hshft)
      if(.not. (iabs(value(ply)).gt.900000 .and.
     *          iabs(value(ply)).lt.1500000)) go to 100
c
c------------------------------< set level to maximum depth
c
          htable(rkey2)=or(htable(rkey2),ishft(127,25+hshft))
c
c------------------------------< set value to mate length
c
          mply=1000000-iabs(value(ply))
          mvalue=isign(1,value(ply))*(1000000-mply+ply)
          htable(rkey1)=ishft(mvalue+2000000,hbsiz)
     *                  +and(htable(rkey1),xor(ishft(hvmask,hbsiz),-1))
     *                  +ishft(onerep(ply),62)
100   continue
      return
      end
