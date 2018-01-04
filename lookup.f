      subroutine lookup
crev last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      lookup examines the hash table to determine if further    *
c     *  analysis can be avoided for the current position.  as each    *
c     *  position is evaluated, it is entered into the hash table      *
c     *  along with the best/worst/current scores for this position.   *
c     *  whenever a new position is reached for analysis, first this   *
c     *  table is examined to determine if this position has been      *
c     *  reached before.  if so, one of three possible cases arises:   *
c     *                                                                *
c     *      1)  the score saved for this position is perfectly        *
c     *          valid.  in this case, return this score and consider  *
c     *          this node complete without doing a search.            *
c     *                                                                *
c     *      2)  the score saved for this position is only known to    *
c     *          be better than some upper bound or worse than some    *
c     *          lower bound.  the score saved is actually the bound   *
c     *          in question.  if it is an upper bound, and it is      *
c     *          less than the current lower bound, or it is a lower   *
c     *          bound that is greater than the current upper bound,   *
c     *          then an immediate alpha/beta cutoff will occur with   *
c     *          no additional searching from this node.               *
c     *                                                                *
c     *      3)  the score saved for this position is useless.  in     *
c     *          this case, return and continue the search normally.   *
c     *                                                                *
c     *      see store1 for form of htable storage                     *
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
      data hvmask / o'17777777' /
      data hmmask / o'37777777' /
      data hbsiz / 40 /
c
c
c------------------------------< hash the current position to get the
c------------------------------< pointer to the hash table.  fold in
c------------------------------< castling and en passant status also.
c
      hmove(ply)=0
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
c------------------------------< in the table. for collisions, use
c------------------------------< the double hash technique to search
c------------------------------< seven additional entries in an attempt
c------------------------------< to locate the correct one.
c
      rindex=or(and(ishft(hashbd,-24),1023),1)
      hkey=and(hashbd,hsizm1)
      rkey1=hkey+1
      if(player.eq.2) rkey1=rkey1+htbl2
c         call lock20
          if(ishft(hashbd,-24) .eq. and(htable(rkey1),bmask)) go to 200
c
c------------------------------< a collision has occurred. determine
c------------------------------< if another probe should be made. if
c------------------------------< so, compute the next hash key and try
c------------------------------< again for a match.
c
          rkey1=rkey1+rindex
          if(ishft(hashbd,-24) .eq. and(htable(rkey1),bmask)) go to 200
          rkey1=rkey1+rindex
          if(ishft(hashbd,-24) .eq. and(htable(rkey1),bmask)) go to 200
          rkey1=rkey1+rindex
          if(ishft(hashbd,-24) .eq. and(htable(rkey1),bmask)) go to 200
          rkey1=rkey1+rindex
          if(ishft(hashbd,-24) .eq. and(htable(rkey1),bmask)) go to 200
          rkey1=rkey1+rindex
          if(ishft(hashbd,-24) .eq. and(htable(rkey1),bmask)) go to 200
          rkey1=rkey1+rindex
          if(ishft(hashbd,-24) .eq. and(htable(rkey1),bmask)) go to 200
          rkey1=rkey1+rindex
          if(ishft(hashbd,-24) .eq. and(htable(rkey1),bmask)) go to 200
c         call unlock20
          go to 9999
c
c------------------------------< the current position is in the table.
c------------------------------< determine if the depth associated with
c------------------------------< the score is sufficient to avoid
c------------------------------< searching any deeper.
c
200   continue
          if(player .eq. 1) then
              rkey2=rkey1+htbl1
          else
              rkey2=rkey1-htbl1
          endif
c         call packlook(rkey1)
          htab1=htable(rkey1)
          htab2=htable(rkey2)
c         call unlock20
          hshft=ishft(player-1,5)
          tmove=and(ishft(htab2,-hshft),hmmask)
          if(tmove .ne. 16383) then
              hmove(ply)=tmove
              if(tmove .ne. 0) onerep(ply)=ishft(ishft(htab1,1),-63) 
          else
              hmove(ply)=0
          endif
          if(and(ishft(htab2,-(25+hshft)),127)-64.lt.rdepth-ply)
     *                                                        go to 9999
          hashes=hashes+1
          tbound=and(ishft(htab1,-hbsiz),hvmask)-2000000
          if(and(ishft(htab2,-(23+hshft)),3).eq.3) go to 900
          if(and(ishft(htab2,-(23+hshft)),3).eq.2) go to 600
c
c------------------------------< the current position has been found.
c------------------------------< the stored score is valid, return it
c------------------------------< to search and consider this node as
c------------------------------< completed with no further searching
c------------------------------< required. (hooray)
c
              mside=-side
              bound=mside*value(ply-1)
              atbnd=iabs(tbound)
              if(atbnd.gt.900000 .and. atbnd.lt.1500000)
     *                        tbound=tbound-isign(1,tbound)*ply
              if(tbound.ge.drawsc .and. tbound.le.drawsc+100)
     *                    tbound=tbound+ply
              if (mside*tbound .le. bound) go to 9997
              limit=ply-1
              do 500 level=1,limit
                  trace(level,ply)=curmvs(level,taskid)
500           continue
              trace(51,ply)=65536+ply-1
              trace(52,ply)=depth
              value(ply)=tbound
              go to 9998
c
c------------------------------< the current position has been found.
c------------------------------< however, in this position, all moves
c------------------------------< for the side on move were pruned
c------------------------------< causing the applicable search bound
c------------------------------< to be backed up. if the bound has
c------------------------------< not changed, simply back it up again.
c
600   continue
          bound=side*value(ply-2)
          if(side*tbound .ge. bound) go to 9999
          go to 9998
c
c------------------------------< the current position has been found.
c------------------------------< however; the score is only known to
c------------------------------< be outside some alpha/beta bound. if
c------------------------------< the stored bound is outside the current
c------------------------------< bound, a cutoff will occur.
c
c
900   continue
          mside=-side
          bound=mside*value(ply-1)
          if(mside*tbound .gt. bound) go to 9999
c
c------------------------------< return
c
9997  continue
          mated(ply-1)=0
          return=2
          return
9998  continue
          mated(ply-1)=0
          return=1
          return
9999  continue
          return=0
          return
      end
