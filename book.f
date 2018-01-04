      subroutine book
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      book is the routine responsible for searching the book    *
c     *  data base.  book uses the move list prepared by base and      *
c     *  sequentially makes each move.  after each move is made, the   *
c     *  book data base is examined to determine if this move results  *
c     *  in a known book position.  if so, the evaluation is saved     *
c     *  if it is the best book position found.                        *
c     *      if none of the moves leads to a book position, a failure  *
c     *  return is taken.  however, it takes 5 failures to avoid any   *
c     *  further book searches to catch transpositions.                *
c     *      to provide additional controls on which openings the      *
c     *  program will play, the book is divided into two separate      *
c     *  data files.  unit 8 is used when the program is playing       *
c     *  white, and unit 9 is used when the program is playing black.  *
c     *  these units are used to store moves that the program should   *
c     *  play if possible.  if unit 8 has only 'e4' as a move for      *
c     *  white, then the program will only play that move.             *
c     *  when no move can be found on unit 8 or 9, then the program    *
c     *  switches to unit 10 (which usually contains the BIG book file *
c     *  for the remainder of the book opening.                        *
c     *      another feature of book is that is also examines the      *
c     *  book database to determine what the opponent's response       *
c     *  should be.  this can be used for pondering while waiting      *
c     *  for his move.  the response might come from the book or it    *
c     *  might come from a search if the book line terminates here.    *
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
      dimension record(100)
c     data bmask / 17777777777777b /
      data bmask / x'ffffffffff' /
c
c------------------------------< initialize.
c
      bookdev=color+7
      if(margin .gt. 500) bookdev=10
      lim=last(1)
      number=0
      inbook=0
      if(margin .ge. 0) inbook=1
      if(inbook .eq. 0) return
      moveds(1)=movedr(1)
      moveds(2)=movedr(2)
      inchk(1)=checki(1)
c
c------------------------------< if there are no legal moves, it is
c------------------------------< best to avoid searching the data base.
c
      if(last(1) .lt. first(1)) return
c
c------------------------------< determine if the base position is in
c------------------------------< the book database.  if not, then we
c------------------------------< are out of book and can save time by
c------------------------------< not searching the database further.
c
100   continue
          read(unit=bookdev,rec=1) sizebook
          if(avgtim.le.1000 .and. numout.ne.npmovs) then
              atemp=and(and(hash,bmask),65535)
              key=mod(atemp,sizebook/2)+(2-color)*sizebook/2+1
              read(unit=bookdev,rec=key) record
              do 200 i=1,100,2
                  if(and(hash,bmask) .eq. record(i)) go to 300
200           continue
              go to 800
300           continue
          endif
c
c------------------------------< now loop through the move list and make
c------------------------------< each move to determine if the resulting
c------------------------------< position is in the book data base.
c
          do 700 where=1,lim
              mfrom=extrct(moves(where))
              call make
              hkey=and(hash,bmask)
              call unmake
              atemp=and(hkey,65535)
              key=mod(atemp,sizebook/2)+(color-1)*sizebook/2+1
              read(unit=bookdev,rec=key) record
c
c------------------------------< determine if this position is a known
c------------------------------< book position. if so, enter it and the
c------------------------------< evaluation in the move list for later
c------------------------------< analysis.
c
              do 400 i=1,100,2
                  if(hkey .eq. record(i)) go to 500
400           continue
              go to 700
c
c------------------------------< this position is the best one so far,
c------------------------------< remember it as best.
c
500           continue
                  number=number+1
                  trace(1,number)=moves(where)
                  trace(2,number)=record(i+1)
                  if(tflag .gt. 0) then
                      call output(0,0)
                      print 600, record(i+1), (text(ll),ll=1,30)
600                   format(1x,i8,2x,30a1)
                  endif
700       continue
c
c------------------------------< if the number of book moves found was
c------------------------------< zero, return a failure indication to
c------------------------------< start the regular search.
c
800   continue
      inbook=0
      if(number .gt. 0) inbook=1
      if(inbook .le. 0) then
          if(bookdev .eq. 10) return
              bookdev=10
              go to 100
      endif
      depth=0
c
c------------------------------< several book positions were found.
c------------------------------< first scan the list and select the
c------------------------------< highest evaluation.
c
      eval=-3999999
      do 900 i=1,number
          eval=max0(eval,trace(2,i))
900   continue
c
c------------------------------< now scan the list and zap all moves
c------------------------------< that are not within the specified score
c------------------------------< of the best move.
c
      num=number
      do 1000 i=1,number
          if(eval-margin .gt. trace(2,i)) then
              trace(1,i)=0
              num=num-1
          endif
1000  continue
c
c------------------------------< now generate a random number between
c------------------------------< 1 and num to select one of the moves
c------------------------------< that pass the criterion test.
c
      call cptime(rsecs,msecs)
      move=mod(rsecs,num)+1
c
c------------------------------< now locate the selected move and return
c------------------------------< it.
c
      do 1100 i=1,number
          if(trace(1,i) .ne. 0) then
              move=move-1
              if(move .le. 0) go to 1200
          endif
1100  continue
1200  continue
      trace(1,1)=trace(1,i)
      trace(51,1)=1
      trace(52,1)=0
      tscore=trace(2,i)
      value(1)=tscore
c
c------------------------------< now determine if there is a move
c------------------------------< that the opponent should make. if
c------------------------------< so, save it also.
c
      mfrom=extrct(trace(1,1))
      call make
      ply=2
      player=2
      inchk(2)=checki(2)
      side=-1
      first(2)=last(1)+1
      dtemp=rdepth
      rdepth=100
      call movgen
      rdepth=dtemp
c
c------------------------------< determine if any of the moves that
c------------------------------< the opponent can make are book moves.
c
      do 1400 where=first(2),last(2)
          mfrom=extrct(moves(where))
          call make
          hkey=and(hash,bmask)
          call unmake
          atemp=and(hkey,65535)
          key=mod(atemp,sizebook/2)+(2-color)*sizebook/2+1
          read(unit=bookdev,rec=key) record
          do 1300 i=1,100,2
              if(hkey .ne. record(i)) go to 1300
                  if(value(1) .eq. -record(i+1)) go to 1500
1300      continue
1400  continue
          go to 1600
1500  continue
          trace(2,1)=moves(where)
          trace(51,1)=2
1600  continue
          mfrom=extrct(trace(1,1))
          ply=1
          player=1
          side=1
          call unmake
          return
      end
