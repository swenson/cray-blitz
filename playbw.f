      subroutine playbw(rcolor)
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      playbw is used to change colors once a game is in prog-   *
c     *  ress.  by using a command such as 'play white', the program   *
c     *  can change from black to white and then after a 'go' command, *
c     *  will select a move for that side.  by continually switching   *
c     *  colors, the program can play itself.                          *
c     *      note that the transposition table is destroyed if the     *
c     *  board is actually reversed.  the hashing algorithm cannot     *
c     *  cope with switching things around and gets bad values from    *
c     *  the table when this happens.  killer moves and other useful   *
c     *  information is kept and used if applicable.                   *
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
      equivalence (blank,alpha(70)),(b,alpha(28)),(w,alpha(49))
c
c
c------------------------------< first, locate the blank after
c------------------------------< the 'play' command.
c
      do 100 char=1,80
          if(atext(char) .eq. blank) go to 200
100   continue
      return
200   continue
c
c------------------------------< now, locate the next non-blank
c------------------------------< character to determine which
c------------------------------< color is desired.
c
      do 300 firstc=char,80
          if(atext(firstc) .ne. blank) go to 400
300   continue
      go to 500
c
c------------------------------< verify that the color requested is
c------------------------------< not the same as the program's color
c------------------------------< is now and then change it.
c
400   continue
          if(color.eq.1 .and. atext(firstc).eq.w) return
          if(color.eq.2 .and. atext(firstc).eq.b) return
500   continue
          color=2-and(color+1,1)
c
c------------------------------< reverse the board and pieces.
c
          do 600 sq=1,60
              if(sq.ge.20 .and. mod(sq,10).ge.2) then
                  temp=board(sq)
                  board(sq)=-board(121-sq)
                  board(121-sq)=-temp
              endif
600       continue
c
c------------------------------< now reverse the castling status to
c------------------------------< match the reversed board.
c
          temp=movedr(1)
          movedr(1)=movedr(2)
          movedr(2)=temp
          if(movedr(1).ne.0 .and. movedr(1).ne.3) then
              if(movedr(1) .ne. 1) then
                  movedr(1)=1
              else
                  movedr(1)=2
              endif
          endif
          if(movedr(2).ne.0 .and. movedr(2).ne.3) then
              if(movedr(2) .ne. 1) then
                  movedr(2)=1
              else
                  movedr(2)=2
              endif
          endif
          temp=psave(1)
          psave(1)=psave(2)
          psave(2)=temp
          if(psave(1).ne.0 .and. psave(1).ne.3) then
              if(psave(1) .ne. 1) then
                  psave(1)=1
              else
                  psave(1)=2
              endif
          endif
          if(psave(2).ne.0 .and. psave(2).ne.3) then
              if(psave(2) .ne. 1) then
                  psave(2)=1
              else
                  psave(2)=2
              endif
          endif
c
c------------------------------< now reverse the last move made
c------------------------------< by each side.
c
          do 1500 i=1,3
              temp=prevmv(i)
              prevmv(i)=prevmv(i+3)
              prevmv(i+3)=temp
              if(i .ne. 3) then
                 prevmv(i)=121-prevmv(i)
                 prevmv(i+3)=121-prevmv(i+3)
              endif
1500      continue
c
c------------------------------< reverse the file scores so the
c------------------------------< evaluator will understand which
c------------------------------< pawns are king-side.
c
          do 1600 i=2,5
              temp=pfiles(i)
              pfiles(i)=pfiles(11-i)
              pfiles(11-i)=temp
1600      continue
c
c------------------------------< reverse the save positions so
c------------------------------< that draws by repetition can be
c------------------------------< detected after the side change
c------------------------------< has been made
c
          do 1700 i=1,100,2
              temp=bdsave(i)
              bdsave(i)=bdsave(i+1)
              bdsave(i+1)=temp
1700      continue
c
c------------------------------< reverse the random number table
c------------------------------< so that the hash keys will agree with
c------------------------------< the book data base where the program
c------------------------------< was assumed to be white, and so that
c------------------------------< the draw table hashing will be correct.
c
      if(rcolor .ne. color) then
          do 1900 i=1,100
              do 1800 j=1,6
                  temp=random(i,j)
                  random(i,j)=random(i,14-j)
                  random(i,14-j)=temp
1800          continue
1900      continue
          do 2100 i=21,60
              do 2000 j=1,13
                  temp=random(i,j)
                  random(i,j)=random(121-i,j)
                  random(121-i,j)=temp
2000          continue
2100      continue
      endif
c
c------------------------------< now reset all of the minor search
c------------------------------< controls since predicted moves,
c------------------------------< expected variations, etc. are no
c------------------------------< good.
c
          pmove=0
          call locate
          ply=0
c
c------------------------------< reverse the timing information
c
          temp=npmovs
          npmovs=nomovs
          nomovs=temp
          temp=pelap
          pelap=oelap
          oelap=temp
c
c------------------------------< zero the transposition table.
c
          do 2200 i=1,(hsize+7168)*3+3*(psize/2)+2*ksize
              htable(i)=0
2200      continue
          return
      end
