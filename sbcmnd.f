      subroutine sbcmnd(inter)
crev  last revision 04/05/91
c
c     ******************************************************************
c     *                                                                *
c     *      sbcmnd is used to set up the board in any position        *
c     *  desired.  it uses a forsythe like string of characters        *
c     *  to describe the board position.                               *
c     *                                                                *
c     *      the standard piece codes p,n,b,r,q,k are used to denote   *
c     *  the type of piece on a square, upper/lower case are used to   *
c     *  indicate the side (program/opp.) of the piece.  if two or     *
c     *  more pieces of the same color have no opposing pieces be-     *
c     *  tween them, only one l or d is required to save time.         *
c     *                                                                *
c     *      the pieces are entered with the rank on the program's     *
c     *  side of the board entered first, and the rank on the          *
c     *  opponent's side entered last.  to enter empty squares, use    *
c     *  a number between 1 and 8 to indicate how many adjacent        *
c     *  squares are empty.  use a / to terminate each rank after all  *
c     *  of the pieces for that rank have been entered.                *
c     *                                                                *
c     *      the following input will setup the board position that    *
c     *  given below:                                                  *
c     *                                                                *
c     *      K2R/PPP////q/5ppp/7k/ b                                   *
c     *                                                                *
c     *  this assumes that k represents a white king and -q repre-     *
c     *  sents a black queen.                                          *
c     *                                                                *
c     *                  k  *  *  r  *  *  *  *                        *
c     *                  p  p  p  *  *  *  *  *                        *
c     *                  *  *  *  *  *  *  *  *                        *
c     *                  *  *  *  *  *  *  *  *                        *
c     *                  *  *  *  *  *  *  *  *                        *
c     *                 -q  *  *  *  *  *  *  *                        *
c     *                  *  *  *  *  * -p -p -p                        *
c     *                  *  *  *  *  *  *  * -k                        *
c     *                                                                *
c     *  the field after the final "/" should be either b or w to      *
c     *  indicate which side is "on move."                             *
c     *                                                                *
c     *  after this side-to-move field any of the following characters *
c     *  can appear to indicate the following:  KQ: white can castle   *
c     *  kingside/queenside/both;  kq: same for black;  a1-h8:         *
c     *  indicates the square occupied by a pawn that can be captured  *
c     *  en passant.                                                   *
c     *                                                                *
c     ******************************************************************
c
      implicit integer(a-z)
c
c
      include 'global.f'
c
c
      include 'common.f'
c
c
      equivalence (o,alpha(41)),(semic,alpha(77)),
     *            (litx,alpha(50)),(aster,alpha(66))
      dimension char(-6:15), stat(13), colors(2), plcmd(3)
      data char / 'k','q','r','b','n','p',' ','P','N','B','R','Q',
     *            'K','1','2','3','4','5','6','7','8','/' /
      data stat / 'K','Q','k','q','h','g','f',
     *            'e','d','c','b','a', ' ' /
      data colors / 'b', 'w' /
      data plcmd / 'p','l',' ' /
c
c
c------------------------------< initialize.
c
      rcolor=color
      color=1
500   continue
      if(inter .ne. 0) then
          print 1000
1000      format(1x,'enter board position, UPPER=white, lower=black.'/
     *           1x,'position must start from square (a8)'/)
          call command(eof)
          if(eof .ne. 0) return
          write(3,1100) atext
1100      format(80a1)
      endif
      if(atext(1) .eq. char(21)) return
      do 1300 i=1,150
          bdsave(i)=0
1300  continue
      do 1500 i=2,9
          do 1400 j=2,9
              board(i*10+j)=0
1400      continue
1500  continue
c
c------------------------------< now scan the input string and set
c------------------------------< the board accordingly.
c
      sq=99
      do 1800 j=1,80
          if(atext(j) .eq. semic) go to 1900
          do 1600 k=-6,15
              if(atext(j) .eq. char(k)) go to 1700
1600      continue
          go to 3300
1700      continue
          if(k .eq. 0) go to 1900
c
c------------------------------< slash indicates end-of-rank
c
          if(k .eq. 15) then
              sq=(sq/10-1)*10+9
c
c------------------------------< numbers indicate empty squares
c
          else if(k .gt. 6) then
              if(mod(sq,10)-(k-6) .lt. 1) go to 3300
              sq=sq-(k-6)
c
c------------------------------< character indicates a piece
c
          else
              if(sq .lt. 22) go to 3300
              board(sq)=k
              sq=sq-1
          endif
1800  continue
c
c------------------------------< blank indicates end of
c------------------------------< string
c
1900  continue
      bkings=0
      wkings=0
      do 2100 i=2,9
          do 2000 k=2,9
              sq=i*10+k
              if(board(sq) .eq. 6) wkings=wkings+1
              if(board(sq) .eq. -6) bkings=bkings+1
2000      continue
2100  continue
      if(bkings.ne.1 .or. wkings.ne.1) go to 3500
c
c------------------------------< determine side to move
c
      do 2120 j=j+1,80
          if(atext(j) .ne. char(0)) go to 2140
2120  continue
      go to 3700
2140  continue
      if(atext(j) .eq. colors(1)) then
          play=atext(j)
      else if(atext(j) .eq. colors(2)) then
          play=atext(j)
      else
          go to 3700
      endif
c
c------------------------------< now get castling status
c------------------------------< and enpassant status.
c
      movedr(1)=0
      movedr(2)=0
      do 3000 j=j+1,80
          do 2200 k=1,13
              if(atext(j) .eq. stat(k)) go to 2300
2200      continue
          go to 3300
2300      continue
          go to (2400,2500,2600,2700,2800,2800,2800,2800,
     *           2800,2800,2800,2800,3000), k
c
c------------------------------< white can castle king-side
c
2400      continue
              movedr(1)=or(movedr(1),1)
              go to 3000
c
c------------------------------< white can castle queen-side
c
2500      continue
              movedr(1)=or(movedr(1),2)
              go to 3000
c
c------------------------------< black can castle king-side
c
2600      continue
              movedr(2)=or(movedr(2),1)
              go to 3000
c
c------------------------------< black can castle queen-side
c
2700      continue
              movedr(2)=or(movedr(2),2)
              go to 3000
c
c------------------------------< en passant capture is possible
c
2800      continue
              epfile=k-3
              do 2810 k=7,14
                  if(atext(j+1) .eq. char(k)) go to 2820
2810          continue
              go to 3900
2820          continue
              atext(j+1)=char(0)
              k=k-5
              if(k .gt. 5) then
                  prevmv(4)=80+epfile
                  prevmv(5)=60+epfile
                  prevmv(6)=0
                  go to 3000
              else
c
c------------------------------< black can capture en passant
c
                  prevmv(1)=30+epfile
                  prevmv(2)=50+epfile
                  prevmv(3)=0
                  go to 3000
              endif
3000  continue
c
c------------------------------< if the program is black,
c------------------------------< flip the board around.
c
      do 3100 i=1,3
          atext(i)=plcmd(i)
3100  continue
      atext(4)=play
      call playbw(rcolor)
      inbook=0
      over=0
      trace(51,1)=0
      lmovep=0
      lmoveo=0
      pmove=0
      call locate
      ply=0
      point=1
      rtemp=repeat(count)
      if(inter .ne. 0) then
          call forsythe(text)
          write(unit=1,rec=1,fmt=3200) text
3200      format(80a1)
      endif
      return
c
c------------------------------< input error
c
3300  continue
          print 3400, (atext(l),l=j,80)
3400      format(1x,'input error.  data from bad character follows:'/
     *           1x,80a1)
          go to 500
3500  continue
          print 3600
3600      format(1x,'each side must have 1 king (Kk).')
          go to 500
3700  continue
          print 3800
3800      format(1x,'you must indicate side to move with b or w'/)
          go to 500
3900  continue
          print 4000
4000      format(1x,'en passant square must be [a-h][4-5]'/)
          go to 500
      end
