      subroutine forsythe(txt)
crev  last revision 04/05/91
c
c     ******************************************************************
c     *                                                                *
c     *       forsythe encodes the current board position into the     *
c     *  so-called 'forsythe' chess notation.  this encoding can be    *
c     *  used to reconstruct the current position at a later time.     *
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
      equivalence (semic,alpha(77)), (blank,alpha(70))
      dimension pcs(14), empty(8), txt(80), colors(2), bstat(2),
     *          wstat(2)
      data pcs / 'K','Q','R','B','N','P',' ','p','n','b',
     *           'r','q','k','/' /
      data empty / '1','2','3','4','5','6','7','8' /
      data colors / 'w', 'b' /
      data bstat / 'k', 'q' /
      data wstat / 'K', 'Q' /
c
c------------------------------< encode the current position.
c------------------------------< caps=white pieces, lowerc=black
c------------------------------< pieces, integers = successive
c------------------------------< empty squares, /=end of rank.
c
      do 100 i=1,80
          txt(i)=blank
100   continue
c
c------------------------------< if the program is white,
c------------------------------< flip the board around.
c
      if(color .eq. 1) then
          do 300 i=2,5
              do 200 j=2,9
                  temp=board(i*10+j)
                  board(i*10+j)=board((11-i)*10+11-j)
                  board((11-i)*10+11-j)=temp
200           continue
300       continue
      endif
      index=0
      do 500 rk=20,90,10
          emp=0
          do 400 fl=2,9
              if(board(rk+fl) .ne. 0) then
                  if(emp .ne. 0) then
                      index=index+1
                      txt(index)=empty(emp)
                      emp=0
                  endif
                  index=index+1
                  pc=board(rk+fl)*(color-1)+board(rk+fl)*(color-2)      
                  txt(index)=pcs(pc+7)
                  go to 400
              endif
              emp=emp+1
400       continue
          index=index+1
          txt(index)=pcs(14)
500   continue 
      index=index+2
      txt(index)=colors(color) 
      index=index+1
      do 600 i=1,2
          if(and(movedr(1),i) .ne. 0) then
              index=index+1
              txt(index)=wstat(i)
          endif
600   continue
      index=index+1
      do 700 i=1,2
          if(and(movedr(2),i) .ne. 0) then
              index=index+1
              txt(index)=bstat(i)
          endif
700   continue
c
c------------------------------< if the program is white,
c------------------------------< flip the board around.
c
      if(color .eq. 1) then
          do 900 i=2,5
              do 800 j=2,9
                  temp=board(i*10+j)
                  board(i*10+j)=board((11-i)*10+11-j)
                  board((11-i)*10+11-j)=temp
800           continue
900       continue
      endif
      return
      end
