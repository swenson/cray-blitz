      subroutine packlook(id)
      implicit integer (a-z)
      include 'global.f'
      include 'common.f'
      common /ptable/ ptable(131072,8)
      dimension zboard(100)
      if(id.lt.1 .or. id.gt.131072) print 50, id
50    format(///1x,'--------------bad id to packlook = ',i9)
c
c----------<  compare saved and real boards
c
      text(2)=' '
      do 200 i=2,9,2
          pack=0
          do 100 j=2,9
              pack=ishft(pack,4)+board(i*10+j)+6
100       continue
          do 150 j=2,9
              pack=ishft(pack,4)+board((i+1)*10+j)+6
150       continue
          if(pack .ne. ptable(id,i/2)) goto 300
200   continue
      return
300   continue
      print 400, hashbd, htable(rkey1)
400   format(//'--current hash=',z16,'  htable=',z16/
     *       1x,'------------- current board --------------'/)
      call dcmnd
c
c----------<  compute hash for real board
c
      hashbd=0
      do 450 i=2,9
          do 450 j=2,9
              temp=random(i*10+j,board(i*10+j)+7)
              hashbd=xor(hashbd,temp)
450   continue
      hashbd=xor(hashbd,random(or(moveds(ply-2),
     *                            ishft(moveds(ply-1),2))+1,2))
      if(board(to(ply-1)).eq.-side .and.
     *   iabs(to(ply-1)-from(ply-1)).eq.20 .and.
     *   (board(to(ply-1)-1).eq.side.or.
     *    board(to(ply-1)+1).eq.side))
     *    hashbd=xor(hashbd,random(cfiles(to(ply-1)),1))
      print 475, hashbd
475   format(1x,'correct hash for current board=',z16//)
c
c----------< now compute hash for saved board
c
      do 500 i=1,100
          zboard(i)=board(i)
500   continue
      do 700 i=2,9
          do 600 j=2,9
              sqval=ptable(id,i-1)
              sqval=ishft(sqval,-(9-j)*4)
              sqval=and(sqval,15)-6
              board(i*10+j)=sqval
600       continue
700   continue
      print 800
800   format(1x,'------------- hash table board --------------'/)
      call dcmnd
      xhashbd=0
      do 900 i=2,9
          do 900 j=2,9
              temp=random(i*10+j,board(i*10+j)+7)
              xhashbd=xor(xhashbd,temp)
900   continue
      xhashbd=xor(xhashbd,ptable(id,5))
      xhashbd=xor(xhashbd,ptable(id,6))
      print 1000, xhashbd
1000  format(1x,'hash for saved board=',z16//)
      print 1100
1100  format(/1x,'------------- differences --------------'/)

      do 1200 i=22,99
          if(board(i) .eq. zboard(i)) board(i)=0
1200  continue
      call dcmnd
      do 2000 i=1,100
          board(i)=zboard(i)
2000  continue
      return
      end
