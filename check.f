      integer function checkg(playr)
crev  last revision 08/29/91
c
c     ******************************************************************
c     *                                                                *
c     *      checkg is called to determine if the current move (in     *
c     *  mfrom, mto, mtype, etc) checks the opponent.  It uses the     *
c     *  new bit board attack and returns zero/nonzero for no check    *
c     *  or gives check.                                               *
c     *                                                                *
c     ******************************************************************
c
c
c    mtype:        0 = normal move
c                  1 = castle king-tside
c                  2 = castle queen-tside
c                  3 = en passant pawn capture
c                  4 = pawn promotion to knight
c                  5 = pawn promotion to bishop
c                  6 = pawn promotion to rook
c                  7 = pawn promotion to queen
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
c------------------------------< initialize
c
      tplayr=playr
      tside=cvside(playr)
c
c------------------------------< if not a normal move, call checkgu
c
      if(mtype .ne. 0) then
          checkg=checkgu(0)
c
c------------------------------< normal move
c
      else
          tbd=and(or(bitbd,bit0(mto)),cbit0(mfrom))
          savpf=plist(pboard(mfrom))
          plist(pboard(mfrom))=mto
c
c------------------------------< normal attack
c
          checkg=-1
          do 100 i=pstart(playr),pend(playr)
              if(plist(i) .ne. 0) then
                  rtemp=and(at(plist(i),kloc(3-playr),vlist(i)+7),tbd)
                  if(rtemp .eq. 0) go to 200
              endif
100       continue
          checkg=0
200       continue
c
c------------------------------< normal unmove
c
          plist(pboard(mfrom))=savpf
      endif
      return
      end
      integer function checkgu(dummy)
crev  last revision 08/29/91
c
c     ******************************************************************
c     *                                                                *
c     *     checkgu is the new mover mover-attack-umover combination.  *
c     *  it toggles the arrays plist and vlist, as well as bitbd and   *
c     *  kloc(tplayr). checkgu handles unusual moves only.             *
c     *                                                                *
c     ******************************************************************
c
c
c    mtype:        0 = normal move
c                  1 = castle king-tside
c                  2 = castle queen-tside
c                  3 = en passant pawn capture
c                  4 = pawn promotion to knight
c                  5 = pawn promotion to bishop
c                  6 = pawn promotion to rook
c                  7 = pawn promotion to queen
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
c         bias color      bits
      data msk01 /o'1700000000000000000000'/
      data msq01 /o'0174000000000000000000'/
      data msk71 /o'0000000000000000000360'/
      data msq71 /o'0000000000000000000037'/
      data msk02 /o'0074000000000000000000'/
      data msq02 /o'1740000000000000000000'/
      data msk72 /o'0000000000000000000017'/
      data msq72 /o'0000000000000000000370'/
      data ack01 /o'0600000000000000000000'/
      data ack02 /o'0030000000000000000000'/
      data ack71 /o'0000000000000000000140'/
      data ack72 /o'0000000000000000000006'/
      data acq01 /o'0060000000000000000000'/
      data acq02 /o'0300000000000000000000'/
      data acq71 /o'0000000000000000000014'/
      data acq72 /o'0000000000000000000060'/
c
c
      go to (100,300,500,600,600,600,600),mtype
c
c------------------------------< castle king-side
c
100   continue
          bias=cbias(tplayr)
          if(color .eq. 1) then
              if(bias.eq.0) tbd=csmgx(ack01,bitbd,msk01)
              if(bias.ne.0) tbd=csmgx(ack71,bitbd,msk71)
              plist(pboard(bias+25))=bias+23
              plist(pboard(bias+22))=bias+24
              kloc(tplayr)=bias+23
          else
              if(bias.eq.0) tbd=csmgx(ack02,bitbd,msk02)
              if(bias.ne.0) tbd=csmgx(ack72,bitbd,msk72)
              plist(pboard(bias+26))=bias+28
              plist(pboard(bias+29))=bias+27
              kloc(tplayr)=bias+28
          endif
          go to 700
c
c------------------------------< castle queen-side
c
300   continue
          bias=cbias(tplayr)
          if(color .eq. 1) then
              if(bias.eq.0) tbd=csmgx(acq01,bitbd,msq01)
              if(bias.ne.0) tbd=csmgx(acq71,bitbd,msq71)
              plist(pboard(bias+25))=bias+27
              plist(pboard(bias+29))=bias+26
              kloc(tplayr)=bias+27
          else
              if(bias.eq.0) tbd=csmgx(acq02,bitbd,msq02)
              if(bias.ne.0) tbd=csmgx(acq72,bitbd,msq72)
              plist(pboard(bias+26))=bias+24
              plist(pboard(bias+22))=bias+25
              kloc(tplayr)=bias+24
          endif
          go to 700
c
c------------------------------< en passant pawn capture
c
500   continue
          savpf=plist(pboard(mfrom))
          savpt=plist(pboard(mto-10*tside))
          savvt=vlist(pboard(mto-10*tside))
          plist(pboard(mfrom))=mto
          plist(pboard(mto-10*tside))=0
          vlist(pboard(mto-10*tside))=0
          tbd=and(or(and(bitbd,cbit0(mfrom)),bit0(mto)),
     *                     cbit0(mto-10*tside))
          go to 700
c
c------------------------------< pawn promotion
c
600   continue
          tbd=and(or(bitbd,bit0(mto)),cbit0(mfrom))
          savpf=plist(pboard(mfrom))
          savpt=plist(pboard(mto))
          savvf=vlist(pboard(mfrom))
          savvt=vlist(pboard(mto))
          plist(pboard(mto))=0
          plist(pboard(mfrom))=mto
          vlist(pboard(mto))=0
          vlist(pboard(mfrom))=mpropc*tside
          go to 700
700   continue
      checkgu=-1
      do 800 i=pstart(tplayr),pend(tplayr)
          if(plist(i) .ne. 0) then
              rtemp=and(at(plist(i),kloc(3-tplayr),vlist(i)+7),tbd)
              if(rtemp .eq. 0) go to 900
          endif
800   continue
      checkgu=0
900   continue
c
      go to (1000,1200,1400,1500,1500,1500,1500),mtype
c
1000  continue
          if(color .eq. 1) then
              kloc(tplayr)=bias+25
              plist(pboard(bias+25))=bias+25
              plist(pboard(bias+22))=bias+22
          else
              plist(pboard(bias+26))=bias+26
              plist(pboard(bias+29))=bias+29
              kloc(tplayr)=bias+26
          endif
          return
1200  continue
          if(color .eq. 1) then
              plist(pboard(bias+25))=bias+25
              plist(pboard(bias+29))=bias+29
              kloc(tplayr)=bias+25
          else
              plist(pboard(bias+26))=bias+26
              plist(pboard(bias+22))=bias+22
              kloc(tplayr)=bias+26
          endif
          return
c
c------------------------------< en passant pawn capture
c
1400  continue
          plist(pboard(mfrom))=savpf
          plist(pboard(mto-10*tside))=savpt
          vlist(pboard(mto-10*tside))=savvt
          return
c
c------------------------------< pawn promotion
c
1500  continue
          plist(pboard(mfrom))=savpf
          plist(pboard(mto))=savpt
          vlist(pboard(mfrom))=savvf
          vlist(pboard(mto))=savvt
          return
      end
      integer function checki(playr)
crev  last revision 08/29/91
c
c     ******************************************************************
c     *                                                                *
c     *      checki is called to determine if the current move (in     *
c     *  mfrom, mto, mtype, etc) leaves the moving side in check       *
c     *  (which would be illegal).  it uses the new bit board attack   *
c     *  and returns zero/nonzero for ok/in check.                     *
c     *                                                                *
c     ******************************************************************
c
c
c    mtype:        0 = normal move
c                  1 = castle king-tside
c                  2 = castle queen-tside
c                  3 = en passant pawn capture
c                  4 = pawn promotion to knight
c                  5 = pawn promotion to bishop
c                  6 = pawn promotion to rook
c                  7 = pawn promotion to queen
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
c------------------------------< initialize
c
      tplayr=playr
      tside=cvside(playr)
c
c------------------------------< if not a normal move, call checkiu
c
      if(mtype .ne. 0) then
          checki=checkiu(0)
      else
          tbd=and(or(bitbd,bit0(mto)),cbit0(mfrom))
          savpt=plist(pboard(mto))
          plist(pboard(mto))=0
c
c------------------------------< if the king is being moved, update
c------------------------------< it's location.
c
          ktemp=kloc(playr)
          if(iabs(board(mfrom)) .eq. 6) ktemp=mto
          checki=-1
          do 300 i=pstart(3-playr),pend(3-playr)
              if(plist(i) .ne. 0) then
                  rtemp=and(at(plist(i),ktemp,vlist(i)+7),tbd)
                  if(rtemp .eq. 0) go to 400
              endif
300       continue
          checki=0
400       continue
          plist(pboard(mto))=savpt
      endif
      return
      end
      integer function checkiu(dummy)
crev  last revision 08/29/91
c
c     ******************************************************************
c     *                                                                *
c     *     checkiu is the new mover mover-attack-umover combination.  *
c     *  it toggles the arrays plist and vlist, as well as bitbd and   *
c     *  kloc(tplayr). checkiu handles unusual moves only.             *
c     *                                                                *
c     ******************************************************************
c
c
c    mtype:        0 = normal move
c                  1 = castle king-tside
c                  2 = castle queen-tside
c                  3 = en passant pawn capture
c                  4 = pawn promotion to knight
c                  5 = pawn promotion to bishop
c                  6 = pawn promotion to rook
c                  7 = pawn promotion to queen
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
c         bias color      bits
      data msk01 /o'1700000000000000000000'/
      data msq01 /o'0174000000000000000000'/
      data msk71 /o'0000000000000000000360'/
      data msq71 /o'0000000000000000000037'/
      data msk02 /o'0074000000000000000000'/
      data msq02 /o'1740000000000000000000'/
      data msk72 /o'0000000000000000000017'/
      data msq72 /o'0000000000000000000370'/
      data ack01 /o'0600000000000000000000'/
      data ack02 /o'0030000000000000000000'/
      data ack71 /o'0000000000000000000140'/
      data ack72 /o'0000000000000000000006'/
      data acq01 /o'0060000000000000000000'/
      data acq02 /o'0300000000000000000000'/
      data acq71 /o'0000000000000000000014'/
      data acq72 /o'0000000000000000000060'/
c
c
      go to (100,300,500,600,600,600,600),mtype
c
c------------------------------< castle king-tside
c
100   continue
          bias=cbias(tplayr)
          if(color .eq. 1) then
              if(bias.eq.0) tbd=csmgx(ack01,bitbd,msk01)
              if(bias.ne.0) tbd=csmgx(ack71,bitbd,msk71)
              plist(pboard(bias+25))=bias+23
              plist(pboard(bias+22))=bias+24
              kloc(tplayr)=bias+23
          else
              if(bias.eq.0) tbd=csmgx(ack02,bitbd,msk02)
              if(bias.ne.0) tbd=csmgx(ack72,bitbd,msk72)
              plist(pboard(bias+26))=bias+28
              plist(pboard(bias+29))=bias+27
              kloc(tplayr)=bias+28
          endif
          go to 700
c
c------------------------------< castle queen-tside
c
300   continue
          bias=cbias(tplayr)
          if(color .eq. 1) then
              if(bias.eq.0) tbd=csmgx(acq01,bitbd,msq01)
              if(bias.ne.0) tbd=csmgx(acq71,bitbd,msq71)
              plist(pboard(bias+25))=bias+27
              plist(pboard(bias+29))=bias+26
              kloc(tplayr)=bias+27
          else
              if(bias.eq.0) tbd=csmgx(acq02,bitbd,msq02)
              if(bias.ne.0) tbd=csmgx(acq72,bitbd,msq72)
              plist(pboard(bias+26))=bias+24
              plist(pboard(bias+22))=bias+25
              kloc(tplayr)=bias+24
          endif
          go to 700
c
c------------------------------< en passant pawn capture
c
500   continue
          savpf=plist(pboard(mfrom))
          savpt=plist(pboard(mto-10*tside))
          savvt=vlist(pboard(mto-10*tside))
          plist(pboard(mfrom))=mto
          plist(pboard(mto-10*tside))=0
          vlist(pboard(mto-10*tside))=0
          tbd=and(or(and(bitbd,cbit0(mfrom)),bit0(mto)),
     *                     cbit0(mto-10*tside))
          go to 700
c
c------------------------------< pawn promotion
c
600   continue
          tbd=and(or(bitbd,bit0(mto)),cbit0(mfrom))
          savpf=plist(pboard(mfrom))
          savpt=plist(pboard(mto))
          savvf=vlist(pboard(mfrom))
          savvt=vlist(pboard(mto))
          plist(pboard(mto))=0
          plist(pboard(mfrom))=mto
          vlist(pboard(mto))=0
          vlist(pboard(mfrom))=mpropc*tside
          go to 700
700   continue
      checkiu=-1
      do 800 i=pstart(3-tplayr),pend(3-tplayr)
          if(plist(i) .ne. 0) then
              rtemp=and(at(plist(i),kloc(tplayr),vlist(i)+7),tbd)
              if(rtemp .eq. 0) go to 900
          endif
800   continue
      checkiu=0
900   continue
c
      go to (1000,1200,1400,1500,1500,1500,1500),mtype
c
1000  continue
          if(color .eq. 1) then
              kloc(tplayr)=bias+25
              plist(pboard(bias+25))=bias+25
              plist(pboard(bias+22))=bias+22
          else
              plist(pboard(bias+26))=bias+26
              plist(pboard(bias+29))=bias+29
              kloc(tplayr)=bias+26
          endif
          return
1200  continue
          if(color .eq. 1) then
              plist(pboard(bias+25))=bias+25
              plist(pboard(bias+29))=bias+29
              kloc(tplayr)=bias+25
          else
              plist(pboard(bias+26))=bias+26
              plist(pboard(bias+22))=bias+22
              kloc(tplayr)=bias+26
          endif
          return
c
c------------------------------< en passant pawn capture
c
1400  continue
          plist(pboard(mfrom))=savpf
          plist(pboard(mto-10*tside))=savpt
          vlist(pboard(mto-10*tside))=savvt
          return
c
c------------------------------< pawn promotion
c
1500  continue
          plist(pboard(mfrom))=savpf
          plist(pboard(mto))=savpt
          vlist(pboard(mfrom))=savvf
          vlist(pboard(mto))=savvt
          return
      end
