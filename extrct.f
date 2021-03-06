      integer function extrct(temp)
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      extrct is used to decode the compressed move that is      *
c     *  generated by the various move generators.  the move is kept   *
c     *  in the following 23-bit form except when being analyzed:      *
c     *                                                                *
c     *        mmmcc ctttdd ddddds ssssss                              *
c     *                                                                *
c     *      mmm  = moving piece                                       *
c     *                                                                *
c     *      ccc  = captured piece (0=none)                            *
c     *                                                                *
c     *      ttt  = move type as follows:                              *
c     *             0 = normal move                                    *
c     *             1 = castle king-side                               *
c     *             2 = castle queen-side                              *
c     *             3 = en passant pawn capture                        *
c     *             4 = pawn promotion to knight                       *
c     *             5 = pawn promotion to bishop                       *
c     *             6 = pawn promotion to rook                         *
c     *             7 = pawn promotion to queen                        *
c     *                                                                *
c     *      ddddddd  = destination square                             *
c     *                                                                *
c     *      sssssss  = source square                                  *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
c
c
      include 'common.f'
c
c
c
c
c
c------------------------------< shift each field and extract using
c------------------------------< the 'and' function.
c
      mtype=and(ishft(temp,-14),7)
      mcappc=and(ishft(temp,-17),7)
      mto=and(ishft(temp,-7),127)
      movgpc=ishft(temp,-20)
      mpropc=mtype-2
      if(mtype .lt. 4) mpropc=0
      extrct=and(temp,127)
      if(mto .gt. 99) then
          mtype=8
          mcappc=0
          mto=0
          movgpc=0
          mpropc=0
          extrct=0
      endif
      return
      end
      integer function extrctf(temp)
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      extrct is used to decode the compressed move that is      *
c     *  generated by the various move generators.  the move is kept   *
c     *  in the following 23-bit form except when being analyzed:      *
c     *                                                                *
c     *        mmmcc ctttdd ddddds ssssss                              *
c     *                                                                *
c     *      mmm  = moving piece                                       *
c     *                                                                *
c     *      ccc  = captured piece (0=none)                            *
c     *                                                                *
c     *      ttt  = move type as follows:                              *
c     *             0 = normal move                                    *
c     *             1 = castle king-side                               *
c     *             2 = castle queen-side                              *
c     *             3 = en passant pawn capture                        *
c     *             4 = pawn promotion to knight                       *
c     *             5 = pawn promotion to bishop                       *
c     *             6 = pawn promotion to rook                         *
c     *             7 = pawn promotion to queen                        *
c     *                                                                *
c     *      ddddddd  = destination square                             *
c     *                                                                *
c     *      sssssss  = source square                                  *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
c
c
      include 'common.f'
c
c
c
c
c
c------------------------------< shift each field and extract using
c------------------------------< the 'and' function.
c
      mtype=and(ishft(temp,-14),7)
      mcappc=and(ishft(temp,-17),7)
      mto=and(ishft(temp,-7),127)
      movgpc=ishft(temp,-20)
      mpropc=mtype-2
      if(mtype .lt. 4) mpropc=0
      extrctf=and(temp,127)
      if(mto .gt. 99) then
          mtype=8
          mcappc=0
          mto=0
          movgpc=0
          mpropc=0
          extrctf=0
      endif
      return
      end
