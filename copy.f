      subroutine copyg(blockid)
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      copyg is used to copy data from the permanent common area *
c     *  to the local or private sub-task common.                      *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
c
c
      include 'common.f'
c
c
      include 'perm.f'
c
c
      include 'global.f'
c
c
      do 100 i=1,749
          tdata(i)=qdata(i,blockid)
100   continue
      do 200 i=1,qdata(663,blockid)
          cappb(i)=qdata(899+i,blockid)
          cappc(i)=qdata(999+i,blockid)
          cappt(i)=qdata(1049+i,blockid)
          cutmvs(i)=qdata(1099+i,blockid)
          first(i)=qdata(1149+i,blockid)
          from(i)=qdata(1199+i,blockid)
          givchk(i)=qdata(1249+i,blockid)
          hmove(i)=qdata(1299+i,blockid)
          inchk(i)=qdata(1349+i,blockid)
          last(i)=qdata(1399+i,blockid)
          mated(i)=qdata(1449+i,blockid)
          moveds(i)=qdata(1501+i,blockid)
          msave1(i)=qdata(1551+i,blockid)
          msave2(i)=qdata(1601+i,blockid)
          msave3(i)=qdata(1651+i,blockid)
          msave4(i)=qdata(1701+i,blockid)
          msave5(i)=qdata(1751+i,blockid)
          msave6(i)=qdata(1801+i,blockid)
          msave7(i)=qdata(1851+i,blockid)
          msave8(i)=qdata(1901+i,blockid)
          msave9(i)=qdata(1951+i,blockid)
          msave10(i)=qdata(2001+i,blockid)
          msave11(i)=qdata(2051+i,blockid)
          odepth(i)=qdata(2101+i,blockid)
          onerep(i)=qdata(5054+i,blockid)
          phase(i)=qdata(2151+i,blockid)
          status(i)=qdata(2201+i,blockid)
          to(i)=qdata(2251+i,blockid)
          type(i)=qdata(4901+i,blockid)
          value(i)=qdata(4953+i,blockid)
          which(i)=qdata(5003+i,blockid)
200   continue
      movedr(1)=qdata(1500,blockid)
      movedr(2)=qdata(1501,blockid)
      do 300 i=1,point+qdata(663,blockid)
          bdsave(i)=qdata(749+i,blockid)
300   continue
      lim=max0(qdata(1399+qdata(663,blockid),blockid),
     *         qdata(1399+qdata(663,blockid)-1,blockid))
      do 400 i=1,lim
          moves(i)=qdata(5599+i,blockid)
400   continue
      do 600 level=1,qdata(663,blockid)
          do 500 i=1,52
              trace(i,level)=qdata(2301+i+52*(level-1),blockid)
500       continue
600   continue
      return
      end
      subroutine copyl(blockid)
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      copyl is used to copy data from the local common area to  *
c     *  the global common.                                            *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
c
c
      include 'common.f'
c
c
      include 'perm.f'
c
c
      include 'global.f'
c
c
      do 100 i=1,749
          qdata(i,blockid)=tdata(i)
100   continue
      do 200 i=1,ply
          qdata(899+i,blockid)=cappb(i)
          qdata(999+i,blockid)=cappc(i)
          qdata(1049+i,blockid)=cappt(i)
          qdata(1099+i,blockid)=cutmvs(i)
          qdata(1149+i,blockid)=first(i)
          qdata(1199+i,blockid)=from(i)
          qdata(1249+i,blockid)=givchk(i)
          qdata(1299+i,blockid)=hmove(i)
          qdata(1349+i,blockid)=inchk(i)
          qdata(1399+i,blockid)=last(i)
          qdata(1449+i,blockid)=mated(i)
          qdata(1501+i,blockid)=moveds(i)
          qdata(1551+i,blockid)=msave1(i)
          qdata(1601+i,blockid)=msave2(i)
          qdata(1651+i,blockid)=msave3(i)
          qdata(1701+i,blockid)=msave4(i)
          qdata(1751+i,blockid)=msave5(i)
          qdata(1801+i,blockid)=msave6(i)
          qdata(1851+i,blockid)=msave7(i)
          qdata(1901+i,blockid)=msave8(i)
          qdata(1951+i,blockid)=msave9(i)
          qdata(2001+i,blockid)=msave10(i)
          qdata(2051+i,blockid)=msave11(i)
          qdata(2101+i,blockid)=odepth(i)
          qdata(5054+i,blockid)=onerep(i)
          qdata(2151+i,blockid)=phase(i)
          qdata(2201+i,blockid)=status(i)
          qdata(2251+i,blockid)=to(i)
          qdata(4901+i,blockid)=type(i)
          qdata(4953+i,blockid)=value(i)
          qdata(5003+i,blockid)=which(i)
200   continue
      qdata(1500,blockid)=movedr(1)
      qdata(1501,blockid)=movedr(2)
      do 300 i=1,point+ply
          qdata(749+i,blockid)=bdsave(i)
300   continue
      lim=max0(last(ply),last(ply-1))
      do 400 i=1,lim
          qdata(5599+i,blockid)=moves(i)
400   continue
      do 600 level=1,ply
          do 500 i=1,52
              qdata(2301+i+52*(level-1),blockid)=trace(i,level)
500       continue
600   continue
      return
      end
