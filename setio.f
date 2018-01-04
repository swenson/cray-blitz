      subroutine setio(contin)
crev  last revision 12/09/90
c
c     ******************************************************************
c     *                                                                *
c     *      setio is used to open fortran i/o units 1, 2 and 3 to     *
c     *  specific file names based on the opponent's name.             *
c     *      'savegm' is an entry point used to save the entire        *
c     *  program data area after each move so that the game may be     *
c     *  restarted after system/communication failure without losing   *
c     *  any status.                                                   *
c     *      setio asks the user if the game is being restarted.  if   *
c     *  so, the entire data area is restored from disc to return      *
c     *  to the game as it was at the time of failure.                 *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      character*20 tname
      character*21 fname1, fname2, fname3
      external control
c
c
      include 'global.f'
c
c
      include 'common.f'
c
c
      dimension tskarray(100),digits(10)
      equivalence (capn,alpha(14)),(capy,alpha(25)),
     *            (litn,alpha(40)),(lity,alpha(51)),
     *            (blank,alpha(70)),(digits(1),alpha(53))
c
c------------------------------< determine whether to create new
c------------------------------< save files or read in existing ones
c------------------------------< to restart an old game.
c
      return=0
      tname=name
      fname1='a'//tname
      fname2='b'//tname
      fname3=name
      if(contin.ne.capy .and. contin.ne.lity) then
          open (unit=1,status='unknown',file=fname1,access='direct',
     *          form='formatted',recl=80)
          open (unit=2,status='unknown',file=fname2,
     *          access='sequential',form='formatted')
          open (unit=3,status='unknown',file=fname3,
     *          access='sequential',form='formatted')
          do 600 i=1,400
              write(unit=1,fmt=500,rec=i) 
500           format('...',77x)
600       continue
          go to 1200
      else
          open (unit=1,status='old',file=fname1,access='direct',
     *          form='formatted',recl=80,err=1000)
          open (unit=2,status='old',file=fname2,
     *          access='sequential', form='formatted',err=1000)
          open (unit=3,status='old',file=fname3,
     *          access='sequential', form='formatted',err=1000)
          do 800 i=1,100000
              read(3,700,end=900) text
700           format(80a1)
800       continue
900       continue
      endif
      backspace 3
      return
c
c------------------------------< the indicated filename did not
c------------------------------< exist!
c
1000  continue
          print 1100
1100      format(///'  error:  no file with that name exists.'/
     *              '  please retry.'//)
          stop
c
c------------------------------< to restart a game, read in the old
c------------------------------< game status to restore the program to
c------------------------------< the point it was at when the game was
c------------------------------< interrupted.
c
      entry restgm
      oldmem=3*(hsize+7168)+2*ksize+3*psize/2
      rewind 2
      read(2,1101) autos, numout, margin, color,
     *         drawsc, fdepth, ftime, snodes, tmode, npmovs,
     *         nomovs, name, pmove, prate, orate
      read(2,1102) timlim, surpls, cntrls, cputim, cquery, cbegin,
     *             pelap, oelap, ncpus, hsize, psize, ksize, minply,
     *             vttype
      read(2,1104) prcmnds
1101  format(11(t30,i10/),t30,a20,3(/t30,i10))
1102  format(30(t30,i10/))
1104  format(/10(5a1/))
      atext(1)=blank
      atext(2)=blank
      hd=(nomovs+1)/100
      td=((nomovs+1)-hd*100)/10
      ud=mod(nomovs+1,10)
      atext(3)=digits(hd+1)
      atext(4)=digits(td+1)
      atext(5)=digits(ud+1)
      atext(6)=blank
      spmove=pmove
      call rcmnd
      pmove=spmove
      hsize=2**hsize
      hsizm1=hsize-1
      psize=2**psize
      psizm1=psize-1
      ksize=2**ksize
      hsizm1=hsize-1
      htbl1=7168+hsize
      htbl2=htbl1*2
      ktbl1=htbl2+hsize+7168+1
      ktbl2=ktbl1+ksize
      ptbl1=ktbl2+ksize+1
      ptbl2=ptbl1+psize
      newmem=3*(hsize+7168)+2*ksize+3*psize/2
      call memory('UC',newmem-oldmem)
      do 1110 i=1,ncpus-1
          tskarray((i-1)*3+1)=3
          tskarray((i-1)*3+2)=0
          tskarray((i-1)*3+3)=i
c         call tskstart(tskarray((i-1)*3+1),control,0)
1110  continue
      backspace 3
      return=1
      return
c
c------------------------------< savegm is called to write all
c------------------------------< useful common blocks to fortran unit
c------------------------------< 2 so that a game may be restarted
c------------------------------< if necessary.
c
      entry savegm
1200  continue
      rewind 2
      write(2,1201) autos, numout, margin, color,
     *         drawsc, fdepth, ftime, snodes, tmode, npmovs,
     *         nomovs, name, pmove, prate, orate
      write(2,1202) timlim, surpls, cntrls, cputim, cquery, cbegin,
     *              pelap, oelap, ncpus, optlog2(hsize),
     *              optlog2(psize), optlog2(ksize), minply, vttype
      write(2,1204) prcmnds
      rewind 2
1201  format(1x,'automatic stats',t30,i10/
     *       1x,'first move out of book',t30,i10/
     *       1x,'book scoring margin',t30,i10/
     *       1x,'program color',t30,i10/
     *       1x,'draw score',t30,i10/
     *       1x,'absolute search depth',t30,i10/
     *       1x,'absolute search time',t30,i10/
     *       1x,'nodes before stat display',t30,i10/
     *       1x,'tournament mode flag',t30,i10/
     *       1x,'program moves made',t30,i10/
     *       1x,'opponent moves made',t30,i10/
     *       1x,'opponents name',t30,a20/
     *       1x,'predicted move',t30,i10/
     *       1x,'programs rating',t30,i10/
     *       1x,'opponents rating',t30,i10)
1202  format(1x,'first timing parameter',t30,i10/
     *       1x,'second timing parameter',t30,i10/
     *       1x,'third timing parameter',t30,i10/
     *       1x,'fourth timing parameter',t30,i10/
     *       1x,'fifth timing parameter',t30,i10/
     *       1x,'clock surplus',t30,i10/
     *       1x,'time controls reached',t30,i10/
     *       1x,'use cpu time flag',t30,i10/
     *       1x,'clock query flag',t30,i10/
     *       1x,'beginning clock time',t30,i10/
     *       1x,'programs elapsed time',t30,i10/
     *       1x,'opponents elapsed time',t30,i10/
     *       1x,'number of processors',t30,i10/
     *       1x,'hash table size',t30,i10/
     *       1x,'pawn hash table size',t30,i10/
     *       1x,'king safety hash table size',t30,i10/
     *       1x,'minimum split ply',t30,i10/
     *       1x,'video device type',t30,i10/)
1204  format(1x,'programmed commands follow'
     *       /10(5a1/))
9999  continue
      return=0
      return
      end
