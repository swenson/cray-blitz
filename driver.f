      subroutine driver
crev  last revision 04/05/91
c
c     ******************************************************************
c     *                                                                *
c     *      driver is used to control the iterated search.  it        *
c     *  generates the ply one move list and then begins the iter-     *
c     *  ative process.  basically, it does a complete one ply ex-     *
c     *  haustive search, then a complete two ply exhaustive search,   *
c     *  and so forth until it has used all available time or until    *
c     *  another iteration would use excessive time based on the time  *
c     *  used by the previous iteration.                               *
c     *      each new iteration uses the same ply one move list which  *
c     *  is re-ordered by 'backup' each time a new best move is backed *
c     *  up to ply one.  in this way, the program 'learns' more about  *
c     *  the position with each iteration and also gets maximum        *
c     *  benefits from the alpha/beta algorithm due to looking at      *
c     *  the best moves first.  it should be noted that the killer     *
c     *  moves and transposition table are kept across iterations      *
c     *  (and even across searches) so that continuity from one depth  *
c     *  to the next is high.                                          *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      real running, factor, npr
c
c
      include 'global.f'
c
c
      include 'common.f'
c
c------------------------------< save starting time
c
      abort=0
      rnodes=0
      rpcalls=0
      rphashs=0
      rhashes=0
      rkhashs=0
      nsplits=0
      do 25 i=1,50
          exnodes(i)=0
          falses(i)=0
          nstops(i)=0
          splits(i)=0
          tprocs(i)=0
25    continue
      overhead=0
      cotime=0
      metime=0
      sptime=0
      untime=0
      do 50 i=1,16
          totnodes(i)=0
          idlet(i)=0
50    continue
      call cptime(rsec1,msec1)
      psec1=rsec1
      if(cputim .ne. 0) psec1=msec1
      fsec1=psec1
c
c------------------------------< mark every entry in the
c------------------------------< transposition table as "old"
c------------------------------< so that they will be overwritten.
c
      do 250 i=1,hsize+7168
         htable(i)=or(htable(i),ishft(1,63))
         htable(i+htbl2)=or(htable(i+htbl2),ishft(1,63))
250   continue
c
c------------------------------< create and sort the ply-1 move
c------------------------------< list with a "true-value" search.
c
      timeup=0
      call sortply1
c
c------------------------------< set up the timing controls by
c------------------------------< calculating how much time should
c------------------------------< be used per move.
c------------------------------< lowlim=time per move when program is
c------------------------------<    behind it's normal target time per move.
c------------------------------< midlim=time per move when program is
c------------------------------<    ahead on time, but not yet ahead by
c------------------------------<    2*surpls units.
c------------------------------< hilim=time per move when program is over
c------------------------------<    2*surpls units of time ahead.  It will
c------------------------------<    try to burn the excess by time control.
c
      if(timlim(5) .eq. 0) then
          lowlim=(timlim(2)-pelap-surpls)/max(timlim(1)-npmovs,1)
          midlim=(timlim(2)-surpls)/timlim(1)
          hilim=(timlim(2)-pelap-2*surpls)/max(timlim(1)-npmovs,1)
          hilim=min(hilim,ifix(timlim(2)/timlim(1)*1.5))
          ahead=timlim(2)-pelap-surpls-(timlim(1)-npmovs)*midlim
          if(ahead .lt. 0) then
              avgtim=lowlim
          else if(ahead .lt. surpls+10) then
              avgtim=midlim
          else
              avgtim=hilim
          endif
          if(avgtim .le. 0) avgtim=
     *               (timlim(2)-pelap-60)/max(timlim(1)-npmovs,1)
          avgtim=max(avgtim,1)
          if(autos .gt. 3) print 100, hhmmss(ahead,5), hhmmss(lowlim,5),
     *                                hhmmss(midlim,5), hhmmss(hilim,5) 
100       format(17x,'ahead=',a5,' lowlim=',a5,' midlim=',a5,
     *           ' hilim=',a5)
           if(cntrls.eq.0 .and. npmovs.ge.6 .and.
     *        timlim(1)-npmovs.ge.max0(10,timlim(3)) .and.
     *        npmovs-numout .le. 3)        then
                   factor=npmovs-numout
                   factor=(3.0-factor)/6.0
                   avgtim=avgtim+avgtim*factor
           endif
           avgtim=min0(avgtim,2*midlim)
      else
          if(pelap .lt. timlim(2)-3*timlim(1)) then
              avgtim=timlim(1)
          elseif(pelap .lt. timlim(2)+timlim(4)-5*timlim(3)) then
              avgtim=timlim(3)
          else
              avgtim=timlim(5)
          endif
      endif
      if(ftime .ne. 0) avgtim=ftime
      avgtim=avgtim*100
c
c------------------------------< print the target time for this
c------------------------------< tree search.
c
      iavg=avgtim/100
      if(easy .eq. 0) then
          if(autos.gt.1 .and. smode.eq.0) print 200, hhmmss(iavg,5)
200       format(17x,'time limit',a5,'.')
      else
          if(autos.gt.1 .and. smode.eq.0) print 300, hhmmss(iavg,5)
300       format(17x,'time limit',a5,' (easy move).')
      endif
      if(autos .gt. 1) print 400
400   format(17x,'depth ',2x,'time',5x,'eval',2x,'variation')
c
c------------------------------< determine if the program and
c------------------------------< opponent are still following
c------------------------------< known book analysis. if so,
c------------------------------< extract the next book move and
c------------------------------< return.
c
      timeup=0
      if(npmovs-numout .le. 5) then
          call book
          if(inbook .ne. 0) then
              numout=npmovs+1
              mfrom=extrct(trace(1,1))
              ply=1
              player=1
              side=1
              call make
              call inform('  ')
              ply=1
              player=1
              side=1
              mfrom=extrct(trace(1,1))
              call unmake
              go to 500
          endif
          inbook = 0;
      endif
      depth=0
      call iterate
      call cptime(rsec2, msec2)
      psec2=rsec2
      if(cputim .ne. 0) psec2=msec2
c
c------------------------------< iteration done, return to calling
c------------------------------< routine after printing the statistics
c------------------------------< if required to.
c
500   continue
      if(((abort.eq.0 .and. timeup.eq.0)  .or. pndrng.eq.0 .or. 
     *   matchd.ne.0) .and. trace(52,1).ne.0) then
          itime=(psec2-fsec1)/100
          jtime=pelap+(psec2-psec1)/100
          npr=float(msec2-msec1)/amax0(rsec2-rsec1,1)
          found=rhashes*100/max0(rnodes,1)
          pfound=rphashs*100/max0(rpcalls,1)
          kfound=rkhashs*100/max0(rpcalls*2,1)
          if(tmode .ne. 1) nps=100.0*(float(rnodes)/
     *                              amax0(msec2-msec1,1))
          if(tmode .eq. 1) nps=100.0*(float(rnodes)/
     *                              amax0(rsec2-rsec1,1))
          running=amax1(1.0,npr)
          if(ncpus.gt.1 .and. tmode.eq.0) nps=nps*running
          if(autos .gt. 1)
     *        print 600, hhmmss(itime,5), hhmmss(jtime+cbegin*60,8), 
     *                    npr, rnodes, found, pfound, kfound, nps
600       format(16x,'time:',a5,a5,1x,f4.1,'p',
     *           1x,'nodes:',i9,1x,'h',i3,'%',i3,'%',i3,'%',i7,' nps')
      endif
      return
      end
