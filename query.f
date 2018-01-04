      subroutine query
crev  last revision 09/17/91
c
c     ******************************************************************
c     *                                                                *
c     *      query is used to ask the operator about the amount of     *
c     *  time that has elapsed on the program's chess clock.  the      *
c     *  elapsed time will be asked for every 10 moves until the       *
c     *  program is within 10 moves of the time control that has been  *
c     *  set.  then the program will ask every 3 moves to make sure    *
c     *  that the time control doesn't slip up on it due to some       *
c     *  type of timing error.  for example, in a 40/2-10/30min        *
c     *  time control, the program would ask after move 10,20,30,33,   *
c     *  36,39,43,46,49, etc.                                          *
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
      equivalence (alphac,alpha(29)),(alphad,alpha(30)),
     *            (alphap,alpha(42)),(atsign,alpha(75))
c
c
c------------------------------< determine if the program should ask
c------------------------------< the operator about the time that
c------------------------------< has been used.
c
      if(cquery.ne.0 ) then
          if(timlim(5) .eq. 0) then
              nmoves=timlim(1)-npmovs
              if(nmoves .le. 10) then
                  nmoves=10-nmoves
                  if(nmoves.eq.10 .or. nmoves.eq.0) return
                  if(mod(nmoves,3) .ne. 0) return
              else
                  if(mod(npmovs,10) .ne. 0) return
              endif
          else
              if(mod(npmovs,10) .ne. 0) return
          endif
c
c------------------------------< ask every 10 moves.
c
          print 100, hhmmss(pelap+cbegin*60,8)
100       format(1x,'chess clock time:',a8//
     *    1x,'if wrong, correct with cc command.'/)
      endif
      return
      end
