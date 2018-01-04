      subroutine cptime(rsecs,msecs)
crev last revision 04/26/89
c     ******************************************************************
c     *                                                                *
c     *      cptime is used to monitor time utilization.  the real or  *
c     *  wall clock time is returned in variable 'rsecs' and the       *
c     *  computer or processor time is returned in variable 'msecs'.   *
c     *      all times are in 1/100 seconds and are cumulative for the *
c     *  current run; to time a process, call time before and after    *
c     *  the process and compute the difference to get the time spent  *
c     *  in processing.                                                *
c     *                                                                *
c     ******************************************************************
      implicit integer (a-z)
 
      real xsecs
      save orsecs
      data orsecs / 0 /
c
c------------------------------< get real and cpu time from cos
c
      common /maxtime/ maxtime
      call cpu_time(xsecs)
      rsecs=xsecs*100
      msecs=rsecs
      if(msecs .ge. maxtime) maxtime=msecs
      msecs=max(msecs,maxtime)
c
c------------------------------< take care of the time when we play
c------------------------------< a game that run past midnite and
c------------------------------< rolls the elapsed clock over to
c------------------------------< 00:00. this will play heck with
c------------------------------< timing as a move could take -23:58
c------------------------------< if it started at 23:59 and finished
c------------------------------< 00:01^
c
      if(rsecs+100000 .lt. orsecs) rsecs=rsecs+8640000
      orsecs=rsecs
      return
      end
