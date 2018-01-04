      subroutine asyncio
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *   asyncio is used to handle terminal input on a cray xmp with  *
c     *  unicos.  it runs as a separate task and uses a "c" routine    *
c     *  to do the actual i/o.  this allows reads to be done at the    *
c     *  same time as fortran writes.  the fortran routine command     *
c     *  waits on the "full" event and then copies the data from       *
c     *  buffer.  this routine simply loops reading, posting and then  *
c     *  starting over again.  if doasync=0, then this routine will    *
c     *  exit after the read completes.                                *
c     *                                                                *
c     ******************************************************************
      implicit integer (a-z)
c
c
      include 'global.f'
c
c
c
c
100   continue
          read 101, buffer
101       format(a128)
c         call evpost(bfull)
          broke=-1
          if(aio .gt. 0) go to 100
      stop
      end
