      integer function scan(column)
crev  last revision 08/21/90
c
c     ******************************************************************
c     *                                                                *
c     *      scan is used to decode numeric input in free format       *
c     *  from the terminal/card reader.  rather than using a specific  *
c     *  'i' type format, numeric input is read in alphabetically      *
c     *  and decoded as needed to make input simpler and more error    *
c     *  free.  an alternate form of input is mm:ss, where mm:ss       *
c     *  represents the time in minutes and seconds.                   *
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
      integer digits(10)
      equivalence (digits(1),alpha(53)),(minus,alpha(64)),
     *(plus,alpha(63)),(blank,alpha(70)),(comma,alpha(72)),
     *(colon,alpha(71))
c
c
c------------------------------< initialize.
c
      scan=0
      hrmin=0
      sign=1
c
c------------------------------< process each of 80 columns.
c
      do 500 i=column,80
c
c------------------------------< blank indicates the end of the number.
c
          if(atext(i) .eq. blank) go to 600
          if(atext(i) .eq. comma) go to 600
c
c------------------------------< match the digit with 0-9.
c
          do 100 j=1,10
              if(atext(i) .eq. digits(j)) then
                  scan=scan*10+j-1
                  go to 500
              endif
100       continue
c
c------------------------------< it may be + or - also.
c
          if(atext(i) .eq. plus) sign=1
          if(atext(i) .eq. minus) sign=-1
          if(atext(i).ne.plus .and. atext(i).ne.minus) then
c
c------------------------------< if a ':' is in the string, everything
c------------------------------< before it should be multiplied by 60
c------------------------------< to convert to seconds.
c
              if(atext(i) .eq. colon) then
                  hrmin=hrmin*60+scan
                  scan=0
                  go to 500
              endif
c
c------------------------------< not 0-9, + or -. it's illegal input
c
              print 300, atext(i)
300           format(1x,'illegal numeric input - (',a1,') try again')
              go to 600
          endif
500   continue
c
c------------------------------< include sign.
c
600   continue
          scan=(scan+hrmin*60)*sign
          column=i
          return
      end
