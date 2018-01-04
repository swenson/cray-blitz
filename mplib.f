       subroutine memory(par,number)
       implicit integer (a-z)
       print 100, number
100    format(1x,'memory request for ',i8,' words.')
c      i=sbreak(number)
       if(i .ge. 0) return
       print 200, i
200    format(1x,'non-zero return code = ',i3)
       return
       end
      subroutine mpcopy
      implicit integer (a-z)
      common /pdata/ pdata(8100)
      common /task/ tdata(8100)
      do 100 i=1,8100
          tdata(i)=pdata(i)
100   continue
      return
      end
      integer function tst124(i)
      tst124=8
      return
      end
