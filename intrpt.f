      integer function intrpt(sigtype)
      implicit integer (a-z)
      include 'global.f'
      print 100
100   format(1x, "signal fired")
      broke=-1
      intrpt=0
      return
      end
