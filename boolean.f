c     boolean operations
      integer function csmgx(x,y,z)
      implicit integer (a-z)
      csmgx=or(and(x,z),and(y,xor(z,-1)))
      return
      end
