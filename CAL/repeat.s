         include   'common'       
         ident     repeat         
         entry     REPEAT%        
*    here begins vfunction repeat
*    it saves the current board position and returns 0 if there are
*    no matches with earlier positions, otherwise returns non-zero.
*    it is inline with no traceback and only a dummy argument.
*     it may be called by: if(repeat(0).eq..false.) . . .
         
         align     
REPEAT%  =         *              
         a1        b60            task common base address
         s2        point,a1       
         s4        ply,a1         
         s3        hash,a1        
         s7        <2             3
         a2        -2             incr
         a6        bdsave-2       base adr for load
         s5        s2-s7          point-3
         a7        0              in case of early return
         s6        s5+s4          temp-3
         a6        a6+a1          actual load adr base
         a5        s6             temp-3
         s6        s6>1           length = (temp-3)/2
         a3        a5+a2          if not + nothing to do
         s1        0              return false if no repeats
         s0        +a3            check if anything to do
         a4        s6             to set vl
         vl        a4             
         a0        a5+a6          initial adr for reverse load
         a5        a5+a1          actual adr for store
         jsm       ret1           just xit if nothing to check
         v0        ,a0,a2         load bdsaves
         v1        s3-v0          check for matches
         vm        v1,z           set bit if match
         bdsave+2,a5 s3           store away new board in bdsave(temp)
         s1        vm             get match bits or zero
         j         b0             
ret1     =         *              
         bdsave+2,a5 s3           store away new board in bdsave(temp)
         j         b0             
         include   'global'       
         end       
