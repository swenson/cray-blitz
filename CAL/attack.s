         include   'common'       
         ident     attack         
         entry     ATTACK%        
         align     
*     s1=side,s2=square
ATTACK%  =         *              
         a7        b60            task common base adr
         a6        pend,a7        pend(1)=vl
         a3        pend+1,a7      pend(2)=vl
         s0        s1             
         a4        16             for player2
         a5        vlist+16       offset vlist for player2
         jsm       playr2         
playr1   =         *              playr=1,pstart(playr)=1
         a4        plist          offset plist(pst(1))
         a5        vlist          offset vlist(pst(1))
         a0        a5+a7          loc vlist(pst(1))
         vl        a6             
*l1
         v5        ,a0,1          vlist(1)
         a0        a4+a7          loc plist(pst(1))
*l16
         v4        ,a0,1          plist(1)
         s5        <3             7
         v1        s5+v5          vlist+7
         a4        100            
         a5        24             
         v2        v1<a5          position for *
         s5        10000          
         a5        s2             square
         s5        s5<24          position
         a5        a5*a4          (0,kloc,0)
         v3        s5*fv2         (0,0,vlist+7)
         s3        bitbd,a7       
         a2        at             loc at(1,1,1)
         a3        10101          
         a2        a2-a3          loc at(0,0,0)
         v0        v4+v3          (plist,0,vlist+7)
         a0        a5+a2          at(0,kloc,0)
*        v6        ,a0,v0         equivalent in xmp/4 assy lang
         vwd       7/o'176,9/o'610 gather at(pl,kl,vl+7)
finsh    =         *              
         v7        s3&v6          and(at,bbd)
         vm        v7,z           set bit if attack
         s1        0              
         s0        vm             will be 0 if no attack, else non-zero
         jsz       done1
         s1        -1             -1
done1    =         *
         j         b0             return
playr2   =         *              playr=2,pstart(playr)=16
         a0        a5+a7          loc vlist(pst(2))
         a6        plist+16       offset plist(pst(2))
         a3        a3-a4          vl=pend(2)-16
         vl        a3             
*l1
         v5        ,a0,1          vlist(2)
         a0        a6+a7          loc plist(pst(2))
*l16
         v4        ,a0,1          plist(2)
         s5        <3             7
         v1        s5+v5          vlist+7
         a4        100            
         a5        24             
         v2        v1<a5          position for *
         s5        10000          
         a5        s2             square
         s5        s5<24          position
         a5        a5*a4          (,kloc,)
         v3        s5*fv2         (,,vlist+7)
         s3        bitbd,a7       
         a2        at             loc at(1,1,1)
         a3        10101          
         a2        a2-a3          loc at(0,0,0)
         v0        v4+v3          (plist,,vlist+7)
         a0        a5+a2          at(,kloc,)
*        v6        ,a0,v0         equivalent in xmp/4 assy lang
         vwd       7/o'176,9/o'610 gather at(pl,kl,vl+7)
         v7        s3&v6          and(at,bbd)
         vm        v7,z           set bit if attack
         s1        0              
         s0        vm             will be 0 if no attack, else non-zero
         jsz       done2         
         s1        -1             -1
done2    =         *
         j         b0             return
         include   'global'       
         end       
