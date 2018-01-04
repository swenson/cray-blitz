         include   'common'       
         ident     checkg         
         ext       CHECKGU        
         entry     CHECKG%        
*
*       we use b64  herein
*
         align     
*     s1=playr=player
CHECKG%  =         *              
         a7        b60            task common base adr
         s0        s1>1           test player
         a1        mfrom,a7       
         a2        mto,a7         
         tplayr,a7 s1             tplayr=playr
         jsn       playr2         
playr1   =         *              playr=1,pstart(playr)=1
         a6        pend,a7        pend(1)=vl
         s0        mtype,a7       
         a4        plist          offset plist(pst(1))
         a5        vlist          offset vlist(pst(1))
         a3        a1+a7          mfrom+base
         s7        kloc+1,a7      kloc(2)
         a3        pboard-1,a3    pboard(mfrom)
         tside,a7  s1             tside=1
         a0        a5+a7          loc vlist(pst(1))
         vl        a6             
*l1
         v5        ,a0,1          vlist(1)
         a0        a4+a7          loc plist(pst(1))
         jsn       l1000          if not normal move
*l16
         v4        ,a0,1          plist(1)
         s4        a2             mto
         j         finsh          
playr2   =         *              playr=2,pstart(playr)=16
         a6        pend+1,a7      pend(2)=vl
         s0        mtype,a7       
         s5        -1             
         a4        16             
         a5        vlist+16       offset vlist(pst(2))
         a3        a1+a7          mfrom+base
         s7        kloc,a7        kloc(1)
         a3        pboard-1,a3    pboard(mfrom)
         tside,a7  s5             tside=-1
         a0        a5+a7          loc vlist(pst(2))
         a6        a6-a4          vl=pend(2)-16
         a5        plist+16       offset plist(pst(2))
         vl        a6             
*l1
         v5        ,a0,1          vlist(2)
         a0        a5+a7          loc plist(pst(2))
         jsn       l1000          if not normal move
*l16
         v4        ,a0,1          plist(2)
         s4        a2             mto
         a3        a3-a4          correct offset of plist
finsh    =         *              
         s5        <3             7
         a3        a3-1           vreg offset of pboard(mfrom)
         v1        s5+v5          vlist+7
         a4        100            
         a5        24             
         v2        v1<a5          position for *
         s5        10000          
         a5        s7             kloc
         s5        s5<24          position
         a5        a5*a4          (,kloc,)
         v3        s5*fv2         (,,vlist+7)
         s3        bitbd,a7       
         s5        bit0-1,a2      bit0(mto)
         s6        cbit0-1,a1     cbit0(mfrom)
         v4,a3     s4             plist(pboard(mfrom))=mto
         v0        v4+v3          (plist,,vlist+7)
         a2        at-10101       loc at(0,0,0)
         a0        a5+a2          at(,kloc,)
*        v6        ,a0,v0         equivalent in xmp/4 assy lang
         vwd       7/o'176,9/o'610 gather at(pl,kl,vl+7)
         s3        s3!s5          or(bbd,bit0(mt))
         s3        s3&s6          and(bbd,cbit0(mf))
         v7        s3&v6          and(at,bbd)
         vm        v7,z           set bit if attack
         s1        vm             will be 0 if no attack, else non-zero
         j         b0             return
l1000    =         *              
         a0        b0             
         b64       a0             
         r         CHECKGU        
         j         b64            
         include   'global'       
         end       
         ident     checki         
         ext       CHECKIU        
         entry     CHECKI%        
*
*     we use b64 herein
*
         align     
*     s1=playr=3-player
CHECKI%  =         *              
         a7        b60            task common base adr
         s0        s1>1           test 3-player
         a1        mfrom,a7       
         a2        mto,a7         
         tplayr,a7 s1             tplayr=playr
         jsn       playr2         
playr1   =         *              playr=1
         a6        pend+1,a7      pend(2)=vl
         s0        mtype,a7       
         a4        16             
         a5        vlist+16       offset vlist(pst(2))
         a1        a1+a7          mfrom+base
         s7        kloc,a7        ktemp=kloc(1)
         tside,a7  s1             tside=1
         a3        a2+a7          mto+base
         s6        board-1,a1     board(mfrom)
         a3        pboard-1,a3    pboard(mto)
         a0        a5+a7          loc vlist(pst(2))
         a6        a6-a4          vl=pend(2)-16
         a5        plist+16       offset plist(pst(2))
         vl        a6             
*l1
         v5        ,a0,1          vlist(2)
         a0        a5+a7          loc plist(pst(2))
         jsn       l1000          if not normal move
         s4        0              
*l16
         v4        ,a0,1          plist(2)
         a3        a3-a4          correct offset of plist
         a3        a3-1           vreg offset of pboard(mto)
         s5        6              
         j         finsh          
playr2   =         *              playr=2,pstart(playr)=16
         a6        pend,a7        pend(1)=vl
         s0        mtype,a7       
         s5        -1             
         a4        plist          offset plist(pst(1))
         a5        vlist          offset vlist(pst(1))
         a1        a1+a7          mfrom+base
         s7        kloc+1,a7      ktemp=kloc(2)
         a3        a2+a7          mto+base
         s6        board-1,a1     board(mfrom)
         a3        pboard-1,a3    pboard(mto)
         tside,a7  s5             tside=-1
         a0        a5+a7          loc vlist(pst(1))
         vl        a6             
*l1
         v5        ,a0,1          vlist(1)
         a0        a4+a7          loc plist(pst(1))
         jsn       l1000          if not normal move
*l16
         v4        ,a0,1          plist(1)
         s4        0              
         a3        a3-1           vreg offset of pboard(mto)
         s5        -6             
finsh    =         *              
         a4        100            
         a5        24             
         s0        s5\s6          is king moving?
         s5        <3             7
         v1        s5+v5          vlist+7
         s5        10000          
         s5        s5<24          position
         v2        v1<a5          position for *
         a5        s7             ktemp
         jsn       skip1          jump if not king
         a5        a2             reset ktemp
skip1    =         *
         v3        s5*fv2         (,,vlist+7)
         a5        a5*a4          (,kloc,)
         s3        bitbd,a7       
         a1        a1-a7          mfrom
         s5        bit0-1,a2      bit0(mto)
         s6        cbit0-1,a1     cbit0(mfrom)
         v4,a3     s4             plist(pboard(mto))=0
         v0        v4+v3          (plist,,vlist+7)
         a2        at-10101       loc at(0,0,0)
         a0        a5+a2          at(,kloc,)
*        v6        ,a0,v0         equivalent in xmp/4 assy lang
         vwd       7/o'176,9/o'610 gather at(pl,kl,vl+7)
         s3        s3!s5          or(bbd,bit0(mt))
         s3        s3&s6          and(bbd,cbit0(mf))
         v7        s3&v6          and(at,bbd)
         vm        v7,z           set bit if attack
         s1        vm             will be 0 if no attack, else non-zero
         j         b0             return
         s1        s1             spacer
         s1        s1             spacer
l1000    =         *              
         a0        b0             
         b64       a0             
         r         CHECKIU        
         j         b64            
         include   'global'       
         end       
