         include   'common'       
         ident     make           
         entry     MAKE           
         ext       MAKE1          
         ext       MAKE2          
         ext       MAKE3          
         align     
MAKE     =         *              
         a7        b60            task common base address
         a3        ply,a7         
         s1        hash,a7        
         s2        phash,a7       
         s3        nppcs,a7       
         s4        nopcs,a7       
         s0        mtype,a7       
         s6        mscore,a7      
         a1        mto,a7         
         a2        mfrom,a7       
         s5        bitbd,a7       
         a3        a3+a7          
         a4        player,a7      
         msave7-1,a3 s1           
         msave8-1,a3 s2           
         msave9-1,a3 s3           
         msave10-1,a3 s4          
         msave11-1,a3 s6          
         a0        33             
         jsn       l2000          
         s6        bit0-1,a1      bit0(mto)
         s7        cbit0-1,a2     cbit0(mf)
         a1        a1+a7          
         a2        a2+a7          
         a5        pboard-1,a1    pbd(mt)
         a6        pboard-1,a2    pbd(mf)
         s3        board-1,a1     bd(mt)
         s4        board-1,a2     bd(mf)
         pboard-1,a2 a0           pbd(mf)=33
         board-1,a2 s0            bd(mf)=0
         cappt-1,a3 a5            cpt(ply)=pbd(mt)
         pboard-1,a1 a6           pbd(mt)=pbd(mf)
         s2        s6&s5          and(bitbd,bit0(mto))
         s6        s6!s5          or(bitbd,bit0(mto))
         s7        s6&s7          and( ,cbit0(mfrom))
         cappb-1,a3 s2            cappb(ply)=
         bitbd,a7  s7             bitbd=
         cappc-1,a3 s3            cpc(ply)=bd(mt)
         board-1,a1 s4            bd(mt)=bd(mf)
         a1        a1-a7          
         a6        a6+a7          
         a5        a5+a7          
         a2        a2-a7          
         plist-1,a6 a1            pls(pbd(mt))=mt
         plist-1,a5 s0            pls(cpt(ply))=0
         vlist-1,a6 s4            vls(pbd(mt))=bd(mt)
         vlist-1,a5 s0            vls(cpt(ply))=0
*    a1=mto,a2=mfrom,a3=ply,s1=hash,s3=cappc(ply),s4=board(mto)
         a6        s4             bd(mt)
         a5        s3             cpc(ply)
         a7        100            
         a5        a5*a7          (0,cpc(ply)
         a7        a6*a7          (0,bd(mt))
         b42       a2             sav mfrom
         a2        a2+a7          (mf,bd(mt))
         a5        a1+a5          (mt,cpc(ply))
         s7        random+599,a2  ran(mf,bd(mt)+7)
         a2        a1+a7          (mt,bd(mt))
         s5        random+599,a5  ran(mt,cpc(ply)+7)
         s6        random+599,a2  ran(mt,bd(mt)+7)
         a0        b0             
         b64       a0             sav return address
         s0        #s4            bd(mt)=-1?
         a0        a6-1           bd(mt)=+1?
         a7        b60            
         s3        mcappc,a7      
         a2        6              
         a4        a4+a7          
         s5        s7\s5          mf.xor.cpc
         s2        movedr-1,a4    pmv(pla)
         s1        s5\s1          .xor.hash
         s1        s1\s6          .xor.mt
         hash,a7   s1             
         jsz       l1080          
         jaz       l1080          
         a0        a6+a2          bd(mt)=-6?
         s6        a2             6
         s0        s4\s6          bd(mt)=+6?
         jaz       l1085          
         jsz       l1085          
         s0        s2             pmv(pla)=0?
         a0        s3             mcappc=0?
         a4        a4-a7          
         jsn       l1090          
l1100    =         *              
         jaz       retn           
         r         MAKE3          
retn     =         *              
         j         b64            
l1080    =         *              
         r         MAKE1          
         j         b64            
         bss       0              startbuf
l2000    =         *              
         a0        b0             
         b64       a0             sav return address
         r         MAKE2          
         j         b64            
l1085    =         *              
         s5        cranks-1,a1    
         s4        cfiles-1,a1    
         a0        s3             mcappc
         s0        s2             
         a7        0              
         moveds-1,a3 a7           mvd(ply)
         kloc-1,a4 a1             kloc(pla)
         krank-1,a4 s5            
         kfile-1,a4 s4            
         j         l1100          
l1090    =         *              
         a6        cbias-1,a4     bias
         s6        moveds-1,a3    mvd(ply)
         s5        1              
         s7        s5+s5          
         a7        22             
         a4        29             
         a2        b42            
         a5        a2-a7          mfrom-22
         a0        a5-a6          -bias=?
         a5        a1-a7          mt0-22
         jan       skip1          
         s6        s6&s7          moveds(ply)=and(moved(ply),2)
skip1    =         *
         a0        a5-a6          
         a5        a2-a4          
         jan       skip2          
         s6        s6&s7          
skip2    =         *
         a0        a5-a6          
         a5        a1-a4          mto-29
         jan       skip3          
         s6        s6&s5          
skip3    =         *
         a0        a5-a6          
         jan       skip4          
         s6        s6&s5          
skip4    =         *
         moveds-1,a3 s6           moveds(ply)=
         a0        s3             
         j         l1100          
         include   'global'       
         end       
