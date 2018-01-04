         include   'common'       
         ident     unmake         
         entry     UNMAKE         
         ext       UNMAKE2        
         ext       UNMAKE3        
         
*         this routine handles unmake of ordinary moves and
*         calls fortran routines to handle unusual situations.
         
         align     
UNMAKE   =         *              
         a7        b60            task common base address
         a1        player,a7      
         a4        mfrom,a7       
         a2        ply,a7         
         a3        mto,a7         
         a0        b0             
         s0        mtype,a7       
         s6        bitbd,a7       
         a1        a1+a7          
         b64       a0             save return address
         s4        bit0-1,a4      bit0(mf)
         a2        a2+a7          
         s5        cbit0-1,a3     cbit0(mt)
         a6        msave9-1,a2    
         s7        cappb-1,a2     cappb(ply)
         a0        movedr-1,a1    
         s1        msave7-1,a2    
         s2        msave8-1,a2    
         s3        msave11-1,a2   
         a3        a3+a7          
         s6        s6!s4          or(bitbd,bit0(mfrom))
         s4        msave10-1,a2   
         s5        s6&s5          and( ,cbit0(mto))
         a5        pboard-1,a3    pboard(mfrom)
         nppcs,a7  a6             
         a6        cappt-1,a2     pboard(mto)
         s6        s5!s7          or( ,cappb(ply))
         s7        board-1,a3     board(mfrom)
         s5        cappc-1,a2     board(mto)
         hash,a7   s1             
         phash,a7  s2             
         mscore,a7 s3             
         nopcs,a7  s4             
         jan       l40            
l60      =         *              
         jsn       l1200          
         bitbd,a7  s6             
         a0        a3-a7          
         s0        a5             
         a5        a5+a7          
         pboard-1,a3 a6           
         plist-1,a5 a4            
         a4        a4+a7          
         a6        a6+a7          
         pboard-1,a4 s0           
         plist-1,a6 a0            
         s6        1              
         board-1,a3 s5            
         board-1,a4 s7            
         vlist-1,a5 s7            
         s0        s7\s6          bd(mf)=1?
         vlist-1,a6 s5            
         jsz       l1010          
         s0        #s7            bd(mf)=-1?
         a5        6              
         jsz       l1010          
         a2        s7             also needed in a-reg
         a0        a2-a5          bd(mf)=6?
         s0        s5             cappc(ply)
         jaz       l1050          
         a0        a2+a5          bd(mf)=-6?
         jaz       l1050          
l1100    =         *              
         jsz       retn           
         r         UNMAKE3        
retn     =         *              
         j         b64            
l40      =         *              
         s3        moveds-3,a2    moveds(ply-2)
         moveds-1,a2 s3           changed apr 84
         j         l60            
l1050    =         *              
         a4        a4-a7          
         s1        cranks-1,a4    
         s2        cfiles-1,a4    
         kloc-1,a1 a4             
         krank-1,a1 s1            
         kfile-1,a1 s2            
         j         l1100          
l1200    =         *              
         r         UNMAKE2        
         j         b64            
l1010    =         *              this is unmake1
*     a2=ply,a3=mto,a4=mfrom
         a3        a3-a7          
         a4        a4-a7          
         a1        cfiles-1,a3    file
         a5        cfiles-1,a4    ffile
         s6        cranks-1,a3    
         s4        cranks-1,a4    frank
         s7        arank,a7       
         a4        afile,a7       
         s0        side,a7        
         a0        a1-a5          cf(mt)-ff
         s1        msave1-1,a2    
         s6        s6-s4          
         a3        a1-a5          
         s2        msave2-1,a2    
         a4        a4-a3          
         s7        s7-s6          
         afile,a7  a4             
         arank,a7  s7             
*     a5=ffile,a1=file
         a1        a1+a7          
         a5        a5+a7          
         jsm       l1030          
         s0        s5             cappc(ply)
         jan       l1020          
         pfirst-1,a5 s1           
         plast-1,a5 s2            
         j         b0             
l1030    =         *              
         jan       l1040          
         ofirst-1,a5 s1           
         olast-1,a5 s2            
         j         b0             
         bss       5              startbuf
l1020    =         *              
         s3        msave3-1,a2    
         s4        msave4-1,a2    
         a4        pcount-1,a1    
         a6        pcount-1,a5    
         pfirst-1,a1 s1           
         plast-1,a1 s2            
         pfirst-1,a5 s3           
         plast-1,a5 s4            
         a4        a4-1           
         a6        a6+1           
         pcount-1,a1 a4           
         pcount-1,a5 a6           
         j         l1100          
l1040    =         *              
         s3        msave3-1,a2    
         s4        msave4-1,a2    
         a4        ocount-1,a1    
         a6        ocount-1,a5    
         ofirst-1,a1 s1           
         olast-1,a1 s2            
         ofirst-1,a5 s3           
         olast-1,a5 s4            
         a4        a4-1           
         a6        a6+1           
         ocount-1,a1 a4           
         ocount-1,a5 a6           
         s0        s5             cappc(ply)
         j         l1100          
         include   'global'       
         end       
