         include   'common'       
         ident     select         
         entry     SELECT         
         ext       PHASE0         
         ext       PHASE1         
         ext       PHASE2         
         ext       PHASE3         
         ext       PHASE4         
         ext       PHASE5         
         ext       MATER          
         ext       MOVGEN         
         ext       SELECT1        
         
*       herein we have b60=base, b57=base+ply, so ply=b57-b60
*       also we save loc(ply,taskid) in b54, return addr in b61
         
         align     
SELECT   =         *              
         a7        b60            task common base address
         a6        ply,a7         
         a3        taskid,a7      
         a2        50             dimension for taskid
         s3        <1             
         s2        nodes,a7       for l140
         s5        xnodes,a7      for l140
         s3        s3<2           4
         a0        b0             
         a1        a6+a7          ply+base
         b61       a0             for return
         s0        which-1,a1     which(ply)
         a2        a3*a2          (0,taskid)
         s4        phase-1,a1     phase(ply)
         b57       a1             sav ply+base
         a0        a6-1           is ply = 1?
         a2        a2+a6          (ply,taskid)
         s7        s2-s5          nodes-xnodes
         b54       a2             sav offset of (ply,taskid)
         jaz       l1200          if ply=1
         a0        s7             is nodes.ge.xnodes (for l140)
         s6        status-1,a1    for l1000 & l800 & l300 & l200
         jsz       l140           if which(ply)=0
         s0        s4\s3          is phase=4?
         s3        <2             for l130
         s1        1              for l130 & l250
         jsn       l130           
         r         PHASE4         
         a7        b60            
         s0        return,a7      
         a1        b57            
         a2        which-1,a1     which(ply) for l9999
         jsz       l9999          
         j         l9998          
l130     =         *              s4=phase(ply),a6=ply,s1=1,a1=ply+base,s3=3,s6=status(ply)
         s0        s3\s4          is phase=3?
         a3        s4             phase(ply)
         a5        2              
         a0        a5-a3          is phase=2?
         jsz       l1000          phase=3
         s0        s4\s1          is phase=1?
         jaz       l800           phase=2
         a0        s4             is phase=0?
         jsz       l300           phase=1
         jaz       l200           phase=0
*  get here if phase=9
         j         l9998          
l140     =         *              a0=nodes-xnodes
         jam       l200           dont check time
         r         SELECT1        check time
         a7        b60            
         s0        return,a7      
         a1        b57            ply+base
         s4        phase-1,a1     phase(ply)  for l200
         s6        status-1,a1    status(ply)  for l200
         s7        0              
         a2        b54            (ply,taskid)
         a5        2              
         a6        a1-a7          ply   for l200
         jsz       l200           
         curmvs-51,a2 s7          curmvs(ply,taskid)=0
         return,a7 a5             return=2
         j         b61            
l200     =         *              a6=ply,a1=ply+base,s6=status(ply),s4=phase(ply)
         a5        xnullm,        
         a2        last-2,a1      last(ply-1)
         a4        rdepth,a7      
         a0        s4             phase(ply)
         s0        a5             null move = 0?
         jan       l300           phase.ne.0
         jsz       l300           xnullm.eq.0
         s0        s6             status(ply)
         a3        a2+1           last(ply-1)+1
         a4        a4-a5          rdepth-xnullm
         a0        a4-a6          is ply.le. rdepth-xnullm
         jsn       l250           status(ply).ne.0
         jam       l250           ply.gt.(rdepth-xnullm)
         first-1,a1 a3            
         last-1,a1 a3             
         r         PHASE0         
         a7        b60            
         s0        return,a7      
         a4        rdepth,a7      for l400
         a1        b57            
         a2        which-1,a1     for l9999
         a6        a1-a7          
         jsz       l9999          
l250     =         *              
         s6        0              status(ply)=0
         s1        1              
         phase-1,a1 s1            phase(ply)=1
         status-1,a1 s6           status(ply)=0
l300     =         *              a1=ply+base,a6=ply,s6=status(ply)
         s0        s6             is status=0?
         a4        rdepth,a7      for l400
         a5        last-2,a1      last(ply-1)
         jsn       l400           normally we do branch
         a0        hmove-1,a1     hmove(ply)
         a3        a5+1           last(ply-1)+1
         first-1,a1 a3            
         last-1,a1 a3             
         jaz       l400           hmove(ply)=0
         r         PHASE1         
         a7        b60            
         s0        return,a7      
         a4        rdepth,a7      for l400
         a1        b57            
         a2        which-1,a1     for l9999
         a6        a1-a7          
         s7        0              
         jsz       l9999          if return=0
         hmove-1,a1 s7            hmove(ply) = 0
l400     =         *              a1=ply+base,a6=ply,a4=rdepth
         s0        inchk-1,a1     
         a0        a4-a6          rdepth-ply
         jap       l410           if ply.le.rdepth
         jsn       l410           if inchk=true
         r         MATER          
l410     =         *              
         r         MOVGEN         
         a7        b60            
         s0        return,a7      
         a1        b57            
         a2        last-1,a1      
         a3        first-1,a1     
         a6        a1-a7          ply
         s6        0              
         jsn       l9997          if return from movgen nonzero
         s0        hmove-1,a1     
         a5        moves-1        offset moves(0)
         mated-2,a1 s6            mated(ply-1)=0
         s3        hmove-1,a1     hmove(ply)
         a0        a2-a3          any work?
         a4        a2-a3          how much?
         a4        a4+1           
         jam       l9998          no work
         a5        a5+a7          .loc.moves(0)
         jsz       l600           no need
         a0        a5+a3          .loc.moves(first(ply))
         vl        a4             
         v0        ,a0,1          moves(i)
         v1        s3-v0          hmove(ply)-moves(i)
         vm        v1,z           bit is on if hmove=moves(i)
         s2        0              
         s0        vm             
         s1        vm             
         jsz       l600           quit if more than 64 moves and no hits
         a4        a5+a3          .loc.moves(first(ply))
         a2        zs1            where was hit
         a2        a4+a2          hit+start addr
         0,a2      s6             moves(i)=0
l600     =         *              s6=0,a1=ply+base
         a3        2              
         s1        1              
         status-1,a1 s6           status(ply)=0
         phase-1,a1 a3            phase(ply)=2
l800     =         *              a6=ply,s1=1,s6=status(ply),a1=ply+base
         a0        s6             
         jan       l850           skip phase2
         r         PHASE2         
         a1        b57            
         s6        status-1,a1    
         a7        b60            
         a6        a1-a7          ply
l850     =         *              a6=ply,s6=status(ply),a1=ply+base
         a5        which-1,a1     
         a3        last-1,a1      
         a4        rdepth,a7      for l900
l855     =         *              
         s1        1              
         s0        s1-s6          test (old)status(ply)
         s6        s6-s1          status(ply)-1
         status-1,a1 s6           (new)status(ply)
         jsp       l900           (old)status.le.1 = (new)status.le.0
         a5        a5+1           which(ply)+1
         a0        a3-a5          last(ply)-which(ply)
         a2        a5+a7          (which(ply))
         s1        moves-1,a2     m(w(p))
         which-1,a1 a5            put away which(ply)
         jam       l900           if which(ply).gt.last(ply)
*    inline    extrct%
         s2        <3             7
         a0        s1             is m(w(p)).eq.0
         s7        s1             temp
         s0        s1>20          movgpc
         s7        s7>14          position mtype
         jaz       l855           loop back to look for non-zero move
         movgpc,a7 s0             
         s7        s7&s2          mtype
         s6        >63            -2
         s6        s7+s6          mpropc?
         s4        <2             3
         s0        s4-s7          is mtype .gt. 3
         s3        s1             
         s3        s3>17          position mcappc
         mtype,a7  s7             
         s3        s3&s2          
         s5        s1             temp
         s5        s5>7           position mto
         jsm       *+3            is mpropc ok
         s6        0              no mpropc
         s2        <7             127
         s5        s5&s2          mto
         s1        s1&s2          mfrom
         mpropc,a7 s6             
         mcappc,a7 s3             
         mto,a7    s5             
         s7        0              
         a2        which-1,a1     which(ply)
         return,a7 s7             
         mfrom,a7  s1             
         j         l9999          
l900     =         *              a1=ply+base,a6=ply,a4=rdepth(ply)
         s0        inchk-1,a1     
         s5        givchk-1,a1    
         s4        a6             ply
         a3        3              phase=3
         s4        s4<16          shiftl(ply,16)
         a0        a4-a6          rdepth-ply
         phase-1,a1 a3            
         status-1,a1 s4           
         jap       l1000          ply.le.rd
         a0        s5             
         jsn       l1000          not inchk
         jan       l1100          not givchk
         j         l9998          
l1000    =         *              
         r         PHASE3         
         a7        b60            
         s0        return,a7      
         a1        b57            
         a2        which-1,a1     which(ply)
         jsz       l9999          
l1100    =         *              a1=ply+base
         a4        4              
         s7        0              
         phase-1,a1 a4            
         status-1,a1 s7           
         r         PHASE4         
         a7        b60            
         s0        return,a7      
         a1        b57            
         a2        which-1,a1     which(ply)
         jsz       l9999          
l9998    =         *              a7=b60
         a1        b57            ply+base
         a4        b54            (ply,taskid)
         a5        9              
         phase-1,a1 a5            phase(ply)=9
         s2        0              
         curmvs-51,a4 s2          curmvs(ply,taskid)=0
         s1        1              
         return,a7 s1             
         j         b61            
l9997    =         *              a7=b60
         a2        b54            (ply,taskid)
         s2        0              
         curmvs-51,a2 s2          curmvs(ply,taskid)=0
         s1        <2             3
         return,a7 s1             
         j         b61            
l9999    =         *              a1=ply+base,a2=which(ply),a7=base
         a3        a2+a7          
         s1        moves-1,a3     moves(which(ply))
         a1        b54            (ply,taskid)
         s7        0              
         curmvs-51,a1 s1          curmvs(ply,taskid)
         moves-1,a3 s7            
         j         b61            
l1200    =         *              
         r         PHASE5         
         a7        b60            
         s0        return,a7      
         a1        b57            
         a2        which-1,a1     which(ply)
         jsn       l9998          
l9996    =         *              a7=base
         a3        a2+a7          (which(ply))
         s2        moves-1,a3     moves(which(ply))
         a2        b54            (ply,taskid)
         curmvs-51,a2 s2          curmvs(ply,taskid)=0
         j         b61            
         include   'global'       
         end       
