         include   'common' 
         ident     search         
         ext       SEARCHMP       
         ext       STATS          
         ext       BACKUP         
         ext       REPEAT%        
         ext       ATTACK%        
         ext       MAKE           
         ext       UNMAKE         
         ext       KILLER1        
         ext       KILLER2        
         ext       STORE1         
         ext       STORE2         
         ext       MERGE          
         ext       SELECT         
         ext       SELGET         
         ext       SELPUT         
         ext       SPLIT          
         ext       UNSPLIT        
         ext       LOOKUP         
         ext       SEARCHE        
         ext       SEARCHO        
         ext       SHARE          
         entry     SEARCH         
*
*       thruout b51=taskid, b52=ply, b53=splitl(taskid)
*
SEARCH   =         *              
         a6        b00            
         b50       a6             save rtnadr
         a7        b60            
         a1        ply,a7         
         a2        taskid,a7      
         s1        depth,a7       
         s3        window,        window(1)
         s4        window+1,      window(2) 
         s5        dnodes,a7      
         s2        0              
         s6        1              
         hashes,a7 s2             
         phashs,a7 s2             
         khashs,a7 s2             
         pcalls,a7 s2             
         nodes,a7  s2             
         cnodes,a7 s2             
         value-2,a7 s3            value(-1)=window(1)
         value-1,a7 s4            value(0)=window(2)
         xnodes,a7 s5             xnodes=dnodes
         s0        +a1            ply
         b51       a2             save taskid
         b52       a1             save ply
         a1        splitl-1,a2    splitl(taskid)
         b53       a1             save in b53
         jsn       l3000          if(ply .gt. 0)
         rdepth,a7 s1             rdepth=depth
         status,a7 s6             status(1)=1
l100     =         *              
         a7        b60            
         a1        b52            ply
         a2        b51            taskid
         s1        rdepth,a7      
         s2        side,a7        
         s3        player,a7      
         a0        abort,         
         a5        timeup,
         a3        maxply,        
         s4        <2             3
         a4        50             dimension for curmvs
         a1        a1+1           up ply by 1
         s7        0              
         a6        a1+a7          ply + base
         a4        a4*a2          (o,taskid)
         s0        stop-1,a2      stop(taskid)
         s5        moveds-3,a6    moveds(ply-2)
         s6        value-3,a6     value(ply-2)
         a4        a4+a1          (ply,taskid)
         s3        s4-s3          3-player
         s2        -s2            -side
         ply,a7    a1             ply=ply+1
         b52       a1             update ply
         odepth-1,a6 s1           odepth(ply)=rdepth
         curmvs-51,a4 s7          curmvs(ply,taskid)=0
         cutmvs-1,a6 s7           cutmvs(ply)=0
         side,a7   s2             side=-side
         player,a7 s3             player=3-player
         moveds-1,a6 s5           moveds(ply)=moveds(ply-2)
         value-1,a6 s6            value(ply)=value(ply-2)
         jan       l7000          if abort .ne. 0
         a0        a5             timeup
         jan       l7000          if timeup .ne. 0
         a0        a1-a3          ply-maxply
         jsn       l7100          if stop(taskid) .ne. 0
         inchk-1,a6 s7            inchk(ply)=0
         givchk-1,a6 s7           givchk(ply)=0
         jap       l5200          if ply .ge. maxply
         which-1,a6 s7            which(ply)=0
         which,a6  s7             which(ply+1)=0
         phase-1,a6 s7            phase(ply)=0
         a0        a1-1           ply-1
         status-1,a6 s7           status(ply)=0
         a6        s2             side (for t2900)
         jaz       t2900          if(ply .eq.1)
         r         LOOKUP         
         a7        b60            
         s1        return,a7      
         a6        side,a7        for l2900
         s2        1              
         a2        2              
         s0        s1\s2          return.eq.1?
         a5        s1             
         a0        a5-a2          return-2
         jsz       l5200          if(return .eq. 1)
         jaz       l4300          if(return .eq. 2)
t2900    =         *              a6=side
         a0        a6+1           side+1
         jaz       t2950          if(side .eq. -1)
         r         SEARCHO        
         j         t2980          
t2950    =         *              
         r         SEARCHE        
t2980    =         *              
         a7        b60            
         a0        return,a7      
         jan       l4000          if(return .ne. 0)
l3000    =         *              
         a7        b60            
         a6        player,a7      
         s0        broke,         
         a0        a6-1           player-1
         jan       t3050          if(player .ne. 1)
         jsm       r3050          if(broke .eq. -1)
t3050    =         *              
         a6        b51            taskid
         s0        shareit-1,a6   
         a0        usplit-1,a6    for t3080
         jsn       r3080          if(shareit(taskid) .ne. 0)
t3080    =         *              a0=usplit(taskid), a7=base
         jan       r3090          if(usplit(taskid) .ne. 0)
t3090    =         *              
         a6        b51            taskid
         a0        help-1,a6      
         s0        stop-1,a6      for t3100
         a4        b53            for t3100
         jan       r3100          if(help(taskid) .ne. 0)
t3100    =         *              a4=splitl(taskid), a6=taskid, a7=base, s0=stop(taskid)
         a5        b52            ply
         a0        a5-a4          ply-splitl
         jsn       l7100          if(stop(taskid) .ne. 0)
         jaz       r3210          if(ply .eq. splitl(taskid))
         r         SELECT         
         a7        b60            
         a4        return,a7      
t3210    =         *              a4=return, a7=base
         a1        tflag,         
         s1        mto,a7         
         s2        mfrom,a7       
         s3        mtype,a7       
         s4        nodes,a7       use s for nodes in case exceeds 2**31
         a5        b52            ply
         a6        a5+a7          ply+base
         s0        a4             return
         s5        1              
         jsn       r3220          if(return .ne. 0)
         to-1,a6   s1             to(ply)=mto
         from-1,a6 s2             from(ply)=mfrom
         type-1,a6 s3             type(ply)=mtype
         s4        s4+s5          nodes+1
         a0        a1-a5          tflag-ply
         nodes,a7  s4             nodes=nodes+1
         jap       r3330          if(ply .le. tflag)
t3330    =         *              
         r         MAKE           
         r         REPEAT%        
         s0        s1             
         a7        b60            
         a6        b51            taskid
         a5        b52            ply
         jsz       l100           if(repeat() .eq. 0)
         a1        2              ...
         count,a7  a1             count=2
         s5        rdepth,a7      ...
         s3        player,a7      ...
         a4        a5+a7          ply+base
         s1        side,a7        ...
         s6        value-3,a4     value(ply-2)
         a3        0              ...
         mated-1,a4 a3            mated(ply)=0
         a5        a5+1           up ply by 1
         a4        a4+1           up ply+base by 1
         ply,a7    a5             ply=ply+1
         b52       a5             update ply
         a2        50             curmvs 1st dimension
         a1        a6*a2          (0,taskid)
         odepth-1,a4 s5           odepth(ply)=rdepth
         cutmvs-1,a4 a3           cutmvs(ply)=0
         a1        a1+a5          (ply,taskid)
         a5        count,a7       for l6100
         s4        3              ...
         curmvs-51,a1 a3          curmvs(ply,taskid)=0
         s4        s4-s3          3-player
         s1        -s1            -side
         value-1,a4 s6            value(ply)=value(ply-2)
         player,a7 s4             player=3-player
         side,a7   s1             side=-side
         j         l6100          
l4000    =         *              a7=base
         a6        b52            ply
         a4        a6+a7          ply+base
         a5        0              
         mated-2,a4 a5            mated(ply-1)=0
l4100    =         *              
         r         KILLER2        
         a7        b60            
l4200    =         *              a7=base
         a5        b51            taskid
         a4        b53            
         a6        b52            ply
         a5        a6+a7          ply+base
         a1        odepth-1,a5    for t4210
         a0        a6-a4          ply-splitl(taskid)
         jaz       r4210          if(ply .eq. splitl(taskid))
t4210    =         *              a1=odepth(ply), a7=base
         rdepth,a7 a1             rdepth=odepth(ply)
         r         STORE2         
l4300    =         *              
         a5        b51            taskid
         a4        b53            
         a7        b60            
         a6        b52            ply
         a0        a6-a4          ply-splitl(taskid)
         jaz       r4310          if(ply .eq. splitl(taskid))
t4310    =         *              a6=ply, a7=base
         a4        a6+a7          oldply+base
         s1        player,a7      ...
         s4        side,a7        ...
         a5        odepth-1,a4    ...
         a1        from-2,a4      ...
         a2        to-2,a4        ...
         a3        type-2,a4      ...
         a6        a6-1           dec ply by one
         s2        3              ...
         ply,a7    a6             newply=ply-1
         b52       a6             update ply
         s1        s2-s1          3-player
         s4        -s4            -side
         rdepth,a7 a5             rdepth=odepth(ply)
         player,a7 s1             player=3-player
         side,a7   s4             side=-side
         mfrom,a7  a1             mfrom=from(ply)
         mto,a7    a2             mto=to(ply)
         mtype,a7  a3             mtype=type(ply)
         r         UNMAKE         
         j         l3000          
l5000    =         *              
         a7        b60            
         a6        b52            ply
         a5        a6+a7          ply+base
         a0        mated-1,a5     
         jan       l6000          if(mated(ply) .ne. 0) 
         r         KILLER1        
l5100    =         *              
         a5        b51            taskid
         a4        b53            
         a7        b60            
         a6        b52            ply
         a3        a6+a7          ply+base
         a1        odepth-1,a3    for t5110
         a0        a6-a4          ply-splitl(taskid)
         jaz       r5110          if(ply .eq. splitl(taskid)
t5110    =         *              a1=odepth(ply),a7=base
         rdepth,a7 a1             rdepth=odepth(ply)
         r         STORE1         
l5200    =         *              
         a6        b52            
         a0        a6-1           ply-1
         a7        b60            
         jaz       retrn          if(ply .eq. 1)
t5210    =         *              
         s0        abort,         
         jsn       l7000          if(abort .ne. 0)
         s0        timeup,         
         jsn       l7000          if(timeup .ne. 0)
         r         BACKUP         
         a7        b60            
         a6        b52            ply
         a5        a6+a7          ply+base
         s1        player,a7      
         s2        side,a7        
         a1        odepth-1,a5    odepth(ply)
         a2        from-2,a5      from(ply-1)
         a3        to-2,a5        to(ply-1)
         a4        type-2,a5      type(ply-5)
         a6        a6-1           ply-1
         ply,a7    a6             ply=ply-1
         b52       a6             save ply
         s3        3              
         s3        s3-s1          3-player
         s2        -s2            -side
         rdepth,a7 a1             rdepth=odepth(ply)
         mfrom,a7  a2             mfrom=from(ply)
         mto,a7    a3             mto=to(ply)
         side,a7   s2             side=-side
         player,a7 s3             player=3-player
         mtype,a7  a4             mtype=type(ply)
         r         UNMAKE         
         a7        b60            
         s0        side,a7        
         a6        b52            ply
         a5        a6+a7          ply+base
         a1        value-1,a5     value(ply)
         a2        value-2,a5     value(ply-1)
         a0        a6-1           ply-1
         jaz       l3000          if(ply .eq. 1)
         jsm       t5220          if(side .lt. 0)
         a0        a1-a2          value(ply)-value(ply-1)
         jam       l3000          if(value(ply) .lt. value(ply-1))
         j         l4000          value(ply) .ge. value(ply-1)
t5220    =         *              a1=value(ply), a2=value(ply-1), a7=base
         a0        a2-a1          value(ply-1)-value(ply)
         jam       l3000          if(value(ply-1) .lt. value(ply))
         j         l4000          value(ply) .le. value(ply-1)
l6000    =         *              
         a7        b60            
         a5        0              
         count,a7  a5             count=0
l6100    =         *              a5=count
         a6        player,a7      
         a4        drawsc,        for t6110
         a3        1              
         a0        a3-a5          1-count
         jam       t6110          if(count .gt. 1)
         s1        side,a7        
         a5        a6+a7          player+base
         s2        kloc-1,a5      kloc(player)
         s1        -s1            -side
         r         ATTACK%        
         a4        drawsc,        for t6110
         s0        s1             attack(-side,kloc(player))
         a7        b60            
         jsn       l6200          if(attack(-side,kloc(player)) .ne. 0)
t6110    =         *              a4=drawsc, a7=base
         a5        b52            ply
         a4        a4+a5          drawsc+ply
         a5        a5+a7          ply+base
         s4        +a4            extend drawsc+ply
         value-1,a5 s4            value(ply)=drawsc+ply
         j         l6300          
l6200    =         *              a7=base
         a3        side,a7        
         a1        1000000        
         a6        b52            ply
         a1        a6-a1          -(1000000-ply) 
         a1        a1*a3          -side*(1000000-ply)
         a5        a6+a7          ply+base
         s1        +a1            extend possible negative number
         value-1,a5 s1            value(ply)=-side*(1000000-ply)
l6300    =         *              a7=base
         s0        side,a7        
         a6        b52            ply
         a5        a6+a7          ply+base
         s1        value-1,a5     value(ply)
         s2        value-2,a5     value(ply-1)
         a5        depth,a7       
         a0        a6-1           ply-1
         jaz       t6310          if(ply .eq. 1)
         jsm       t6320          if(side .lt. 0)
         s0        s1-s2          value(ply)-value(ply-1)
         jsm       t6310          if(value(ply) .lt. value(ply-1))
         j         l4200          value(ply) .ge. value(ply-1)
t6320    =         *              s1=value(ply), s2=value(ply-1)
         s0        s2-s1          value(ply-1)-value(ply)
         jsm       t6310          if(value(ply-1) .lt. value(ply))
         j         l4200          value(ply) .le. value(ply-1)
t6310    =         *              a5=depth,a6=ply,a7=base
         a4        b51            taskid
         a3        50             curmvs dimension
         a4        a4-1           taskid-1
         a3        a3*a4          (taskid-1)*50
         a2        curmvs         base address of curmvs(1,1)
         a3        a3+a2          address of curmvs(1,taskid)
         a2        a6-1           ply-1
         vl        a2             vector length = ply-1
         a0        a3             for vector f.u.
         v1        ,a0,1          curmvs(i,taskid), i=1,ply-1
         a3        52             trace dimension
         a3        a3*a2          (ply-1)*52
         a4        trace          offset to trace(1,1) in task common
         a3        a3+a7          add task common start
         a3        a3+a4          address of trace(1,ply)
         a0        a3             for vector f.u.
         ,a0,1     v1             trace(i,ply)=curmvs(i,taskid), i=1,ply-1
         50,a3     a2             trace(51,ply)=ply-1
         51,a3     a5             trace(52,ply)=depth
         j         l5100          
l7000    =         *              
         a7        b60            
         a6        b52            ply
         a5        a6+a7          ply+base
         s2        from-2,a5      from(ply-1)
         s3        to-2,a5        to(ply-1)
         s4        type-2,a5      type(ply-1)
         s5        player,a7      
         s6        side,a7        
         a6        a6-1           decrement ply
         b52       a6             save it
         a5        a5-1           decrement ply
         ply,a7    a6             ply=ply-1
         a0        a6             
         jaz       retrn          if(ply .eq. 0)
         mfrom,a7  s2             mfrom=from(ply)
         mto,a7    s3             mto=to(ply)
         mtype,a7  s4             mtype=type(ply)
         s6        -s6            -side
         side,a7   s6             side=-side
         s6        3              
         s6        s6-s5          3-player
         player,a7 s6             player=3-player
         r         UNMAKE         
         j         l7000          
l7100    =         *              
         r         SEARCHMP       
         a4        b51            taskid
         a3        splitl-1,a4    splitl(taskid)
         b53       a3             save in b53
         a7        b60            
         a6        ply,a7         ply may be changed by searchmp
         s0        return,a7      
         b52       a6             restore ply
         jsn       l4000          if(return .ne. 0) (=4000)
         j         b50            
r3050    =         *              
         r         STATS          
         a7        b60            
         j         t3050          
r3080    =         *              
         r         SHARE          
         a7        b60            
         a6        b51            
         a0        usplit-1,a6    for t3080
         j         t3080          
r3090    =         *              a7=base
         a5        0              ...
         result,a7 a5             result=0
         vwd       10/o'34,6/o'22 call lockon(22)
         r         UNSPLIT        
         vwd       10/o'36,6/o'22 call lockoff(22)
         a4        b51            taskid
         a3        splitl-1,a4    splitl(taskid)
         b53       a3             save in b53
         a7        b60            
         a5        result,a7      
         a6        ply,a7         
         a0        a5-1           result-1
         b52       a6             
         jaz       l4000          if(result .eq. 1)
         j         t3090          
r3100    =         *              s0=stop(taskid)
         jsn       t3095          if(stop .ne. 0)
         r         SPLIT          
         a4        b51            taskid
         a3        splitl-1,a4    splitl(taskid)
         b53       a3             save in b53
         a7        b60            
         a6        b51            taskid
         s0        stop-1,a6      for t3100
         a4        b53            for t3100
         j         t3100          
t3095    =         *              
         a3        ncpus,         ...
         a6        b51            taskid
         s0        stop-1,a6      for t3100
         a4        b53            for t3100
         a5        a6+1           taskid+1
         a0        a3-a5          ncpus-(taskid+1)
         jap       t3097          if(helpid .le. ncpus)
         a5        1              helpid=1
t3097    =         *              
         a2        20             ...
         a3        0              ...
         vwd       10/o'34,6/o'22 call lockon(22)
         help-1,a5 a2             help(helpid)=20
         help-1,a6 a3             help(taskid)=0
         vwd       10/o'36,6/o'22 call lockoff(22)
         j         t3100          
r3210    =         *              
         r         SELGET         
         r         SELECT         
         r         SELPUT         
         a7        b60            
         a4        return,a7      
         j         t3210          
r3220    =         *              a4=return (.ne.0)
         s6        2              ...
         a0        a4-1           return-1
         s7        a4             return
         s0        s6\s7          return.eq.2?
         jaz       l5000          if(return .eq. 1)
         jsz       l7000          if(return .eq. 2)
         j         l4300          return .eq. 3
r3330    =         *              
*        r         SEARCHP
         j         t3330          
r4210    =         *              a7=base
         a2        1              
         result,a7 a2             result=1
         r         MERGE          
         a4        b51            taskid
         a3        splitl-1,a4    splitl(taskid)
         b53       a3             save in b53
         a7        b60            
         a6        ply,a7         
         a0        return,a7      
         a5        a6+a7          ply+base
         b52       a6             
         a1        odepth-1,a5    for t4210
         jaz       l7000          if(return .eq. 0)
         j         t4210          
r4310    =         *              a7=base
         a1        1              
         result,a7 a1             result=1
         r         MERGE          
         a4        b51            taskid
         a3        splitl-1,a4    splitl(taskid)
         b53       a3             save in b53
         a7        b60            
         a6        ply,a7         
         a0        return,a7      
         b52       a6             
         jan       t4310          if(return .ne. 0)
         j         l7000          
r5110    =         *              a7=base
         a1        0              
         result,a7 a1             result=0
         r         MERGE          
         a4        b51            taskid
         a3        splitl-1,a4    splitl(taskid)
         b53       a3             save in b53
         a7        b60            
         a6        ply,a7         
         s0        return,a7      
         a2        result,a7      
         b52       a6             
         a6        a6+a7          ply+base
         a1        odepth-1,a6    for t5110
         a0        a2-1           result-1
         jsz       l7000          if(return .eq. 0)
         jaz       l4100          if(result .eq. 1)
         j         t5110          
retrn    =         *              
         j         b50            
         include   'global' 
         end       
         ident     searche        
         entry     SEARCHE        
         ext       ATTACK%        
         ext       SCORE          
         align     
SEARCHE  =         *              
         a2        b60            task common base address
         a1        ply,a2         
         a6        rdepth,a2      
         a5        b0             get rtn adr
         b56       a5             save rtn adr
         a7        70             
         s1        1              
         s2        -1             
         a3        a1+a2          ply+base
         b55       a1             sav ply
         a4        to-2,a3        to(ply-1)
         s7        type-2,a3      type(ply-1)
         a0        a6-a1          check for ply>rdepth
         s6        a6             hold rdepth for l100
         a6        a6+1           rdepth+1
         b43       a6             sav rdepth+1
         jap       l300           if ply.le.rdepth
*   get here when ply.gt.rdepth
         a5        a4+a2          to(ply-1)+base
         a0        a7-a4          70-to(ply-1)
         s3        board-1,a5     board(to(ply-1))
         a4        board+9,a5     board(to(ply-1)+10)
         s4        s6+s6          rdepth*2 (hln)
         t27       s7             save type(ply-2)
         s7        a1             move ply to sreg (hln)
         s5        board+8,a5     board(to(ply-1)+9)
         s0        s4-s7          rdepth*2-ply (hln)
         a7        board+10,a5    board(to(ply-1)+11)
         a6        2              for l100
         a5        a1-a6          ply-2 (for l100)
         jsm       l100           exit if(ply .gt. 2*rdepth) (hln)
         s0        t27            type(ply-2)
         jsz       typeok         type(ply-2).eq.0
         s4        4
         s7        t27            type(ply-2)
         s0        s7-s4          type(ply-2)-4
         jsm       l100           type(ply-2).gt.0 .and. type(ply-2).le.3
         s4        8
         s0        s7-s4          type(ply-2)-8
         jsm       l100           type(ply-2).ge.8
typeok   =         *
         jap       l100           if to.le.70
         s0        s1\s3          is +pawn?
         a0        a4+1           is opponents pawn blocking?
         jsn       l100           if not + pawn
         s0        s2\s5          can opponent pawn capture?
         jaz       l100           if there is a blocker
         a0        a7+1           can opponent pawn capture?
         jsz       l100           if capture available
         jaz       l100           if capture available
         inchk-1,a3 s1            inchk(ply)=.true.
*     s1=inchk(ply),a3=ply+base
l1401    =         *              
         s7        0              
         killmv+100,a3 s7         killmv(ply+1,3)=0
         killmv+150,a3 s7         killmv(ply+1,4)=0
         mated-1,a3 s1            mated(ply)=inchk(ply)
         a2        b60            
         a5        b56            
         b0        a5             
         return,a2 s7             
         j         b0             return
* s1=1,s6=rdepth,b43=rdepth+1,a6=2,a5=ply-2,a2=base,a3=ply+base
l100     =         *              
         a0        a5-a6          make sure ply.ge.4
         vl        a5             
         s1        -1             need bits for shift
         a7        inchk+1        .loc.inchk(2)
         jam       l200           skip if ply too low
         a0        a2+a7          
         v0        ,a0,a6         get even inchks.ge.2
         s7        a5             
         s7        s7>1           hack vl in half
         s6        s6>1           
         a6        s7             
         vl        a6             now use right vl
         vm        v0,z           set bits where not inchk
         a7        s6             (rdepth-1)/2
         s6        0              clear s6
         s6        s1,s6>a7       set skip bits (up to rdepth+1)
         s7        vm             
         s0        #s6&s7         use complement of skip bits
         jsn       l900           if any ply>rdepth not inchk
l200     =         *              s1=-1
         s2        kloc+1,a2      kloc(2)
         s1        1              
         r         ATTACK%        
         s0        s1             
         a2        b60            
         a1        b55            ply
         a3        a2+a1          
         inchk-1,a3 s1            
         jsn       l1401          if still inchk
         j         l900           
*     s1=1,rdepth+1=b43,ply=b55,a4=to(ply-1),a2=base,a3=ply+base
         align     
l300     =         *              
         a0        cappc-2,a3     cappc(ply-1)
         s6        cranks-1,a4    cranks(to(ply-1))
         s7        krank+1,a2     krank(2)
         a6        cfiles-1,a4    
         a7        kfile+1,a2     
         s2        kloc+1,a2      kloc(2) - - for l400
         jaz       l400           if no capture
         s5        s7-s6          check
         s7        s6-s7          for
         s0        s5+s1          next
         a5        a7-a6          any
         a7        a6-a7          captures
         a0        a5+1           to
         jsm       l400           the
         s0        s7+s1          king.
         jam       l400           if
         a0        a7+1           not,
         jsm       l400           go to
         jam       l400           l400.
         inchk-1,a3 s1            inchk(ply)=.true.
         a7        b43            rdepth+1
         rdepth,a2 a7             rdepth=rdepth+1
         j         l600           
*   b55=ply,s2=kloc(2),b43=rdepth+1
l400     =         *              
         s1        1              
         r         ATTACK%        uses t77 only (among t70's)
         a1        b55            ply
         a2        b60            
         a3        a1+a2          
         s0        s1             
         inchk-1,a3 s0            inchk(ply)=attack%(-1,kloc(1))
         jsz       l500           
         a7        b43            rdepth+1
         rdepth,a2 a7             rdepth=rdepth+1
         j         l600           
l500     =         *              
         a2        b60            base
         a3        b55            ply
         a3        a2+a3          ply+base
         a0        cappc-3,a3     cappc(ply-2)
         s0        cappc-2,a3     cappc(ply-1)
         s6        to-3,a3        to(ply-2)
         s7        to-2,a3        to(ply-1)
         a6        mscore,a2      
         a7        msave11-3,a3   msave11(ply-2)
         jaz       l600           cappc(ply-2) = 0
         jsz       l600           cappc(ply-1) = 0
         s0        s6-s7          
         jsn       l600           to(ply-2) != to(ply-1)
         a0        a6-a7          
         jan       l600           mscore != msave11(ply-2)
         a7        b43            rdepth+1
         rdepth,a2 a7             
l600     =         *              
         s1        1              .true.
         s3        0              
         killmv+100,a3 s3         killmv(ply+1,3)=0
         killmv+150,a3 s3         killmv(ply+1,4)=0
         mated-1,a3 s1            mated(ply)=.true.
*     inline l3000 here
         a5        b56            
         b0        a5             replace return register
         return,a2 s3             return=0
         j         b0             
*     a1=ply,a2=base,a3=ply+base
         align     
l900     =         *              
         a0        nppcs,a2       
         s0        nopcs,a2       
         a7        nppwns,a2      
         s7        nopwns,a2      
         s6        mscore,a2      
         s5        minmax+1,a2    minmax(1)
         s1        value-2,a3     value(ply-1)
         jaz       l1000          
         a0        a7             
         jsz       l1000          
         a4        2              
         s0        s7             
         jaz       l1000          
         jsz       l1000          
         s4        s5+s6          tscore
         s0        s1-s4          value(ply-1)-tscore
         tscore,a2 s4             
         s7        4000           for return
         jsp       l4000          
l1000    =         *              
         r         SCORE          clobbers b40's
         a2        b60            
         a1        b55            ply
         a3        a1+a2          ply+base
         s4        tscore,a2      stored by score
         s1        value-2,a3     value(ply-1)
         a5        taskid,a2      
         s6        value-1,a3     value(ply)
         s0        s1-s4          value(ply-1)-tscore
         a4        50             
         a7        a5*a4          (0,taskid)
         s7        4000           for return
         jsp       l4000          
l1100    =         *              
         s0        s4-s6          is tscore.ge.value(ply)
         s7        curmvs-50,a7   curmvs(1,taskid)
         a6        52             dimension of trace
         s1        inchk-1,a3     for l1400
         jsp       l1400          
*   get here if tscore.gt.value(ply)
         value-1,a3 s4            value(ply)=tscore
         a3        a6*a1          ply*dimen
         a6        1              
         a0        a1-1           check for ply=1
         a5        a1-1           ply-1
         s5        depth,a2       
         a4        a3+a2          store level=1
         a2        a3+a2          save for l1300
         jaz       l1300          if ply=1 skip
l1200    =         *              
         trace-52,a4 s7           trace(level,ply)=curmvs(level)
         a0        a6-a5          dun?
         s7        curmvs-49,a7   next curmvs
         a7        a7+1           
         a6        a6+1           
         a4        a4+1           
         jam       l1200          
l1300    =         *              
         trace-2,a2 a5            trace(51,ply)=ply-1
         trace-1,a2 s5            trace(52,ply)=depth
         a2        b60            
         a3        a1+a2          ply+base
*     s1=inchk(ply),a3=ply+base
l1400    =         *              
         s7        0              
         killmv+100,a3 s7         killmv(ply+1,3)=0
         killmv+150,a3 s7         killmv(ply+1,4)=0
         mated-1,a3 s1            mated(ply)=inchk(ply)
l4000    =         *              
         a2        b60            
         a5        b56            
         b0        a5             
         return,a2 s7             
         j         b0             return
         include   'global' 
         end       
         ident     searcho        
         entry     SEARCHO        
         ext       ATTACK%        
         ext       SCORE          
         
         align     
         
SEARCHO  =         *              
         a2        b60            task common base address
         a1        ply,a2         
         a6        rdepth,a2      
         a5        b0             get rtn adr
         b56       a5             save rtn adr
         a7        50             
         s1        1              
         s2        -1             
         a3        a1+a2          ply+base
         b55       a1             sav ply
         a4        to-2,a3        to(ply-1)
         s7        type-2,a3      type(ply-1)
         a0        a6-a1          check for ply>rdepth
         s6        a6             hold rdepth for l100
         a6        a6+1           rdepth+1
         b43       a6             sav rdepth+1
         jap       l300           if ply.le.rdepth
*   get here when ply.gt.rdepth
         a5        a4+a2          to(ply-1)+base
         a0        a4-a7          to(ply-1)-50
         s3        board-1,a5     board(to(ply-1))
         a4        board-11,a5    board(to(ply-1)-10)
         s4        s6+s6          rdepth*2 (hln)
         t27       s7             save type(ply-2)
         s7        a1             move ply to sreg (hln)
         s5        board-10,a5    board(to(ply-1)-9)
         a7        board-12,a5    board(to(ply-1)-11)
         s0        s4-s7          rdepth*2-ply (hln)
         a6        2              for l100
         a5        a1-a6          ply-2 (for l100)
         jsm       l100           exit if(ply .gt. 2*rdepth) (hln)
         s0        t27            type(ply-2)
         jsz       typeok         type(ply-2).eq.0
         s4        4  
         s7        t27            type(ply-2) 
         s0        s7-s4          type(ply-2)-4 
         jsm       l100           type(ply-2).gt.0 .and. type(ply-2).le.3 
         s4        8 
         s0        s7-s4          type(ply-2)-8 
         jsm       l100           type(ply-2).ge.8 
typeok   =         *
         jap       l100           if to.ge.50
         s0        s2\s3          is -pawn?
         a0        a4-1           is opponents pawn blocking?
         jsn       l100           if not -pawn
         s0        s1\s5          can opponent pawn capture?
         jaz       l100           if there is a blocker
         a0        a7-1           can opponent pawn capture?
         jsz       l100           if capture available
         jaz       l100           if capture available
         inchk-1,a3 s1            inchk(ply)=.true.
         j         l1400          
*     s1=inchk(ply),a3=ply+base
l1401    =         *              
         s7        0              
         killmv+100,a3 s7         killmv(ply+1,3)=0
         killmv+150,a3 s7         killmv(ply+1,4)=0
         mated-1,a3 s1            mated(ply)=inchk(ply)
         a2        b60            
         a5        b56            
         b0        a5             
         return,a2 s7             
         j         b0             return
* s1=1,s6=rdepth,b43=rdepth+1,a6=2,a5=ply-2,a2=base,a3=ply+base
l100     =         *              
         a0        a6-a5          make sure ply.ge.5
         vl        a5             
         s1        -1             need bits for shift
         a7        inchk+2        .loc.inchk(3)
         jap       l200           skip if ply too low
         a0        a2+a7          
         v0        ,a0,a6         get odd inchks.ge.3
         a5        a5-1           ply-3
         s6        s6+s1          rdepth-1
         s7        a5             
         s7        s7>1           hack vl in half
         s6        s6>1           
         a6        s7             
         vl        a6             now use right vl
         vm        v0,z           set bits where not inchk
         a7        s6             (rdepth-1)/2
         s6        0              clear s6
         s6        s1,s6>a7       set skip bits (up to rdepth+1)
         s7        vm             
         s0        #s6&s7         use complement of skip bits
         jsn       l900           if any ply>rdepth not inchk
l200     =         *              s1=-1
         s2        kloc,a2        kloc(1)
         r         ATTACK%        
         s0        s1             
         a2        b60            
         a1        b55            ply
         a3        a2+a1          
         inchk-1,a3 s1            
         jsn       l1401          if still inchk
         j         l900           
         align     
*     s1=1,rdepth+1=b43,ply=b55,a4=to(ply-1),a2=base,a3=ply+base
l300     =         *              
         a0        cappc-2,a3     cappc(ply-1)
         s6        cranks-1,a4    cranks(to(ply-1))
         s7        krank,a2       krank(1)
         a6        cfiles-1,a4    
         a7        kfile,a2       
         s2        kloc,a2        kloc(1) - - for l400
         jaz       l400           if no capture
         s5        s7-s6          check
         s7        s6-s7          for
         s0        s5+s1          next
         a5        a7-a6          any
         a7        a6-a7          captures
         a0        a5+1           to
         jsm       l400           the
         s0        s7+s1          king.
         jam       l400           if
         a0        a7+1           not,
         jsm       l400           go to
         jam       l400           l400.
         inchk-1,a3 s1            inchk(ply)=.true.
         a7        b43            rdepth+1
         rdepth,a2 a7             rdepth=rdepth+1
         j         l600           
*   b55=ply,s2=kloc(1),b43=rdepth+1
l400     =         *              
         a1        b55
         a0        a1-1           test ply=1
         s1        0              fake attack=0
         jaz       skip
         s1        -1             
         r         ATTACK%        uses t77 only (among t70's)
skip     =         *
         a1        b55            ply
         a2        b60            
         a3        a1+a2          
         s0        s1             
         inchk-1,a3 s0            inchk(ply)=attack%(-1,kloc(1))
         jsz       l500           
         a7        b43            rdepth+1
         rdepth,a2 a7             rdepth=rdepth+1
         j         l600           
l500     =         *              
         a2        b60            base
         a3        b55            ply
         a3        a2+a3          ply+base
         a0        cappc-3,a3     cappc(ply-2)
         s0        cappc-2,a3     cappc(ply-1)
         s6        to-3,a3        to(ply-2)
         s7        to-2,a3        to(ply-1)
         a6        mscore,a2      
         a7        msave11-3,a3   msave11(ply-2)
         jaz       l600           cappc(ply-2) = 0
         jsz       l600           cappc(ply-1) = 0
         s0        s6-s7          
         jsn       l600           to(ply-2) != to(ply-1)
         a0        a6-a7          
         jan       l600           mscore != msave11(ply-2)
         a7        b43            rdepth+1
         rdepth,a2 a7             
l600     =         *              
         s1        1              .true.
         s3        0              
         killmv+100,a3 s3         killmv(ply+1,3)=0
         killmv+150,a3 s3         killmv(ply+1,4)=0
         mated-1,a3 s1            mated(ply)=.true.
*     inline l3000 here
         a5        b56            
         b0        a5             replace return register
         return,a2 s3             return=0
         j         b0             
*     a1=ply,a2=base,a3=ply+base
         align     
l900     =         *              
         a0        nppcs,a2       
         s0        nopcs,a2       
         a7        nppwns,a2      
         s7        nopwns,a2      
         s6        mscore,a2      
         s5        minmax,a2      minmax(1)
         s1        value-2,a3     value(ply-1)
         jaz       l1000          
         a0        a7             
         jsz       l1000          
         a4        2              
         s0        s7             
         jaz       l1000          
         jsz       l1000          
         s4        s5+s6          tscore
         s0        s4-s1          tscore-value(ply-1)
         tscore,a2 s4             
         s7        4000           for return
         jsp       l4000          
l1000    =         *              
         r         SCORE          clobbers b40's
         a2        b60            
         a1        b55            ply
         a3        a1+a2          ply+base
         s4        tscore,a2      stored by score
         s1        value-2,a3     value(ply-1)
         a5        taskid,a2      
         s6        value-1,a3     value(ply)
         s0        s4-s1          tscore-value(ply-1)
         a4        50             
         a7        a5*a4          (0,taskid)
         s7        4000           for return
         jsp       l4000          
l1100    =         *              
         s0        s6-s4          is tscore.le.value(ply)
         s7        curmvs-50,a7   curmvs(1,taskid)
         a6        52             dimension of trace
         s1        inchk-1,a3     for l1400
         jsp       l1400          
*   get here if tscore.gt.value(ply)
         value-1,a3 s4            value(ply)=tscore
         a3        a6*a1          ply*dimen
         a6        1              
         a0        a1-1           check for ply=1
         a5        a1-1           ply-1
         s5        depth,a2       
         a4        a3+a2          store level=1
         a2        a3+a2          save for l1300
         jaz       l1300          if ply=1 skip
l1200    =         *              
         trace-52,a4 s7           trace(level,ply)=curmvs(level)
         a0        a6-a5          dun?
         s7        curmvs-49,a7   next curmvs
         a7        a7+1           
         a6        a6+1           
         a4        a4+1           
         jam       l1200          
l1300    =         *              
         trace-2,a2 a5            trace(51,ply)=ply-1
         trace-1,a2 s5            trace(52,ply)=depth
         a2        b60            
         a3        a1+a2          ply+base
*     s1=inchk(ply),a3=ply+base
l1400    =         *              
         s7        0              
         killmv+100,a3 s7         killmv(ply+1,3)=0
         killmv+150,a3 s7         killmv(ply+1,4)=0
         mated-1,a3 s1            mated(ply)=inchk(ply)
l4000    =         *              
         a2        b60            
         a5        b56            
         b0        a5             
         return,a2 s7             
         j         b0             return
         include   'global' 
         end       
