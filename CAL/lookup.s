         include   'common'       
         ident     lookup         
         entry     LOOKUP         
         align     
possep   =         *              
         a4        a6+a7          
         a5        board-1,a4     board(to(ply-1))
         a2        cfiles-1,a6    cfiles(to(ply-1))
         a1        board-0,a4     board(to(ply-1)+1)
         a6        board-2,a4     board(to(ply-1)-1)
         a4        b44            restore side to a4
         a0        a5+a4          was pawn pushed by -
         a5        b45            
         jan       l100           
         a0        a1-a4          can this pawn take it?
         s2        random-1,a2    random(cfiles(to(ply-1)))
         jan       ckosp          no
         j         l100m1         
ckosp    =         *              
         a0        a6-a4          can this pawn take it?
         jan       l100           
l100m1   =         *              
         s1        s1\s2          hashbd=xor(hash,ran)
         s6        <10            1023
         s6        s6<24          1023.ls.24
         s6        s6&s1          rindex.ls.24
         s6        s6>24          rindex if not 0
         j         l100           
LOOKUP   =         *              
*        s3        debug,
         a7        b60            task common base address
         a5        ply,a7         
         a4        side,a7        
         s1        hash,a7        
         s4        0              
         s5        hsizm1,        hsize-1
         a1        a5+a7          add in base
         b45       a5             save ply
         s2        moveds-2,a1    moveds(ply-1)
         a3        moveds-3,a1    moveds(ply-2)
         a6        to-2,a1        to(ply-1)
         a2        20             
         a2        a4*a2          20*side
         hmove-1,a1 s4            set hmove(ply) to 0
         b43       a1             sav ply+base
         a1        from-2,a1      from(ply-1)
         s6        <10            1023
         s2        s2<2           
         b44       a4             save side
         a4        s2             
*        s2        4
         a3        a4+a3          ndx=moveds(ply-2)+(moved(ply-1).ls.2)
         s7        1              for or of rindex
*        s0        s2&s3
         s3        random+100,a3  random(ndx+1,2)
         a3        htbl1,         incr to 2nd word of table
         a2        a6+a2          to+20*side
         s6        s6<24          1023.ls.24
         a0        a2-a1          to-from+20*side
*        jsn       l9999
         s1        s1\s3          hashbd
         s6        s6&s1          rindex.ls.24
         s6        s6>24          rindex if not even
         jaz       possep         if to-from.eq.20*side
l100     =         *              
         a4        a3+a3          htbl2
         a0        b44            which side?
         s5        s1&s5          hkey=and(hbd,hsizm1)
         s7        s7!s6          rindex=rindex.or.1
         a6        0              hshft if side=1
         a1        s5             rkey1=hkey
         a2        s7             rindex
         jap       pnot2          side .eq. 1
         a6        32             hshft if side=-1
         a1        a1+a4          rkey1=hkey+htbl2
         a3        -a3            subtract if side=-1
pnot2    =         *              
         a4        htable         base adr of htable
         a7        8              number of colsns (+1)
         a0        a1+a4          loc(htable(rkey1))
         vl        a7             8
         vwd       10/o'34,6/o'20 call lockon(store)
         v0        ,a0,a2         htable(rkey1)
         s1        s1>24          now use left 40 bits
         a7        a3+a1          rkey2
         s1        s1<24          hashbd.ls.(64-hbsiz)
         a0        a7+a4          loc(htable(rkey2))
         a3        24             64-hbsiz
         v1        v0<a3          and(htable(rkey1),bmask)
         v3        ,a0,a2         htable(rkey2)
         v2        s1-v1          hashbd-and((),)
         vwd       16/o'2700      (cmr) make sure all fetches have begun
         vwd       10/o'36,6/o'20 call lockoff(store)
         a7        b60            
         vm        v2,z           any hits
         return,a7 s4             set return to 0
         s0        vm             any hits?
         s1        vm             for l200 if hit
         jsn       l200           if hit do more
         j         b0             
*     *         word 1   2 bits : move number this position stored from
*     *                 22 bits : position's value + 2000000           *
*     *                 40 bits : hashed board position                *
*     *                                                                *
*     *         word 2   7 bits : level of search from this position   *
*     *                           plus a bias of 64                    *
*     *                  2 bits : table entry type (good, bound, etc.) *
*     *                 23 bits : suggested move for this position     *
*   a1=rkey1,a3=hitincr,a5=ply,b43=hshft
l200     =         *              
         a1        zs1            leftmost hit
         a4        rdepth,a7      
         s2        v3,a1          htable(rkey2)
      s1        v0,a1          htable(rkey1)
         a2        zs0            64
         s4        <23            hmmask
         s7        hashes,a7      mlevel
         s2        s2>a6          htable(rkey2).rs.hshft
         a2        a5-a2          ply-64
         s5        s2             copy htable(rkey2).rs.hshft
      s1        s1<1           onerep(ply) in leftmost bit
         s2        s2>25          htable(rkey2).rs.25
         s6        <7             mask for mlevel
         s2        s2&s6          mlevel+64
         s6        v0,a1          htable(rkey1)
         s3        a0             1
      s1        s1>63          onerep(ply) in rightmost bit
         a3        s2             mlevel+64
         a6        a4-a2          
         s7        s7+s3          hashes+1
*     onerep-1,a6 s1           onerep(ply)=htable onerep(ply)
      t30       s1             save htable onerep(ply)
         a0        a3-a6          mlevel-(rdepth-ply)
         s6        s6>40          (movenum+tbound).rs.hbsiz
         a6        b43            recover ply+base
         s1        <14            16383
         s4        s5&s4          hmove
         s0        s4\s1          
      s1        t30            htable onerep(ply)
         jsn       l305           
         s4        0              may set hmove to 0
      hmove-1,a6 s4            store in hmove(ply)
      jap       l310           
      j         b0             
l305     =         *              
         hmove-1,a6 s4            store in hmove(ply)
      onerep-1,a6 s1           onerep(ply)
         jap       l310           
         j         b0             
l9999    =         *              return=0
         a7        b60
         s1        0
         return,a7 s1
         j         b0
l310     =         *              a6=ply+base
         hashes,a7 s7             if counting a hash only when used
         s2        <22            value mask
         s4        2000000        
         s1        <2             3
         s1        s1<23          3.ls.23
         s7        s1&s5          type
         s0        s7\s1          type-3
         s1        value-3,a6     value(ply-2) for l600
         s5        s6&s2          tbound+2000000
         s6        value-2,a6     value(ply-1) for l900
         s3        s3<24          2.ls.23=4.ls.22
         s5        s5-s4          tbound
         s4        0              .false.
         jsz       l900           if type=3
         s0        s7\s3          
         jsz       l600           if  type=2
*       b44=side,b45=ply,b43=ply+base,s5=tbound
l400     =         *              type=1
         a5        b45            ply
         s2        1500000        
         s0        s5             is tbound + ?
         s1        900000         
         s7        s5             atbnd
         jsp       l422           if was +
         s7        -s5            atbnd
l422     =         *              
         a4        drawsc,        
         s3        a5             ply
         s1        s1-s7          atbnd in range?
         s2        s7-s2          atbnd in range?
         s4        s7-s3          abs(tbnd-ply)
         jsp       l423           
         s4        -s4            -abs(tbound-ply)
l423     =         *              
         s0        s1&s2          - if in range
         jsp       l424           if out range
         s5        s4             new tbound if in range
l424     =         *              
         a2        s5             tbound
         a0        a2-a4          tb-drawsc
         a6        100            
         a4        a4+a6          ds+100
         jam       l425           
         a0        a4-a2          ds+100-tb
         a2        a2+a5          tbound+ply
         jam       l425           
         s5        +a2            if changing tbound
l425     =         *              
         a0        b44            which side?
         s0        s5-s6          quit like l9997
         a2        2              return
         jam       l470           go to alternate exit test
*  from here on leave s4=0 for return
         s4        0              .false.
         jsm       l500           dont quit
         a5        b43            ply+base
         return,a7 a2             return=2
         mated-2,a5 s4            mated(ply-1)=.false.
         j         b0             
l470     =         *              
         s0        s6-s5          quit like l9997
         a2        2              return
         s4        0              .false.
         jsm       l500           dont quit
         a5        b43            ply+base
         return,a7 a2             return=2
         mated-2,a5 s4            mated(ply-1)=.false.
         j         b0             
*    a5=ply,s5=tbound,a7=base,a0=side
l500     =         *              
         a2        taskid,a7      
         a0        depth,a7       
         a6        trace-52       loc(trace(1,0))
         a4        52             dimension of trace
         a4        a4*a5          52*ply
         a3        b43            ply+base
         value-1,a3 s5            value(ply)=tbound
         a3        a4+a6          loc(trace(1,ply))
         a1        50             
         a2        a2*a1          (0,taskid)
         a1        65535          
         a1        a1+a5          65536+ply-1
         a3        a3+a7          
         a6        a5-1           limit=ply-1
         51,a3     a0             trace(52,ply)=depth
         s2        curmvs-50,a2   curmvs(1,taskid)
         s0        a6             is ply=1
         50,a3     a1             trace(51,ply)=
         a0        a6-1           one pass only?
         s3        1              return for 9998
         jsz       m9998          
         a6        a6-1           fix bug
         a1        a7+1           level=2
         jaz       e550m          
         a6        a6+a7          
l530m    =         *              
         a0        a6-a1          limit-level
         a1        a1+1           level=level+1
         a2        a2+1           
         0,a3      s2             trace(level,ply)=curmvs((level),taskid)
         s2        curmvs-50,a2   curmvs((level+1),taskid)
         a3        a3+1           level,ply=level+1,ply
         jan       l530m          
e550m    =         *              
         0,a3      s2             final store
m9998    =         *              s3=1
         a5        b43            ply+base
         return,a7 s3             
         mated-2,a5 s4            mated(ply-1)=.false.
         j         b0             
*      b44=side,b43=ply+base,s5=tbound,s1=value(ply-2),s4=0
l600     =         *              
         s0        s5-s1          tbound-value(ply-2)
         a0        b44            which side?
         a2        1              
         a5        b43            ply+base
*        jsz       l9998          
         jsz       m9999          
         jam       l600m          
         jsp       m9999          
l9998    =         *              
         return,a7 a2             
         mated-2,a5 s4            mated(ply-1)=.false.
         j         b0             
l600m    =         *              
         jsm       m9999          
         return,a7 a2             
         mated-2,a5 s4            mated(ply-1)=.false.
m9999    =         *              
         j         b0             
*      b44=side,b43=ply+base,s5=tbound,s6=value(ply-1),s4=0
l900     =         *              
         s0        s5-s6          tbound-value(ply-1)
         a0        b44            which side?
         a2        2              
         a5        b43            ply+base
         jsz       l9997          
         jap       l900p          
         jsp       m9999          
l9997    =         *              
         return,a7 a2             
         mated-2,a5 s4            mated(ply-1)=.false.
         j         b0             
l900p    =         *              
         jsm       m9999          
         return,a7 a2             
         mated-2,a5 s4            mated(ply-1)=.false.
         j         b0             
         include   'global'       
         end       
