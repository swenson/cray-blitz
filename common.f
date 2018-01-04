c     task    common (local)
c
c
c------------------------------< this common is local to each
c------------------------------< task created during the tree
c------------------------------< search when using multi-
c------------------------------< processing. take note that some
c------------------------------< entries are 64 bits, even on a
c------------------------------< 32 bit machine and should be
c------------------------------< dimensioned accordingly.
c
      common /task/ tdata(8100)
c
      dimension
     *  bdsave(150),board(-55:176), cappb(50),cappc(50), cappt(50),
     *  cutmvs(50),  first(50),  from(50),  givchk(50),  hmove(50),
     *  inchk(50),   kfile(2),  killmv(50,4),   kloc(2),  krank(2),
     *  last(50), mated(50), minmax(2),  movedr(2),  moveds(-1:50),
     *  moves(2500),msave1(50), msave10(50),msave11(50),msave2(50),
     *  msave3(50), msave4(50), msave5(50), msave6(50), msave7(50),
     *  msave8(50), msave9(50), ocount(10), odepth(50), ofirst(10),
     *  olast(10),  opass(10),  oweakp(10), pboard(100),  pcend(2),
     *  pcount(10), pend(2),   pfirst(10),  phase(50),   plast(10),
     *  plist(33),  ppass(10),  pstart(2),  pweakp(10), pwnstrt(2),
     *  status(50), tdepth(50), text(80),   to(50),   trace(52,50),
     *  type(50),   val(200),   value(-1:50), vlist(33), which(50),
     *  onerep(50)
c
c------------------------------< all of this common section will
c------------------------------< be copied when parallel tasks
c------------------------------< are created.  note that if this
c------------------------------< section of the task common is
c------------------------------< made longer, copyl and copyg
c------------------------------< must be modified to copy the
c------------------------------< added data. (words 1-749 will
c------------------------------< be copied automatically.)
c
      equivalence
     *  (afile,tdata(1)),         (arank,tdata(2)),
     *  (bitbd,tdata(3)),         (board(1),tdata(63)),
     *  (depth,tdata(239)),       (dnodes,tdata(241)),
     *  (hash,tdata(242)),        (hashbd,tdata(243)),
     *  (kfile(1),tdata(246)),    (killmv(1,1),tdata(249)),
     *  (kloc(1),tdata(449)),     (krank(1),tdata(451)),
     *  (minmax(1),tdata(453)),   (mscore,tdata(455))
      equivalence
     *  (nopcs,tdata(457)),       (nopwns,tdata(458)),
     *  (npawns,tdata(459)),      (nppcs,tdata(460)),
     *  (nppwns,tdata(461)),      (ocount(1),tdata(462)),
     *  (ofirst(1),tdata(472)),   (olast(1),tdata(482)),
     *  (pboard(1),tdata(492)),   (pcend(1),tdata(593)),
     *  (pcount(1),tdata(595)),   (pend(1),tdata(605)),
     *  (pfirst(1),tdata(607)),   (phash,tdata(617)),
     *  (plast(1),tdata(619)),    (player,tdata(629)),
     *  (plist(1),tdata(630)),    (ply,tdata(663))
      equivalence
     *  (point,tdata(664)),       (pstart(1),tdata(665)),
     *  (pwnstrt(1),tdata(667)),  (rdepth,tdata(669)),
     *  (return,tdata(670)),      (side,tdata(671)),
     *  (result,tdata(672)),      (tscore,tdata(673)),
     *  (vlist(1),tdata(674)),    (xnodes,tdata(709)),
     *  (xscore,tdata(710)),      (znodes,tdata(711))
c
c------------------------------< each of the arrays in this common
c------------------------------< section will have the first 'ply'
c------------------------------< elements copied when parallel
c------------------------------< tasks are created.  any arrays in
c------------------------------< this section must have one of its
c------------------------------< subscripts equal to 50.
c
      equivalence
     *  (bdsave(1),tdata(750)),   (cappb(1),tdata(900)),
     *  (cappc(1),tdata(1000)),   (cappt(1),tdata(1050)),
     *  (cutmvs(1),tdata(1100)),  (first(1),tdata(1150)),
     *  (from(1),tdata(1200)),    (givchk(1),tdata(1250)),
     *  (hmove(1),tdata(1300)),   (inchk(1),tdata(1350)),
     *  (last(1),tdata(1400)),    (mated(1),tdata(1450)),
     *  (movedr(1),tdata(1500)),  (moveds(1),tdata(1502)),
     *  (msave1(1),tdata(1552)),  (msave2(1),tdata(1602)),
     *  (msave3(1),tdata(1652)),  (msave4(1),tdata(1702)),
     *  (msave5(1),tdata(1752)),  (msave6(1),tdata(1802))
      equivalence
     *  (msave7(1),tdata(1852)),  (msave8(1),tdata(1902)),
     *  (msave9(1),tdata(1952)),  (msave10(1),tdata(2002)),
     *  (msave11(1),tdata(2052)), (odepth(1),tdata(2102)),
     *  (phase(1),tdata(2152)),   (status(1),tdata(2202)),
     *  (to(1),tdata(2252)),      (trace(1,1),tdata(2302)),
     *  (type(1),tdata(4902)),    (value(1),tdata(4954)),
     *  (which(1),tdata(5004)),   (onerep(1),tdata(5054))
c
c------------------------------< none of this common section will
c------------------------------< be copied when parallel tasks
c------------------------------< are created.  note that this
c------------------------------< section of the task common is
c------------------------------< used to pass local parameters
c------------------------------< between subroutines in the same
c------------------------------< task/ply and are temporary.
c
      equivalence
     *  (checkp,tdata(5200)),     (count,tdata(5201)),
     *  (gencap,tdata(5202)),     (hashes,tdata(5203)),
     *  (hmask,tdata(5204)),      (hshft,tdata(5205)),
     *  (khash,tdata(5206)),      (khashs,tdata(5207)),
     *  (kkey1,tdata(5208)),      (mcappc,tdata(5209)),
     *  (mfrom,tdata(5210)),      (movenm,tdata(5211)),
     *  (movgpc,tdata(5212)),     (mpiece,tdata(5213)),
     *  (mpropc,tdata(5214)),     (mto,tdata(5215)),
     *  (mtype,tdata(5216)),      (nodes,tdata(5217)),
     *  (opass(1),tdata(5218)),   (opassed,tdata(5228)),
     *  (osafety,tdata(5229)),    (oweakp(1),tdata(5230)),
     *  (pcalls,tdata(5240)),     (pflag,tdata(5241)),
     *  (pflag1,tdata(5242))
      equivalence
     *  (pflag2,tdata(5243)),     (phashs,tdata(5244)),
     *  (pkey1,tdata(5245)),      (ppass(1),tdata(5246)),
     *  (ppassed,tdata(5256)),    (psafety,tdata(5257)),
     *  (pscore,tdata(5258)),     (pweakp(1),tdata(5259)),
     *  (rkey1,tdata(5269)),      (rkey2,tdata(5270)),
     *  (rpflag,tdata(5271)),     (sbnodes,tdata(5272)),
     *  (square,tdata(5273)),     (taskid,tdata(5274)),
     *  (text(1),tdata(5275)),    (tplayr,tdata(5355)),
     *  (tside,tdata(5356)),      (tradepsc,tdata(5357)),
     *  (val(1),tdata(5358)),     (tpscor,tdata(5558)),
     *  (toscor,tdata(5559)),     (cnodes,tdata(5560))
c
c------------------------------< the "used" part of the moves array
c------------------------------< will be copied when parallel tasks
c------------------------------< are created.  
c
      equivalence 
     *  (moves(1),tdata(5600))
c     end task common
