c     global  (shared) common
      character name*20,version*4, hhmmss*8
      dimension isolat(0:8), isolatof(0:8), pfiles(10), oranks(9),
     * pranks(9), atkfil(10), ksafety(10), iblocker(-6:6), 
     * blocker(-6:6)
c
c
      common /global1/  abort,   aio,   aiow,   alloc(16),   alpha(80),
     * at(100,100,13),  atext(80),  atply(16),  autos,  avgtim,   beep,
     * begin(6),  bell,   bfull,  bit0(100),   bit1(100),   bit10(100),
     * bit11(100),  bit9(100),  block(16),  broke,  buffer(80),   burp,
     * castkg,   castqn,  cbegin,  cbias(2),  cbit0(100),  center(100),
     * centern(100), cfile,   cfiles(0:100),   cntrls,  color,  cotime,
     * cpu, cputim,cquery,crank, cranks(0:100),curmvs(50,16),cvside(2),
     * debug, devdone, dlevel(50,16), d4, drawsc, easy, end(6), enpass,
     * eval, evbnbd(100), even(50),exnodes(50), expect, e4, falses(50),
     * fdepth, foundm,  frmove, fsec1, ftime,  full,  help(16),  hsize,
     * hsizm1,  htbl1, htbl2, idle, idlet(16), inbook,  ksize,  ksizm1,
     * ktbl1, ktbl2,  lmoveo, lmovep, lockht,  lockmp, lockpt,  locksd,
     * lowered, margin,  matchd,  matebd(100),  matem,  maxply, mchtyp,
     * metime, minply, movdir(28)
       common /global2/ names(100),  nbusy(16),  nchar,  ncpus, nomovs,
     * normal, npmovs,nsplits, nstops(50),numout, odbnbd(100), odd(50),
     * oelap,oopost(100), orate,order(200), osec1, osec2,over,overhead,
     * pbsave(150),  pelap,  peval,  piecem(28), pieces(-6:6), playslf,
     * playfp(2),    pmfrom,  pmove,  pmto,  pmtype,  pndrng,  pointsv,
     * ponmvs(7,200),  popost(100), prate, prcmnds(5,10),  predmv(400),
     * prevmv(6), promot, psave(2), psec1, psec2, psize, psizm1, ptbl1,
     * ptbl2,  ptext(30),  raised,  random(100,13),  realid,   rhashes,
     * rkhashs,rmoves(2), rnodes,rpcalls, rphashs, rttime, scolor(100),
     * sdebug,   shareit(16),   smode,   snodes,   sortp1,  splitl(16),
     * splits(50), sptime, stop(16), stopping(16),surpls, tflag,timeup,
     * timlim(5),tmode,tmpnodes(100), tnodes, totnodes(16), tprocs(50),
     * tried(200), trueval, unit,  untime, usplit(16),  vterm,  vttype,
     * window(2),  working(16),  xnullm, xomovgen(100),  xpmovgen(100),
     * zrev(3), zunrev(3)
      common /global3/ name, version
c
c
      common /scoring/ scoring(200), okingb(100), oqueenb(100),
     * orookb(100), obishopb(100),  oknightb(100), opawnb(100),
     * pkingb(100),  pqueenb(100),  prookb(100), pbishopb(100),
     * pknightb(100), ppawnb(100)
      equivalence
     * (bbonus,scoring(1)),         (bcenter,scoring(2)),
     * (bkingtrp,scoring(3)),       (bocdiv,scoring(4)),
     * (brpscore,scoring(5)),       (isolat(0),scoring(6)),
     * (isolatof(0),scoring(15)),   (kaqmov,scoring(24)),
     * (koqmov,scoring(25)),        (ncenter,scoring(26)),
     * (nkingtrp,scoring(27)),      (noutpost,scoring(28)),
     * (oranks(1),scoring(29)),     (pbackwd,scoring(38)),
     * (pdouble,scoring(39)),       (pen8p,scoring(40)),
     * (pfiles(1),scoring(41)),     (pkclose,scoring(51)),
     * (develop,scoring(52)),       (popose,scoring(53))
      equivalence
     * (poutside,scoring(54)),      (ppasspn,scoring(55)),
     * (ppawncon,scoring(56)),      (ppawnrnk,scoring(57)),
     * (pqueen,scoring(58)),        (prams,scoring(59)),
     * (pranks(1),scoring(60)),     (ptriple,scoring(69)),
     * (pwonkp,scoring(70)),        (qcenter,scoring(71)),
     * (qearly,scoring(72)),        (qkingtrp,scoring(73)),
     * (rbehindp,scoring(74)),      (rhalfopn,scoring(75)),
     * (rkingtrp,scoring(76)),      (ron7th,scoring(77))
      equivalence
     * (ropen,scoring(78)),         (rpmaydrw,scoring(79)),
     * (rbad,scoring(80)),          (rtrapped,scoring(81)),
     * (kcenter,scoring(82)),       (khole,scoring(83)),
     * (kkingtrp,scoring(84)),      (kluft,scoring(85)),
     * (knocast,scoring(86)),       (knocastk,scoring(87)),
     * (kpawns,scoring(88)),        (atkfil(1),scoring(89)),
     * (ksafety,scoring(99)),       (pblocked,scoring(109)),
     * (tradown,scoring(110)),      (tradpwin,scoring(111)),
     * (iblocker(-6),scoring(112)), (blocker(-6),scoring(125)),
     * (atkweakp,scoring(138))
c
c
      common htable(971776)
c
c     end of global common
