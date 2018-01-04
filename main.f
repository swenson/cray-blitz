      program blitz
crev  last revision 12/09/90
c
c     ******************************************************************
c     *                                                                *
c     *                                                                *
c     *             cccccc    rrrrrrr      aa      yy    yy            *
c     *            cc    cc   rr    rr    aaaa      yy  yy             *
c     *            cc         rr    rr   aa  aa      yyyy              *
c     *            cc         rrrrrrr   aaaaaaaa      yy               *
c     *            cc         rr   rr   aa    aa      yy               *
c     *            cc    cc   rr    rr  aa    aa      yy               *
c     *             cccccc    rr    rr  aa    aa      yy               *
c     *                                                                *
c     *                                                                *
c     *       bbbbbbb    ll         iiii   tttttttt   zzzzzzzz         *
c     *       bb    bb   ll          ii       tt           zz          *
c     *       bb    bb   ll          ii       tt          zz           *
c     *       bbbbbbb    ll          ii       tt         zz            *
c     *       bb    bb   ll          ii       tt        zz             *
c     *       bb    bb   ll          ii       tt       zz              *
c     *       bbbbbbb    llllllll   iiii      tt      zzzzzzzz         *
c     *                                                                *
c     *                                                                *
c     ******************************************************************
c     ******************************************************************
c     *                                                                *
c     *          Robert M Hyatt                                        *
c     *          University of Alabama at Birmingham                   *
c     *          Department of Computer and Information Sciences       *
c     *          Birmingham, AL  35294                                 *
c     *          phone 205-934-2213                                    *
c     *                                                                *
c     *          Albert E Gower                                        *
c     *          University of Southern Mississippi                    *
c     *          Department of Music                                   *
c     *          Hattiesburg, MS  39406                                *
c     *          phone 601/266-5369                                    *
c     *                                                                *
c     *          Harry L Nelson                                        *
c     *          Lawrence Livermore National Laboratory                *
c     *          4259 Emory Way                                        *
c     *          Livermore, CA  94550                                  *
c     *          phone 510/422-4204                                    *
c     *                                                                *
c     ******************************************************************
c     *                                                                *
c     *      Cray Blitz is in its seventh major revision of the        *
c     *  chess program developed  jointly by the university of south-  *
c     *  ern mississippi and  lawrence livermore  national laboratory  *
c     *  and   supported  by   cray  research,  incorporated.  it  is  *
c     *  basically  a  shannon type 'a'  chess program that  searches  *
c     *  full width to some time-determined depth.                     *
c     *                                                                *
c     *      this code is the latest revision and incorporates multi-  *
c     *  processing / multi-tasking  as an  integral part of the tree  *
c     *  search mechanism.  the program uses  the dynamic tree         *
c     *  splitting algorithm as developed by the authors.              *
c     *                                                                *
c     *     1.  the program divides up the work at  whatever depth it  *
c     *         determines that sufficient work is available for other *
c     *         processors to help with.  busy processors do this      *
c     *         'split' operation whenever another processor exhausts  *
c     *         its work and becomes idle.                             *
c     *                                                                *
c     *     2.  when a processor goes idle, it first checks to see if  *
c     *         a valid split point already exists (caused by some     *
c     *         other processor already having done a 'help').  if so, *
c     *         it merely joins in and helps at that node without      *
c     *         informing any other processor or invoking any overhead *
c     *         to do so.                                              *
c     *                                                                *
c     *     3.  if the above test fails, it then broadcasts a 'help'   *
c     *         request to any busy processors.  these processors then *
c     *         analyze the tree they are currently working on and     *
c     *         determine if they have potential nodes where the       *
c     *         work can be shared with idle processors.  if so, the   *
c     *         bookkeeping is done and the idle processors join in    *
c     *         to help the current processor.                         *
c     *                                                                *
c     *     4.  with this DTS algorithm, there is absolutely no        *
c     *         master/slave relationship.  if processor 2 gets a      *
c     *         help request from processor 4 and splits with it, it   *
c     *         is perfectly acceptable for processor 2 to finish      *
c     *         first and then ask processor 4 if it can help with     *
c     *         the remaining work.  in this way, no one 'owns' a node *
c     *         and there is no synchronization as a node is           *
c     *         completed, eliminating significant delays with other   *
c     *         approaches.                                            *
c     *                                                                *
c     *     5.  if desired, the program can be configured so that all  *
c     *         processors  work  on the  same  ply= 1 move  together  *
c     *         so that  a better move  that occurs early in the move  *
c     *         list will  be examined more quickly. in this case the  *
c     *         time  to  complete  the  complete search  will be  no  *
c     *         faster but  the time to find  the correct move can be  *
c     *         speeded up dramatically.                               *
c     *                                                                *
c     *      this Cray Blitz, while being a full-width or brute-force  *
c     *  chess program, has several unique features to distinguish it  *
c     *  from other full-width programs.  first, it has an evaluation  *
c     *  function that is not designed solely on the basis of speed.   *
c     *                                                                *
c     *      the second,  and perhaps  most important  feature of the  *
c     *  evaluation function of this Cray Blitz is it's understanding  *
c     *  of pawns.  in particular, Blitz understands the concept of a  *
c     *  pawn being unstoppable or uncatchable  (with no enemy pieces  *
c     *  on  the  board  of course)  during  endgame  analysis.  this  *
c     *  is augmented by the  deep searches possible on the cray-ymp8  *
c     *  computer system, allows Blitz to play extremely well in king  *
c     *  pawn  endgames.                                               *
c     *                                                                *
c     *      another difference between Blitz and it's contemporaries  *
c     *  is in the quiescence search.  while most full-width programs  *
c     *  examine all  possible sequences of captures, Blitz does not.  *
c     *  generally, it only analyzes  captures that appear to be even  *
c     *  or better.  the principle  function of the quiescence search  *
c     *  in Blitz is  not to win material  via sacs and combinations,  *
c     *  but rather  to  refine  the tactical  accuracy of  the  tree  *
c     *  search that  is conducted.  both  methods  leave much  to be  *
c     *  desired  due to  the  exclusion of  non-capturing  moves  in  *
c     *  quiescence.    however,  Blitz   does  do   some  additional  *
c     *  searching  when  conducting  the  quiescence  search, namely  *
c     *  analyzing  checks.  Blitz  can analyze  checks quite deeply,  *
c     *  and, once the opponents king gets exposed, usually find ways  *
c     *  to win material  or deliver  checkmate rather quickly.  this  *
c     *  does not  hinder the basic  search since the check responses  *
c     *  are evaluated  full-width to make  sure that whatever is won  *
c     *  is really forced.  briefly, with  a n ply search, Blitz will  *
c     *  analyze checking sequences up to  4 additional plies  in the  *
c     *  quiescence search with some extremely strict rules governing  *
c     *  when (see routine mater for more specific information) these  *
c     *  checks are included.  also, plies where the side on move are  *
c     *  in check are not against the  the basic depth  since getting  *
c     *  out  of  check  is  forced  (if  possible) and  reduces  the  *
c     *  branching factor  for that ply  drastically.  a  move  which  *
c     *  captures a pieces adjacent to  the  king is also  considered  *
c     *  a check regardless of whether it is or not. it seems foolish  *
c     *  to have the king on g1 and have the  opponent play bxh2+ and  *
c     *  see the mate, but if the opponent can play bxg2, not see the  *
c     *  mate because bxg2 was not a check.                            *
c     *                                                                *
c     *      this code also considers pawn promotions and passed pawn  *
c     *  moves beyond the 5th rank since these moves sometimes         *
c     *  threaten to upset the material balance. when considering      *
c     *  pushes  to the seventh rank,  these moves are considered      *
c     *  checks so that the opposing side must make a move giving the  *
c     *  pawn a chance to push on and perhaps promote.                 *
c     *                                                                *
c     *      Cray Blitz is currently rated at around 2500 by various   *
c     *  means. it is doing a parallel tree search to a depth of 9-11  *
c     *  plies during the middle game when running on a cray-ymp/C90.  *
c     *  it also  seems to be playing around  the 2800 level in speed  *
c     *  chess where it does  exhaustive 7 to 9 ply searches in about  *
c     *  5 seconds  during the  middle game, and  searches  beyond 15  *
c     *  plies in king and pawn endgames.                              *
c     *                                                                *
c     *      main  is the  driver of  the  program.  it inputs moves/  *
c     *  commands, calls  the appropriate  routines  and  outputs the  *
c     *  program's chosen move  whenever necessary.  it also performs  *
c     *  timing functions  for both the  player and program notifying  *
c     *  the operator of the amount of time used in move selection.    *
c     *                                                                *
c     *    Recent changes:                                             *
c     *                                                                *
c     *    48a:  eliminated the old way of sorting the ply-1 move      *
c     *          list.  we now use a couple of seconds to do a one,    *
c     *          two, ..., n ply "absolute value" search where the     *
c     *          alpha/beta window is infinitely wide to compute       *
c     *          absolute scores for each ply-1 move.  this score      *
c     *          is used to (a) sort the ply-1 moves and (b) to        *
c     *          produce a reasonable move to ponder when the program  *
c     *          has a "short" principle variation.                    *
c     *                                                                *
c     *    48b:  added the "true-valued search" option so that the     *
c     *          program can be forced to give analysis for every      *
c     *          legal move in the ply-1 move list.                    *
c     *                                                                *
c     *    48c:  Cray Blitz now resigns when more than 5 pawns of      *
c     *          material behind.                                      *
c     *                                                                *
c     *    48d:  rewrite time allocation code to fix multiple          *
c     *          bugs that were created as this code was modified      *
c     *          over several years.  easy moves now work correctly.   *
c     *          when the code fails low, it used to stop the time     *
c     *          overflow on the first move that avoided the loss      *
c     *          of material.  now it trys to both save the material   *
c     *          and not commit positional suicide by searching for    *
c     *          better ways to save the material without destroying   *
c     *          it's position in the process.                         *
c     *                                                                *
c     *    48e:  modified the way scoredv is called and slightly       *
c     *          changed it's scoring.  locate now calls scoredv       *
c     *          to determine if development is complete.  this is     *
c     *          "discovered" by scoredv returning a zero score.       *
c     *          when this happens, a flag, "devdone" is set so that   *
c     *          scoredv is never called again.  scoredv now gives a   *
c     *          penalty if either center pawn hasn't moved as well    *
c     *          so that they will not be blocked by a piece.  since   *
c     *          scoredv detects a piece blocking a center pawn, we    *
c     *          need to make sure it can't happen before disabling    *
c     *          scoredv completely.                                   *
c     *                                                                *
c     *    48f:  changed piece values so that knight or bishop is      *
c     *          now worth 3.5 pawns.  this was done to discourage     *
c     *          trading two pieces for a rook plus a pawn, which      *
c     *          normally is not enough compensation.                  *
c     *                                                                *
c     *    48g:  changed king scoring to make the king do better than  *
c     *          simply advancing toward the center of the "pawn mass" *
c     *          with no regard for passed pawns, etc.                 *
c     *                                                                *
c     *    48h:  added code for blockading passed pawns.  the program  *
c     *          will try to blockade an isolated passed pawn with     *
c     *          any piece, but lower-valued pieces get higher scores. *
c     *          for protected passed pawns, it prefers bishops,       *
c     *          queen and king since a rook or knight can be driven   *
c     *          away by the supporting pawn.                          *
c     *                                                                *
c     *    48i:  modified phase5 to not include all checking moves     *
c     *          at the front of the search.  it now includes captures *
c     *          and pawn pushes to the 5th rank or further.  this now *
c     *          relies on the new ply-1 move ordering to get the good *
c     *          checks to the front of the move list.  fixed the      *
c     *          ponder move code to prevent overlaying a variation    *
c     *          with a variation of length one.  also, the hash       *
c     *          table is disabled for the first to plies of the       *
c     *          true-value search used to sort the ply-1 move list    *
c     *          so that *every* move will have a corresponding        *
c     *          predicted move attached to it.                        *
c     *                                                                *
c     *    48j:  modified LOCATE to set either of two flags, D4 and    *
c     *          E4 depending on the opening chosen by white.  D4      *
c     *          is used to avoid blocking the c-pawn, and also is     *
c     *          used to encourage it's advance.  also tweaked the     *
c     *          values used to encourage/discourage pawn advances.    *
c     *          C-B would often play b4 (which was o.k.) and then     *
c     *          push b5 to drive a knight away temporarily.  then,    *
c     *          the b5 pawn would be weak and have to be defended     *
c     *          tying the program up occasionally.  tightened up the  *
c     *          null-move code.  the null-move is terminated when     *
c     *          either side has fewer than 18 points of pieces so     *
c     *          that zugszwang won't cause problems.  increased the   *
c     *          penalty for isolated pawns, particularly those on     *
c     *          open files.                                           *
c     *                                                                *
c     *    48l:  rewrote the extend on capture code.  CB now counts    *
c     *          a capture / recapture as only one ply, in effect      *
c     *          extending the search by one ply so that a capture /   *
c     *          recapture sequence don't "monopolize" the principal   *
c     *          variation.  note that the capture / recapture must    *
c     *          be on the same square, and that the material balance  *
c     *          after the exchange must be the same as it was before  *
c     *          the first capture.                                    *
c     *                                                                *
c     *    48n:  fixed a long-term problem in the passed pawn race     *
c     *          code.  when we rewrote attack to use the bitboard,    *
c     *          this code was never fixed.  it still simply moved     *
c     *          pawns around on the board and expected attack() to    *
c     *          work correctly.  it didn't.                           *
c     *                                                                *
c     *    48p:  fixed an old problem in the king safety module where  *
c     *          the score it saves in the king safety hash depends    *
c     *          not only on king/pawn position but also on whether or *
c     *          not the king has given up the right to castle.  this  *
c     *          caused two identical "positions" to be scored         *
c     *          differently.  now, scorekm1 computes the true king/   *
c     *          pawn hash score, and scorekm2 adds in the loss of     *
c     *          castling privilege penalty independently of the       *
c     *          hashing.  this cured a lot of strange scores.         *
c     *                                                                *
c     *    48q:  various problems fixed relative to things like        *
c     *          annotating a game, printing the game history, etc.    *
c     *                                                                *
c     *    48r:  added analysis for weak pawns.  inserted code to make *
c     *          cray blitz want to attack pawns that are awkward to   *
c     *          defend.                                               *
c     *                                                                *
c     *    48s:  really cleaned scoring up and made values more        *
c     *          consistent with each other.                           *
c     *                                                                *
c     *    48t:  fixed another passed pawn bug dealing with rook       *
c     *          pawns.  this bug made the program produce incorrect   *
c     *          evaluations when one side had a passed rook pawn with *
c     *          its king in front of the passed rook pawn with no way *
c     *          to unblock it due to the opponent's king.  it now     *
c     *          produces the correct evaluation.                      *
c     *                                                                *
c     *    49a:  moved scoring values based on "static" knowledge to   *
c     *          "piece/square" tables to prevent evaluation overhead. *
c     *          this includes some new scoring ideas such as bad      *
c     *          squares for rooks (ie rook at a2 defending a pawn at  *
c     *          b2 is much worse than a rook at b1 defending the      *
c     *          pawn.  this will be further refined for other pieces  *
c     *          later this year.                                      *
c     *                                                                *
c     *    49b:  significant scoring changes for pawns.                *
c     *                                                                *
c     *    49c:  added the "one legal reply to check" quiescence       *
c     *          extension.  If a check in the quiescence is part of   *
c     *          a continuous series of quiescence checks, and there   *
c     *          only one legal reply, continue on without using the   *
c     *          normal quiescence limit of no more than two non-      *
c     *          capturing checks.                                     *
c     *                                                                *
c     *    49d:  fixed hashing of the one-legal-reply-to-check move    *
c     *          so that when the move comes from the transposition    *
c     *          table, it won't fail to extend as it would if it      *
c     *          was produced normally.  added the "in file [title]"   *
c     *          option to allow the creation of a library of test     *
c     *          positions in one file, by using a "title" line to     *
c     *          locate the proper problem within the file.            *
c     *                                                                *
c     *    49e:  added a new move selection module phase5() (the old   *
c     *          phase5() is now phase6()).  this new phase is only    *
c     *          used when the side to move is in check, and it tries  *
c     *          moves in a more "sane" order, moving the king or      *
c     *          "safe" interpositions.                                *
c     *                                                                *
c     *    49f:  removed 49e version phase6().  cleaned up some old    *
c     *          bugs as well.                                         *
c     *                                                                *
c     *    49g:  modified search so that if in check at ply=1, it does *
c     *          *not* extend the search, which simply made a 4 ply    *
c     *          search a 5 ply search without gaining anything.       *
c     *          the principal variation is now checked whenever it    *
c     *          is backed up to the root of the tree, and the hash    *
c     *          table is used to extend it if possible.  This both    *
c     *          provides more operator information as well as improve *
c     *          move ordering and pondering.                          *
c     *                                                                *
c     *    49h:  repaired a very old scoring bug in scorep2() which    *
c     *          would "create" pawns on the board under certain       *
c     *          circumstances.  the result was often lots of "bad     *
c     *          move hashed" error messages as well as more serious   *
c     *          problems.                                             *
c     *                                                                *
c     *    NOTICE:  this version of Cray Blitz is in a completely      *
c     *    unknown state.  It has been compiled and has played a few   *
c     *    games, but that is all that is known.  I can not tell what  *
c     *    changes were being made at the time of this 'snapshot'      *
c     *    which means that bugs might be lurking here and there.  It  *
c     *    appears that this version was an "in progress" work of      *
c     *    1991.  The backup date was simply listed as "June 1991" so  *
c     *    it is impossible to know if we were in the middle of        *
c     *    making changes or if this was fairly stable.  We generally  *
c     *    spent a lot of time in the Summer getting ready for the     *
c     *    annual ACM computer chess events, however, so the state of  *
c     *    this code is impossible to guess. multiprocessing does not  *
c     *    work as there is common that has to be local to each        *
c     *    process (called task common on the Cray) and I could not    *
c     *    find a portable solution.  pondering does not work as we    *
c     *    used a second thread to do the I/O and then "events" on the *
c     *    cray to inform the search that something had been typed     *
c     *    by the operator.  This might could be fixed, but it would   *
c     *    take some time.                                             *
c     *                                                                *
c     *    the book programs (book, books) work.  I have included a    *
c     *    sample bookin file.  to create a book file, you run bookp   *
c     *    and it will ask for the input file, the output file, and    *
c     *    the number of positions it will have (you can guess, but    *
c     *    if you guess low it will break, if you guess high it will   *
c     *    just waste a bit of disk space.)  blitz is somewhat like    *
c     *    crafty in that the book binary files should be named book   *
c     *    (this is a big book file), and bookw/bookb (which are       *
c     *    normally smaller and contain selected lines to play.  The   *
c     *    crafty-like e4! says "always play e4" while e4? would say   *
c     *    "never play e4".  Once you run bookp, you then run books    *
c     *    which minimaxes the scores for the book so that CB has some *
c     *    idea of which book line to follow...                        *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      real xeval
c
c
      include 'global.f'
c
c
      include 'common.f'
c
c
      equivalence (blank,alpha(70))
      equivalence (lbrack,alpha(73)), (rbrack,alpha(74))
      equivalence (alphag,alpha(33)), (alphao,alpha(41))
      equivalence (alphay,alpha(51))
      equivalence (alphae,alpha(31)), (alphan,alpha(40)),
     *            (alphad,alpha(30))
c
c
c------------------------------< initialize.
c
      call mpcopy
c     tdata(1)=setb60(loc(tdata(1)))
c     call lockasgn(lockht)
c     call lockasgn(lockmp)
c     call lockasgn(lockpt)
c     call lockasgn(locksd)
c     call evasgn(bfull)
      open (unit=8,file='bookw',status='OLD',
     *      access='DIRECT',form='UNFORMATTED',recl=800)
      open (unit=9,file='bookb',status='OLD',
     *      access='DIRECT',form='UNFORMATTED',recl=800)
      open (unit=10,file='book',status='OLD',
     *      access='DIRECT',form='UNFORMATTED',recl=800)
      mchtyp=tst124(0)
      if(mchtyp .eq. 1) print 101
101   format(1x,'running on a cray-1'/)
      if(mchtyp.gt.1 .and. mchtyp.le.4) print 102, mchtyp
102   format(1x,'running on a cray xmp ',i1,/)
      if(mchtyp.gt.4 .and. mchtyp.lt.7) print 103
103   format(1x,'running on a cray xmp-ea '/)
      if(mchtyp .ge. 7 .and. mchtyp.ne.90) print 104
104   format(1x,'running on a cray ymp '/)
      if(mchtyp .eq. 90) print 105
105   format(1x,'running on a cray-1'/)
      hsize=2**18
      hsizm1=hsize-1
      psize=ishft(hsize,-2)
      psizm1=psize-1
      ksize=ishft(hsize,-3)
      ksizm1=ksize-1
      htbl1=7168+hsize
      htbl2=htbl1*2
      ktbl1=htbl2+hsize+7168+1
      ktbl2=ktbl1+ksize
      ptbl1=ktbl2+ksize+1
      ptbl2=ptbl1+psize
c
c------------------------------< determine color desired and
c------------------------------< initialize data area
c
      print 300, version
300   format(1x,'Cray Blitz version ',a4/1x,'what is your name?')
      read 400, name
400   format(a20)
      print 500
500   format(1x,'are you continuing your last game?')
      call command(eof)
      contin=atext(1)  
      print 600
600   format(1x,'should I play white?')
      call command(eof)
      if(atext(1) .eq. alphay) color=1
      call setio(contin)
      call setup
      call setgb(1)
      if(contin .ne. alphay) then
          if(npmovs+nomovs .eq. 0) then
              call forsythe(text)
              write(unit=1,rec=1,fmt=650) text
650           format(80a1)
          endif
      else
          call restgm
      endif
      open (unit=11,status='old',file='default.scores',
     *      access='sequential', form='formatted',err=800)
      read(11,700) scoring
700   format(t45,i4)
      print 750
750   format(1x,'using default.scores')
      close (unit=11)
800   continue
      if(mchtyp .eq. 1) write(3,101)
      if(mchtyp.gt.1 .and. mchtyp.le.4) write(3,102) mchtyp
      if(mchtyp.gt.4 .and. mchtyp.lt.7) write(3,103)
      if(mchtyp .ge. 7) write(3,104)
      write(3,850) version
850   format(1x,'Sparc Blitz version ',a4/)
      write(3,900) name
900   format(1x,a20)
      atext(1)='a'
      atext(2)='i'
      do 950 i=3,80
          atext(i)=' '
950   continue
      if(name .ne. 'batch') call options(0)
c
c=====================================<
c=====================================<  m a i n   l o o p
c=====================================<
c
1000  continue
          call cptime(osec1,msec1)
          call query
c
c------------------------------< execute programmed commands
c
          if(over .eq. 0) then
              do 1200 i=1,10
                  if(prcmnds(1,i) .eq. blank) go to 1200
                      do 1100 j=1,5
                          atext(j)=prcmnds(j,i)
1100                  continue
                      do 1150 j=6,80
                          atext(j)=blank
1150                  continue
                      call options(0)
                      if(return .eq. 0) then
                          if(atext(1).eq.alphag .and. 
     *                       atext(2).eq.alphao)     go to 2700
                      endif
1200          continue
          endif
          go to 1500
c
c------------------------------< input the command or move.
c------------------------------< if tmode is set, call ponder to
c------------------------------< compute while waiting on the
c------------------------------< opponent to move.
c
1300      continue
1500      continue
              call savegm
              if(autos.eq.0 .and. tmode.ne.0) print 1900, name
              if(over .eq. 0) then
                  call ponder
                  if(return .eq. 1) go to 2200
                  if(return .eq. 2) go to 2300
              endif
1800          continue
              if(autos.ne.0 .or. tmode.eq.0) print 1900, name
1900          format(1x,'your move, ',a20)
              if(playslf .eq. 0) then
                  call command(eof)
                  if(autos .eq. 0) print 2002
2002              format(1x,'ok.')
              else
c                 call piperd(playfp(2),atext)
              endif
              if(eof .ne. 0) go to 1800
              if(unit .ne. 5) print 2100, (atext(i),i=1,30)
2100          format(1x,30a1)
              if(atext(1) .eq. blank) go to 1800
2200      continue
          call cptime(osec2,msec2)
          call options(0)
          if(return .ne. 0) go to 1500
          if(over .ne. 0) go to 1500
          if(atext(1).eq.alphag .and. atext(2).eq.alphao) go to 2700
          ply=2
          call input(1)
          if(return .ne. 0) go to 1300
c
c------------------------------< determine how much time was used
c------------------------------< by the opponent to chose his move
c------------------------------< and adjust his chess clock to
c------------------------------< reflect it.
c
2300      continue
          ply=2
          player=2
          side=-1
          otype=0
          ctemp=checkg(2)
          if(ctemp .ne. 0) otype=1
          call output(otype,1)
c
c------------------------------< remember his move for en passant
c------------------------------< capture analysis and then make
c------------------------------< it on the game board.
c
          prevmv(4)=mfrom
          prevmv(5)=mto
          prevmv(6)=mtype
          call pmover
          call reverse
          lmoveo=mfrom+ishft(mto,7)+ishft(mtype,14)
     *           +ishft(movgpc,20)+ishft(mcappc,17)
          if(color .eq. 2)
     *         print 2400, (npmovs+nomovs)/2+1, (text(l),l=1,10)
          if(color .eq. 1)
     *         print 2500, (npmovs+nomovs)/2+1, (text(l),l=1,10)
          if(color .eq. 2) write(3,2400)
     *         (npmovs+nomovs)/2+1,(text(l),l=1,10)
          if(color .eq. 1) write(3,2500)
     *         (npmovs+nomovs)/2+1,(text(l),l=1,10)
2400      format(1x,'your move',i3,'. ',10a1)
2500      format(1x,'your move',i3,'. ...  ',10a1)
          elapo=(osec2-osec1)/100
          nomovs=nomovs+1
          nmoves=npmovs+nomovs
          if((color.eq.1 .and. mod(nmoves,2).ne.0) .or.
     *       (color.eq.2 .and. mod(nmoves,2).eq.0)) nmoves=nmoves+1
          oelap=oelap+elapo
          call setclk
          write(unit=1,fmt=2501,rec=nmoves+1) (text(l),l=1,30),elapo,
     *                                 pelap,oelap,numout
2501      format(30a1,4i6)
          print 2600, hhmmss(elapo,5)
2600      format(17x,'clock time was ',a5)
c
c------------------------------< determine if a draw by repetition has
c------------------------------< occurred.
c
          point=point+1
          if(point .gt. 100) go to 5200
          ply=0
          call draw
          if(return.ne.0 .and. eval.le.900000) go to 5400
          if(dupchk(count) .eq. 3) go to 5000
c
c------------------------------< now call 'driver' to select the
c------------------------------< program's next move....from the
c------------------------------< book database or via the minimax
c------------------------------< tree search.
c
2700      continue
          call match
          if(return .eq. 0) then
              call savegb
              call driver
          endif
c
c------------------------------< determine how much time was used
c------------------------------< by Blitz to chose it's move and
c------------------------------< adjust it's chess clock to reflect
c------------------------------< it.
c
          call cptime(psec2,msec2)
          if(cputim .ne. 0) psec2=msec2
          elapp=(psec2-psec1)/100
c
c------------------------------< if doing a true-valued search,
c------------------------------< find the best move to make.
c
          if(trueval.ne.0 .and. inbook.eq.0) then
              max=1
              maxval=ponmvs(5,1)
              do 2800 i=1, last(1)
                  if(maxval .lt. ponmvs(5,i)) then
                      maxval=ponmvs(5,i)
                      max=i
                  endif
2800          continue
              do 2900 i=1,2
                  trace(i,1)=ponmvs(i,max)
2900          continue
              trace(51,1)=ponmvs(3,max)
              trace(52,1)=ponmvs(4,max)
              eval=ponmvs(5,max)
          endif
          if(and(trace(51,1),65535) .eq. 0) go to 4800
          ply=1
          player=1
          side=1
          mfrom=extrct(trace(1,1))
c
c------------------------------< shift the killer array up since the
c------------------------------< search will continue two plies deeper
c------------------------------< for the next program move.
c
          do 3400 j=1,48
              do 3300 i=1,4
                  killmv(j,i)=killmv(j+2,i)
3300          continue
3400      continue
c
c------------------------------< announce mate if one found
c
          if(eval .gt. 900000) then
              n=(1000000-eval)/2
              if(n .ne. 1) then
                  print 4100, bell
                  print 3500, n
                  write(3,3500) n
3500              format(/'     Cray Blitz will mate in 'i2,' moves.'/)
              endif
          endif
c
c------------------------------< output the move returned by
c------------------------------< 'driver'.
c
          side=1
          player=1
          ply=1
          otype=0
          ctemp=checkg(1)
          if(ctemp .ne. 0)  otype=1
          if(eval .eq. 999998) otype=2
          call output(otype,1)
          print 4100, bell
          if(color .eq. 1) print 3900, 
     *         (npmovs+nomovs)/2+1,(text(i),i=1,10)
          if(color .eq. 2) print 4000, 
     *         (npmovs+nomovs)/2+1,(text(i),i=1,10)
          if(color .eq. 1) write(3,3901)
     *         (npmovs+nomovs)/2+1,(text(i),i=1,10)
          if(color .eq. 2) write(3,4001)
     *         (npmovs+nomovs)/2+1,(text(i),i=1,10)
          if(eval.lt.900000 .and. nodes.gt.0) then
              if(eval-peval .gt. pieces(1)*1.75) print 4200
              if(peval-eval .gt. pieces(1)*1.75) print 4300
          endif
          peval=eval
          xeval=eval/1000.0
          if(autos .eq. 0) print 4400, xeval
3900      format(1x,'my move  ',i3,'. ',10a1)
3901      format(1x,'my move  ',i3,'. ',10a1)
4000      format(1x,'my move  ',i3,'. ...  ',10a1)
4001      format(1x,'my move  ',i3,'. ...  ',10a1)
4100      format(1x,a1)
4200      format(/1x,'be careful.'/)
4300      format(/1x,'I didn''t see that.'/)
4400      format(/1x,'eval',f8.1)
c
c------------------------------< remember the program's move for
c------------------------------< en passant capture analysis and
c------------------------------< then make it on the game board.
c
          prevmv(1)=mfrom
          prevmv(2)=mto
          prevmv(3)=mtype
          call pmover
          call reverse
          lmovep=trace(1,1)
          npmovs=npmovs+1
          nmoves=npmovs+nomovs
          if((color.eq.1 .and. mod(nmoves,2).eq.0) .or.
     *       (color.eq.2 .and. mod(nmoves,2).ne.0)) nmoves=nmoves+1
          pelap=pelap+elapp
          matchd=0
          call setclk
          write(unit=1,fmt=2501,rec=nmoves+1)(text(l),l=1,30),elapp,
     *                            pelap,oelap,numout
          print 2600, hhmmss(elapp,5)
c
c------------------------------< remember the second move in the
c------------------------------< principle variation (if there is
c------------------------------< one) for pondering.
c
          pmove=trace(2,1)
          if(and(trace(51,1),65535) .lt. 2) pmove=0
c
c------------------------------< if the predicted move is zero, 
c------------------------------< then "borrow" one from the list
c------------------------------< of saved variations.
c
          if(pmove .eq. 0) then
              do 4500 i=1,200
                  if(ponmvs(1,i) .eq. trace(1,1)) pmove=ponmvs(2,i)
4500          continue
              ply=2
              player=2
              side=-1
              first(2)=1000
              rdepth=100
              call movgen
              do 4550 i=first(2),last(2)
                  mfrom=extrct(moves(i))
                  call make
                  illegal=checki(2)
                  call unmake
                  if(illegal .ne. 0) go to 4550
                  if(pmove .eq. moves(i)) go to 4580
4550          continue
              do 4570 nmove=1,20
                  pmove=ponmvs(2,nmove)
                  do 4560 i=first(2),last(2)
                      mfrom=extrct(moves(i))
                      call make
                      illegal=checki(2)
                      call unmake
                      if(illegal .ne. 0) go to 4560
                      if(pmove .eq. moves(i)) go to 4580
4560              continue
4570          continue
                  pmove=0
4580          continue
          endif
c
c------------------------------< determine if a draw by repetition or
c------------------------------< a draw due to insufficient material
c------------------------------< has occurred.  
c
          if(eval .eq. 999998) go to 5800
          point=point+1
          ply=0
          if(eval .le. 900000) then
              if(point .gt. 100) go to 5200
              call draw
              if(return .ne. 0) go to 5400
              if(dupchk(count) .eq. 3) go to 5000
          endif
c
c------------------------------< store predicted move, trace, and other
c------------------------------< useful info for statistical purposes.
c------------------------------< then adjust them so that they will be
c------------------------------< useful in the next search (or possibly
c------------------------------< while thinking on the opponent's time)
c
          do 4600 i=1,48
              trace(i,1)=trace(i+2,1)
4600      continue
          stemp=and(trace(51,1),65536)
          atemp=and(trace(51,1),65535)
          trace(51,1)=stemp+max0(atemp-2,0)
          trace(52,1)=max0(trace(52,1)-2,0)
          foundm=0
          if(matem .ne. 0) then
              vterm=vterm+2
              maxply=maxply-2
          endif
c
c------------------------------< determine if it is time for Cray
c------------------------------< Blitz to resign.  the rule is simple:
c------------------------------< if more than 5 pawns down, the
c------------------------------< program will resign.
c
          if(eval.le.-5000 .and. trueval.eq.0) 
     *                              print 4700, beep, name, beep
4700      format(/1x,a1,'I resign, ',a20,a1/)
      go to 1000
c
c------------------------------< game is over, set the 'over'
c------------------------------< indicator to inhibit any more
c------------------------------< moves and return to the command
c------------------------------< input loop for cleanup and exit.
c
4800  continue
          if(attack(-1,kloc(1)) .eq. 0) go to 5600
          print 4900, name
          write(3,4900) name
4900      format(1x,'checkmate ',a20)
          go to 5800
5000  continue
          rmoves(1)=(npmovs+nomovs)/2+1-(point-rmoves(1))/2
          rmoves(2)=(npmovs+nomovs)/2+1-(point-rmoves(2))/2
          print 5100, name, rmoves
          write(3,5100) name, rmoves
5100      format(/1x,'I claim a draw by repetition, ',a20/
     *    1x,'this position repeats the ones at moves',i3,' and',i3/)
          go to 5800
5200  continue
          print 5300
          write(3,5300)
5300      format(1x,'the game is a draw due to the 50 move rule.')
          go to 5800
5400  continue
          print 5500, bell, bell, bell
          write(3,5500)
5500      format(/1x,'the game is a draw due to insufficient material'/
     *    1x,'by either side to force checkmate',3(a1,15(' '))/)
          go to 5800
5600  continue
          print 5700, name
          write(3,5700) name
5700      format(1x,'the game is a stalemate, ',a20)
5800      continue
          over=1
          go to 1000
      end
