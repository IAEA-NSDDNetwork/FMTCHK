!***********************************************************************
!                                                                      *
!  PROGRAM:          FMTCHK                                            *
!                                                                      *
!  PURPOSE:          THIS PROGRAM WILL READ ENSDF DATA SET FILES FROM  *
!                    FORTRAN LOGICAL UNIT 21.                          *
!                                                                      *
!                    IT WILL ANALYZE THE FORMAT OF THE DATA CARDS IN   *
!                    THE DATA SET FILE TO VERIFY THAT THEY CONFORM TO  *
!                    "EVALUATED NUCLEAR STRUCTURE DATA FILE. "A MANUAL *
!                    FOR PREPARATION OF DATA SETS" BY J.K.TULI         *
!                    (BNL-NCS-52655 - Rev. 87, April 1987) and         *
!                    subsequent memos                                  *
!                                                                      *
!                    A REPORT OF ALL FORMAT ERRORS FOUND WILL BE PRO-  *
!                    DUCED ON FORTRAN LOGICAL UNIT 22.                 *
!                                                                      *
!                    THIS REPORT WILL CONTAIN A LIST OF THE DATA SET   *
!                    CARDS, FLAG LINES TO INDICATE THE FIELDS IN ERROR,*
!                    AND DESCRIPTIVE TEXT EXPLAINING THE ERROR.        *
!                                                                      *
!                                                                      *
!    Version 10.0 : February 19, 2004   C.L.Dunford                    *
!                   Converted to Fortran 95                            *
!                   Command line input added                           *
!    Version 10.1 : May 5, 2006 T.W. Burrows                           *
!                    1. Added coding to recognize new quantities output*
!                      BrIcc                                           *
!                    2. Corrected errors in checking decay branching   *
!                      quantities                                      *
!                    3. Corrected problems caused by different ways of *
!                      ordering multiple PARENT and NORMALIZATION      *
!                      records                                         *
!                    4. Corrected error in calculating asterisk        *
!                      position* for ID continuation records           *
!                    5. Changed following from error to warning        *
!                      message:                                        *
!                      UNBALANCED BRACKETS                             *
!                    6. Changed following from fatal to error message: *
!                      NUCID MISMATCH WITH DSID PARENT                 *    
!                    7. Relaxed criteria for comparing final level to  *
!                      calculated final level                          *
!                    8. Left blank suppress input file name
!                   Implemented the following on an error report from  *
!                     PNPI in May 2005                                 *
!                    9. Check residual calculated from target and      *
!                      reaction                                        *
!                   10. Report uncertainties identical to "0"          *
!                   11. Error if parent level energy is zero and IT    *
!                      decay                                           *
!                   12. Report error if level of form A+X where A is   *
!                      not zero and no preceding X level               *
!                   13. More checks on asymmetric uncertainties to     *
!                      catch cases like "+-4"                          *
!                   14. Error if multiple "PN" records                 *
!                   15. Corrected problem in checking prompt particle  *
!                      comment records                                 *
!                   16. Implemented checking of "D" comment records    *
!                   17. Check to ensure "PN" record precedes first data*
!                      record                                          *
!                   18. Check for multiple unplaced gammas of the same *
!                      energy                                          *
!                   19. Check for non-blank DE if energy completely    *
!                      non-numeric                                     *
!                   20. Check for negative energies. Warning if level; *
!                      error, otherwise                                *
!    Version 10.1a : May 12, 2006 T.W. Burrows                         *
!                    1. Corrected some parsing problems in GETOUT      *
!    Version 10.1b : May 18, 2006 T.W. Burrows                         *
!                    1. Corrected bug in GETOUT which caused a         *
!                      segmentation fault under LF 95                  *
!    Version 10.1c : May 23, 2006 T.W. Burrows                         *
!                    1. Removed extraneous STOP in Getout left from    *
!                      debugging                                       *
!                    2. Corrected parsing problems in Getout which     *
!                      resulted in confusion between particle          *
!                      admixtures and nuclides                         *
!                    3. Relaxed test of residual vs nuclide when IAR   *
!                    4. Increased estimate of gamma uncertainty from 2 *
!                      to 5 if one significant digit past decimal point*
!                      given                                           *
!                    5. Corrected problem in assigning numeric value   *
!                      for current level introduced in the conversion  *
!                      from F77 to F95                                 *
!                    6. Corrected erroneous error messages for DSID's  *
!                      with continuations                              *
!                    7. Added check to see if FL= was identical to     *
!                      current level energy                            *
!    Version 10.1d : June 21, 2006 T.W. Burrows                        *
!                    1. Increased array size for gamma final level     *
!                      check                                           *
!    Version 10.1e : August 1, 2006 T.W. Burrows                       *
!                    1. Added check for KAPPA's as incident and        *
!                      outgoing                                        *
!    Version 10.1f : October 21, 2006 T.W. Burrows                     *
!                    1. Attempted to handle cases where other types of *
!                      resonances might have been studied. If so, issue*
!                      a warning instead of an error message for       *
!                      target, reaction, NUCID mismatches              *
!    Version 10.1g : March 5, 2007 T.W. Burrows
!                    1. Corrected problem in GetOut which caused an
!                      infinite loop
!                    2. Corrected problem in checking Parent Comment
!                      records when multiple SF parents on DSID	     
!    Version 10.1h : April 16, 2007 T.W. Burrows
!                    1. Corrected problem in GetOut which caused an
!                      infinite loop
!    Version 10.2  : May 3, 2007  T.W. Burrows
!                    1. Changed from error to warning trailing "subroutine chkt?" for
!                      BElW and BMlW on level continuation records
!                    2. Added check for embedded "+" or "-" on outgoing
!                      reactions. This solved an infinite loop in GetOut
!                    3. Cleaned up coding on checking reactions to reduce
!                      extranous error messages and improve checks for
!                      compound nucleus as nuclide being studied
!                    4. Allow "%CEK*" on gamma continuation records
!                    5. Allow XREF=A(123?) and XREF=A(?) on level
!                      continuation records
!                    6. Corrected erroneous "NO FINAL LVL" messages when
!                      non-numeric gamma energy given
!                    7. Corrected erroneous "EL-EG" messages when parent
!                      level had "SP" or "SN" in energy field
!    Version 10.3  : July 30, 2007 T.W. Burrows                     
!                    1.  Added a check for multiple "FL="'s
!                    2.  Corrected logic problem which was causing the
!                       Level record S field not to be checked when it
!                       was relabeled
!                    3.  Added check for possible typo error if a period
!                       is in the last field of a value and a number is
!                       in firsf field of the uncertainty
!                    4.  Attempt to handle cases where level energy
!                       should probably be "E+X+Y" which is not currently
!                       allowed
!                    5.  Added check for extraneous characters (usually
!                       uncetainties) following a final level energy
!                    6.  Added various additional checks for embedded
!                       blanks
!                    7.  Added check for possible misuse of an operator                                                                      *
!                       before parentheses in the J field
!                    8.  Added various additional checks for junk
!                       before open parentheses. Usually misalignment
!                       of the E and DE fields on a level record
!                    9.  Check for use of period instead of comma
!                    10. Added check for missing MS field if
!                       T1/2>=0.1 ms and not g.s.
!                    11. Cleared up problems in getting closest levels
!                       if "X+E" were used instead of "E+X"
!    Version 10.3a : September 28, 2007 (Thomas W. Burrows)
!                    1. Corrected error in version 10.3 which caused an
!                      erroneous "EMBEDDED BLANK INVALID" when an
!                      operator was preceded by a "(" for a JPI value.
!                    2. Changed metastable limit check from 0.1 ms to
!                      1 ms.
!    Version 10.3b : February 5, 2008 (Thomas W. Burrows)
!                    Added check for more than one comma in a reaction
!
!    Version 10.3c : February 27, 2008 (Thomas W. Burrows)
!                    1. Allow History records in Comments dataset
!                    2. Check for unmatched curly brackets on "T" or
!                       "t" records. Also, if more than one issue error
!                       instead of warning
!    Version 10.3d : October 07, 2011 (T.D. Johnson)
!                    1. Extended the maximum number of gammas to 2000
!                       from 800
!    Version 10.3e : November 2, 2015 (J.K. Tuli)
!                    1. Commented out error message for MR
!                    2. Commented out Y2K compliance warning
!                    3. Stopped check for Sign for QP on P record
!                    4. Suppressed comment when "Label" encountered
!                    5. Suppress message T, L relabelled
!                    Some nonstandard code removed zv (Viktor Zerkin)
!                    December 15, 2015 (J.K. Tuli)
!                    Removed check for NP to be equal to BR on N card
!    Version 10.3f   Fixed the stop check for sign on QP record (TDJ)
!                    Changed this to a warning, just in case
!    Version 10.3g   Added support for HALF-LIFE datasets (TDJ)
!    Version 10.3h : October 20, 2016. Added check to make sure levels
!                    do not exceed available decay energy in
!                    decay datasets. (TDJ) 
!    Version 10.3i : April 21, 2017. Added SEQ field name similar to 
!                    bands (TDJ)
!    Version 10.3j : May 1, 2017, Fixed issue in IT datasets where
!                    error messages were incorrectly generated for
!                    exceeding energy balance.
!    Version 10.3k : May 4, 2017, Fixed issue in SF datasets where
!                    error messages were incorrectly generated for
!                    exceeding energy balance. (TDJ)
!    Version 10.4  : May 10, 2017, Check for mass numbers in 
!                    columns 1-3 and element in 4-5.  (TDJ)
!                    
!    Version 10.4a : July 19, 2017, Allow SN-number+X for levels above
!                    neutron sep. energy for beta decay (TDJ)
!
!    Version 10.4b : Aug 04, 2017, Allow POL field in gamma
!                     continuation record (TDJ)
!                    
!    Version 10.4c : 1. Sep 19, 2017, Fix issue where level energies had
!                     an offset in SF decay (TDJ)
!                    2. Pass array element instead of array to 
!                       LBSUP as Intel compiler was complaining.
!                       Pointed out by T. Kibedi. (TDJ)
!
!    Version 10.4d   1. Oct. 18, 2018m Fix another parent offset issue.    
!  
!    Version 10.5d   1. Nov. 29, 2018. Added lower case check to dsid                  
!    
!    Requires NNDCLIB (NSDFLIB95) subroutine library                   *
!
!
!                                                                      *
!  REVISION HISTORY:                                                   *
!                                                                      *
!  1(  0) 30-SEP-83. FIRST RELEASE (FORTRAN-77 STANDARD).              *
!  1(  1) 12-AUG-83. FLAGGED COMMENTS MUST PRECEDE L, G, A, B, E CARDS.*
!  1(  2) 12-AUG-83. CHECK FOR JUNK AFTER SYM(FLAG).                   *
!  1(  3) 29-NOV-83. DSID'S MUST NOT CONTAIN ANY EXTRA BLANKS (I.E.,   *
!                    NO LEADING BLANKS AND ONLY ONE BLANK BETWEEN      *
!                    WORDS OR SYMBOLS).                                *
!  1(  4) 12-DEC-83. REPORT ERROR IF JUNK AT END OF UNCERTAINTY FIELD. *
!  1(  5) 29-DEC-83. FIX RATIOS VALID FOR CONT FIELD NAMES.            *
!  1(  6)  6-JAN-84. CHECK CONT.,COMM./PRIMARY CARD CONFLICT.          *
!  1(  7)  6-JAN-84. BLANK CARD WITH ONLY NUCID CAUSED ABORT, FIX TO   *
!                    REPORT ERROR AND CONTINUE.                        *
!  1( 10) 14-FEB-84. FLAG RATIO AS FIELD NAME WITH NO FINAL FIELD AS   *
!                    ERROR ON CONTINUATION CARD.                       *
!  1( 11) 11-MAY-84. CALL TO CHKEND SHOULD HAVE ARGUMENT (CARD).       *
!  1( 12) 20-JUN-84. FIX HANDLING OF MULTIPLE HALF-LIVES IN DSID.      *
!  2( 13) 20-JUN-84. NEW FORMAT CHANGES...                             *
!                    ACCEPT (E FIELD) S<N/P>+<NUM/A>.                  *
!  2( 14) 22-JUN-84. G CARD/C FIELD, ALLOW '&' AS WELL AS '*'.         *
!  2( 15) 10-JUL-84. NEW 2G FIELD, FL. ADD CHECKS AS NEEDED.           *
!  2( 16) 16-JUL-84. MODIFIED COMMENT CARD FORMAT.                     *
!  2( 17) 18-JUL-84. ALLOW CA/SY AS UNCERT ON CONTIN CARDS.            *
!  2( 20) 20-JUL-84. WIDTH? FIELDS HAVE UNITS.                         *
!  2( 21) 23-JUL-84. REWORK L FIELD CHECKS.                            *
!  2( 22) 25-JUL-84. EXTEND SYMBOL CHECKING.                           *
!  2( 23) 11-SEP-84. CORRECT FOR NEW F77STR EXTENSIONS.                *
!  2( 24) 11-SEP-84. CHECK COL. 9 = BLANK WHERE IT SHOULD BE.          *
!  2( 25) 13-SEP-84. CALL CHKCNT IN FMTCHK, WRONG NUMBER ARGUMENTS.    *
!  2( 26) 13-SEP-84. CKSYM, REORDER REPSTR CALLS FOR EXTRA G SYMS.     *
!  2( 27) 13-SEP-84. CKCFLD, TOO MUCH CHECKING FOR RATIOS.             *
!  3( 30) 14-SEP-84. ADD CHECKING FOR CONF, FLAG, AND XREF CONTINUATION*
!                    FIELDS.                                           *
!  3( 31) 18-SEP-84. CKCFLD(RATIOS) LOOK FOR UNCERTAINTIES.            *
!  3( 32) 20-SEP-84. EXTEND XREF CHECKING.                             *
!  3( 33) 20-SEP-84. CKREF, BREAK .AND.ED IF INTO TWO PARTS.           *
!  3( 34) 20-SEP-84. CKE, CKXREF, FIX STRING RANGES EXCEEDED.          *
!  3( 35) 20-SEP-84. CKT, REWORK UNITS CHECKING.                       *
!  3( 36) 21-SEP-84. CKCOP, REWORK.                                    *
!  3( 37) 21-SEP-84. MISC. CLEANUP.                                    *
!  3( 40) 21-SEP-84. ADD CHKX TO CHECK XREF CARDS.                     *
!  3( 41) 24-SEP-84. ADD NP, DNP TO N-CARD.                            *
!  3( 42) 25-SEP-84. CHKCMG, VERIFY RTYPE = PRTYPE. CLEAN UP CRDTYP.   *
!  3( 43) 22-OCT-84. CKT, CHECK FOR MISSING UNITS.                     *
!  3( 44)  8-MAR-85. ALLOW ? IN ORG FIELD OF G CARD.                   *
!  3( 45) 11-MAR-85. ALLOW ? (LIKE C) IN COL. 77 OF A, B, AND E CARDS. *
!  3( 46) 11-MAR-85. IGNORE D TYPE COMMENTS.                           *
!  3( 47) 11-MAR-85. ALLOW T TYPE COMMENTS AS GENERAL OR RECORD TYPE.  *
!  3( 50) 11-MAR-85. TREAT LOWER CASE c AND t AS UPPER CASE.           *
!  3( 51) 11-MAR-85. ADD O TO D AND Q AS VALID FORMS FOR M FIELD.      *
!  3( 52) 11-MAR-85. PRINT "NUM LVLS EXCEEDED" MSG ONLY ONCE.          *
!  3( 53) 13-MAR-85. REMOVE PROCESSING OF L CARD S FIELD.              *
!  3( 54) 13-MAR-85. ALLOW SIGN IN FRONT OF VALUE FOR ISPINZ.          *
!  3( 55) 13-MAR-85. ALLOW PARENTHESES AROUND CONTINUATION VALUES.     *
!  3( 56) 13-MAR-85. ALLOW ? AS VALUE FOR % FIELDS ON CONTINUATIONS.   *
!  3( 57) 13-MAR-85. REWORK USE OF * IN XREF FIELD.                    *
!  4( 60) 13-MAR-85. ADD DELAYED PARTICLE CARD PROCESSING.             *
!  4( 61) 14-MAR-85. RENAME CKOE TO CKCOIN AND CLEAN UP CALLS.         *
!  4( 62) 14-MAR-85. WRITE CKBLNK AND CALL WHERE NEEDED.               *
!  4( 63) 14-MAR-85. ADD ENTRY POINT CKVPS (SIGN & PARENS).            *
!  4( 64) 14-MAR-85. USE CKV FOR D:T NOT CKT.                          *
!  4( 65) 14-MAR-85. ADD D CARD SYMBOLS TO CKSYM.                      *
!  4( 66) 15-MAR-85. MAKE ERRORS ONLY DEFAULT INSTEAD OF FULL.         *
!  4( 67) 15-MAR-85. D CARD COMM. AND CONT. HAVE PARTICLE IN COL. 9.   *
!  4( 70) 15-MAR-85. ADD D CARD TO FLAG PROCESSING.                    *
!  4( 71) 15-MAR-85. CHECK EOF W/O END CARD.                           *
!  4( 72) 15-MAR-85. ALLOW GEV AS ENERGY UNIT ON DSID.                 *
!  4( 73) 19-MAR-85. CHKX DIED ON FIRST SYMBOL FOR AZ.                 *
!  4( 74) 19-MAR-85. ADD CHECK FOR MISSING XREF/FLAG VALUE.            *
!  4( 75) 20-MAR-85. REWORK REACTION DSID CHECKING.                    *
!  4( 76) 21-MAR-85. TELL USER THE DATA SET WE ARE PROCESSING.         *
!  4( 77) 21-MAR-85. ALLOW PARENS ON G:MR FIELD.                       *
!  4(100) 21-MAR-85. ALLOW PARENS ON SPINZ FIELD.                      *
!  4(101)  5-APR-85. ALLOW PARENS ON SECOND CONTIN. VALUE.             *
!  4(102)  5-APR-85. ADD COMTYP, CONTYP TO /CRDCOM/ COMMON BLOCK.      *
!  4(103)  5-APR-85. CONTINUATION INVALID FOR I, N, P, AND Q CARDS.    *
!  4(104)  9-APR-85. REWORK CKSYM FOR SPEC CHANGES.                    *
!  4(105) 12-APR-85. ADD VAX SETMDC CODE.                              *
!  4(106) 15-APR-85. IF EXTRA BLANK CARDS, DON'T DO EXTRA CHECKS.      *
!  4(107) 15-APR-85. CKCFLD: ALLOW (VAL) UNC AS WELL AS (VAL UNC).     *
!  4(110)  3-JUN-85. G CARD, C FIELD: ACCEPT BLANK OR C.               *
!  5(111) 26-JUN-85. MULTIPLE PATCHES, MOSTLY PARTICLE RECORDS.        *
!  5(112) 26-JUN-85. REMOVE CHECKING OF ALL R TYPE CARDS.              *
!  5(113) 22-JUN-84. G CARD/C FIELD, ALLOW '@' AS WELL AS '*' AND '&'  *
!  6(114)  3-APR-87. G CARD/C FIELD, CORRECTLY ALLOW '@' AS WELL AS '*'*
!                    AND '&'. '[' AND ']' ALLOWED IN J, L, AND M FIELDS*
!  6(115)  2-NOV-87. VAX MDC READONLY added to OPEN input files        *
!  7(116)  6-Feb-89. Add PN record, delayed particle check, id card con*
!                    etc. Indicate error or warning messages.          *
!  7(117) 24-FEB-89. Check end record blank, End rec. before I record  *
!  7(118) 26-OCT-89. Check reaction set id's extra blank, >80char check*
!  7(119) 30-OCT-89. Check special chars, check repeated SYM           *
!  7(120) 14-DEC-89. Check unbalanced brackets in comments, trailing nul
!  7(121) 16-Jul-91. Level limit up to 1000. Check Pub and U commants. *
!            DSID continuation cards check. Mod CHKI and created CKDSID*
!            XREF rec check mod. etc.                                  *
!  7(122) 22-Aug-91. Continuation Gamma record dta type check mod.     *
!  7(123)  6-May-92. Modified reaction 2nd field check.                *
!  7(124) 15-Oct-92. Added MDC coding for ANS.                         *
!  7(125)  1-Dec-92. 1) Changed check on Final Level from floating     *
!                       to character to allow for check on "+X" formats*
!                       Also stored offset for "X",... to use in check *
!                       on order of level energies                     *
!                    2) Added check on order of level energies         *
!                       Error if E(n)<E(n-1)                           *
!                       Warning if E(n)=E(n-1)                         *
!                    3) Added check on valid DSID for X card           *
!                    4) Included multipolarities in check on L field   *
!                       and corrected logic error which produced       *
!                       erroneous error messages                       *
!                    5) Added subroutine CKNUCID to check NUCID and    *
!                       consistency with ID record NUCID               *
!                    6) Replaced "(:n)" with "(1:n)" due to known      *
!                       problems with some compilers                   *
!                    7) Corrected some logic errors in CKREA           *
!                    8) Corrected subscript overflow problems in       *
!                       several subroutines                            *
!                    9) Finished typing of variables and functions     *
!  8.0    23-Mar-93. 1) Corrected output overflow errors               *
!                    2) Corrected misspellings in OPEN statements for  *
!                       CDC and IBM                                    *
!                    3) Added warning for obsolete IF formalism in M   *
!                       field                                          *
!                    4) Added summary of messages to terminal output   *
!                    5) Added more syntax checking for N and PN records*
!                    6) Removed "CHECK CC FOR ..." as per JKT          *
!                    7) Replaced string concatanation with ADDSTR due  *
!                       to problems with some FORTRAN's                *
!                    8) Added cross checks between intensities found   *
!                       and contents of N record                       *
!                    9) Added checks of PUB and DATE fields of ID      *
!                       record and corrected check on DREF to allow for*
!                       only three references                          *
!                   10) Updated LEVEL continuation record checking to  *
!                       allow for new formalisms                       *
!                   11) Updated DSID checking to allow for heavy-ion   *
!                       decay and multiparticle decay                  *
!                   12) Corrected logic error in DSID continuation     *
!                       resulted in error flags in wrong columns       *
!                   13) Added checks for reserved characters in flagged*
!                       comments                                       *
!                   14) Added more checks on GAMMA coincidence field   *
!                   15) Changed subprogram names to six characters for *
!                       ANSI compatibility                             *
!                   16) Added subroutine WRTMS2 to flag errors in two  *
!                       related fields                                 *
!                   17) Changed RECCHK to LOGICAL FUNCTION and reworked*
!                       logic so that record and flag positions may be *
!                       output                                         *
!                   18) Modified CKCOP to return if field is a limit   *
!                       and type of limit                              *
!                   19) Allowed for ranges with uncertainties          *
!                   20) Added checks between COMMENT record type and   *
!                       type of data set                               *
!                   21) Allow use of "J" and "J+1" in J field of LEVEL
!                       record
!  8.1    11-May-93. 1) Corrected logic error in checking DSID
!                       continuations.
!                    2) Finished implementation of U comment as per
!                       10-May-93 memo of JKT.
!  8.1a   13-May-93. Corrected error in CKDSID which caused possible
!                      fatal error when no target given
!  8.1b   03-Aug-93. Rearranged order of variables in commons OFFSET
!                      and SAVEID to avoid misalignment problems in
!                      ALPHA FORTRAN
!  8.1c   31-Aug-93. 1) Added check for embedded blanks following E=
!                      on DSID and XREF records.
!                    2) Changed message on multiple PARENT records from
!                      error to warning.
!  8.2    19-Nov-93  1) Added check for unrealistic exponents.
!                    2) Added check for position of "2 G" with "FL="
!                    3) Added cross checks between optional data in
!                       parentheses in DSID of IDENTIFICATION record
!                       and PARENT record and possible illogical
!                       existence of NORMALIZATION record for multiple
!                       parents
!                    4) Added consistency check between "*", "@", and
!                       "&" in column 77 of GAMMA record and RI and TI
!                    5) Added cross checks on "*","@", and "&"
!                    6) Extended relationals allowed in L field of
!                       LEVEL record
!                    7) Minor program cleanup.
!  8.3     6-Jun-94  1) Check for unrealistic exponents now field and
!                       record dependent
!                    2) Added option to suppress warning messages
!                    3) Removed check on position of continuation record
!                       with "FL="
!                    4) Changed from error to warning
!                       "UNDEFINED CHARACTER ASSIGNED"
!                    5) Increased to 26 number of characters allowed
!                       for "+X" formalism in E(level)
!                    6) Added check on total feeding for B, EC, and
!                       A decay data sets
!                    7) Added checks for ambiguity in final level fed,
!                       multiple gammas of same energy from same level,
!                       and order of gammas
!                    8) Added consistency check between adopted DSID
!                       and presence of gammas in data set
!                    9) Differented between signed and unsigned values
!                       on continuation records
!                   10) Corrected logic error for "J+n" check
!                   11) Extended definitions of particle decay modes
!                   12) Corrected confusion between uncertain ISPIN
!                       and keynumbers
!                   13) Added check for missing MULT if MR is given
!                   14) Expanded definitions of legal decay types for
!                       DSID
!                   15) Allow delayed-particle records for modes such
!                       as B-2N (As per JKT on 2-June-94)
!                   16) Cross check NUCID in DSID and PARENT NUCID
!                   17) Added check for proper form of and existence
!                       of first COMMENT record
!  8.3a   06-Oct-94 1) Corrected error in parsing J on LEVEL
!                      continuation record
!                   2) Allow new definition of PUB for prepublication.
!                      Allow YYNP in PUB field
!  8.3b   11-Oct-94 1) Corrected errors reported by MS FORTRAN 5.0
!                      compiler.
!  8.4    17-Mar-95 1)  New reaction DSID - HIGH-SPIN LEVELS, GAMMAS
!                   2)  X card allowed in new data set
!                   3)  New record type, "S". S and X mutually exclusive
!                   4)  GAMMA may have symbol in col. 9 for new data set
!                       As a corollary so may GAMMA continuation and
!                       comments
!                   5)  "%" flag for GAMMA now has a special meaning
!                   6)  Was not reporting illegal continuation for
!                       several record types
!                   7)  Hadn't anticipated all the ways evaluators use
!                       "+X" formalism on levels
!                   8)  Erroneous error messages on GAMMAs when levels
!                       are out of order
!                   9)  Check for non-numeric EG's and attempt to
!                       associate with non-numeric E(level)'s
!                   10) Added informational message on offset assumed
!                       for non-numeric E(level)'s
!                   11) Not catching mispunch in col 9 of general
!                       comments
!                   12) Changed flag to 8 and 9 from 8 for CONT./PRIMARY
!                   13) Expanded "XYX" in CKE for LEVEL and GAMMA
!                   14) Allow "J AP" in J field
!                   15) Added second numeric comparison for multiply-
!                       placed gammas
!                   16) Added check on missing branching ratios for
!                       non-stable g.s. and metastable states in
!                       adopted data sets
!                   17) Improved logic on warning of non-numeric level
!                       characters being out of order
!                   18) Corrected logic problem on INVALID HALF-LIFE
!                       on DSID when non-numeric uncertainty
!                   19) Was not checking col. 77 of PN vs existence or
!                       non-existence of "2PN"
!                   20) For check on equal E(level), include J and T
!                       fields (as per JKT)
!                   21) Allow D, Q, and C# and nested () in L field
!                   22) Program not branching to CKL for L= on CONT
!  8.4a   21-Apr-95 Corrected compilation errors reported by MS FORTRAN
!                     5.0
!  8.4b   12-Oct-95 Corrected range error problem in CKJ
!  8.4c   19-Oct-95 Added check for FLAG on COMMENT records where no
!                     FLAG allowed - also corrected a subscript range
!                     error by doing this
!  8.5    28-Feb-96 1) Store J, XREF, and band information and output
!                     with ambiguous final level messages
!                   2) Now that XREF's saved add another check on FL=
!                   3) Tried to make messages on missing final levels
!                      more informative and added check on out of
!                      order levels
!                   4) Corrected confusion on gammas from multiple
!                     sources in HIGH-SPIN LEVELS, GAMMAS
!                   5) Corrected logic error in storing level
!                     information when MAXLVL exceeded
!                   6) Cleaned up output formating a little
!                   7) CKCFLD bombed out or stored data wrong when
!                     continuation/primary card conflict - Fixed
!                   8) Corrected logic and typo errors in CHKCNT
!                     dealing with continuation/primary conflicts
!                   9) Added check for levels with missing XREF=
!  8.6    29-Aug-96 1) Corrected range error in common RECNAM
!                     introduced with the addition of "S" record
!                   2) Added additional checks on DSID after problems
!                     with evaluations from inexperienced ENSDF
!                     evaluators
!                   3) Added checks for new HISTORY record (evaluation
!                     date no longer valid as per JKT)
!                   4) Added more consistency checks between MULT and
!                     MR fields on GAMMA record (based on report from
!                     FCONV)
!                   5) Corrected logic error on missing XREF= check and
!                     added warning or error on reserved symbols for X
!                     record
!                   6) Assume DE=1 in least significant digit of level
!                     and gamma energies for FL checks
!                   7) Check for multiple gammas from one level feeding
!                     same final level
!                   8) Added global cross check between X records in
!                     adopted and ID records (Generic problem reported
!                     in TofI production)
!                   9) Check for defined qualifier after ":" in DSID
!                     and issue warning if found
!                  10) Error if symbol assigned to more than one band.
!                     Also corrected typo error in storage of band
!                     symbols.
!  8.6a   23-Oct-96 Corrected compilation errors found by J. Blachot
!                     using Microsoft Power Fortran
!  8.6b   24-Oct-96 Deleted extraneous DATA statement left in 23-Oct-96
!                     correction
!  8.6c   18-Nov-96 Not suppressing warning message for "missing decay
!                     modes - fixed
!                   Logic error in saving XREF symbols with modifiers
!                     corrected
!  8.6d   21-Nov-96 Added check on missing blank in T field
!                   Suppress XREF vs DSID check if no adopted data set
!                     and only one source data set for a nuclide
!                   Logic error in sorting caused loss of some XREF vs
!                     DSID messages - Fixed
!  8.6e   06-Dec-96 1) Changed XREF error on final level to warning
!                   2) Changed assumption when no uncertainties given
!                     for level and gamma records: "1" if no trailing
!                     digits after decimal; "2" if one trailing digit;
!                     "15" if more than one. Also checked for "+X", in
!                     setting this assumption
!                   3) Corrected erroneous COMMENT/PRIMARY error
!                     message for EC2P decay data sets
!                   4) Corrected spurious "Excluded by XREF:" output
!                   5) Added array limit checks for X record and DSID
!                     storage
!                   6) Changed array limit warnings to informational
!                   7) Corrected output overflow error when there were
!                     no X vs DSID mismatches
!                   8) Expanded allowed types of WIDTH's - See
!                     subroutine Dowidths
!                   9) Expanded allowed mixtures of decay modes - See
!                     subroutine Dodecays
!  8.7    30-Dec-96 1) Not suppressing level string on warning for
!                     missing decay modes - corrected
!                   2) Logical function Ismet was misinterpreting
!                     energies in T1/2 field - corrected
!                   3) Subscript range error corrected in CHKP
!                   4) Strict interpretation of memo on H record means
!                     no cutoff date should be required for ERR.
!                     Changed lack of AUT for ERR from <E> to <W>
!                   5) Added "YYTOI" as a valid DSPUB
!                   6) Corrected typo error in RDECTYP for recognizing
!                     multiple decay modes
!                   7) NUCID A and Z are now checked against A and Z
!                     calculated from the parent and decay mode for
!                     decay data sets
!                   8) Liberalized checking for decay type using new
!                     GETDECAZ routine as a final resort and defining
!                     as DECTYP=13
!                   9) Changed logic on checking (delayed-)particle
!                     records to allow greater flexibility. Also
!                     separated nuclide decay from particle
!                     multiparticle decay
!                  10) Added option to suppress output from XREF/DSID
!                     checking
!                  11) Added checks on "LABEL=". Also if found, change
!                     error messages to warnings
!                  12) Allow "INFNT" in MR field
!                  13) Allow "L+?" and "L1+(L2)" in L field
!                  14) For adopted data sets with no PN record and any
!                     data set with option 6 on PN record, check for
!                     existence of one gamma with RI=100 for each set
!                     of deexciting gammas
!                  15) Gamma-order check got confused with parentheses
!                     around EG or leading blanks - Fixed
!                  16) Fixed erroneous error messages when "E AP "
!                     and when leading blanks for "E" encountered on X
!                     record DSID
!                  17) Suppress consistency check between P and DSID if
!                     no P record
!                  18) Check value for leading 0 followed by an integer
!                     and flag as a probable error
!                  19) Allow EKC, etc. standalone and as ratios on E
!                     continuation records
!                  20) Corrected error in parsing nuclide decay modes
!                  21) Allow sign only for "G="
!                  22) Corrected errors when checking ID record
!                     continuations. NOTE: Now all ID record
!                     continuations will always be printed out.
!  8.7a    6-Jan-97 1) Corrected erroneous output when UNASSIGNED
!                     CHARACTER found and added check to see if zero
!                     gamma energy.
!                   2) Corrected erroneous error counting when
!                     <I> LABEL= message.
!  8.7b    9-Jan-97 1) Source lines out of order for NO RI=100... -
!                     Corrected.
!                   2) Subscript range error when FL= encountered on
!                     L Continuation record - Corrected
!                   3) Subscript range error when L Continuation with
!                     XREF occured prior to any L records - Corrected
!                     and added general check and error message for this
!                     this type of problem
!  8.7c   10-Sep-98 1) Factored in Jean Blachot's 1997 corrections for
!                     MS-DOS
!                   2) Upgraded to IUPAC symbols for Z>103 and added
!                     warning message for the old formalism
!  8.7d    8-Oct-98 Corrected error in call to CKU in line 02 00980
!  8.7e   21-Dec-98 Corrected error in Chkh when missing volume. Also
!                     corrected position of asterisks
!  8.7f   27-Jan-99 Corrected substring range errors in Chkdex
!  8.7g   09-Apr-99 Corrected substring range error in Chkh
!                   Allow lower case D comments
!                   Corrected initialization problem in Chkcmp
!                   Allow angular distributions and DCO
!  8.8    08-Jun-99 Added coding for formats adopted at 1998 Vienna
!                   NSDD meeting
!                   a. Change in ID record
!                   b. Addition of Ionized Atom datasets
!                   c. Neutron symbol changed from "N" to "NN"
!                   d. P records required for IT and SF datasets
!  8.8a   10-Jun-99 Corrected errors in 8.8 noted by J. Blachot and
!                     J. Katakura
!  8.8b   16-Jun-99 Corrected:
!                   a. Error in checking PUB field for new Y2K
!                   b. Problems in Ckref in handling both old 6- and
!                     new 8-byte keynumbers
!                   c. Erroneous report of unmatched brackets when table
!                     Comments
!  8.8c   21-Jun-99 Check for and report blank H record - These caused
!                     an infinite loop in ChkH
!                   Cleaned up coding in ChkP to properly handle QP for
!                     IT and SF decay
!                   Corrected problem in Nucid when a blank string was
!                     passed to it
!                   Added check on validity of parents in DSID
!                   Allow multiple parents if SF decay (As per JKT.
!                     6/18/1999)
!                   Updated H record checks to include proposed
!                     revisions discussed but not adopted at 1999 USNDP
!                     meeting
!                   Cleaned up logic for handling H records
!  8.8d   25-Jun-99 Allow COMMENTS datasets for Nuclides (As per JKT.
!                     6/25/1999)
!  8.8e   28-Jun-99 Not catching some types of comments which should not
!                     be in the body of a dataset - Corrected
!                   Was not checking non-general comments for valid
!                     fields - Corrected
!  8.8f   13-Jul-99 Missing "=" for XREF and FLAG not being flagged -
!                     Corrected
!                   Comment/Primary conflicts were being overlooked -
!                     Corrected
!  8.9    23-Jul-99 1. Split errors (<E>) into errors (<E>) and fatal
!                     errors (<F>) and added option to only report
!                     fatal errors
!                   2. Corrected logic introduced in 8.8f
!                   3. Added check for two numeric level energies
!                   4. Added check for a blank PN record
!                   5. Corrected logic problems in CKCFLD
!                   6. CRDTYP was not recognizing prompt decay -
!                     Fixed
!  8.9a   30-Jul-99 Modified CKPUNC to handle a "?" associated with a
!                     half-life.
!  8.9b   04-Aug-99 Added check for duplicate X records.
!  8.9c   31-Aug-99 Corrected compiler-dependent typos in Finh
!  8.9d   14-Sep-99 In Chkdex, give non-numeric calculated final
!                     levels if parent is non-numeric
!  8.9e   26-Oct-99 1. Corrected erroneous error message for %xx=?
!                   2. Corrected error in checking col. 77 of level
!                     versus band symbols
!                   3. Added warning if level member of more than one
!                     band
!  8.9f   27-Oct-99 Corrected substring range errors in CKCFLD when
!                     nothing given for FLAG= or XREF=
!  8.9g   03-Apr-2000 1. Added a check for duplicate DSID's
!                     2. Changed from <W> to <E> message on long records
!  8.9h   21-Apr-2000 Call to Repchr instead of Repstr in Dowidths
!                       caused an infinite loop - Fixed
!  8.9i   22-May-2000 Implemented format changes approved at USNDP2000
!                     1. Allow FL=?
!                     2. Allow lowercase second letter for Author code
!                     3. %xx=? was allowed in version 8.9e
!                     4. Allow ENSDF as a citation on H record
!                     5. Remove warning for H record changes proposed
!                       at USNDP1999 and adopted in 2000
!  8.9j   31-May-2000 Overlooked difference in format for ENSDF citation
!                       compared to NDS and NP - Fixed
!                     Corrected substring error in Chkh
!  8.9k   13-Oct-2000 Implementation of angular distributions and DCO
!                       in version 8.7g never worked correctly - fixed
!  8.9l   01-Mar-2001 Added MDC coding for f77 under Linux
!  8.9m   11-Jul-2001 Allow non-unique forbidden UN for B and E records
!  8.9n   24-Jul-2001 Corrected error on checking for duplicate DSID's
!                       when first DSID was shorter than second
!  8.9o   16-Aug-2001 Added check for trailing "," if reference list was
!                       not closed by ")" in CHKCNT.
!  8.9p   22-Aug-2001 Added more checks on the N record for possible
!                       mistypes
!  8.9q   17-Sep-2001 Corrected erroneous warning on A/B/E intensities
!                       when uncertainty was very small.
!  8.9r   13-Nov-2001 Variable adptnuc needed to be added to COMMON
!                       XC2DSID
!  9.0    15-Nov-2001 Corrected error in checking op code when followed
!                       by a "(".
!                     Added check to see if value for "FL=" consistent
!                       with E(level)-Egamma.
!  9.0a   28-Nov-2001 Allow "E AP " for reaction DSID.
!                     Expand allowed characters in JPI field of LEVEL
!                       record
!  9.0b   30-Nov-2001 Add check for blank field preceding "$" on COMMENT
!                       record
!                     Corrected error in checking HISTORY record when
!                       comment was not the last entry on a continuation
!  9.0c   12-Feb-2002 Added check for SUMOF or DELTA on Comment record
!                       not followed by " ".
!                     Added consistency checks:
!                       1. Non-numeric uncertainties in IB, IE, LOGFT,
!                         and TI fields of B and E records.
!                       2. IB on B and E records vs EAV on continuation
!                         records.
!                       3. IE/TI from E record versus sum of capture
!                         fractions on continuation record
!                       4. Existence of TI on E record if IB and IE
!                         are given
!                     Flag where first "{" occurs when unbalanced.
!  9.0d   13-May-2002 Removed expansion of allowed characters added in
!                       9.0a and replaced with check on new formalism
!                       of J, J1, etc. for JPI of level record.
!  9.0e   14-Aug-2002 1. Changed from <E> to <W> "TI field missing"
!                     2. Fixed erroneous report for IB+IE .NE. TI when
!                       uncetainties non-numeric
!                     3. Added warning on missing RI if absolute
!                       normalization, EG<=400 keV, and no CC but TI
!                       given on G record.
!  9.0f   23-Jun-2003 1. Allow "J GE" and "J LE" in J field
!                     2. Added checks for possible junk before first
!                       parentheses or square bracket and after last
!                       parentheses or square bracket in J field
!                     3. Added check for "/2" without preceding
!                       numeric character in J field
!  9.0g   04-Aug-2003 1. IB and EAV check for BETA records not being
!                       initialized - Corrected.
!                     2. For EL-EG message, changed to non-numeric
!                       format if E(level) had a "+X,..."
!  9.0h   05-Dec-2003 1. Modified logic in CHKE in attempt to handle
!                       IB, IE, and TI with no uncertainties.
!                     2. Corrected problems in CHKI when validating
!                       PUB field.
!                     3. Changed ZERO ENERGY GAMMA FOUND from <W> to
!                       <E>.
!                     4. Added checks for blank Egamma in Chkdex to
!                       avoid fatal error.
!                     5. Modified logic in Ckflval to be more tolerant
!                       of inconsistencies between FL and EL-EG.
!
!  PLEASE DIRECT ANY QUESTIONS OR COMMENTS TO:                         *
!                    NATIONAL NUCLEAR DATA CENTER                      *
!                    BROOKHAVEN NATIONAL LABORATORY                    *
!                    BLDG. 197D                                        *
!                    UPTON, NEW YORK  11973                            *
!                    (631) 344-2902                                    *
!                                                                      *
!                                                                      *
!***********************************************************************
!
      PROGRAM FMTCHK
!
!     PARAMETER definitions
!
!     MAX. NUMBER OF LEVELS FOR FINAL LEVEL CHECKS.
      INTEGER(KIND=4), PARAMETER :: maxlvl = 1000
!     Maximum number of gammas allowed from level
      INTEGER(KIND=4), PARAMETER :: maxdxg = 2000
      INTEGER(KIND=4), PARAMETER :: maxxc = 200, maxds = maxxc,         &
     &                              maxadpt = 150
      INTEGER(KIND=4), PARAMETER :: nalab = 8, nblab = 9, ndlab = 11
      INTEGER(KIND=4), PARAMETER :: nelab = 13, nglab = 14, nllab = 11
      INTEGER(KIND=4), PARAMETER :: ngmax = 400, maxsf = 3
      INTEGER(KIND=4), PARAMETER :: maxoff = 26, jmax = 30
!
!      COMMON /VERSION/ PAGE, VERSION, VERDATE
!
      INTEGER(KIND=4) page
      CHARACTER(LEN=*), PARAMETER :: version = 'FMTCHK version 10.5d'
      CHARACTER(LEN=*), PARAMETER :: verdate = '29-Nov-2018'
!
!     FORTRAN UNIT NUMBERS FOR FILES.
!
      INTEGER(KIND=4), PARAMETER :: idefi = 5, idefo = 6
      INTEGER(KIND=4), PARAMETER :: inp = 21, out = 22, tmp = 23
!
!+++MDC+++
!...VMS
!/      CHARACTER(LEN=*), PARAMETER :: nuldev = 'NL:'
!/      CHARACTER(LEN=*), PARAMETER :: ostat = 'NEW'
!...UNX
      CHARACTER(LEN=*), PARAMETER :: nuldev = '/dev/null'
      CHARACTER(LEN=*), PARAMETER :: ostat = 'REPLACE'
!...DVF
!      CHARACTER(LEN=*), PARAMETER :: nuldev = '  '
!      CHARACTER(LEN=*), PARAMETER :: ostat = 'REPLACE'
!---MDC---
!
!     COMMON /BCHR  / INBAND, BSYMBS
!
      LOGICAL(KIND=4) :: inband
      LOGICAL(KIND=4) :: inseq
      CHARACTER(LEN=93) :: bsymbs
      CHARACTER(LEN=93) :: seqsymbs
!
!      COMMON /CRDCOM/ RTYPE, PRTYPE, CONTYP, PRVTYP, GCOUNT
!
      INTEGER(KIND=4) :: rtype, prtype, prvtyp, contyp, gcount
!        VARIOUS SUBROUTINES NEED TO KNOW THE CARD TYPE OF THE
!        CURRENT CARD AND THAT OF THE PREVIOUS PRIMARY CARD.
!        PREVIOUS DIFFERENT PRTYPE.
!        CONTINUATION TYPE (0 => PRIM., 1 => CONT.)
!        Count of GAMMA record plus its COMMENTs and CONTINUATIONs
!
!      COMMON /DEXCIT/ NDXG, RECDXG, RECNUM, DXGE, DXGDE, DXFLV, DXMUL, &
!    &                DXGCOL9
!
      INTEGER(KIND=4) :: ndxg
!        ndxg   - number of gammas deexciting level
      INTEGER(KIND=4), DIMENSION(maxdxg) :: recdxg, recnum
!        recdxg - sequence number of dexciting gamma
!        recnum - record number in temprorary direct access file
      CHARACTER(LEN=2), DIMENSION(maxdxg) :: dxgde
!        dxgde - uncertainty of gamma energy
      CHARACTER(LEN=10), DIMENSION(maxdxg) :: dxflv, dxge
!        dxflv - final level energy from "FL="
!        dxge  - energy of dexciting gamma
      CHARACTER(LEN=1), DIMENSION(maxdxg) :: dxgcol9, dxmul
!        dxmul - multiple placement character
!        dxgcol9 - column 9 of GAMMA record
!
!      COMMON /DSID  / DSA, DSZ, DSTYPE, DECTYP, HISPIN, ISY2K, DCOL89
!
      INTEGER(KIND=4) :: dsa, dsz
!        DSA - MASS NUMBER.
!        DSZ - ELEMENT NUMBER.
      INTEGER(KIND=4) :: dstype
!        DSTYPE - DATA SET TYPE:
!           0 => COMNT, 1 => ADOPT, 2 => DECAY, 3 => REACT, 4 => REFER
      INTEGER(KIND=4) :: dectyp
!        DECTYP - DECAY TYPE:
!           0 => UNKNOWN, 1 => A, 2 => P, 3 => N, 4 => B-, 5 => B+/EC,
!           6 => IT, 7 => SF, 8 => B-N, 9 => B-P/B+P/ECP,
!           10 => B-A/B+A/ECA, 11 => B-2N, 12 => B+2P/EC2P,
!           13 => Multiparticle decay, 14 => Heavy ion (e.g. 14C)
      LOGICAL(KIND=4) :: hispin, isy2k
!           HISPIN - TRUE. => HIGH-SPIN LEVELS, GAMMAS
!                    FALSE. => Other reaction dataset
!           ISY2K - TRUE. => Y2K ENSDF
!                   FALSE. => Old ENSDF
      CHARACTER(LEN=2) :: dcol89
!           DCOL89 - Allowed characters for columns 8 and 9
!                              of (delayed-)particle records
!
!      COMMON /EAVINFO/ DOEAVCHK, IBPRES, EAVPRES, B_ENUM, SB_ENUM,     &
!     &                 B_ECARD, SB_ECARD
!
      LOGICAL(KIND=4) :: doeavchk, ibpres, eavpres
!        Flag to do IB versus EAV consistency check
!        Existence of IB or EAV
      INTEGER(KIND=4) :: b_enum, sb_enum
      CHARACTER(LEN=80) :: b_ecard, sb_ecard
!        Record information
!
!      COMMON /ECCARDS/ ENUM, SENUM, ECARD, SECARD
!
      INTEGER(KIND=4) :: enum, senum
      CHARACTER(LEN=80) :: ecard, secard
!        Save EC/B+ and "S E" info for electron-capture fraction
!          consistency check
!
!      COMMON /ECINFO/ DOECCHK, ECCAPT, DECCAPT, IESTR, TISTR, DIESTR,  &
!    &                DTISTR
!
      LOGICAL(KIND=4) :: doecchk
!        Flag to do check
      REAL(KIND=4) :: deccapt, eccapt
!        Capture fractions
      REAL(KIND=4) :: avail_energy
!  energy available for decay, using decay level energy and Q value
!  for decay datasets only
      LOGICAL(KIND=4) :: isenernum=.FALSE.
!  logical value to indicate the the energy available for decay is available
!  as a number
      CHARACTER(LEN=8) :: iestr
!        IE field
      CHARACTER(LEN=10) :: tistr
!        TI field
      CHARACTER(LEN=2) :: diestr, dtistr
!        IE, TI uncertainties
!
!      COMMON /FINLVL/ FLVL, DFLVL, NFLVL, LVLEXC, BADORD, FJPI, FXREF, &
!    &                FBAND
!
      CHARACTER(LEN=10), DIMENSION(maxlvl) :: flvl
!        Level energies for final level checks and ambiguous final
!           level checks.
      CHARACTER(LEN=2), DIMENSION(maxlvl) :: dflvl
!        Level energy uncertainties for ambiguous final level checks.
      INTEGER(KIND=4) :: nflvl
!        Number of levels seen.
      LOGICAL(KIND=4) :: lvlexc
!        Flag is number of levels has been exceeded.
      LOGICAL(KIND=4) :: badord
!        Level energies are out of order
      CHARACTER(LEN=18), DIMENSION(maxlvl) :: fjpi
!        Store J field
      CHARACTER(LEN=66), DIMENSION(maxlvl) :: fxref
!        Store XREF field
      CHARACTER(LEN=1), DIMENSION(maxlvl) :: fband
!        Store BAND symbol
      CHARACTER(LEN=1), DIMENSION(maxlvl) :: fseq
!        Store SEQ symbol
!
!      COMMON /HISTORY/ DOHIST, FQUANT, EVALDATE, NODOL, ETYPE, HSEQ,   &
!    &                 DOHOUT, HCARD, CURQUANT
!
      LOGICAL(KIND=4) :: dohist, dohout, nodol
      INTEGER(KIND=4) :: evaldate, etype, hseq
      CHARACTER(LEN=80) :: hcard
      CHARACTER(LEN=3) :: curquant
      INTEGER(KIND=4), PARAMETER :: nquant = 6
      LOGICAL(KIND=4), DIMENSION(nquant) :: fquant
      CHARACTER(LEN=3), DIMENSION(NQUant) :: quant
      DATA quant/'TYP', 'AUT', 'CUT', 'CIT', 'COM', 'DAT'/
!
!      COMMON /RECNAM/ CHRREC
!
      CHARACTER(LEN=2), DIMENSION(0:14) :: chrrec
      DATA chrrec/'ID', 'N', 'P', 'Q', 'L', 'A', 'B', 'E', 'G', 'R',    &
     &     'X', 'D', 'PN', 'S', 'H'/
!        Array of character record types as RECTYP defined
!
!      COMMON /RPT   / ERPT, CRDWRT, NSEQ, CONTCK, NOWARN, NOXCHK, NOERR
!
      LOGICAL(KIND=4) :: erpt
!        ERPT - TRUE IF ERROR ONLY REPORT, FALSE IF FULL REPORT.
      LOGICAL(KIND=4) :: crdwrt
!        CRDWRT - TRUE IF DATA CARD HAS BEEN WRITTEN, FALSE IF NOT
!                  WRITTEN.
      INTEGER(KIND=4) :: nseq
!        NSEQ - SEQUENCE NUMBER OF LAST CARD SEEN IN THE DATA SET.
      LOGICAL(KIND=4) :: contck
!        CONTCK - TRUE IF CONTINUATION CARDS ARE TO BE CHECKED, FALSE
!                 IF NOT.
      LOGICAL(KIND=4) :: nowarn
!        NOWARN -TRUE if warning messages are to be suppressed, FALSE
!                 if not.
      LOGICAL(KIND=4) :: noxchk
!        NOXCHK - TRUE if XREF versus DSID checking is to be suppressed,
!                 FALSE if not.
      LOGICAL(KIND=4) :: noerr
!        NOERR - TRUE if error messages are to be suppressed, FALSE if
!                 not.
!
!      COMMON /SAVEID/ TOTCON, CONPOS, EXDSCN, DSIDSV
!
      INTEGER(KIND=4) :: totcon
      INTEGER(KIND=4), DIMENSION(6) :: conpos
      LOGICAL(KIND=4) :: exdscn
      CHARACTER(LEN=203) :: dsidsv
!
!      COMMON /SEEN  / ISEEN, NSEEN, NRSEEN, NTSEEN, BRSEEN, NBSEEN,    &
!
!    &                NPSEEN, PSEEN, QSEEN, RISEEN, TISEEN, GABELD,     &
!    &                PNSEEN, PNABS, IBSEEN, IPSEEN, NOBRPCT, EXP2PN,   &
!    &                CHGBR
      LOGICAL(KIND=4) :: iseen, nseen, nrseen, ntseen, brseen, nbseen
      LOGICAL(KIND=4) :: npseen, pseen, qseen, riseen, tiseen, gabeld
      LOGICAL(KIND=4) :: pnseen, pnabs, ibseen, ipseen, nobrpct
      LOGICAL(KIND=4) :: exp2pn, chgbr, flseen
!        iseen  - ID record seen
!        nseen  - Normalization record seen
!        nrseen - NR given on normalization record
!        ntseen - NT given on normalization record
!        brseen - BR given on normalization record
!        nbseen - NB given on normalization record
!        npseen - NP given on normalization record
!        pseen  - Parent record seen
!        qseen  - Q record seen
!        riseen - RI given on G record
!        tiseen - TI given on G record
!        gabeld - G, A, B, E, L, or (delayed-)particle record seen
!        pnseen - PN record seen
!        pnabs - Absolute gamma intensity indicated on PN RECORD
!                or by default if PN record not given
!        ibseen - IB given on B record or IB, IE, or TI given ON
!                EC record
!        ipseen - IP given on (delayed-)particle record
!        nobrpct - G.S. or metastable state encountered but no
!                  branching ratios on "2 L" records
!        chgbr - Check branching ratios for each set of deexciting
!                  gammas
!        flseen - Final level on Gamma continuation record seen
!
!      COMMON /TOTMES/ TOTWAR, TOTERR, TOTFAT
!
      INTEGER(KIND=4) :: toterr, totfat, totwar
!        Totals of warning and error messages for a data set
!
!     COMMON /XC2DSID/ NXC, NDS, SKIPXC, SKIPDS, XCSAVE, DSSAVE, ADPTNUC
!
      INTEGER(KIND=4) :: nxc, nds, skipxc, skipds
      CHARACTER(LEN=39), DIMENSION(maxxc) :: xcsave
      CHARACTER(LEN=39), DIMENSION(maxds) :: dssave
      CHARACTER(LEN=maxadpt) :: adptnuc
!
!      COMMON /TOTFED/ FN, DFN, TOTIN, DTOTIN
!
      REAL(KIND=4) :: dfn, dtotin, fn, totin
!
!      COMMON /ALABEL/ ALABON
!
      LOGICAL(KIND=4), DIMENSION(nalab) :: alabon
!
!      COMMON /BLABEL/ BLABON
!
      LOGICAL(KIND=4), DIMENSION(nblab) :: blabon
!
!     COMMON /DLABEL/ DLABON
!
      LOGICAL(KIND=4), DIMENSION(ndlab) :: dlabon
!
!     COMMON /ELABEL/ ELABON
!
      LOGICAL(KIND=4), DIMENSION(nelab) :: elabon
!
!     COMMON /GLABEL/ GLABON
!
      LOGICAL(KIND=4), DIMENSION(nglab) :: glabon
!
!     COMMON /LLABEL/ LLABON
!
      LOGICAL(KIND=4), DIMENSION(nllab) :: llabon
!
!      COMMON /IZLCOM/ IZLMSG
!
      CHARACTER(LEN=80) :: izlmsg
!
!     COMMON /SAVET12/ NPT12, MATCHT12, NPCARD, NSAVE9, PT12, SAVE9,    &
!    &                 NCOL9, PCOL9, PARNUCID, DPT12
!
      INTEGER(KIND=4) :: npt12, npcard
      INTEGER(KIND=4), DIMENSION(2) :: nsave9
      CHARACTER(LEN=2), DIMENSION(2) :: dpt12
      CHARACTER(LEN=1) :: ncol9, pcol9
      LOGICAL(KIND=4), DIMENSION(2) :: matcht12
      CHARACTER(LEN=5) :: parnucid
      CHARACTER(LEN=10), DIMENSION(2) :: pt12
      CHARACTER(LEN=1), DIMENSION(2,9) :: save9
!
!      COMMON /SCHR  / SSYMBS, GCOL9
!
      CHARACTER(LEN=93) :: ssymbs
      CHARACTER(LEN=1) :: gcol9
!
!     COMMON /ION   / IONSTA, ISION, TOTAL
!      COMMON /LEVION/ SUBSHE
!
      LOGICAL(KIND=4) :: ision, total
      INTEGER(KIND=4) :: ionsta
      CHARACTER(LEN=2) :: subshe
!
!     COMMON /FLGCHR/ FLAG
!     COMMON /FLGCOM/ FLGREF, NFLAG
!
      CHARACTER(LEN=1), DIMENSION(55,6) :: flag
      LOGICAL(KIND=4), DIMENSION(55,6) :: flgref
      INTEGER(KIND=4), DIMENSION(6) :: nflag
!
!     COMMON /SAVEG / NG, GSEQ, EGSTR, DEGSTR, GSYMB
!
      CHARACTER(LEN=1), DIMENSION(ngmax) :: gsymb
      CHARACTER(LEN=10), DIMENSION(ngmax) :: egstr
      CHARACTER(LEN=2), DIMENSION(ngmax) :: degstr
      INTEGER(KIND=4), DIMENSION(ngmax) :: gseq
      INTEGER(KIND=4) :: ng
!
!     COMMON /SFINFO/ NTOTSF, SFPAR
!
      CHARACTER(LEN=5), DIMENSION(maxsf) :: sfpar
      INTEGER(KIND=4) :: ntotsf
!
!     COMMON /XRFCHR/ XREFS
!
      CHARACTER(LEN=93) :: xrefs
!
!     COMMON /SAVLEV/ RIFND, NOBR100, LEVSEQ, LEVSTR
!
      CHARACTER(LEN=80) :: levstr
      LOGICAL(KIND=4) :: rifnd, nobr100
      INTEGER(KIND=4) :: levseq
!
!     COMMON /JSAVED/ JNO, JSAV
!
      INTEGER(KIND=4) :: jno
      CHARACTER(LEN=3), DIMENSION(jmax) :: jsav
!
!     COMMON /OFFSET/ OFFAMT, NOFF, OFFCHR
!
      INTEGER(KIND=4) :: noff
      REAL(KIND=4), DIMENSION(maxoff) :: offamt
      CHARACTER(LEN=1), DIMENSION(maxoff) :: offchr
!
!     Retain metastablestate information for checking
!
      INTEGER(KIND=4), PARAMETER :: maxms=20
      INTEGER(KIND=4) :: nms
      INTEGER(KIND=4), DIMENSION(MAXMS) :: seqms
      CHARACTER(LEN=2), DIMENSION(MAXMS) :: msstr
!
!     Reaction information for possible checking
!
      INTEGER(KIND=4) :: tara,tarz
      INTEGER(KIND=4) :: inca,incz
      INTEGER(KIND=4) :: outa,outz
      INTEGER(KIND=4) :: resa,resz
      INTEGER(KIND=4) :: coma,comz
      INTEGER(KIND=4) :: tarpos
      LOGICAL(KIND=4) :: isiar
      CHARACTER(LEN=5) :: incnuc,resnuc
!
!           FLAG INDICATING FIRST CARD OR CONT.
!              TRUE:  CONTINUATION, FALSE: FIRST CARD
      LOGICAL(KIND=4) :: contin
!
      CALL RUN_FMTCHK
!
      STOP
!
      CONTAINS
!
!***********************************************************************
!
      SUBROUTINE RUN_FMTCHK
!
      IMPLICIT NONE
!
!     Functions used
!
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
      CHARACTER(LEN=1), EXTERNAL :: UPCASE
      INTEGER(KIND=4), INTRINSIC :: INDEX, LEN_TRIM
!
!     Local variables
!
      CHARACTER(LEN=3) :: errtyp
      CHARACTER(LEN=80) :: card, lcard
      CHARACTER(LEN=256) :: crdtmp !     CARD IMAGE JUST READ.
      CHARACTER(LEN=1) :: old8
      CHARACTER(LEN=3) :: resp
      CHARACTER(LEN=100) :: name
      LOGICAL(KIND=4) :: newcom
!           Flag indicating that next COMMENT record should be start
!               of a comment
      INTEGER(KIND=4) :: comnt
!        FLAG INDICATING 'C', 'T', OR 'D' IN COL. 7:
!           0 => NONE, 1 => TYPE C COMMENT, 2 => TYPE T COMMENT
!           3 => TYPE D COMMENT, 4 => TYPE U COMMENT
!           5 => TYPE P COMMENT, -1 => ERROR
      INTEGER(KIND=4) :: lseq, pncnt
      INTEGER(KIND=4) :: i  ! Local counter
      INTEGER(KIND=4) :: kstart,kend
      INTEGER(KIND=4) :: ibrac
!        Check on matching curly brackets in comments
      INTEGER(KIND=4) :: rectyp
!        CARD RECORD TYPE:
!           0 => DSID, 1 => N, 2 => P, 3 => Q, 4 => L, 5 => A
!           6 => B, 7 => E, 8 => G, 9 => R, 10 => X, 11 => D
!           12 => PN, 13 => S, 14 => H, 99 => END, -1 => ERROR
!
      INTEGER(KIND=4), PARAMETER :: nf = 4
      CHARACTER(LEN=50), DIMENSION(nf) :: carray
      CHARACTER(LEN=50), DIMENSION(nf-1) :: file
      CHARACTER(LEN=50) :: carrayuc
      INTEGER(KIND=4) ::  npar
!
!     INITIALIZE SOME COMMON VARIABLES.
!
      PAGe = 0
      DSA = 0
      DSZ = -1
      ISEen = .FALSE.
      DOEcchk = .FALSE.
      DOEavchk = .FALSE.
      IBPres = .FALSE.
      EAVpres = .FALSE.
      ECArd = ' '
      SECard = ' '
      B_Ecard = ' '
      SB_ecard = ' '
      PRVtyp = 88
      PRType = 88
      DSIdsv = ' '
      TOTwar = 0
      TOTerr = 0
      TOTfat = 0
      old8 = ' '
      NXC = 0
      NDS = 0
      SKIpxc = 0
      SKIpds = 0

	NG=0	!---zv2015---
	NFLvl=0	!---zv2015---
!
      file(1) = 'fmtchk.inp'
      file(2) = 'fmtchk.rpt'
      file(3) = ' '
!
      npar = nf
!zv2015      CALL GET_COMMAND_LINE('%',carray,npar)
      CALL GET_COMMAND_LINE(' ',carray,npar)
!
!     Check input for new file names
!
!	write(*,*) '...NPAR=',npar
      IF(npar.GT.1) THEN
         DO i = 2, npar
!	    write(*,*) '...i=',i,' ',trim(carray(i))
            IF(carray(i).EQ.' '.OR.carray(i).EQ.'#') CYCLE
            file(i-1) = carray(i)
            carrayuc = carray(i)
            CALL UPSTR(carrayuc)
            IF(carrayuc.EQ.'NULL') file(i-1) = nuldev
!	    write(*,*) '...file-',i-1,trim(file(i-1))
         END DO
      END IF
!
!     open standard output file if not blank
!
      IF(file(3).NE.' ') CALL OPEN_FILE(idefo,file(3),OSTat)
!
      WRITE(IDEFO,'(/1X,A)') VERSION//' ['//VERDATE//']'
!
!     Open temporary intermediate file
!
      OPEN(UNIT=tmp,ACCESS='DIRECT',FORM='FORMATTED',STATUS='SCRATCH',  &
     &     RECL=80)
!
!     REQUEST INPUT FILE NAME AND OPEN IT
!
      IF(npar.EQ.0) THEN
         WRITE(idefo,'(/3A,$)')                                         &
     &            ' INPUT file (DEF: ',TRIM(file(1)),'):   '
         READ(idefi,'(A)') name
         IF(name.EQ.' ') name = file(1)
      ELSE
         name = file(1)
         write(idefo,*) '...Input file: ',trim(name)
      END IF
      Call Lbsup(name)
      OPEN(UNIT=inp,FILE=name,STATUS='OLD',ACTION='READ',ERR=80)
!
!     REQUEST OUTPUT FILE NAME AND OPEN IT
!
      IF(npar.EQ.0) THEN
         WRITE(idefo,'(3A,$)')                                          &
     &            ' OUTPUT file (DEF: ',TRIM(file(2)),'):  '
         READ(idefi,'(A)') name
         IF(name.EQ.' ') name = file(2)
      ELSE
         name = file(2)
         Call Lbsup(name)
         write(idefo,*) '...Output file: ',trim(name),' ',OSTat
      END IF
      CALL OPEN_FILE(out,name,OSTat)
!
!     REQUEST USER OPTIONS:
!     FULL REPORT OR ERRORS ONLY.
!     CHECK CONTINUATION CARDS OPTION.
!     FOR DEC PROMPT USER; FOR CDC OR IBM READ STANDARD INPUT.
!     COMMAND FILE IS ON STANDARD INPUT UNIT.
!
      ERPt = .TRUE.
      CONtck = .TRUE.
      NOErr = .FALSE.
      NOWarn = .FALSE.
      NOXchk = .FALSE.
      NDXg = 0
      IF(npar.EQ.0) THEN
         WRITE(idefo,'(/A,$)') ' Errors only or full report (E, F): '
         READ(idefi,'(A)',END=10) resp
         IF(UPCASE(resp(1:1)).EQ.'F') ERPt = .FALSE.
      ELSE
         IF(UPCASE(carray(1)(1:1)).EQ.'F') ERPt = .FALSE.
         write(idefo,*) '...Errors/Full report flag: ',carray(1)(1:1)
      ENDIF
!
      IF(npar.EQ.0) THEN
         WRITE(idefo,'(A,$)') ' Check continuation cards (Y, N):   '
         READ(idefi,'(A)',END=10) resp
         IF(UPCASE(resp(1:1)).EQ.'N') CONtck = .FALSE.
      ELSE
         IF(UPCASE(carray(1)(2:2)).EQ.'N') CONtck = .FALSE.
         write(idefo,*) '...Check continuation: ',carray(1)(2:2)
      END IF
!
      IF(npar.EQ.0) THEN
         WRITE(idefo,'(A,$)') ' Report only fatal errors (N, Y):   '
         READ(idefi,'(A)',END=10) resp
         IF(UPCASE(resp(1:1)).EQ.'Y') THEN
            NOErr = .TRUE.
            NOWarn = .TRUE.
         END IF
      ELSE
         IF(UPCASE(carray(1)(3:3)).EQ.'Y') THEN
            NOErr = .TRUE.
            NOWarn = .TRUE.
         END IF
         write(idefo,*) '...Report only fatal errors: ',carray(1)(3:3)
      END IF
!
      IF(.NOT.NOWarn) THEN
         IF(npar.EQ.0) THEN
            WRITE(idefo,'(A,$)') ' Suppress warning messages (N, Y):  '
            READ(idefi,'(A)',END=10) resp
            IF(UPCASE(resp(1:1)).EQ.'Y') NOWarn = .TRUE.
         ELSE
            IF(UPCASE(carray(1)(4:4)).EQ.'Y') NOWarn = .TRUE.
         write(idefo,*) '...Suppress warning messages: ',carray(1)(4:4)
         END IF
      END IF
!
      IF(npar.EQ.0) THEN
         WRITE(idefo,'(A,$)') ' Suppress XREF/DSID check (N, Y):   '
         READ(idefi,'(A)',END=10) resp
         IF(UPCASE(resp(1:1)).EQ.'Y') NOXchk = .TRUE.
      ELSE
         IF(UPCASE(carray(1)(5:5)).EQ.'Y') NOXchk = .TRUE.
         write(idefo,*) '...Suppress XREF/DSID check: ',carray(1)(5:5)
      END IF
!
!     PROCESS INPUT FILE
!
   10 DO WHILE (.TRUE.)
!
!        READ NEXT CARD.
!
         READ(inp,'(A)',ERR=20,END=20) crdtmp
         card = crdtmp(1:80)
         CRDwrt = .FALSE.
!
!        Check long lines and special characters
         IF(RECCHK(crdtmp)) CYCLE
!        If first 80 characters of card have a special character other
!        than <NULL>, cannot reliable process.
!
!        CHECK CARD TYPE.
!
         CALL CRDTYP(card,rectyp,comnt,contin)
!
!        Check for expected or unexpected PN continuation records
         IF(rectyp.EQ.12.AND.contin) pncnt = pncnt + 1
         IF(EXP2pn) THEN
            IF(rectyp.NE.12.OR.(rectyp.EQ.12.AND..NOT.contin)) THEN
               CALL WRTMSG('<E> MISSING EXPECTED "2PN"',77,77)
            END IF
            EXP2pn = .FALSE.
         ELSE
            IF(rectyp.EQ.12.AND.contin.AND.pncnt.EQ.1)                  &
     &         CALL WRTMSG('<E> UNEXPECTED "2PN" FOUND',77,77)
         END IF
!
         IF(comnt.LE.0.OR.comnt.EQ.5) THEN
            newcom = .TRUE.
         ELSE
            IF(card(8:8).NE.old8) newcom = .TRUE.
            old8 = card(8:8)
         END IF
         IF(rectyp.EQ.99) THEN
!           END CARD WILL NOT BE PRINTED, BUT SET CRDWRT ANYWAY.
            CRDwrt = .TRUE.
            IF(NDXg.GT.0.AND.NFLvl.GT.0) CALL CHKDEX(lcard,lseq)
            IF(NOBrpct) CALL WRTMS3
            IF(DOEcchk) CALL CKECCONS
            IF(DOEavchk) CALL CKEAV(.TRUE.,.TRUE.)
            CALL CHKEND(card)
            CYCLE
         ELSE IF(rectyp.EQ.0.AND.comnt.EQ.0.AND.(.NOT.contin)) THEN
!           DSID CARD WILL BE PRINTED WITH DATA SET SEPARATOR.
            CALL WRTDSI(card)
         ELSE
!           WRITE OUT ALL OTHER CARDS.
            IF(.NOT.CRDwrt) CALL WRTCRD(card)
            IF(rectyp.EQ.9) CYCLE
!           IGNORE ALL REFERENCE CARDS.
            IF(rectyp.LT.0) THEN
!              REPORT INVALID RECORD TYPE.
               CALL WRTMSG('<F> INVALID RECORD TYPE',8,8)
               CYCLE
            ELSE IF(comnt.LT.0) THEN
!              REPORT INVALID COMMENT TYPE.
               CALL WRTMSG('<F> INVALID COMMENT TYPE',7,7)
               CYCLE
            END IF
         END IF
!
!        CHECK COMMENTS AND CONTINUATIONS.
!
         IF(comnt.GT.0) THEN
!           COMMENT CARD...
!
!
!           Check the NUCID
!
            Call Ckncid(card)

!           on any comment card, {}has to be matched - Except Table
!
            ibrac = 0
            DO i = 1, LEN_TRIM(card)
               IF(card(i:i).EQ.'{') ibrac = ibrac + 1
               IF(card(i:i).EQ.'}') ibrac = ibrac - 1
            END DO
            IF(ibrac.NE.0) THEN
               kstart=INDEX(card,'{')
               If(kstart .GT. 0)Then
	          kend=LEN_TRIM(card)
               Else
	          kend=INDEX(card,'}')
		  kstart=10
               EndIf
               If(ibrac .GT. 1)Then
                  errtyp='<E>'
               Else
                  errtyp='<W>'
               EndIf
               CALL WRTMSG(errtyp//' UNBALANCED BRACKETS',kstart,kend)
            END IF
!TWB - 20080227            END IF
!
!           U comments (COMNT=4) cannot appear in adopted
!           Also cannot appear in COMMENTS or REFERENCES (TWB. 930511)
!
            IF(comnt.EQ.4) THEN
               IF(DSType.EQ.1) THEN
                  CALL WRTMSG('<E> U COMMENT NOT ALLOWED IN ADOPTED',0, &
     &                        0)
               ELSE IF(DSType.EQ.0) THEN
                  CALL WRTMSG('<E> U COMMENT NOT ALLOWED IN COMMENTS',0,&
     &                        0)
               ELSE IF(DSType.EQ.4) THEN
                  CALL WRTMSG('<E> U COMMENT NOT ALLOWED IN REFER''S',0,&
     &                        0)
               END IF
            END IF
!
!TWB-20060503            IF(comnt.EQ.3) THEN
!TWB-20060503               IF(newcom.AND.contin) THEN
!TWB-20060503                  newcom = .FALSE.
!TWB-20060503                  CALL WRTMSG('<E> NO STARTING COMMENT FOUND',6,6)
!TWB-20060503               END IF
!TWB-20060503               IF(.NOT.contin) newcom = .FALSE.
!TWB-20060503               CYCLE
 !TWB-20060503           END IF
!TWB-20060503!           IGNORE D TYPE COMMENTS.
!
!           check publication comment separately
!
            IF(comnt.EQ.5) THEN
               CALL CHKCMP(card)
            ELSE IF((rectyp.EQ.0).OR.(contin)) THEN
!              GENERAL COMMENT...
               IF(newcom.AND.contin) THEN
                  newcom = .FALSE.
                  CALL WRTMSG('<F> NO STARTING COMMENT FOUND',6,6)
               END IF
               IF(.NOT.contin) newcom = .FALSE.
               IF(DOHist) CALL FINH
               CALL CHKCMG(card)
            ELSE
!              RECORD COMMENT...
               newcom = .FALSE.
               CALL CHKCMR(card)
!              U comments can only be general comments (TWB. 930511)
               IF(comnt.EQ.4)                                           &
     &            CALL WRTMSG('<E> U COMMENT MUST BE GENERAL',0,0)
            END IF
!           History record cannot have comments
            IF(rectyp.EQ.14) CALL WRTMSG('<F> H CARD COMMENT ILLEGAL',0,&
     &                                  0)
            CYCLE
         ELSE IF(contin) THEN
!           CONTINUATION CARD...
            IF(CONtck) CALL CHKCNT(card)
            CYCLE
         END IF
!
!        CHECK RECORD TYPE FIRST CARDS.
!
         IF(DOHist) CALL FINH
         IF(DOEcchk) CALL CKECCONS
         IF(DOEavchk) CALL CKEAV(.TRUE.,.TRUE.)
         SELECT CASE(rectyp+1)
         CASE(1)
!
            old8 = ' '
            CALL CHKI(card)
         CASE(2)
            CALL CHKN(card)
         CASE(3)
            CALL CHKP(card)
         CASE(4)
            CALL CHKQ(card)
         CASE(5)
            IF(NDXg.GT.0.AND.NFLvl.GT.0) CALL CHKDEX(lcard,lseq)
            IF(NOBrpct) CALL WRTMS3
            lseq = NSEq
            lcard = card
            CALL CHKL(card)
         CASE(6)
            IF(NOBrpct) CALL WRTMS3
            CALL CHKA(card)
         CASE(7)
            IF(NOBrpct) CALL WRTMS3
            CALL CHKB(card)
         CASE(8)
            IF(NOBrpct) CALL WRTMS3
            CALL CHKE(card)
         CASE(9)
            IF(NOBrpct) CALL WRTMS3
            CALL CHKG(card)
         CASE(10)
         CASE(11)
            CALL CHKX(card)
         CASE(12)
            IF(NOBrpct) CALL WRTMS3
            CALL CHKD(card)
         CASE(13)
            CALL CHKPN(card)
            pncnt = 0
         CASE(14)
            CALL CHKS(card)
         CASE(15)
            CALL CHKH(card,1)
         CASE DEFAULT
!           FELL THROUGH.
            CALL WRTMSG('<F> INVALID RECORD TYPE',8,8)
         END SELECT
      END DO
!
!     END OF INPUT FOUND.
!
   20 IF(ISEen) THEN
!        Must call Chkend to check consistencies on whole data set
!        (TWB. 930323)
         card = ' '
         IF(NDXg.GT.0.AND.NFLvl.GT.0) CALL CHKDEX(lcard,lseq)
         IF(DOEcchk) CALL CKECCONS
         IF(DOEavchk) CALL CKEAV(.TRUE.,.TRUE.)
         CALL CHKEND(card)
         CALL WRTMSG('<F> NO END CARD BEFORE END OF FILE',0,0)
      END IF
!     Print to terminal or LOG file summary of errors and warnings
!     (TWB. 930323)
      IF(TOTfat.GT.0) THEN
         WRITE(idefo,'(/1X,I4,A)') TOTfat, ' fatal errors reported'
      END IF
      IF(TOTerr.GT.0) THEN
         IF(.NOT.NOErr) THEN
            WRITE(idefo,'(1X,I4,A)') TOTerr, ' error(s) reported'
         ELSE
            WRITE(idefo,'(1X,I4,A)') TOTerr, ' error(s) suppressed'
         END IF
      END IF
      IF(TOTwar.GT.0) THEN
         IF(.NOT.NOWarn) THEN
            WRITE(idefo,'(1X,I4,A)') TOTwar, ' warning(s) reported'
         ELSE
            WRITE(idefo,'(1X,I4,A)') TOTwar, ' warning(s) suppressed'
         END IF
      END IF
      CALL GLOBAL
      GO TO 100
   80 WRITE(IDEFO,'(/6X,A)') 'INPUT FILE OPEN ERROR'
!
  100 WRITE(idefo,'(/A)') ' Program completed successfully'
!
      RETURN
      END SUBROUTINE RUN_FMTCHK
!
!***********************************************************************
!
      SUBROUTINE OPEN_FILE(I,Ofile,Ostat)
!
!     MACHINE DEPENDENT FILE OPEN ROUTINE
!
      IMPLICIT NONE
!
!     Dummy variables
!
      CHARACTER(LEN=*) :: Ofile,Ostat
      INTEGER(KIND=4) :: I
!
!+++MDC+++
!...VMS
!/      OPEN(UNIT=I,FILE=Ofile,ACCESS='SEQUENTIAL',STATUS=Ostat,        &
!/     &     CARRIAGECONTROL='LIST')
!...UNX, DVF
      OPEN(UNIT=I,FILE=Ofile,ACCESS='SEQUENTIAL',STATUS=Ostat)
!---MDC---
!
      RETURN
      END SUBROUTINE OPEN_FILE
!
!***********************************************************************
!
      SUBROUTINE CHKA(Card)
!
!     CHECK ALPHA CARD.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=80) :: Card
!
!     Functions used
!
      INTEGER(KIND=4), EXTERNAL :: SPAN
!
!     Local variables
!
      REAL(KIND=4) :: dx, x
!
!     CHECK CARD SEQUENCE.
!
      IF(.NOT.ISEen) CALL WRTMSG('<F> DSID CARD NOT YET SEEN',0,0)
      IF(DSType.NE.2.OR.DECtyp.NE.1)                                    &
     &   CALL WRTMSG('<F> A CARD ILLEGAL FOR THIS DATASET',8,8)
      GABeld = .TRUE.
!
!     VALIDATE NUCID.
!
      CALL CKNCID(Card)
      CALL CKBLNK(Card(9:9),9,9)
!
!     VALIDATE DATA FIELDS.
!
      IF(ALAbon(1)) THEN
         CALL CKE(Card(10:19),10,19,'<W>')
      ELSE
         CALL CKE(Card(10:19),10,19,'<E>')
      END IF
      CALL CKU(Card(20:21),20)
      IF(Card(22:29).EQ.' ') THEN
      ELSE IF(Card(SPAN(Card,22,' '):29).NE.'WEAK') THEN
         IF(BRSeen) THEN
            CALL CNVS2U(Card(22:29),Card(30:31),x,dx)
            IF(Card(30:30).EQ.'L') THEN
               x = x/2.
               dx = x
            END IF
            IF(Card(30:30).EQ.'G') THEN
               x = (100.+x)/2.
               dx = x
            END IF
            IF(Card(30:30).EQ.'A') dx = 0.5*x
            TOTin = TOTin + x
            DTOtin = DTOtin + dx*dx
         END IF
         IF(ALAbon(3)) THEN
            CALL CKVPRN(Card(22:29),22,.TRUE.,'<W>')
         ELSE
            CALL CKVPRN(Card(22:29),22,.TRUE.,'<E>')
         END IF
      END IF
      CALL CKU(Card(30:31),30)
      IF(ALAbon(5)) THEN
         CALL CKV(Card(32:39),32,.FALSE.,'<W>')
      ELSE
         CALL CKV(Card(32:39),32,.FALSE.,'<E>')
      END IF
      CALL CKU(Card(40:41),40)
      CALL CKBLNK(Card(42:76),42,76)
      CALL CKC(Card(77:77),77,77)
      CALL CKBLNK(Card(78:79),78,79)
      CALL CKQ(Card(80:80),80,80)
!
!     CONSISTENCY BETWEEN UNC FIELDS AND THE MAIN VALUE FIELDS
!
      CALL CKVU(Card(10:19),Card(20:21),10,21)
      CALL CKVU(Card(22:29),Card(30:31),22,31)
      CALL CKVU(Card(32:39),Card(40:41),32,41)
!
      RETURN
      END SUBROUTINE CHKA
!
!***********************************************************************
!
      SUBROUTINE CHKB(Card)
!
!     CHECK BETA- CARD.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=80) :: Card
!
!     FUNCTIONS USED
!
      INTEGER(KIND=4), INTRINSIC :: INDEX
      INTEGER(KIND=4), EXTERNAL :: SPAN, TYPSTR
!
!     Local variables
!
      REAL(KIND=4) :: dx, x
!
!     CHECK CARD SEQUENCE.
!
      IF(.NOT.ISEen) CALL WRTMSG('<F> DSID CARD NOT YET SEEN',0,0)
      IF(DSType.NE.2.OR.DECtyp.NE.4)                                    &
     &   CALL WRTMSG('<F> B CARD ILLEGAL FOR THIS DATASET',8,8)
      GABeld = .TRUE.
!
!     VALIDATE NUCID.
!
      CALL CKNCID(Card)
      CALL CKBLNK(Card(9:9),9,9)
!
!     VALIDATE DATA FIELDS.
!
      IF(BLAbon(1)) THEN
         CALL CKE(Card(10:19),10,19,'<W>')
      ELSE
         CALL CKE(Card(10:19),10,19,'<E>')
      END IF
      CALL CKU(Card(20:21),20)
      IF(Card(22:29).EQ.' ') THEN
      ELSE IF(Card(SPAN(Card,22,' '):29).NE.'WEAK') THEN
         IBSeen = .TRUE.
         IBPres = .TRUE.
         IF(NBSeen) THEN
            CALL CNVS2U(Card(22:29),Card(30:31),x,dx)
            IF(Card(30:30).EQ.'L') THEN
               x = x/2.
               dx = x
            END IF
            IF(Card(30:30).EQ.'G') THEN
               x = (100.+x)/2.
               dx = x
            END IF
            IF(Card(30:30).EQ.'A') dx = 0.5*x
            TOTin = TOTin + x
            DTOtin = DTOtin + dx*dx
         END IF
         IF(BLAbon(3)) THEN
            CALL CKVPRN(Card(22:29),22,.TRUE.,'<W>')
         ELSE
            CALL CKVPRN(Card(22:29),22,.TRUE.,'<E>')
         END IF
      END IF
      CALL CKU(Card(30:31),30)
      CALL CKBLNK(Card(32:41),32,41)
      IF(BLAbon(5)) THEN
         CALL CKV(Card(42:49),42,.TRUE.,'<W>')
      ELSE
         CALL CKV(Card(42:49),42,.TRUE.,'<E>')
      END IF
      CALL CKU(Card(50:55),50)
      CALL CKBLNK(Card(56:76),56,76)
      CALL CKC(Card(77:77),77,77)
      CALL CKUN(Card(78:79),78,79)
      CALL CKQ(Card(80:80),80,80)
!
!     CONSISTENCY BETWEEN UNC FIELDS AND THE MAIN VALUE FIELDS
!
      CALL CKVU(Card(10:19),Card(20:21),10,21)
      CALL CKVU(Card(22:29),Card(30:31),22,31)
      CALL CKVU(Card(42:49),Card(50:55),42,55)
      IF(Card(22:29).NE.' '.AND.TYPSTR(Card(30:31)).EQ.2.AND.Card(42:49)&
     &   .NE.' '.AND.INDEX(Card(50:55),'SY').EQ.0) THEN
         IF(Card(30:31).EQ.'AP'.AND.INDEX(Card(50:55),'AP').EQ.0)       &
     &      CALL WRTMS2('<E> Inconsistent uncert.',30,31,50,55)
         IF(Card(30:30).EQ.'G'.AND.INDEX(Card(50:55),'L').EQ.0)         &
     &      CALL WRTMS2('<E> Inconsistent uncert.',30,31,50,55)
         IF(Card(30:30).EQ.'L'.AND.INDEX(Card(50:55),'G').EQ.0)         &
     &      CALL WRTMS2('<E> Inconsistent uncert.',30,31,50,55)
      END IF
      B_Enum = NSEq
      B_Ecard = Card
!
      RETURN
      END SUBROUTINE CHKB
!
!***********************************************************************
!
      SUBROUTINE CHKCMG(Card)
!
!     CHECK GENERAL COMMENTS CARD.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=80) :: Card
!
!     Functions used
!
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
!
!     Local variables
!
      INTEGER(KIND=4) :: a, z
!
!     CHECK CARD SEQUENCE.
!
      IF(.NOT.ISEen) CALL WRTMSG('<F> DSID CARD NOT YET SEEN',0,0)
      IF(GABeld) THEN
         IF(Card(6:6).EQ.' '.OR.Card(6:6).EQ.'1') THEN
            CALL WRTMSG('<F> GEN''L COMMENT ILLEGAL HERE',0,0)
         ELSE IF(RTYpe.NE.PRType) THEN
            CALL WRTMSG('<F> COMMENT/PRIMARY CARD TYPE CONFLICT',8,9)
!           D records have a particle in colum 9; G, P, N, a symbol
         ELSE IF((RTYpe.EQ.8.AND.Card(9:9).NE.GCOl9).OR.                &
     &           (RTYpe.EQ.2.AND.Card(9:9).NE.PCOl9).OR.                &
     &           (RTYpe.EQ.1.AND.Card(9:9).NE.NCOl9).OR.                &
     &           (RTYpe.EQ.11.AND.Card(8:9).NE.DCOl89)) THEN
            CALL WRTMSG('<F> COMMENT/PRIMARY CARD TYPE CONFLICT',8,9)
         END IF
      ELSE
         IF(RTYpe.NE.PRType) THEN
            CALL WRTMSG('<F> COMMENT/PRIMARY CARD TYPE CONFLICT',8,9)
         END IF
      END IF
!
!     VALIDATE NUCID.
!
      CALL NUCID(Card(1:5),a,z)
      IF(a.EQ.0.OR.z.EQ.-1) THEN
         CALL WRTMSG('<F> INVALID NUCID',1,5)
      ELSE IF(IZLmsg.NE.' ') THEN
         CALL REPSTR(IZLmsg,'Obsolete formalism','Obsol. form')
         CALL ADDSTR(IZLmsg,1,'<W> ')
         CALL WRTMSG(TRIM(IZLmsg),4,5)
      END IF
      IF(RTYpe.EQ.2) THEN
!        NUCID FOR PARENT COMMENTS WON'T MATCH DSID.
      ELSE IF(a.NE.DSA.OR.z.NE.DSZ) THEN
         CALL WRTMSG('<F> NUCID DOESN''T MATCH DSID',1,5)
      END IF
!     D records records have particle in column 9; G, N, and P, symbol
      IF(RTYpe.NE.1.AND.RTYpe.NE.2.AND.RTYpe.NE.8.AND.RTYpe.NE.11)      &
     &   CALL CKBLNK(Card(9:9),9,9)
      CALL CKSPEC(Card)
!
      RETURN
      END SUBROUTINE CHKCMG
!
!***********************************************************************
!
      SUBROUTINE CHKCMP(Card)
!
!     CHECK PUBLICATION COMMENTS CARD.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=80) :: Card
!
!     Functions used
!
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
      INTEGER(KIND=4), INTRINSIC :: INDEX, LEN, LEN_TRIM
      INTEGER(KIND=4), EXTERNAL :: BREAK
!
!     Local variables
!
      CHARACTER(LEN=8) :: tsym
      CHARACTER(LEN=71) :: work
      INTEGER(KIND=4) :: i1, i2, j, k, l
!
      tsym = ' '
!
!     CHECK CARD SEQUENCE.
!
      IF(.NOT.ISEen) CALL WRTMSG('<F> DSID CARD NOT YET SEEN',0,0)
      IF(GABeld.AND.RTYpe.NE.PRType) THEN
         CALL WRTMSG('<F> COMMENT/PRIMARY CARD TYPE CONFLICT',8,9)
      END IF
!
!     VALIDATE NUCID.
!
      CALL CKNCID(Card)
!
!     Only L,G,A,E,B are allowed
!
      IF(INDEX('LGAEB',Card(8:8)).EQ.0) THEN
         CALL WRTMSG('<E> PUB COMNT ALLOWED FOR LGAEB ONLY',0,0)
         RETURN
      END IF
!
      i1 = 10
      DO WHILE (.TRUE.)
         i2 = BREAK(Card,i1,'$') - 1
         IF(i2.LE.i1) RETURN
!
         work = Card(i1:i2)
         l = LEN_TRIM(work)
         IF(i2.GT.l) i2 = l + 9
!
!        Only PUB= is allowed
!
         CALL LBSUP(work)
         IF(work(1:4).NE.'PUB=') THEN
            CALL WRTMSG('<E> PUB COMNT REQUIRES PUB=',i1,i1+3)
            RETURN
         END IF
!
!        check publication modification SYM
!
         IF(work(5:5).EQ.'(') THEN
            IF(work(l:l).NE.')') THEN
               CALL WRTMSG('<E> MISSING ")"',i2,i2)
               l = l + 1
            END IF
            tsym = work(6:l-1)
         END IF
         IF(work(5:5).EQ.'[') THEN
            IF(work(l:l).NE.']') THEN
               CALL WRTMSG('<E> MISSING "]"',i2,i2)
               l = l + 1
            END IF
            tsym = work(6:l-1)
         END IF
         IF(work(5:5).EQ.'''') THEN
            IF(work(l:l).NE.'''') THEN
               CALL WRTMSG('<E> QUOTE EXPECTED',i2,i2)
               l = l + 1
            END IF
            tsym = work(6:l-1)
         END IF
         IF(work(l:l).EQ.'?'.OR.work(l:l).EQ.'<'.OR.work(l:l).EQ.'>')   &
     &      THEN
            tsym = work(5:l-1)
         END IF
         CALL LBSUP(tsym)
         IF(tsym.EQ.' ') THEN
            CALL WRTMSG('<W> PUB COMNT SYM FIELD MISSING',i1+4,i2)
            RETURN
         END IF
!
!        CHECK SYM FIELD OF PUB COMNT
!        FOR CKSYM, FIND WHERE THIS FIELD STARTS
!
         j = i1 + INDEX(work(i1:i2),TRIM(tsym)) + 4
         k = i1 + LEN_TRIM(tsym) + 4
         CALL CKSYM(tsym,j,k)
!
         i1 = i2 + 2
         IF(i1.LE.LEN_TRIM(Card)) CYCLE
         RETURN
      END DO
!
      RETURN
      END SUBROUTINE CHKCMP
!
!***********************************************************************
!
      SUBROUTINE CHKCMR(Card)
!
!     CHECK RECORD COMMENTS CARD.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=80) :: Card
!
!     Functions used
!
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
      INTEGER(KIND=4), INTRINSIC :: INDEX, LEN_TRIM
      INTEGER(KIND=4), EXTERNAL :: BREAK, INDEXF
!
!     Local variables
!
      CHARACTER(LEN=1) :: c
      CHARACTER(LEN=71) :: work
      CHARACTER(LEN=7), DIMENSION(20) :: savsym
      CHARACTER(LEN=5) :: tmpnuc
      CHARACTER(LEN=38) :: message
      LOGICAL(KIND=4) :: labfnd, lflag
      INTEGER(KIND=4) :: a, i, icard, iend, iflag, j, jend, k, l, r, z
      INTEGER(KIND=4) :: seqno
!
      labfnd = .FALSE.
!
!     CHECK CARD SEQUENCE.
!
      IF(.NOT.ISEen) CALL WRTMSG('<F> DSID CARD NOT YET SEEN',0,0)
      IF(GABeld.AND.RTYpe.NE.PRType) THEN
         CALL WRTMSG('<F> COMMENT/PRIMARY CARD TYPE CONFLICT',8,9)
      END IF
!     D records have a particle in colum 9; G, P, N, a symbol
      IF(GABeld.AND.(RTYpe.LE.3.OR.RTYpe.EQ.10.OR.RTYpe.GE.12))         &
     &   CALL WRTMSG('<F> Com''nt type illegal in body',7,9)
      IF((RTYpe.EQ.8.AND.Card(9:9).NE.GCOl9).OR.                        &
     &   (RTYpe.EQ.2.AND.Card(9:9).NE.PCOl9).OR.                        &
     &   (RTYpe.EQ.1.AND.Card(9:9).NE.NCOl9).OR.                        &
     &   (RTYpe.EQ.11.AND.Card(8:9).NE.DCOl89)) THEN
         CALL WRTMSG('<F> COMMENT/PRIMARY CARD TYPE CONFLICT',8,9)
      END IF
!     VALIDATE NUCID.
!
      CALL NUCID(Card(1:5),a,z)
      IF(a.EQ.0.OR.z.LT.0) THEN
         CALL WRTMSG('<F> INVALID NUCID',1,5)
      ELSE IF(IZLmsg.NE.' ') THEN
         CALL REPSTR(IZLmsg,'Obsolete formalism','Obsol. form')
         CALL ADDSTR(IZLmsg,1,'<W> ')
         CALL WRTMSG(TRIM(IZLmsg),4,5)
      END IF
      IF(RTYpe.EQ.2) THEN
         tmpnuc=card(1:5)
	 Call Lbsup(tmpnuc)
         IF(NTOtsf.EQ.0) THEN
            IF(tmpnuc.NE.PARnucid)                                      &
     &         CALL WRTMSG('<E> NUCID MISMATCH WITH DSID PARENT',1,5)
         ELSE
            message = '<E> NUCID MISMATCH WITH DSID PARENT'
            DO i = 1, NTOtsf
               IF(tmpnuc.EQ.SFPar(i)) message = ' '
            END DO
            IF(message.NE.' ') CALL WRTMSG(message,1,5)
         END IF
      ELSE IF(a.NE.DSA.OR.z.NE.DSZ) THEN
         CALL WRTMSG('<F> NUCID DOESN''T MATCH DSID',1,5)
      END IF
!     Check for illegal record types (TWB. 930323)
      r = 8
      IF(RTYpe.EQ.11) r = 9
      IF(DSType.NE.2) THEN
         IF((RTYpe.GE.5.AND.RTYpe.LE.7).OR.RTYpe.EQ.11)                 &
     &      CALL WRTMSG('<F> INVALID RECORD TYPE FOR DATASET',8,r)
      ELSE IF(RTYpe.LE.4.OR.RTYpe.EQ.8) THEN
      ELSE IF(DECtyp.EQ.1.AND.RTYpe.EQ.5) THEN
      ELSE IF((DECtyp.EQ.2.OR.DECtyp.EQ.3).AND.RTYpe.EQ.11) THEN
         IF(Card(8:9).NE.DCOl89)                                        &
     &      CALL WRTMSG('<F> INVALID RECORD TYPE FOR DATASET',8,r)
      ELSE IF(DECtyp.EQ.4.AND.RTYpe.EQ.6) THEN
      ELSE IF(DECtyp.EQ.5.AND.RTYpe.EQ.7) THEN
      ELSE IF((DECtyp.GE.8.AND.DECtyp.LE.13).AND.RTYpe.EQ.11) THEN
         IF(Card(8:9).NE.DCOl89)                                        &
     &      CALL WRTMSG('<F> INVALID RECORD TYPE FOR DATASET',8,r)
      ELSE
         CALL WRTMSG('<F> INVALID RECORD TYPE FOR DATASET',8,r)
      END IF
!     D records records have particle in column 9; G, N, and P, symbol
      IF(RTYpe.NE.1.AND.RTYpe.NE.2.AND.RTYpe.NE.8.AND.RTYpe.NE.11)      &
     &   CALL CKBLNK(Card(9:9),9,9)
!     EXTRACT SYM LIST FROM TEXT.
!
      iend = BREAK(Card,10,'$') - 1
      IF(iend.EQ.80) THEN
!        $ NOT FOUND, CHECK FOR GENERAL COMMENT OR OLD STYLE SYM(FLAG).
         IF(Card(10:19).EQ.' ') RETURN
         IF(Card(19:19).NE.' ') THEN
            IF(Card(19:19).EQ.')') THEN
!              Assume this is a footnote comment and continue processing
               CALL WRTMSG('<E> TERMINATE WITH "$"',19,19)
            ELSE IF(Card(8:8).EQ.' ' .AND. rtype.NE.11) THEN
!              Otherwise a general comment and return (TWB. 930323)
               RETURN
            END IF
         END IF
         iend = 19
      END IF
!     No symbols found
      IF(iend.LT.10) RETURN
      IF(INDEXF(Card,iend+2,'LABEL=').EQ.iend+2) labfnd = .TRUE.
      work = Card(10:iend)

      i = INDEX(work,'BAND(')
      IF(i.GT.0) THEN
         i = i + 5
         DO j = i, LEN_TRIM(work)
            IF(work(j:j).EQ.')') EXIT
            IF(work(j:j).NE.',') THEN
               IF(INDEX(BSYmbs,work(j:j)).EQ.0) THEN
                  CALL ADDSTR(BSYmbs,LEN_TRIM(BSYmbs)+1,work(j:j))
               ELSE
                  CALL WRTMSG('<E> BAND SYMBOL ALREADY ASSIGNED',9+j,   &
     &                        9+j)
               END IF
            END IF
         END DO
      END IF
!     Save flags if a BAND comment
! New if block for add seq symbols
! This is where we are trying to work with SEQ as a sub for BAND
      seqno = INDEX(work,'SEQ')
      if (seqno.GT.0) THEN
!       write(*,*) 'Got the sequence'
        seqno = seqno + 5
        DO j = seqno, LEN_TRIM(work)
           IF(work(j:j).EQ.')') EXIT
           IF(work(j:j).NE.',') THEN
              IF(INDEX(BSYmbs,work(j:j)).EQ.0) THEN
                  CALL ADDSTR(BSYmbs,LEN_TRIM(BSYmbs)+1,work(j:j))
              ELSE
                  CALL WRTMSG('<E> SEQ SYMBOL ALREADY ASSIGNED',9+j,    &
     &                       9+j)
              END IF
           END IF
        END DO
      end if
!     Save flags if a SEQ comment
!
!
!     CONVERT ',' BETWEEN SYMS TO ';'.
!
      jend = LEN_TRIM(work)
!     Check for blank field preceding "$"
      IF(jend.EQ.0) THEN
         CALL WRTMSG('<W> MISSING SYMBOL',10,iend)
         RETURN
      END IF
!
!     Check for missing commas between flagged symbols
!     (TWB. 930323)
      i = INDEX(work,')')
      IF(i.GT.0.AND.i.NE.jend) THEN
         IF(work(i+1:i+1).NE.',') THEN
            CALL WRTMSG('<E> MISSING COMMA',10+i,10+i)
            RETURN
         END IF
      END IF
      j = 0
!     J IS PARENTHESIS PARITY.
      lflag = .FALSE.
!     LFLAG INDICATES IF THERE ARE ANY FLAG LISTS.
      DO i = 1, jend
         c = work(i:i)
         IF(c.EQ.'(') THEN
            j = j + 1
            lflag = .TRUE.
         ELSE IF(c.EQ.')') THEN
            j = j - 1
         ELSE IF(c.EQ.','.AND.j.EQ.0) THEN
!           THIS COMMA IS OUTSIDE FLAG LIST, MAKE IT SEMICOLON.
            work(i:i) = ';'
         END IF
      END DO
!
!     CHECK FOR UNBALANCED PARENTHESES.
!
      IF(j.NE.0) THEN
         CALL WRTMSG('<E> UNBALANCED PARENTHESES',10,iend)
         RETURN
      END IF
!
!     CHECK FOR FLAGS IN BODY OF DATA SET.
!
      IF(GABeld.AND.lflag) CALL WRTMSG(                                 &
     &                           '<F> FLAGS ILLEGAL IN BODY OF DATA SET'&
     &                           ,10,iend)
!     LABEL= may not be used with flagged comments
      IF(labfnd.AND.lflag) CALL WRTMSG(                                 &
     &                      '<F> "LABEL=" ILLEGAL with FLAGGED COMMENTS'&
     &                      ,10,iend+7)
!     Check for LABEL= in body of data set
      IF(GABeld.AND.labfnd) CALL WRTMSG(                                &
     &                        '<F> "LABEL=" ILLEGAL IN BODY OF DATA SET'&
     &                        ,iend+2,iend+7)
!     Now make sure the flag and label logicals are set to false
      IF(lflag) labfnd = .FALSE.
      IF(GABeld) THEN
         labfnd = .FALSE.
         lflag = .FALSE.
      END IF
!
      IF(labfnd) THEN
!        Issue informational message and store what column has been
!        renamed
         CALL CKLABEL(Card(10:iend),labfnd)
*JKT* Suppressed comment
C         IF(labfnd)CALL WRTMSG('<I> "LABEL=". FLD CHCKD AS <W> NOT <E>',&
C     &                         10,iend+7)
*JKT*
      END IF
!     PROCESS EACH SYM(FLAG).
!
      k = 0
      l = 1
      DO WHILE (.TRUE.)
!        L IS LEFT HAND COLUMN OF THIS SYM(FLAG).
         r = BREAK(work(1:jend),l,';') - 1
!        R IS RIGHT HAND COLUMN OF THIS SYM(FLAG).
         i = BREAK(work(1:r),l,'(')
!        I IS LOCATION OF (, IF ANY, OR END OF THIS SYM(FLAG).
         CALL CKSYM(work(l:i-1),9+l,9+i-1)
!        Check for duplicate SYM
         k = k + 1
         savsym(k) = work(l:i-1)
         IF(k.GT.1) THEN
            DO j = 1, k - 1
               IF(savsym(j).EQ.savsym(k))                               &
     &            CALL WRTMSG('<E> SYM MAY NOT BE REPEATED',9+l,9+i-1)
            END DO
         END IF
         IF(i.LT.r.AND.lflag) THEN
!           THERE IS A FLAG LIST AND IT SHOULD BE PROCESSED.
            DO j = i + 1, r - 1
!              CHECK EACH CHARACTER IN FLAG LIST.
               c = work(j:j)
               IF(c.NE.' '.AND.c.NE.',') THEN
!                 IGNORE BLANKS AND COMMAS.
!                 CHECK IF THIS FLAG HAS BEEN SEEN BEFORE.
                  icard = RTYpe - 3
                  IF(RTYpe.EQ.11) icard = 6
!                 FIX FOR D CARDS.
!                 Check to see if record type is allowed to have flags;
!                 Also keeps program from bombing out (TWB. 951019)
                  IF(icard.LE.0.OR.icard.GT.6) THEN
                     CALL WRTMSG('<E> FLAG NOT ALLOWED',9+j,9+j)
                     CYCLE
                  ELSE
                     DO iflag = 1, NFLag(icard)
                        IF(FLAg(iflag,icard).EQ.c) GO TO 5
                     END DO
                  END IF
!                 FLAG NOT SEEN BEFORE, ADD IT TO LIST.
                  NFLag(icard) = iflag
                  FLAg(iflag,icard) = c
                  FLGref(iflag,icard) = .FALSE.
               END IF
    5       END DO
!           Check for reserved flags (TWB. 930312)
            IF((RTYpe.GE.5.AND.RTYpe.LE.7).AND.(c.EQ.'C'.OR.c.EQ.'?'))  &
     &         CALL WRTMSG('<E> FLAG RESERVED',8+j,8+j)
            IF(RTYpe.EQ.8.AND.                                          &
     &         (c.EQ.'*'.OR.c.EQ.'&'.OR.c.EQ.'@'.OR.c.EQ.'%'))          &
     &         CALL WRTMSG('<E> FLAG RESERVED',8+j,8+j)
         END IF
!        PREPARE FOR NEXT SYM(FLAG).
         l = r + 2
!        IF MORE, CONTINUE.
         IF(l.LT.jend) CYCLE
         CALL CKSPEC(Card)
         EXIT
      END DO
!
      RETURN
      END SUBROUTINE CHKCMR
!
!***********************************************************************
!
      SUBROUTINE CHKCNT(Card)
!
!     CHECK CONTINUATION CARD.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=80) :: Card
!
!     Functions used
!
      INTEGER(KIND=4), INTRINSIC :: INDEX, LEN_TRIM
      INTEGER(KIND=4), EXTERNAL :: SPAN
!
!     Local variables
!
      CHARACTER(LEN=30) :: tdsid
      LOGICAL(KIND=4) :: conflct
      INTEGER(KIND=4) :: d, i, j, l, r
!
      conflct = .FALSE.
!     CHECK CARD SEQUENCE.
!
      IF(.NOT.ISEen) CALL WRTMSG('<F> DSID CARD NOT YET SEEN',0,0)
      IF(GABeld.AND.RTYpe.NE.PRVtyp) THEN
         PRType = PRVtyp
         CALL WRTCRD(Card)
         CALL WRTMSG('<F> CONTIN/PRIMARY CARD TYPE CONFLICT',8,9)
         conflct = .TRUE.
      ELSE IF(.NOT.GABeld.AND.                                          &
     &        ((RTYpe.GE.4.AND.RTYpe.LE.8).OR.RTYpe.EQ.11)) THEN
         CALL WRTMSG('<E> MISSING PRIMARY CARD',1,9)
      END IF
!     D records have a particle in colum 9; G, a symbol
      IF((RTYpe.EQ.8.AND.Card(9:9).NE.GCOl9).OR.                        &
     &   (RTYpe.EQ.11.AND.Card(8:9).NE.DCOl89))                         &
     &   CALL WRTMSG('<F> CONTIN/PRIMARY CARD TYPE CONFLICT',8,9)
      IF((RTYpe.GT.0.AND.RTYpe.LT.3).OR.(RTYpe.GT.8.AND.RTYpe.LT.11).OR.&
     &   RTYpe.EQ.13) THEN
         write(*,*) 'rtyp is : ', RTYpe
         CALL WRTMSG('<E> NO CONTINUATIONS FOR THIS CARD TYPE',8,9)
         RETURN
      END IF
!
!     VALIDATE NUCID.
!
      CALL CKNCID(Card)
!     D records records have particle in column 9; G, symbol
      IF(RTYpe.NE.8.AND.RTYpe.NE.11) CALL CKBLNK(Card(9:9),9,9)
!
!     PICK UP EACH FIELD AND CHECK IT.
!
      r = LEN_TRIM(Card)
      l = 10
      IF(l.GE.r) THEN
         CALL WRTMSG('<W> BLANK CONTINUATION CARD',10,80)
      END IF
!
!     Check HISTORY record separately
!
      IF(RTYpe.EQ.14) THEN
         CALL CHKH(Card,2)
         RETURN
      END IF
!
!     check ID record cont separately
!
      IF(RTYpe.EQ.0) THEN
!
!        Must write out all ID record continuations due to links
!        between them
         WRITE(out,'(I6,2A)') NSEq, '.  ', Card
!        Set flag that the card has been written.
         CRDwrt = .TRUE.
!        Check for conflict
         If(rtype . NE. PRVtyp) THEN
            PRType = PRVtyp
            CALL WRTMSG('<F> CONTIN/PRIMARY CARD TYPE CONFLICT',8,9)
            conflct = .TRUE.
	 EndIf 
!
         CALL CKBLNK(Card(40:80),40,80)
!
!        for this continuation to be valid, previous DSID field should
!        have ended with , and saved in DSIDSV
!
!        For correct parsing need to add comma (TWB. 930309)
         j = LEN_TRIM(DSIdsv)
         write(*,*) 'len j is:', j
         write(*,*) 'string is: ', DSIdsv
         IF(DSIdsv(j:j).NE.',') THEN
            CALL WRTMSG('<E> PREV DSID SHOULD END WITH COMMA',0,0)
            CALL ADDSTR(DSIdsv,j+1,',')
         END IF
         j = LEN_TRIM(DSIdsv)
         TOTcon = TOTcon + 1
         CONpos(TOTcon) = j
         tdsid = ' '
         tdsid = Card(10:39)
         CALL ADDSTR(DSIdsv,j+1,tdsid)
         resnuc=card(1:5)
         i = LEN_TRIM(tdsid)
         IF(tdsid(i:i).NE.',') THEN
!
!           No more DSID cont expected, check DSID field now
!
            CALL CKDSID(DSIdsv)
            IF(DSType.EQ.2.AND.DECtyp.NE.0.AND.DECtyp.NE.7)             &
     &         CALL NUC2DEC(Card(1:5),DSIdsv)
            EXDscn = .FALSE.
         END IF
         RETURN
      END IF
!
!     check PN record cont
!
      IF(RTYpe.EQ.12) THEN
         RETURN
      END IF
!
      IF(RTYpe.EQ.7) CALL ECSTORE(Card)
      IF(RTYpe.EQ.6.OR.RTYpe.EQ.7) THEN
         IF(INDEX(Card,'EAV').GT.0) THEN
            EAVpres = .TRUE.
            SB_enum = NSEq
            SB_ecard = Card
            DOEavchk = .TRUE.
         END IF
      END IF
      DO WHILE (.TRUE.)
         l = SPAN(Card(1:r),l,' ')
         d = l - 1 + INDEX(Card(l:r),'$')
         IF(d.EQ.l) THEN
            CALL WRTMSG('<E> EMPTY FIELD',d,d)
         ELSE IF(d.GT.l) THEN
            CALL CKCFLD(Card(l:d-1),l,d-1,conflct)
         ELSE
            CALL CKCFLD(Card(l:r),l,r,conflct)
            RETURN
         END IF
         l = d + 1
         IF(l.GT.r) RETURN
      END DO
!
      RETURN
      END SUBROUTINE CHKCNT
!
!***********************************************************************
!
      SUBROUTINE CHKD(Card)
!
!     CHECK [DELAYED] PARTICLE CARD.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=80) :: Card
!
!     Functions used
!
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
      INTEGER(KIND=4), INTRINSIC :: INDEX
      INTEGER(KIND=4), EXTERNAL :: SPAN
!
!     Local variables
!
      CHARACTER(LEN=38) :: message
!
!     CHECK CARD SEQUENCE.
!
      IF(.NOT.ISEen) CALL WRTMSG('<F> DSID CARD NOT YET SEEN',0,0)
      GABeld = .TRUE.
!
!     Check to see if this is a legal (delayed-)particle record for
!     the data set
      message = '<F> "" CARD ILLEGAL FOR THIS DATASET'
      CALL ADDSTR(message,6,Card(8:9))
      IF(DSType.NE.2) THEN
         CALL WRTMSG(TRIM(message),8,9)
      ELSE
         IF(Card(8:9).NE.DCOl89) CALL WRTMSG(TRIM(message),8,9)
      END IF
!
!     VALIDATE NUCID.
!
      CALL CKNCID(Card)
!
!     VALIDATE DATA FIELDS.
!
      IF(DLAbon(1)) THEN
         CALL CKE(Card(10:19),10,19,'<W>')
      ELSE
         CALL CKE(Card(10:19),10,19,'<E>')
      END IF
      CALL CKU(Card(20:21),20)
      IF(Card(22:29).EQ.' ') THEN
      ELSE IF(Card(SPAN(Card,22,' '):29).NE.'WEAK') THEN
         IPSeen = .TRUE.
         IF(DLAbon(3)) THEN
            CALL CKVPRN(Card(22:29),22,.TRUE.,'<W>')
         ELSE
            CALL CKVPRN(Card(22:29),22,.TRUE.,'<E>')
         END IF
      END IF
      CALL CKU(Card(30:31),30)
      IF(DLAbon(5)) THEN
         CALL CKE(Card(10:19),10,19,'<W>')
      ELSE
         CALL CKE(Card(10:19),10,19,'<E>')
      END IF
      IF(DLAbon(6)) THEN
         CALL CKV(Card(40:49),40,.TRUE.,'<W>')
      ELSE
         CALL CKV(Card(40:49),40,.TRUE.,'<E>')
      END IF
      CALL CKU(Card(50:55),50)
      IF(DLAbon(8)) THEN
         CALL CKL(Card(56:64),56,64,'<W>')
      ELSE
         CALL CKL(Card(56:64),56,64,'<E>')
      END IF
      CALL CKBLNK(Card(65:76),65,76)
      CALL CKC(Card(77:77),77,77)
      CALL CKCOIN(Card(78:78),78,78)
      CALL CKBLNK(Card(79:79),79,79)
      CALL CKQ(Card(80:80),80,80)
!
!     CONSISTENCY BETWEEN UNC FIELDS AND THE MAIN VALUE FIELDS
!
      CALL CKVU(Card(10:19),Card(20:21),10,21)
      CALL CKVU(Card(22:29),Card(30:31),22,31)
      CALL CKVU(Card(40:49),Card(50:55),40,55)
!
      RETURN
      END SUBROUTINE CHKD
!
!***********************************************************************
!
      SUBROUTINE CHKE(Card)
!
!     CHECK ELECTRON CAPTURE (BETA+) CARD.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=80) :: Card
!
!     Functions used
!
      INTEGER(KIND=4), INTRINSIC :: INDEX
      INTEGER(KIND=4), EXTERNAL :: SPAN, TYPSTR
      REAL(KIND=4), INTRINSIC :: ABS, AMIN1, SQRT
!
!     Local variables
!
      REAL(KIND=4) :: dib, die, dti, dx, ib, ie, test, ti, x
!
!     Save information for electron-capture fraction check
!
      ENUm = NSEq
      ECArd = Card
      IEStr = Card(32:39)
      DIEstr = Card(40:41)
      TIStr = Card(65:74)
      DTIstr = Card(75:76)
      DOEcchk = .TRUE.
!     Save information for IB/EAV consistency check
      DOEavchk = .TRUE.
      IBPres = (Card(22:29).NE.' ')
      B_Enum = NSEq
      B_Ecard = Card
!     CHECK CARD SEQUENCE.
!
      IF(.NOT.ISEen) CALL WRTMSG('<F> DSID CARD NOT YET SEEN',0,0)
      IF(DSType.NE.2.OR.DECtyp.NE.5)                                    &
     &   CALL WRTMSG('<F> E CARD ILLEGAL FOR THIS DATASET',8,8)
      GABeld = .TRUE.
!
!     VALIDATE NUCID.
!
      CALL CKNCID(Card)
      CALL CKBLNK(Card(9:9),9,9)
!
!     Make sure TI is given if IE and IB present
      IF((Card(22:29).NE.' '.AND.Card(32:39).NE.' ').AND.Card(65:74)    &
     &   .EQ.' ') THEN
         CALL WRTMSG('<W> TI field missing',65,74)
         DOEcchk = .FALSE.
         ECArd = ' '
         IEStr = ' '
         DIEstr = ' '
         TIStr = ' '
         DTIstr = ' '
      END IF
!
!     VALIDATE DATA FIELDS.
!
      IF(ELAbon(1)) THEN
         CALL CKE(Card(10:19),10,19,'<W>')
      ELSE
         CALL CKE(Card(10:19),10,19,'<E>')
      END IF
      CALL CKU(Card(20:21),20)
      IF(Card(22:29).EQ.' ') THEN
      ELSE IF(Card(SPAN(Card,22,' '):29).NE.'WEAK') THEN
         IBSeen = .TRUE.
         IF(ELAbon(3)) THEN
            CALL CKVPRN(Card(22:29),22,.TRUE.,'<W>')
         ELSE
            CALL CKVPRN(Card(22:29),22,.TRUE.,'<E>')
         END IF
      END IF
      CALL CKU(Card(30:31),30)
      IF(Card(32:39).EQ.' ') THEN
      ELSE IF(Card(SPAN(Card,32,' '):39).EQ.'WEAK') THEN
         IF(TOTal) CALL WRTMSG('<E> No EC for total ionization',32,41)
      ELSE
         IBSeen = .TRUE.
         IF(ELAbon(5)) THEN
            CALL CKVPRN(Card(32:39),32,.TRUE.,'<W>')
         ELSE
            CALL CKVPRN(Card(32:39),32,.TRUE.,'<E>')
         END IF
         IF(TOTal) CALL WRTMSG('<E> No EC for total ionization',32,41)
      END IF
      CALL CKU(Card(40:41),40)
      IF(ELAbon(7)) THEN
         CALL CKV(Card(42:49),42,.TRUE.,'<W>')
      ELSE
         CALL CKV(Card(42:49),42,.TRUE.,'<E>')
      END IF
      CALL CKU(Card(50:55),50)
      CALL CKBLNK(Card(56:64),56,64)
      IF(Card(65:74).EQ.' ') THEN
         IF(NBSeen) THEN
            CALL CNVS2U(Card(22:29),Card(30:31),x,dx)
            IF(Card(30:30).EQ.'L') THEN
               x = x/2.
               dx = x
            END IF
            IF(Card(30:30).EQ.'G') THEN
               x = (100.+x)/2.
               dx = x
            END IF
            IF(Card(30:30).EQ.'A') dx = 0.5*x
            TOTin = TOTin + x
            DTOtin = DTOtin + dx*dx
            CALL CNVS2U(Card(32:39),Card(40:41),x,dx)
            IF(Card(40:40).EQ.'L') THEN
               x = x/2.
               dx = x
            END IF
            IF(Card(40:40).EQ.'G') THEN
               x = (100.+x)/2.
               dx = x
            END IF
            IF(Card(40:40).EQ.'A') dx = 0.5*x
            TOTin = TOTin + x
            DTOtin = DTOtin + dx*dx
         END IF
      ELSE IF(Card(SPAN(Card,65,' '):74).EQ.'WEAK') THEN
         IF(NBSeen) THEN
            CALL CNVS2U(Card(22:29),Card(30:31),x,dx)
            IF(Card(30:30).EQ.'L') THEN
               x = x/2.
               dx = x
            END IF
            IF(Card(30:30).EQ.'G') THEN
               x = (100.+x)/2.
               dx = x
            END IF
            IF(Card(30:30).EQ.'A') dx = 0.5*x
            TOTin = TOTin + x
            DTOtin = DTOtin + dx*dx
            CALL CNVS2U(Card(32:39),Card(40:41),x,dx)
            IF(Card(40:40).EQ.'L') THEN
               x = x/2.
               dx = x
            END IF
            IF(Card(40:40).EQ.'G') THEN
               x = (100.+x)/2.
               dx = x
            END IF
            IF(Card(40:40).EQ.'A') dx = 0.5*x
            TOTin = TOTin + x
            DTOtin = DTOtin + dx*dx
         END IF
      ELSE
         IBSeen = .TRUE.
         IF(NBSeen) THEN
            CALL CNVS2U(Card(65:74),Card(75:76),x,dx)
            IF(Card(75:75).EQ.'L') THEN
               x = x/2.
               dx = x
            END IF
            IF(Card(75:75).EQ.'G') THEN
               x = (100.+x)/2.
               dx = x
            END IF
            IF(Card(75:75).EQ.'A') dx = 0.5*x
            TOTin = TOTin + x
            DTOtin = DTOtin + dx*dx
         END IF
         IF(ELAbon(9)) THEN
            CALL CKVPRN(Card(65:74),65,.TRUE.,'<W>')
         ELSE
            CALL CKVPRN(Card(65:74),65,.TRUE.,'<E>')
         END IF
      END IF
      CALL CKU(Card(75:76),75)
      CALL CKC(Card(77:77),77,77)
      CALL CKUN(Card(78:79),78,79)
      CALL CKQ(Card(80:80),80,80)
!
!     CONSISTENCY BETWEEN UNC FIELDS AND THE MAIN VALUE FIELDS
!
      CALL CKVU(Card(10:19),Card(20:21),10,21)
      CALL CKVU(Card(22:29),Card(30:31),22,31)
      CALL CKVU(Card(32:39),Card(40:41),32,41)
      CALL CKVU(Card(42:49),Card(50:55),42,55)
      CALL CKVU(Card(65:74),Card(75:76),65,76)
!     Check for consistency between IB+IE and TI
      IF(Card(65:74).NE.' '.AND.                                        &
     &   (Card(22:29).NE.' '.OR.Card(32:39).NE.' ')) THEN
         CALL CNVS2U(Card(22:29),Card(30:31),ib,dib)
         CALL CNVS2U(Card(32:39),Card(40:41),ie,die)
         CALL CNVS2U(Card(65:74),Card(75:76),ti,dti)
         IF(ib.EQ.0.0) THEN
            IF(ie.NE.ti.OR.die.NE.dti) THEN
               TOTerr = TOTerr + 1
               IF(.NOT.NOErr) CALL WRTMS2('<E> IE .NE. TI',32,41,65,76)
            END IF
         ELSE IF(ie.EQ.0.0) THEN
            IF(ib.NE.ti.OR.dib.NE.dti) THEN
               TOTerr = TOTerr + 1
               IF(.NOT.NOErr) CALL WRTMS2('<E> IB .NE. TI',22,29,65,76)
            END IF
         ELSE
            IF(dti.NE.0) THEN
               test = dti**2
            ELSE
               test = (0.05*ti)**2
            END IF
            IF(dib.NE.0) THEN
               test = test + dib**2
            ELSE
               test = test + (0.05*ib)**2
            END IF
            IF(die.NE.0) THEN
               test = test + die**2
            ELSE
               test = test + (0.05*ie)**2
            END IF
            test = SQRT(test)
            IF(ABS(ti-ib-ie).GT.test) THEN
               IF(dti.NE.0.0.OR.dib.NE.0.0.OR.die.NE.0.0) THEN
                  TOTerr = TOTerr + 1
                  CALL WRTMS2('<E> IB+IE .NE. TI',22,41,65,76)
               ELSE IF(ABS(ti-ib-ie).GT.1.01*AMIN1(ib,ie)) THEN
                  TOTerr = TOTerr + 1
                  CALL WRTMS2('<E> IB+IE .NE. TI',22,41,65,76)
               END IF
            END IF
         END IF
      END IF
!     Check for consistency of non-numeric uncertanties
      IF(Card(65:74).NE.' ') THEN
         IF(TYPSTR(Card(75:76)).EQ.2) THEN
            IF(Card(22:29).NE.' '.AND.Card(30:31).NE.Card(75:76))       &
     &         CALL WRTMS2('<E> Inconsistent uncert.',30,31,75,76)
            IF(Card(32:39).NE.' '.AND.Card(40:41).NE.Card(75:76))       &
     &         CALL WRTMS2('<E> Inconsistent uncert.',40,41,75,76)
            IF(Card(42:49).NE.' '.AND.INDEX(Card(50:55),'SY').EQ.0) THEN
               IF(Card(75:76).EQ.'AP'.AND.INDEX(Card(50:55),'AP').EQ.0) &
     &            CALL WRTMS2('<E> Inconsistent uncert.',50,55,75,76)
               IF(Card(75:75).EQ.'G'.AND.INDEX(Card(50:55),'L').EQ.0)   &
     &            CALL WRTMS2('<E> Inconsistent uncert.',50,55,75,76)
               IF(Card(75:75).EQ.'L'.AND.INDEX(Card(50:55),'G').EQ.0)   &
     &            CALL WRTMS2('<E> Inconsistent uncert.',50,55,75,76)
            END IF
         END IF
      ELSE IF(Card(22:29).NE.' '.AND.Card(42:49).NE.' '.AND.            &
     &        INDEX(Card(50:55),'SY').EQ.0) THEN
         IF(TYPSTR(Card(30:31)).EQ.2) THEN
            IF(Card(30:31).EQ.'AP'.AND.INDEX(Card(50:55),'AP').EQ.0)    &
     &         CALL WRTMS2('<E> Inconsistent uncert.',30,31,50,55)
            IF(Card(30:30).EQ.'G'.AND.INDEX(Card(50:55),'L').EQ.0)      &
     &         CALL WRTMS2('<E> Inconsistent uncert.',30,31,50,55)
            IF(Card(30:30).EQ.'L'.AND.INDEX(Card(50:55),'G').EQ.0)      &
     &         CALL WRTMS2('<E> Inconsistent uncert.',30,31,50,55)
         END IF
      ELSE IF(Card(32:39).NE.' '.AND.Card(42:49).NE.' '.AND.            &
     &        INDEX(Card(50:55),'SY').EQ.0) THEN
         IF(TYPSTR(Card(40:41)).EQ.2) THEN
            IF(Card(40:41).EQ.'AP'.AND.INDEX(Card(50:55),'AP').EQ.0)    &
     &         CALL WRTMS2('<E> Inconsistent uncert.',40,41,50,55)
            IF(Card(40:40).EQ.'G'.AND.INDEX(Card(50:55),'L').EQ.0)      &
     &         CALL WRTMS2('<E> Inconsistent uncert.',40,41,50,55)
            IF(Card(40:40).EQ.'L'.AND.INDEX(Card(50:55),'G').EQ.0)      &
     &         CALL WRTMS2('<E> Inconsisten uncert.',40,41,50,55)
         END IF
      END IF
!
      RETURN
      END SUBROUTINE CHKE
!
!***********************************************************************
!
      SUBROUTINE CHKEND(Card)
!
!     CHECK END OF DATA SET CONDITIONS.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=80) :: Card
!
!     Functions used
!
      INTEGER(KIND=4) :: INDEX, LEN_TRIM
      REAL(KIND=4), INTRINSIC :: ABS, FLOAT, SQRT
!
!     Local variables
!
      CHARACTER(LEN=38) :: msg, msg2
      CHARACTER(LEN=2) :: sdx
      CHARACTER(LEN=80) :: strout
      CHARACTER(LEN=10) :: sx
      LOGICAL(KIND=4) :: didit, didit2, match
      INTEGER(KIND=4) :: begin, i, icard, iflag, irt, j
      REAL(KIND=4) :: dg1, dg2, dx, g1, g2, x
!
!     Data initializations
!
      DATA MSG/'<W> FLAG NOT REFERENCED:   R TYPE: '/
!
      didit = .FALSE.
!
!     make sure this card is truly blank
!
      IF(Card.NE.' ') CALL WRTMSG('<F> NON-BLANK END CARD',0,0)
!
!     CHECK CARD SEQUENCE.
!
      IF(.NOT.ISEen) THEN
         CALL WRTMSG('<E> END CARD NOT IN ANY DATASET',0,0)
         RETURN
      END IF
!
!     MAKE SURE THAT CARDS THAT SHOULD BE IN DATASETS ARE THERE.
!
      IF(DSType.EQ.1) THEN
         IF(.NOT.QSEen) CALL CHKBRK(didit,                              &
     &                        '<E> Q CARD REQUIRED FOR ADOPTED DATASETS'&
     &                        )
!        Check on consistency between Adopted DSID and presence of
!        gammas in data set
         IF(GCOunt.GT.0) THEN
            IF(INDEX(DSIdsv,', GAMMAS').EQ.0) CALL CHKBRK(didit,        &
     &         '<E> ", GAMMAS" MISSING IN ADOPTED DSID')
         ELSE
            IF(INDEX(DSIdsv,', GAMMAS').GT.0)                           &
     &         CALL CHKBRK(didit,'<E> ", GAMMAS" IN ADOPTED DSID')
         END IF
      ELSE IF(DSType.EQ.2) THEN
!        Parent must be given except for IT and SF (TWB. 930309)
!        Parent must now be given for all except unknown (19990608)
         IF(.NOT.DECtyp.EQ.0.AND..NOT.PSEen)                            &
     &      CALL CHKBRK(didit,'<E> P CARD REQUIRED FOR DECAY DATASETS')
!        Add checks between data seen and fields on N card
!        (TWB. 930315)
         IF(.NOT.NSEen.AND.DECtyp.NE.7.AND.DECtyp.NE.0) THEN
            CALL CHKBRK(didit,'<W> N CARD SHOULD BE GIVEN IF BR KNOWN')
         ELSE IF(PNAbs) THEN
            IF(.NOT.BRSeen.AND.DECtyp.NE.7.AND.DECtyp.NE.0)             &
     &         CALL CHKBRK(didit,                                       &
     &         '<F> BR MUST BE GIVEN FOR PN OPTIONS 2-4')
            IF(RISeen.AND..NOT.NRSeen) CALL CHKBRK(didit,               &
     &         '<F> NR MUST BE GIVEN FOR PN OPTIONS 2-4')
            IF(TISeen.AND..NOT.NTSeen) CALL CHKBRK(didit,               &
     &         '<F> NT MUST BE GIVEN FOR PN OPTIONS 2-4')
         ELSE
            IF(.NOT.BRSeen.AND.DECtyp.NE.7.AND.DECtyp.NE.0)             &
     &         CALL CHKBRK(didit,'<W> BR SHOULD BE GIVEN IF KNOWN')
            IF(.NOT.PNSeen.AND.DECtyp.NE.0.AND.NPCard.LE.1) THEN
               IF(RISeen.AND..NOT.NRSeen)                               &
     &            CALL CHKBRK(didit,'<W> NR SHOULD BE GIVEN IF KNOWN')
               IF(TISeen.AND..NOT.NTSeen)                               &
     &            CALL CHKBRK(didit,'<W> NT SHOULD BE GIVEN IF KNOWN')
               IF(IBSeen.AND..NOT.NBSeen)                               &
     &            CALL CHKBRK(didit,'<W> NB SHOULD BE GIVEN IF KNOWN')
               IF(IPSeen.AND..NOT.NPSeen.AND.                           &
     &            (DECtyp.GE.8.AND.DECtyp.LE.13.AND.DCOl89.NE.'**'))    &
     &            CALL CHKBRK(didit,'<W> NP SHOULD BE GIVEN IF KNOWN')
            END IF
         END IF
      ELSE IF(DSType.EQ.3.AND.PNAbs) THEN
         IF(RISeen.AND..NOT.NRSeen)                                     &
     &      CALL CHKBRK(didit,'<E> NR MUST BE GIVEN FOR PN OPTIONS 2-4')
         IF(TISeen.AND..NOT.NTSeen)                                     &
     &      CALL CHKBRK(didit,'<E> NT MUST BE GIVEN FOR PN OPTIONS 2-4')
      END IF
!     Should also give warning if PN present but no intensities
!     (TWB. 930309)
      IF(.NOT.(RISeen.OR.TISeen)) THEN
         IF(PNSeen) CALL CHKBRK(didit,                                  &
     &                         '<W> PN GIVEN BUT NO GAMMA INTENSITIES')
      ELSE IF(.NOT.PNSeen) THEN
         IF(DSType.EQ.1) THEN
            CALL CHKBRK(didit,'<W> PN NOT GIVEN-WILL USE OPTION 6')
         ELSE IF(DSType.NE.2) THEN
            CALL CHKBRK(didit,'<W> PN NOT GIVEN-WILL USE OPTION 5')
         ELSE
            CALL CHKBRK(didit,'<W> PN NOT GIVEN-WILL USE OPTION 3')
         END IF
      END IF
!
      IF(NPT12.GT.0) THEN
         IF(NPT12.NE.NPCard.AND.PSEen)                                  &
     &      CALL CHKBRK(didit,'<W> CHECK CONSISTENCY BETWEEN P AND DSID'&
     &      )
      ELSE IF(NTOtsf.GT.0) THEN
         IF(NTOtsf.NE.NPCard.AND.PSEen)                                 &
     &      CALL CHKBRK(didit,'<W> CHECK CONSISTENCY BETWEEN P AND DSID'&
     &      )
      END IF
!
!     Check to see if sum of feedings is consistent for 100 per decay
!     via mode
      IF((DECtyp.EQ.1.OR.DECtyp.EQ.4.OR.DECtyp.EQ.5).AND.FN.NE.0.0.AND. &
     &   TOTin.NE.0) THEN
         x = FN*TOTin
         dx = x*SQRT((DFN/FN)**2+(DTOtin/(TOTin*TOTin)))
!        Convert to string and back to round off on error
         CALL CNVU2S(x,dx,sx,10,sdx,2)
         IF(sx.NE.'**********') THEN
            CALL CNVS2U(sx,sdx,x,dx)
         END IF
         IF(dx.LE.0.1.AND.x.GE.99.9.AND.x.LE.100.1) GO TO 10
         DO i = 1, 5
            j = i
            IF((x-FLOAT(j)*dx).LE.100.0.AND.(x+FLOAT(j)*dx).GE.100.0)   &
     &         GO TO 5
         END DO
         j = j + 1
    5    IF(j.GT.1) THEN
            strout = 'Sum of A/B/E intensities='
            CALL LBSUP(sx)
            CALL LBSUP(sdx)
            CALL ADDSTR(strout,LEN_TRIM(strout)+2,sx)
            CALL ADDSTR(strout,LEN_TRIM(strout)+2,sdx)
            j = j - 1
            IF((x+FLOAT(j)*dx).LE.100.0) THEN
!              Warning only if it does not sum up to 100
               strout(LEN_TRIM(strout)+2:)                              &
     &             = 'less than 100 by more than'
               msg2 = '<W> CHECK A/B/E INTENSITIES'
            ELSE
               strout(LEN_TRIM(strout)+2:) = 'exceeds 100 by more than'
               IF(j.LE.2) THEN
!                 Warning if close but sum exceeds 100
                  msg2 = '<W> CHECK A/B/E INTENSITIES'
               ELSE
!                 Error if not close and sum exceeds 100
                  msg2 = '<E> CHECK A/B/E INTENSITIES'
               END IF
            END IF
            WRITE(strout(LEN_TRIM(strout)+2:),'(I1)') j
            CALL ADDSTR(strout,LEN_TRIM(strout)+2,'sigma(s)')
            IF(INDEX(msg2,'<W>').GT.0) THEN
               TOTwar = TOTwar + 1
               IF(.NOT.NOWarn) THEN
                  CALL CHKBRK(didit,' ')
                  CALL WRTCHK(1)
                  WRITE(out,'(9X,A,T92,A)') strout(1:LEN_TRIM(strout)), &
     &                           msg2(1:LEN_TRIM(msg2))
                END IF
            ELSE
               TOTerr = TOTerr + 1
               IF(.NOT.NOErr) THEN
                  CALL CHKBRK(didit,' ')
                  CALL WRTCHK(1)
                  WRITE(out,'(9X,A,T92,A)') strout(1:LEN_TRIM(strout)), &
     &                           msg2(1:LEN_TRIM(msg2))
               END IF
            END IF
         END IF
      END IF
!
   10 IF(NG.GT.0) THEN
         IF(NG.GT.1) THEN
            begin = 0
   15       begin = begin + 1
            match = .FALSE.
   20       IF(begin.EQ.NG) GO TO 30
            DO j = 1, NG
               IF(j.EQ.begin) CYCLE
               IF(EGStr(begin).EQ.EGStr(j)) THEN
                  match = .TRUE.
                  IF(GSYmb(begin).EQ.GSYmb(j)) GO TO 25
                  IF(.NOT.NOWarn) THEN
                     CALL CHKBRK(didit,' ')
                     CALL WRTCHK(1)
                     WRITE(out,'(1X,I5,6A)') GSEq(begin),'.  <W> "',    &
     &                    GSYmb(begin),'" for ',EGStr(begin),           &
     &                    ' does not match'
                     WRITE(out,'(1X,I5,5A,T92,A)') GSEq(j), '.      "', &
     &                    GSYmb(j),'" for ', EGStr(j),                  &
     &                    '<W> INCONSISTENT USE OF "*", "@", "&"'
                  END IF
                  TOTwar = TOTwar + 1
                  GO TO 25
               END IF
            END DO
            IF(.NOT.match) THEN
!              No exact string match - try energy including
!              uncertainties
               CALL CNVS2U(EGStr(begin),DEGstr(begin),g1,dg1)
               IF(dg1.EQ.0.0) dg1 = 0.1
               DO j = 1, NG
                  IF(j.EQ.begin) CYCLE
                  CALL CNVS2U(EGStr(j),DEGstr(j),g2,dg2)
                  IF(dg2.EQ.0.0) dg2 = 0.1
                  IF(ABS(g2-g1).LE.(dg1+dg2)) THEN
                     match = .TRUE.
                     IF(GSYmb(begin).EQ.GSYmb(j)) GO TO 25
                     IF(.NOT.NOWarn) THEN
                        CALL CHKBRK(didit,' ')
                        CALL WRTCHK(1)
                        WRITE(out,'(1X,I5,6A)') GSEq(begin),'.  <W> "', &
     &                       GSYmb(begin),'" for ',EGStr(begin)//       &
     &                       DEGstr(begin),' does not match'
                        WRITE(out,'(1X,I5,5A,T92,A)') GSEq(j),          &
     &                       '.      "',GSYmb(j),'" for ', EGStr(j)//   &
     &                       DEGstr(j),                                 &
     &                       '<W> INCONSISTENT USE OF "*", "@", "&"'
                     END IF
                     TOTwar = TOTwar + 1
                     GO TO 25
                  END IF
               END DO
               IF(.NOT.NOWarn) THEN
                  CALL CHKBRK(didit,' ')
                  CALL WRTCHK(1)
                  WRITE(out,'(1X,I5,6A,T92,A)') GSEq(begin),'.  <W> "', &
     &                 GSYmb(begin),'" for ',EGStr(begin),              &
     &                 '<W> INCONSISTENT USE OF "*", "@", "&"'
               END IF
               TOTwar = TOTwar + 1
            END IF
            GO TO 15
   25       DO i = j, NG - 1
               GSEq(i) = GSEq(i+1)
               GSYmb(i) = GSYmb(i+1)
               EGStr(i) = EGStr(i+1)
            END DO
            NG = NG - 1
            GO TO 20
         ELSE
            IF(.NOT.NOWarn) THEN
               CALL CHKBRK(didit,' ')
               CALL WRTCHK(1)
               WRITE(out,'(1X,I5,6A)') GSEq(NG),'.  <W> "', GSYmb(NG),  &
     !             '" for ', EGStr(NG)
            END IF
            TOTwar = TOTwar + 1
         END IF
      END IF
!
   30 IF(EXDscn) THEN
         CALL CHKBRK(didit,'<F> DSID CONT CARD NOT FOUND')
         DSIdsv = ' '
      END IF
!
!     RESET SEEN FLAGS.
!
      ISEen = .FALSE.
      NSEen = .FALSE.
      NRSeen = .FALSE.
      NTSeen = .FALSE.
      BRSeen = .FALSE.
      NBSeen = .FALSE.
      NPSeen = .FALSE.
      PSEen = .FALSE.
      QSEen = .FALSE.
      RISeen = .FALSE.
      TISeen = .FALSE.
      GABeld = .FALSE.
      PNSeen = .FALSE.
      PNAbs = .FALSE.
      IBSeen = .FALSE.
      IPSeen = .FALSE.
!     Reset counters
      NPCard = 0
      Do i=1,2
         NSAve9(i) = 0
      EndDo
      NG = 0
!     CHECK COMMENT FLAG TABLE FOR ALL FLAGS REFERENCED.
!
      DO icard = 1, 6
         DO iflag = 1, NFLag(icard)
            IF(.NOT.FLGref(iflag,icard)) THEN
               msg(25:25) = FLAg(iflag,icard)
               irt = icard + 3
               IF(icard.EQ.6) irt = 11
               msg(35:36) = CHRrec(irt)
               CALL CHKBRK(didit,msg)
            END IF
         END DO
      END DO
!     Check for levels with a missing XREF=
      IF(XREfs.NE.' ') THEN
         didit2 = .FALSE.
         DO i = 1, NFLvl
            IF(FXRef(i).EQ.' ') THEN
               IF(.NOT.didit2) THEN
                  CALL CHKBRK(didit,'<W> Missing XREF= for level(s):')
                  didit2 = .TRUE.
               ELSE
                  TOTwar = TOTwar + 1
               END IF
               IF(.NOT.NOWarn) THEN
                  CALL WRTCHK(1)
                  WRITE(out,'(T92,A)') FLVl(i)
               END IF
            END IF
         END DO
      END IF
!
      RETURN
      END SUBROUTINE CHKEND
!
!***********************************************************************
!
      SUBROUTINE CHKG(Card)
!
!     CHECK GAMMA CARD.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=80) :: Card
!
!     Functions used
!
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
      INTEGER(KIND=4), INTRINSIC :: INDEX, LEN_TRIM
      INTEGER(KIND=4), EXTERNAL :: SPAN, TYPSTR
!
!     Local variables
!
      CHARACTER(LEN=10) :: tmpstr
      INTEGER(KIND=4) :: ends, i, perpos
      REAL(KIND=4) :: deg, eg
!
!     Reset logical for FL seen
      flseen=.FALSE.
!
!
!     CHECK CARD SEQUENCE.
!
      IF(.NOT.ISEen) CALL WRTMSG('<F> DSID CARD NOT YET SEEN',0,0)
!     Should only worry about gammas with intensities (TWB. 930309)
      GABeld = .TRUE.
!
!     VALIDATE NUCID.
!
      CALL CKNCID(Card)
!     Column 9 may have a symbol associated with it
      IF(SSYmbs.EQ.' ') THEN
         CALL CKBLNK(Card(9:9),9,9)
      ELSE IF(Card(9:9).NE.' ') THEN
         IF(.NOT.HISpin)CALL WRTMSG('<F> CARD ILLEGAL FOR THIS DATASET',&
     &                              8,9)
         IF(INDEX(SSYmbs,Card(9:9)).EQ.0)                               &
     &      CALL WRTMSG('<E> UNDEFINED SYM CHAR.',9,9)
      END IF
!     Save symbol for continuation and comment checks
      GCOl9 = Card(9:9)
!     VALIDATE DATA FIELDS.
!
      IF(GLAbon(1)) THEN
         CALL CKE(Card(10:19),10,19,'<W>')
      ELSE
         CALL CKE(Card(10:19),10,19,'<E>')
      END IF
      CALL CKU(Card(20:21),20)
      IF(Card(22:29).EQ.' ') THEN
      ELSE IF(Card(SPAN(Card,22,' '):29).EQ.'WEAK') THEN
         IF(Card(77:77).NE.'%') RIFnd = .TRUE.
      ELSE
!        Don't count RI if "%" in column 77
         IF(Card(77:77).NE.'%') THEN
            RISeen = .TRUE.
            IF(CHGbr) THEN
               RIFnd = .TRUE.
               NOBr100 = NOBr100.AND.CGRI100(Card(22:29))
            END IF
         END IF
         IF(GLAbon(3)) THEN
            CALL CKVPRN(Card(22:29),22,.TRUE.,'<W>')
         ELSE
            CALL CKVPRN(Card(22:29),22,.TRUE.,'<E>')
         END IF
      END IF
      CALL CKU(Card(30:31),30)
      IF(GLAbon(5)) THEN
         CALL CKM(Card(32:41),32,41,'<W>')
      ELSE
         CALL CKM(Card(32:41),32,41,'<E>')
      END IF
      tmpstr = Card(42:49)
      CALL LBSUP(tmpstr)
      IF(tmpstr.NE.' '.AND.tmpstr.NE.'INFNT') THEN
         IF(GLAbon(6)) THEN
            CALL CKVPS(Card(42:49),42,.TRUE.,'<W>')
         ELSE
            CALL CKVPS(Card(42:49),42,.TRUE.,'<E>')
         END IF
      END IF
      CALL CKU(Card(50:55),50)
      IF(GLAbon(8)) THEN
         CALL CKV(Card(56:62),56,.FALSE.,'<W>')
      ELSE
         CALL CKV(Card(56:62),56,.FALSE.,'<E>')
      END IF
      CALL CKU(Card(63:64),63)
      IF(TOTal.AND.DECtyp.NE.4)                                         &
     &   CALL WRTMSG('<E> No CC for total ionization',56,64)
      IF(Card(65:74).EQ.' ') THEN
      ELSE IF(Card(SPAN(Card,65,' '):74).EQ.'WEAK') THEN
         RIFnd = .TRUE.
      ELSE
!        Don't count TI if "%" in column 77
         IF(Card(77:77).NE.'%') THEN
            TISeen = .TRUE.
            IF(CHGbr) THEN
               RIFnd = .TRUE.
               NOBr100 = NOBr100.AND.CGRI100(Card(65:74))
            END IF
         END IF
         IF(GLAbon(10)) THEN
            CALL CKVPRN(Card(65:74),65,.TRUE.,'<W>')
         ELSE
            CALL CKVPRN(Card(65:74),65,.TRUE.,'<E>')
         END IF
      END IF
      CALL CKU(Card(75:76),75)
      CALL CKC(Card(77:77),77,77)
!     If "%" on GAMMA, parent level should be band member
      IF(Card(77:77).EQ.'%'.AND..NOT.INBand)  THEN
         CALL WRTMSG('<E> PARENT LEVEL NOT BAND MEMBER',77,77)
      END IF
      CALL CKCOIN(Card(78:78),78,78)
!     Manual states column 79 should be blank (TWB. 930309)
      IF(Card(78:79).EQ.' C'.OR.Card(78:79).EQ.' ?') THEN
         CALL WRTMSG('<E> COINCIDENCE IN WRONG FIELD',78,79)
      ELSE
         CALL CKBLNK(Card(79:79),79,79)
      END IF
      CALL CKQ(Card(80:80),80,80)
!
!     CONSISTENCY BETWEEN UNC FIELDS AND THE MAIN VALUE FIELDS
!
      CALL CKVU(Card(10:19),Card(20:21),10,21)
      CALL CKVU(Card(22:29),Card(30:31),22,31)
      CALL CKVU(Card(42:49),Card(50:55),42,55)
      CALL CKVU(Card(56:62),Card(63:64),56,64)
      CALL CKVU(Card(65:74),Card(75:76),65,76)
!
!     Check for missing RI
      IF(PNAbs) THEN
         CALL CNVS2U(Card(10:19),Card(20:21),eg,deg)
         IF(eg.LE.400) THEN
            IF(Card(22:29).EQ.' '.AND.Card(56:62).EQ.' '.AND.Card(65:74)&
     &         .NE.' ') THEN
               CALL WRTMSG('<W> Check missing RI',22,29)
            END IF
         END IF
      END IF
!
!     Consistency checks between MULT and MR
!
      IF(Card(32:41).EQ.' '.AND.Card(42:49).NE.' ') THEN
         IF(GLAbon(5).OR.GLAbon(6)) THEN
            CALL WRTMSG('<W> MISSING MULT',32,41)
         ELSE
            CALL WRTMSG('<E> MISSING MULT',32,41)
         END IF
      END IF
*JKT*Comment out  warning re missing MR when + in Mult
C      IF(INDEX(Card(32:41),'+').GT.0.AND.INDEX(Card(32:41),',')         &
C     &   .EQ.0.AND.Card(42:49).EQ.' ')                                  &
C     &   CALL WRTMSG('<W> MISSING MR',42,49)
*JKT
      IF(INDEX(Card(32:41),'+').EQ.0.AND.Card(42:49).NE.' ') THEN
         IF(INDEX(Card(32:41),',').EQ.0.AND.Card(32:41).NE.' ') THEN
            IF(GLAbon(5).OR.GLAbon(6)) THEN
               CALL WRTMSG('<W> MISSING MULT ADMIXTURE',32,41)
            ELSE
               CALL WRTMSG('<E> MISSING MULT ADMIXTURE',32,41)
            END IF
         ELSE IF(Card(32:41).NE.' ') THEN
            IF(GLAbon(5).OR.GLAbon(6)) THEN
               CALL WRTMSG('<W> "," FOUND WHERE "+" REQUIRED',32,41)
            ELSE
               CALL WRTMSG('<E> "," FOUND WHERE "+" REQUIRED',32,41)
            END IF
         END IF
      END IF
!
      IF(Card(77:77).EQ.'*'.OR.Card(77:77).EQ.'@'.OR.Card(77:77).EQ.'&')&
     &   THEN
!        Consistency check between column 77 and intensities
         IF(Card(77:77).EQ.'*') THEN
            IF(Card(22:29).NE.' '.OR.Card(65:74).NE.' ')                &
     &         CALL WRTMSG('<W> CHECK USE OF "*"',77,77)
         ELSE
            IF(Card(22:29).EQ.' '.AND.Card(65:74).EQ.' ')               &
     &         CALL WRTMSG('<W> CHECK USE OF "@" OR "&"',77,77)
         END IF
!        Save data for later comparison
         NG = NG + 1
         IF(NG.LE.ngmax) THEN
            EGStr(NG) = Card(10:19)
            CALL LBSUP(EGStr(NG))
            DEGstr(NG) = Card(20:21)
            GSYmb(NG) = Card(77:77)
            GSEq(NG) = NSEq
         ELSE
            NG = ngmax
         END IF
!        Check to see if a preceding gamma of same energy is multiply
!        placed
      ELSE IF(NG.GT.0) THEN
         DO i = 1, NG - 1
            IF(INDEX(Card(10:19),TRIM(EGStr(i))).GT.0) THEN
               CALL WRTMSG('<W> MISSING MULTIPLE-PLACEMENT SYMBOL',77,  &
     &                     77)
               EXIT
            END IF
         END DO
      END IF
      If(nflvl .EQ. 0)Then
         If(ndxg .LT. maxdxg)Then
	    ndxg=ndxg+1
            RECdxg(NDXg) = NSEq
            RECnum(NDXg) = NDXg
            WRITE(UNIT=tmp,FMT='(A)',REC=NDXg) Card
	 Else
            CALL WRTMSG('<I> MAX GAMMA FOR CHECKING EXCEEDED',0,0)
	 EndIf
      Else
         IF(NDXg.LT.maxdxg) THEN
            NDXg = NDXg + 1
            RECdxg(NDXg) = NSEq
            RECnum(NDXg) = NDXg
            WRITE(UNIT=tmp,FMT='(A)',REC=NDXg) Card
            DXGcol9(NDXg) = Card(9:9)
            DXGe(NDXg) = Card(10:19)
            DXGde(NDXg) = Card(20:21)
            IF(DXGde(NDXg).EQ.' '.AND.TYPSTR(DXGe(NDXg)).NE.0.AND.      &
     &         TYPSTR(DXGe(NDXg)).NE.2) THEN
               tmpstr = DXGe(NDXg)
               CALL LBSUP(tmpstr)
               perpos = INDEX(tmpstr,'.')
               ends = INDEX(tmpstr,'E') - 1
               IF(ends.LT.0) THEN
                  IF(TYPSTR(tmpstr).NE.-1) THEN
                     ends = LEN_TRIM(tmpstr)
                  ELSE IF(INDEX(tmpstr,'+').GT.perpos) THEN
                     ends = INDEX(tmpstr,'+') - 1
                  ELSE
                     ends = LEN_TRIM(tmpstr)
                  END IF
               END IF
               IF(perpos.EQ.0.OR.(ends-perpos).EQ.0) THEN
                  DXGde(NDXg) = '1'
               ELSE IF((ends-perpos).EQ.1) THEN
                  DXGde(NDXg) = '5'
               ELSE
                  DXGde(NDXg) = '15'
               END IF
            END IF
            DXFlv(NDXg) = ' '
            IF(Card(77:77).EQ.'*'.OR.Card(77:77).EQ.'@'.OR.Card(77:77)  &
     &         .EQ.'&') THEN
               DXMul(NDXg) = Card(77:77)
            ELSE
               DXMul(NDXg) = ' '
            END IF
         ELSE
            CALL WRTMSG('<I> MAX GAMMA FOR CHECKING EXCEEDED',0,0)
         END IF
      END IF
!
      RETURN
      END SUBROUTINE CHKG
!
!***********************************************************************
!
      SUBROUTINE CHKI(Card)
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=80) :: Card
!
!     Functions used
!
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
      INTEGER(KIND=4), INTRINSIC :: INDEX, MAX0, LEN_TRIM
      INTEGER(KIND=4), EXTERNAL :: SPAN, TYPSTR
!
!     Local variables
!
      CHARACTER(LEN=30) :: colstr
      CHARACTER(LEN=203) :: dsid
      CHARACTER(LEN=26) :: test
      CHARACTER(LEN=39) :: tmpds
      CHARACTER(LEN=10) :: work
      INTEGER(KIND=4) :: cklen, colpos, i, j, k, r
!
!     VARIABLE INITIALIZATIONS.
!
      ISEen = .TRUE.
      NSEen = .FALSE.
      NRSeen = .FALSE.
      NTSeen = .FALSE.
      BRSeen = .FALSE.
      NBSeen = .FALSE.
      NPSeen = .FALSE.
      PSEen = .FALSE.
      QSEen = .FALSE.
      RISeen = .FALSE.
      TISeen = .FALSE.
      GABeld = .FALSE.
      PNSeen = .FALSE.
      PNAbs = .FALSE.
      IBSeen = .FALSE.
      IPSeen = .FALSE.
      NOBrpct = .FALSE.
      EXDscn = .FALSE.
      HISpin = .FALSE.
      ISY2k = .FALSE.
      BADord = .FALSE.
      EXP2pn = .FALSE.
      CHGbr = .FALSE.
      TOTcon = 0
      FN = 0.0
      DFN = 0.0
      TOTin = 0.0
      DTOtin = 0.0
      GCOunt = 0
      DO i = 1, 6
         NFLag(i) = 0
      END DO
      NFLvl = 0
      NOFf = 0
      LVLexc = .FALSE.
      DSIdsv = ' '
      XREfs = ' '
      SSYmbs = ' '
      GCOl9 = ' '
      BSYmbs = ' '
      SEQSymbs = ' '
      INBand = .FALSE.
      INSEQ = .FALSE.
      NCOl9 = ' '
      PCOl9 = ' '
      LEVstr = ' '
      CALL INITH
      CALL INITLAB
      JNO = 0
      nms=0
!
!     If not the first card, previous card has to be end card (99)
!
      IF(PRVtyp.NE.88) THEN
         IF(PRVtyp.NE.99) CALL WRTMSG('<F> NEED BLANK CARD',0,0)
      END IF
!     Attempt to establish version of ENSDF file
      IF(Card(75:80).NE.' '.AND.TYPSTR(Card(75:80)).NE.1) THEN
         CALL WRTMSG('<E> DATE FIELD MUST BE BLANK OR NUMERIC',75,80)
      ELSE IF(Card(75:80).NE.' ') THEN
         IF(Card(75:76).GE.'19'.AND.Card(75:76).LE.'30') THEN
            ISY2k = .TRUE.
            ISY2k = ISY2k.AND.                                          &
     &              (Card(77:78).GE.'00'.AND.Card(77:78).LE.'99')
            ISY2k = ISY2k.AND.                                          &
     &              (Card(79:80).GE.'01'.AND.Card(79:80).LE.'12')
         END IF
         IF(.NOT.ISY2k) THEN
            CALL WRTMSG('<W> Obsol. form. - Use YYYYMM',75,80)
            IF(Card(75:76).LT.'01'.OR.Card(75:76).GT.'99')              &
     &         CALL WRTMSG('<E> INVALID YEAR',75,76)
            IF(Card(77:78).LT.'01'.OR.Card(77:78).GT.'12')              &
     &         CALL WRTMSG('<E> INVALID MONTH',77,78)
            IF(Card(79:80).LT.'01'.OR.Card(79:80).GT.'31')              &
     &         CALL WRTMSG('<E> INVALID DAY',79,80)
         END IF
      END IF
      IF(.NOT.ISY2k) THEN
         test = Card(40:65)
         i = INDEX(test,',')
         IF(i.GT.0) THEN
            i = i - 1
         ELSE
            i = LEN_TRIM(test)
         END IF
         IF(i.EQ.8) ISY2k = .TRUE.
      END IF
**JKT*Comment out Y2K comment
C      IF(.NOT.ISY2k) CALL WRTMSG('<W> Check dataset for Y2K compl.',0,0)
**JKT*
!     ESTABLISH DSTYPE.
!
!     Save NUCID A and Z and if Isobaric data
      Call Nucid(card(1:5),resa,resz)
      If(INDEX(card(10:39),'IAR').GT.0                                  &
     &  .OR. INDEX(card(10:39),'IAS').GT.0)Then
         isiar=.TRUE.
      Else
         isiar=.FALSE.
      EndIf
!
!     LOOK AT DSID.
      dsid = Card(10:39)
      r = LEN_TRIM(dsid)
      IF(r.LE.0) THEN
         CALL WRTMSG('<F> BLANK DSID',10,39)
         GO TO 20
      END IF
!     REMOVE EXTRANEOUS CHARACTERS.
!     IGNORE COLON (:) AND FOLLOWING TEXT.
      colpos = 0
      i = INDEX(dsid(1:r),':')
      IF(i.GT.0) THEN
         r = LEN_TRIM(dsid(1:i-1))
         colpos = i
         colstr = dsid(i+1:)
      END IF
!
!     NOW, DETERMINE DSTYPE.
      IF(INDEX(dsid(1:r),'COMMENTS').GT.0) THEN
         DSType = 0
      ELSE IF(INDEX(dsid(1:r),'ADOPTED').GT.0) THEN
         DSType = 1
         CHGbr = .TRUE.
!        Save the NUCID for latter XREF vs DSID checks
         IF(LEN_TRIM(ADPtnuc).EQ.0) THEN
            ADPtnuc = Card(1:5)
         ELSE
            k = LEN_TRIM(ADPtnuc)
            DO WHILE (.TRUE.)
               IF(5*(k/5).NE.k) THEN
                  k = k + 1
                  CYCLE
               END IF
               k = k + 1
               CALL ADDSTR(ADPtnuc,k,Card(1:5))
               EXIT
            END DO
         END IF
      ELSE IF(INDEX(dsid(1:r),'DECAY').GT.0) THEN
         DSType = 2
      ELSE IF(INDEX(dsid(1:r),'MUONIC ATOM').GT.0) THEN
         DSType = 2
      ELSE IF(INDEX(dsid(1:r),'REFERENCES').GT.0) THEN
         DSType = 4
      ELSE IF(INDEX(dsid(1:r),'HALF-LIFE').GT.0) THEN
         DSType = 5
      ELSE
         resnuc=card(1:5)
         DSType = 3
!        IF ALL ELSE FAILS, ASSUME REACTION TYPE.
!        New Reaction dataset type (950223)
         IF(INDEX(dsid(1:r),'HIGH-SPIN LEVELS, GAMMAS').GT.0)           &
     &      HISpin = .TRUE.
         IF(colpos.GT.0) CALL CKCOL(colpos,colstr)
      END IF
!     Save ID record for latter global check if not "source data set"
!     WRITE(*,*) 'Data set type is', DSType 
      IF(DSType.GE.2.AND.DSType.LE.3) THEN
         IF(NDS.GE.maxds) THEN
            IF(SKIpds.EQ.0)CALL WRTMSG('<I> MAX COUNT FOR DSID EXCEEDED'&
     &                                 ,0,0)
            SKIpds = SKIpds + 1
!           Check to see if DSID already exists
         ELSE IF(NDS.GT.0) THEN
            tmpds = Card
            DO i = NDS, 1, -1
               cklen = MAX0(LEN_TRIM(tmpds),LEN_TRIM(DSSave(i)))
               IF(tmpds(1:cklen).EQ.DSSave(i)(1:cklen)) THEN
                  CALL WRTMSG('<E> DSID already encountered',1,39)
                  GO TO 10
               END IF
            END DO
            NDS = NDS + 1
            DSSave(NDS) = Card
         ELSE
            NDS = NDS + 1
            DSSave(NDS) = Card
         END IF
      END IF
!
!     VALIDATE NUCID.
!
   10 CALL NUCID(Card(1:5),DSA,DSZ)
      IF(DSA.EQ.0.OR.DSZ.EQ.-1) THEN
         CALL WRTMSG('<F> INVALID NUCID',1,5)
      ELSE IF(IZLmsg.NE.' ') THEN
         CALL REPSTR(IZLmsg,'Obsolete formalism','Obsol. form')
         CALL ADDSTR(IZLmsg,1,'<W> ')
         CALL WRTMSG(TRIM(IZLmsg),4,5)
      ELSE IF(DSA.LE.7.AND.DSZ.EQ.7) THEN
         CALL WRTMSG('<W> Obsol. form - Use "NN"',4,5)
      END IF
      IF(DSType.EQ.4) THEN
         IF(DSZ.NE.-2) THEN
            CALL WRTMSG('<E> REFERENCES MUST HAVE BLANK Z',4,5)
         END IF
      ELSE IF(DSType.NE.0) THEN
         IF(DSZ.EQ.-2) THEN
            CALL WRTMSG('<F> DATA SET MUST HAVE Z',4,5)
         END IF
      END IF
      CALL CKBLNK(Card(9:9),9,9)
!
      IF(dsid(r:r).NE.',') THEN
!
!        not expecting cont card
!
         dsid(r+1:) = ' '
         CALL CKDSID(dsid)
         IF(DSType.EQ.2.AND.DECtyp.NE.0.AND.DECtyp.NE.7)                &
     &      CALL NUC2DEC(Card(1:5),dsid)
!        Must still save record in case "," omitted (TWB. 930511)
         DSIdsv = dsid
      ELSE
!
!        Expecting cont card to follow
!
         DSIdsv = dsid
         EXDscn = .TRUE.
      END IF
!
!     VALIDATE REFERENCE FIELD.
!
   20 IF(ISY2k) THEN
         CALL CKREF(Card(40:65),40)
      ELSE
         IF(Card(60:64).NE.' ') CALL WRTMSG('<E> JUNK AT END OF FIELD', &
     &      60,64)
         CALL CKREF(Card(40:59),40)
      END IF
!
!     Attempt to validate PUB field (TWB. 930315)
      IF(ISY2k) THEN
         test = Card(66:74)
      ELSE
         test = Card(66:74)
      END IF
      IF(test.NE.' ') THEN
         i = 1
         work = test
         r = SPAN(work,1,' ')
         IF(r.GT.1) THEN
            CALL WRTMSG('<E> LEADING BLANKS INVALID',66,65+r)
            i = r
         END IF
         r = LEN_TRIM(work)
         IF(i+1.GT.r) THEN
            CALL WRTMSG('<E> INVALID PUB FIELD',64+i,74)
            GO TO 30
         END IF
!        Check first five columns - should be of the form YYNDS
         IF(TYPSTR(work(i:i+1)).EQ.1) THEN
            IF(i+3.GT.r) THEN
               CALL WRTMSG('<E> INVALID PUB FIELD',68+i,74)
               GO TO 30
            END IF
            IF(work(i+2:i+4).NE.'NDS') THEN
               IF(work(i+2:i+4).NE.'NP '.AND.work(i+2:i+4).NE.'TOI')    &
     &            CALL WRTMSG('<E> INVALID NDS PUBLICATION',65+i,69+i)
            ELSE
               i = i + 5
               IF(i.LE.r) THEN
                  IF(work(i:i).EQ.'+') THEN
                     i = i + 1
                     IF(i+1.GT.r) THEN
                        CALL WRTMSG('<E> INVALID PUB FIELD',64+i,74)
                        GO TO 30
                     END IF
                     IF(TYPSTR(work(i:i+1)).NE.1)                       &
     &                  CALL WRTMSG('<E> INVALID YEAR',64+i,65+i)
                  ELSE IF(work(i:i).EQ.',') THEN
                     i = i + 1
                     IF(i+1.GT.r) THEN
                        CALL WRTMSG('<E> INVALID PUB FIELD',64+i,74)
                        GO TO 30
                     END IF
                     IF(TYPSTR(work(i:r)).NE.2)                         &
     &                  CALL WRTMSG('<E> INVALID INITIALS',64+i,64+r)
                  ELSE
                     CALL WRTMSG('<E> INVALID CHARACTER',64+i,64+i)
                  END IF
               END IF
            END IF
!           Allow for 'PREAAA' in PUB field (TWB. 940819)
         ELSE IF(work(i:i+2).EQ.'PRE') THEN
            IF(LEN_TRIM(work(i:)).NE.6) THEN
               CALL WRTMSG('<E> INVALID NDS PUBLICATION',65+i,70+i)
            ELSE
               j = i + 3
               IF((work(j:j).LT.'0'.OR.work(j:j).GT.'2').OR.            &
     &            (work(j+1:j+1).LT.'0'.OR.work(j+1:j+1).GT.'9').OR.    &
     &            (work(j+2:j+2).LT.'0'.OR.work(j+2:j+2).GT.'9'))       &
     &            CALL WRTMSG('<E> INVALID NDS PUBLICATION',65+i,70+i)
            END IF
         ELSE IF(work(i:i+4).EQ.'ENSDF') THEN
            IF(work(i+5:).NE.' ') CALL WRTMSG('<E> INVALID PUB FIELD',  &
     &         65+i,74)
         ELSE
            CALL WRTMSG('<E> INVALID NDS PUBLICATION',65+i,70+i)
         END IF
      END IF
!
   30 RETURN
      END SUBROUTINE CHKI
!
!***********************************************************************
!
      SUBROUTINE CHKL(Card)
!
!     CHECK LEVEL CARD.
!
      IMPLICIT NONE
!
!     PARAMETER definitions
!
      REAL(KIND=4), PARAMETER :: incrmnt = 0.00001
!
!     Dummy arguments
!
      CHARACTER(LEN=80) :: Card
!
!     Functions used
!
      CHARACTER(LEN=1), INTRINSIC :: CHAR
      INTEGER(KIND=4), INTRINSIC :: INDEX, LEN_TRIM
      INTEGER(KIND=4), EXTERNAL :: INDEXF, TYPSTR
      REAL(KIND=4), INTRINSIC :: FLOAT
      REAL(KIND=4), EXTERNAL :: VALSTR
!
!     Local variables
!
      CHARACTER(LEN=18) :: curjpi, prejpi
      CHARACTER(LEN=10) :: curt12, levene, pret12, tmpstr, test
      CHARACTER(LEN=40) :: messag
      CHARACTER(LEN=2) :: oldshe
      CHARACTER(LEN=1) :: thechr
      LOGICAL(KIND=4) :: chkord, newchr, odd
      LOGICAL(KIND=4) :: nounc
      LOGICAL(KIND=4) :: bnrflag
      INTEGER(KIND=4) :: add, ends, i, jplus, perpos, typlev
      REAL(KIND=4),save :: curlev, prelev
!
      CHARACTER(LEN=maxoff),                                            &
     &          PARAMETER :: chkchr = 'XYZUVWSTABCDEFGHIJKLMNOPQR'
!
      INBand = .FALSE.
      NOBrpct = .FALSE.
      INSEQ = .FALSE.
      bnrflag = .FALSE.
      messag = ' '
      LEVseq = NSEq
      LEVstr = Card
      oldshe = ' '
      IF(2*(DSA/2).NE.DSA) THEN
         odd = .TRUE.
      ELSE
         odd = .FALSE.
      END IF
!
      RIFnd = .FALSE.
      NOBr100 = .TRUE.
      nounc=.FALSE.
!     CHECK CARD SEQUENCE.
!
      IF(.NOT.ISEen) CALL WRTMSG('<F> DSID CARD NOT YET SEEN',0,0)
      GABeld = .TRUE.
!
!     VALIDATE NUCID.
!
      CALL CKNCID(Card)
      CALL CKBLNK(Card(9:9),9,9)
!
!     VALIDATE DATA FIELDS.
!
      IF(LLAbon(1)) THEN
         CALL CKE(Card(10:19),10,19,'<W>')
      ELSE
         CALL CKE(Card(10:19),10,19,'<E>')
      END IF
      i=Indexf(Card,10,'SN+')
      If(i .EQ. 0)i=Indexf(Card,10,'SP+')
      If(i .GT. 0)Then
         i=i+3
	 If(INDEX(chkchr,card(i:i)) .GT. 0)nounc=.TRUE.
      EndIf
      IF(NFLvl.GT.0) THEN
         chkord = .TRUE.
      ELSE
         chkord = .FALSE.
         prelev = 0.0
         prejpi = Card(22:39)
         CALL LBSUP(prejpi)
         pret12 = Card(40:49)
         CALL LBSUP(pret12)
      END IF
      NFLvl = NFLvl + 1
      If(nflvl .EQ. 1)Then
         Call CHKUNP
         ndxg=0
      EndIf 
      IF(NFLvl.GT.maxlvl) THEN
         IF(LVLexc) THEN
            NFLvl = maxlvl
         ELSE
            LVLexc = .TRUE.
            CALL WRTMSG('<I> MAX LEVEL FOR FL= CHECKING EXCEEDED',0,0)
            NFLvl = maxlvl
         END IF
      END IF
      levene = Card(10:19)
      i=LEN_TRIM(levene)
      CALL LBSUP(levene)
      add=i-LEN_TRIM(levene)
      IF(.NOT.LVLexc) THEN
         FLVl(NFLvl) = levene
         CALL LBSUP(FLVl(NFLvl))
         DFLvl(NFLvl) = Card(20:21)
         IF(DFLvl(NFLvl).EQ.' '.AND.TYPSTR(levene).NE.0.AND.            &
     &      TYPSTR(levene).NE.2) THEN
            perpos = INDEX(levene,'.')
            ends = INDEX(levene,'E') - 1
            IF(ends.LT.0) THEN
               IF(TYPSTR(levene).NE.-1) THEN
                  ends = LEN_TRIM(levene)
               ELSE IF(INDEX(levene,'+').GT.perpos) THEN
                  ends = INDEX(levene,'+') - 1
                  IF((TYPSTR(levene(1:ends)).EQ.1.OR.TYPSTR(levene(1:   &
     &               ends)).EQ.-2).AND.                                 &
     &               (TYPSTR(levene(ends+2:)).EQ.1.OR.TYPSTR            &
     &               (levene(ends+2:)).EQ.-2)) THEN
                     CALL WRTMSG('<F> Two level energies',10,19)
                  END IF
               ELSE
                  ends = LEN_TRIM(levene)
               END IF
            END IF
            IF(perpos.EQ.0.OR.(ends-perpos).EQ.0) THEN
               DFLvl(NFLvl) = '1'
            ELSE IF((ends-perpos).EQ.1) THEN
               DFLvl(NFLvl) = '2'
            ELSE
               DFLvl(NFLvl) = '15'
            END IF
         END IF
         FJPi(NFLvl) = Card(22:39)
         CALL SQZSTR(FJPi(NFLvl),' ')
         FXRef(NFLvl) = ' '
         FBAnd(NFLvl) = ' '
         FSEq(NFLvl) = ' '
      END IF
      If(nounc .AND. Card(20:21).NE.' ')Then
         Call Wrtmsg('<E> Unexpected uncertainty found',20,21)
      EndIf
      CALL CKU(Card(20:21),20)
      IF(LLAbon(3)) THEN
         CALL CKJ(Card(22:39),22,39,odd,'<W>')
      ELSE
         CALL CKJ(Card(22:39),22,39,odd,'<E>')
      END IF
      IF(LLAbon(4)) THEN
**JKT**if field relabelled, then no check
          continue
**JKT
c         CALL CKT(Card(40:49),40,49,'<W>')
      ELSE
         CALL CKT(Card(40:49),40,49,'<E>')
      END IF
      CALL CKU(Card(50:55),50)
      IF(LLAbon(6)) THEN
**JKT**if field relabelled, then no check
        continue
**JKT
c         CALL CKL(Card(56:64),56,64,'<W>')
      ELSE
         CALL CKL(Card(56:64),56,64,'<E>')
      END IF
      If(LLAbon(7))Then
**JKT**if field relabelled, then no check
        continue
**JKT
C         CALL CKS(Card(65:74),65,74,'<W>')
      Else
         CALL CKS(Card(65:74),65,74,'<E>')
      EndIf
      CALL CKU(Card(75:76),75)
      CALL CKC(Card(77:77),77,77)
!     Is it a band member?
      IF(INDEX(BSYmbs,Card(77:77)).GT.0.AND.Card(77:77).NE.' ') THEN
         INBand = .TRUE.
         FBAnd(NFLvl) = Card(77:77)
      END IF
!     Is it a sequence member?      
      IF(INDEX(SEQSYmbs,Card(77:77)).GT.0.AND.Card(77:77).NE.' ') THEN
         INSEQ = .TRUE.
         FSEq(NFLvl) = Card(77:77)
      END IF
      IF(ISIon.AND.DECtyp.EQ.4) THEN
         oldshe = SUBshe
         SUBshe = Card(78:79)
         IF(SUBshe.LT.SUBshe)CALL WRTMSG('<F> Shell out of order',79,80)
         IF(SUBshe(1:1).EQ.'K') THEN
            IF(SUBshe(2:2).NE.' ')                                      &
     &         CALL WRTMSG('<E> Incorrect electron shell',79,80)
         ELSE IF(SUBshe(1:1).EQ.'L') THEN
            IF(SUBshe(2:2).LT.'1'.OR.SUBshe(2:2).GT.'3')                &
     &         CALL WRTMSG('<E> Incorrect electron shell',79,80)
         ELSE IF(SUBshe(1:1).EQ.'M') THEN
            IF(SUBshe(2:2).LT.'1'.OR.SUBshe(2:2).GT.'5')                &
     &         CALL WRTMSG('<E> Incorrect electron shell',79,80)
         ELSE IF(SUBshe(1:1).EQ.'N') THEN
            IF(SUBshe(2:2).LT.'1'.OR.SUBshe(2:2).GT.'7')                &
     &         CALL WRTMSG('<E> Incorrect electron shell',79,80)
         ELSE IF(SUBshe(1:1).EQ.'O') THEN
            IF(SUBshe(2:2).LT.'1'.OR.SUBshe(2:2).GT.'5')                &
     &         CALL WRTMSG('<E> Incorrect electron shell',79,80)
         ELSE IF(SUBshe(1:1).EQ.'P') THEN
            IF(SUBshe(2:2).LT.'1'.OR.SUBshe(2:2).GT.'3')                &
     &         CALL WRTMSG('<E> Incorrect electron shell',79,80)
         ELSE
            CALL WRTMSG('<E> Incorrect electron shell',79,80)
         END IF
      ELSE
         if (DSType.EQ.2) then
            bnrflag = Card(78:79).EQ.'R'
         end if
         if (bnrflag.NEQV..TRUE.) then
            CALL CKMS(Card(78:79),78,79)
         end if
      END IF
      CALL CKQ(Card(80:80),80,80)
!
!     CONSISTENCY BETWEEN UNC FIELDS AND THE MAIN VALUE FIELDS
!
      CALL CKVU(Card(10:19),Card(20:21),10,21)
      CALL CKVU(Card(40:49),Card(50:55),40,55)
      CALL CKVU(Card(65:74),Card(75:76),65,76)
!
      i=LEN_TRIM(levene)
      CALL REPSTR(levene,'(',CHAR(0))
      add=add+i-LEN_TRIM(levene)
      CALL REPSTR(levene,')',CHAR(0))
      typlev = TYPSTR(levene)
!
!     Check to see if this is a non-stable g.s. or a metastable state
      IF(DSType.EQ.1) THEN
         NOBrpct = Card(78:79).NE.' '
         IF(INDEX(Card(40:49),'STABLE').EQ.0) THEN
            NOBrpct = NOBrpct.OR.                                       &
     &                ((typlev.EQ.1.OR.typlev.EQ.-2).AND.VALSTR(levene) &
     &                .EQ.0.0)
            IF(.NOT.NOBrpct.AND.Card(40:49).NE.' ')NOBrpct = NOBrpct.OR.&
     &         ISMET(Card(40:49))
            If(ISMET(Card(40:49)) .AND. Card(78:79).EQ.' ')Then         &
               If(nflvl .GT. 1)Call                                     &
     &           Wrtmsg('<W> Missing MS. T1/2>=1 ms',78,79)
            EndIf
         END IF
      ELSE IF (DSType.EQ.2) THEN
! PUT IN CHECK FOR R here for beta delayed where possible bn can occur
! TIM
           bnrflag = Card(78:79).EQ.'R'
      END IF
      IF(.NOT.NOBrpct) LEVstr = ' '
!
!     Check on level order and storage of offset for "+X"...
      IF(typlev.EQ.0) THEN
         CALL WRTMSG('<E> ENERGY FIELD CAN NOT BE BLANK',10,19)
         RETURN
      END IF
      IF(typlev.EQ.1.OR.typlev.EQ.-2) THEN
         curlev = VALSTR(levene)
!        if (isenernum) then
!           If ( (dstype.eq.2) .and. (curlev.gt.avail_energy) ) Then
!              CALL WRTMSG('<E> E > Q + Parent decay level',10,19)
!           End if
!        end if
         curjpi = Card(22:39)
         CALL LBSUP(curjpi)
         curt12 = Card(40:49)
         CALL LBSUP(curt12)
!	write(*,*) '..tst:',curlev,prelev,NFlvl
         IF(chkord.AND.SUBshe.LE.oldshe) THEN
            IF(curlev.LT.prelev) THEN
               CALL WRTMSG('<F> E < PRECEDING LEVEL ENERGY',10,19)
               BADord = .TRUE.
            END IF
            IF(curlev.EQ.prelev.AND.curjpi.EQ.prejpi.AND.               &
     &         curt12.EQ.pret12)                                        &
     &         CALL WRTMSG('<W> E = PRECEDING LEVEL ENERGY',10,19)
         END IF
         prelev = curlev
         prejpi = Card(22:39)
         CALL LBSUP(prejpi)
         pret12 = Card(40:49)
         CALL LBSUP(pret12)
         RETURN
      END IF
      IF(typlev.EQ.2) THEN
         IF(LEN_TRIM(levene).EQ.1) THEN
            IF(NOFf.GT.0) THEN
               DO i = 1, NOFf
                  IF(levene(1:1).EQ.OFFchr(i)) THEN
                     CALL WRTMSG('<E> CHARACTER ALREADY ASSIGNED',10,19)
                     RETURN
                  END IF
               END DO
            END IF
            IF(NOFf.LT.maxoff) THEN
               NOFf = NOFf + 1
               OFFchr(NOFf) = levene(1:1)
               IF(INDEX(chkchr,OFFchr(NOFf)).EQ.0) THEN
                  CALL WRTMSG('<W> UNDEFINED CHARACTER ASSIGNED',10,19)
               ELSE IF(DSType.EQ.1) THEN
                  IF(OFFchr(NOFf).NE.chkchr(NOFf:NOFf))                 &
     &               CALL WRTMSG('<W> CHARACTER ASSIGNED OUT OF ORDER', &
     &               10,19)
               ELSE IF(NOFf.GT.1) THEN
                  IF(OFFchr(NOFf-1).NE.chkchr(NOFf-1:NOFf-1))           &
     &               CALL WRTMSG('<W> CHARACTER ASSIGNED OUT OF ORDER', &
     &               10,19)
               END IF
               OFFamt(NOFf) = prelev + incrmnt*FLOAT(NOFf)
               WRITE(messag,'(2A,F13.5)') OFFchr(NOFf),'=',OFFamt(NOFf)
               CALL SQZSTR(messag,' ')
               CALL ADDSTR(messag,1,'<I> Assuming ')
            ELSE
               CALL WRTMSG('<E> TOO MANY CHARACTERS ASSIGNED',10,19)
            END IF
         END IF
         IF(messag.NE.' ') CALL WRTMSG(messag,0,0)
         RETURN
      END IF
      IF(typlev.EQ.-1) THEN
         IF(INDEX(levene,'SP')+INDEX(levene,'SN').GT.0) THEN
            IF(INDEX(levene,'SP').EQ.1) RETURN
            IF(INDEX(levene,'SN').EQ.1) RETURN
            CALL WRTMSG('<E> INCORRECT ORDER FOR SP OR SN',10,19)
            RETURN
         END IF
         jplus = INDEX(levene,'+')
         IF(jplus.EQ.0) THEN
            CALL WRTMSG('<W> CHECK FIELD',10,19)
            RETURN
         END IF
         IF(levene(jplus-1:jplus-1).EQ.'E')                             &
     &      jplus = INDEXF(levene,jplus+1,'+')
         IF(jplus.EQ.0) THEN
            CALL WRTMSG('<W> CHECK FIELD',10,19)
            RETURN
         END IF
         curlev = VALSTR(levene(1:jplus-1))
         if (curlev .GT. avail_energy .AND. DSType .EQ. 2 .AND.         &
     &      isenernum) then
            CALL WRTMSG('<E> Energy greater than available Q',10,19)
            RETURN
         end if
	 If(curlev .NE. 0)Then
            tmpstr=levene(1:jplus-1)
            thechr=levene(jplus+1:)
            if (bnrflag) then
              GoTo 100
            end if
            Do i=1,noff
	       If(thechr .EQ. offchr(i))GoTo 100
	    EndDo
	    Do i=1,nflvl-1 
	       If(tmpstr(1:LEN_TRIM(tmpstr)) .EQ. flvl(i))GoTo 100
	    EndDo
!           Attempt to handle cases where level energy should probably
!             be "E+X+Y" which is not currently allowed
	    Do i=1,nflvl-1
               If(INDEX(flvl(i),tmpstr(1:LEN_TRIM(tmpstr))).GT.0        &
     &           .OR. INDEX(flvl(i),thechr).GT.0)Then
                  messag='<W> No "'//thechr//'" or "'
                  Call Addstr(messag,LEN_TRIM(messag)+1,tmpstr)
                  Call Addstr(messag,LEN_TRIM(messag)+1,                &
     &              '" level assigned')
                  Call Wrtmsg(messag(1:LEN_TRIM(messag)),               &
     &              add+jplus+10,add+jplus+10)
                  If(INDEX(flvl(i),tmpstr(1:LEN_TRIM(tmpstr))).GT.0)Then
                     WRITE(messag,'(2A)')thechr,                        &
     &                 '" to be a valid character'
                     CALL ADDSTR(messag,1,'<I> Assuming "')
                     Call Wrtmsg(messag,0,0)
                  EndIf
                  GoTo 100
               EndIf
            EndDo
            write(*,*) 'Writing our message now'
            messag='<E> No "'//thechr//'" or "'
            Call Addstr(messag,LEN_TRIM(messag)+1,tmpstr)
	    Call Addstr(messag,LEN_TRIM(messag)+1,'" level assigned')
            Call Wrtmsg(messag(1:LEN_TRIM(messag)),add+jplus+10,        &
     &        add+jplus+10)
            Return
100         Continue
	 EndIf
         newchr = .FALSE.
         DO WHILE (.TRUE.)
            tmpstr = levene(1:jplus-1)
            levene = levene(jplus+1:)
            typlev = TYPSTR(tmpstr)
            IF(typlev.EQ.0.OR.typlev.EQ.-1) THEN
               CALL WRTMSG('<W> CHECK FIELD',10,19)
               RETURN
            END IF
            IF(typlev.EQ.1.OR.typlev.EQ.-2) THEN
               If(curlev .NE. VALSTR(tmpstr))Then
                  curlev = curlev + VALSTR(tmpstr)
               EndIf
            ELSE
               IF(LEN_TRIM(tmpstr).NE.1) THEN
                  CALL WRTMSG('<W> CHECK FIELD',10,19)
                  RETURN
               END IF
               i = INDEX(levene,tmpstr(1:1))
               IF(i.GT.0) THEN
                  IF(tmpstr.NE.'E') THEN
                     CALL WRTMSG('<W> CHECK FIELD',10,19)
                     RETURN
                  ELSE IF(.NOT.((levene(i+1:i+1).EQ.'+'.AND.TYPSTR(     &
     &                    levene(i+2:i+2)).EQ.1).OR.                    &
     &                    TYPSTR(levene(i+1:i+1)).EQ.1)) THEN
                     CALL WRTMSG('<W> CHECK FIELD',10,19)
                     RETURN
                  END IF
               END IF
               DO i = 1, NOFf
                  IF(tmpstr.EQ.OFFchr(i)) THEN
                     newchr = .FALSE.
                     curlev = curlev + OFFamt(i)
                     GO TO 5
                  END IF
               END DO
               IF(newchr) THEN
                  CALL WRTMSG('<W> CHECK FIELD',10,19)
                  RETURN
               END IF
               newchr = .TRUE.
    5          IF(newchr) THEN
                  chkord = .FALSE.
                  IF(NOFf.LT.maxoff) THEN
                     NOFf = NOFf + 1
                     OFFchr(NOFf) = tmpstr
                     IF(INDEX(chkchr,OFFchr(NOFf)).EQ.0) THEN
                        CALL WRTMSG('<W> UNDEFINED CHARACTER ASSIGNED', &
     &                              10,19)
                     ELSE
                        IF(OFFchr(NOFf).NE.chkchr(NOFf:NOFf))           &
     &                     CALL WRTMSG(                                 &
     &                     '<W> CHARACTER ASSIGNED OUT OF ORDER',10,19)
                     END IF
                     OFFamt(NOFf) = incrmnt*FLOAT(NOFf)
                     WRITE(messag,'(2A,F13.5)') OFFchr(NOFf),  '=',     &
     &                     OFFamt(NOFf)
                     CALL SQZSTR(messag,' ')
                     CALL ADDSTR(messag,1,'<I> Assuming ')
                  ELSE
                     CALL WRTMSG('<E> TOO MANY CHARACTERS ASSIGNED',10, &
     &                           19)
                     RETURN
                  END IF
               END IF
            END IF
            IF(levene.NE.' ') THEN
               jplus = INDEX(levene,'+')
               IF(jplus.EQ.0.OR.                                        &
     &            (levene(jplus-1:jplus-1).EQ.'E'.AND.jplus.NE.1))      &
     &            jplus = LEN_TRIM(levene) + 1
               CYCLE
            END IF
            curjpi = Card(22:39)
            CALL LBSUP(curjpi)
            curt12 = Card(40:49)
            CALL LBSUP(curt12)
            IF(chkord) THEN
               IF(curlev.LT.prelev) THEN
                  CALL WRTMSG('<E> E < PRECEDING LEVEL ENERGY',10,19)
                  BADord = .TRUE.
               END IF
               IF(curlev.EQ.prelev.AND.curjpi.EQ.prejpi.AND.            &
     &            curt12.EQ.pret12)                                     &
     &            CALL WRTMSG('<W> E = PRECEDING LEVEL ENERGY',10,19)
            ELSE IF(curlev.LE.prelev) THEN
               OFFamt(NOFf) = prelev - curlev + incrmnt
               curlev = prelev + incrmnt
               WRITE(messag,'(2A,F13.5)') OFFchr(NOFf), '=',            &
     &               OFFamt(NOFf)
               CALL SQZSTR(messag,' ')
               CALL ADDSTR(messag,1,'<I> Assuming ')
            END IF
            prelev = curlev
            prejpi = Card(22:39)
            CALL LBSUP(prejpi)
            pret12 = Card(40:49)
            CALL LBSUP(pret12)
            IF(messag.NE.' ') CALL WRTMSG(messag,0,0)
            RETURN
         END DO
      END IF
      CALL WRTMSG('<W> CHECK FIELD',10,19)
      IF(messag.NE.' ') CALL WRTMSG(messag,0,0)
!
      RETURN
      END SUBROUTINE CHKL
!
!***********************************************************************
!
      SUBROUTINE CHKN(Card)
!
!     CHECK NORMALIZATION CARD.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=80) :: Card
!
!     Functions used
!
      INTEGER(KIND=4), EXTERNAL :: TYPSTR
!
!     Local variables
!
      CHARACTER(LEN=2) :: dbrs, dnps
      REAL(KIND=4) :: br, dbr, dnp, dx, np, x
!
!     CHECK CARD SEQUENCE.
!
      IF(.NOT.ISEen) CALL WRTMSG('<F> DSID CARD NOT YET SEEN',0,0)
      IF(NSEen.AND.Card(9:9).EQ.' ')                                    &
     &   CALL WRTMSG('<E> N CARD ALREADY SEEN',8,8)
      NSEen = .TRUE.
      IF(GABeld) CALL WRTMSG('<F> N CARD ILLEGAL IN BODY OF DATA SET',0,&
     &                      0)
      IF(PNSeen) CALL WRTMSG('<E> PN CARD ALREADY SEEN',8,8)
      NCOl9 = Card(9:9)
      IF(NSave9(1) .GE. NSave9(2)) THEN
         CALL CKF9(NCOl9,'P')
      EndIf
      NSAve9(2) = NSAve9(2) + 1
      SAVe9(2,NSAve9) = NCOl9
!
!     VALIDATE NUCID.
!
      CALL CKNCID(Card)
      IF(TYPSTR(Card(9:9)).LT.0.OR.1.LT.TYPSTR(Card(9:9)))              &
     &   CALL WRTMSG('<E> MUST BE DIGIT OR BLANK',9,9)
!
!     Check to see if PN has already occurred (TWB. 930311)
      IF(PNSeen) CALL WRTMSG('<F> N CARD MUST PRECEDE PN',0,0)
!     Check against dataset type and decay type (TWB. 930308)
      IF(DSType.LE.1.OR.DSType.EQ.4) THEN
         CALL WRTMSG('<F> N CARD ILLEGAL FOR DATASET',0,0)
         RETURN
      ELSE IF(DSType.EQ.3) THEN
         NRSeen = Card(10:19).NE.' '
         CALL CKV(Card(10:19),10,.TRUE.,'<E>')
         CALL CKU(Card(20:21),20)
         NTSeen = Card(22:29).NE.' '
         CALL CKV(Card(22:29),22,.TRUE.,'<E>')
         CALL CKU(Card(30:31),30)
         CALL CNVS2U(Card(22:29),Card(30:31),x,dx)
         IF(x.EQ.0.0.AND.Card(22:22).EQ.'.')                            &
     &      CALL WRTMSG('<W> CHECK FOR POSSIBLE TYPING ERROR',20,22)
         CALL CKVU(Card(10:19),Card(20:21),10,21)
         CALL CKVU(Card(22:29),Card(30:31),22,31)
         CALL CKBLNK(Card(32:80),32,80)
         RETURN
      END IF
!
!     VALIDATE DATA FIELDS.
!
      NRSeen = Card(10:19).NE.' '
      CALL CKV(Card(10:19),10,.TRUE.,'<E>')
      CALL CKU(Card(20:21),20)
      NTSeen = Card(22:29).NE.' '
      CALL CKV(Card(22:29),22,.TRUE.,'<E>')
      CALL CKU(Card(30:31),30)
      CALL CNVS2U(Card(22:29),Card(30:31),x,dx)
      IF(x.EQ.0.0.AND.Card(22:22).EQ.'.')                               &
     &   CALL WRTMSG('<W> CHECK FOR POSSIBLE TYPING ERROR',20,22)
      CALL CKV(Card(32:39),32,.TRUE.,'<E>')
      CALL CKU(Card(40:41),40)
!     Special check for branching ratio. It should either be blank or
!     between 0.0 and 1.0 (TWB. 930308)
      IF(Card(32:39).NE.' ') THEN
         BRSeen = .TRUE.
         CALL CNVS2U(Card(32:39),Card(40:41),br,dbr)
         IF(br.GT.1.00001) CALL WRTMSG('<F> BR > 1.0',32,39)
         IF(br.EQ.0.0.AND.Card(32:32).EQ.'.')                           &
     &      CALL WRTMSG('<W> CHECK FOR POSSIBLE TYPING ERROR',30,32)
         IF(DECtyp.EQ.1) FN = 1.0
      END IF
      CALL CKVU(Card(10:19),Card(20:21),10,21)
      CALL CKVU(Card(22:29),Card(30:31),22,31)
      CALL CKVU(Card(32:39),Card(40:41),32,41)
!     Rest of N record must be blank for unknown, A, P, N, IT, and SF
!     decay (TWB. 930308)
      IF(DECtyp.LE.3.OR.(DECtyp.GE.6.AND.DECtyp.LE.7)) THEN
         CALL CKBLNK(Card(42:80),42,80)
         RETURN
      END IF
!     Only B- and B+/EC decay may have 42 to 55 non-blank and rest of
!     card must be blank (TWB. 930308)
      IF(DECtyp.EQ.4.OR.DECtyp.EQ.5) THEN
         NBSeen = Card(42:49).NE.' '
         CALL CKV(Card(42:49),42,.TRUE.,'<E>')
         CALL CKU(Card(50:55),50)
         CALL CNVS2U(Card(42:49),Card(50:55),x,dx)
         IF(x.EQ.0.0.AND.Card(42:42).EQ.'.')                            &
     &      CALL WRTMSG('<W> CHECK FOR POSSIBLE TYPING ERROR',40,42)
         CALL CKVU(Card(42:49),Card(50:55),42,55)
         CALL CKBLNK(Card(56:80),56,80)
         IF(NBSeen) CALL CNVS2U(Card(42:49),Card(50:55),FN,DFN)
         RETURN
      ELSE
         CALL CKBLNK(Card(42:55),42,55)
      END IF
      CALL CKV(Card(56:62),56,.TRUE.,'<E>')
      CALL CKU(Card(63:64),63)
!     Special check for delayed particle normalization. It should either
!     be blank or between 0.0 and 1.0 (TWB. 930308)
      IF(Card(56:62).NE.' ') THEN
         NPSeen = .TRUE.
         CALL CNVS2U(Card(56:62),Card(63:64),np,dnp)
         IF(np.GT.1.0001) CALL WRTMSG('<E> NP > 1.0',56,62)
         IF(np.EQ.0.0.AND.Card(56:56).EQ.'.')                           &
     &      CALL WRTMSG('<W> CHECK FOR POSSIBLE TYPING ERROR',54,56)
!        BR and NP should be equal (TWB. 930308)
**JKT    BR may not be eqal to NP (JKT 151215) comment out error message
!         IF(Card(32:41).EQ.' ') CALL WRTMS2('<E> BR .NE. NP',32,41,56,64)
!         ELSE
!            IF(br.NE.np) CALL WRTMS2('<E> BR .NE. NP',32,39,56,62)
!            dbrs = Card(40:41)
!            dnps = Card(63:64)
!            CALL LBSUP(dbrs)
!            CALL LBSUP(dnps)
!            IF(dbrs.NE.dnps) CALL WRTMS2('<E> DBR .NE. DNP',40,41,63,64)
!         END IF
!      ELSE
!         IF(Card(32:41).NE.' ')CALL WRTMS2('<E> BR .NE. NP',32,41,56,64)
**JKT
      END IF
      CALL CKVU(Card(56:62),Card(63:64),56,64)
      CALL CKBLNK(Card(65:80),65,80)
!
      RETURN
      END SUBROUTINE CHKN
!
!***********************************************************************
!
      SUBROUTINE CHKP(Card)
!
!     CHECK PARENT CARD.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=80) :: Card
!
!     Functions used
!
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
      INTEGER(KIND=4), INTRINSIC :: INDEX, LEN_TRIM
      INTEGER(KIND=4), EXTERNAL :: INDEXF, IVLSTR, TYPSTR
!
!     Local variables
!
      CHARACTER(LEN=38) :: message
      CHARACTER(LEN=5) :: tmpnuc
      CHARACTER(LEN=10) :: energy_str
      LOGICAL(KIND=4) :: odd
      LOGICAL(KIND=4) :: energy_num
      INTEGER(KIND=4) :: a, begin, ends, i, j, k, z, e
      REAL(KIND=4) :: dx, x
      REAL(KIND=4) :: penergy
!
!     CHECK CARD SEQUENCE.
!
      IF(.NOT.ISEen) CALL WRTMSG('<F> DSID CARD NOT YET SEEN',0,0)
      PCOl9 = Card(9:9)
      IF(.NOT.PSEen) THEN
         NPCard = 1
         IF(.NOT.NSEen) THEN
            NSAve9(1) = nsave9(1)+1
            SAVe9(1,nsave9(1)) = PCOl9
         ELSE
            CALL CKF9(PCOl9,'N')
         END IF
      ELSE
         IF(nsave9(2) .GT. nsave9(1)) THEN
            CALL CKF9(PCOl9,'N')
         EndIf
         NSAve9(1) = NSAve9(1) + 1
         SAVe9(1,NSAve9(1)) = PCOl9
         NPCard = NPCard + 1
      END IF
      IF(PSEen.AND.NPT12.LT.2.AND.Card(9:9).EQ.' ')                     &
     &   CALL WRTMSG('<W> P CARD ALREADY SEEN',8,8)
      PSEen = .TRUE.
      IF(GABeld) CALL WRTMSG('<F> P CARD ILLEGAL IN BODY OF DATA SET',0,&
     &                      0)
      IF(DSType.NE.2) THEN
         CALL WRTMSG('<F> P CARD ILLEGAL FOR THIS DATASET',8,8)
      ELSE IF(DECtyp.EQ.0) THEN
         CALL WRTMSG('<F> P CARD ILLEGAL FOR THIS DATASET',8,8)
      END IF
!
!     VALIDATE NUCID.
!
      CALL NUCID(Card(1:5),a,z)
      IF(a.EQ.0.OR.z.LT.0) THEN
         CALL WRTMSG('<F> INVALID NUCID',1,5)
      ELSE
         IF(IZLmsg.NE.' ') THEN
            CALL REPSTR(IZLmsg,'Obsolete formalism','Obsol. form')
            CALL ADDSTR(IZLmsg,1,'<W> ')
            CALL WRTMSG(TRIM(IZLmsg),4,5)
         END IF
         tmpnuc = Card(1:5)
         CALL LBSUP(tmpnuc)
         IF(NTOtsf.EQ.0) THEN
            IF(tmpnuc.NE.PARnucid)                                      &
     &         CALL WRTMSG('<E> NUCID MISMATCH WITH DSID PARENT',1,5)
         ELSE
            message = '<E> NUCID MISMATCH WITH DSID PARENT'
            DO i = 1, NTOtsf
               IF(tmpnuc.EQ.SFPar(i)) message = ' '
            END DO
            IF(message.NE.' ') CALL WRTMSG(message,1,5)
         END IF
      END IF
      IF(2*(a/2).NE.a) THEN
         odd = .TRUE.
      ELSE
         odd = .FALSE.
      END IF
      IF(TYPSTR(Card(9:9)).LT.0.OR.1.LT.TYPSTR(Card(9:9)))              &
     &   CALL WRTMSG('<E> MUST BE DIGIT OR BLANK',9,9)
!
!     VALIDATE DATA FIELDS.
!
      IF(Card(10:19).EQ.' ') CALL WRTMSG(                               &
     &                               '<E> ENERGY FIELD CAN NOT BE BLANK'&
     &                               ,10,19)
!
!     Special check for IT decay datasets
      If(dectyp .EQ. 6)Then
         If(Typstr(card(10:19)).NE.-1 .AND. Typstr(card(10:19)).NE.2)   &
     &     Then
            Call Cnvs2u(card(10:19),card(20:21),x,dx)
            If(x .EQ. 0.0)Call Wrtmsg('<E> GS Parent in IT decay',10,19)
	 End If 
      End If
!
! Checking the P card..TIM
      CALL CKE(Card(10:19),10,19,'<E>')
! ADDED TDJ
      energy_num=.FALSE.
      energy_str = TRIM(Card(10:19))
      READ(energy_str,*,IOSTAT=e) penergy 
      If (e.eq. 0) Then
        energy_num=.TRUE.
        write(*,*) 'energy_num is true'
      Else
        energy_num=.FALSE.
        write(*,*) 'energy_num is false'
      End If


      CALL CKU(Card(20:21),20)
      CALL CKJ(Card(22:39),22,39,odd,'<E>')
      IF(LEN_TRIM(Card(40:49)).EQ.0) THEN
         CALL WRTMSG('<E> MISSING T FIELD',40,49)
      ELSE
         CALL CKT(Card(40:49),40,49,'<E>')
         CALL CKU(Card(50:55),50)
      END IF
      CALL CKBLNK(Card(56:64),56,64)
      IF(Card(65:74).EQ.' ') THEN
         IF(DECtyp.LT.6.OR.DECtyp.GT.7)                                 &
     &      CALL WRTMSG('<E> MISSING QP FIELD',65,74)
         
      ELSE
         IF(DECtyp.EQ.7) CALL WRTMSG('<W> QP should be blank for SF',65,&
     &                              74)
         IF(DECtyp.EQ.6) THEN
            CALL CNVS2U(Card(65:74),Card(75:76),x,dx)
            IF(x.NE.0) CALL WRTMSG('<E> QP should be blank or 0 for IT',&
     &                            65,74)
            IF(dx.NE.0) CALL WRTMSG('<E> DQP should be blank for IT',75,&
     &                             76)
         ELSE
           CALL CNVS2U(Card(65:74),Card(75:76),x,dx)
           if (energy_num) Then
              avail_energy = x + penergy
              isenernum=.TRUE.
           else 
              isenernum=.FALSE.
           END IF
         END IF
         IF(ISIon) THEN
*JKT*Allow QP to be negative, comment out next record
             CALL CKVSGN(Card(65:74),65,.TRUE.,'<W>')
*JKT*
            CALL CKU(Card(75:76),75)
         ELSE
*TDJ* call ckvsgn here to allow QP to be negative
            CALL CKV(Card(65:74),65,.TRUE.,'<W>')
            CALL CKU(Card(75:76),75)
         END IF
      END IF
      if (DECtyp.EQ.7) then
            isenernum = .false.
      end if
      IF(ISIon) THEN
         IF(IVLSTR(Card(77:80)).NE.IONsta)                              &
     &      CALL WRTMSG('<E> Inconst. ionization',77,80)
      ELSE
         CALL CKBLNK(Card(77:80),77,80)
      END IF
!
!     CONSISTENCY BETWEEN UNC FIELDS AND THE MAIN VALUE FIELDS
!
      CALL CKVU(Card(10:19),Card(20:21),10,21)
      CALL CKVU(Card(40:49),Card(50:55),40,55)
      CALL CKVU(Card(65:74),Card(75:76),65,76)
!     Cross check between optional T1/2 info on DSID and PARENT info
      IF(NPT12.GT.0) THEN
         DO i = 1, NPT12
            IF(MATcht12(i)) CYCLE
!           Sometimes JPI given - allow that possibility
            IF(INDEX(TRIM(PT12(i)),' ').GT.0.OR.TRIM(PT12(i)).EQ.'?')   &
     &         THEN
               begin = 40
               ends = 49
            ELSE
               begin = 22
               ends = 39
            END IF
            IF(INDEX(Card(begin:ends),TRIM(PT12(i))).LE.0) THEN
               j = INDEXF(Card(1:ends),begin,'E')
               k = INDEX(PT12(i),'E')
               IF(j+k.EQ.0) CYCLE
               IF((j.EQ.0.OR.k.EQ.0).AND.(j+k).NE.0) CYCLE
               IF(INDEX(Card(begin:j-1),PT12(i)(1:k-1)).EQ.0) CYCLE
               IF(IVLSTR(Card(j+1:ends)).EQ.IVLSTR(PT12(i)(k+1:))) THEN
                  MATcht12(i) = .TRUE.
                  GO TO 100
               END IF
            ELSE IF(begin.NE.40.OR.DPT12(i).EQ.' ') THEN
               MATcht12(i) = .TRUE.
               GO TO 100
            ELSE IF(INDEX(Card(50:55),DPT12(i)).GT.0) THEN
               MATcht12(i) = .TRUE.
               GO TO 100
            END IF
         END DO
         CALL WRTMSG('<E> DISAGREES WITH DSID',begin,ends)
      END IF
!
  100 RETURN
      END SUBROUTINE CHKP
!
!***********************************************************************
!
      SUBROUTINE CHKPN(Card)
!
!     CHECK PRODUCTION NORMALIZATION CARD.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=80) :: Card
!
!     Local variables
!
      REAL(KIND=4) :: dx, x
!
      EXP2pn = .FALSE.
!
!     CHECK CARD SEQUENCE.
!
      IF(.NOT.ISEen) CALL WRTMSG('<F> DSID CARD NOT YET SEEN',0,0)
      If(PNSeen)Then
         Call Wrtmsg('<E> PN CARD ALREADY SEEN',6,9)
      EndIf
      IF(NSEen.AND.PRVtyp.NE.1)                                         &
     &   CALL WRTMSG('<E> PN CARD SHOULD FOLLOW N CARD',0,0)
      IF(GABeld)CALL WRTMSG('<F> PN CARD ILLEGAL IN BODY OF DATA SET',0,&
     &                      0)
!
      PNSeen = .TRUE.
!     VALIDATE NUCID.
!
      CALL CKNCID(Card)
      IF(NCOl9.NE.' ') THEN
         IF(NCOl9.NE.Card(9:9).AND.Card(9:9).NE.' ') THEN
            CALL WRTMSG('<F> COMMENT/PRIMARY CARD TYPE CONFLICT',8,9)
         END IF
      ELSE IF(Card(9:9).NE.' ') THEN
         CALL WRTMSG('<F> COMMENT/PRIMARY CARD TYPE CONFLICT',8,9)
      END IF
!     VALIDATE DATA FIELDS.
!
      CALL CKINTN(Card(78:78),78,78)
!     COMMENT and REFERENCE datasets cannot have PN records
!     (TWB. 930308)
      IF(DSType.EQ.0.OR.DSType.EQ.4) THEN
         CALL WRTMSG('<E> PN RECORD NOT ALLOWED FOR DATASET',0,0)
         RETURN
      END IF
!
!     No need to check a blank PN record. Issue a warning and return
      IF(Card(10:80).EQ.' ') THEN
         CALL WRTMSG('<W> Blank PN record',10,80)
         RETURN
      END IF
!
!     If OPTION=6, then check branching ratios; otherwise no check
      IF(Card(78:78).EQ.'6') THEN
         CHGbr = .TRUE.
      ELSE
         CHGbr = .FALSE.
      END IF
      IF(Card(78:78).EQ.' '.OR.                                         &
     &   (Card(78:78).GE.'2'.AND.Card(78:78).LE.'4')) PNAbs = .TRUE.
!     If ADOPTED dataset, cannot be absolute intensity and should be
!     option 6 (TWB. 930308)
      IF(DSType.EQ.1) THEN
         IF(PNAbs) THEN
            IF(Card(78:78).EQ.' ') THEN
               CALL WRTMSG('<E> DEFAULT OPTION(=3) NOT ALLOWED',78,78)
            ELSE
               CALL WRTMSG('<E> OPTIONS 2,3,4 NOT ALLOWED FOR ADOPTED', &
     &                     78,78)
            END IF
            PNAbs = .FALSE.
         ELSE
            IF(Card(78:78).NE.'6')                                      &
     &         CALL WRTMSG('<W> OPTION 6 RECOMMENDED FOR ADOPTED',78,78)
         END IF
         CALL CKBLNK(Card(10:76),10,76)
         IF(Card(77:77).EQ.' ') THEN
         ELSE IF(Card(77:77).EQ.'C') THEN
            EXP2pn = .TRUE.
         ELSE
            CALL WRTMSG('<E> SHOULD BE BLANK OR C',77,77)
         END IF
         CALL CKBLNK(Card(79:80),79,80)
         RETURN
      ELSE IF(NCOl9.NE.' '.AND.PNAbs) THEN
         CALL WRTMSG('<W> CHECK OPTION - MULTIPLE N RECORDS',78,78)
      END IF
      IF(Card(22:31).NE.' ') THEN
         CALL CNVS2U(Card(22:29),Card(30:31),x,dx)
         IF(x.EQ.0.0.AND.Card(22:22).EQ.'.')                            &
     &      CALL WRTMSG('<W> CHECK FOR POSSIBLE TYPING ERROR',20,22)
      END IF
      IF(Card(42:55).NE.' ') THEN
         IF(.NOT.(DSType.EQ.2.AND.(DECtyp.EQ.4.OR.DECtyp.EQ.5))) THEN
            CALL CKBLNK(Card(42:55),42,55)
         END IF
      END IF
      IF(DSType.NE.2.OR.(DECtyp.LT.8.OR.DECtyp.GT.13.OR.DCOl89.EQ.'**'))&
     &   THEN
         CALL CKBLNK(Card(56:64),56,64)
      ELSE IF(Card(56:62).NE.' ') THEN
         CALL CNVS2U(Card(56:62),Card(63:64),x,dx)
         IF(x.GT.1.00001) CALL WRTMSG('<E> NP > 1.0',56,62)
         IF(x.EQ.0.0.AND.Card(56:56).EQ.'.')                            &
     &      CALL WRTMSG('<W> CHECK FOR POSSIBLE TYPING ERROR',54,56)
      END IF
      IF(Card(10:19).NE.' '.AND.NSEen.AND..NOT.NRSeen)                  &
     &   CALL WRTMSG('<W> FIELD NOT BLANK BUT NR IS',10,19)
      CALL CKV(Card(10:19),10,.TRUE.,'<E>')
      CALL CKU(Card(20:21),20)
      IF(Card(22:29).NE.' '.AND.NSEen.AND..NOT.NTSeen)                  &
     &   CALL WRTMSG('<W> FIELD NOT BLANK BUT NT IS',22,29)
      CALL CKV(Card(22:29),22,.TRUE.,'<E>')
      CALL CKU(Card(30:31),30)
      IF(Card(42:49).NE.' '.AND.NSEen.AND..NOT.NBSeen)                  &
     &   CALL WRTMSG('<W> FIELD NOT BLANK BUT NB IS',42,49)
      CALL CKV(Card(42:49),42,.TRUE.,'<E>')
      CALL CKU(Card(50:55),50)
      IF(Card(56:62).NE.' '.AND.NSEen.AND..NOT.NPSeen)                  &
     &   CALL WRTMSG('<W> FIELD NOT BLANK BUT NP IS',56,62)
      CALL CKV(Card(56:62),56,.TRUE.,'<E>')
      CALL CKU(Card(63:64),63)
!
!     CONSISTENCY BETWEEN UNC FIELDS AND THE MAIN VALUE FIELDS
!
      CALL CKVU(Card(10:19),Card(20:21),10,21)
      CALL CKVU(Card(22:29),Card(30:31),22,31)
      CALL CKVU(Card(42:49),Card(50:55),42,55)
      CALL CKVU(Card(56:62),Card(63:64),56,64)
!     Columns 32 through 41 must also be blank (TWB. 930308)
      CALL CKBLNK(Card(32:41),32,41)
      CALL CKBLNK(Card(65:76),65,80)
      IF(Card(77:77).EQ.' ') THEN
      ELSE IF(Card(77:77).EQ.'C') THEN
         EXP2pn = .TRUE.
      ELSE
         CALL WRTMSG('<E> SHOULD BE BLANK OR C',77,77)
      END IF
      CALL CKBLNK(Card(79:80),79,80)
      IF(.NOT.NSEen.AND.PNAbs)                                          &
     &   CALL WRTMSG('<F> PN CARD SHOULD FOLLOW N CARD',0,0)
!
      RETURN
      END SUBROUTINE CHKPN
!
!***********************************************************************
!
      SUBROUTINE CHKQ(Card)
!
!     CHECK Q-VALUE CARD.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=80) :: Card
!
!     CHECK CARD SEQUENCE.
!
      IF(.NOT.ISEen) CALL WRTMSG('<F> DSID CARD NOT YET SEEN',0,0)
      IF(QSEen) CALL WRTMSG('<E> Q CARD ALREADY SEEN',8,8)
      QSEen = .TRUE.
      IF(GABeld) CALL WRTMSG('<F> Q CARD ILLEGAL IN BODY OF DATA SET',0,&
     &                      0)
!
!     VALIDATE NUCID.
!
      CALL CKNCID(Card)
      CALL CKBLNK(Card(9:9),9,9)
!
!     VALIDATE DATA FIELDS.
!
      CALL CKVSGN(Card(10:19),10,.TRUE.,'<E>')
      CALL CKU(Card(20:21),20)
      CALL CKVSGN(Card(22:29),22,.TRUE.,'<E>')
      CALL CKU(Card(30:31),30)
      CALL CKVSGN(Card(32:39),32,.TRUE.,'<E>')
      CALL CKU(Card(40:41),40)
      CALL CKVSGN(Card(42:49),42,.TRUE.,'<E>')
      CALL CKU(Card(50:55),50)
      CALL CKREF(Card(56:80),56)
!
!     CONSISTENCY BETWEEN UNC FIELDS AND THE MAIN VALUE FIELDS
!
      CALL CKVU(Card(10:19),Card(20:21),10,21)
      CALL CKVU(Card(22:29),Card(30:31),22,31)
      CALL CKVU(Card(32:39),Card(40:41),32,41)
      CALL CKVU(Card(42:49),Card(50:55),42,55)
!
      RETURN
      END SUBROUTINE CHKQ
!
!***********************************************************************
!
      SUBROUTINE CHKX(Card)
!
!     CHECK CROSS REFERENCE CARD.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=80) :: Card
!
!     Functions used
!
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
      INTEGER(KIND=4), EXTERNAL :: BREAK, INDEXF, SPAN
      INTEGER(KIND=4), INTRINSIC :: INDEX, LEN_TRIM
!
!     Local variables
!
      CHARACTER(LEN=30) :: colstr, xdsid
      INTEGER(KIND=4) :: a, colpos, i, j, l, m, r, z
!
!     Non-numeric uncertainties
!
      INTEGER(KIND=4), PARAMETER :: nonmax = 7
      CHARACTER(LEN=2), DIMENSION(nonmax) :: nonunc
      DATA nonunc/'LT', 'GT', 'LE', 'GE', 'AP', 'CA', 'SY'/
!
!     CHECK CARD SEQUENCE.
!
      IF(.NOT.ISEen) CALL WRTMSG('<F> DSID CARD NOT YET SEEN',0,0)
      IF(GABeld) CALL WRTMSG('<F> X CARD ILLEGAL IN BODY OF DATA SET',0,&
     &                      0)
      IF(DSType.NE.1.AND..NOT.HISpin)                                   &
     &   CALL WRTMSG('<F> CARD ILLEGAL FOR THIS DATASET',8,8)
      IF(SSYmbs.NE.' ')CALL WRTMSG('<E> S RECORD ALREADY ENCOUNTERED',8,&
     &                             8)
!
!     Save X card for latter global comparison if found in adopted
      IF(DSType.EQ.1) THEN
         IF(NXC.GE.1) THEN
            DO i = 1, NXC
               IF(Card(1:5).EQ.XCSave(i)(1:5).AND.Card(10:).EQ.XCSave(i)&
     &            (10:)) THEN
                  CALL WRTMSG('<E> Duplicate X record',10,LEN_TRIM(Card)&
     &                        )
                  EXIT
               END IF
            END DO
         END IF
         IF(NXC.LT.maxxc) THEN
            NXC = NXC + 1
            XCSave(NXC) = Card
         ELSE
            IF(SKIpxc.EQ.0) CALL WRTMSG(                                &
     &                               '<I> MAX COUNT FOR X CARD EXCEEDED'&
     &                               ,0,0)
            SKIpxc = SKIpxc + 1
         END IF
      END IF
!     VALIDATE NUCID.
!
      CALL CKNCID(Card)
      resnuc=card(1:5)
!
!     Save Isobaric data
      If(INDEX(card(10:39),'IAR').GT.0                                  &
     &  .OR. INDEX(card(10:39),'IAS').GT.0)Then
         isiar=.TRUE.
      Else
         isiar=.FALSE.
      EndIf
      tara=-9999
      tarz=-9999
      tarpos=-9999
!
!     VALIDATE XREF CHARACTER ONLY.
!
      r = LEN_TRIM(XREfs)
      IF(r.EQ.0) r = 1
!     SET BLANK AS INITIAL XREF, THUS NO STARTUP PROBLEM WITH ZERO
!     LENGTH STRINGS. ALSO WILL REPORT ERROR IF BLANK USED AS ID,
!     ALTHOUGH IT WILL BE THE ALREADY USED MESSAGE.
      IF(INDEX(XREfs(1:r),Card(9:9)).EQ.0) THEN
         r = r + 1
         XREfs(r:r) = Card(9:9)
      ELSE
         CALL WRTMSG('<E> XREF CODE ALREADY USED',9,9)
      END IF
!     Issue warnings if "(", ")", "*", ".", or "0" through "9"
      IF(Card(9:9).EQ.'('.OR.Card(9:9).EQ.')'.OR.Card(9:9).EQ.'*'.OR.   &
     &   Card(9:9).EQ.','.OR.Card(9:9).EQ.'.'.OR.                       &
     &   (Card(9:9).GE.'0'.AND.Card(9:9).LE.'9'))                       &
     &   CALL WRTMSG('<W> SYMBOL MAY BE MISINTERPETED',9,9)
!     Issue error if "$"
      IF(Card(9:9).EQ.'$') CALL WRTMSG('<E> INVALID SYMBOL',9,9)
!
      IF(LEN_TRIM(Card(10:39)).EQ.0) THEN
         CALL WRTMSG('<E> BLANK DSID',10,39)
      ELSE
         CALL CKBLNK(Card(40:80),40,80)
         l = 1
         xdsid = Card(10:39)
         IF(xdsid(1:1).EQ.' ') THEN
            CALL WRTMSG('<E> START DSID ON COLUMN 10',10,10)
            DO l = 1, 30
               IF(xdsid(l:l).NE.' ') EXIT
            END DO
         END IF
!        Ignore ":" and information following it
         i = INDEXF(xdsid,l,':')
         IF(i.GT.0) THEN
            r = LEN_TRIM(xdsid(1:i-1))
            colpos = i
            colstr = xdsid(i+1:)
         ELSE
            colpos = 0
            r = LEN_TRIM(xdsid)
         END IF
!        "," is a valid terminator but should be ignored in checking
         IF(xdsid(r:r).EQ.',') r = r - 1
         IF(INDEX(xdsid(l:r),'COMMENTS').GT.0) THEN
            CALL WRTMSG('<F> INVALID DATA SET ID FOR X CARD',9+l,9+r)
            RETURN
         END IF
         IF(INDEX(xdsid(l:r),'ADOPTED').GT.0) THEN
            CALL WRTMSG('<F> INVALID DATA SET ID FOR X CARD',9+l,9+r)
            RETURN
         END IF
         IF(INDEX(xdsid(l:r),'REFERENCES').GT.0) THEN
            CALL WRTMSG('<F> INVALID DATA SET ID FOR X CARD',9+l,9+r)
            RETURN
         END IF
         IF(INDEX(xdsid,'MUONIC ATOM').GT.0) THEN
            IF(xdsid(l:r).NE.'MUONIC ATOM')                             &
     &         CALL WRTMSG('<E> INVALID DECAY DATA SET ID',9+l,9+r)
            RETURN
         END IF
         CALL CKPUNC(xdsid(1:r),r)
         IF(INDEX(xdsid(l:r),'DECAY').GT.0) THEN
!           Check parent NUCID
            i = BREAK(xdsid(1:r),l,' ')
            IF(i.GT.r) GO TO 11
            CALL NUCID(xdsid(l:i-1),a,z)
            IF(a.EQ.0.OR.z.LT.0) THEN
               CALL WRTMSG('<F> INVALID PARENT ID',9+l,8+i)
            ELSE IF(IZLmsg.NE.' ') THEN
               CALL REPSTR(IZLmsg,'Obsolete formalism','Obsol. form')
               CALL ADDSTR(IZLmsg,1,'<W> ')
               CALL WRTMSG(TRIM(IZLmsg),12+l,8+i)
            END IF
            l = i
!           Check decay type
            i = SPAN(xdsid(1:r),l,' ')
            IF(i.GT.l+1) THEN
               CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID',9+l,8+i)
            END IF
            l = i
            i = BREAK(xdsid(1:r),l,' ')
            IF(i.GT.r) GO TO 11
            i = i - 1
            IF(RDECTYP(xdsid(l:i)).EQ.0)                                &
     &         CALL WRTMSG('<E> INVALID DECAY TYPE',9+l,9+i)
            l = i + 1
!           'DECAY' must be next, skip over it.
            i = SPAN(xdsid(1:r),l,' ')
            IF(i.GT.l+1) THEN
               CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID',9+l,8+i)
            END IF
            l = i
            i = BREAK(xdsid(1:r),l,' ')
            IF(xdsid(l:i-1).NE.'DECAY')                                 &
     &         CALL WRTMSG('<E> INVALID "DECAY"',9+l,8+i)
            l = i
!           Look for optional half-life and check it
            IF(l.LT.r) THEN
               i = SPAN(xdsid(1:r),l,' ')
               IF(i.GT.l+1) THEN
                  CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID',9+l,8+i)
               END IF
               l = i
               IF(xdsid(l:l).NE.'('.OR.xdsid(r:r).NE.')') THEN
                  CALL WRTMSG('<E> INVALID HALF-LIFE',9+l,9+r)
               ELSE
                  l = l + 1
                  r = r - 1
                  i = INDEX(xdsid(l:r),'+')
!                 LOOK FOR SEPARATOR.
                  IF(i.NE.0) THEN
                     j = l + i - 2
                     IF(xdsid(j:j).EQ.'E') THEN
!                       '+' IS PART OF NUM, NOT SEPARATOR.
                        j = j + 2
                        i = INDEX(xdsid(j:r),'+')
!                       LOOK FOR SEPARATOR ON THE RIGHT.
                        IF(i.NE.0) i = j - l + i
                     END IF
                  END IF
                  IF(i.EQ.0) THEN
                     IF(xdsid(l:l).EQ.' ')                              &
     &                  CALL WRTMSG('<E> LEADING BLANKS INVALID',9+l,   &
     &                  9+r)
                     IF(xdsid(r:r).EQ.' ')                              &
     &                  CALL WRTMSG('<E> TRAILING BLANKS INVALID',9+l,  &
     &                  9+r)
                     DO m = 1, nonmax
                        IF(xdsid(l:l+1).EQ.nonunc(m)) THEN
                           l = l + 3
                           EXIT
                        END IF
                     END DO
                     CALL CKT(xdsid(l:r),9+l,9+r,'<E>')
                  ELSE
                     i = i + l - 1
                     IF(xdsid(l:l).EQ.' ')                              &
     &                  CALL WRTMSG('<E> LEADING BLANKS INVALID',9+l,   &
     &                  8+i)
                     IF(xdsid(i-1:i-1).EQ.' ')                          &
     &                  CALL WRTMSG('<E> TRAILING BLANKS INVALID',9+l,  &
     &                  8+i)
                     DO m = 1, nonmax
                        IF(xdsid(l:l+1).EQ.nonunc(m)) THEN
                           l = l + 3
                           EXIT
                        END IF
                     END DO
                     CALL CKT(xdsid(l:i-1),9+l,8+i,'<E>')
                     IF(xdsid(i+1:i+1).EQ.' ')                          &
     &                  CALL WRTMSG('<E> LEADING BLANKS INVALID',10+i,  &
     &                  9+r)
                     IF(xdsid(r:r).EQ.' ')                              &
     &                  CALL WRTMSG('<E> TRAILING BLANKS INVALID',10+i, &
     &                  9+r)
                     DO m = 1, nonmax
                        IF(xdsid(i:i+2).EQ.nonunc(m)) THEN
                           i = i + 3
                           EXIT
                        END IF
                     END DO
                     CALL CKT(xdsid(i+1:r),10+i,9+r,'<E>')
                  END IF
               END IF
            END IF
            RETURN
         END IF
!        Assumed to be a reaction ID
!        Handle literal reaction types
         IF(xdsid(l:r).EQ.'INELASTIC SCATTERING'.OR.xdsid(l:r)          &
     &      .EQ.'(HI,XNG)'.OR.xdsid(1:r).EQ.'HIGH-SPIN LEVELS, GAMMAS') &
     &      RETURN
         IF(xdsid(l:17+l).EQ.'COULOMB EXCITATION') THEN
!           COULOMB EXCITATION may be followed by '(<ANY>*,<ANY>*)'
            l = 18 + l
            i = SPAN(xdsid(1:r),l,' ')
            IF(i.GT.l+1) THEN
               CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID',9+l,8+i)
            END IF
            l = i
            tara=resa
	    tarz=resz
            IF(l.LT.r) CALL CKREA(xdsid(l:r),9+l,9+r)
            RETURN
         END IF
         IF(colpos.GT.0) CALL CKCOL(colpos,colstr)
!        Handle normal reaction type.
!        If qualifier present, set R to ignore it hereafter.
         IF(xdsid(r-3:r).EQ.' RES'.OR.xdsid(r-3:r).EQ.' IAR'.OR.        &
     &      xdsid(r-3:r).EQ.' IAS') THEN
            r = r - 4
            i = LEN_TRIM(xdsid(1:r))
            IF(i.LT.r) CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID',   &
     &                            9+l+1,9+r)
            r = i
         END IF
!        Check target.
         i = SPAN(xdsid(1:r),l,' ')
         IF(i.GT.l) THEN
            CALL WRTMSG('<E> LEADING BLANKS INVALID',9+l,8+i)
            l = i
         END IF
    5    i = BREAK(xdsid(1:r),l,'(')
         IF(i.GT.r) GO TO 11
         IF(i.GT.l) THEN
            CALL NUCID(xdsid(l:i-1),a,z)
            tarpos=9+l
            If(a .GT. 0)tara=a
	    tarz=z
            IF(z.LT.0) THEN
               tara=-9999
	       tarz=-9999
               tarpos=-9999
               CALL WRTMSG('<E> INVALID REACTION TARGET',9+l,8+i)
            ELSE IF(IZLmsg.NE.' ') THEN
               CALL REPSTR(IZLmsg,'Obsolete formalism','Obsol. form')
               CALL ADDSTR(IZLmsg,1,'<W> ')
               CALL WRTMSG(TRIM(IZLmsg),12+l,8+i)
            END IF
            IF(xdsid(i-1:i-1).EQ.' ')                                   &
     &         CALL WRTMSG('<E> INVALID BLANK',9+i-1,9+i-1)
            l = i
         ELSE
            IF(INDEXF(xdsid(1:r),i,'(HI,').LE.0)                        &
     &         CALL WRTMSG('<E> INVALID REACTION TARGET',9+l,9+l)
         END IF
         DO WHILE (.TRUE.)
!           Check reaction.
            i = BREAK(xdsid(1:r),l,')')
            IF(i.GT.r) GO TO 11
            CALL CKREA(xdsid(l:i),9+l,9+i)
            l = i + 1
            IF(l.GE.r) RETURN
!           Check for more reactions.
            IF(xdsid(l:l).EQ.',') THEN
               l = l + 1
               i = SPAN(xdsid(1:r),l,' ')
               IF(i.GT.l+1) THEN
                  CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID',9+l,8+i)
               END IF
               l = i
!              Check for target (GOTO 3O0) or not (GOTO 350).
               IF(xdsid(l:l).EQ.'(') CYCLE
               GO TO 5
            END IF
            IF(l.GE.r) RETURN
!           Check for energy field.
            i = SPAN(xdsid(1:r),l,' ')
            IF(i.GT.l+1) THEN
               CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID',9+l,8+i)
            END IF
            l = i
            IF(xdsid(l:l+1).EQ.'E='.OR.xdsid(l:l+4).EQ.'E AP ') THEN
               IF(xdsid(l:l+1).EQ.'E=') THEN
                  l = l + 2
               ELSE
                  l = l + 5
               END IF
               i = SPAN(xdsid(1:r),l,' ')
               IF(i.GT.l+1) THEN
                  CALL WRTMSG('<E> EMBEDDED BLANK INVALID',9+l,8+i)
               END IF
               l = i
               IF(xdsid(l:r).EQ.'TH'.OR.xdsid(l:r).EQ.'RES'.OR.         &
     &            xdsid(l:r).EQ.'THERMAL'.OR.xdsid(l:r).EQ.'RESONANCE') &
     &            RETURN
!              Check energy value.
               i = l
               IF(xdsid(l:l+2).EQ.'TH,') THEN
                  l = l + 3
                  i = SPAN(xdsid(1:r),l,' ')
                  IF(i.GT.l+1)                                          &
     &               CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID',9+l, &
     &               8+i)
               ELSE IF(xdsid(l:l+3).EQ.'RES,') THEN
                  l = l + 4
                  i = SPAN(xdsid(1:r),l,' ')
                  IF(i.GT.l+1)                                          &
     &               CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID',9+l, &
     &               8+i)
               END IF
               DO WHILE (.TRUE.)
                  l = i
                  i = BREAK(xdsid(1:r),l,' ,-')
                  IF(i.GT.r) GO TO 11
                  CALL CKV(xdsid(l:i-1),9+l,.TRUE.,'<e>')
                  IF(xdsid(i:i).EQ.',') THEN
                     l = i + 1
                     i = SPAN(xdsid(1:r),l,' ')
                     IF(i.GT.l+1)                                       &
     &                  CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID',  &
     &                  9+l,8+i)
                     CYCLE
                  ELSE IF(xdsid(i:i).EQ.'-') THEN
                     l = i + 1
                     i = SPAN(xdsid(1:r),l,' ')
                     IF(i.GT.l)CALL WRTMSG('<E> LEADING BLANKS INVALID',&
     &                  9+l,8+i)
                     CYCLE
                  END IF
                  l = i
                  i = SPAN(xdsid(1:r),l,' ')
                  IF(i.GT.l+1)                                          &
     &               CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID',9+l, &
     &               8+i)
                  l = i
                  i = BREAK(xdsid(1:r),l,' ')
                  IF(xdsid(l:i-1).NE.'EV'.AND.xdsid(l:i-1).NE.'EV'.AND. &
     &               xdsid(l:i-1).NE.'KEV'.AND.xdsid(l:i-1)             &
     &               .NE.'MEV'.AND.xdsid(l:i-1).NE.'GEV')               &
     &               CALL WRTMSG('<E> INVALID UNITS',9+l,8+i)
                  IF(i.LE.r) CALL WRTMSG('<E> JUNK AT END OF FIELD',9+i,&
     &                                  9+r)
                  GO TO 10
               END DO
            ELSE
               CALL WRTMSG('<E> JUNK AT END OF FIELD',9+l,9+r)
            END IF
   10       EXIT
         END DO
      END IF
      RETURN
   11 CALL WRTMSG('<E> JUNK AT END OF FIELD',9+l,39)
!
      RETURN
      END SUBROUTINE CHKX
!
!***********************************************************************
!
      SUBROUTINE CKBLNK(Field,From,To)
!
!     CHECK IF FIELD IS BLANK, AS IT SHOULD BE.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Field
      INTEGER(KIND=4) :: From, To
!
!     CHECK FIELD FOR BLANKS.
!
      IF(Field.NE.' ') CALL WRTMSG('<E> FIELD(S) MUST BE BLANK',From,To)
!
      RETURN
      END SUBROUTINE CKBLNK
!
!***********************************************************************
!
      SUBROUTINE CKC(Field,From,To)
!
!     CHECKS THE VALIDITY OF COMMENT FLAG FIELD.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Field
      INTEGER(KIND=4) :: From, To
!
!     Local variables
!
      INTEGER(KIND=4) :: icard, iflag
!
      IF(Field.EQ.' ') RETURN
      IF(RTYpe.EQ.8.AND.                                                &
     &   (Field.EQ.'*'.OR.Field.EQ.'&'.OR.Field.EQ.'@'.OR.Field.EQ.'%'))&
     &   THEN
!        Special symbols for GAMMA's
      ELSE IF(.NOT.(5.LE.RTYpe.AND.RTYpe.LE.7.AND.Field.EQ.'C'.OR.      &
     &        Field.EQ.'?')) THEN
!        COINCIDENCE.
         icard = RTYpe - 3
         IF(RTYpe.EQ.11) icard = 6
!        FIX FOR D CARDS.
         DO iflag = 1, NFLag(icard)
            IF(FLAg(iflag,icard).EQ.Field) THEN
               FLGref(iflag,icard) = .TRUE.
               RETURN
            END IF
         END DO
         if (CONTYp.EQ.1.AND.Field.EQ.',') then
           CALL WRTMSG('<W> Comma in FLAG list. Check end card',From,To)
           return
         end if
         CALL WRTMSG('<E> INVALID FLAG CHARACTER',From,To)
      END IF
!
      RETURN
      END SUBROUTINE CKC
!
!***********************************************************************
!
      SUBROUTINE CKCFLD(Field,From,To,Conflct)
!
!     CHECK CONTINUATION FIELD.
!
!     FORMS ARE:
!     FIELD-NAME OP VALUE [UNCERT]                   [(REF[,REF]...)]
!     FIELD-NAME OP VALUE [UNCERT] OP VALUE [UNCERT] [(REF[,REF]...)]
!     FLD[:FLD]... OP VAL[:VAL]... [UNCERT]          [(REF[,REF]...)]
!     'D'FLD-NAM =              UNCERT               [(REF[,REF]...)]
!     'T'        OP VALUE UNITS [UNCERT]             [(REF[,REF]...)]
!     'T'        OP VALUE UNITS [UNCERT] OP VALUE UNITS [UNCERT]
!     [(REF[,REF]...)]
!     'WIDTH??' FIELDS ALSO HAVE UNITS
!     'ISPIN' [=]SPIN                                [(REF[,REF]...)]
!     'ISPINZ' [=][+|-]SPIN                          [(REF[,REF]...)]
!     '%'FIELD = ?                                   [(REF[,REF]...)]
!     CONF = ANY TEXT BUT CANNOT HAVE SQUARE BRACKETS AND MUST HAVE
!     BALANCED PARENTHESES
!     FLAG = SEQUENCE OF FLAG CHARACTERS
!     XREF = (SEE CKXREF)
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Field
      LOGICAL(KIND=4) :: Conflct
      INTEGER(KIND=4) :: From, To
!
!     Functions used
!
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
      INTEGER(KIND=4), INTRINSIC :: INDEX, LEN, MIN0, LEN_TRIM
      INTEGER(KIND=4), EXTERNAL :: BREAK, INDEXF, IVLSTR, SPAN, TYPSTR
      REAL(KIND=4), EXTERNAL :: VALSTR
!
!     Local variables
!
      CHARACTER(LEN=4) :: test
      CHARACTER(LEN=10) :: flev
      CHARACTER(LEN=1) :: l1, l2
      LOGICAL(KIND=4) :: angmom, fl, gfactor, ioninf, islimt, ispin,    &
     &                   ispinz, jwarn, mrwarn, odd, pct, ratio, s,     &
     &                   signed, spin, twarn, uncert, units, chques
      INTEGER(KIND=4) :: i, j, k, l, lold, nratio, r, savlvl
!
      IF(2*(DSA/2).NE.DSA) THEN
         odd = .TRUE.
      ELSE
         odd = .FALSE.
      END IF
      jwarn = .FALSE.
      mrwarn = .FALSE.
      twarn = .FALSE.
      signed = .FALSE.
      gfactor = .FALSE.
      r = LEN_TRIM(Field)
      l = SPAN(Field(1:r),1,' ')
!     CONF, FLAG, AND XREF ARE SPECIAL.
!     So is possibility of parentheses around sign for static momments
!     (TWB. 930323)
      IF(Field(l:MIN0(l+3,LEN(Field))).EQ.'CONF') THEN
!        Check for lack of leading parentheses, square brackets,
!        unbalanced parentheses, and missing operator
!        (TWB. 930323)
         j = SPAN(Field(1:r),l+4,' =')
         IF(INDEX(Field(l+4:j),'=').EQ.0.AND.INDEX(Field(l+4:j),'EQ')   &
     &      .EQ.0) CALL WRTMSG('<E> INVALID RELATION',From+l+3,From+j-2)
         l = j
         IF(Field(j:j).NE.'(') THEN
            CALL WRTMSG('<E> JUNK AT END OF FIELD',From-1+l,From-1+r)
            RETURN
         END IF
         IF(INDEX(Field(l:r),'[').GT.0.OR.INDEX(Field(l:r),']').GT.0)   &
     &      CALL WRTMSG('<E> SQUARE BRACKETS NOT ALLOWED',From+l-1,     &
     &                  From+r-1)
         j = 0
         DO i = l, r
            IF(Field(i:i).EQ.'(') j = j + 1
            IF(Field(i:i).EQ.')') j = j - 1
         END DO
         IF(j.NE.0) CALL WRTMSG('<E> UNBALANCED PARENTHESES',From+l-1,  &
     &                         From+r-1)
         RETURN
      ELSE IF(Field(l:MIN0(l+3,LEN(Field))).EQ.'FLAG') THEN
         k = INDEXF(Field(1:r),l+4,'=')
         IF(k.EQ.0) THEN
            CALL WRTMSG('<E> INVALID FIELD',From-1+l,From-1+r)
            RETURN
         ELSE
            k = k + 1
            IF(k.LE.LEN(Field)) THEN
               DO WHILE (Field(k:k).EQ.' ')
                  k = k + 1
                  IF(k.LE.LEN(Field)) CYCLE
                  EXIT
               END DO
            ELSE
               k = r + 1
            END IF
         END IF
         IF(k.GT.r) THEN
            CALL WRTMSG('<E> MISSING FLAG VALUE',From-1+k,From-1+k)
         ELSE
            DO i = k, r
               CALL CKC(Field(i:i),From-1+i,From-1+i)
!              Is the level a band member
               IF(RTYpe.EQ.4) THEN
                  IF(INDEX(BSYmbs,Field(i:i)).GT.0) THEN
                     INBand = .TRUE.
                     IF(Conflct) THEN
                        CALL WRTMSG('<W> Band info not retained',       &
     &                              From-1+i,From-1+i)
                     ELSE IF(FBAnd(NFLvl).NE.' ') THEN
                        CALL WRTMSG('<W> Assigned to Band '//           &
     &                              FBAnd(NFLvl),From-1+i,From-1+i)
                     ELSE
                        FBAnd(NFLvl) = Field(i:i)
                     END IF
                  END IF
! New SEQ info here 04/21/2017
                  IF(INDEX(SEQSYmbs,Field(i:i)).GT.0) THEN
                     INSEQ = .TRUE.
                     IF(Conflct) THEN
                        CALL WRTMSG('<W> SEQ info not retained',        &
     &                              From-1+i,From-1+i)
                     ELSE IF(FSEq(NFLvl).NE.' ') THEN
                        CALL WRTMSG('<W> Assigned to SEQ ' //           &
     &                              FSEq(NFLvl),From-1+i,From-1+i)
                     ELSE
                        FSEq(NFLvl) = Field(i:i)
                     END IF
                  END IF
               END IF
!              If "%" on GAMMA, parent level should be band member
               IF(RTYpe.EQ.8.AND.Field(i:i).EQ.'%'.AND..NOT.INBand)     &
     &            CALL WRTMSG('<E> PARENT LEVEL NOT BAND MEMBER',       &
     &                        From-1+i,From-1+i)
            END DO
         END IF
         RETURN
      ELSE IF(Field(l:MIN0(l+3,LEN(Field))).EQ.'XREF') THEN
         k = INDEXF(Field(1:r),l,'=')
         IF(k.EQ.0) THEN
            CALL WRTMSG('<E> INVALID FIELD',From-1+l,From-1+r)
            RETURN
         ELSE
            k = k + 1
            IF(k.LE.LEN(Field)) THEN
               DO WHILE (Field(k:k).EQ.' ')
                  k = k + 1
                  IF(k.LE.LEN(Field)) CYCLE
                  EXIT
               END DO
            ELSE
               k = r + 1
            END IF
         END IF
         IF(k.GT.r) THEN
            CALL WRTMSG('<E> MISSING XREF VALUE',From-1+k,From-1+k)
         ELSE
            CALL CKXREF(Field(k:r),From-1+k,From-1+r,Conflct)
         END IF
         RETURN
      ELSE IF(Field(l:MIN0(l+3,LEN(Field))).EQ.'MOMM'.OR.               &
     &        Field(l:MIN0(l+3,LEN(Field))).EQ.'MOME'.OR.Field(l:l)     &
     &        .EQ.'G') THEN
         i = INDEX(Field,'(')
         j = INDEX(Field,')')
         IF(j-i.EQ.2) THEN
            IF(Field(i+1:i+1).EQ.'+'.OR.Field(i+1:i+1).EQ.'-') THEN
               CALL DELSTR(Field,j,1)
               CALL DELSTR(Field,i,1)
               r = r - 2
            END IF
         END IF
         signed = .TRUE.
         IF(Field(l:l).EQ.'G') gfactor = .TRUE.
      END IF
!
!     ISOLATE FIELD-NAME AND CHECK IT.
!
      i = BREAK(Field(1:r),l,' <=>')
      IF(i.GT.r) THEN
         CALL WRTMSG('<E> INVALID FIELD',From-1+l,From-1+r)
         RETURN
      END IF
      IF(i.EQ.r) THEN
         CALL WRTMSG('<E> MISSING VALUE',From+r,To)
         RETURN
      ELSE IF((TYPSTR(Field(i+1:i+1)).LT.0.OR.TYPSTR(Field(i+1:i+1))    &
     &        .GT.2).AND.                                               &
     &        .NOT.(Field(i+1:i+1).EQ.'-'.OR.Field(i+1:i+1).EQ.'+'.OR.  &
     &        Field(i+1:i+1).EQ.'(').AND.Field(i+1:i+1).NE.'?') THEN
         CALL WRTMSG('<E> INVALID RELATION',From-1+i,From+i)
         RETURN
      END IF
      angmom = .FALSE.
      fl = .FALSE.
      ioninf = .FALSE.
      ispin = .FALSE.
      ispinz = .FALSE.
      pct = .FALSE.
      ratio = .FALSE.
      s = .FALSE.
      spin = .FALSE.
      units = .FALSE.
      uncert = .FALSE.
      chques=.FALSE.
      k=1
      If(Field(l:l) .EQ. 'B')Then
         Do While (k.LE.5 .AND. .NOT.chques)
	    Write(test,'(A,I1,A)')'BE',k,'W'
	    If (Field(l:i-1) .EQ. test)chques=.TRUE.
	    Write(test,'(A,I1,A)')'BM',k,'W'
	    If (Field(l:i-1) .EQ. test)chques=.TRUE.
	    k=k+1
	 EndDo
      EndIf
      IF(Field(l:i-1).EQ.'T') THEN
         units = .TRUE.
         IF((RTYpe.EQ.4.AND.LLAbon(4)).OR.(RTYpe.EQ.11.AND.DLAbon(6)))  &
     &      twarn = .TRUE.
      ELSE IF(Field(l:i-1).EQ.'S') THEN
         s = .TRUE.
      ELSE IF(i-l.GE.5.AND.Field(l:MIN0(l+4,LEN(Field))).EQ.'WIDTH')THEN
         CALL CKSYM(Field(l:i-1),From-1+l,From-2+i)
!        Allow for forms such as WIDTHG0**2/WIDTH (TWB. 930312)
         IF(INDEX(Field(l:i-1),'/').EQ.0.OR.INDEX(Field(l:i-1),'**2/')  &
     &      .GT.0) THEN
            units = .TRUE.
            twarn = .FALSE.
         END IF
!        Angular Distributions may be signed
      ELSE IF(Field(l:l).EQ.'A'.AND.                                    &
     &        (Field(l+1:l+1).GE.'0'.AND.Field(l+1:l+1).LE.'9')) THEN
         signed = .TRUE.
!        Allow for DCO, which may be signed
      ELSE IF(Field(l:MIN0(l+2,LEN(Field))).EQ.'DCO') THEN
!        Allow for POL (polarizations)
      ELSE IF(Field(l:MIN0(l+2,LEN(Field))).EQ.'POL') THEN
         signed = .TRUE.             
      ELSE IF(Field(l:l).EQ.'D') THEN
         uncert = .TRUE.
         CALL CKSYM(Field(l:i-1),From-1+l,From-2+i)
      ELSE IF(Field(l:MIN0(l,LEN(Field))).EQ.'J') THEN
         spin = .TRUE.
         IF(RTYpe.EQ.4.AND.LLAbon(3)) jwarn = .TRUE.
      ELSE IF(Field(l:MIN0(l+4,LEN(Field))).EQ.'ISPIN') THEN
         ispin = .TRUE.
         ispinz = .FALSE.
         IF(i.GT.l+5) THEN
            IF(Field(l+5:i-1).EQ.'Z') THEN
               ispinz = .TRUE.
            ELSE
               CALL WRTMSG('<E> INVALID FIELD NAME',From-1+l,From-2+i)
            END IF
         END IF
      ELSE IF(Field(l:MIN0(l+1,LEN(Field))).EQ.'MR') THEN
         signed = .TRUE.
         IF(RTYpe.EQ.8.AND.GLAbon(6)) mrwarn = .TRUE.
      ELSE IF(INDEX(Field(l:i-1),':').GT.0) THEN
         ratio = .TRUE.
         nratio = 0
         j = l
         DO WHILE (.TRUE.)
            j = INDEXF(Field(1:i-1),j,':')
            IF(j.GT.0) THEN
               nratio = nratio + 1
               j = j + 1
               CYCLE
            END IF
            CALL CKSYM(Field(l:i-1),From-1+l,From-2+i)
            EXIT
         END DO
      ELSE IF(ISIon.AND.Field(l:MIN0(l+2,LEN(Field))).EQ.'ION') THEN
         signed = .TRUE.
         ioninf = .TRUE.
      ELSE
         CALL CKSYM(Field(l:i-1),From-1+l,From-2+i)
         IF(Field(l:i-1).EQ.'FL')Then
            fl = .TRUE.
            If(flseen)Then
               Call Wrtmsg('<E> FL= already encountered',9+l,9+i)
            Else
               flseen=.TRUE.
            EndIf
         EndIf
         IF(Field(l:i-1).EQ.'L'.AND.RTYpe.EQ.4) angmom = .TRUE.
!        There was never a check for MULT on continuation record (TWB.
!        961218)
         IF(Field(l:i-1).EQ.'MULT') THEN
            IF(RTYpe.EQ.8.AND.GLAbon(5)) THEN
               CALL CKM(Field(i+1:r),From+i,From-1+r,'<W>')
            ELSE
               CALL CKM(Field(i+1:r),From+i,From-1+r,'<E>')
            END IF
            RETURN
         END IF
      END IF
      IF(Field(l:l).EQ.'%') THEN
         pct = .TRUE.
         NOBrpct = .FALSE.
         LEVstr = ' '
      END IF
      l = i
!
!     CHECK FOR RELATION (OP).
!
      IF(ioninf.OR.ispin.OR.spin.OR.uncert.OR.fl) THEN
         l = SPAN(Field(1:r),l,' ')
         IF(Field(l:l).EQ.'=') THEN
            l = l + 1
         ELSE IF(uncert.OR.fl) THEN
            CALL WRTMSG('<E> RELATION MUST BE "="',From-1+l,From-1+l)
         END IF
      ELSE
         CALL CKCOP(Field,From,l,r,islimt,l1)
         IF(l.GT.r) RETURN
      END IF
!
!     CHECK FOR VALUE.
!
      Call Ckrange(Field,From,To)
      l = SPAN(Field(1:r),l,' ')
      IF(units) THEN
!        FIND END OF VALUE AND UNITS.
         i = BREAK(Field(1:r),l,' ')
         i = SPAN(Field(1:r),i,' ')
         i = BREAK(Field(1:r),i,' (<>')
!        CHECK VALUE AND UNITS.
         IF(twarn) THEN
            CALL CKT(Field(l:i-1),From-1+l,From-2+i,'<W>')
         ELSE
            CALL CKT(Field(l:i-1),From-1+l,From-2+i,'<E>')
         END IF
      ELSE IF(uncert) THEN
         i = BREAK(Field(1:r),l,' (')
         CALL CKU(Field(l:i-1),From-1+l)
      ELSE IF(ispin) THEN
!        ISPIN should be handled differently from J according to JKT
!        (TWB. 961218)
         IF(ispinz) THEN
            IF(Field(l:l).EQ.'+'.OR.Field(l:l).EQ.'-') THEN
               l = l + 1
            ELSE IF(Field(l:MIN0(l+2,LEN(Field))).EQ.'(+)'.OR.          &
     &              Field(l:MIN0(l+2,LEN(Field))).EQ.'(-)') THEN
               l = l + 3
               IF(Field(r:r).EQ.')') THEN
                  r = r - 1
               ELSE
                  CALL WRTMSG('<E> MISSING ")"',From-1+r,From-1+r)
               END IF
            ELSE IF(Field(l:l+1).EQ.'(+'.OR.Field(l:l+1).EQ.'(-') THEN
               l = l + 2
               IF(Field(r:r).EQ.')') THEN
                  r = r - 1
               ELSE
                  CALL WRTMSG('<E> MISSING ")"',From-1+r,From-1+r)
               END IF
            END IF
            IF(Field(r:r).EQ.')') THEN
               CALL WRTMSG('<E> MISSING "("',From-1+l,From-1+l)
               r = r - 1
            END IF
         END IF
         IF(l.LE.r) THEN
            i = INDEXF(Field,l,' (')
            IF(i.EQ.0) i = r + 1
            CALL CKISPIN(Field(l:i-1),From-1+l,From-2+i,odd)
            IF(i-1.EQ.r) RETURN
         END IF
      ELSE IF(spin) THEN
         i = BREAK(Field(1:r),l,' (')
         IF(i.EQ.l) THEN
            i = BREAK(Field(1:r),i+1,' (')
         ELSE IF(i+1.LE.r) THEN
            IF(INDEX(Field(i+1:r),'&').GT.0) THEN
               IF(Field(i+3:i+3).EQ.')'.AND.Field(i+2:i+2).GE.'0'.AND.  &
     &            Field(i+2:i+2).LE.'9'.AND.Field(i+1:i+1).EQ.'&')      &
     &            i = BREAK(Field(1:r),i+1,' (')
            ELSE
               IF(Field(i+2:i+2).EQ.')'.AND.Field(i+1:i+1).GE.'0'.AND.  &
     &            Field(i+1:i+1).LE.'9') i = BREAK(Field(1:r),i+1,' (')
            END IF
         END IF
         IF(jwarn) THEN
            CALL CKJ(Field(l:i-1),From-1+l,From-2+i,odd,'<W>')
         ELSE
            CALL CKJ(Field(l:i-1),From-1+l,From-2+i,odd,'<E>')
         END IF
      ELSE IF(angmom) THEN
         IF(RTYpe.EQ.4.AND.LLAbon(6)) THEN
            CALL CKL(Field(i+1:r),From+i,From-1+r,'<W>')
         ELSE
            CALL CKL(Field(i+1:r),From+i,From-1+r,'<E>')
         END IF
         RETURN
      ELSE IF(ratio) THEN
         DO WHILE (.TRUE.)
            j = INDEXF(Field(1:r),l,':')
            IF(j.GT.0) THEN
               nratio = nratio - 1
               l = SPAN(Field(1:j-1),l,' ')
               i = BREAK(Field(1:j-1),l,' ')
               IF(.NOT.(signed)) THEN
                  CALL CKVPRN(Field(l:i-1),From-1+l,.FALSE.,'<E>')
               ELSE IF(mrwarn) THEN
                  CALL CKVPS(Field(l:i-1),From-1+l,.FALSE.,'<W>')
               ELSE
                  CALL CKVPS(Field(l:i-1),From-1+l,.FALSE.,'<E>')
               END IF
               IF(i.LT.j) CALL CKU(Field(i:j-1),From-1+i)
               l = j + 1
               CYCLE
            END IF
            IF(nratio.NE.0) THEN
               CALL WRTMSG('<E> INVALID RATIO COUNT',From-1+l,From-1+r)
            END IF
            l = SPAN(Field(1:r),l,' ')
            i = BREAK(Field(1:r),l,' (')
            IF(i.EQ.l) i = BREAK(Field(1:r),l+1,' (')
            IF(.NOT.(signed)) THEN
               CALL CKVPRN(Field(l:i-1),From-1+l,.FALSE.,'<E>')
            ELSE IF(mrwarn) THEN
               CALL CKVPS(Field(l:i-1),From-1+l,.FALSE.,'<W>')
            ELSE
               CALL CKVPS(Field(l:i-1),From-1+l,.FALSE.,'<E>')
            END IF
            EXIT
         END DO
      ELSE IF(s) THEN
         l = SPAN(Field(1:r),l,' ')
         i = BREAK(Field(1:r),l,' (')
         IF(i.EQ.l) i = BREAK(Field(1:r),l+1,' (')
         IF(.NOT.LLAbon(7)) THEN
            CALL CKS(Field(l:i-1),From-1+l,From-2+i,'<E>')
         END IF
      ELSE
!        FIND END OF VALUE.
         IF(Field(l:l).EQ.'(') l = l + 1
         i = BREAK(Field(1:r),l,' (<>')
         IF(Field(i-1:i-1).EQ.')') i = i - 1
!        CHECK VALUE.
         IF(Field(l:i-1).EQ.'?') THEN
            IF(.NOT.pct.AND..NOT.fl)                                    &
     &         CALL WRTMSG('<E> INVALID VALUE FOR THIS FIELD TYPE',     &
     &         From-1+l,From-2+i)
            IF(fl) DXFlv(NDXg) = '?'
         ELSE
            IF(fl) THEN
               flev = Field(l:i-1)
               If(field(i:) .NE. ' ')                                   &
     &           Call Wrtmsg('<E> JUNK AT END OF FIELD',from+i-1,to)
               CALL LBSUP(flev)
               DO j = 1, NFLvl
                  IF(flev.EQ.FLVl(j)) THEN
                     IF(Conflct) THEN
                        CALL WRTMSG('<W> FL info not retained',From-1+l,&
     &                              From-2+i)
                        IF(XREfs.NE.' ')                                &
     &                     CALL WRTMSG('<W> XREF overlap not checked',  &
     &                     From-1+l,From-2+i)
                        GO TO 10
                     ELSE
                        IF(NDXg.LE.0) THEN
                           GO TO 10
                        END IF
                        DXFlv(NDXg) = flev
                        CALL LBSUP(DXFlv(NDXg))
                        savlvl = j
                        GO TO 5
                     END IF
                  END IF
               END DO
               IF(flev.NE.'?')                                          &
     &            CALL WRTMSG('<E> MATCHING LEVEL NOT FOUND',From-1+l,  &
     &            From-2+i)
               GO TO 10
!              Check to see if final level is consistent with
!              E(level)-Egamma
    5          CALL CKFLVAL(TRIM(flev),From-1+l,From-2+i)
!              Check to see if there is an overlap in XREF between
!              parent and daughter levels
               IF(FXRef(NFLvl).NE.' '.AND.FXRef(savlvl).NE.' '.AND.     &
     &            FXRef(NFLvl).NE.'+'.AND.FXRef(savlvl).NE.'+') THEN
                  DO j = 1, LEN_TRIM(FXRef(NFLvl))
                     IF(INDEX(FXRef(savlvl),FXRef(NFLvl)(j:j)).GT.0)    &
     &                  GO TO 10
                  END DO
                  CALL WRTMSG('<W> XREF''s inconsistent',From-1+l,      &
     &                        From-2+i)
               END IF
!              No need to check value after FL= since it must match a
!              previous level (TWB. 930323)
            ELSE IF(signed) THEN
               IF(gfactor) THEN
                  IF(Field(l:i-1).NE.'+'.AND.Field(l:i-1).NE.'-')       &
     &               CALL CKVSGN(Field(l:i-1),From-1+l,.FALSE.,'<E>')
               ELSE
                  CALL CKVSGN(Field(l:i-1),From-1+l,.FALSE.,'<E>')
               END IF
            ELSE
               If(chques .AND. Field(i-1:i-1).EQ.'?')Then
                  CALL CKV(Field(l:i-2),From-1+l,.FALSE.,'<E>')
                  CALL WRTMSG('<W> Check use of "?"',From-2+i,From-2+i)
               Else
                  CALL CKV(Field(l:i-1),From-1+l,.FALSE.,'<E>')
               EndIf
            END IF
   10       IF(i.LE.r) THEN
               IF(Field(i:i).EQ.')') i = i + 1
            END IF
         END IF
      END IF
      IF(ioninf) THEN
         IF(IVLSTR(Field(l:i-1)).NE.IONsta)                             &
     &      CALL WRTMSG('<E> Inconst. ionization',From-1+l,From-2+i)
      END IF
      l = i
!
!     LOOK FOR:
!     1) UNCERTAINTY.
!     2) ANOTHER OP.
!
      IF(l.GT.r) RETURN
      l = SPAN(Field(1:r),l,' ')
      IF(l.GT.r) RETURN
      IF(Field(l:l).EQ.'(') THEN
!        SKIP TO CHECK REFS.
      ELSE IF(TYPSTR(Field(l:l)).EQ.1.OR.Field(l:l).EQ.'+'.OR.Field(l:l)&
     &        .EQ.'C'.OR.Field(l:l).EQ.'S') THEN
!        UNCERTAINTY.
         i = BREAK(Field(1:r),l,' (')
         IF(Field(i-1:i-1).EQ.')') THEN
            CALL CKU(Field(l:i-2),From-1+l)
         ELSE
            CALL CKU(Field(l:i-1),From-1+l)
         END IF
         l = i
      ELSE
!        ANOTHER OP.
         IF(.NOT.islimt) THEN
            CALL WRTMSG('<E> JUNK AT END OF FIELD',From-1+l,From-1+r)
            RETURN
         END IF
         lold = l
         CALL CKCOP(Field,From,l,r,islimt,l2)
         IF(.NOT.islimt) THEN
            CALL WRTMSG('<E> JUNK AT END OF FIELD',From-1+lold,From-1+r)
            RETURN
         ELSE IF(l1.EQ.l2) THEN
            CALL WRTMSG('<E> JUNK AT END OF FIELD',From-1+lold,From-1+r)
            RETURN
         END IF
         IF(l.GT.r) RETURN
!        AND ANOTHER VALUE.
         l = SPAN(Field(1:r),l,' ')
         IF(units) THEN
!           FIND END OF VALUE AND UNITS.
            i = BREAK(Field(1:r),l,' ')
            i = SPAN(Field(1:r),i,' ')
            i = BREAK(Field(1:r),i,' (')
!           CHECK VALUE AND UNITS.
            IF(twarn) THEN
               CALL CKT(Field(l:i-1),From-1+l,From-2+i,'<W>')
            ELSE
               CALL CKT(Field(l:i-1),From-1+l,From-2+i,'<E>')
            END IF
         ELSE
!           FIND END OF VALUE.
            IF(Field(l:l).EQ.'(') l = l + 1
            i = BREAK(Field(1:r),l,' (')
            IF(Field(i-1:i-1).EQ.')') THEN
               CALL CKVSGN(Field(l:i-2),From-1+l,.FALSE.,'<E>')
            ELSE
               CALL CKVSGN(Field(l:i-1),From-1+l,.FALSE.,'<E>')
            END IF
         END IF
         l = i
         IF(l.GT.r) RETURN
         l = SPAN(Field(1:r),l,' ')
         IF(Field(l:l).EQ.'(') THEN
         ELSE IF(TYPSTR(Field(l:l)).EQ.1.OR.Field(l:l).EQ.'+'.OR.       &
     &           Field(l:l).EQ.'C'.OR.Field(l:l).EQ.'S') THEN
            i = BREAK(Field(1:r),l,' (')
            IF(Field(i-1:i-1).EQ.')') THEN
               CALL CKU(Field(l:i-2),From-1+l)
            ELSE
               CALL CKU(Field(l:i-1),From-1+l)
            END IF
            l = i
         ELSE
            CALL WRTMSG('<E> JUNK AT END OF FIELD',From-1+l,From-1+r)
            RETURN
         END IF
      END IF
!
!     Check for continuation of range (TWB. 930323)
!
      IF(l.GT.r) RETURN
      l = SPAN(Field(1:r),l,' ')
      IF(Field(l:l).NE.'(') THEN
         IF(islimt) THEN
            lold = l
            CALL CKCOP(Field,From,l,r,islimt,l2)
            IF(.NOT.islimt) THEN
               CALL WRTMSG('<E> JUNK AT END OF FIELD',From-1+lold,      &
     &                     From-1+r)
               RETURN
            ELSE IF(l1.EQ.l2) THEN
               CALL WRTMSG('<E> JUNK AT END OF FIELD',From-1+lold,      &
     &                     From-1+r)
               RETURN
            END IF
            l = SPAN(Field(1:r),l,' ')
            IF(units) THEN
               i = BREAK(Field(1:r),l,' ')
               i = SPAN(Field(1:r),i,' ')
               i = BREAK(Field(1:r),i,' (')
               IF(twarn) THEN
                  CALL CKT(Field(l:i-1),From-1+l,From-2+i,'<W>')
               ELSE
                  CALL CKT(Field(l:i-1),From-1+l,From-2+i,'<E>')
               END IF
            ELSE
               IF(Field(l:l).EQ.'(') l = l + 1
               i = BREAK(Field(1:r),l,' (')
               IF(Field(i-1:i-1).EQ.')') THEN
                  CALL CKVSGN(Field(l:i-2),From-1+l,.FALSE.,'<E>')
               ELSE
                  CALL CKVSGN(Field(l:i-1),From-1+l,.FALSE.,'<E>')
               END IF
            END IF
            l = i
            IF(l.GT.r) RETURN
            l = SPAN(Field(1:r),l,' ')
            IF(Field(l:l).EQ.'(') THEN
            ELSE IF(TYPSTR(Field(l:l)).EQ.1.OR.Field(l:l).EQ.'+'.OR.    &
     &              Field(l:l).EQ.'C'.OR.Field(l:l).EQ.'S') THEN
               i = BREAK(Field(1:r),l,' (')
               IF(Field(i-1:i-1).EQ.')') THEN
                  CALL CKU(Field(l:i-2),From-1+l)
               ELSE
                  CALL CKU(Field(l:i-1),From-1+l)
               END IF
               l = i
            ELSE
               CALL WRTMSG('<E> JUNK AT END OF FIELD',From-1+l,From-1+r)
               RETURN
            END IF
         ELSE
            CALL WRTMSG('<E> JUNK AT END OF FIELD',From-1+l,From-1+r)
            RETURN
         END IF
         l = l + 1
      END IF
!     CHECK FOR (REF... .
      IF(l.GT.r) RETURN
      l = SPAN(Field,l,' ')
      l = l + 1
      IF(l.GT.r) RETURN
      IF(Field(r:r).NE.')') THEN
         CALL WRTMSG('<E> NO ")" TO END LIST',From-1+r,From-1+r)
         IF(Field(r:r).EQ.',') THEN
            r = r - 1
         END IF
      ELSE
         r = r - 1
      END IF
      CALL CKREF(Field(l:r),From-1+l)
!
      RETURN
      END SUBROUTINE CKCFLD
!
!***********************************************************************
!
      SUBROUTINE CKCOIN(Field,From,To)
!
!     CHECKS THE VALIDITY OF COIN FIELD.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Field
      INTEGER(KIND=4) :: From, To
!
      IF(Field.EQ.' ') THEN
      ELSE IF(Field.EQ.'C') THEN
      ELSE IF(Field.NE.'?') THEN
         CALL WRTMSG('<E> INVALID COIN FIELD',From,To)
      END IF
!
      RETURN
      END SUBROUTINE CKCOIN
!
!***********************************************************************
!
      SUBROUTINE CKCOP(Field,From,L,R,Islimt,Limit)
!
!     CHECK CONTINUATION FIELD OP CODE.
!        VALID OP CODES ARE: * <, =, >, EQ, AP, LT, LE, GT, GE.
!      THE LAST SIX MUST BE FOLLOWED BY A BLANK.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Field, Limit
      LOGICAL(KIND=4) :: Islimt
      INTEGER(KIND=4) :: From, L, R
!
!     Functions used
!
      INTEGER(KIND=4), INTRINSIC :: INDEX
      INTEGER(KIND=4), EXTERNAL :: BREAK, SPAN
!
!     Local variables
!
      INTEGER(KIND=4) :: i, j, k
!
      Islimt = .FALSE.
      Limit = ' '
!
!     SKIP LEADING BLANKS (IF ANY).
!
      L = SPAN(Field(1:R),L,' ')
!
!     CHECK FOR VARIOUS FORMS.
!
      i = BREAK(Field(1:R),L,' ')
      j = i
      k = INDEX('<=>',Field(L:L))
      IF(k.GT.0) THEN
         IF(k.EQ.1.OR.k.EQ.4) Islimt = .TRUE.
         IF(k.EQ.1) Limit = 'L'
         IF(k.EQ.4) Limit = 'G'
         j = L + 1
      ELSE IF(Field(L:i-1).EQ.'EQ') THEN
      ELSE IF(Field(L:i-1).EQ.'AP') THEN
      ELSE IF(Field(L:i-1).EQ.'LT'.OR.Field(L:i-1).EQ.'LE') THEN
         Islimt = .TRUE.
         Limit = 'L'
      ELSE IF(Field(L:i-1).EQ.'GT'.OR.Field(L:i-1).EQ.'GE') THEN
         Islimt = .TRUE.
         Limit = 'G'
      ELSE
         CALL WRTMSG('<E> INVALID RELATION',From-1+L,From-2+i)
      END IF
      L = j
!
      RETURN
      END SUBROUTINE CKCOP
!
!***********************************************************************
!
      SUBROUTINE CKDSID(Dsid)
!
!     DETERMINES THE DATA SET TYPE AND DECAY TYPE AND
!             REPORTS ANY ERRORS FOUND ON THIS CARD.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Dsid
!
!     Functions used
!
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
      INTEGER(KIND=4), INTRINSIC :: INDEX, INT, LEN_TRIM
      INTEGER(KIND=4), EXTERNAL :: BREAK, INDEXF, IVLSTR, SPAN
      REAL(KIND=4), INTRINSIC :: ALOG10, REAL
      INTEGER(KIND=4), EXTERNAL :: LOWINDEX
!TIM2
!
!     Local variables
!
      CHARACTER(LEN=30) :: colstr
      INTEGER(KIND=4) :: a, colpos, from, i, j, k, l, m, n, offset,     &
     & parbeg, parend, r, to, z
!
      INTEGER(KIND=4), PARAMETER :: nonmax = 7
      CHARACTER(LEN=2), DIMENSION(nonmax) :: nonunc
      DATA nonunc/'LT', 'GT', 'LE', 'GE', 'AP', 'CA', 'SY'/
!
      INTEGER(KIND=4), PARAMETER :: ndtype = 12
      CHARACTER(LEN=2), DIMENSION(ndtype) :: dtype
      DATA dtype/'A ', ' P', ' N', 'B ', 'E ', '**', '**', 'DN', 'DP',  &
     &     'DA', 'DN', 'DP'/
      INTEGER(KIND=4) :: lowletin
!
      r = LEN_TRIM(Dsid)
      NPT12 = 0
      PARnucid = ' '
      DCOl89 = '**'
      offset = 0
      ISIon = .FALSE.
      TOTal = .FALSE.
      IONsta = 0
      SUBshe = ' '
      NTOtsf = 0
      DO i = 1, maxsf
         SFPar(i) = ' '
      END DO
      tara=-9999
      tarz=-9999
      tarpos=-9999
      inca=-9999
      incz=-9999
      outa=-9999
      outz=-9999
      coma=-9999
      comz=-9999
      write(*,*) 'In chkdsid, starting r is: ', r
      write(*,*) 'dsid here is: ', Dsid
      lowletin = LOWINDEX(Dsid)
      IF (lowletin.GT.0) Then
        CALL WRTMSG('<F> Lower case letters in DSID',1,r)
        CALL UPSTR(Dsid)
      ENDIF
!
!     CHECK FOR ILLEGAL PUNCTUATIONS
!     write(*,*) 'Checking punctions for: '
!     write(*,*) Dsid(1:r)
      CALL CKPUNC(Dsid(1:r),r)
!
      i = INDEX(Dsid,':')
      IF(i.GT.0) THEN
         r = i - 1
         colpos = i
         colstr = Dsid(i+1:)
      ELSE
         r = LEN_TRIM(Dsid)
         colpos = 0
      END IF
!
!     VALIDATE COMMENTS DATA SET ID'S.
!
      IF(DSType.EQ.0) THEN
         IF(r.NE.8) THEN
            CALL WRTMSG('<E> INVALID COMMENTS DATA SET ID',10,9+r)
         END IF
!
!        VALIDATE ADOPTED DATA SET ID'S.
!
      ELSE IF(DSType.EQ.1) THEN
         IF(Dsid(1:r).EQ.'ADOPTED LEVELS') THEN
         ELSE IF(Dsid(1:r).NE.'ADOPTED LEVELS, GAMMAS') THEN
            CALL WRTMSG('<E> INVALID ADOPTED DATA SET ID',10,9+r)
         END IF
!
!        VALIDATE DECAY DATA SET ID'S.
!
      ELSE IF(ABS(DSType).EQ.2) THEN
!        HANDLE LITERAL DECAY TYPE.
         IF(INDEX(Dsid(1:r),'MUONIC ATOM').GT.0) THEN
            DECtyp = 0
            IF(Dsid(1:r).NE.'MUONIC ATOM') THEN
               CALL WRTMSG('<E> INVALID DECAY DATA SET ID',10,9+r)
            END IF
!           HANDLE NORMAL DECAY TYPE.
         ELSE
            l = 1
!           FIRST COMES PARENT NUCID.
            i = SPAN(Dsid(1:r),l,' ')
            IF(i.GT.l) THEN
               CALL WRTMSG('<E> LEADING BLANKS INVALID',9+l,8+i)
               l = i
            END IF
            i = BREAK(Dsid(1:r),l,' ')
            IF(i.GT.r) THEN
!
!              JUNK AT END OF FIELD.
!
               CALL WRTMSG('<E> JUNK AT END OF FIELD: here',10,39)
               GO TO 100
            END IF
            CALL NUCID(Dsid(l:i-1),a,z)
            IF(a.EQ.0.OR.z.LT.0) THEN
               CALL WRTMSG('<F> INVALID PARENT ID',9+l,8+i)
            ELSE
               IF(IZLmsg.NE.' ') THEN
                  CALL REPSTR(IZLmsg,'Obsolete formalism','Obsol. form')
                  CALL ADDSTR(IZLmsg,1,'<W> ')
                  CALL WRTMSG(TRIM(IZLmsg),10+l+INT(ALOG10(REAL(a))),   &
     &                        8+i)
               END IF
               IF(LEN_TRIM(Dsid(l:i-1)).GT.5.AND.INDEX(Dsid(l:i-1),',') &
     &            .GT.0) THEN
                  k = l
                  DO WHILE (.TRUE.)
                     m = INDEXF(Dsid(l:i-1),k,',')
                     IF(m.GT.0) THEN
                        NTOtsf = NTOtsf + 1
                        SFPar(NTOtsf) = Dsid(k:m-1)
                        CALL LBSUP(SFPar(NTOtsf))
                        CALL NUCID(SFPar(NTOtsf),a,z)
                        IF(a.EQ.0.OR.z.EQ.-1) THEN
                           CALL WRTMSG('<E> Invalid Decay Parent',9+k,  &
     &                                 8+m)
                        ELSE IF(IZLmsg.NE.' ') THEN
                           CALL REPSTR(IZLmsg,'Obsolete formalism',     &
     &                                 'Obsol. form')
                           CALL ADDSTR(IZLmsg,1,'<W> ')
                           CALL WRTMSG(TRIM(IZLmsg),9+k,8+m)
                        END IF
                        k = m + 1
                        CYCLE
                     END IF
                     NTOtsf = NTOtsf + 1
                     SFPar(NTOtsf) = Dsid(k:i-1)
                     CALL LBSUP(SFPar(NTOtsf))
                     IF(a.EQ.0.OR.z.EQ.-1) THEN
                        CALL WRTMSG('<E> Invalid Decay Parent',9+k,8+m)
                     ELSE IF(IZLmsg.NE.' ') THEN
                        CALL REPSTR(IZLmsg,'Obsolete formalism',        &
     &                              'Obsol. form')
                        CALL ADDSTR(IZLmsg,1,'<W> ')
                        CALL WRTMSG(TRIM(IZLmsg),9+k,8+m)
                     END IF
                     EXIT
                  END DO
               END IF
               parbeg = l
               parend = i - 1
            END IF
            DO WHILE (.TRUE.)
!              Check for stuff trailing the parent
               IF((Dsid(l:l).GE.'0'.AND.Dsid(l:l).LE.'9').OR.           &
     &            (Dsid(l:l).GE.'A'.AND.Dsid(l:l).LE.'Z')) THEN
                  l = l + 1
                  CYCLE
               ELSE IF(NTOtsf.GT.0.AND.Dsid(l:l).EQ.',') THEN
                  l = l + 1
                  CYCLE
               END IF
               IF(l.NE.i) THEN
!                 Only Ionized Atom information allowed
                  IF(Dsid(l:l).EQ.'['.AND.Dsid(i-1:i-1).EQ.']') THEN
                     ISIon = .TRUE.
                     IONsta = IVLSTR(Dsid(l+1:i-1))
                     IF(IONsta.GT.z) THEN
                        CALL WRTMSG('<E> Ionization>Z',9+l,8+i)
                     ELSE IF(IONsta.EQ.z) THEN
                        TOTal = .TRUE.
                     END IF
                  ELSE
                     CALL WRTMSG('<E> JUNK AT END OF FIELD',9+l,8+i)
                  END IF
                  parend = l - 1
               END IF
               IF(NTOtsf.EQ.0) THEN
                  PARnucid = Dsid(parbeg:parend)
                  CALL LBSUP(PARnucid)
                  CALL NUCID(PARnucid,a,z)
                  IF(a.EQ.0.OR.z.EQ.-1) THEN
                     CALL WRTMSG('<E> Invalid Decay Parent',9+parbeg,   &
     &                           9+parend)
                  ELSE IF(IZLmsg.NE.' ') THEN
                     CALL REPSTR(IZLmsg,'Obsolete formalism',           &
     &                           'Obsol. form')
                     CALL ADDSTR(IZLmsg,1,'<W> ')
                     CALL WRTMSG(TRIM(IZLmsg),9+parbeg,8+parend)
                  END IF
               END IF
               l = i
!              THEN DECAY TYPE (CHECK AND SET DECTYP).
               i = SPAN(Dsid(1:r),l,' ')
               IF(i.GT.l+1) THEN
                  CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID',9+l,8+i)
               END IF
               l = i
               i = BREAK(Dsid(1:r),l,' ')
               IF(i.GT.r) THEN
!*****            The following statement is a duplicate copy made by
!                 SPAG
                  CALL WRTMSG('<E> JUNK AT END OF FIELD',10,39)
                  EXIT
               END IF
               j = i - l
               i = i - 1
!              IF (J .EQ. 3) THEN
!              IF (DSID(L:L+1) .EQ. 'B-'
!              +         .OR. DSID(L:L+1) .EQ. 'B+'
!              +         .OR. DSID(L:L+1) .EQ. 'EC') L = L + 2
!              ENDIF
               DECtyp = RDECTYP(Dsid(l:i))
               IF(DECtyp.EQ.0) THEN
                  CALL WRTMSG('<E> INVALID DECAY TYPE',9+l,9+i)
               ELSE IF(DECtyp.LE.12) THEN
                  DCOl89 = dtype(DECtyp)
                  IF(Dsid(l:l+1).EQ.'EC'.AND.TOTal)                     &
     &               CALL WRTMSG('<E> No EC allowed - total ion.',9+l,  &
     &               10+l)
               ELSE IF(DECtyp.EQ.13) THEN
!                 Only "A", "P", and "N" currently defined
                  IF(Dsid(i:i).EQ.'A') DCOl89 = 'DA'
                  IF(Dsid(i:i).EQ.'P') DCOl89 = 'DP'
                  IF(Dsid(i:i).EQ.'N') DCOl89 = 'DN'
               END IF
               IF(DECtyp.NE.7.AND.NTOtsf.GT.0) THEN
                  parend = parbeg - 1
                  DO k = 1, NTOtsf
                     parend = parend + LEN_TRIM(SFPar(k))
                  END DO
                  parend = parend + NTOtsf - 1
                  CALL WRTMSG('<F> Multiple parents only for SF',       &
     &                        9+parbeg,9+parend)
               END IF
               l = i + 1
!              'DECAY' MUST BE NEXT, SKIP OVER IT.
               i = SPAN(Dsid(1:r),l,' ')
               IF(i.GT.l+1) THEN
                  CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID',9+l,8+i)
               END IF
               l = i
               i = BREAK(Dsid(1:r),l,' ')
               IF(Dsid(l:i-1).NE.'DECAY') THEN
                  CALL WRTMSG('<E> INVALID "DECAY"',9+l,8+i)
               END IF
               l = i
!              LOOK FOR OPTIONAL HALF-LIFE AND CHECK.
               IF(l.LT.r) THEN
                  i = SPAN(Dsid(1:r),l,' ')
                  IF(i.GT.l+1) THEN
                     CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID',9+l, &
     &                           8+i)
                  END IF
                  l = i
                  IF(Dsid(l:l).NE.'('.OR.Dsid(r:r).NE.')') THEN
                     CALL WRTMSG('<E> INVALID HALF-LIFE',9+l,9+r)
                  ELSE
                     l = l + 1
                     r = r - 1
                     i = INDEX(Dsid(l:r),'+')
!                    LOOK FOR SEPARATOR.
                     IF(i.NE.0) THEN
                        j = l + i - 2
                        IF(Dsid(j:j).EQ.'E') THEN
!                          '+' IS PART OF NUM, NOT SEPARATOR.
                           j = j + 2
                           i = INDEX(Dsid(j:r),'+')
!                          LOOK FOR SEPARATOR ON THE RIGHT.
                           IF(i.NE.0) i = j - l + i
                        END IF
                     END IF
                     IF(i.EQ.0) THEN
                        IF(Dsid(l:l).EQ.' ')                            &
     &                     CALL WRTMSG('<E> LEADING BLANKS INVALID',9+l,&
     &                     9+r)
                        IF(Dsid(r:r).EQ.' ')                            &
     &                     CALL WRTMSG('<E> TRAILING BLANKS INVALID',   &
     &                     9+l,9+r)
                        IF(DECtyp.NE.6) THEN
                           NPT12 = 1
                           DPT12(NPT12) = ' '
                        END IF
                        DO m = 1, nonmax
                           IF(Dsid(l:l+1).EQ.nonunc(m)) THEN
                              l = l + 3
                              IF(DECtyp.NE.6) DPT12(NPT12) = nonunc(m)
                              EXIT
                           END IF
                        END DO
                        CALL CKT(Dsid(l:r),9+l,9+r,'<E>')
                        IF(DECtyp.NE.6) THEN
                           PT12(NPT12) = Dsid(l:r)
                           MATcht12(NPT12) = .FALSE.
                           CALL LBSUP(PT12(NPT12))
                        END IF
                     ELSE
                        i = i + l - 1
                        IF(Dsid(l:l).EQ.' ')                            &
     &                     CALL WRTMSG('<E> LEADING BLANKS INVALID',9+l,&
     &                     8+i)
                        IF(Dsid(i-1:i-1).EQ.' ')                        &
     &                     CALL WRTMSG('<E> TRAILING BLANKS INVALID',   &
     &                     9+l,8+i)
                        CALL CKT(Dsid(l:i-1),9+l,8+i,'<E>')
                        IF(DECtyp.NE.6) THEN
                           NPT12 = 1
                           DPT12(NPT12) = ' '
                        END IF
                        DO m = 1, nonmax
                           IF(Dsid(l:l+1).EQ.nonunc(m)) THEN
                              l = l + 3
                              IF(DECtyp.NE.6) DPT12(NPT12) = nonunc(m)
                              EXIT
                           END IF
                        END DO
                        IF(DECtyp.NE.6) THEN
                           PT12(NPT12) = Dsid(l:i-1)
                           MATcht12(NPT12) = .FALSE.
                           CALL LBSUP(PT12)
                        END IF
                        IF(Dsid(i+1:i+1).EQ.' ')                        &
     &                     CALL WRTMSG('<E> LEADING BLANKS INVALID',    &
     &                     10+i,9+r)
                        IF(Dsid(r:r).EQ.' ')                            &
     &                     CALL WRTMSG('<E> TRAILING BLANKS INVALID',   &
     &                     10+i,9+r)
                        IF(DECtyp.NE.6) THEN
                           NPT12 = 2
                           DPT12(NPT12) = ' '
                        END IF
                        DO m = 1, nonmax
                           IF(Dsid(i+1:i+2).EQ.nonunc(m)) THEN
                              i = i + 3
                              IF(DECtyp.NE.6) DPT12(NPT12) = nonunc(m)
                              EXIT
                           END IF
                        END DO
                        CALL CKT(Dsid(i+1:r),10+i,9+r,'<E>')
                        IF(DECtyp.NE.6) THEN
                           PT12(NPT12) = Dsid(i+1:r)
                           MATcht12(NPT12) = .FALSE.
                           CALL LBSUP(PT12)
                        END IF
                     END IF
                  END IF
               END IF
               EXIT
            END DO
         END IF
!
!        VALIDATE REACTION DATA SET ID'S.
!
      ELSE IF(ABS(DSType).EQ.3) THEN
!        MAKE SURE THERE IS SOMETHING THERE.
         write(*,*) 'Checking reaction now.. and r is: ', r
         IF(r.EQ.0) THEN
            CALL WRTMSG('<F> EMPTY DSID',10,39)
            GO TO 100
         END IF
!        HANDLE LITERAL REACTION TYPES.
         IF(Dsid(1:r).EQ.'INELASTIC SCATTERING') THEN
            Call Wrtmsg('<W> Obsolete formalism',10,10+r) 
            GO TO 100
         END IF
         IF(Dsid(1:r).EQ.'(HI,XNG)') THEN
            GO TO 100
         END IF
         IF(Dsid(1:r).EQ.'HIGH-SPIN LEVELS, GAMMAS') THEN
            Call Wrtmsg('<W> Obsolete formalism',10,10+r) 
            GO TO 100
         END IF
         IF(Dsid(1:18).EQ.'COULOMB EXCITATION') THEN
!           COULOMB EXCITATION MAY BE FOLLOWED BY '(<ANY>*,<ANY>*)'
            l = 19
            i = SPAN(Dsid(1:r),l,' ')
            IF(i.GT.l+1) THEN
               CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID',9+l,8+i)
            END IF
            l = i
            IF(l.LT.r) THEN
               tara=resa
	       tarz=resz
               CALL CKREA(Dsid(l:r),9+l,9+r)
            END IF
            GO TO 100
         END IF
!        HANDLE NORMAL REACTION TYPE.
!        IF QUALIFIER PRESENT, SET R TO IGNORE IT HEREAFTER.
         IF(Dsid(r-3:r).EQ.' RES'.OR.Dsid(r-3:r).EQ.' IAR'.OR.          &
     &      Dsid(r-3:r).EQ.' IAS') THEN
            r = r - 4
            l = LEN_TRIM(Dsid(1:r))
            IF(l.LT.r) CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID',   &
     &                            9+l+1,9+r)
            r = l
         END IF
         l = 1
!        CHECK TARGET.
         i = SPAN(Dsid(1:r),l,' ')
         IF(i.GT.l) THEN
            CALL WRTMSG('<E> LEADING BLANKS INVALID',9+l,8+i)
            l = i
         END IF
    5    i = BREAK(Dsid(1:r),l,'(')
         IF(i.GT.r) THEN
!*****      The following statement is a duplicate copy made by SPAG
            CALL WRTMSG('<E> JUNK AT END OF FIELD',10,39)
            GO TO 100
         END IF
         IF(i.LE.l) THEN
            CALL WRTMSG('<E> INVALID REACTION TARGET',9+l,9+l)
         ELSE
            DO k = i - 1, l, -1
               IF(Dsid(k:k).EQ.' ') THEN
                  CALL WRTMSG('<F> JUNK AT BEGINNING OF FIELD',9+l,9+k)
                  EXIT
               END IF
            END DO
            write(*,*) 'Calling the nucid on: ', Dsid(l:i-1)
            CALL NUCID(Dsid(l:i-1),a,z)
            If(a .GT. 0)tara=a
	    tarz=z
            tarpos=9+l
            IF(z.LT.0) THEN
               If((9+l).GT.39 .AND. totcon .GT. 0)Then
                  If(l .GE. conpos(totcon))Then
                     CALL WRTMSG('<E> INVALID REACTION TARGET',         &
     &                 9+l-conpos(totcon),8+i-conpos(totcon))
                  Else
	             Do n=totcon-1,1,-1
		        If(l.GE.conpos(n) .AND. l.LT.conpos(n-1))Then
                           CALL WRTMSG('<E> INVALID REACTION TARGET',   &
     &                       9+l-conpos(n),8+i-conpos(n))
			EndIf
	             EndDo
		  EndIf
	       Else
                  CALL WRTMSG('<E> INVALID REACTION TARGET',9+l,8+i)
	       EndIf
               tara=-9999
	       tarz=-9999
               tarpos=-9999
            ELSE IF(IZLmsg.NE.' ') THEN
               CALL REPSTR(IZLmsg,'Obsolete formalism','Obsol. form')
               CALL ADDSTR(IZLmsg,1,'<W> ')
               CALL WRTMSG(TRIM(IZLmsg),12+l,8+i)
            END IF
            IF(Dsid(i-1:i-1).EQ.' ')                                    &
     &         CALL WRTMSG('<E> INVALID BLANK',9+i-1,9+i-1)
         END IF
         l = i
!        CHECK REACTION.
   10    i = BREAK(Dsid(1:r),l,')')
         IF(i.GT.r) THEN
!*****      The following statement is a duplicate copy made by SPAG
            CALL WRTMSG('<E> JUNK AT END OF FIELD',10,39)
            GO TO 100
         END IF
!        Calculate correct position of flag for DSID continuations
!        (TWB. 930323)
         from = 9 + l
         to = 9 + i
         IF(TOTcon.GT.1) THEN
            IF(l.GT.CONpos(1)) THEN
               DO j = 1, TOTcon - 1
                  IF(l.GT.CONpos(j).AND.l.LE.CONpos(j+1)) THEN
                     offset = CONpos(j)
                     from = from - offset
                     to = to - offset
                     GO TO 15
                  END IF
               END DO
               offset = CONpos(TOTcon)
               from = from - offset
               to = to - offset
            END IF
         ELSE IF(TOTcon.EQ.1) THEN
            IF(l.GE.CONpos(1)) THEN
               offset = CONpos(1)
               from = from - offset
               to = to - offset
            END IF
         END IF
   15    Continue
         CALL CKREA(Dsid(l:i),from,to)
         IF(colpos.GT.0) CALL CKCOL(colpos-offset,colstr)
         l = i + 1
         from = 9 + l - offset
         IF(l.GE.r) GO TO 100
!        CHECK FOR MORE REACTIONS.
         IF(Dsid(l:l).EQ.',') THEN
            l = l + 1
            from = 9 + l - offset
            i = SPAN(Dsid(1:r),l,' ')
            to = 8 + i - offset
            IF(i.GT.l+1) CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID', &
     &                              from,to)
            l = i
            from = 9 + l - offset
!           CHECK FOR TARGET (GOTO 20) OR NOT (GOTO 30).
            IF(Dsid(l:l).EQ.'(') GO TO 10
            GO TO 5
         END IF
         IF(l.GE.r) GO TO 100
!        CHECK FOR ENERGY FIELD.
         i = SPAN(Dsid(1:r),l,' ')
         to = 8 + i - offset
         IF(i.GT.l+1)CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID',from,&
     &                           to)
         l = i
         from = 9 + l - offset
         IF(Dsid(l:l+1).EQ.'E='.OR.Dsid(l:l+4).EQ.'E AP ') THEN
            IF(Dsid(l:l+1).EQ.'E=') THEN
               l = l + 2
            ELSE
               l = l + 5
            END IF
            from = 9 + l - offset
            i = SPAN(Dsid(1:r),l,' ')
            to = 8 + i - offset
            IF(i.GT.l+1) CALL WRTMSG('<E> EMBEDDED BLANK INVALID',from, &
     &                              to)
            l = i
            from = 9 + l - offset
            IF(Dsid(l:r).EQ.'TH') GO TO 100
            IF(Dsid(l:r).EQ.'RES') GO TO 100
            IF(Dsid(l:r).EQ.'THERMAL') GO TO 100
            IF(Dsid(l:r).EQ.'RESONANCE') GO TO 100
!           CHECK ENERGY VALUE.
            i = l
            IF(Dsid(l:l+2).EQ.'TH,') THEN
               l = l + 3
               from = 9 + l - offset
               i = SPAN(Dsid(1:r),l,' ')
               to = 8 + i - offset
               IF(i.GT.l+1)CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID'&
     &                                 ,from,to)
            ELSE IF(Dsid(l:l+3).EQ.'RES,') THEN
               l = l + 4
               from = 9 + l - offset
               i = SPAN(Dsid(1:r),l,' ')
               to = 8 + i - offset
               IF(i.GT.l+1)CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID'&
     &                                 ,from,to)
            END IF
            DO WHILE (.TRUE.)
               l = i
               from = 9 + l - offset
               i = BREAK(Dsid(1:r),l,' ,-')
               to = 8 + i - offset
               IF(i.GT.r) THEN
!*****            The following statement is a duplicate copy made by
!                 SPAG
                  CALL WRTMSG('<E> JUNK AT END OF FIELD: first',10,39)
                  EXIT
               END IF
               CALL CKV(Dsid(l:i-1),from,.TRUE.,'<E>')
               IF(Dsid(i:i).EQ.',') THEN
                  l = i + 1
                  from = 9 + l - offset
                  i = SPAN(Dsid(1:r),l,' ')
                  to = 8 + i - offset
                  IF(i.GT.l+1)                                          &
     &               CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID',from,&
     &               to)
                  CYCLE
               ELSE IF(Dsid(i:i).EQ.'-') THEN
                  l = i + 1
                  from = 9 + l - offset
                  i = SPAN(Dsid(1:r),l,' ')
                  to = 8 + i - offset
                  IF(i.GT.l) CALL WRTMSG('<E> LEADING BLANKS INVALID',  &
     &                                  from,to)
                  CYCLE
               END IF
               l = i
               from = 9 + l - offset
               i = SPAN(Dsid(1:r),l,' ')
               to = 8 + i - offset
               IF(i.GT.l+1)CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID'&
     &                                 ,from,to)
               l = i
               from = 9 + l - offset
               i = BREAK(Dsid(1:r),l,' ')
               If(dsid(i-1:i-1) .EQ. ',')Then
	          i=i-1
	       EndIf
               to = 8 + i - offset
               IF(Dsid(l:i-1).EQ.'EV') THEN
               ELSE IF(Dsid(l:i-1).EQ.'KEV') THEN
               ELSE IF(Dsid(l:i-1).EQ.'MEV') THEN
               ELSE IF(Dsid(l:i-1).NE.'GEV') THEN
                  CALL WRTMSG('<E> INVALID UNITS',from,to)
               END IF
               from = 9 + i - offset
               to = 9 + r - offset
!TWB-20060523               IF(i.LE.r)Then
!TWB-20060523	          CALL WRTMSG('<E> JUNK AT END OF FIELD',from,to)
!TWB-20060523	       EndIf
               EXIT
            END DO
         ELSE IF(Dsid(l:r).NE.' '.AND.Dsid(l:r).NE.',') THEN
            from = 9 + i - offset
            to = 9 + r - offset
            CALL WRTMSG('<E> JUNK AT END OF FIELD: second',from,to)
         END IF
!
!        VALIDATE REFERENCE DATA SET ID'S.
!
      ELSE IF(DSType.EQ.4) THEN
         IF(r.NE.10)CALL WRTMSG('<F> INVALID REFERENCES DATA SET ID',10,&
     &                          9+r)
      END IF
!
  100 RETURN
      END SUBROUTINE CKDSID
!
!***********************************************************************
!
      SUBROUTINE CKE(Field,From,To,Type)
!
!     CHECKS THE VALIDITY OF ENERGY FIELD.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Field, Type
      INTEGER(KIND=4) :: From, To
!
!     Functions used
!
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
      INTEGER(KIND=4), INTRINSIC :: LEN, LEN_TRIM
      INTEGER(KIND=4), EXTERNAL :: BREAK, INDEXF, IVLSTR, RLSCN, SPAN,  &
     &                             TYPSTR
!
!     Local variables
!
      CHARACTER(LEN=38) :: message
      INTEGER(KIND=4) :: i, l, m, r
      REAL(KIND=4) :: v
!
!     CHECK FOR BLANK FIELD.
!
      r = LEN_TRIM(Field)
      IF(r.EQ.0) RETURN
!
!     SKIP LEADING BLANKS.
!
      l = 1
      l = SPAN(Field(1:r),l,' ')
!
!     IGNORE ( AND ) IF PRESENT.
!
      IF(Field(l:l).EQ.'(') THEN
         l = l + 1
         IF(Field(r:r).EQ.')') THEN
            r = r - 1
         ELSE
            message = ' MISSING ")"'
            CALL ADDSTR(message,1,TRIM(Type))
            CALL WRTMSG(message,From,To)
         END IF
!        Parentheses allowed but sometimes used wrong (TWB. 930323)
         CALL WRTMSG('<W> CHECK USE OF "(...)" IN E FIELD',From,To)
      END IF
!
!     IGNORE SN+ OR SN-.
!
      IF(LEN(Field)-l.GE.2) THEN
!        Also allow for bound resonances (TWB. 930323)
         IF(Field(l:l+2).EQ.'SN+'.OR.Field(l:l+2).EQ.'SP+'.OR.          &
     &      Field(l:l+2).EQ.'SN-'.OR.Field(l:l+2).EQ.'SP-') l = l + 3
      END IF
!
!     CHECK FOR A = 'X', 'Y', 'Z', 'U', 'V', 'W'.
!     FORMS:  A, A+NUM, NUM+A
!
      IF(RTYpe.EQ.4.OR.RTYpe.EQ.8) THEN
         i = BREAK(Field(1:r),l,'XYZUVWSTABCDEFGHIJKLMNOPQR')
         IF(i.LT.r.AND.i.GT.l) THEN
            IF(Field(i:i).EQ.'E'.AND.                                   &
     &         ((Field(i+1:i+1).GE.'0'.AND.Field(i+1:i+1).LE.'9').OR.   &
     &         (Field(i+1:i+1).EQ.'+'.OR.Field(i+1:i+1).EQ.'-')).AND.   &
     &         (TYPSTR(Field(l:i-1)).EQ.1.OR.TYPSTR(Field(l:i-1)).EQ.-2)&
     &         ) i = r + 1
         END IF
      ELSE
         i = BREAK(Field(1:r),l,'XYZUVW')
      END IF
      IF(i.GT.r) THEN
         m = INDEXF(Field(1:r),l,'E')
         IF(m.GT.0.AND.m.LE.r-2) THEN
            m = m + 1
            IF(Field(m:m).EQ.'+') m = m + 1
            IF(Field(m:m).EQ.'-') THEN
               m = m + 1
               message = 'EXPONENT TOO SMALL'
            ELSE
               message = 'EXPONENT TOO LARGE'
            END IF
            IF(IVLSTR(Field(m:r)).GE.10) THEN
               IF(IVLSTR(Field(m:r)).LE.30) THEN
                  CALL ADDSTR(message,1,'<W> ')
                  CALL WRTMSG(TRIM(message),From-1+m,From-1+r)
               ELSE
                  CALL ADDSTR(message,1,TRIM(Type))
                  CALL WRTMSG(TRIM(message),From-1+m,From-1+r)
                  RETURN
               END IF
            END IF
         END IF
         i = RLSCN(Field(1:r),l,v)
         IF(i.LE.r) THEN
            message = ' JUNK AT END OF FIELD'
            CALL ADDSTR(message,1,TRIM(Type))
            CALL WRTMSG(message,From-1+i,From-1+r)
         END IF
         If(v .LT. 0)Then
            message=' Negative energy found'
	    If(rtype .EQ. 4)Then
               Call Addstr(message,1,'<W>')
	    Else
	       CALL ADDSTR(message,1,TRIM(Type))
	    EndIf
	    CALL WRTMSG(message,From,From+r)
	 EndIf
      ELSE IF(l.EQ.r) THEN
!        FIRST FORM.
      ELSE IF(i.EQ.l) THEN
!        SECOND FORM.
         IF(Field(l+1:l+1).EQ.'+') THEN
            CALL CKV(Field(l+2:r),From-1+l+2,.TRUE.,TRIM(Type))
         ELSE
            message = ' MISSING +'
            CALL ADDSTR(message,1,TRIM(Type))
            CALL WRTMSG(message,From-1+l+1,From-1+l+1)
         END IF
      ELSE IF(i.EQ.r) THEN
!        THIRD FORM.
         IF(Field(r-1:r-1).EQ.'+') THEN
            CALL CKV(Field(l:r-2),From-1+l,.TRUE.,TRIM(Type))
         ELSE
            message = ' MISSING +'
            CALL ADDSTR(message,1,TRIM(Type))
            CALL WRTMSG(message,From-1+r-1,From-1+r-1)
         END IF
      ELSE
         message = ' INVALID E FIELD'
         CALL ADDSTR(message,1,TRIM(Type))
         CALL WRTMSG(message,From,To)
!
!        CHECK FOR NUM.
!
      END IF
!
      RETURN
      END SUBROUTINE CKE
!
!***********************************************************************
!
      SUBROUTINE CKINTN(Field,From,To)
!
!     CHECK INTENSITY FOR THE PN RECORD.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Field
      INTEGER(KIND=4) :: From, To
!
!     Functions used
!
      INTEGER(KIND=4), INTRINSIC :: ICHAR
!
!     BLANK OR 1-7 ACCEPTED.
!
      IF(Field.NE.' ') THEN
         IF(ICHAR(Field(1:1)).LT.ICHAR('1').OR.ICHAR(Field(1:1))        &
     &      .GT.ICHAR('7'))CALL WRTMSG('<E> FIELD MUST BE BLANK OR 1-7',&
     &                                 From,To)
      END IF
!
      RETURN
      END SUBROUTINE CKINTN
!
!***********************************************************************
!
      SUBROUTINE CKJ(Field,From,To,Odd,Type)
!
!     CHECKS THE VALIDITY OF SPIN-PARITY FIELD.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Field, Type
      LOGICAL(KIND=4) :: Odd
      INTEGER(KIND=4) :: From, To
!
!     Functions used
!
      CHARACTER(LEN=1), INTRINSIC :: CHAR
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
      INTEGER(KIND=4), INTRINSIC :: INDEX, LEN, LEN_TRIM
      INTEGER(KIND=4), EXTERNAL :: BREAK, INDEXF, SPAN, TYPSTR
!
!     Local variables
!
      CHARACTER(LEN=38) :: message
      CHARACTER(LEN=68) :: work, work2
      INTEGER(KIND=4) :: add, i, im, ip, j, k, l, n, r
      CHARACTER(LEN=4), Dimension(3) :: str1
      CHARACTER(LEN=4), Dimension(6) :: str2
      DATA str1/' OR',' AND',' TO'/
      DATA str2/'AP ','GE ','GT ','LE ','LT ','NOT '/
!
!     CHECK FOR BLANK FIELD.
!
      r = LEN_TRIM(Field)
      IF(r.EQ.0) RETURN
!
!     CHECK FOR '[UN]NATURAL'.
!
      l = SPAN(Field(1:r),1,' ')
      IF(Field(l:r).EQ.'NATURAL') RETURN
      IF(Field(l:r).EQ.'UNNATURAL') RETURN
!
      work = Field
!
!     Check for embedded blanks
!
      i=LEN_TRIM(work)
      Call Lbsup(work)
      j=LEN_TRIM(work)
      add=i-j
      i=INDEX(work,' ')
      If(i .LT. LEN_TRIM(work))Then
         Do j=1,3
            If(i .EQ. INDEX(work,str1(j)))GoTo 100
         EndDo
         Do j=1,6
            If(work(1:i) .EQ. str2(j))Then
               If(work(i+1:i+1) .EQ. '(')Then
                  message='<W> Check use of '//TRIM(str2(j))
                  message=TRIM(message)//' before open paren'
                  Call Wrtmsg(message,from,from+add+i-1)
                  work=work(i+1:)
                  GoTo 100
               Else
                  GoTo 100
               EndIf
            ElseIf(work(1:1) .EQ. '(')Then
               If(work(2:i) .EQ. str2(j))GoTo 100
            EndIf
         EndDo
         message=' EMBEDDED BLANK INVALID'
         CALL ADDSTR(message,1,TRIM(Type))
         CALL WRTMSG(message,from+add+i-1,from+add+i-1)
         Return
100      Continue
      EndIf
!
!     SQUEEZE OUT BLANKS.
!
      CALL SQZSTR(work,' ')
      r = LEN_TRIM(work)
!
!     CHECK FOR BALANCED PARENTHESES, AND DELETE ALL PARENTHESES.
!
      n = 0
      DO i = 1, r
         IF(work(i:i).EQ.'(') n = n + 1
         IF(work(i:i).EQ.')') n = n - 1
      END DO
      IF(n.NE.0) THEN
         message = ' UNBALANCED PARENTHESES'
         CALL ADDSTR(message,1,TRIM(Type))
         CALL WRTMSG(message,From,To)
         RETURN
      END IF
!     Check for garbage before the first or after the last parentheses
!     before deleting
      i = INDEX(work,'(')
      IF(i.GT.1) THEN
         message = ' JUNK BEFORE OPEN PAREN'
         im = i - 1
         ip = i + 1
!        Try some simplification
         IF(i.NE.2) THEN
            work2 = work(1:im)
            CALL REPSTR(work2,'OR',',')
            CALL REPSTR(work2,'AND',',')
            CALL REPSTR(work2,'TO',',')
            CALL REPSTR(work2,'&',',')
            CALL REPSTR(work2,':',',')
            CALL ADDSTR(work2,LEN_TRIM(work2)+1,work(i:LEN_TRIM(work)))
            work = work2
            i = INDEX(work,'(')
            work2=work(1:i-1)
            im = i - 1
            ip = i + 1
            If(Typstr(work2).EQ.1 .AND.                                 &
     &        .NOT.(work(ip:ip).EQ.'-' .OR. work(ip:ip).EQ.'+'))Then
               CALL ADDSTR(message,1,TRIM(Type))
               CALL WRTMSG(message,From,from+add+im-1)
               RETURN
            EndIf
         ELSE IF(.NOT.(work(1:1).EQ.'<'.OR.work(1:1).EQ.'>'.OR.         &
     &           (work(1:1).GE.'0'.AND.work(1:1).LE.'9'))) THEN
            CALL ADDSTR(message,1,TRIM(Type))
            CALL WRTMSG(message,From,from+add+im-1)
           RETURN
         END IF
         IF(work(ip:ip).EQ.'+'.OR.work(ip:ip).EQ.'-') THEN
            IF(.NOT.((work(im:im).GE.'0'.AND.work(im:im).LE.'9').OR.    &
     &         work(im:im).EQ.')')) THEN
               CALL ADDSTR(message,1,TRIM(Type))
               CALL WRTMSG(message,From,To)
               RETURN
            END IF
         ElseIf(Typstr(work(ip:ip)).EQ.1 .AND. Typstr(work(im:im)).EQ.1)&
     &     Then
            CALL ADDSTR(message,1,TRIM(Type))
            CALL WRTMSG(message,From,from+add+im-1)
            RETURN
         ELSE IF((work(im:im).EQ.'+'.OR.work(im:im).EQ.'-').AND.        &
     &           .NOT.(work(1:1).EQ.'J'.OR.work(ip:ip).EQ.'&')) THEN
            CALL ADDSTR(message,1,TRIM(Type))
            CALL WRTMSG(message,From,To)
            RETURN
         ELSE IF(work(ip:ip).EQ.'&'.AND.                                &
     &           .NOT.(work(im:im).EQ.'-'.OR.work(im:im).EQ.'+'.OR.     &
     &           (work(im:im).GE.'0'.AND.work(im:im).LE.'9'))) THEN
            CALL ADDSTR(message,1,TRIM(Type))
            CALL WRTMSG(message,From,To)
            RETURN
         ELSE IF(work(ip:ip).EQ.'&') THEN
         ELSE IF(work(im:im).EQ.'&'.AND.                                &
     &           .NOT.(work(i-2:i-2).EQ.'-'.OR.work(i-2:i-2).EQ.'+'.OR. &
     &           (work(i-2:i-2).GE.'0'.AND.work(i-2:i-2).LE.'9'))) THEN
            CALL ADDSTR(message,1,TRIM(Type))
            CALL WRTMSG(message,From,To)
            RETURN
         ELSE IF(work(im:im).EQ.'&') THEN
         ELSE IF(work(1:1).EQ.'J'.OR.                                   &
     &           (work(im:im).GE.'0'.AND.work(im:im).LE.'9').OR.        &
     &           work(im:im).EQ.'<'.OR.work(im:im).EQ.'>'.OR.           &
     &           work(i-2:im).EQ.'AP'.OR.work(i-2:i-2).EQ.'G'.OR.       &
     &           work(i-2:i-2).EQ.'L') THEN
         ELSE IF(work(im:im).NE.',') THEN
            CALL ADDSTR(message,1,TRIM(Type))
            CALL WRTMSG(message,From,To)
            RETURN
         END IF
      END IF
      i = INDEX(work,')')
      IF(i.GT.0.AND.i.LT.LEN_TRIM(work)) THEN
         i = LEN_TRIM(work)
         DO WHILE (work(i:i).NE.')')
            i = i - 1
         END DO
         IF(work(i+1:i+1).NE.',' .AND. work(i+1:i+1).NE.'+' .AND.       &
     &      work(i+1:i+1).NE.'-' .AND. work(i+1:i+1).NE.' ' .AND.       &
     &      work(i+1:i+1).NE.':' .AND. work(i+1:i+2).NE.'TO' .AND.      &
     &      work(i+1:i+2).NE.'OR' .AND. work(i+1:i+3).NE.'AND') THEN
            message = ' JUNK AFTER CLOSE PAREN'
            CALL ADDSTR(message,1,TRIM(Type))
            CALL WRTMSG(message,From,To)
            RETURN
         END IF
      END IF
      CALL SQZSTR(work(1:r),'(')
      CALL SQZSTR(work(1:r),')')
      r = LEN_TRIM(work)
!
!     CHECK FOR BALANCED SQUARE BRACKETS, AND DELETE ALL SQUARE
!     BRACKETS.
      n = 0
      DO i = 1, r
         IF(work(i:i).EQ.'[') n = n + 1
         IF(work(i:i).EQ.']') n = n - 1
      END DO
      IF(n.NE.0) THEN
         message = ' UNBALANCED SQUARE BRACKETS'
         CALL ADDSTR(message,1,TRIM(Type))
         CALL WRTMSG(message,From,To)
         RETURN
      END IF
!     Check for garbage before the first or after the last parentheses
!     before deleting
      i = INDEX(work,'[')
      IF(i.GT.1) THEN
         message = ' JUNK BEFORE OPEN BRACKET'
         im = i - 1
         ip = i + 1
!        Try some simplification
         IF(i.NE.2) THEN
            work2 = work(1:im)
            CALL REPSTR(work2,'OR',',')
            CALL REPSTR(work2,'AND',',')
            CALL REPSTR(work2,'TO',',')
            CALL REPSTR(work2,'&',',')
            CALL REPSTR(work2,':',',')
            CALL ADDSTR(work2,LEN_TRIM(work2)+1,work(i:LEN_TRIM(work)))
            work = work2
            i = INDEX(work,'(')
            im = i - 1
            ip = i + 1
         ELSE IF(.NOT.(work(1:1).EQ.'<'.OR.work(1:1).EQ.'>'.OR.         &
     &           (work(1:1).GE.'0'.AND.work(1:1).LE.'9'))) THEN
            CALL ADDSTR(message,1,TRIM(Type))
            CALL WRTMSG(message,From,To)
            RETURN
         END IF
         IF(work(ip:ip).EQ.'+'.OR.work(ip:ip).EQ.'-') THEN
            IF(.NOT.((work(im:im).GE.'0'.AND.work(im:im).LE.'9').OR.    &
     &         work(im:im).EQ.')')) THEN
               CALL ADDSTR(message,1,TRIM(Type))
               CALL WRTMSG(message,From,To)
               RETURN
            END IF
         ELSE IF((work(im:im).EQ.'+'.OR.work(im:im).EQ.'-').AND.        &
     &           .NOT.(work(1:1).EQ.'J'.OR.work(ip:ip).EQ.'&')) THEN
            CALL ADDSTR(message,1,TRIM(Type))
            CALL WRTMSG(message,From,To)
            RETURN
         ELSE IF(work(ip:ip).EQ.'&'.AND.                                &
     &           .NOT.(work(im:im).EQ.'-'.OR.work(im:im).EQ.'+'.OR.     &
     &           (work(im:im).GE.'0'.AND.work(im:im).LE.'9'))) THEN
            CALL ADDSTR(message,1,TRIM(Type))
            CALL WRTMSG(message,From,To)
            RETURN
         ELSE IF(work(ip:ip).EQ.'&') THEN
         ELSE IF(work(im:im).EQ.'&'.AND.                                &
     &           .NOT.(work(i-2:i-2).EQ.'-'.OR.work(i-2:i-2).EQ.'+'.OR. &
     &           (work(i-2:i-2).GE.'0'.AND.work(i-2:i-2).LE.'9'))) THEN
            CALL ADDSTR(message,1,TRIM(Type))
            CALL WRTMSG(message,From,To)
            RETURN
         ELSE IF(work(im:im).EQ.'&') THEN
         ELSE IF(work(1:1).EQ.'J'.OR.                                   &
     &           (work(im:im).GE.'0'.AND.work(im:im).LE.'9').OR.        &
     &           work(im:im).EQ.'<'.OR.work(im:im).EQ.'>'.OR.           &
     &           work(i-2:im).EQ.'AP'.OR.work(i-2:i-2).EQ.'G'.OR.       &
     &           work(i-2:i-2).EQ.'L') THEN
         ElseIf(INDEX(work(1:i-1),'/') .GT. 0)Then
            If(Typstr(work(1:INDEX(work(1:i-1),'/')-1)).NE.1 .OR.       &
     &        Typstr(work(INDEX(work(1:i-1),'/')+1:i-1)).NE.1)Then      &
               CALL ADDSTR(message,1,TRIM(Type))
               CALL WRTMSG(message,From,To)
            EndIf 
         ElseIf(Typstr(work(1:i-1)) .EQ. 1)Then
         ELSE IF(work(im:im).NE.',') THEN
            CALL ADDSTR(message,1,TRIM(Type))
            CALL WRTMSG(message,From,To)
            RETURN
         END IF
      END IF
      i = INDEX(work,']')
      IF(i.GT.0.AND.i.LT.LEN_TRIM(work)) THEN
         i = LEN_TRIM(work)
         DO WHILE (work(i:i).NE.']')
            i = i - 1
         END DO
         IF(work(i+1:i+1).NE.','.AND.work(i+1:i+1).NE.'+'.AND.          &
     &      work(i+1:i+1).NE.'-') THEN
            CALL ADDSTR(message,1,TRIM(Type))
            CALL WRTMSG(message,From,To)
            RETURN
         END IF
      END IF
      CALL SQZSTR(work(1:r),'[')
      CALL SQZSTR(work(1:r),']')
      r = LEN_TRIM(work)
!     Check for leading "+" or "-" and warn of possible misalignment
      IF(LEN_TRIM(work).GT.1.AND.(work(1:1).EQ.'+'.OR.work(1:1).EQ.'-'))&
     &   THEN
         message = 'CHECK FOR POSSIBLE MISALIGNMENT'
         CALL ADDSTR(message,1,'<W>')
         CALL WRTMSG(message,From,To)
      END IF
!     Check for non-numeric characters preceding "/2"
      i = 1
      DO WHILE (.TRUE.)
         i = INDEXF(work,i,'/2')
         IF(i.GT.0) THEN
            IF(i.EQ.1) THEN
               CALL WRTMSG('<E> INVALID J FIELD',From,To)
               RETURN
            ELSE IF(TYPSTR(work(i-1:i-1)).NE.1) THEN
               CALL WRTMSG('<E> INVALID J FIELD',From,To)
               RETURN
            ELSE
               i = i + 2
               CYCLE
            END IF
         END IF
!        Check for leading "J" or "J+" and delete (TWB. 930323)
!        Also allow "J AP"
!        Expand to allow other characters as per new manual (TWB.
!        20011128). Now only J+ or Jx+ allowed (TWB. 20020509)
         i = INDEX(work,'J')
         IF(i.GT.0) THEN
!           Simplify for check on operators
            CALL REPSTR(work,'GE','AP')
            CALL REPSTR(work,'LE','AP')
            IF(work(1:i-1).EQ.' '.OR.i.EQ.1) THEN
               CALL DELSTR(work,i,1)
               r = LEN_TRIM(work)
               IF(r.EQ.0) THEN
                  IF(JNO.EQ.0) THEN
                     JNO = 1
                     JSAv(JNO) = 'J'
                  ELSE
                     DO j = 1, JNO
                        IF(JSAv(j).EQ.'J') THEN
                           message = ' J PREVIOUSLY ASSIGNED'
                           CALL ADDSTR(message,1,TRIM(Type))
                           CALL WRTMSG(message,From,To)
                           RETURN
                        END IF
                     END DO
                     IF(JNO.LT.jmax) THEN
                        JNO = JNO + 1
                        JSAv(JNO) = 'J'
                     ELSE
                        CALL WRTMSG('<I> MAX JX FOR CHECKING EXCEEDED', &
     &                              0,0)
                     END IF
                  END IF
                  RETURN
               ELSE
                  j = INDEX(work,'+')
                  IF(j.GT.0) THEN
                     IF(j.GT.1) THEN
                        IF(TYPSTR(work(1:j-1)).NE.1) THEN
                           message = ' INVALID J FIELD'
                           CALL ADDSTR(message,1,TRIM(Type))
                           CALL WRTMSG(message,From,To)
                           RETURN
                        END IF
                        IF(JNO.GT.0) THEN
                           DO k = 1, JNO
                              IF(JSAv(k).EQ.'J'//work(1:j-1)) GO TO 5
                           END DO
                           message = '<W> HEAD J'//work(1:j-1)          &
     &                               //' MISSING'
                           CALL WRTMSG(message,From,To)
                        END IF
                     END IF
    5                Continue
                     work = work(j+1:)
                     r = LEN_TRIM(work)
                     IF(r.EQ.0) THEN
                        message = ' INVALID J FIELD'
                        CALL ADDSTR(message,1,TRIM(Type))
                        CALL WRTMSG(message,From,To)
                        RETURN
                     END IF
                     IF(TYPSTR(work).NE.1) THEN
                        message = ' INVALID J FIELD'
                        CALL ADDSTR(message,1,TRIM(Type))
                        CALL WRTMSG(message,From,To)
                        RETURN
                     END IF
                  ELSE IF(TYPSTR(work).EQ.1) THEN
                     IF(JNO.EQ.0) THEN
                        JNO = 1
                        JSAv(JNO) = 'J'//work
                     ELSE
                        DO k = 1, JNO
                           IF(JSAv(k).EQ.'J'//work) THEN
                              message = ' J'//work
                              CALL ADDSTR(message,LEN_TRIM(message)+1,  &
     &                           ' PREVIOUSLY ASSIGNED')
                              CALL ADDSTR(message,1,TRIM(Type))
                              CALL WRTMSG(message,From,To)
                              RETURN
                           END IF
                        END DO
                        JNO = JNO + 1
                        JSAv(JNO) = 'J'//work
                     END IF
                     RETURN
                  ELSE IF(INDEX(work,'AP').NE.i.AND.INDEX(work,'AP')    &
     &                    .GT.0) THEN
                     j = INDEX(work,'AP')
                     IF(TYPSTR(work(i:j-1)).NE.1) THEN
                        message = ' INVALID J FIELD'
                        CALL ADDSTR(message,1,TRIM(Type))
                        CALL WRTMSG(message,From,To)
                        RETURN
                     END IF
                     IF(JNO.EQ.0) THEN
                        JNO = 1
                        JSAv(JNO) = 'J'//work(i:j-1)
                     ELSE
                        DO k = 1, JNO
                           IF(JSAv(k).EQ.'J'//work(i:j-1)) THEN
                              CALL ADDSTR(message,LEN_TRIM(message)+1,  &
     &                           ' PREVIOUSLY ASSIGNED')
                              CALL ADDSTR(message,1,TRIM(Type))
                              CALL WRTMSG(message,From,To)
                              RETURN
                           END IF
                        END DO
                        JNO = JNO + 1
                        JSAv(JNO) = 'J'//work(i:j-1)
                     END IF
                     work = work(j+2:)
                     CALL LBSUP(work)
                     r = LEN_TRIM(work)
                     IF(r.EQ.0) THEN
                        message = ' INVALID J FIELD'
                        CALL ADDSTR(message,1,TRIM(Type))
                        CALL WRTMSG(message,From,To)
                        RETURN
                     END IF
                     IF(work(r:r).EQ.'+'.OR.work(r:r).EQ.'-') THEN
                        CALL DELSTR(work,r,1)
                        r = r - 1
                     END IF
!                    Check even/odd A versus J first
                     IF(Odd) THEN
                        IF(INDEX(work,'/2').EQ.0.AND.                   &
     &                     BREAK(work,1,'0123456789').LE.r) THEN
                           message = ' ODD A, INTEGER J'
                           CALL ADDSTR(message,1,TRIM(Type))
                           CALL WRTMSG(message,From,To)
                        END IF
                     ELSE IF(INDEX(work,'/2').NE.0) THEN
                        message = ' EVEN A, HALF-INTEGER J'
                        CALL ADDSTR(message,1,TRIM(Type))
                        CALL WRTMSG(message,From,To)
                     END IF
                     IF(r.GT.1) THEN
                        IF(work(r-1:r).EQ.'/2') CALL DELSTR(work,r-1,2)
                     END IF
                  ELSE IF(work(i:i+1).EQ.'AP') THEN
                     IF(JNO.EQ.0) THEN
                        JNO = 1
                        JSAv(JNO) = 'J'
                     ELSE
                        DO j = 1, JNO
                           IF(JSAv(j).EQ.'J') THEN
                              message = ' J PREVIOUSLY ASSIGNED'
                              CALL ADDSTR(message,1,TRIM(Type))
                              CALL WRTMSG(message,From,To)
                              RETURN
                           END IF
                        END DO
                        IF(JNO.LT.jmax) THEN
                           JNO = JNO + 1
                           JSAv(JNO) = 'J'
                        ELSE
                           CALL WRTMSG(                                 &
     &                                '<I> MAX JX FOR CHECKING EXCEEDED'&
     &                                ,0,0)
                        END IF
                     END IF
                     CALL DELSTR(work,i,2)
                     r = LEN_TRIM(work)
                     IF(r.EQ.0) THEN
                        message = ' INVALID J FIELD'
                        CALL ADDSTR(message,1,TRIM(Type))
                        CALL WRTMSG(message,From,To)
                        RETURN
                     END IF
                     IF(work(r:r).EQ.'+'.OR.work(r:r).EQ.'-') THEN
                        CALL DELSTR(work,r,1)
                        r = r - 1
                     END IF
!                    Check even/odd A versus J first
                     IF(Odd) THEN
                        IF(INDEX(work,'/2').EQ.0.AND.                   &
     &                     BREAK(work,1,'0123456789').LE.r) THEN
                           message = ' ODD A, INTEGER J'
                           CALL ADDSTR(message,1,TRIM(Type))
                           CALL WRTMSG(message,From,To)
                        END IF
                     ELSE IF(INDEX(work,'/2').NE.0) THEN
                        message = ' EVEN A, HALF-INTEGER J'
                        CALL ADDSTR(message,1,TRIM(Type))
                        CALL WRTMSG(message,From,To)
                     END IF
                     IF(r.GT.1) THEN
                        IF(work(r-1:r).EQ.'/2') CALL DELSTR(work,r-1,2)
                     END IF
                  ELSE
                     message = ' INVALID J FIELD'
                     CALL ADDSTR(message,1,TRIM(Type))
                     CALL WRTMSG(message,From,To)
                     RETURN
                  END IF
               END IF
               IF(TYPSTR(work).NE.1) THEN
                  message = ' INVALID J FIELD'
                  CALL ADDSTR(message,1,TRIM(Type))
                  CALL WRTMSG(message,From,To)
                  RETURN
               END IF
            END IF
            RETURN
         END IF
!
!        Simplified following logic by using REPSTR (TWB. 930323)
!
!        REPLACE ALL OCCURRANCES OF 'OR', 'AND', 'TO', '&', AND ':'
!        WITH ','.
         CALL REPSTR(work,'OR',',')
         CALL REPSTR(work,'AND',',')
         CALL REPSTR(work,'TO',',')
         CALL REPSTR(work,'&',',')
         CALL REPSTR(work,':',',')
!
!        REPLACE '+' WITH '-'.
!
         CALL REPSTR(work,'+','-')
!
!        Check even/oddness of A before removing "/2"
         IF(Odd) THEN
            message = ' ODD A, INTEGER J'
            i = INDEX(work,'/2')
            IF(i.EQ.0.AND.BREAK(work,1,'0123456789').LE.LEN(work)) THEN
               CALL ADDSTR(message,1,TRIM(Type))
               CALL WRTMSG(message,From,To)
            ELSE
               j = INDEX(work,',')
               DO WHILE (.TRUE.)
                  IF(j.EQ.0) THEN
                     GO TO 10
                  END IF
                  IF(i.GT.j) THEN
                     CALL ADDSTR(message,1,TRIM(Type))
                     CALL WRTMSG(message,From,To)
                     GO TO 10
                  ELSE
                     i = INDEXF(work,j,'/2')
                     IF(i.EQ.0) THEN
                        CALL ADDSTR(message,1,TRIM(Type))
                        CALL WRTMSG(message,From,To)
                        GO TO 10
                     END IF
                     j = INDEXF(work,i,',')
                  END IF
               END DO
            END IF
         ELSE IF(INDEX(work,'/2').GT.0) THEN
            message = ' EVEN A, HALF-INTEGER J'
            CALL ADDSTR(message,1,TRIM(Type))
            CALL WRTMSG(message,From,To)
         END IF
   10    EXIT
      END DO
!
!     REMOVE '/2'.
!
      CALL REPSTR(work,'/2',CHAR(0))
      DO WHILE (.TRUE.)
!
!        REPLACE DIGIT STRING WITH 'J'.
!
         i = 1
         i = BREAK(work(1:r),i,'0123456789')
         IF(i.LE.r) THEN
            l = SPAN(work(1:r),i,'0123456789')
            work(i:i) = 'J'
            l = l - i
            IF(l.GT.1) THEN
               CALL DELSTR(work(i:r),2,l-1)
               r = r - l + 1
            END IF
            CYCLE
         END IF
!
!        REPLACE 'J-' WITH 'K'.
!
         CALL REPSTR(work,'J-','K')
!
!        REPLACE 'J' WITH 'K'.
!
         CALL REPSTR(work,'J','K')
!
!        REPLACE '-' WITH 'K'.
!
         CALL REPSTR(work,'-','K')
!
!        NOW, LOOK FOR 'NOTK' AS STAND ALONE.
!
         IF(work(1:r).EQ.'NOTK') RETURN
         EXIT
      END DO
      DO WHILE (.TRUE.)
!
!        REPLACE 'LEK' AND 'GEK' WITH 'K'.
!
         i = INDEX(work(1:r),'EK')
         IF(i.GT.0) THEN
            i = i - 1
            IF(work(i:i).EQ.'L'.OR.work(i:i).EQ.'G') THEN
               CALL DELSTR(work(i:r),1,2)
               r = r - 2
               CYCLE
            END IF
         END IF
         EXIT
      END DO
      DO WHILE (.TRUE.)
!
!        REDUCE 'K,K' TO 'K'.
!
         i = INDEX(work(1:r),'K,K')
         IF(i.GT.0) THEN
            CALL DELSTR(work(i:r),2,2)
            r = r - 2
            CYCLE
         END IF
!
!        FINALLY, LOOK FOR 'K' ALONE.
!
         IF(work(1:r).NE.'K') THEN
            message = ' INVALID J FIELD'
            CALL ADDSTR(message,1,TRIM(Type))
            CALL WRTMSG(message,From,To)
         END IF
         EXIT
      END DO
!
      RETURN
      END SUBROUTINE CKJ
!
!***********************************************************************
!
      SUBROUTINE CKL(Field,From,To,Type)
!
!     CHECKS THE VALIDITY OF ANGULAR MOMENTUM TRANSFER FIELD
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Field, Type
      INTEGER(KIND=4) :: From, To
!
!     Functions used
!
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
      INTEGER(KIND=4), INTRINSIC :: LEN_TRIM
!
!     Local variables
!
      CHARACTER(LEN=38) :: message
      CHARACTER(LEN=30) :: work
!
!     CHECK FOR BLANK FIELD.
!
      IF(LEN_TRIM(Field).EQ.0) RETURN
!
!     EVALUATE THE FIELD.
!
      work = Field
!     DELETE LEADING (OR INTERMEDIARY) BLANKS.
      CALL SQZSTR(work,' ')
!     Replace "+?" with "+#" - do this first since this is the only
!     allowed usage of ?
      CALL REPSTR(work,'+?','+#')
!     REPLACE ',' WITH '+'.
!     REPLACE '0-9' WITH '#'.
!     REPLACE 'G' AS IN GE WITH 'L' AS IN LE.
!     REPLACE '[' WITH '(' AND ']' WITH ')'.
      CALL REPCHR(work,',0123456789G[]','+##########L()')
!     REDUCE MULTIPLE DIGITS TO A SINGLE DIGIT.
      CALL REPSTR(work,'##','#')
!     RELATIONALS BECOME DIGIT.
      CALL REPSTR(work,'AP#','#')
      CALL REPSTR(work,'LT#','#')
      CALL REPSTR(work,'<#','#')
      CALL REPSTR(work,'>#','#')
      CALL REPSTR(work,'LE#','#')
!     If an L record, multipolarity may be given or transverse component
      IF(RTYpe.EQ.4) THEN
         CALL REPCHR(work,'MEDQOC','######')
         CALL REPSTR(work,'##','#')
      END IF
!     MULTIPLES BECOME SINGLE.
      CALL REPSTR(work,'#+#+#','#')
      CALL REPSTR(work,'#+#','#')
!     REMOVE PROPERLY POSITIONED PARENTHESES.
      CALL REPSTR(work,'(#)','#')
!     MULTIPLES BECOMES SINGLES (NEEDED AGAIN SINCE WE DROPPED PARENS.
      CALL REPSTR(work,'#+#','#')
      CALL REPSTR(work,'(+#)','+#')
      CALL REPSTR(work,'+(#)','+#')
!     MULTIPLES BECOMES SINGLES (NEEDED AGAIN SINCE WE DROPPED PARENS.
      CALL REPSTR(work,'#+#','#')
!     REMOVE PROPERLY POSITIONED PARENTHESES. (Needed again for nesting)
      CALL REPSTR(work,'(#)','#')
!     IF EVERYTHING IS RIGHT WE SHOULD HAVE JUST '#'.
      IF(work.NE.'#') THEN
         message = ' INVALID L FIELD'
         CALL ADDSTR(message,1,TRIM(Type))
         CALL WRTMSG(message,From,To)
      END IF
!
      RETURN
      END SUBROUTINE CKL
!
!***********************************************************************
!
      SUBROUTINE CKM(Field,From,To,Type)
!
!     CHECKS THE VALIDITY OF MULTIPOLARITY FIELD.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Field, Type
      INTEGER(KIND=4) :: From, To
!
!     Functions used
!
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
      INTEGER(KIND=4), INTRINSIC :: INDEX, LEN_TRIM
      INTEGER(KIND=4), EXTERNAL :: TYPSTR
!
!     Local variables
!
      CHARACTER(LEN=38) :: message
      CHARACTER(LEN=10) :: work
      INTEGER(KIND=4) :: i, n, r
!
!     CHECK FOR BLANK FIELD.
!
      r = LEN_TRIM(Field)
      IF(r.EQ.0) RETURN
      work = Field
!
!     CHECK FOR 'IF' AND 'NOT'.
!     IF 'IF' AND ',' IN STRING, ISSUE WARNING.
!
      i = INDEX(work(1:r),'IF')
      IF(i.GT.0) THEN
         CALL WRTMSG('<W> OBSOLESCENT FORMALISM - USE [...]',From+i-1,  &
     &               From+i+1)
         CALL DELSTR(work(i:r),1,2)
         r = r - 2
      END IF
!
!     SQUEEZE OUT BLANKS.
      CALL SQZSTR(work,' ')
      r = LEN_TRIM(work)
!
      i = INDEX(work(1:r),'NOT')
      IF(i.GT.0) THEN
         CALL DELSTR(work(i:r),1,3)
         r = r - 3
      END IF
!
!     CHECK FOR BALANCED PARENTHESES, AND DELETE ALL PARENTHESES.
!
      n = 0
      DO i = 1, r
         IF(work(i:i).EQ.'(') n = n + 1
         IF(work(i:i).EQ.')') n = n - 1
      END DO
      IF(n.NE.0) THEN
         message = ' UNBALANCED PARENTHESES'
         CALL ADDSTR(message,1,TRIM(Type))
         CALL WRTMSG(message,From,To)
         RETURN
      END IF
      CALL SQZSTR(work(1:r),'(')
      CALL SQZSTR(work(1:r),')')
      r = LEN_TRIM(work)
!
!     CHECK FOR BALANCED SQUARE BRACKETS, AND DELETE ALL SQUARE
!     BRACKETS.
      n = 0
      DO i = 1, r
         IF(work(i:i).EQ.'[') n = n + 1
         IF(work(i:i).EQ.']') n = n - 1
      END DO
      IF(n.NE.0) THEN
         message = ' UNBALANCED SQUARE BRACKETS'
         CALL ADDSTR(message,1,TRIM(Type))
         CALL WRTMSG(message,From,To)
         RETURN
      END IF
      CALL SQZSTR(work(1:r),'[')
      CALL SQZSTR(work(1:r),']')
      r = LEN_TRIM(work)
      DO WHILE (.TRUE.)
!
!        CHECK AND REPLACE 'E'L WITH 'X'.
!
         i = INDEX(work(1:r),'E')
         IF(i.GT.0) THEN
            IF(TYPSTR(work(i+1:i+1)).EQ.1) THEN
               work(i:i) = 'X'
               CALL DELSTR(work(i:r),2,1)
               r = r - 1
               CYCLE
            ELSE
               message = ' E MUST BE FOLLOWED BY DIGIT'
               CALL ADDSTR(message,1,TRIM(Type))
               CALL WRTMSG(message,From,To)
               RETURN
            END IF
         END IF
         EXIT
      END DO
      DO WHILE (.TRUE.)
!
!        CHECK AND REPLACE 'M'L WITH 'Y'.
!
         i = INDEX(work(1:r),'M')
         IF(i.GT.0) THEN
            IF(TYPSTR(work(i+1:i+1)).EQ.1) THEN
               IF(work(i+1:i+1).EQ.'0') THEN
                  message = ' M CAN''T BE FOLLOWED BY ZERO'
                  CALL ADDSTR(message,1,TRIM(Type))
                  CALL WRTMSG(message,From,To)
               END IF
               work(i:i) = 'Y'
               CALL DELSTR(work(i:r),2,1)
               r = r - 1
               CYCLE
            ELSE
               message = ' M MUST BE FOLLOWED BY DIGIT'
               CALL ADDSTR(message,1,TRIM(Type))
               CALL WRTMSG(message,From,To)
               RETURN
            END IF
         END IF
         EXIT
      END DO
      DO WHILE (.TRUE.)
!
!        REDUCE 'X+Y' TO 'X'.
!
         i = INDEX(work(1:r),'X+Y')
         IF(i.GT.0) THEN
            work(i:i) = 'X'
            CALL DELSTR(work(i:r),2,2)
            r = r - 2
            CYCLE
         END IF
         EXIT
      END DO
      DO WHILE (.TRUE.)
!
!        REDUCE 'Y+X' TO 'X'.
!
         i = INDEX(work(1:r),'Y+X')
         IF(i.GT.0) THEN
            work(i:i) = 'X'
            CALL DELSTR(work(i:r),2,2)
            r = r - 2
            CYCLE
         END IF
         EXIT
      END DO
      DO WHILE (.TRUE.)
!
!        REDUCE 'D' TO 'X'.
!
         i = INDEX(work(1:r),'D')
         IF(i.GT.0) THEN
            work(i:i) = 'X'
            CYCLE
         END IF
         EXIT
      END DO
      DO WHILE (.TRUE.)
!
!        REDUCE 'Q' TO 'X'.
!
         i = INDEX(work(1:r),'Q')
         IF(i.GT.0) THEN
            work(i:i) = 'X'
            CYCLE
         END IF
         EXIT
      END DO
      DO WHILE (.TRUE.)
!
!        REDUCE 'O' TO 'X'.
!
         i = INDEX(work(1:r),'O')
         IF(i.GT.0) THEN
            work(i:i) = 'X'
            CYCLE
         END IF
         EXIT
      END DO
      DO WHILE (.TRUE.)
!
!        REDUCE 'Y' TO 'X'.
!
         i = INDEX(work(1:r),'Y')
         IF(i.GT.0) THEN
            work(i:i) = 'X'
            CYCLE
         END IF
         EXIT
      END DO
      DO WHILE (.TRUE.)
!
!        REDUCE 'X+X' TO 'X'.
!
         i = INDEX(work(1:r),'X+X')
         IF(i.GT.0) THEN
            work(i:i) = 'X'
            CALL DELSTR(work(i:r),2,2)
            r = r - 2
            CYCLE
         END IF
         EXIT
      END DO
      DO WHILE (.TRUE.)
!
!        REDUCE 'X,X' TO 'X'.
!
         i = INDEX(work(1:r),'X,X')
         IF(i.GT.0) THEN
            work(i:i) = 'X'
            CALL DELSTR(work(i:r),2,2)
            r = r - 2
            CYCLE
         END IF
!
!        FINALLY, LOOK FOR 'X' ALONE.
!
         IF(work(1:r).NE.'X') THEN
            message = ' INVALID M FIELD'
            CALL ADDSTR(message,1,TRIM(Type))
            CALL WRTMSG(message,From,To)
         END IF
         EXIT
      END DO
!
      RETURN
      END SUBROUTINE CKM
!
!***********************************************************************
!
      SUBROUTINE CKMS(Field,From,To)
!
!     CHECKS THE VALIDITY OF METASTABLE STATE FIELD.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Field
      INTEGER(KIND=4) :: From, To
!
!     Functions used
!
      INTEGER(KIND=4), EXTERNAL :: TYPSTR
!
!     Local variables
!
      INTEGER(KIND=4) :: i, t
      Character(LEN=38) :: message
!
      IF(Field.EQ.'  ') THEN
      ELSE IF(Field(1:1).EQ.'M') THEN
         message=' '
         t = TYPSTR(Field(2:2))
         IF(t.LT.0.OR.1.LT.t) THEN
            CALL WRTMSG('<E> MUST BE DIGIT OR BLANK',To,To)
         END IF
         nms=nms+1
	 seqms(nms)=nseq
	 msstr(nms)=field
	 Do i=1,nms-1
	    If(field .EQ. msstr(i))Then
	       write(message,'(I6)')seqms(i)
	       Call Lbsup(message)
               Call Addstr(message,1,                                   &
     &           '<E> MS identical to that for record')
	    Elseif(field .LT. msstr(i))Then
	       write(message,'(I6)')seqms(i)
	       Call Lbsup(message)
               Call Addstr(message,1,                                   &
     &           '<E> MS<'//msstr(i)//' for record ')
	    EndIf
	 EndDo
         If(message .NE. ' ')Call Wrtmsg(message,From,To)
      ELSE
         CALL WRTMSG('<E> INVALID MS FIELD',From,To)
      END IF
!
      RETURN
      END SUBROUTINE CKMS
!
!***********************************************************************
!
      SUBROUTINE CKQ(Field,From,To)
!
!     CHECKS THE VALIDITY OF Q FIELD.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Field
      INTEGER(KIND=4) :: From, To
!
      IF(Field.EQ.' ') THEN
      ELSE IF(Field.EQ.'?') THEN
      ELSE IF(Field.NE.'S') THEN
         CALL WRTMSG('<E> INVALID Q FIELD',From,To)
      END IF
!
      RETURN
      END SUBROUTINE CKQ
!
!***********************************************************************
!
      SUBROUTINE CKREA(Field,From,To)
!
!     CHECK REACTION.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Field
      INTEGER(KIND=4) :: From, To
!
!     Functions used
!
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
      INTEGER(KIND=4), INTRINSIC :: INDEX, LEN
      INTEGER(KIND=4), EXTERNAL :: INDEXF, TYPSTR
      REAL(KIND=4), INTRINSIC :: ABS
!
!     Local variables
!
      INTEGER(KIND=4) :: a, i, j, l, m, r, z
      INTEGER(KIND=4) :: calca,calcz,testa,testz
      INTEGER(KIND=4) :: from2
      Logical(KIND=4) :: plerr,mierr
!
!     CHECK/SET LEFT AND RIGHT EXCLUSIVE OF ().
!
      l = 1
      IF(Field(1:1).NE.'(') THEN
         CALL WRTMSG('<E> MISSING "("',From,From)
      ELSE
         l = 2
      END IF
      r = LEN(Field)
      IF(Field(r:r).NE.')') THEN
         CALL WRTMSG('<E> MISSING ")"',To,To)
      ELSE
         r = r - 1
      END IF
!
!     CHECK/SET MIDDLE (COMMA).  IF NOT FOUND, STOP HERE.
!
      m = INDEX(Field(1:r),',')
      IF(m.EQ.0) THEN
         CALL WRTMSG('<F> MISSING COMMA',From-1+l,From-1+r)
         RETURN
      END IF
!
!     Check for too many commas
      i=Indexf(Field(1:r),m+1,',')
      If(i .GT. 0)Then
        CALL WRTMSG('<F> Too many commas',From-1+l,From-1+r) 
         RETURN
      END IF
!
!
!     CHECK LEFT SIDE FIRST.
!
      
      If(Field(l:l+3) .EQ. 'POL ')l=l+4
      incnuc=Field(l:m-1)
      IF(Field(l:m-1).EQ.'X') THEN
         inca=0
	 incz=0
      ELSE IF(Field(l:m-1).EQ.'G') THEN
         inca=0
	 incz=0
      ELSE IF(Field(l:m-1).EQ.'N') THEN
         inca=1
	 incz=0
      ELSE IF(Field(l:m-1).EQ.'P') THEN
         inca=1
	 incz=1
      ELSE IF(Field(l:m-1).EQ.'D') THEN
         inca=2
	 incz=1
      ELSE IF(Field(l:m-1).EQ.'T') THEN
         inca=3
	 incz=1
      ELSE IF(Field(l:m-1).EQ.'A') THEN
         inca=4
	 incz=2
      ELSE IF(Field(l:m-1).EQ.'MU') THEN
      ELSE IF(Field(l:m-1).EQ.'KAPPA') THEN
      ELSE IF(Field(l:m-1).EQ.'KAPPA+') THEN
         inca=0
	 incz=1
      ELSE IF(Field(l:m-1).EQ.'KAPPA-') THEN
         inca=0
	 incz=-1
      ELSE IF(Field(l:m-1).EQ.'MU+') THEN
         inca=0
	 incz=1
      ELSE IF(Field(l:m-1).EQ.'MU-') THEN
         inca=0
	 incz=-1
      ELSE IF(Field(l:m-1).EQ.'MU0') THEN
         inca=0
	 incz=0
      ELSE IF(Field(l:m-1).EQ.'PI') THEN
      ELSE IF(Field(l:m-1).EQ.'PI+') THEN
         inca=0
	 incz=1
      ELSE IF(Field(l:m-1).EQ.'PI-') THEN
         inca=0
	 incz=-1
      ELSE IF(Field(l:m-1).EQ.'NU') THEN
         inca=0
	 incz=0
      ELSE IF(Field(l:m-1).EQ.'K') THEN
      ELSE IF(Field(l:m-1).EQ.'K+') THEN
         inca=0
	 incz=1
      ELSE IF(Field(l:m-1).EQ.'K-') THEN
         inca=0
	 incz=-1
      ELSE IF(Field(l:m-1).EQ.'E') THEN
      ELSE IF(Field(l:m-1).EQ.'E+') THEN
         inca=0
	 incz=1
      ELSE IF(Field(l:m-1).EQ.'E-') THEN
         inca=0
	 incz=-1
      ELSE IF(Field(l:m-1).EQ.'HI') THEN
!        Manual only defines "(HI,XNG)" which is taken care of in
!        routines - make this a warning since there is some ambiguity
         IF(Field(m+1:m+3).NE.'XNG')                                    &
     &      CALL WRTMSG('<W> CHECK REACTION',From-1+l,From-2+r)
         RETURN
      ELSE IF(Field(l:m-1).EQ.'POL PARTICLE') THEN
      ELSE IF(Field(l:m-1).NE.'POL NUCLIDE') THEN
         CALL NUCID(Field(l:m-1),a,z)
         inca=a
	 incz=z
         IF(z.LE.0) THEN
            CALL WRTMSG('<E> INVALID NUCLID',From-1+l,From-2+m)
            inca=-9999
	    incz=-9999      
         ELSE IF(IZLmsg.NE.' ') THEN
            CALL REPSTR(IZLmsg,'Obsolete formalism','Obsol. form')
            CALL ADDSTR(IZLmsg,1,'<W> ')
            CALL WRTMSG(TRIM(IZLmsg),From+2+l,From-2+m)
         END IF
      END IF
!
!     NOW CHECK RIGHT SIDE.
!
      i = INDEXF(Field(1:r),m+1,' ')
!     Must allow for polarization of outgoing particle (TWB. 930309)
      IF(i.GT.0) THEN
         IF(Field(m+1:i).EQ.'POL ') THEN
            m = i
         ELSE
            CALL WRTMSG('<E> INVALID BLANK',From-1+i,From-1+i)
         END IF
      END IF
      DO i = m + 1, r
         j = TYPSTR(Field(i:i))
         IF(j.GT.0) THEN
         ELSE IF(Field(i:i).EQ.'+') THEN
         ELSE IF(Field(i:i).EQ.'-') THEN
         ELSE IF(Field(i:i).EQ.' ') THEN
         ELSE IF(Field(i:i).NE.'''') THEN
            CALL WRTMSG('<E> INVALID CHARACTER',From-1+i,From-1+i)
         END IF
      END DO
      Call GetOut(field(m+1:r),plerr,mierr)
      If(plerr)Then
         i=r
	 Do While(field(i:i) .NE. '+')
            i=i-1
	 EndDo
         Call Wrtmsg('<E> Embedded "+" illegal in outgoing',            &
     &     to-(r-i+1),to-(r-i+1))
      ElseIf(mierr)Then
         i=r
	 Do While(field(i:i) .NE. '-')
            i=i-1
	 EndDo
         Call Wrtmsg('<E> Embedded "-" illegal in outgoing',            &
     &     to-(r-i+1),to-(r-i+1))
      EndIf
      If(tara.NE.-9999 .AND. tarz.NE.-9999 .AND.                        &
     &  inca.NE.-9999 .AND. incz.NE.-9999 .AND.                         &
     &  outa.NE.-9999 .AND. outz.NE.-9999)Then
         calca=tara+inca-outa
         calcz=tarz+incz-outz
        testa=resa-calca
	testz=resz-calcz
        If(tarpos.LT.0)Then
	   from2=from
        ElseIf(tarpos.GE.to .AND. totcon.GT.0)Then
           from2=from
           If(tarpos .GT. conpos(totcon))Then
	      from2=tarpos-conpos(totcon)
	   Else
	      Do i=totcon-1,1,-1
	         If(tarpos.GE.conpos(i) .AND. tarpos.LT.conpos(i+1))Then
		    from2=tarpos-conpos(i)
		 EndIf
	      EndDo
	   EndIf
	Else
	   from2=tarpos
	EndIf
        If(.NOT.isiar)Then
           If((testa.NE.0 .OR. testz.NE.0))Then
              If((ABS(resa-tara).LE.1 .AND. ABS(resz-tarz).LE.1)        &
     &          .AND. (ABS(testa).LE.1 .AND. ABS(testz).LE.1))Then
                 Call                                                   &
     &            Wrtmsg('<W> Possible target, reac. & NUCID mismatch', &
     &            from2,To)
              ElseIf((tara+inca).EQ.resa .AND. (tarz+incz).EQ.resz)Then
                 Call                                                   &
     &             Wrtmsg('<W> Possible target, reac. & NUCID mismatch',&
     &             from2,To)
              ElseIf(outa.EQ.resa .AND. outz.EQ.resz)Then
                 Call                                                   &
     &             Wrtmsg('<W> Possible target, reac. & NUCID mismatch',&
     &             from2,To)
              ElseIf(outa.EQ.0 .AND. outz.EQ.0 .AND. inca.EQ.0          &
     &               .AND. incz.EQ.0)Then
                 Call                                                   &
     &             Wrtmsg('<W> Possible target, reac. & NUCID mismatch',&
     &             from2,To)
              ElseIf((tara+coma).EQ.resa .AND. (tarz+comz).EQ.resz)Then
                 Call                                                   &
     &             Wrtmsg('<W> Possible target, reac. & NUCID mismatch',&
     &             from2,To)
              Else
                 Call Wrtmsg('<E> Target, reaction, & NUCID mismatch',  &
     &             from2,To)
              EndIf
           EndIf
	ElseIf(ABS(testa).GT.1 .OR. ABS(testz).GT.1)Then
               Call Wrtmsg('<E> Target, reaction, & NUCID mismatch',    &
     &           from2,To)
        EndIf
      EndIf  
!
      RETURN
      END SUBROUTINE CKREA
!
!***********************************************************************
!
      SUBROUTINE CKREF(Field,From)
!
!     CHECKS THE VALIDITY OF REFERENCE FIELD.
!     REFERENCES ARE OF THE FORM: YYYYAADD OR YYYYAAAA.
!     YYYY is a Four digit year.
!     AA IS A TWO LETTER CODE.
!     DD IS A TWO DIGIT CODE.
!     Obsolete version has a two digit integer
!     THE FIELD CONTAINS ONE OR MORE OF THESE, SEPARATED BY COMMAS.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Field
      INTEGER(KIND=4) :: From
!
!     Functions used
!
      INTEGER(KIND=4), INTRINSIC :: INDEX, LEN_TRIM
      INTEGER(KIND=4), EXTERNAL :: INDEXF, TYPSTR
!
!     Local variables
!
      INTEGER(KIND=4) :: i, l, r, refcnt, yrlen
      CHARACTER(LEN=8), DIMENSION(10) :: reflst
      INTEGER(KIND=4), DIMENSION(10) :: refbeg, refend
!
!     CHECK FOR BLANK FIELD.
!
      r = LEN_TRIM(Field)
      IF(r.EQ.0) RETURN
!     Set up for year length
      IF(ISY2k) THEN
         yrlen = 4
      ELSE
         yrlen = 2
      END IF
!
!     CHECK FOR SHORT FIELD.
!
      IF(r.LT.yrlen+4) THEN
         CALL WRTMSG('<E> INVALID REFERENCE FIELD',From,From-1+r)
         RETURN
      END IF
!
!     CHECK FIELD.
      l = INDEX(Field,',')
      IF(l.EQ.0) THEN
         refcnt = 1
         reflst(refcnt) = Field(1:r)
         refbeg(refcnt) = 1
         refend(refcnt) = r
      ELSE
         refcnt = 1
         reflst(refcnt) = Field(1:l-1)
         refbeg(refcnt) = 1
         refend(refcnt) = l - 1
         i = l + 1
         DO WHILE (.TRUE.)
            refcnt = refcnt + 1
            l = INDEXF(Field,i,',')
            IF(l.GT.0) THEN
               reflst(refcnt) = Field(i:l-1)
               refbeg(refcnt) = i
               refend(refcnt) = l - 1
               i = l + 1
               CYCLE
            ELSE
               reflst(refcnt) = Field(i:r)
               refbeg(refcnt) = i
               refend(refcnt) = r
            END IF
            EXIT
         END DO
      END IF
      DO i = 1, refcnt
         DO WHILE (.TRUE.)
            IF(refend(i)-refbeg(i)+1.EQ.8) THEN
               IF(TYPSTR(reflst(i)(1:4)).NE.1)                          &
     &            CALL WRTMSG('<E> INVALID YEAR',From-1+refbeg(i),      &
     &            From+refbeg(i)+3)
               IF(TYPSTR(reflst(i)(5:6)).NE.2) THEN
                  IF(TYPSTR(reflst(i)(5:5)).NE.2.OR.                    &
     &               (reflst(i)(6:6).LT.'a'.OR.reflst(i)(6:6).GT.'z'))  &
     &               THEN
                     CALL WRTMSG('<E> INVALID AUTHOR CODE',             &
     &                           From+refbeg(i)+3,From+refbeg(i)+4)
                  END IF
               END IF
               IF(TYPSTR(reflst(i)(7:8)).LT.1)                          &
     &            CALL WRTMSG('<E> INVALID UNIQUENESS CODE',            &
     &            From+refbeg(i)+5,From+refbeg(i)+6)
            ELSE IF(refend(i)-refbeg(i)+1.EQ.6) THEN
               IF(ISY2k) THEN
                  CALL WRTMSG('<E> INVALID REFERENCE FIELD',            &
     &                        From-1+refbeg(i),From-1+refend(i))
               ELSE
                  IF(TYPSTR(reflst(i)(1:2)).NE.1)                       &
     &               CALL WRTMSG('<E> INVALID YEAR',From-1+refbeg(i),   &
     &               From+refbeg(i)+1)
                  IF(TYPSTR(reflst(i)(3:4)).NE.2) THEN
                     IF(TYPSTR(reflst(i)(3:3)).NE.2.OR.                 &
     &                  (reflst(i)(4:4).LT.'a'.OR.reflst(i)(4:4).GT.'z')&
     &                  ) THEN
                        CALL WRTMSG('<E> INVALID AUTHOR CODE',          &
     &                              From+refbeg(i)+1,From+refbeg(i)+2)
                     END IF
                  END IF
                  IF(TYPSTR(reflst(i)(5:6)).LT.1)                       &
     &               CALL WRTMSG('<E> INVALID UNIQUENESS CODE',         &
     &               From+refbeg(i)+3,From+refbeg(i)+4)
                  CALL WRTMSG('<W> Obsol. form. - Use YYYYAASS',        &
     &                        From-1+refbeg(i),From-1+refend(i))
               END IF
            ELSE IF(refend(i)-refbeg(i)+1.LT.6) THEN
               CALL WRTMSG('<E> INVALID REFERENCE FIELD',               &
     &                     From-1+refbeg(i),From-1+refend(i))
            ELSE IF(refend(i)-refbeg(i)+1.GT.6.AND.refend(i)-refbeg(i)  &
     &              +1.LT.8) THEN
               CALL WRTMSG('<E> MISSING COMMA BETWEEN REFERENCES',      &
     &                     From+refbeg(i)+5,From-1+refend(i))
               refend(i) = 6
               CYCLE
            ELSE
               CALL WRTMSG('<E> MISSING COMMA BETWEEN REFERENCES',      &
     &                     From+refbeg(i)+7,From-1+refend(i))
               refend(i) = 8
               CYCLE
            END IF
            EXIT
         END DO
      END DO
!
      RETURN
      END SUBROUTINE CKREF
!
!***********************************************************************
!
      SUBROUTINE CKS(Field,From,To,Type)
!
!     CHECKS THE VALIDITY OF SPECTROSCOPIC STRENGTH FIELD.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Field, Type
      INTEGER(KIND=4) :: From, To
!
!     Functions used
!
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
      INTEGER(KIND=4), INTRINSIC :: LEN_TRIM
      INTEGER(KIND=4), EXTERNAL :: INDEXF, IVLSTR, RLSCN, SPAN
!
!     Local variables
!
      Character(LEN=10) work
      CHARACTER(LEN=38) :: message
      INTEGER(KIND=4) :: iend, i, icom, ipcnt, l, m, np, r
      REAL(KIND=4) :: v
!
!     CHECK FOR BLANK FIELD.
!
      r = LEN_TRIM(Field)
      IF(r.EQ.0) RETURN
!
!     Check for use of period instead of comma
!
      icom=INDEX(field,',')
      If(icom .EQ. 0)icom=INDEX(field,'+')
      If(icom .EQ. 0)Then
         ipcnt=0
         Do i=1,LEN_TRIM(field)      
            If(field(i:i) .EQ. '.')ipcnt=ipcnt+1
            If(ipcnt .EQ. 2)iend=i   
         EndDo             
         If(ipcnt .GT. 1)Call Wrtmsg('<E> Use "," instead of "."',      &
     &     from+iend-2,from+iend-2)
      EndIf
!
!     SEQUENTIALLY SCAN FIELD.
!
      np = 0
      i = 0
      DO WHILE (.TRUE.)
         l = SPAN(Field(1:r),i+1,' ')
         IF(Field(l:l).EQ.'(') THEN
            l = l + 1
            np = np + 1
         END IF
         m = INDEXF(Field(1:r),l,')')
         IF(m.GT.0) THEN
            m = INDEXF(Field(1:m),l,'E')
         ELSE
            m = INDEXF(Field(1:r),l,'E')
         END IF
         IF(m.GT.0.AND.m.LE.r-2) THEN
            m = m + 1
            IF(Field(m:m).EQ.'+') m = m + 1
            IF(Field(m:m).EQ.'-') THEN
               m = m + 1
               message = 'EXPONENT TOO SMALL'
            ELSE
               message = 'EXPONENT TOO LARGE'
            END IF
            IF(IVLSTR(Field(m:r)).GE.10) THEN
               IF(IVLSTR(Field(m:r)).LE.30) THEN
                  CALL ADDSTR(message,1,'<W> ')
                  CALL WRTMSG(TRIM(message),From-1+m,From-1+r)
               ELSE
                  CALL ADDSTR(message,1,TRIM(Type))
                  CALL WRTMSG(TRIM(message),From-1+m,From-1+r)
                  RETURN
               END IF
            END IF
         END IF
         i = RLSCN(Field(1:r),l,v)
         IF(i.LE.r) THEN
            IF(Field(i:i).EQ.')') THEN
               i = i + 1
               np = np - 1
            END IF
         END IF
         message = ' INVALID S FIELD'
         CALL ADDSTR(message,1,TRIM(Type))
         IF(i.EQ.l) THEN
            CALL WRTMSG(message,From,To)
         ELSE IF(i.LE.r) THEN
            IF(Field(i:i).EQ.','.OR.Field(i:i).EQ.'+') THEN
               CYCLE
            END IF
            CALL WRTMSG(message,From,To)
         END IF
         EXIT
      END DO
!
      RETURN
      END SUBROUTINE CKS
!
!***********************************************************************
!
      SUBROUTINE CKSYM(Field,From,To)
!
!     CHECK COMMENT/CONTINUATION FIELD NAME.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Field
      INTEGER(KIND=4) :: From, To
!
!     Functions used
!
      INTEGER(KIND=4), INTRINSIC :: ICHAR, INDEX, LEN, LEN_TRIM
      INTEGER(KIND=4), EXTERNAL :: BREAK, SPAN
!
!     Local variables
!
      CHARACTER(LEN=1) :: ch, d1, d2
      CHARACTER(LEN=38) :: messag
      CHARACTER(LEN=71) :: work
      INTEGER(KIND=4) :: a, i, j, k, z
      INTEGER(KIND=4) :: idig
!
      CHARACTER(LEN=9), PARAMETER :: adigits = '123456789'
      INTEGER(KIND=4), PARAMETER :: nndig = 8
      CHARACTER(LEN=nndig), PARAMETER :: shell = 'KLMNOPQR'
      INTEGER(KIND=4), DIMENSION(nndig) :: ndig
      DATA ndig/0, 3, 5, 7, 9, 9, 3, 2/
!
      INTEGER(KIND=4), PARAMETER :: nchar = 7
      INTEGER(KIND=4), PARAMETER :: ncont = 6
      CHARACTER(LEN=nchar), DIMENSION(ncont) :: invcnt
      DATA invcnt/'TITLE', 'BAND', 'SEQ', 'MS', 'COIN', 'UN'/
!
      INTEGER(KIND=4), PARAMETER :: maxsym = 22, ntype = 9
      INTEGER(KIND=4), DIMENSION(ntype) :: nsym
      DATA nsym/10, 7, 16, 22, 9, 11, 15, 18, 11/
      CHARACTER(LEN=nchar), DIMENSION(maxsym,ntype) :: sym
!
      INTEGER(KIND=4), PARAMETER :: cardn = 1, cardp = 2, cardq = 3,    &
     &                              cardl = 4, carda = 5, cardb = 6,    &
     &                              carde = 7, cardg = 8, cardd = 9
!
      DATA(sym(i,cardn),i=1,10)/'NR', 'NT', 'BR', 'NB', 'NP', 'DNR',    &
     &     'DNT', 'DBR', 'DNB', 'DNP'/
!
      DATA(sym(i,cardp),i=1,7)/'E', 'J', 'T', 'QP', 'DE', 'DT', 'DQP'/
!
      DATA(sym(i,cardq),i=1,9)/'Q-', 'SN', 'SP', 'QA', 'QREF', 'DQ-',   &
     &     'DSN', 'DSP', 'DQA'/
!
      DATA(sym(i,cardl),i=1,9)/'E', 'J', 'T', 'L', 'S', 'MS', 'DE',     &
     &     'DT', 'DS'/
!
      DATA(sym(i,carda),i=1,7)/'E', 'IA', 'HF', 'COIN', 'DE', 'DIA',    &
     &     'DHF'/
!
      DATA(sym(i,cardb),i=1,8)/'E', 'IB', 'LOGFT', 'COIN', 'UN', 'DE',  &
     &     'DIB', 'DFT'/
!
      DATA(sym(i,carde),i=1,12)/'E', 'IB', 'IE', 'LOGFT', 'TI', 'COIN', &
     &     'UN', 'DE', 'DIB', 'DIE', 'DFT', 'DTI'/
!
      DATA(sym(i,cardg),i=1,12)/'E', 'RI', 'M', 'MR', 'CC', 'TI',       &
     &     'COIN', 'DE', 'DRI', 'DMR', 'DCC', 'DTI'/
!
      DATA(sym(i,cardd),i=1,9)/'E', 'IP', 'EI', 'T', 'L', 'COIN', 'DE', &
     &     'DIP', 'DT'/
!
!     CONTINUATION CARDS.
!
      DATA(sym(i,cardl),i=10,22)/'TITLE', 'BAND', 'CONF', 'XREF',       &
     &     'FLAG', 'G', 'ISPIN', 'ISPINZ', '%G', '%IT', '%SF',          &
     &     '%EC+%B+', 'SEQ'/
!
      DATA(sym(i,cardq),i=10,16)/'Q+_', 'S(2N)', 'S(2P)', 'QP',         &
     &     'Q(B-N)', 'Q(2B-)', 'Q(ECP)'/   
!
      DATA(sym(i,carda),i=8,9)/'TITLE', 'FLAG'/
!
      DATA(sym(i,cardb),i=9,11)/'TITLE', 'FLAG', 'EAV'/
!
      DATA(sym(i,carde),i=13,15)/'TITLE', 'FLAG', 'EAV'/
!
      DATA(sym(i,cardg),i=13,18)/'TITLE', 'FLAG', 'FL', 'ECC', 'CE',    &
     &     '%IG'/
!
      DATA(sym(i,cardd),i=10,11)/'TITLE', 'FLAG'/
!
!     STATEMENT FUNCTIONS.
!
      IDIG(ch) = ICHAR(ch) - ICHAR('0')
!
!     MAKE ADJUSTMENT FOR D CARD, RTYPE FROM 11 TO 9 (THIS SUBR ONLY).
!
      IF(RTYpe.EQ.11) RTYpe = cardd
!
!     MAKE SURE RTYPE IS BETWEEN CARDN AND CARDD.
!
      IF(RTYpe.LT.cardn.OR.cardd.LT.RTYpe) THEN
         CALL WRTMSG('<F> INVALID RECORD TYPE FOR SYMBOL',0,0)
         GO TO 20
      END IF
!
!     CHECK RECORD TYPES AND FIELDS WHICH ARE INVALID FOR CONTINUATIONS.
!
      IF(CONtyp.GT.0) THEN
         IF(RTYpe.LT.cardq) THEN
            write(*,*) 'cardq and rtype are: ', cardq, rtype
            CALL WRTMSG('<F> NO CONTINUATIONS FOR THIS CARD TYPE',0,0)
            GO TO 20
         END IF
         DO i = 1, ncont
            IF(Field.EQ.invcnt(i)) THEN
               CALL WRTMSG('<E> SYMBOL INVALID FOR CONTINUATIONS',From, &
     &                     To)
               GO TO 20
            END IF
         END DO
      END IF
!
!     CHECK FIELD AGAINST STANDARD FIELD NAMES.
!
      DO i = 1, nsym(RTYpe)
! Put some debugging statements here...
!        write(*,*) 'Checking field ', Field
!        write(*,*) 'i, RTYpe ', i, RTYpe
!        write(*,*) '..and sym-i-rtype ', sym(i,RTYpe)
         IF(Field.EQ.sym(i,RTYpe)) THEN
            IF(TOTal) THEN
               IF(RTYpe.EQ.carde.AND.(Field.EQ.'IE'.OR.Field.EQ.'DIE')) &
     &            CALL WRTMSG('<E> No EC for total ionization',From,To)
               IF(RTYpe.EQ.cardg.AND.DECtyp.NE.4.AND.                   &
     &            (Field.EQ.'CC'.OR.Field.EQ.'DCC'.OR.Field.EQ.'ECC'.OR.&
     &            Field.EQ.'CE'))                                       &
     &            CALL WRTMSG('<E> No CC for total ionization',From,To)
            END IF
            GO TO 20
         END IF
      END DO
!
!     STANDARD NAME NOT FOUND, DO MORE CHECKING.
!
      work = Field
      CALL LBSUP(work)
!
!     L CARDS HAVE SOME EXTRA CHECKS.
!
      IF(RTYpe.EQ.cardl) THEN
!        Check to see if this might be heavy-ion decay (e.g. 14C)
!        (TWB. 930311)
         IF(INDEX(work,'%').EQ.1.AND.                                   &
     &      (work(2:2).GT.'0'.AND.work(2:2).LE.'9')) THEN
            CALL NUCID(work(2:),a,z)
            IF(a.GT.0.AND.z.GT.0) GO TO 20
         END IF
         
!        Cannot have BM0 (TWB. 930312)
         IF(work.EQ.'BM0') GO TO 10
!        Handle fields like WIDTHG0**2/WIDTH before replacing digits
!        (TWB. 930312)
         CALL REPSTR(work,'**2/','/')
         CALL REPSTR(work,'%3HE','%~')
         CALL REPSTR(work,'%3H','%~')
         CALL REPSTR(work,'%8BE','%~')
!        Handling WIDTH getting too complicated - separate (TWB. 961206)
         IF(INDEX(work,'WIDTH').GT.0) CALL DOWIDTHS(work)
!        Handling the multitude of decay modes getting too complicated
!        - separate (TWB. 961206)
         IF(INDEX(work,'%').EQ.1.AND.work.NE.'%'.AND.work.NE.'%~')      &
     &      CALL DODECAYS(work)
         CALL REPCHR(work,adigits,'#########')
         CALL REPSTR(work,'E#','E')
         CALL REPSTR(work,'M#','M')
         CALL REPSTR(work,'B#','B')
!        Allow BM# (TWB. 930312)
         IF(work.EQ.'MOME'.OR.work.EQ.'MOMM'.OR.work.EQ.'BE'.OR.        &
     &      work.EQ.'B'.OR.work.EQ.'BM') GO TO 20
!        Allow for multiparticle decay of the same type (TWB. 930311)
         CALL REPSTR(work,'%#','%')
         CALL REPCHR(work,'NPA','~~~')
         CALL REPSTR(work,'%G','%~')
         CALL REPSTR(work,'%D','%~')
         IF(work.EQ.'%~') GO TO 20
         CALL REPSTR(work,'#~','*')
         CALL REPSTR(work,'~','*')
         CALL REPSTR(work,'-*','-')
         CALL REPSTR(work,'+*','+')
         CALL REPSTR(work,'C*','C')
         IF(work.EQ.'%B-'.OR.work.EQ.'%B+'.OR.work.EQ.'%EC') GO TO 20
         IF(work.EQ.'%B-~'.OR.work.EQ.'%B+~'.OR.work.EQ.'%EC~') GO TO 20
         CALL REPSTR(work,'*','$')
         CALL REPSTR(work,'G0','G')
         CALL REPSTR(work,'G#','G')
         CALL REPSTR(work,'G','$')
         CALL REPSTR(work,'/',':')
         CALL REPSTR(work,'WIDTH$','?')
         CALL REPSTR(work,'WIDTH','?')
         CALL REPSTR(work,'?:?','?')
         IF(work.EQ.'?') GO TO 20
!
!        E CARDS HAVE SOME EXTRA CHECKS.
!
      ELSE IF(RTYpe.EQ.carde) THEN
         CALL REPCHR(work,shell,'@@@@@@')
         CALL REPSTR(work,'@+','@')
         IF(work.EQ.'@'.OR.work(1:2).EQ.'@/')                           &
     &      CALL WRTMSG('<W> OBSOLETE FORMALISM - SEE MANUAL',From,To)
         CALL REPSTR(work,'EC@','$')
         CALL REPSTR(work,'$/$','?')
         CALL REPSTR(work,'$:$','$')
         CALL REPSTR(work,'$:$','$')
         CALL REPSTR(work,'$','?')
         CALL REPSTR(work,'C@','@')
         CALL REPSTR(work,'@/T','?')
         CALL REPSTR(work,'@','?')
         IF(work.EQ.'?') THEN
            IF(TOTal) CALL WRTMSG('<E> No EC for total ionization',From,&
     &                           To)
            GO TO 20
         END IF
!
!        G CARDS HAVE SOME EXTRA CHECKS.
!
      ELSE IF(RTYpe.EQ.cardg) THEN
!        Allow IPC
         If(work .EQ. 'IPC')GoTo 20
!        Allow DCO
         IF(work.EQ.'DCO') GO TO 20
!        DO NOT PROCESS M SHELL IF BM FORM.
         IF(work(1:1).EQ.'B') GO TO 5
!        CHECK FOR VALIDITY AND REDUCE SHELL/SUBSHELL CODES.
!        Check for sum of shells - e.g. K/LMN (TWB. 930323)
         k = INDEX(work,'/')
         IF(k.GT.1) THEN
            i = INDEX(shell,work(1:1))
            IF(i.GT.0) THEN
               DO j = i + 1, LEN(shell)
                  IF(work(2:2).EQ.'+') EXIT
                  IF(work(2:2).EQ.shell(j:j)) THEN
                     CALL DELSTR(work,2,1)
                     k = k - 1
                  END IF
               END DO
            END IF
            k = k + 1
            i = INDEX(shell,work(1:1))
            IF(LEN_TRIM(work)-k.GT.0.AND.i.GT.0) THEN
               k = k + 1
               DO j = i + 1, LEN(shell)
                  IF(work(k:k).EQ.'+') EXIT
                  IF(work(k:k).EQ.shell(j:j)) CALL DELSTR(work,k,1)
               END DO
            END IF
         END IF
         i = 0
         DO WHILE (.TRUE.)
            i = BREAK(work,i+1,shell)
            IF(i.LE.71) THEN
               j = SPAN(work,i+1,adigits)
               IF(j-i.EQ.3) THEN
!                 TWO DIGIT SUBSHELL.
                  d1 = work(i+1:i+1)
                  d2 = work(i+2:i+2)
                  IF(d1.LT.d2) THEN
                     work(i+1:i+1) = d2
                     work(i+2:i+2) = '#'
                     j = j - 1
                  END IF
               END IF
               IF(j-i.EQ.2) THEN
!                 ONE DIGIT SUBSHELL OR SECOND DIGIT OF TWO DIGIT
!                 SUBSHELL.
                  ch = work(i:i)
                  d1 = work(i+1:i+1)
                  DO k = 2, nndig
                     IF(ch.EQ.shell(k:k)) THEN
                        IF(IDIG(d1).LE.ndig(k)) work(i+1:i+1) = '#'
                     END IF
                  END DO
               END IF
!              NOW CONVERT VALID SHELL TO '@'.
               CALL REPCHR(work(i:i),shell,'@@@@@@@@')
!              LOOK FOR MORE SHELLS.
               CYCLE
            END IF
            EXIT
         END DO
!        REPLACE LOOSE DIGITS WITH # FOR BE#[W], BM#[W].
    5    CALL REPCHR(work,adigits,'#########')
         CALL REPSTR(work,'#W','#')
         IF(work.EQ.'BE#'.OR.work.EQ.'BM#') GO TO 20
!        Allow angular distributions
         IF(work.EQ.'A#') GO TO 20
!        NOW TRY FOR WIDTH... FORMS.
         CALL REPSTR(work,'/W',':W')
         CALL REPSTR(work,'G0','G')
         CALL REPSTR(work,'HG','H')
         CALL REPSTR(work,'WIDTH','W')
         CALL REPSTR(work,'W:W','W')
         IF(work.EQ.'W') GO TO 20
!        NOW THAT WE HAVE @[#[#]] START REDUCING FORMS.
         CALL SQZSTR(work,'(')
         CALL SQZSTR(work,')')
         CALL REPSTR(work,'CC','@')
         CALL REPSTR(work,'T/','T')
         CALL REPSTR(work,'/T','T')
         CALL REPSTR(work,'/',':')
         CALL REPSTR(work,'##','#')
         CALL REPSTR(work,'@#','@')
         CALL REPSTR(work,'@','*')
         Call Repstr(work,'%CE*','?')
         CALL REPSTR(work,'CE*','?')
         CALL REPSTR(work,'?+','?')
         CALL REPSTR(work,'*C','$')
         CALL REPSTR(work,'E$','$')
         CALL REPSTR(work,'*','$')
         CALL REPSTR(work,'+$',':$')
         CALL REPSTR(work,'$+','$')
         CALL REPSTR(work,'$T','?')
         CALL REPSTR(work,'T$','?')
         CALL REPSTR(work,'$:$','$')
         CALL REPSTR(work,'$:$','$')
         CALL REPSTR(work,'$','?')
!        FINALLY, CHECK THAT THE ONLY THING LEFT IS '?'.
         IF(work.EQ.'?') THEN
            IF(TOTal) THEN
               IF(DECtyp.NE.4.OR.SUBshe.EQ.' ') THEN
                  CALL WRTMSG('<E> No CC for total ionization',From,To)
               ELSE
                  work = Field
                  CALL REPSTR(work,SUBshe,'@')
                  CALL REPSTR(work,SUBshe(1:1),'@')
                  CALL REPSTR(work,'CC','@')
                  CALL REPSTR(work,'@','*')
                  CALL REPSTR(work,'CE*','?')
                  IF(work.NE.'*'.OR.work.NE.'?') THEN
                     messag = '<E> Only'
                     CALL ADDSTR(messag,LEN_TRIM(messag)+2,SUBshe)
                     CALL ADDSTR(messag,LEN_TRIM(messag)+1,' electrons')
                     CALL WRTMSG(messag,From,To)
                  END IF
               END IF
            END IF
            GO TO 20
         END IF
      END IF
!
!     IF WE HAVEN'T RETURNED YET, INVALID FIELD NAME.
!
   10 CALL WRTMSG('<E> INVALID FIELD NAME',From,To)
!
!     RESTORE D CARD RTYPE IF NEEDED.
!
   20 IF(RTYpe.EQ.9) RTYpe = 11
!
      RETURN
      END SUBROUTINE CKSYM
!
!***********************************************************************
!
      SUBROUTINE CKT(Field,From,To,Type)
!
!     CHECKS THE VALIDITY OF HALF-LIFE FIELD.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Field, Type
      INTEGER(KIND=4) :: From, To
!
!     Functions used
!
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
      INTEGER(KIND=4), INTRINSIC :: LEN_TRIM
      INTEGER(KIND=4), EXTERNAL :: INDEXF, IVLSTR, RLSCN, SPAN
!
!     Local variables
!
      CHARACTER(LEN=38) :: message
      INTEGER(KIND=4) :: i, l, r
      REAL(KIND=4) :: value
!
!     CHECK FOR BLANK FIELD.
!
      r = LEN_TRIM(Field)
      IF(r.EQ.0) RETURN
!
!     CHECK FOR 'STABLE'.
!
      l = SPAN(Field(1:r),1,' ')
      IF(Field(l:r).EQ.'STABLE') RETURN
!
!     CHECK NUMERIC VALUE.
!
      i = RLSCN(Field(1:r),l,value)
      IF(i.EQ.l) THEN
         message = ' INVALID HALF-LIFE'
         CALL ADDSTR(message,1,TRIM(Type))
         CALL WRTMSG(message,From,To)
         RETURN
      END IF
      l = i
!
!     CHECK FOR FINAL ?.
!
      IF(Field(r:r).EQ.'?') THEN
         r = r - 1
         r = LEN_TRIM(Field(1:r))
      END IF
!
!     CHECK FOR UNITS.
!
      IF(l.GT.r) THEN
         message = ' MISSING UNIT SUB-FIELD'
         CALL ADDSTR(message,1,TRIM(Type))
         CALL WRTMSG(message,From-1+r,From-1+r)
         RETURN
      END IF
      i = SPAN(Field(1:r),l,' ')
      IF(i.GT.l+1) THEN
         message = ' MORE THAN 1 BLANK BEFORE UNIT INVALID'
         CALL ADDSTR(message,1,TRIM(Type))
         CALL WRTMSG(message,From,To)
      ELSE IF(i.EQ.l) THEN
         message = ' BLANK REQUIRED BETW''N VALUE & UNIT'
         CALL ADDSTR(message,1,TRIM(Type))
         CALL WRTMSG(message,From,To)
      END IF
      l = i
      IF(Field(l:r).EQ.'KEV') THEN
      ELSE IF(Field(l:r).EQ.'MEV') THEN
      ELSE IF(Field(l:r).EQ.'MS') THEN
      ELSE IF(Field(l:r).EQ.'US') THEN
      ELSE IF(Field(l:r).EQ.'NS') THEN
      ELSE IF(Field(l:r).EQ.'PS') THEN
      ELSE IF(Field(l:r).EQ.'FS') THEN
      ELSE IF(Field(l:r).EQ.'AS') THEN
      ELSE IF(Field(l:r).EQ.'EV') THEN
      ELSE IF(Field(l:r).EQ.'Y') THEN
      ELSE IF(Field(l:r).EQ.'D') THEN
      ELSE IF(Field(l:r).EQ.'H') THEN
      ELSE IF(Field(l:r).EQ.'M') THEN
      ELSE IF(Field(l:r).NE.'S') THEN
         message = ' INVALID UNITS'
         CALL ADDSTR(message,1,TRIM(Type))
         CALL WRTMSG(message,From-1+l,From-1+r)
         RETURN
      END IF
!
      RETURN
      END SUBROUTINE CKT
!
!***********************************************************************
!
      SUBROUTINE CKU(Field,From)
!
!     CHECKS THE VALIDITY OF UNCERTAINTY FIELDS.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Field
      INTEGER(KIND=4) :: From
!
!     Functions used
!
      INTEGER(KIND=4), INTRINSIC :: INDEX, LEN, LEN_TRIM
      INTEGER(KIND=4), EXTERNAL :: INTSCN, SPAN, Typstr
!
!     Local variables
!
      INTEGER(KIND=4) :: i, l, r, value
!
!     CHECK FOR BLANK FIELD.
!
      r = LEN_TRIM(Field)
      IF(r.EQ.0) RETURN
!
!     SKIP LEADING BLANKS.
!
      l = 1
      l = SPAN(Field(1:r),l,' ')
!
!     CHECK FOR LITERAL UNCERTAINTY.
!
      IF(Field(l:r).EQ.'LT') THEN
      ELSE IF(Field(l:r).EQ.'GT') THEN
      ELSE IF(Field(l:r).EQ.'LE') THEN
      ELSE IF(Field(l:r).EQ.'GE') THEN
      ELSE IF(Field(l:r).EQ.'AP') THEN
      ELSE IF(Field(l:r).EQ.'CA') THEN
      ELSE IF(Field(l:r).EQ.'SY') THEN
!
!        CHECK FOR ASYMMETRIC UNCERTAINTY. (only for the large field)
!
      ELSE IF(LEN(Field).GT.2.AND.                                      &
     &        (Field(l:l).EQ.'+'.OR.Field(l:l).EQ.'-')) THEN
         If(Typstr(Field(l+1:l+1)) .NE. 1)Then
	    CALL WRTMSG('<E> INVALID UNCERTAINTY',From-1+l,From-1+r)
         ElseIf(Field(l:l).EQ.'+' .AND. INDEX(Field,'-').LE.0)Then
	    CALL WRTMSG('<E> INVALID UNCERTAINTY',From-1+l,From-1+r)
         ElseIf(Field(l:l).EQ.'-' .AND. INDEX(Field,'+').LE.0)Then
	    CALL WRTMSG('<E> INVALID UNCERTAINTY',From-1+l,From-1+r)
	 EndIf
         i = INTSCN(Field(1:r),l,.TRUE.,value)
         IF(i.EQ.l) THEN
            CALL WRTMSG('<E> INVALID UNCERTAINTY',From-1+i,From-1+r)
         ELSE
            l = i
            i = INTSCN(Field(1:r),l,.TRUE.,value)
            IF(i.LE.r) THEN
               CALL WRTMSG('<E> INVALID UNCERTAINTY',From-1+i,From-1+r)
            END IF
         END IF
!
!        CHECK FOR SYMMETRIC UNCERTAINTY.
!
      ELSE
         i = INTSCN(Field(1:r),l,.FALSE.,value)
         IF(i.EQ.l) THEN
            CALL WRTMSG('<E> INVALID UNCERTAINTY',From-1+i,From-1+r)
         ELSE IF(i.LE.r) THEN
            CALL WRTMSG('<E> JUNK AT END OF FIELD',From-1+i,From-1+r)
	 ElseIf(value .EQ. 0)Then
            CALL WRTMSG('<E> UNCERTAINTY OF ZERO',From,                 &
     &        From+LEN_TRIM(Field))
         END IF
      END IF
!
      RETURN
      END SUBROUTINE CKU
!
!***********************************************************************
!
      SUBROUTINE CKUN(Field,From,To)
!
!     CHECKS THE VALIDITY OF UNIQUENESS FIELD.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Field
      INTEGER(KIND=4) :: From, To
!
!     Functions used
!
      INTEGER(KIND=4), EXTERNAL :: TYPSTR
!
      IF(Field.EQ.'  ') THEN
      ELSE IF(Field(2:2).EQ.'U') THEN
         IF(TYPSTR(Field(1:1)).NE.1) THEN
            CALL WRTMSG('<E> MUST BE DIGIT',From,From)
         END IF
      ELSE IF(Field(2:2).EQ.' ') THEN
         IF(TYPSTR(Field(1:1)).NE.1) THEN
            CALL WRTMSG('<E> MUST BE DIGIT',From,From)
         END IF
      ELSE
         CALL WRTMSG('<E> INVALID UN FIELD',From,To)
      END IF
!
      RETURN
      END SUBROUTINE CKUN
!
!***********************************************************************
!
      SUBROUTINE CKV (FIELD, FROM, CHKEXP, type)
!
!     CKV: CHECKS THE VALIDITY OF UNSIGNED NUMERIC FIELDS.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Field, Type
      LOGICAL(KIND=4) :: Chkexp
      INTEGER(KIND=4) :: From
!
      CALL CLDZERO(Field,From,Type)
      CALL CKVSUB(.FALSE.,.FALSE.,Field,From,Chkexp,Type)
!
      RETURN
      END SUBROUTINE CKV
!
!***********************************************************************
!
      SUBROUTINE CKVSGN(Field,From,Chkexp,Type)
!
!     CKVSGN: CHECKS THE VALIDITY OF SIGNED NUMERIC FIELDS.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Field, Type
      LOGICAL(KIND=4) :: Chkexp
      INTEGER(KIND=4) :: From
!
      CALL CLDZERO(Field,From,Type)
      CALL CKVSUB(.FALSE.,.TRUE.,Field,From,Chkexp,Type)
!
      RETURN
      END SUBROUTINE CKVSGN
!
!***********************************************************************
!
      SUBROUTINE CKVPRN(Field,From,Chkexp,Type)
!
!     CKVPRN: CHECKS THE VALIDITY OF UNSIGNED NUMERIC FIELDS,
!           WHICH MAY BE ENCLOSED IN PARENTHESES.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Field, Type
      LOGICAL(KIND=4) :: Chkexp
      INTEGER(KIND=4) :: From
!
      CALL CLDZERO(Field,From,Type)
      CALL CKVSUB(.TRUE.,.FALSE.,Field,From,Chkexp,Type)
!
      RETURN
      END SUBROUTINE CKVPRN
!
!***********************************************************************
!
      SUBROUTINE CKVPS(Field,From,Chkexp,Type)
!
!     CKVPS:  CHECKS THE VALIDITY OF   SIGNED NUMERIC FIELDS,
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Field, Type
      LOGICAL(KIND=4) :: Chkexp
      INTEGER(KIND=4) :: From
!
      CALL CKVSUB(.TRUE.,.TRUE.,Field,From,Chkexp,Type)
!
      RETURN
      END SUBROUTINE CKVPS
!
!***********************************************************************
!
      SUBROUTINE CKVSUB(Parens,Signed,Field,From,Chkexp,Type)
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Field, Type
      LOGICAL(KIND=4) :: Chkexp, Parens, Signed
      INTEGER(KIND=4) :: From
!
!     Functions used
!
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
      INTEGER(KIND=4), INTRINSIC :: INDEX, LEN_TRIM
      INTEGER(KIND=4), EXTERNAL :: INDEXF, IVLSTR, RLSCN, SPAN
!
!     Local variables
!
      CHARACTER(LEN=38) :: message
      INTEGER(KIND=4) :: l, m, r
      REAL(KIND=4) :: value
!
!     CHECK FOR BLANK FIELD.
!
   10 r = LEN_TRIM(Field)
      IF(r.EQ.0) RETURN
!
!     SKIP LEADING BLANKS.
!
      l = 1
      l = SPAN(Field(1:r),l,' ')
      If(INDEX(field(l:r),' ') .GT. 0)Then
         message=' EMBEDDED BLANKS'
         CALL ADDSTR(message,1,TRIM(Type))
         CALL WRTMSG(message,From-1+l,From-1+r)
      EndIf
	 
!
!     LOOK FOR PARENTHESES AND SET L, R TO SKIP THEM.
!
      IF(parens) THEN
         IF(Field(l:l).EQ.'(') THEN
            l = l + 1
            IF(Field(r:r).NE.')') THEN
               message = ' UNBALANCED PARENTHESES'
               CALL ADDSTR(message,1,TRIM(Type))
               CALL WRTMSG(message,From-1+l,From-1+r)
            ELSE
               r = r - 1
            END IF
         END IF
      END IF
!
!     LOOK FOR LEADING SIGN AND SET L TO SKIP IT.
!
      IF(signed) THEN
         IF(Field(l:l).EQ.'+'.OR.Field(l:l).EQ.'-') THEN
            l = l + 1
!           Sign by itself is not a valid numeric value (TWB. 930326)
            IF(l.GT.r) THEN
               message = ' INVALID NUMERIC VALUE'
               CALL ADDSTR(message,1,TRIM(Type))
               CALL WRTMSG(message,From-2+l,From-1+r)
               RETURN
            END IF
         END IF
      ELSE
        IF(Field(l:l).EQ.'+'.OR.Field(l:l).EQ.'-') THEN
           message = ' UNSIGNED NUMBER EXPECTED'
           CALL ADDSTR(message,1,TRIM(Type))
           CALL WRTMSG(message,From-1+l,From-1+l)
           l = l + 1
        END IF
      END IF
!
!     FIND UNSIGNED NUMERIC VALUE.
!
      
      m = INDEXF(Field(1:r),l,'E')
      IF(m.GT.0.AND.m.LE.r-2.AND.Chkexp) THEN
         m = m + 1
         IF(Field(m:m).EQ.'+') m = m + 1
         IF(Field(m:m).EQ.'-') THEN
            m = m + 1
            message = 'EXPONENT TOO SMALL'
         ELSE
            message = 'EXPONENT TOO LARGE'
         END IF
         IF(IVLSTR(Field(m:r)).GE.10) THEN
            IF(IVLSTR(Field(m:r)).LE.30) THEN
               CALL ADDSTR(message,1,'<W> ')
               CALL WRTMSG(TRIM(message),From-1+m,From-1+r)
            ELSE
               CALL ADDSTR(message,1,TRIM(Type))
               CALL WRTMSG(TRIM(message),From-1+m,From-1+r)
               RETURN
            END IF
         END IF
      END IF
      l = RLSCN(Field(1:r),l,value)
      IF(l.LE.r) THEN
         message = ' INVALID NUMERIC VALUE'
         CALL ADDSTR(message,1,TRIM(Type))
         CALL WRTMSG(message,From-1+l,From-1+r)
      END IF
!
      RETURN
      END SUBROUTINE CKVSUB
!
!***********************************************************************
!
      SUBROUTINE CKVU(Fieldv,Fieldu,From,To)
!
!     IF FIELD V IS BLANK, FIELD U SHOULD BE BLANK
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Fieldu, Fieldv
      INTEGER(KIND=4) :: From, To
!
!     Functions used
!
      INTEGER(KIND=4), INTRINSIC :: LEN
      INTEGER(KIND=4), EXTERNAL :: TYPSTR
!
!     Also check for possible typo error
!
      If(fieldv(LEN(fieldv):LEN(fieldv)).EQ.'.' .AND.                   &
     &  Typstr(Fieldu(1:1)).EQ. 1)                                      &
     &  Call Wrtmsg('<W> Possible typing error',from+9,from+9)
!
      IF(Fieldv.EQ.' '.AND.Fieldu.NE.' ')Then
         CALL WRTMSG('<E> IF VALUE BLANK THEN UNCERT. BLANK',From,To)
	 Return
      EndIf
      If(Typstr(Fieldv).EQ.2.AND. Fieldu.NE.' ')Then      
         CALL WRTMSG('<E> IF VALUE UNKNOWN THEN UNCERT. BLANK',From,To)
	 Return
      EndIf
!
      RETURN
      END SUBROUTINE CKVU
!
!***********************************************************************
!
      LOGICAL(KIND=4) FUNCTION CKXANY(String,From)
!
!     SCANS STRING TO VERIFY THAT ALL CHARACTERS ARE VALID XREF FLAG
!         CHARACTERS. THIS MEANS THAT '()+-*,' ARE INVALID.
!         '$' IS ALSO INVALID BUT WON'T OCCUR HERE SINCE IT WAS STRIPPED
!         OUT BY A HIGHER LEVEL SUBROUTINE.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: String
      INTEGER(KIND=4) :: From
!
!     Functions used
!
      INTEGER(KIND=4), INTRINSIC :: INDEX, LEN
!
!     Local variables
!
      CHARACTER(LEN=1) :: ch
      INTEGER(KIND=4) :: i, j
!
!     SET DEFAULT VALUE.
!
      CKXANY = .TRUE.
!
!     SCAN AND CHECK FOR THE FIRST INVALID CHARACTER.
!
      DO j = 1, LEN(String)
         ch = String(j:j)
         i = INDEX('()+-*,',ch)
         IF(i.GT.0) THEN
            CKXANY = .FALSE.
            RETURN
         ELSE
            i = INDEX(XREfs,ch)
            IF(i.EQ.0) THEN
               CALL WRTMSG('<E> UNDEFINED XREF CHAR.',From-1+j,From-1+j)
            END IF
         END IF
      END DO
!
      RETURN
      END FUNCTION CKXANY
!
!***********************************************************************
!
      SUBROUTINE CKXREF(Field,From,To,Conflct)
!
!     CHECKS XREF FIELD ON 2L CARD.
!
!     FORMS ARE:
!      1. +              MUST STAND ALONE
!      2. -(L[L]...)     MUST STAND ALONE
!      3. L[(*)]         3 AND 4 MAY BE REPEATED IN ANY ORDER
!      4. L(E[,E]...)
!
!     L IS ANY CHARACTER EXCEPT '(', ')', '+', '-', '*', AND ','.
!     E IS ANY ENERGY (SEE CKE FOR FORMAT CHECKING) AND MAY
!          OPTIONALLY BE PRECEDED BY AN ASTERISK '*'.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Field
      LOGICAL(KIND=4) :: Conflct
      INTEGER(KIND=4) :: From, To
!
!     Functions used
!
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
      INTEGER(KIND=4), INTRINSIC :: LEN, LEN_TRIM
      INTEGER(KIND=4), EXTERNAL :: BREAK, INDEXF
!
!     Local variables
!
      CHARACTER(LEN=66) :: blank, tmpstr
      CHARACTER(LEN=93) :: tmpstr2
      LOGICAL(KIND=4) :: invert
      INTEGER(KIND=4) :: i, j, l, ltmpstr, r
!
!     Store XREF without qualifiers for later use
      IF(LVLexc.OR.NFLvl.EQ.0) GO TO 10
      IF(Conflct) THEN
         CALL WRTMSG('<W> XREF info not retained',From,To)
         GO TO 10
      END IF
      blank = ' '
      tmpstr = Field
      IF(tmpstr(1:1).EQ.'-') THEN
         invert = .TRUE.
         IF(tmpstr(2:2).EQ.'(') THEN
            tmpstr(2:) = tmpstr(3:)
            ltmpstr = LEN_TRIM(tmpstr)
            IF(tmpstr(ltmpstr:ltmpstr).EQ.')') THEN
               tmpstr(ltmpstr:ltmpstr) = ' '
            END IF
         END IF
      ELSE
         invert = .FALSE.
      END IF
      IF(LEN_TRIM(tmpstr).GT.0) THEN
         i = 1
         DO WHILE (i.LE.LEN_TRIM(tmpstr))
            IF(tmpstr(i:i).EQ.'(') THEN
               j = INDEXF(tmpstr,i,')')
               IF(j.LE.0) THEN
                  i = i + 1
               ELSE IF(j.LT.LEN_TRIM(tmpstr)) THEN
                  tmpstr(i:) = tmpstr(j+1:)
               ELSE
                  tmpstr(i:) = ' '
                  EXIT
               END IF
            ELSE
               i = i + 1
            END IF
         END DO
      END IF
      IF(invert) THEN
         tmpstr2 = XREfs
         CALL REPCHR(tmpstr2,TRIM(tmpstr),blank(1:LEN_TRIM(tmpstr)))
         CALL SQZSTR(tmpstr2,' ')
         IF(LEN_TRIM(tmpstr).LE.LEN(FXRef(NFLvl))) THEN
            FXRef(NFLvl) = tmpstr2
         ELSE
            FXRef(NFLvl) = '+'
         END IF
      ELSE
         FXRef(NFLvl) = tmpstr
      END IF
!
!     CHECK FOR '+' ONLY.
!
   10 IF(Field(1:1).EQ.'+') THEN
         IF(LEN(Field).GT.1) CALL WRTMSG('<E> JUNK AT END OF FIELD',    &
     &                                  From+1,To)
         IF(.NOT.LVLexc) FXRef(NFLvl) = '+'
         RETURN
      END IF
!
!     CHECK FOR '-(L[L]...)'.
!
      IF(Field(1:1).EQ.'-') THEN
         IF(Field(2:2).NE.'(') THEN
            CALL WRTMSG('<E> MISSING "("',From+1,From+1)
            RETURN
         END IF
         i = BREAK(Field,3,')')
         IF(i.GT.LEN(Field)) CALL WRTMSG('<E> MISSING ")"',From+2,To)
         IF(.NOT.CKXANY(Field(3:i-1),From+2))                           &
     &      CALL WRTMSG('<E> INVALID XREF FIELD',From+2,From-2+i)
         RETURN
      END IF
!
!     CHECK FOR OTHER FORMS.
!
      l = 1
      r = LEN_TRIM(Field)
      DO WHILE (.TRUE.)
         IF(l.GT.r) THEN
            RETURN
         END IF
         IF(.NOT.CKXANY(Field(l:l),From-1+l)) THEN
            CALL WRTMSG('<E> INVALID XREF CHARACTER',From-1+l,From-1+l)
            RETURN
         ELSE
            l = l + 1
            IF(l.GT.r) RETURN
            IF(Field(l:l).EQ.'(') THEN
               l = l + 1
               i = INDEXF(Field,l,')')
               IF(i.EQ.0) THEN
                  CALL WRTMSG('<E> MISSING ")"',From-1+l,To)
                  RETURN
               END IF
               IF(Field(l:i-1).NE.'*') THEN
                  DO WHILE (.TRUE.)
                     j = INDEXF(Field,l,',')
                     IF(0.LT.j.AND.j.LT.i) THEN
                        IF(Field(l:l).EQ.'*') l = l + 1
                        CALL CKE(Field(l:j-1),From-1+l,From-2+j,'<E>')
                        l = j + 1
                        CYCLE
                     END IF
                     IF(Field(l:l).EQ.'*') l = l + 1
                     If(field(i-1:i-1).EQ.'?')Then
                        If(LEN_TRIM(field(l:i-1)).GT.1)Call                    &
     &                    CKE(Field(l:i-2),From-1+l,From-3+i,'<E>')
                     Else
                        CALL CKE(Field(l:i-1),From-1+l,From-2+i,'<E>')
                     EndIf
                     EXIT
                  END DO
               END IF
               l = i + 1
            END IF
         END IF
      END DO
!
      RETURN
      END SUBROUTINE CKXREF
!
!***********************************************************************
!
      SUBROUTINE CRDTYP(Card,Rectyp,Comnt,Contin)
!
!     CHECKS CARD TO FIND OUT WHAT TYPE OF RECORD IT IS.
!        THREE VALUES ARE RETURNED:
!           RECTYP: THE RECORD TYPE
!           COMNT:  WHETHER OR NOT THE CARD IS A COMMENT CARD
!           CONTIN: WHETHER THE CARD IS THE FIRST CARD OF THIS TYPE
!                   OR A CONTINUATION CARD.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=80) :: Card
      INTEGER(KIND=4) :: Comnt, Rectyp
      LOGICAL(KIND=4) :: Contin
!
!     Functions used
!
      INTEGER(KIND=4), INTRINSIC :: INDEX
!
!     Local variables
!
      CHARACTER(LEN=1) :: khar
      CHARACTER(LEN=*), PARAMETER :: rt = 'NPQLABEGRXD~SH'
!
!     CHECK FOR CONTINUATION CARD, NON-' ' OR NON-'1' IN COLUMN 6.
!
      khar = Card(6:6)
      Contin = .TRUE.
      CONtyp = 1
      IF(khar.EQ.' '.OR.khar.EQ.'1') THEN
         Contin = .FALSE.
         CONtyp = 0
      END IF
!
!     CHECK FOR COMMENT CARD, 'C', 'T', OR 'D' IN COLUMN 7.
!
      khar = Card(7:7)
      IF(khar.EQ.' ') THEN
         Comnt = 0
      ELSE IF((khar.EQ.'C').OR.(khar.EQ.'c')) THEN
         Comnt = 1
      ELSE IF((khar.EQ.'T').OR.(khar.EQ.'t')) THEN
         Comnt = 2
      ELSE IF((khar.EQ.'D').OR.(khar.EQ.'d')) THEN
         Comnt = 3
      ELSE IF((khar.EQ.'U').OR.(khar.EQ.'u')) THEN
         Comnt = 4
      ELSE IF(khar.EQ.'P'.AND.Card(8:8).NE.'N') THEN
         Comnt = 5
      ELSE IF(khar.EQ.'P'.AND.Card(8:8).EQ.'N') THEN
         Comnt = 0
      ELSE
         Comnt = -1
      END IF
!     Check for obsolete formalism for COMMENT
      IF(Comnt.GE.1.AND.Comnt.LE.4.AND.Card(6:6).EQ.'1'.AND..NOT.NOWarn)&
     &   THEN
         CALL WRTCRD(Card)
         CALL WRTMSG('<W>  OBSOLESCENT FORMALISM - USE " "',6,6)
      END IF
!
!     CHECK FOR RECORD TYPE.
!
      khar = Card(8:8)
!     LOOK FOR ONE OF THE VALID TABLE ENTRIES.
      Rectyp = INDEX(rt,khar)
      IF(Rectyp.EQ.12) THEN
         Rectyp = -1
         RTYpe = -1
         RETURN
      END IF
!
!     Also have to handle prompt decay
      IF(khar.EQ.' ') THEN
         IF(Card(9:9).EQ.'N'.OR.Card(9:9).EQ.'P'.OR.Card(9:9).EQ.'D'.OR.&
     &      Card(9:9).EQ.'T'.OR.Card(9:9).EQ.'A') Rectyp = 11
      END IF
!
!     Check for comment/primary conflict
      IF(.NOT.GABeld.AND.PRType.NE.Rectyp.AND.Rectyp.GT.0.AND.          &
     &   (Rectyp.LE.3.OR.Rectyp.EQ.12).AND.Comnt.GT.0) THEN
         IF(.NOT.(PRType.EQ.1.AND.Rectyp.EQ.12).AND.                    &
     &      .NOT.(PRType.EQ.12.AND.Rectyp.EQ.1)) THEN
            CALL WRTCRD(Card)
            CALL WRTMSG('<F> COMMENT/PRIMARY CARD TYPE CONFLICT',8,9)
         END IF
      ELSE IF(GABeld.AND.PRType.NE.Rectyp.AND.Comnt.GT.0) THEN
         CALL WRTCRD(Card)
         CALL WRTMSG('<F> COMMENT/PRIMARY CARD TYPE CONFLICT',8,9)
      END IF
!
!     but for PN type, check card 7:7
!
      IF(Rectyp.EQ.1.AND.Card(7:7).EQ.'P') Rectyp = 12
!
      RTYpe = Rectyp
!
!     Modified logic below (2/24/89)
!
      IF(Rectyp.NE.0) THEN
         PRVtyp = PRType
         PRType = RTYpe
         IF(.NOT.Contin.AND.Comnt.EQ.0) THEN
            IF(RTYpe.EQ.8) GCOunt = 1
         ELSE
            IF(RTYpe.EQ.8) GCOunt = GCOunt + 1
         END IF
         RETURN
      END IF
!
!     NOT STANDARD RECORD TYPE, CHECK FOR SPECIALS.
!
      IF(Card(1:8).EQ.' ') THEN
         Rectyp = 99
         RTYpe = 99
         PRVtyp = PRType
         PRType = 99
      ELSE IF(khar.EQ.' ') THEN
         IF(Card(9:9).EQ.' ') THEN
            Rectyp = 0
            RTYpe = 0
            PRVtyp = PRType
            PRType = 0
         ELSE
!           PARTICLE (NON-DELAYED) RECORD.
            Rectyp = 11
            RTYpe = 11
            PRVtyp = PRType
            IF(.NOT.Contin) PRType = 11
         END IF
      ELSE
         Rectyp = -1
         RTYpe = -1
      END IF
!
      RETURN
      END SUBROUTINE CRDTYP
!
!***********************************************************************
!
      SUBROUTINE NUCID(Str,A,Z)
!
!     SCANS STR LOOKING FOR A VALID NUCLEUS ID (A 0-3 DIGIT MASS
!       FOLLOWED BY A 0-2 LETTER (OR DIGIT FOR LARGE Z) ELEMENT
!        SYMBOL).
!        RETURNS THE A AND Z FOUND, OR, IF PROBLEMS, ONE OF:
!            A =  0 => NOT GIVEN  Z = -1 => ERROR
!             -2 => NOT GIVEN
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      INTEGER(KIND=4) :: A, Z
      CHARACTER(LEN=*) :: Str
!
!     Functions used
!
      INTEGER(KIND=4), INTRINSIC :: LEN_TRIM
      INTEGER(KIND=4), EXTERNAL :: INTSCN, SPAN
!
!     Local variables
!
      CHARACTER(LEN=2) :: sym
      INTEGER(KIND=4) :: l, r
!
!     Initialize message of obsolete formalism
      IZLmsg = ' '
!
!     SET VALID STRING BOUNDS.
!
      l = 1
      r = LEN_TRIM(Str)
      IF(r.LE.0) THEN
         A = 0
         Z = -1
         RETURN
      END IF
      l = SPAN(Str(1:r),l,' ')
!
!     FIND LEADING NUMERIC VALUE, STORE AS A.
!
      l = INTSCN(Str(1:r),l,.FALSE.,A)
!
!     ALL NUMERIC FIELD.
!
      IF(l.LE.r) THEN
         sym = Str(l:r)
         CALL IZEL(sym,Z)
         IF(Z.EQ.7.AND.(0.LT.A.AND.A.LT.10)) THEN
            Z = 0
            IZLmsg = 'Obsolete formalism. Use NN'
         END IF
      ELSE IF(A.LT.1000) THEN
!        NO Z GIVEN, SIGNAL THAT.
         Z = -2
      ELSE
!        HIGH Z, SPLIT OFF A AND Z.
         A = A/100
         sym = Str(r-1:r)
         CALL IZEL(sym,Z)
!
!        NON-NUMERIC TRAILER, CONVERT ELEMENT SYMBOL TO Z.
!
!        'N' IS USED FOR BOTH NITROGEN AND NEUTRON.
!        RESOLVE THE DIFFERENCE USING A.
      END IF
!
      RETURN
      END SUBROUTINE NUCID
!
!
!***********************************************************************
!
      SUBROUTINE FNUCID(Str,A,Z)
!
!     SCANS STR LOOKING FOR A VALID NUCLEUS ID (A 0-3 DIGIT MASS
!       FOLLOWED BY A 0-2 LETTER (OR DIGIT FOR LARGE Z) ELEMENT
!        SYMBOL).
!        RETURNS THE A AND Z FOUND, OR, IF PROBLEMS, ONE OF:
!            A =  0 => NOT GIVEN  Z = -1 => ERROR
!             -2 => NOT GIVEN
!     This is supposed to be called only for the first 5 column 
!     check.  We want to make sure things are the correct format
!     Further, this makes sure that we have numbers in columns 
!     1-3 and a valid nuc in 4-5-- TDJ
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      INTEGER(KIND=4) :: A, Z
      CHARACTER(LEN=*) :: Str
!
!     Functions used
!
      INTEGER(KIND=4), INTRINSIC :: LEN_TRIM
      INTEGER(KIND=4), EXTERNAL :: INTSCN, SPAN
!
!     Local variables
!
      CHARACTER(LEN=2) :: sym
      INTEGER(KIND=4) :: l, r
!
!     Initialize message of obsolete formalism
      IZLmsg = ' '
!
!     SET VALID STRING BOUNDS.
!
      l = 1
      r = LEN_TRIM(Str)
      IF(r.LE.0) THEN
         A = 0
         Z = -1
         RETURN
      END IF
      l = SPAN(Str(1:r),l,' ')
!
!     FIND LEADING NUMERIC VALUE, STORE AS A.
!
      l = INTSCN(Str(1:3),l,.FALSE.,A)
!
!     ALL NUMERIC FIELD.
!
      IF(l.LE.r) THEN
         sym = Str(l:r)
         CALL IZEL(sym,Z)
         IF(Z.EQ.7.AND.(0.LT.A.AND.A.LT.10)) THEN
            Z = 0
            IZLmsg = 'Obsolete formalism. Use NN'
         END IF
      ELSE IF(A.LT.1000) THEN
!        NO Z GIVEN, SIGNAL THAT.
         Z = -2
      ELSE
!        HIGH Z, SPLIT OFF A AND Z.
         A = A/100
         sym = Str(r-1:r)
         CALL IZEL(sym,Z)
!
!        NON-NUMERIC TRAILER, CONVERT ELEMENT SYMBOL TO Z.
!
!        'N' IS USED FOR BOTH NITROGEN AND NEUTRON.
!        RESOLVE THE DIFFERENCE USING A.
      END IF
!
      RETURN
      END SUBROUTINE FNUCID
!

!***********************************************************************
!
      SUBROUTINE WRTCHK(Nlines)
!
!     COUNT LINES AS WRITTEN TO THE REPORT FILE.
!        IF OVER 60 LINES, EJECT A PAGE AND WRITE NEW HEADING.
!        IF NLINES NEGATIVE, CHECK -NLINES LINES BUT COUNT 1 LINE.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      INTEGER(KIND=4) :: Nlines
!
!     Functions used
!
      CHARACTER(LEN=1), INTRINSIC :: CHAR
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
!
!     Local variables
!
      INTEGER(KIND=4) :: line
      DATA line/60/
!
!     CHECK FOR END OF PAGE.
!
      IF(line+ABS(Nlines).GT.58) THEN
         IF(PAGe.NE.0) WRITE(out,'(/9X,2A)')                            &
     &                      '....:....1....:....2....:....3....:....4', &
     &                      '....:....5....:....6....:....7....:....8'
         PAGe = PAGe + 1
         IF(PAGe.GT.1) WRITE(out,'(A)') CHAR(12)
         WRITE(out,'(A,15X,A,10X,3A,T118,A,I4)')                        &
     &         'EVALUATED NUCLEAR STRUCTURE DATA FILE', 'SYNTAX CHECK', &
     &         version, ' AS OF ', verdate, 'PAGE ', PAGe
         WRITE(out,'(/9X,2A)')'....:....1....:....2....:....3....:....4'&
     &                        ,                                         &
     &                        '....:....5....:....6....:....7....:....8'
         line = 3
      END IF
!
!     INCREMENT LINE COUNT BY NUMBER OF LINES PASSED TO ROUTINE.
!
      IF(Nlines.GT.0) THEN
         line = line + Nlines
      ELSE
         line = line + 1
      END IF
!
      RETURN
      END SUBROUTINE WRTCHK
!
!***********************************************************************
!
      SUBROUTINE WRTCRD(Card)
!
!     IF FULL REPORT, WRITE CARD TO REPORT FILE.
!
!     IF ERROR ONLY REPORT:
!       IF CARD NOT ALL BLANK, SAVE IN KARD.
!       ELSE WRITE KARD TO REPORT (PREVIOUSLY SAVED).
!       CARD ALL BLANK IS SENT FROM WRTMSG.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=80) :: Card
!
!     Local variables
!
      CHARACTER(LEN=80), SAVE :: kard
!
      IF(.NOT.(ERPt)) THEN
!        FULL REPORT, WRITE CARD.
         NSEq = NSEq + 1
         CALL WRTCHK(-3)
         WRITE(out,'(1X,I5,2A)') NSEq, '.  ', Card
         CRDwrt = .TRUE.
!        ERROR ONLY REPORT.
      ELSE IF(Card.NE.' ') THEN
!        SAVE FOR POSSIBLE FUTURE USE.
         kard = Card
         NSEq = NSEq + 1
      ELSE
!        THE FUTURE IS NOW.
         IF(LEVseq.EQ.NSEq.OR.LEVstr.EQ.' ') THEN
            CALL WRTCHK(-3)
!           SEE IF THERE ARE 3 LINES, BUT COUNT 1 LINE.
         ELSE
            CALL WRTLEV
         END IF
         WRITE(out,'(1X,I5,2A)') NSEq, '.  ', kard
         CRDwrt = .TRUE.
      END IF
!
      RETURN
      END SUBROUTINE WRTCRD
!
!***********************************************************************
!
      SUBROUTINE WRTDSI(Card)
!
!     WRITE DATA SET ID CARD TO REPORT FILE.
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=80) :: Card
!
      NSEq = 1
      CALL WRTCHK(5)
      WRITE(out,'(/9X,2A)')'---------+---------+-+-------+-+-------+-+',&
     &                     '-------+-----+------+-+---------+-++-+'
      WRITE(out,'(/I6,2A)') NSEq, '.  ', Card
!
!     SET FLAG THAT THE CARD HAS BEEN WRITTEN.
!
      CRDwrt = .TRUE.
!     Print to terminal or LOG file summary of errors and warnings
!     (TWB. 930323)
      IF(TOTfat.GT.0) THEN
         WRITE(idefo,'(/1X,I4,A)') TOTfat, ' fatal errors reported'
      END IF
      IF(TOTerr.GT.0) THEN
         IF(.NOT.NOErr) THEN
            WRITE(idefo,'(1X,I4,A)') TOTerr, ' error(s) reported'
         ELSE
            WRITE(idefo,'(1X,I4,A)') TOTerr, ' error(s) suppressed'
         END IF
      END IF
      IF(TOTwar.GT.0) THEN
         IF(.NOT.NOWarn) THEN
            WRITE(idefo,'(1X,I4,A)') TOTwar, ' warning(s) reported'
         ELSE
            WRITE(idefo,'(1X,I4,A)') TOTwar, ' warning(s) suppressed'
         END IF
      END IF
!     TELL USER THE DATA SET WE ARE PROCESSING.
      WRITE(idefo,'(/1X,A80)') Card
      TOTwar = 0
      TOTerr = 0
      TOTfat = 0
!
      RETURN
      END SUBROUTINE WRTDSI
!
!***********************************************************************
!
      SUBROUTINE WRTMSG(Msg,Kolfrm,Kolto)
!
!     AN ERROR HAS BEEN ENCOUNTERED:
!       1. IF ERPT IS TRUE AND THE CARD IMAGE HAS NOT BEEN WRITTEN,
!           CALL WRTCRD WITH NULL CARD AS FLAG TO WRITE SAVED CARD IMAGE
!           AT THIS TIME.
!       2. WRITE OUT THE SPECIFIED ERROR MESSAGE.
!       3. IF KOLFRM AND KOLTO ARE NOT ZERO (0),
!            FLAG THE CARD IMAGE FIELD WITH STARS (*).
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Msg
      INTEGER(KIND=4) :: Kolfrm, Kolto
!
!     Functions used
!
      INTEGER(KIND=4), INTRINSIC :: INDEX
!
!     Local variables
!
      CHARACTER(LEN=80) :: flag
      INTEGER(KIND=4) :: kol
!
!     BLANK OUT FIELD FLAGGING STRING.
      flag = ' '
!     CHECK IF WE SHOULD WRITE THE CARD FIRST.
      IF(ERPt.AND..NOT.CRDwrt) THEN
         IF(INDEX(Msg,'<F>').EQ.1.OR.                                   &
     &      (INDEX(Msg,'<E>').EQ.1.AND..NOT.NOErr).OR.INDEX(Msg,'<I>')  &
     &      .EQ.1.OR..NOT.NOWarn) THEN
            CALL WRTCRD(flag)
         END IF
      END IF
!     FLAG IS BEING USED AS ARGUMENT SINCE IT IS ' '*80.
!     IF KOLFRM, KOLTO IN BOUNDS, SET FLAG DATA
      IF(1.LE.Kolfrm.AND.Kolfrm.LE.Kolto) THEN
         IF(Kolto.LE.80) THEN
            DO kol = Kolfrm, Kolto
               flag(kol:kol) = '*'
            END DO
         END IF
      END IF
!     WRITE FLAGS AND MESSAGE.
      IF(INDEX(Msg,'<W>').EQ.1) THEN
         IF(.NOT.NOWarn) THEN
            CALL WRTCHK(1)
            WRITE(out,'(9X,A,2X,A)') flag, Msg
         END IF
         TOTwar = TOTwar + 1
      ELSE IF(INDEX(Msg,'<E>').EQ.1) THEN
         IF(.NOT.NOErr) THEN
            CALL WRTCHK(1)
            WRITE(out,'(9X,A,2X,A)') flag, Msg
         END IF
         TOTerr = TOTerr + 1
      ELSE
         CALL WRTCHK(1)
         WRITE(out,'(9X,A,2X,A)') flag, Msg
      END IF
      IF(INDEX(Msg,'<F>').EQ.1) TOTfat = TOTfat + 1
!
      RETURN
      END SUBROUTINE WRTMSG
!
!***********************************************************************
!
      LOGICAL(KIND=4) FUNCTION RECCHK(Crdtmp)
!
!     Subroutine to check length of the record and special characters
!     Changed to logical for easy checking in MAIN (TWB. 930323)
!     Returns true if any non-printable characters except NUL found
!     in first clen characters
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Crdtmp
!
!     Functions used
!
      CHARACTER(LEN=1), INTRINSIC :: CHAR
      INTEGER(KIND=4), INTRINSIC :: ICHAR, INDEX, MIN0, LEN_TRIM
!
!     Local variables
!
      CHARACTER(LEN=3) :: cclen
      CHARACTER(LEN=26) :: messag
      LOGICAL(KIND=4) :: first
      INTEGER(KIND=4) :: badcnt, i, j, k, n
!
      INTEGER(KIND=4), PARAMETER :: clen=80
      CHARACTER(LEN=clen) :: crdloc
      INTEGER(KIND=4), DIMENSION(clen) :: badloc, badsym
!
!     SPEC(I)  ITH ASCII CHARACTER
!
      CHARACTER(LEN=1), PARAMETER :: nul = CHAR(0)
      CHARACTER(LEN=4), DIMENSION(34) :: spec
      DATA spec/'NULL', 'SOH', 'STX', 'ETX', 'EOT', 'ENQ', 'ACK', 'BEL',&
     &     'BS', 'HT', 'LF', 'VT', 'FF', 'CR', 'SO', 'SI', 'DLE', 'DC1',&
     &     'DC2', 'DC3', 'DC4', 'NAK', 'SYN', 'ETB', 'CAN', 'EM', 'SUB',&
     &     'ESC', 'FS', 'GS', 'RS', 'US', 'DEL', '****'/
!
      badcnt = 0
      first = .TRUE.
      n = LEN_TRIM(Crdtmp)
      crdloc = Crdtmp
      RECCHK = .FALSE.
      IF(n.GT.0) THEN
         k = MIN0(clen,n)
!        Save symbols and locations of non-printable characters in first
!        80 columns (TWB. 930323)
         DO i = 1, k
            IF(ICHAR(Crdtmp(i:i)).GT.126.OR.ICHAR(Crdtmp(i:i)).LT.32)   &
     &         THEN
               badcnt = badcnt + 1
               badloc(badcnt) = i
               badsym(badcnt) = ICHAR(Crdtmp(i:i))
               IF(badsym(badcnt).EQ.127) badsym(badcnt) = 32
               IF(badsym(badcnt).GT.127) badsym(badcnt) = 33
            END IF
         END DO
         IF(badcnt.GT.0) THEN
!           Replace non-printable characters with spaces and set RECCHK
!           to TRUE (TWB. 930323)
            DO i = 1, badcnt
               crdloc(badloc(i):badloc(i)) = ' '
               IF(badsym(i).NE.0) RECCHK = .TRUE.
            END DO
            IF(.NOT.NOErr) CALL WRTCRD(crdloc)
            first = .FALSE.
!           Indicate position and symbols (TWB. 930323)
            DO i = 1, badcnt
               messag = '<E> NONPRINTABLE CHAR: '
               CALL ADDSTR(messag,LEN_TRIM(messag)+1,spec(badsym(i)+1))
               CALL WRTMSG(messag,badloc(i),badloc(i))
            END DO
         END IF
         IF(n.GT.clen) THEN
            IF(first) THEN
               IF(.NOT.NOWarn) CALL WRTCRD(crdloc)
               first = .FALSE.
            END IF
            WRITE(cclen,'(I3)') clen
            CALL LBSUP(cclen)
            CALL WRTMSG('<E> LONGER THAN '//TRIM(cclen)//' CHAR',       &
     &                   clen,clen)
            DO i = clen+1, n
               IF(ICHAR(Crdtmp(i:i)).GT.126.OR.ICHAR(Crdtmp(i:i)).LT.32)&
     &            THEN
                  j = ICHAR(Crdtmp(i:i))
                  IF(j.EQ.127) j = 32
                  IF(j.GT.127) j = 33
                  messag = '<W> NONPRINTABLE CHAR: '
                  CALL ADDSTR(messag,LEN_TRIM(messag)+1,spec(j+1))
                  IF(first) THEN
                     IF(.NOT.NOWarn) CALL WRTCRD(crdloc)
                     first = .FALSE.
                  END IF
                  CALL WRTMSG(messag,clen,clen)
               END IF
            END DO
         END IF
      END IF
!
!     make sure this record is not null filled
!
      IF(n.LT.clen) THEN
         j = INDEX(Crdtmp(n+1:clen),nul)
         IF(j.NE.0) THEN
!           Write out temporary record if not done already (TWB. 930323)
            IF(first.AND..NOT.NOErr) CALL WRTCRD(Crdtmp(1:clen))
            CALL WRTMSG('<E> NUL CHARACTER FOUND',j,j)
         END IF
      END IF
!
      RETURN
      END FUNCTION RECCHK
!
!***********************************************************************
!
      SUBROUTINE CKNCID(Card)
!
!     Checks the nucleus ID for errors and consistency with DSID record
!     nucleus ID
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Card
!
!     Local variables
!
      CHARACTER(LEN=5) :: string, tmpnuc
      CHARACTER(LEN=38) :: message
      INTEGER(KIND=4) :: a, i, z
!
      string = Card(1:5)
!      CALL NUCID(string,a,z)
      CALL FNUCID(string,a,z)
      If(a .EQ. 0)Then
         CALL WRTMSG('<F> INVALID NUCID',1,5)
      ElseIf(z.LT.0 .AND. (dstype.GT.0 .AND. dstype.LT.4)) THEN
         CALL WRTMSG('<F> INVALID NUCID',1,5)
      ELSE
         IF(IZLmsg.NE.' ') THEN
            CALL REPSTR(IZLmsg,'Obsolete formalism','Obsol. form')
            CALL ADDSTR(IZLmsg,1,'<W> ')
            CALL WRTMSG(TRIM(IZLmsg),4,5)
         END IF
         IF(RTYpe.EQ.2) THEN
            tmpnuc=string
	    Call Lbsup(tmpnuc)
            IF(NTOtsf.EQ.0) THEN
               IF(tmpnuc.NE.PARnucid)                                   &
     &            CALL WRTMSG('<E> NUCID MISMATCH WITH DSID PARENT',1,5)
            ELSE
               message = '<E> NUCID MISMATCH WITH DSID PARENT'
               DO i = 1, NTOtsf
                  IF(tmpnuc.EQ.SFPar(i)) message = ' '
               END DO
               IF(message.NE.' ') CALL WRTMSG(message,1,5)
            END IF
         Else
            IF(a.NE.DSA.OR.z.NE.DSZ)                                    &
     &        CALL WRTMSG('<F> NUCID DOESN''T MATCH DSID',1,5)
         EndIf
      END IF
!
      RETURN
      END SUBROUTINE CKNCID
!
!***********************************************************************
!
      SUBROUTINE CKPUNC(String,Ends)
!
!     Checks for illegal punctuation characters embedded in the DSID
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: String
      INTEGER(KIND=4) :: Ends
!
!     Functions used
!
      INTEGER(KIND=4), INTRINSIC :: INDEX, LEN
      INTEGER(KIND=4), EXTERNAL :: INDEXF
!
!     Local variables
!
      INTEGER(KIND=4) :: i, j, parbeg, parend
!
      INTEGER(KIND=4), PARAMETER :: npunc = 10
      CHARACTER(LEN=npunc) :: cpunc = '!"#$%&*/;@'
!
      Ends = LEN(String)
      DO i = 1, Ends
         IF(INDEX(cpunc,String(i:i)).GT.0) THEN
            CALL WRTMSG('<E> INVALID CHARACTER',9+i,9+i)
            CALL DELSTR(String,i,1)
            Ends = Ends - 1
         END IF
      END DO
!     Special handling of "?" due to half-lifes
      i = INDEX(String,'?')
   10 IF(i.EQ.0) RETURN
      parbeg = INDEX(String,'(')
      DO j = Ends, parbeg, -1
         IF(String(j:j).EQ.')') THEN
            parend = j
            EXIT
         END IF
      END DO
      IF(i.LT.parbeg.OR.i.GT.parend) THEN
         CALL WRTMSG('<E> INVALID CHARACTER',9+i,9+i)
         CALL DELSTR(String,i,1)
         i = INDEXF(String,i,'?')
         GO TO 10
      END IF
      IF(i.EQ.LEN(String)) THEN
         CALL WRTMSG('<E> INVALID CHARACTER',9+i,9+i)
         CALL DELSTR(String,i,1)
      ELSE IF(String(i+1:i+1).EQ.'+'.OR.String(i+1:i+1).EQ.')') THEN
         i = INDEXF(String,i+1,'?')
         GO TO 10
      ELSE
         CALL WRTMSG('<E> INVALID CHARACTER',9+i,9+i)
         CALL DELSTR(String,i,1)
         i = INDEXF(String,i,'?')
         GO TO 10
      END IF
!
      RETURN
      END SUBROUTINE CKPUNC
!
!***********************************************************************
!
      SUBROUTINE WRTMS2(Msg,Kolfr1,Kolto1,Kolfr2,Kolto2)
!
!     An error has been encountered in the relationship between two
!     fields of a record:
!       1. If ERPT is TRUE and the card image has not been written,
!          call WRTCRD with NULL CARD as flag to write saved card image
!          at this time.
!       2. Write out the specified error message.
!       3. If KOLFR1 and KOLTO1 and KOLFR2 and KOLTO1 are not zero (0),
!          flag the card image field with stars (*).
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Msg
      INTEGER(KIND=4) :: Kolfr1, Kolfr2, Kolto1, Kolto2
!
!     Functions used
!
      INTEGER(KIND=4), INTRINSIC :: INDEX
!
!     Local variables
!
      CHARACTER(LEN=80) :: flag
      INTEGER(KIND=4) :: kol
!
!     BLANK OUT FIELD FLAGGING STRING.
      flag = ' '
!     CHECK IF WE SHOULD WRITE THE CARD FIRST.
      IF(ERPt.AND..NOT.CRDwrt) THEN
         IF(INDEX(Msg,'<F>').EQ.1.OR.                                   &
     &      (INDEX(Msg,'<E>').EQ.1.AND..NOT.NOErr).OR.INDEX(Msg,'<I>')  &
     &      .EQ.1.OR..NOT.NOWarn) THEN
            CALL WRTCRD(flag)
         END IF
      END IF
!     FLAG IS BEING USED AS ARGUMENT SINCE IT IS ' '*80.
!     IF KOLFRM, KOLTO IN BOUNDS, SET FLAG DATA
      IF(1.LE.Kolfr1.AND.Kolfr1.LE.Kolto1) THEN
         IF(Kolto1.LE.80) THEN
            DO kol = Kolfr1, Kolto1
               flag(kol:kol) = '*'
            END DO
         END IF
      END IF
      IF(Kolto1.LE.Kolfr2.AND.Kolfr2.LE.Kolto2) THEN
         IF(Kolto2.LE.80) THEN
            DO kol = Kolfr2, Kolto2
               flag(kol:kol) = '*'
            END DO
         END IF
      END IF
!     WRITE FLAGS AND MESSAGE.
      IF(INDEX(Msg,'<W>').NE.0) THEN
         IF(.NOT.NOWarn) THEN
            CALL WRTCHK(1)
            WRITE(out,'(9X,A,2X,A)') flag, Msg
         END IF
         TOTwar = TOTwar + 1
      ELSE IF(INDEX(Msg,'<E>').NE.0) THEN
         IF(.NOT.NOErr) THEN
            CALL WRTCHK(1)
            WRITE(out,'(9X,A,2X,A)') flag, Msg
         END IF
         TOTerr = TOTerr + 1
      ELSE
         CALL WRTCHK(1)
         WRITE(out,'(9X,A,2X,A)') flag, Msg
      END IF
      IF(INDEX(Msg,'<F>').EQ.1) TOTfat = TOTfat + 1
!
      RETURN
      END SUBROUTINE WRTMS2
!
!***********************************************************************
!
      SUBROUTINE CHKDEX(Lcard,Lseq)
!
!     Checks deexciting gammas for ambiguous final level, multiple
!     gamma's of same energy from one level, and order of gammas.
!     Also issues a warning if OPTION=6 for intensities, an RI has
!     been found and no RI=100 found
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Lcard
      INTEGER(KIND=4) :: Lseq
!
!     Functions used
!
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
      INTEGER(KIND=4), INTRINSIC :: INDEX, LEN, MAX0, LEN_TRIM
      INTEGER(KIND=4), EXTERNAL :: BREAK, SPAN
      REAL(KIND=4), INTRINSIC :: FLOAT, SQRT
      REAL(KIND=4), EXTERNAL :: VALSTR
!
!     Local variables
!
      CHARACTER(LEN=1) :: lvlchr
      CHARACTER(LEN=38) :: msg
      CHARACTER(LEN=5) :: number
      CHARACTER(LEN=2) :: sdlvl
      CHARACTER(LEN=10) :: slvl, srange, tmpstr
      CHARACTER(LEN=34) :: strout
      CHARACTER(LEN=80) :: card
      LOGICAL(KIND=4) :: doexcl
      INTEGER(KIND=4) :: c1, c2, i, is, j, k, l, lncnt, lvl1, lvl2,     &
     &                   lvlt, m, maxcnt, namb, o1, o2, savlev, thecnt
      REAL(KIND=4) :: cleve, dgam, dleve, dsavlvl, emass, flmax, flmin, &
     &                gam, grec, leve, rrange, savlvl
!
      INTEGER(KIND=4), PARAMETER :: maxamb = 10
      INTEGER(KIND=4), DIMENSION(maxamb) :: amblv
      LOGICAL(KIND=4), DIMENSION(maxamb) :: excl
      CHARACTER(LEN=maxdxg) :: store
      LOGICAL(KIND=4), DIMENSION(maxdxg) :: did1, did2
      CHARACTER(LEN=10), DIMENSION(maxlvl) :: agamma
!
      emass = 931501.6*FLOAT(DSA)
      DO i = 1, NFLvl
         agamma(i) = ' '
      END DO
!
      IF(CHGbr.AND.RIFnd.AND.NOBr100) THEN
         TOTwar = TOTwar + 1
         IF(.NOT.NOWarn) THEN
            CALL WRTCHK(-2)
            WRITE(out,'(1X,I5,2A)') Lseq, '.  ', Lcard
            WRITE(out,'(T92,A)')'<W> NO RI=100 FOR G''s FROM LVL'
         END IF
      END IF
!
!     Cannot be more deexciting gammas than there are levels to feed
      IF(NDXg.GT.1) THEN
         store = DXGcol9(1)
         j = 1
         DO i = 2, NDXg
            IF(INDEX(store,DXGcol9(i)).EQ.0) THEN
               j = j + 1
               CALL ADDSTR(store,j,DXGcol9(i))
            END IF
         END DO
         IF(LEN_TRIM(store).LE.1) THEN
            maxcnt = NDXg
         ELSE
            maxcnt = 0
            DO i = 1, j
               thecnt = 0
               DO k = 1, NDXg
                  IF(store(i:i).EQ.DXGcol9(k)) thecnt = thecnt + 1
               END DO
               maxcnt = MAX0(maxcnt,thecnt)
            END DO
         END IF
      ELSE
         maxcnt = NDXg
      END IF
      IF(maxcnt.GT.NFLvl-1) THEN
         IF(.NOT.NOErr) THEN
            CALL WRTCHK(-2)
            WRITE(out,'(1X, I5, 2A)') Lseq, '.  ', Lcard
            msg = '<E> MORE G''s('
            CALL NUMSTR(NDXg,number)
            CALL LBSUP(number)
            msg(LEN_TRIM(msg)+1:) = number
            msg(LEN_TRIM(msg)+1:) = ') THAN LVL('
            CALL NUMSTR(NFLvl-1,number)
            CALL LBSUP(number)
            msg(LEN_TRIM(msg)+1:) = number
            msg(LEN_TRIM(msg)+1:) = ') TO FEED'
            CALL WRTCHK(1)
            WRITE(out,'(T92,A)') msg
         END IF
         TOTerr = TOTerr + 1
      END IF
      IF(NFLvl.LE.1) GO TO 30
      IF(INDEX(FLVl(NFLvl),'SN')+INDEX(FLVl(NFLvl),'SP').GT.0) GO TO 20
      DO i = 1, NDXg
         IF(DXFlv(i).NE.' ') THEN
            DO j = 1, NFLvl - 1
               IF(DXFlv(i).EQ.FLVl(j)) THEN
                  IF(agamma(j).EQ.' ') THEN
                     agamma(j) = DXGe(i)
                  END IF
               END IF
            END DO
         END IF
      END DO
      DO i = 1, NDXg
!        Check for ambiguity in final levels and possible bad energy
!        mismatch
         IF(DXFlv(i).EQ.' ') THEN
            CALL CNVS2U(FLVl(NFLvl),DFLvl(NFLvl),leve,dleve)
            lvlchr = ' '
            IF(NOFf.GT.0) THEN
               DO j = 1, NOFf
                  IF(INDEX(FLVl(NFLvl),OFFchr(j)).NE.0) THEN
                     leve = leve + OFFamt(j)
                     lvlchr = OFFchr(j)
                  END IF
               END DO
            END IF
            CALL CNVS2U(DXGe(i),DXGde(i),gam,dgam)
!           Check for correspondence to non-numeric levels and set
!           gamma energy to level offset
            IF(gam.EQ.0.) THEN
               DO j = 1, NOFf
                  IF(INDEX(DXGe(i),OFFchr(j)).GT.0) THEN
                     gam = OFFamt(j)
                     GO TO 5
                  END IF
               END DO
               CALL WRTCHK(-2)
               READ(UNIT=tmp,FMT='(A)',REC=RECnum(i)) card
               tmpstr = card(10:19)
               CALL LBSUP(tmpstr)
               IF(BREAK(tmpstr,1,'+123456789').LE.LEN_TRIM(tmpstr)) THEN
                  IF(.NOT.NOErr) THEN
                     WRITE(out,'(1X, I5, 2A)') RECdxg(i), '.  ', card
                     CALL WRTCHK(1)
                     WRITE(out,'(18X,A,T92,A)') '**********',           &
     &                     '<E> UNASSIGNED CHARACTER FOUND'
                  END IF
                  TOTerr = TOTerr + 1
               ElseIf(.NOT.((tmpstr.GE.'A' .AND. tmpstr.LE.'Z')         &
     &           .OR. (tmpstr.GE.'a' .AND. tmpstr.LE.'z')))Then
                  TOTErr = TOTErr + 1
                  IF(.NOT.NOErr) THEN
                     WRITE(out,'(1X, I5, 2A)') RECdxg(i), '.  ', card
                     CALL WRTCHK(1)
                     WRITE(out,'(18X,A,T92,A)') '**********',           &
     &                     '<E> ZERO ENERGY GAMMA FOUND'
                  END IF
               END IF
               GoTo 100
            END IF
    5       IF(DXGde(i)(1:1).EQ.'L') THEN
               gam = gam/2.
               dgam = gam
            END IF
            grec = gam**2/(2.0*emass)
            savlvl = leve - (gam+grec)
            dsavlvl = SQRT(dleve**2+dgam**2)
            flmin = leve - dleve - (gam+grec+dgam)
            flmax = leve + dleve - (gam+grec-dgam)
            doexcl = .FALSE.
            namb = 0
            DO j = 1, NFLvl - 1
               CALL CNVS2U(FLVl(j),DFLvl(j),leve,dleve)
               DO k = 1, NOFf
                  IF(INDEX(FLVl(j),OFFchr(k)).NE.0) leve = leve +       &
     &               OFFamt(k)
               END DO
               IF((leve-dleve).GT.flmax.AND..NOT.BADord) EXIT
               IF(((leve-dleve).GE.flmin.AND.(leve-dleve).LE.flmax).OR. &
     &            (leve.GE.flmin.AND.leve.LE.flmax).OR.                 &
     &            ((leve+dleve).GE.flmin.AND.(leve+dleve).LE.flmax))THEN
                  savlev = j
                  IF(namb.LT.maxamb.AND.INDEX(FLVl(j),lvlchr).NE.0) THEN
                     namb = namb + 1
                     amblv(namb) = j
                     excl(namb) = .FALSE.
                     IF(FXRef(NFLvl).NE.' '.AND.FXRef(j).NE.' '.AND.    &
     &                  FXRef(NFLvl).NE.'+'.AND.FXRef(j).NE.'+') THEN
                        excl(namb) = .TRUE.
                        DO k = 1, LEN_TRIM(FXRef(NFLvl))
                           IF(INDEX(FXRef(j),FXRef(NFLvl)(k:k)).NE.0)   &
     &                        THEN
                              excl(namb) = .FALSE.
                              EXIT
                           END IF
                        END DO
                        doexcl = doexcl.OR.excl(namb)
                     END IF
                  END IF
               END IF
            END DO
            IF(namb.GT.1) THEN
               TOTerr = TOTerr + 1
               IF(.NOT.NOErr) THEN
                  READ(UNIT=tmp,FMT='(A)',REC=RECnum(i)) card
                  lncnt = 4 + namb
                  IF(doexcl) lncnt = lncnt + 1
                  CALL WRTCHK(-lncnt)
                  WRITE(out,'(1X, I5, 2A)')RECdxg(i), '.  ', card
                  CALL WRTCHK(1)
                  WRITE(out,'(18X,A,T92,A)') '**********',              &
     &                  '<E> AMBIGUOUS FINAL LVL. USE "FL="'
                  CALL MAKEEJ(strout,FLVl(NFLvl),DFLvl(NFLvl),          &
     &                        FJPi(NFLvl),FBAnd(NFLvl))
                  CALL WRTCHK(1)
                  WRITE(out,'(T84,2A)') 'Parent: ',TRIM(strout)
                  IF(lvlchr.EQ.' ') THEN
                     CALL CNVU2S(savlvl,dsavlvl,slvl,10,sdlvl,2)
                     CALL LBSUP(slvl)
                  ELSE
                     DO m = 1, NOFf
                        IF(OFFchr(m).EQ.lvlchr) THEN
                           CALL CNVU2S(savlvl-OFFamt(m),dsavlvl,slvl,10,&
     &                                 sdlvl,2)
                           CALL LBSUP(slvl)
                           CALL ADDSTR(slvl,LEN_TRIM(slvl)+1,           &
     &                                 '+'//lvlchr)
                        END IF
                     END DO
                  END IF
                  CALL LBSUP(sdlvl)
                  CALL MAKEEJ(strout,slvl,sdlvl,' ',' ')
                  CALL WRTCHK(1)
                  WRITE(out,'(T83,2A)') 'FL(cal): ',TRIM(strout)
                  DO j = 1, namb
                     IF(.NOT.excl(j)) THEN
                        CALL MAKEEJ(strout,FLVl(amblv(j)),              &
     &                              DFLvl(amblv(j)),FJPi(amblv(j)),     &
     &                              FBAnd(amblv(j)))
                        CALL WRTCHK(1)
                        WRITE(out,'(T92,A)') strout
                     END IF
                  END DO
                  IF(doexcl) THEN
                     CALL WRTCHK(1)
                     WRITE(out,'(T92,A)') 'Excluded by XREF:'
                     DO j = 1, namb
                        IF(excl(j)) THEN
                           CALL MAKEEJ(strout,FLVl(amblv(j)),           &
     &                                 DFLvl(amblv(j)),FJPi(amblv(j)),  &
     &                                 FBAnd(amblv(j)))
                           CALL WRTCHK(1)
                           WRITE(out,'(T92,A)') strout
                        END IF
                     END DO
                  END IF
               END IF
            ELSE IF(namb.EQ.0) THEN
               IF(dgam.EQ.0.0) THEN
                  dgam = 1.0
                  is = 1
               ELSE
                  is = 2
               END IF
               DO j = is, 5
                  l = j
                  flmin = flmin - dgam
                  flmax = flmax + dgam
                  DO k = 1, NFLvl - 1
                     CALL CNVS2U(FLVl(k),DFLvl(k),leve,dleve)
                     DO m = 1, NOFf
                        IF(INDEX(FLVl(k),OFFchr(m)).NE.0) leve = leve + &
     &                     OFFamt(m)
                     END DO
                     IF((leve-dleve).GT.flmax) THEN
                        IF(.NOT.BADord.OR.k.EQ.NFLvl-1) EXIT
!                       If out of order check next level energy before
!                       quiting
                        cleve = VALSTR(FLVl(k+1))
                        DO m = 1, NOFf
                           IF(INDEX(FLVl(k+1),OFFchr(m)).NE.0)          &
     &                        cleve = cleve + OFFamt(m)
                        END DO
                        IF(cleve.GE.leve) EXIT
                     END IF
                     IF(((leve-dleve).GE.flmin.AND.(leve-dleve).LE.flmax&
     &                  ).OR.(leve.GE.flmin.AND.leve.LE.flmax).OR.      &
     &                  ((leve+dleve).GE.flmin.AND.(leve+dleve)         &
     &                  .LE.flmax)) GO TO 10
                  END DO
               END DO
               rrange = flmax - (flmax+flmin)/2. - dgam
               CALL CNVU2S(rrange,0.0,srange,10,sdlvl,-2)
               CALL LBSUP(srange)
               msg = '<E> NO FINAL LVL WITHIN'
               WRITE(msg(LEN_TRIM(msg)+2:),'(A)') srange
               msg(LEN_TRIM(msg)+2:) = 'keV'
               GO TO 15
   10          IF(l.LE.3) THEN
                  CYCLE
               END IF
               rrange = flmax - (flmax+flmin)/2. - dgam
               CALL CNVU2S(rrange,0.0,srange,10,sdlvl,-2)
               CALL LBSUP(srange)
               msg = '<W> NO FINAL LVL WITHIN'
               WRITE(msg(LEN_TRIM(msg)+2:),'(A)') srange
               msg(LEN_TRIM(msg)+2:) = 'keV'
   15          lvl1 = NFLvl - 1
               DO j = 1, NFLvl - 1
                  k=INDEX(flvl(j),'+')
                  If(k .EQ. 0)Then
                     leve = VALSTR(FLVl(j))
                  Else
                     If(flvl(j)(k-1:k-1) .EQ. 'E')Then
                        leve = VALSTR(FLVl(j))
                     ElseIf(flvl(j)(k-1:k-1).GE.'A'                     &
     &                 .AND. flvl(j)(k-1:k-1).LE.'Z')Then 
                        leve = VALSTR(FLVl(j)(k+1:))
                     Else
                        leve = VALSTR(FLVl(j)(1:k-1))
                     EndIf
                  EndIf
                  IF(NOFf.GT.0) THEN
                     DO k = 1, NOFf
                        IF(INDEX(FLVl(j),OFFchr(k)) .NE. 0)Then         &
                           leve = leve + OFFamt(k)
                        EndIf
                     END DO
                  END IF
                  IF(leve.GE.savlvl) THEN
                     lvl1 = j
                     EXIT
                  END IF
               END DO
               lvl2 = 1
               DO j = NFLvl - 1, 1, -1
                  k=INDEX(flvl(j),'+')
                  If(k .EQ. 0)Then
                     leve = VALSTR(FLVl(j))
                  Else
                     If(flvl(j)(k-1:k-1) .EQ. 'E')Then
                        leve = VALSTR(FLVl(j))
                     ElseIf(flvl(j)(k-1:k-1).GE.'A'                     &
     &                 .AND. flvl(j)(k-1:k-1).LE.'Z')Then 
                        leve = VALSTR(FLVl(j)(k+1:))
                     Else
                        leve = VALSTR(FLVl(j)(1:k-1))
                     EndIf
                  EndIf
                  IF(NOFf.GT.0) THEN
                     DO k = 1, NOFf
                        IF(INDEX(FLVl(j),OFFchr(k)) .NE. 0)Then         &
                           leve = leve + OFFamt(k)
                        EndIf
                     END DO
                  END IF
                  IF(leve.LE.savlvl) THEN
                     lvl2 = j
                     EXIT
                  END IF
               END DO
               IF(lvl1.GT.lvl2) THEN
                  lvlt = lvl2
                  lvl2 = lvl1
                  lvl1 = lvlt
               END IF
               IF(lvl2.LT.NFLvl-1) THEN
                  cleve = VALSTR(FLVl(lvl2))
                  IF(NOFf.GT.0) THEN
                     DO j = 1, NOFf
                        IF(INDEX(FLVl(lvl2),OFFchr(k)).NE.0)            &
     &                     cleve = cleve + OFFamt(k)
                     END DO
                  END IF
                  DO j = lvl2 + 1, NFLvl - 1
                     leve = VALSTR(FLVl(j))
                     IF(NOFf.GT.0) THEN
                        DO k = 1, NOFf
                           IF(INDEX(FLVl(j),OFFchr(k)).NE.0)            &
     &                        leve = leve + OFFamt(k)
                        END DO
                     END IF
                     IF(leve.GT.cleve) THEN
                        EXIT
                     END IF
                     lvl2 = j
                     cleve = leve
                  END DO
               END IF
               IF(lvlchr.EQ.' ') THEN
                  CALL CNVU2S(savlvl,dsavlvl,slvl,10,sdlvl,2)
                  CALL LBSUP(slvl)
               ELSE
                  DO m = 1, NOFf
                     IF(OFFchr(m).EQ.lvlchr) THEN
                        CALL CNVU2S(savlvl-OFFamt(m),dsavlvl,slvl,10,   &
     &                              sdlvl,2)
                        CALL LBSUP(slvl)
                        CALL ADDSTR(slvl,LEN_TRIM(slvl)+1,'+'//lvlchr)
                     END IF
                  END DO
               END IF
               CALL LBSUP(slvl)
               IF(INDEX(msg,'<W>').NE.0) THEN
                  TOTwar = TOTwar + 1
                  IF(.NOT.NOWarn) THEN
                     lncnt = 6
                     lncnt = lncnt + lvl2 - lvl1
                     CALL WRTCHK(-lncnt)
                     READ(UNIT=tmp,FMT='(A)',REC=RECnum(i)) card
                     WRITE(out,'(1X, I5, 2A)') RECdxg(i), '.  ', card
                     CALL WRTCHK(1)
                     WRITE(out,'(18X,A,T92,A)') '**********',msg
                     CALL MAKEEJ(strout,FLVl(NFLvl),DFLvl(NFLvl),       &
     &                           FJPi(NFLvl),FBAnd(NFLvl))
                     CALL WRTCHK(1)
                     WRITE(out,'(T83,2A)') 'Parent:  ',TRIM(strout)
                     CALL MAKEEJ(strout,slvl,sdlvl,' ',' ')
                     CALL WRTCHK(1)
                     WRITE(out,'(T83,2A)') 'FL(cal): ',TRIM(strout)
                     CALL WRTCHK(1)
                     WRITE(out,'(T92,A)') 'Closest level(s):'
                     CALL MAKEEJ(strout,FLVl(lvl1),DFLvl(lvl1),         &
     &                           FJPi(lvl1),FBAnd(lvl1))
                     CALL WRTCHK(1)
                     WRITE(out,'(T92,A)') strout
                     IF(lvl1.NE.lvl2) THEN
                        DO j = lvl1 + 1, lvl2
                           CALL MAKEEJ(strout,FLVl(j),DFLvl(j),FJPi(j), &
     &                                 FBAnd(j))
                           CALL WRTCHK(1)
                           WRITE(out,'(T92,A)') strout
                        END DO
                     END IF
                  END IF
               ELSE
                  TOTerr = TOTerr + 1
                  IF(.NOT.NOErr) THEN
                     lncnt = 6
                     lncnt = lncnt + lvl2 - lvl1
                     CALL WRTCHK(-lncnt)
                     READ(UNIT=tmp,FMT='(A)',REC=RECnum(i)) card
                     WRITE(out,'(1X, I5, 2A)') RECdxg(i), '.  ', card
                     CALL WRTCHK(1)
                     WRITE(out,'(18X,A,T92,A)') '**********',msg
                     CALL MAKEEJ(strout,FLVl(NFLvl),DFLvl(NFLvl),       &
     &                           FJPi(NFLvl),FBAnd(NFLvl))
                     CALL WRTCHK(1)
                     WRITE(out,'(T83,2A)') 'Parent:  ',TRIM(strout)
                     CALL MAKEEJ(strout,slvl,sdlvl,' ',' ')
                     CALL WRTCHK(1)
                     WRITE(out,'(T83,2A)') 'FL(cal): ',TRIM(strout)
                     CALL WRTCHK(1)
                     WRITE(out,'(T92,A)')  'Closest level(s):'
                     CALL MAKEEJ(strout,FLVl(lvl1),DFLvl(lvl1),         &
     &                           FJPi(lvl1),FBAnd(lvl1))
                     CALL WRTCHK(1)
                     WRITE(out,'(T92,A)') strout
                     IF(lvl1.NE.lvl2) THEN
                        DO j = lvl1 + 1, lvl2
                           CALL MAKEEJ(strout,FLVl(j),DFLvl(j),FJPi(j), &
     &                                 FBAnd(j))
                           CALL WRTCHK(1)
                           WRITE(out,'(T92,A)') strout
                        END DO
                     END IF
                  END IF
               END IF
            ELSE IF(agamma(savlev).EQ.' ') THEN
               agamma(savlev) = DXGe(i)
            ELSE
               TOTerr = TOTerr + 1
               IF(.NOT.NOErr) THEN
                  msg = '<E>'
                  CALL ADDSTR(msg,LEN_TRIM(msg)+2,agamma(savlev))
                  CALL ADDSTR(msg,LEN_TRIM(msg)+2,'FEEDS')
                  CALL ADDSTR(msg,LEN_TRIM(msg)+2,FLVl(savlev))
                  READ(UNIT=tmp,FMT='(A)',REC=RECnum(i)) card
                  WRITE(out,'(1X, I5, 2A)')RECdxg(i), '.  ', card
                  CALL WRTCHK(1)
                  WRITE(out,'(18X,A,T92,A)') '**********',msg
               END IF
            END IF
         ELSE
            DO j = 1, NFLvl - 1
               IF(DXFlv(i).EQ.FLVl(j)) THEN
                  IF(agamma(j).NE.' '.AND.agamma(j).NE.DXGe(i)) THEN
                     TOTerr = TOTerr + 1
                     IF(.NOT.NOErr) THEN
                        msg = '<E>'
                        CALL ADDSTR(msg,LEN_TRIM(msg)+2,agamma(j))
                        CALL ADDSTR(msg,LEN_TRIM(msg)+2,'FEEDS')
                        CALL ADDSTR(msg,LEN_TRIM(msg)+2,FLVl(j))
                        READ(UNIT=tmp,FMT='(A)',REC=RECnum(i)) card
                        WRITE(out,'(1X, I5, 2A)')RECdxg(i), '.  ', card
                        CALL WRTCHK(1)
                        WRITE(out,'(18X,A,T92,A)') '**********',msg
                     END IF
                  END IF
               END IF
            END DO
         END IF
100      Continue
      END DO
!     Check for order of gammas and multiple gammas of same energy
   20 IF(NDXg.GT.1) THEN
         DO i = 1, NDXg
            did1(i) = .FALSE.
            did2(i) = .FALSE.
         END DO
         DO i = 1, NDXg - 1
            IF(LEN_TRIM(DXGe(i)).EQ.0) CYCLE
            IF(did1(i)) CYCLE
            READ(UNIT=tmp,FMT='(A)',REC=RECnum(i)) card
            DO j = i + 1, NDXg
               IF(LEN_TRIM(DXGe(j)).EQ.0) CYCLE
               IF(DXGcol9(i).NE.DXGcol9(j)) CYCLE
               o1 = INDEX(DXGe(j),'(')
               IF(o1.EQ.0) THEN
                  o1 = 1
               ELSE
                  o1 = o1 + 1
               END IF
               o2 = INDEX(DXGe(i),'(')
               IF(o2.EQ.0) THEN
                  o2 = 1
               ELSE
                  o2 = o2 + 1
               END IF
               c1 = INDEX(DXGe(j),')')
               IF(c1.EQ.0) THEN
                  c1 = LEN_TRIM(DXGe(j))
               ELSE
                  c1 = c1 - 1
               END IF
               c2 = INDEX(DXGe(i),')')
               IF(c2.EQ.0) THEN
                  c2 = LEN_TRIM(DXGe(i))
               ELSE
                  c2 = c2 - 1
               END IF
               o1 = SPAN(DXGe(j)(1:c1),o1,' ')
               o2 = SPAN(DXGe(i)(1:c2),o2,' ')
               IF(o1.GT.c1) o1 = 1
               IF(o2.GT.c2) o2 = 1
               IF(VALSTR(DXGe(j)(o1:c1)).LT.VALSTR(DXGe(i)(o2:c2))) THEN
                  IF(.NOT.NOErr) THEN
                     CALL WRTCHK(-2)
                     WRITE(out,'(1X, I5, 2A)') RECdxg(i), '.  ', card
                     CALL WRTCHK(1)
                     WRITE(out,'(18X,A,T92,A)') '**********',           &
     &                     '<E> GAMMA OUT OF ORDER'
                     card = ' '
                  END IF
                  did1(j) = .TRUE.
                  TOTerr = TOTerr + 1
                  EXIT
               END IF
            END DO
            IF(did2(i)) CYCLE
            namb = 0
            DO j = i + 1, NDXg
               IF(LEN_TRIM(DXGe(j)).EQ.0) CYCLE
               IF(DXGcol9(i).NE.DXGcol9(j)) CYCLE
               o1 = INDEX(DXGe(j),'(')
               IF(o1.EQ.0) THEN
                  o1 = 1
               ELSE
                  o1 = o1 + 1
               END IF
               o2 = INDEX(DXGe(i),'(')
               IF(o2.EQ.0) THEN
                  o2 = 1
               ELSE
                  o2 = o2 + 1
               END IF
               c1 = INDEX(DXGe(j),')')
               IF(c1.EQ.0) THEN
                  c1 = LEN_TRIM(DXGe(j))
               ELSE
                  c1 = c1 - 1
               END IF
               c2 = INDEX(DXGe(i),')')
               IF(c2.EQ.0) THEN
                  c2 = LEN_TRIM(DXGe(i))
               ELSE
                  c2 = c2 - 1
               END IF
               o1 = SPAN(DXGe(j)(1:c1),o1,' ')
               o2 = SPAN(DXGe(j)(1:c2),o2,' ')
               IF(o1.GT.c1) o1 = 1
               IF(o2.GT.c2) o2 = 1
               IF(VALSTR(DXGe(i)(o2:c2)).EQ.VALSTR(DXGe(j)(o1:c1))) THEN
                  IF(DXFlv(i).EQ.' '.OR.DXFlv(j).EQ.' '.OR.DXFlv(i)     &
     &               .EQ.DXFlv(j)) THEN
                     IF(namb.LT.maxamb) THEN
                        did2(i) = .TRUE.
                        namb = namb + 1
                        amblv(namb) = j
                     END IF
                  END IF
               END IF
            END DO
            IF(namb.GT.0) THEN
               IF(card.NE.' ') THEN
                  CALL WRTCHK(-(2+namb/3))
                  WRITE(out,'(1X, I5, 2A)')RECdxg(i), '.  ', card
                  card = ' '
               ELSE
                  CALL WRTCHK(-(1+namb/3))
               END IF
               CALL WRTCHK(1)
               WRITE(out,'(18X,A,T92,A)')  '**********',                &
     &               '<E> IDENTICAL GAMMA ENERGIES'
               strout = DXGe(amblv(1))
               IF(namb.GT.1) strout(LEN_TRIM(strout)+1:) = ','
               DO j = 2, namb
                  IF(LEN_TRIM(strout)+2+LEN_TRIM(DXGe(amblv(j)))        &
     &               .LE.LEN(strout)) THEN
                     strout(LEN_TRIM(strout)+2:) = DXGe(amblv(j))
                     IF(j.LT.namb) strout(LEN_TRIM(strout)+1:) = ','
                  ELSE
                     CALL WRTCHK(1)
                     WRITE(out,'(T96,A)') strout
                     strout = DXGe(amblv(j))
                     IF(j.LT.namb) strout(LEN_TRIM(strout)+1:) = ','
                  END IF
               END DO
               IF(.NOT.NOErr) THEN
                  CALL WRTCHK(1)
                  WRITE(out,'(T96,A)') strout
               END IF
               TOTerr = TOTerr + 1
            END IF
         END DO
      END IF
   30 NDXg = 0
!
      RETURN
      END SUBROUTINE CHKDEX
!
!***********************************************************************
!
      SUBROUTINE CKF9(Col9,Cardtyp)
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Cardtyp, Col9
!
!     Functions used
!
      INTEGER(KIND=4), INTRINSIC :: LEN_TRIM
!
!     Local variables
!
      CHARACTER(LEN=38) :: msg
      INTEGER(KIND=4) :: i,rtype
!
      If(cardtyp .EQ. 'P')Then
         rtype=1
      Else
         rtype=2
      EndIf
      If(nsave9(rtype) .EQ. 0)Return
      DO i = 1, NSAve9(rtype)
         IF(SAVe9(rtype,i).EQ.' ' .AND. NSAve9(rtype).GT.1) THEN
            msg = '<E>'
            msg(LEN_TRIM(msg)+2:) = Cardtyp
            msg(LEN_TRIM(msg)+2:) = 'HAS BLANK IN COL. 9'
            CALL WRTMSG(msg,0,0)
            RETURN
         END IF
         IF(Col9.EQ.SAVe9(rtype,i)) RETURN
      END DO
      msg = '<E> MISMATCH WITH COL. 9 OF'
      msg(LEN_TRIM(msg)+2:) = Cardtyp
      msg(LEN_TRIM(msg)+2:) = 'CARD'
      CALL WRTMSG(msg,9,9)
!
      RETURN
      END SUBROUTINE CKF9
!
!***********************************************************************
!
      INTEGER(KIND=4) FUNCTION RDECTYP(Mode)
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Mode
!
!     Functions used
!
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
      INTEGER(KIND=4), INTRINSIC :: LEN_TRIM
!
!     Local variables
!
      INTEGER(KIND=4) :: a, i, z
!
      INTEGER(KIND=4), PARAMETER :: ntypes = 18
      CHARACTER(LEN=4), DIMENSION(ntypes) :: type
      DATA type/'A', 'P', 'N', 'B-', 'B+', 'EC', 'IT', 'SF', 'B-N',     &
     &     'B-P', 'B+P', 'ECP', 'B-A', 'B+A', 'ECA', 'B-2N', 'B+2P',    &
     &     'EC2P'/
      INTEGER(KIND=4), DIMENSION(ntypes) :: dtype
      DATA dtype/1, 2, 3, 4, 5, 5, 6, 7, 8, 9, 9, 9, 10, 10, 10, 11, 12,&
     &     12/
!
      RDECTYP = 0
!     Check for normal decay modes
      DO i = 1, ntypes
         IF(TRIM(Mode).EQ.TRIM(type(i))) THEN
            RDECTYP = dtype(i)
            RETURN
         END IF
      END DO
!     Check for multiple particle/Heavy-ion decay
      IF(Mode(1:1).GE.'1'.AND.Mode(1:1).LE.'9') THEN
         DO i = 1, 6
            IF(Mode(2:LEN_TRIM(Mode)).EQ.TRIM(type(i))) THEN
               RDECTYP = 13
               RETURN
            END IF
         END DO
         CALL NUCID(TRIM(Mode),a,z)
         IF(a.GT.0.AND.z.GT.0) THEN
            RDECTYP = 14
         ELSE
            CALL GETDECAZ(TRIM(Mode),a,z)
            IF(a.NE.-999.AND.z.NE.-999) RDECTYP = 13
         END IF
      ELSE
         CALL GETDECAZ(TRIM(Mode),a,z)
         IF(a.NE.-999.AND.z.NE.-999) RDECTYP = 13
      END IF
!
      RETURN
      END FUNCTION RDECTYP
!
!***********************************************************************
!
      SUBROUTINE CHKS(Card)
!
!     CHECK Source Card
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=80) :: Card
!
!     Functions used
!
      INTEGER(KIND=4), INTRINSIC :: INDEX, LEN_TRIM
      INTEGER(KIND=4), EXTERNAL :: BREAK, INDEXF, SPAN
!
!     Local variables
!
      CHARACTER(LEN=30) :: xdsid
      INTEGER(KIND=4) :: a, i, j, l, r, z
!
!     CHECK CARD SEQUENCE.
!
      IF(.NOT.ISEen) CALL WRTMSG('<F> DSID CARD NOT YET SEEN',0,0)
      IF(GABeld) CALL WRTMSG('<F> S CARD ILLEGAL IN BODY OF DATA SET',0,&
     &                      0)
      IF(DSType.NE.3.OR..NOT.HISpin)                                    &
     &   CALL WRTMSG('<F> CARD ILLEGAL FOR THIS DATASET',8,8)
      IF(XREfs.NE.' ') CALL WRTMSG('<E> X RECORD ALREADY ENCOUNTERED',8,&
     &                            8)
!
!     VALIDATE NUCID.
!
      CALL CKNCID(Card)
      resnuc=card(1:5)
!
!     VALIDATE ssymb CHARACTER ONLY.
!
      r = LEN_TRIM(SSYmbs)
      IF(r.EQ.0) r = 1
!     SET BLANK AS INITIAL symbol, THUS NO STARTUP PROBLEM WITH ZERO
!     LENGTH STRINGS. ALSO WILL REPORT ERROR IF BLANK USED AS ID,
!     ALTHOUGH IT WILL BE THE ALREADY USED MESSAGE.
      IF(INDEX(SSYmbs(1:r),Card(9:9)).EQ.0) THEN
         r = r + 1
         SSYmbs(r:r) = Card(9:9)
      ELSE
         CALL WRTMSG('<E> SYMBOL CODE ALREADY USED',9,9)
      END IF
!
      IF(LEN_TRIM(Card(10:39)).EQ.0) THEN
         CALL WRTMSG('<E> BLANK DSID',10,39)
      ELSE
         CALL CKREF(Card(40:59),40)
         CALL CKBLNK(Card(60:80),60,80)
         l = 1
         xdsid = Card(10:39)
         IF(xdsid(1:1).EQ.' ') THEN
            CALL WRTMSG('<E> START DSID ON COLUMN 10',10,10)
            DO l = 1, 30
               IF(xdsid(l:l).NE.' ') EXIT
            END DO
         END IF
!        Ignore ":" and information following it
         i = INDEXF(xdsid,l,':')
         IF(i.GT.0) THEN
            r = LEN_TRIM(xdsid(1:i-1))
         ELSE
            r = LEN_TRIM(xdsid)
         END IF
!        "," is a valid terminator but should be ignored in checking
         IF(xdsid(r:r).EQ.',') r = r - 1
         IF(INDEX(xdsid(l:r),'COMMENTS').GT.0) THEN
            CALL WRTMSG('<F> INVALID DATA SET ID FOR X CARD',9+l,9+r)
            RETURN
         END IF
         IF(INDEX(xdsid(l:r),'ADOPTED').GT.0) THEN
            CALL WRTMSG('<F> INVALID DATA SET ID FOR X CARD',9+l,9+r)
            RETURN
         END IF
         IF(INDEX(xdsid(l:r),'REFERENCES').GT.0) THEN
            CALL WRTMSG('<F> INVALID DATA SET ID FOR X CARD',9+l,9+r)
            RETURN
         END IF
         IF(INDEX(xdsid,'MUONIC ATOM').GT.0) THEN
            IF(xdsid(l:r).NE.'MUONIC ATOM')                             &
     &         CALL WRTMSG('<E> INVALID DECAY DATA SET ID',9+l,9+r)
            RETURN
         END IF
         CALL CKPUNC(xdsid(1:r),r)
         IF(INDEX(xdsid(l:r),'DECAY').GT.0) THEN
!           Check parent NUCID
            i = BREAK(xdsid(1:r),l,' ')
            IF(i.GT.r) GO TO 11
            CALL NUCID(xdsid(l:i-1),a,z)
            IF(a.EQ.0.OR.z.LT.0) THEN
               CALL WRTMSG('<F> INVALID PARENT ID',9+l,8+i)
            END IF
            l = i
!           Check decay type
            i = SPAN(xdsid(1:r),l,' ')
            IF(i.GT.l+1) THEN
               CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID',9+l,8+i)
            END IF
            l = i
            i = BREAK(xdsid(1:r),l,' ')
            IF(i.GT.r) GO TO 11
            i = i - 1
            IF(RDECTYP(xdsid(l:i)).EQ.0)                                &
     &         CALL WRTMSG('<E> INVALID DECAY TYPE',9+l,9+i)
            l = i + 1
!           'DECAY' must be next, skip over it.
            i = SPAN(xdsid(1:r),l,' ')
            IF(i.GT.l+1) THEN
               CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID',9+l,8+i)
            END IF
            l = i
            i = BREAK(xdsid(1:r),l,' ')
            IF(xdsid(l:i-1).NE.'DECAY')                                 &
     &         CALL WRTMSG('<E> INVALID "DECAY"',9+l,8+i)
            l = i
!           Look for optional half-life and check it
            IF(l.LT.r) THEN
               i = SPAN(xdsid(1:r),l,' ')
               IF(i.GT.l+1) THEN
                  CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID',9+l,8+i)
               END IF
               l = i
               IF(xdsid(l:l).NE.'('.OR.xdsid(r:r).NE.')') THEN
                  CALL WRTMSG('<E> INVALID HALF-LIFE',9+l,9+r)
               ELSE
                  l = l + 1
                  r = r - 1
                  i = INDEX(xdsid(l:r),'+')
!                 LOOK FOR SEPARATOR.
                  IF(i.NE.0) THEN
                     j = l + i - 2
                     IF(xdsid(j:j).EQ.'E') THEN
!                       '+' IS PART OF NUM, NOT SEPARATOR.
                        j = j + 2
                        i = INDEX(xdsid(j:r),'+')
!                       LOOK FOR SEPARATOR ON THE RIGHT.
                        IF(i.NE.0) i = j - l + i
                     END IF
                  END IF
                  IF(i.EQ.0) THEN
                     IF(xdsid(l:l).EQ.' ')                              &
     &                  CALL WRTMSG('<E> LEADING BLANKS INVALID',9+l,   &
     &                  9+r)
                     IF(xdsid(r:r).EQ.' ')                              &
     &                  CALL WRTMSG('<E> TRAILING BLANKS INVALID',9+l,  &
     &                  9+r)
                     CALL CKT(xdsid(l:r),9+l,9+r,'<E>')
                  ELSE
                     i = i + l - 1
                     IF(xdsid(l:l).EQ.' ')                              &
     &                  CALL WRTMSG('<E> LEADING BLANKS INVALID',9+l,   &
     &                  8+i)
                     IF(xdsid(i-1:i-1).EQ.' ')                          &
     &                  CALL WRTMSG('<E> TRAILING BLANKS INVALID',9+l,  &
     &                  8+i)
                     CALL CKT(xdsid(l:i-1),9+l,8+i,'<E>')
                     IF(xdsid(i+1:i+1).EQ.' ')                          &
     &                  CALL WRTMSG('<E> LEADING BLANKS INVALID',10+i,  &
     &                  9+r)
                     IF(xdsid(r:r).EQ.' ')                              &
     &                  CALL WRTMSG('<E> TRAILING BLANKS INVALID',10+i, &
     &                  9+r)
                     CALL CKT(xdsid(i+1:r),10+i,9+r,'<E>')
                  END IF
               END IF
            END IF
            RETURN
         END IF
!        Assumed to be a reaction ID
!        Handle literal reaction types
         IF(xdsid(l:r).EQ.'INELASTIC SCATTERING'.OR.xdsid(l:r)          &
     &      .EQ.'(HI,XNG)'.OR.xdsid(1:r).EQ.'HIGH-SPIN LEVELS, GAMMAS') &
     &      RETURN
         IF(xdsid(l:17+l).EQ.'COULOMB EXCITATION') THEN
!           COULOMB EXCITATION may be followed by '(<ANY>*,<ANY>*)'
            l = 18 + l
            i = SPAN(xdsid(1:r),l,' ')
            IF(i.GT.l+1) THEN
               CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID',9+l,8+i)
            END IF
            l = i
            tara=resa
	    tarz=resz
            IF(l.LT.r) CALL CKREA(xdsid(l:r),9+l,9+r)
            RETURN
         END IF
!        Handle normal reaction type.
!        If qualifier present, set R to ignore it hereafter.
         IF(xdsid(r-3:r).EQ.' RES'.OR.xdsid(r-3:r).EQ.' IAR'.OR.        &
     &      xdsid(r-3:r).EQ.' IAS') THEN
            r = r - 4
            i = LEN_TRIM(xdsid(1:r))
            IF(i.LT.r) CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID',   &
     &                            9+l+1,9+r)
            r = i
         END IF
!        Check target.
         i = SPAN(xdsid(1:r),l,' ')
         IF(i.GT.l) THEN
            CALL WRTMSG('<E> LEADING BLANKS INVALID',9+l,8+i)
            l = i
         END IF
    5    i = BREAK(xdsid(1:r),l,'(')
         IF(i.GT.r) GO TO 11
         IF(i.GT.l) THEN
            CALL NUCID(xdsid(l:i-1),a,z)
            tarpos=9+l
            If(a .GT. 0)tara=a
	    tarz=z
            IF(z.LT.0)Then
	       CALL WRTMSG('<E> INVALID REACTION TARGET',9+l,8+i)
	       tara=-9999
	       tarz=-9999
               tarpos=-9999
	    EndIf
            IF(xdsid(i-1:i-1).EQ.' ')                                   &
     &         CALL WRTMSG('<E> INVALID BLANK',9+i-1,9+i-1)
            l = i
         ELSE
            IF(INDEXF(xdsid(1:r),i,'(HI,').LE.0)                        &
     &         CALL WRTMSG('<E> INVALID REACTION TARGET',9+l,9+l)
         END IF
         DO WHILE (.TRUE.)
!           Check reaction.
            i = BREAK(xdsid(1:r),l,')')
            IF(i.GT.r) GO TO 11
            CALL CKREA(xdsid(l:i),9+l,9+i)
            l = i + 1
            IF(l.GE.r) RETURN
!           Check for more reactions.
            IF(xdsid(l:l).EQ.',') THEN
               l = l + 1
               i = SPAN(xdsid(1:r),l,' ')
               IF(i.GT.l+1) THEN
                  CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID',9+l,8+i)
               END IF
               l = i
!              Check for target (GOTO 3O0) or not (GOTO 350).
               IF(xdsid(l:l).EQ.'(') CYCLE
               GO TO 5
            END IF
            IF(l.GE.r) RETURN
!           Check for energy field.
            IF(xdsid(l:l+2).EQ.' E=') THEN
               l = l + 3
               IF(xdsid(l:l).EQ.' ') THEN
                  DO WHILE (.TRUE.)
                     i = l + 1
                     IF(xdsid(i:i).EQ.' ') CYCLE
                     CALL WRTMSG('<E> EMBEDDED BLANK INVALID',9+l,8+i)
                     l = i
                     EXIT
                  END DO
               END IF
               IF(xdsid(l:r).EQ.'TH'.OR.xdsid(l:r).EQ.'RES'.OR.         &
     &            xdsid(l:r).EQ.'THERMAL'.OR.xdsid(l:r).EQ.'RESONANCE') &
     &            RETURN
!              Check energy value.
               i = l
               IF(xdsid(l:l+2).EQ.'TH,') THEN
                  l = l + 3
                  i = SPAN(xdsid(1:r),l,' ')
                  IF(i.GT.l+1)                                          &
     &               CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID',9+l, &
     &               8+i)
               ELSE IF(xdsid(l:l+3).EQ.'RES,') THEN
                  l = l + 4
                  i = SPAN(xdsid(1:r),l,' ')
                  IF(i.GT.l+1)                                          &
     &               CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID',9+l, &
     &               8+i)
               END IF
               DO WHILE (.TRUE.)
                  l = i
                  i = BREAK(xdsid(1:r),l,' ,-')
                  IF(i.GT.r) GO TO 11
                  CALL CKV(xdsid(l:i-1),9+l,.TRUE.,'<E>')
                  IF(xdsid(i:i).EQ.',') THEN
                     l = i + 1
                     i = SPAN(xdsid(1:r),l,' ')
                     IF(i.GT.l+1)                                       &
     &                  CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID',  &
     &                  9+l,8+i)
                     CYCLE
                  ELSE IF(xdsid(i:i).EQ.'-') THEN
                     l = i + 1
                     i = SPAN(xdsid(1:r),l,' ')
                     IF(i.GT.l)CALL WRTMSG('<E> LEADING BLANKS INVALID',&
     &                  9+l,8+i)
                     CYCLE
                  END IF
                  l = i
                  i = SPAN(xdsid(1:r),l,' ')
                  IF(i.GT.l+1)                                          &
     &               CALL WRTMSG('<E> MORE THAN ONE BLANK INVALID',9+l, &
     &               8+i)
                  l = i
                  i = BREAK(xdsid(1:r),l,' ')
                  IF(xdsid(l:i-1).NE.'EV'.AND.xdsid(l:i-1).NE.'EV'.AND. &
     &               xdsid(l:i-1).NE.'KEV'.AND.xdsid(l:i-1)             &
     &               .NE.'MEV'.AND.xdsid(l:i-1).NE.'GEV')               &
     &               CALL WRTMSG('<E> INVALID UNITS',9+l,8+i)
                  IF(i.LE.r) CALL WRTMSG('<E> JUNK AT END OF FIELD',9+i,&
     &                                  9+r)
                  GO TO 10
               END DO
            ELSE
               CALL WRTMSG('<E> JUNK AT END OF FIELD',9+l,9+r)
            END IF
   10       EXIT
         END DO
      END IF
      RETURN
   11 CALL WRTMSG('<E> JUNK AT END OF FIELD',9+l,39)
!
      RETURN
      END SUBROUTINE CHKS
!
!***********************************************************************
!
      LOGICAL(KIND=4) FUNCTION ISMET(Tstr)
!
!     Checks to see if a half-life on LEVEL record indicates state
!     is metastable (>=1 ms)
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Tstr
!
!     Functions used
!
      INTEGER(KIND=4), INTRINSIC :: INDEX
      REAL(KIND=4), EXTERNAL :: VALSTR
!
!     Local variables
!
      INTEGER(KIND=4) :: i, unpla
      REAL(KIND=4) :: thalf
!
      INTEGER(KIND=4), PARAMETER :: ntul=11
      REAL(KIND=4), DIMENSION(ntul) :: tul
      DATA tul/31536000., 86400., 3600., 60., 1., 1., 1., 1., 1., 1.,   &
     &     1./
      CHARACTER(LEN=2), DIMENSION(ntul) :: ut
      DATA ut/' Y', ' D', ' H', ' M', ' S', 'MS', 'US', 'NS', 'PS',     &
     &     'FS', 'AS'/
!
      ISMET = .FALSE.
!
      IF(INDEX(Tstr,'EV').GT.0) THEN
         RETURN
      END IF
      DO i = 1, ntul
         unpla = INDEX(Tstr,ut(i))
         IF(unpla.GT.0) GO TO 10
      END DO
      RETURN
!
   10 thalf = VALSTR(Tstr(1:unpla-1))*tul(i)
      IF(i.GT.5) thalf = thalf*0.001**(i-5)
      IF(thalf.GT.0.001) ISMET = .TRUE.
!
      RETURN
      END FUNCTION ISMET
!
!***********************************************************************
!
      SUBROUTINE WRTLEV
!
!     Writes out level record and sequence number - needed for missing
!     decay mode check
!
      IMPLICIT NONE
!
      IF(ERPt.AND.LEVstr.NE.' ') THEN
         CALL WRTCHK(-3)
         WRITE(out,'(1X,I5,A,A80)') LEVseq, '.  ', LEVstr
         LEVstr = ' '
      END IF
!
      RETURN
      END SUBROUTINE WRTLEV
!
!***********************************************************************
!
      SUBROUTINE WRTMS3
!
!     Special message for missing decay modes
!
      IMPLICIT NONE
!
      IF(.NOT.NOWarn) THEN
         CALL WRTLEV
         WRITE(out,'(T92,A)') '<W> CHECK FOR MISSING DECAY MODES'
      END IF
      NOBrpct = .FALSE.
      TOTwar = TOTwar + 1
!
      RETURN
      END SUBROUTINE WRTMS3
!
!***********************************************************************
!
      SUBROUTINE MAKEEJ(Strout,E,De,Jpi,Band)
!
!     Combines energy,jpi, and band information into a string for output
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Band, De, E, Jpi, Strout
!
!     Functions used
!
      INTEGER(KIND=4), INTRINSIC :: LEN_TRIM
!
      Strout = E
      CALL ADDSTR(Strout,LEN_TRIM(Strout)+2,De)
      IF(Jpi.EQ.' '.AND.Band.EQ.' ') RETURN
      CALL ADDSTR(Strout,LEN_TRIM(Strout)+1,'[')
      IF(Jpi.NE.' ') THEN
         CALL ADDSTR(Strout,LEN_TRIM(Strout)+1,Jpi)
         IF(Band.NE.' ') CALL ADDSTR(Strout,LEN_TRIM(Strout)+1,':')
      END IF
      CALL ADDSTR(Strout,LEN_TRIM(Strout)+1,Band)
      CALL ADDSTR(Strout,LEN_TRIM(Strout)+1,']')
!
      RETURN
      END SUBROUTINE MAKEEJ
!
!***********************************************************************
!
      SUBROUTINE CHKBRK(Didit,Message)
!
!     Composite checks at end of data set - output a break line if
!     not previously done already
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Message
      LOGICAL(KIND=4) :: Didit
!
!     Functions used
!
      INTEGER(KIND=4), INTRINSIC :: INDEX
!
!     Local variables
!
      CHARACTER(LEN=38) :: breakit =                                    &
     &                     '======Composite Data Set Checks======='
!
      IF(Message.EQ.' ') RETURN
      IF(.NOT.Didit.AND.                                                &
     &   ((.NOT.(NOWarn.AND.INDEX(Message,'<W>').GT.0).AND.             &
     &   .NOT.(NOErr.AND.INDEX(Message,'<E>').GT.0)).OR.                &
     &   INDEX(Message,'<F>').GT.0)) THEN
         CALL WRTMSG(breakit,0,0)
         Didit = .TRUE.
      END IF
      CALL WRTMSG(Message,0,0)
!
      RETURN
      END SUBROUTINE CHKBRK
!
!***********************************************************************
!
      SUBROUTINE INITH
!
!     Initializes HISTORY record checking for a new data set
!
      IMPLICIT NONE
!
!     Local variables
!
      INTEGER(KIND=4) :: i
!
      DOHist = .FALSE.
      DOHout = .TRUE.
      NODol = .TRUE.
      DO i = 1, NQUant
         FQUant(i) = .FALSE.
      END DO
      EVAldate = 0
      CURquant = ' '
      HSEq = 0
      HCArd = ' '
      ETYpe = 0
!
      RETURN
      END SUBROUTINE INITH
!
!***********************************************************************
!
      SUBROUTINE CHKH(Card,Ipath)
!
!     Checks HISTORY record
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Card
      INTEGER(KIND=4) :: Ipath
!
!     Functions used
!
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
      INTEGER(KIND=4), INTRINSIC :: INDEX, LEN, MIN0, LEN_TRIM
      INTEGER(KIND=4), EXTERNAL :: INDEXF, IVLSTR, TYPSTR
!
!     Local variables
!
      CHARACTER(LEN=3) :: month
      CHARACTER(LEN=71) :: tmpstr
      INTEGER(KIND=4) :: a, c1, c2, clospar, comma, dash1, dash2, day,  &
     &                   dolsign, endblank, ends, eqsign, from, i, j,   &
     &                   openpar, start, thequant, to, year, z
!
      INTEGER(KIND=4), PARAMETER :: nevaltyp=9
      CHARACTER(LEN=3), DIMENSION(nevaltyp) :: evaltyp
      DATA evaltyp/'FUL', 'ERR', 'UPD', 'HIS', 'SDB', 'DEC', 'FMT',     &
     &     'EXP', 'MOD'/
      CHARACTER(LEN=1), DIMENSION(nevaltyp) :: typsta
      DATA typsta/' ', ' ', ' ', 'O', 'O', 'O', ' ', ' ', ' '/
!
      INTEGER(KIND=4), PARAMETER :: nmo=12
      CHARACTER(LEN=3), DIMENSION(nmo) :: months
      DATA months/'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL',      &
     &     'AUG', 'SEP', 'OCT', 'NOV', 'DEC'/
      INTEGER(KIND=4), DIMENSION(nmo) :: days
      DATA days/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
!
      INTEGER(KIND=4), PARAMETER :: nref=3
      CHARACTER(LEN=5), DIMENSION(nref) :: ref
      DATA ref/'NDS', 'NP', 'ENSDF'/
      LOGICAL(KIND=4), DIMENSION(nref) :: refchk
      DATA refchk/.TRUE., .TRUE., .FALSE./
!
      IF(Ipath.EQ.2) GO TO 5
!
      IF(.NOT.ISEen) CALL WRTMSG('<F> DSID CARD NOT YET SEEN',0,0)
      IF(DSType .EQ. 4) THEN
         CALL WRTMSG('<F> H CARD ILLEGAL FOR THIS DATASET',8,8)
         DOHout = .FALSE.
      END IF
      IF(GABeld) THEN
         CALL WRTMSG('<F> H CARD ILLEGAL IN BODY OF DATA SET',8,8)
         DOHout = .FALSE.
      END IF
!
!     Validate NUCID
!
      CALL NUCID(Card(1:5),a,z)
      IF(a.EQ.0.OR.z.EQ.-1) THEN
         CALL WRTCRD(Card)
         CALL WRTMSG('<F> INVALID NUCID',1,5)
         DOHout = .FALSE.
      ELSE IF(IZLmsg.NE.' ') THEN
         CALL REPSTR(IZLmsg,'Obsolete formalism','Obsol. form')
         CALL ADDSTR(IZLmsg,1,'<W> ')
         CALL WRTMSG(TRIM(IZLmsg),4,5)
         DOHout = .FALSE.
      END IF
!
      DOHout = .TRUE.
      DO i = 1, NQUant
         FQUant(i) = .FALSE.
      END DO
      NODol = .TRUE.
      tmpstr = Card(10:)
      HCArd = Card
      IF(HSEq.NE.0.AND.HSEq.NE.NSEq-1) THEN
         CALL WRTMSG('<W> H RECORD OUT OF GROUP',0,0)
         DOHout = .FALSE.
      END IF
!
      IF(tmpstr.EQ.' ') THEN
         CALL WRTMSG('<E> Blank H record',10,80)
         RETURN
      END IF
      DOHist = .TRUE.
!
      HSEq = NSEq
      start = 10
      DO WHILE (.TRUE.)
         IF(tmpstr(1:1).EQ.' ') THEN
            start = start + 1
            tmpstr = tmpstr(2:)
            CYCLE
         END IF
         ends = LEN_TRIM(Card)
         EXIT
      END DO
      GO TO 10
!
!     Checks HISTORY continuation records
!
    5 HCArd = Card
      HSEq = NSEq
      IF(NODol.AND.CURquant.NE.quant(5))                                &
     &   CALL WRTMSG('<E> MISSING "$" AT END OF PRIOR H RECORD',0,0)
      IF(CURquant.EQ.quant(5)) THEN
         IF(INDEX(Card,'$').EQ.0.OR.INDEX(Card,'$').EQ.LEN_TRIM(Card))  &
     &      RETURN
         start = INDEX(Card,'$') + 1
         tmpstr = Card(start:)
      ELSE
         tmpstr = Card(10:)
      END IF
!
   10 DO WHILE (.TRUE.)
         IF(tmpstr.EQ.' ') THEN
            IF(HCArd(LEN_TRIM(HCArd):LEN_TRIM(HCArd)).EQ.'$')           &
     &         CURquant = ' '
            RETURN
         END IF
         eqsign = INDEX(tmpstr,'=')
         IF(eqsign.EQ.0) THEN
            CALL WRTMSG('<E> MISSING "=" sign',start,ends)
            DOHout = .FALSE.
            RETURN
         END IF
         dolsign = INDEX(tmpstr,'$')
         IF(dolsign.EQ.0) THEN
            NODol = .TRUE.
            dolsign = LEN_TRIM(tmpstr) + 1
         ELSE
            NODol = .FALSE.
         END IF
         IF(dolsign.LT.eqsign) THEN
            CALL WRTMSG('<E> MISSING "=" sign',start,dolsign-1)
            DOHout = .FALSE.
            tmpstr = tmpstr(dolsign+1:)
            start = start + dolsign
            CYCLE
         END IF
         DO i = 1, NQUant
            IF(quant(i).EQ.tmpstr(1:MIN0(3,eqsign-1))) THEN
               thequant = i
               FQUant(i) = .TRUE.
               CURquant = quant(i)
               GO TO 20
            END IF
         END DO
         CALL WRTMSG('<E> INVALID FIELD NAME',start,start+eqsign-2)
         DOHout = .FALSE.
         tmpstr = tmpstr(dolsign+1:)
         IF(tmpstr.EQ.' ') THEN
            RETURN
         ELSE
            start = start + dolsign
         END IF
      END DO
   20 c1 = eqsign + 1
      c2 = dolsign - 1
      from = start + eqsign
      to = start + dolsign - 2
      IF(c2.LE.c1) THEN
         IF(from.LE.80) THEN
            CALL WRTMSG('<E> MISSING VALUE',from,from)
            DOHout = .FALSE.
            tmpstr = tmpstr(dolsign+1:)
            start = start + dolsign
            GO TO 10
         ELSE
            CALL WRTMSG('<E> MISSING VALUE',80,80)
            RETURN
         END IF
      END IF
      endblank = 0
      DO WHILE (.TRUE.)
         IF(tmpstr(c1:c1).EQ.' ') THEN
            c1 = c1 + 1
            endblank = endblank + 1
            CYCLE
         END IF
         IF(endblank.NE.0) THEN
            CALL WRTMSG('<E> EMBEDDED BLANK INVALID',start+c1-endblank- &
     &                  1,start+c1-2)
            DOHout = .FALSE.
            from = from + c1
         END IF
!        Check evaluation type
         IF(thequant.EQ.1) THEN
            ETYpe = 0
            DO i = 1, nevaltyp
               IF(evaltyp(i).EQ.tmpstr(c1:MIN0(c1+2,c2))) THEN
                  ETYpe = i
                  IF(typsta(i).NE.' ') THEN
                     DOHout = .FALSE.
                     IF(typsta(i).EQ.'O') THEN
                        CALL WRTMSG('<W> Obsolete Evaluation Type',from,&
     &                              to)
                     ELSE
                        CALL WRTMSG('<W> Proposed Evaluation Type',from,&
     &                              to)
                     END IF
                  END IF
                  GO TO 25
               END IF
            END DO
            CALL WRTMSG('<E> INVALID EVALUATION TYPE',from,to)
            DOHout = .FALSE.
   25       tmpstr = tmpstr(dolsign+1:)
            start = start + dolsign
!           Check author - too ill defined to check at present
         ELSE IF(thequant.EQ.2) THEN
            tmpstr = tmpstr(dolsign+1:)
            start = start + dolsign
!           Check cutoff date
         ELSE IF(thequant.EQ.3.OR.thequant.EQ.6) THEN
            dash1 = INDEX(tmpstr(1:dolsign-1),'-')
            IF(dash1.EQ.0) THEN
               CALL WRTMSG('<E> INVALID DATE',from,to)
               DOHout = .FALSE.
            ELSE
               dash2 = INDEXF(tmpstr(1:dolsign-1),dash1+1,'-')
               IF(dash2.EQ.0) THEN
                  CALL WRTMSG('<E> INVALID DATE',from,to)
                  DOHout = .FALSE.
                  GO TO 30
               END IF
            END IF
            day = IVLSTR(tmpstr(c1:dash1-1))
            month = tmpstr(dash1+1:dash2-1)
            CALL UPSTR(month)
            year = IVLSTR(tmpstr(dash2+1:dolsign-1))
            DO i = 1, nmo
               IF(months(i).EQ.month) THEN
                  IF(day.LT.1.OR.day.GT.days(i)) THEN
!                    Take care of leap years
                     IF(i.NE.2.OR.4*(year/4).NE.year.OR.1000*(year/1000)&
     &                  .EQ.year)                                       &
     &                  CALL WRTMSG('<E> INVALID DAY',start+c1-1,       &
     &                  start+dash1-2)
                  END IF
                  GO TO 30
               END IF
            END DO
            CALL WRTMSG('<E> INVALID MONTH',start+dash1+1,start+dash2-1)
            DOHout = .FALSE.
   30       tmpstr = tmpstr(dolsign+1:)
            start = start + dolsign
!           Check citation
         ELSE IF(thequant.EQ.4) THEN
            DO i = 1, nref
               IF(.NOT.refchk(i).AND.                                   &
     &            INDEXF(tmpstr(1:dolsign-1),c1,TRIM(ref(i))).GT.0) THEN
                  IF(tmpstr(c1:dolsign-1).EQ.ref(i)) GO TO 40
                  comma = c1 + LEN_TRIM(ref(i))
                  IF(dolsign-comma.GT.1) THEN
                     IF(tmpstr(comma+1:dolsign-1).NE.' ') THEN
                        CALL WRTMSG('<E> JUNK AT END OF FIELD',         &
     &                              start+comma,start+dolsign-1)
                        DOHout = .FALSE.
                        GO TO 40
                     END IF
                  END IF
               END IF
            END DO
            comma = INDEX(tmpstr(1:dolsign-1),',')
            IF(comma.EQ.0) GO TO 35
            openpar = INDEXF(tmpstr(1:dolsign-1),comma+1,'(')
            IF(openpar.EQ.0) GO TO 35
            clospar = INDEXF(tmpstr(1:dolsign-1),openpar+1,')')
            IF(clospar.EQ.0) GO TO 35
            DO i = 1, nref
               j = INDEX(tmpstr,TRIM(ref(i)))
               IF(j.EQ.c1) THEN
                  j = j + LEN_TRIM(ref(i))
                  DO WHILE (.TRUE.)
                     IF(tmpstr(j:j).EQ.' ') THEN
                        j = j + 1
                        CYCLE
                     END IF
                     IF(j.GE.comma) THEN
                        CALL WRTMSG('<E> MISSING VOLUME',j+start-1,     &
     &                              j+start-1)
                        DOHout = .FALSE.
                     ELSE IF(i.EQ.2) THEN
                        IF(tmpstr(j:j).NE.'A'.OR.                       &
     &                     TYPSTR(tmpstr(j+1:comma-1)).NE.1) THEN
                           CALL WRTMSG('<E> INVALID VOLUME',j+start-1,  &
     &                                 start+comma-2)
                           DOHout = .FALSE.
                        END IF
                     ELSE IF(TYPSTR(tmpstr(j+1:comma-1)).NE.1) THEN
                        CALL WRTMSG('<E> INVALID VOLUME',j+start-1,     &
     &                              start+comma-2)
                        DOHout = .FALSE.
                     END IF
                     j = comma + 1
                     EXIT
                  END DO
                  DO WHILE (.TRUE.)
                     IF(tmpstr(j:j).EQ.' ') THEN
                        j = j + 1
                        CYCLE
                     END IF
                     IF(TYPSTR(tmpstr(j:openpar-1)).NE.1) THEN
                        CALL WRTMSG('<E> INVALID PAGE',j+start-1,       &
     &                              start+openpar-2)
                        DOHout = .FALSE.
                     END IF
                     j = openpar + 1
                     IF((clospar-j).NE.4.OR.TYPSTR(tmpstr(j:clospar-1)) &
     &                  .NE.1) THEN
                        CALL WRTMSG('<E> INVALID YEAR',j+start,         &
     &                              start+clospar-2)
                        DOHout = .FALSE.
                     END IF
                     GO TO 40
                  END DO
               END IF
            END DO
   35       IF(clospar.EQ.0) clospar = INDEX(tmpstr,'=')
            IF(dolsign-clospar.GT.1) THEN
               IF(tmpstr(clospar+1:dolsign-1).NE.' ') THEN
                  CALL WRTMSG('<E> JUNK AT END OF FIELD',               &
     &                        start+clospar+1,start+dolsign-1)
                  DOHout = .FALSE.
               END IF
            END IF
            DOHout = .FALSE.
            CALL WRTMSG('<E> INVALID CITATION',start+c1,start+dolsign-1)
   40       tmpstr = tmpstr(dolsign+1:)
            start = start + dolsign
!           Check comments - too ill defined to check at present
         ELSE
            IF(dolsign.GE.LEN(tmpstr)) THEN
               tmpstr = ' '
            ELSE
               tmpstr = tmpstr(dolsign+1:)
            END IF
            start = start + dolsign
         END IF
         GO TO 10
      END DO
!
      RETURN
      END SUBROUTINE CHKH
!
!***********************************************************************
!
      SUBROUTINE FINH
!
!     Finishes checking History record when another primary record is
!     encountered
!
      IMPLICIT NONE
!
!     Functions used
!
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
      INTEGER(KIND=4), INTRINSIC :: INDEX, LEN_TRIM
!
!     Local variables
!
      CHARACTER(LEN=38) :: message
      LOGICAL(KIND=4) :: didit
      INTEGER(KIND=4) :: i
!
      LOGICAL(KIND=4), DIMENSION(NQUant) :: rquant, wquant
!
      didit = .FALSE.
!     Set up required quantities
!     Evaluation type always required
      rquant(1) = .TRUE.
      wquant(1) = .FALSE.
!     Author always required
      rquant(2) = .TRUE.
      wquant(2) = .FALSE.
      DO i = 3, 6
         rquant(i) = .FALSE.
         wquant(i) = .FALSE.
      END DO
!     Full evaluation
      IF(ETYpe.EQ.1) THEN
         rquant(3) = .TRUE.
         wquant(4) = .TRUE.
!        Errata
      ELSE IF(ETYpe.EQ.2) THEN
         rquant(5) = .TRUE.
         rquant(6) = .TRUE.
!        Update evaluation
      ELSE IF(ETYpe.EQ.3) THEN
         rquant(3) = .TRUE.
         wquant(4) = .TRUE.
!        High Spin Evaluation
      ELSE IF(ETYpe.EQ.4) THEN
         rquant(3) = .TRUE.
         wquant(4) = .TRUE.
!        Super Deformed Band Evaluation
      ELSE IF(ETYpe.EQ.5) THEN
         rquant(3) = .TRUE.
         wquant(4) = .TRUE.
!        Decay Data evaluation
      ELSE IF(ETYpe.EQ.6) THEN
         rquant(3) = .TRUE.
         wquant(4) = .TRUE.
!        Reformating
      ELSE IF(ETYpe.EQ.7) THEN
         rquant(5) = .TRUE.
         rquant(6) = .TRUE.
!        Experimental Compilation
      ELSE IF(ETYpe.EQ.8) THEN
         rquant(6) = .TRUE.
!        Modification of dataset
      ELSE IF(ETYpe.EQ.9) THEN
         rquant(5) = .TRUE.
         rquant(6) = .TRUE.
      END IF
      DO i = 1, NQUANT
         message = ' '
         IF(.NOT.FQUant(i).AND.rquant(i)) message = '<E> MISSING "'
         IF(.NOT.FQUant(i).AND.wquant(i)) message = '<W> MISSING "'
         IF(message.NE.' ') THEN
            IF(INDEX(message,'<E>').GT.0) THEN
               TOTerr = TOTerr + 1
            ELSE
               TOTwar = TOTwar + 1
            END IF
            CALL ADDSTR(message,LEN_TRIM(message)+1,quant(i))
            CALL ADDSTR(message,LEN_TRIM(message)+1,'" FIELD')
            IF(.NOT.(didit)) THEN
               IF(DOHout) THEN
                  DOHout = .FALSE.
                  IF((INDEX(message,'<W>').GT.0.AND..NOT.NOWarn).OR.    &
     &               (INDEX(message,'<E>').GT.0.AND..NOT.NOErr)) THEN
                     CALL WRTCHK(-2)
                     WRITE(out,'(1X,I5,2A,T92,A)') HSEq, '.  ',         &
     &                     TRIM(HCArd),TRIM(message)
                  END IF
               ELSE IF((INDEX(message,'<W>').GT.0.AND..NOT.NOWarn).OR.  &
     &                 (INDEX(message,'<E>').GT.0.AND..NOT.NOErr)) THEN
                  WRITE(out,'(T92,A)') TRIM(message)
               END IF
               didit = .TRUE.
            ELSE IF((INDEX(message,'<W>').GT.0.AND..NOT.NOWarn).OR.     &
     &              (INDEX(message,'<E>').GT.0.AND..NOT.NOErr)) THEN
               CALL WRTCHK(1)
               WRITE(out,'(T92,A)') TRIM(message)
            END IF
         END IF
      END DO
!
      DOHist = .FALSE.
      NODol = .TRUE.
      DO i = 1, NQUant
         FQUant(i) = .FALSE.
      END DO
      EVAldate = 0
      CURquant = ' '
      HCArd = ' '
!
      RETURN
      END SUBROUTINE FINH
!
!***********************************************************************
!
      SUBROUTINE GLOBAL
!
      IMPLICIT NONE
!
!     Functions used
!
      CHARACTER(LEN=1), INTRINSIC :: CHAR
      INTEGER(KIND=4), INTRINSIC :: INDEX
!
!     Local variables
!
      CHARACTER(LEN=35) :: comp1, comp2
      CHARACTER(LEN=5) :: dsnuc
      CHARACTER(LEN=83) :: tmpstr
      INTEGER(KIND=4) :: dscnt, i, j, nout
!
      LOGICAL(KIND=4), DIMENSION(maxds) :: match
      INTEGER(KIND=4), PARAMETER :: maxout = 1.5*(maxxc+maxds)
      CHARACTER(LEN=83), DIMENSION(maxout) :: outstr
!
!     Compares X cards from Adopted to ID cards and reports differences
!
      PAGe = PAGe + 1
      IF(.NOT.NOXchk) THEN
         IF(PAGe.GT.1) WRITE(out,'(A)') CHAR(12)
         WRITE(out,'(A,15X,A,10X,3A,T118,A,I4)')                        &
     &         'EVALUATED NUCLEAR STRUCTURE DATA FILE', 'SYNTAX CHECK', &
     &         version, ' AS OF ', verdate, 'PAGE ', PAGe
         WRITE(out,'(1X,A//)') 'X RECORD TO ID RECORD COMPARISON'
         IF(NXC.EQ.0) WRITE(out,'(A)')' No X records'
         IF(NDS.EQ.0) WRITE(out,'(A)')' No "source" datasets'
      END IF
      IF(NXC.EQ.0.OR.NDS.EQ.0) RETURN
      DO i = 1, NXC - 1
         comp1 = XCSave(i)(1:5)
         CALL ADDSTR(comp1,6,XCSave(i)(10:))
         DO j = i + 1, NXC
            comp2 = XCSave(j)(1:5)
            CALL ADDSTR(comp2,6,XCSave(j)(10:))
            IF(comp1.GT.comp2) THEN
               tmpstr = XCSave(j)
               XCSave(j) = XCSave(i)
               XCSave(i) = tmpstr
               comp1 = XCSave(i)(1:5)
               CALL ADDSTR(comp1,6,XCSave(i)(10:))
            END IF
         END DO
      END DO
      DO i = 1, NDS - 1
         DO j = i + 1, NDS
            IF(DSSave(i).GT.DSSave(j)) THEN
               tmpstr = DSSave(j)
               DSSave(j) = DSSave(i)
               DSSave(i) = tmpstr
            END IF
         END DO
      END DO
      DO i = 1, NDS
         match(i) = .FALSE.
      END DO
!     If only one source data set for a NUCID and no corresponding
!     adopted, suppress check
      dsnuc = DSSave(1)(1:5)
      dscnt = 1
      IF(NDS.GT.1) THEN
         DO i = 2, NDS
            IF(dsnuc.NE.DSSave(i)(1:5)) THEN
               IF(dscnt.EQ.1.AND.INDEX(ADPtnuc,DSSave(i-1)(1:5)).EQ.0)  &
     &            match(i-1) = .TRUE.
               dsnuc = DSSave(i)(1:5)
               dscnt = 1
            ELSE
               dscnt = dscnt + 1
            END IF
         END DO
      END IF
      nout = 0
      DO i = 1, NXC
         comp1 = XCSave(i)(1:5)
         CALL ADDSTR(comp1,6,XCSave(i)(10:))
         DO j = 1, NDS
            comp2 = DSSave(j)(1:5)
            CALL ADDSTR(comp2,6,DSSave(j)(10:))
            IF(comp1.EQ.comp2) THEN
               match(j) = .TRUE.
               GO TO 10
            END IF
         END DO
         nout = nout + 1
         outstr(nout) = XCSave(i)
         CALL ADDSTR(outstr(nout),44,'***** NO MATCH *****')
   10 END DO
      DO i = 1, NDS
         IF(.NOT.match(i)) THEN
            comp1 = DSSave(i)(1:5)
            CALL ADDSTR(comp1,6,DSSave(i)(10:))
            DO j = 1, NXC
               comp2 = XCSave(j)(1:5)
               CALL ADDSTR(comp2,6,XCSave(j)(10:))
               IF(comp1.EQ.comp2) GO TO 20
            END DO
            nout = nout + 1
            outstr(nout) = '***** NO MATCH *****'
            CALL ADDSTR(outstr(nout),44,DSSave(i))
         END IF
   20 END DO
      DO i = 1, nout - 1
         IF(outstr(i)(1:1).EQ.'*') THEN
            comp1 = outstr(i)(44:48)
            CALL ADDSTR(comp1,6,outstr(i)(53:))
         ELSE
            comp1 = outstr(i)(1:5)
            CALL ADDSTR(comp1,6,outstr(i)(10:43))
         END IF
         DO j = i + 1, nout
            IF(outstr(j)(1:1).EQ.'*') THEN
               comp2 = outstr(j)(44:48)
               CALL ADDSTR(comp2,6,outstr(j)(53:))
            ELSE
               comp2 = outstr(j)(1:5)
               CALL ADDSTR(comp2,6,outstr(j)(10:43))
            END IF
            IF(comp1.GT.comp2) THEN
               tmpstr = outstr(i)
               outstr(i) = outstr(j)
               outstr(j) = tmpstr
            END IF
         END DO
      END DO
      IF(nout.EQ.0) THEN
         IF(.NOT.NOXchk) WRITE(out,'(A)')                               &
     &                ' No mismatches in X Records and ID Records found'
         RETURN
      END IF
      IF(NOXchk) THEN
         WRITE(idefo,'(//A/A)')                                         &
     &          ' ***** Mismatches in X Records and ID Records found',  &
     &          '       (Output suppressed)'
      ELSE
         WRITE(out,'(A,T45,A)')' X Record', 'ID Record'
         WRITE(idefo,'(//A)')                                           &
     &             ' ***** Mismatches in X Records and ID Records found'
         DO i = 1, nout
            WRITE(out,'(1X,A)') outstr(i)
         END DO
         IF(SKIpxc+SKIpds.GT.0) THEN
            WRITE(out,'(//)')
            WRITE(idefo,'(//)')
            IF(SKIpxc.GT.0) THEN
               WRITE(out,'(1X,I8,A)') SKIpxc,' X Records ignored'
               WRITE(idefo,'(1X,I8,A)') SKIpxc,' X Records ignored'
            END IF
            IF(SKIpds.GT.0) THEN
               WRITE(out,'(1X,I8,A)') SKIpds, ' DSID ignored'
               WRITE(idefo,'(1X,I8,A)') SKIpds, ' DSID ignored'
            END IF
         END IF
      END IF
!
      RETURN
      END SUBROUTINE GLOBAL
!
!***********************************************************************
!
      SUBROUTINE CKCOL(Colpos,Colstr)
!
!     Checks string after colon on DSID for defined qualifiers and
!     issues warning if found
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Colstr
      INTEGER(KIND=4) :: Colpos
!
!     Local variables
!
      INTEGER(KIND=4) :: i
!
      CHARACTER(LEN=30) :: tmpstr
      INTEGER(KIND=4), PARAMETER :: nqual=3
      CHARACTER(LEN=3), DIMENSION(nqual) :: qual
      DATA qual/'RES', 'IAR', 'IAS'/
!
      tmpstr = Colstr
      IF(tmpstr.EQ.' ') THEN
         CALL WRTMSG('<W> Blank field after ":"',9+Colpos,39)
         RETURN
      END IF
      CALL SQZSTR(tmpstr,' ')
      DO i = 1, nqual
         IF(tmpstr.EQ.qual(i)) THEN
            CALL WRTMSG('<W> Qualifier found after ":"',9+Colpos,       &
     &                  9+Colpos)
            RETURN
         END IF
      END DO
      IF(tmpstr.EQ.'E=TH'.OR.tmpstr.EQ.'E=RES'.OR.                      &
     &   tmpstr.EQ.'E=THERMAL'.OR.tmpstr.EQ.'E=RESONANCE')              &
     &   CALL WRTMSG('<W> Qualifier found after ":"',9+Colpos,9+Colpos)
!
      RETURN
      END SUBROUTINE CKCOL
!
!***********************************************************************
!
      SUBROUTINE DOWIDTHS(Work)
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Work
!
!     Functions used
!
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
!
!     Local variables
!
      CHARACTER(LEN=10) :: tmpstr
      INTEGER(KIND=4) :: i
      INTEGER(KIND=4) :: INDEX
!
      CHARACTER(LEN=10), PARAMETER :: cdigits = '0123456789'
      CHARACTER(LEN=10), PARAMETER :: pounds = '##########'
!
      INTEGER(KIND=4), PARAMETER :: npart=6
      CHARACTER(LEN=1), DIMENSION(npart) :: particle
      DATA particle/'G', 'N', 'P', 'D', 'T', 'A'/
!
      tmpstr = 'WIDTH'
      tmpstr(7:7) = ''''
      DO i = 1, npart
         tmpstr(6:6) = particle(i)
         CALL REPSTR(Work,TRIM(tmpstr),'WIDTH')
      END DO
      IF(Work.EQ.'WIDTH') RETURN
!
      CALL REPCHR(Work,cdigits,pounds)
      DO WHILE (.TRUE.)
         IF(INDEX(Work,'##').GT.0) THEN
            CALL REPSTR(Work,'##','#')
            CYCLE
         END IF
         IF(Work.EQ.'WIDTH#') THEN
            Work = 'WIDTH'
            RETURN
         END IF
!
         tmpstr(7:7) = '#'
         DO i = 1, npart
            tmpstr(6:6) = particle(i)
            CALL REPSTR(Work,TRIM(tmpstr),'WIDTH')
         END DO
         IF(Work.EQ.'WIDTH#') RETURN
!
         tmpstr(6:) = ' '
         DO i = 1, npart
            tmpstr(6:6) = particle(i)
            CALL REPSTR(Work,TRIM(tmpstr),'WIDTH')
         END DO
!
         RETURN
      END DO
!
      RETURN
      END SUBROUTINE DOWIDTHS
!
!***********************************************************************
!
      SUBROUTINE DODECAYS(Work)
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Work
!
!     Functions used
!
      INTEGER(KIND=4), INTRINSIC :: LEN_TRIM
      INTEGER(KIND=4), EXTERNAL :: TYPSTR
!
!     Local variables
!
      INTEGER(KIND=4) :: i
!
      INTEGER(KIND=4), PARAMETER :: npart=5
      LOGICAL(KIND=4) :: isbr
      CHARACTER(LEN=40) :: temp
      CHARACTER(LEN=1), DIMENSION(5) :: particle
      DATA particle/'N', 'P', 'D', 'T', 'A'/
      INTEGER(KIND=4), PARAMETER :: ndec=4
      CHARACTER(LEN=2), DIMENSION(4) :: decay
      DATA decay/'B-', 'EC', 'IT', 'SF'/
	temp=work
	If(temp(1:1) .EQ. '%')Then
	   isbr=.TRUE.
           temp=temp(2:)
	Else
	   isbr=.FALSE.
	EndIf
!
!     Shouldn't have SF-delayed decay
      IF(temp(1:2).EQ.'SF'.AND.LEN_TRIM(temp).GT.2) RETURN
   10 Continue
      IF(TYPSTR(temp(1:)).EQ.0.OR.TYPSTR(temp(1:)).EQ.1.OR.             &
     &   TYPSTR(temp(1:)).EQ.-2) RETURN
!
!     Check for leading number or X
      IF(temp(1:1).EQ.'X') THEN
         DO i = 1, npart
            IF(temp(2:).EQ.particle(i)) THEN
               Work = '%~'
               RETURN
            END IF
         END DO
!        Shouldn't have SF or IT preceded by an X
         IF(temp(2:3).EQ.'SF'.OR.temp(2:3).EQ.'IT') RETURN
         DO i = 1, 2
            IF(temp(2:).EQ.decay(i)) THEN
               Work = '%~'
               RETURN
            END IF
         END DO
      END IF
!     Special check if "1"
      IF(temp(1:1).EQ.'1'.AND.(temp(2:2).LT.'0'.OR.temp(2:2).GT.'9'))   &
     &   RETURN
      IF(temp(1:1).GE.'1'.AND.temp(1:1).LE.'9') THEN
         i = 2
         DO WHILE (.TRUE.)
            IF(temp(i:i).GE.'1'.AND.temp(i:i).LE.'9') THEN
               i = i + 1
               CYCLE
            END IF
!           Shouldn't have SF or IT preceded by a number
            IF(temp(i:i+1).EQ.'SF'.OR.temp(i:i+1).EQ.'IT') RETURN
            temp(1:) = temp(i:)
            EXIT
         END DO
      END IF
      DO WHILE(.TRUE.)
!
!        Remove leading decay modes and particles
         DO i = 1, ndec
            IF(temp(1:2).EQ.decay(i)) THEN
               temp(1:) = temp(3:)
               IF(isbr .AND. LEN_TRIM(TEMP).EQ.0) THEN
                  Work = '%~'
                  RETURN
               END IF
            END IF
         END DO
         DO i = 1, npart
            IF(temp(1:1).EQ.particle(i)) THEN
               temp(1:) = temp(2:)
               IF(isbr .AND. LEN_TRIM(TEMP).EQ.0) THEN
                  Work = '%~'
                  RETURN
               END IF
            END IF
         END DO
!
!        Still something left - do we iterate or give up
         IF((temp(1:1).GE.'1'.AND.temp(1:1).LE.'9').OR.temp(1:1).EQ.'X')&
     &      GO TO 10
         DO i = 1, ndec
            IF(temp(1:2).EQ.decay(i)) GO TO 20
         END DO
         DO i = 1, npart
            IF(temp(1:1).EQ.particle(i)) GO TO 20
         END DO
!
         RETURN
   20 END DO
!
      RETURN
      END SUBROUTINE DODECAYS
!
!***********************************************************************
!
      SUBROUTINE NUC2DEC(Nucin,Dsid)
!
!     Compares A and Z of NUCID to that expected from the DSID for
!     decay data sets and issues an error if a mismatch
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Dsid, Nucin
!
!     Functions used
!
      INTEGER(KIND=4), INTRINSIC :: INDEX
!
!     Local variables
!
      CHARACTER(LEN=40) :: mode
      INTEGER(KIND=4) :: cala, calz, deca, decz, i, nuca, nucz, para,   &
     &                   parz
!
      CALL NUCID(Nucin,nuca,nucz)
!
!     Error or other problem cannot check more
      IF(nuca.EQ.0.OR.nucz.LT.0) RETURN
!
      mode = Dsid
      CALL LBSUP(mode)
      i = INDEX(mode,' ')
      IF(i.EQ.0) RETURN
      CALL NUCID(mode(1:i-1),para,parz)
      cala = para
      calz = parz
!
!     Most common decay modes may be gotten from the dectype
!     Unknown or SF decay - Cannot do calculation
      IF(DECtyp.EQ.0.OR.DECtyp.EQ.7) RETURN
!     Mixture - must try to decode
      IF(DECtyp.EQ.9.OR.DECtyp.EQ.10.OR.DECtyp.EQ.13) THEN
         mode = mode(i+1:)
         CALL LBSUP(mode)
         i = INDEX(mode,' ')
         IF(i.GT.1) mode = mode(1:i-1)
         IF(DECtyp.EQ.9) THEN
            cala = cala - 1
            calz = calz - 1
            IF(INDEX(mode,'B-').EQ.1) THEN
               calz = calz + 1
            ELSE
               calz = calz - 1
            END IF
         ELSE IF(DECtyp.EQ.10) THEN
            cala = cala - 4
            calz = calz - 2
            IF(INDEX(mode,'B-').EQ.1) THEN
               calz = calz + 1
            ELSE
               calz = calz - 1
            END IF
!           Must really work now
         ELSE
            CALL GETDECAZ(mode,deca,decz)
            IF(deca.EQ.-999.OR.decz.EQ.-999) THEN
               RETURN
            ELSE
               cala = cala - deca
               calz = calz - decz
            END IF
         END IF
      ELSE IF(DECtyp.EQ.1) THEN
         cala = cala - 4
         calz = calz - 2
      ELSE IF(DECtyp.EQ.2) THEN
         cala = cala - 1
         calz = calz - 1
      ELSE IF(DECtyp.EQ.3) THEN
         cala = cala - 1
      ELSE IF(DECtyp.EQ.4) THEN
         calz = calz + 1
      ELSE IF(DECtyp.EQ.5) THEN
         calz = calz - 1
      ELSE IF(DECtyp.EQ.6) THEN
      ELSE IF(DECtyp.EQ.8) THEN
         cala = cala - 1
         calz = calz + 1
      ELSE IF(DECtyp.EQ.11) THEN
         cala = cala - 2
         calz = calz + 1
      ELSE IF(DECtyp.EQ.12) THEN
         cala = cala - 2
         calz = calz - 3
      ELSE
         RETURN
      END IF
      IF((nuca-cala).NE.0.OR.(nucz-calz).NE.0)                          &
     &   CALL WRTMS2('<F> NUCID, PARENT, MODE DISCREPANT',1,5,10,       &
     &   9+LEN_TRIM(Dsid))
!
      RETURN
      END SUBROUTINE NUC2DEC
!
!***********************************************************************
!
      SUBROUTINE GETDECAZ(Mode,Deca,Decz)
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      INTEGER(KIND=4) :: Deca, Decz
      CHARACTER(LEN=*) :: Mode
!
!     Functions used
!
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
      INTEGER(KIND=4), INTRINSIC :: LEN_TRIM
      INTEGER(KIND=4), EXTERNAL :: IVLSTR
!
!     Local variables
!
      CHARACTER(LEN=40) :: tmpmode
      INTEGER(KIND=4) :: i, mult, tmpa, tmpz
!
      INTEGER(KIND=4), PARAMETER :: ntypes = 6
      CHARACTER(LEN=2), DIMENSION(ntypes) :: type
      DATA type/'A', 'P', 'N', 'B-', 'B+', 'EC'/
      INTEGER(KIND=4), DIMENSION(ntypes) :: typea, typez
      DATA typea/4, 1, 1, 0, 0, 0/
      DATA typez/2, 1, 0, -1, +1, +1/
!
      Deca = -999
      Decz = -999
      tmpmode = Mode
      tmpa = 0
      tmpz = 0
      DO WHILE (.TRUE.)
         IF(tmpmode.EQ.' ') THEN
            Deca = tmpa
            Decz = tmpz
            RETURN
         END IF
         IF(tmpmode(1:1).GE.'1'.AND.tmpmode(1:1).LE.'9') THEN
            DO i = 1, ntypes
               IF(tmpmode(2:LEN_TRIM(type(i))+1).EQ.TRIM(type(i))) THEN
                  mult = IVLSTR(tmpmode(1:1))
                  tmpa = tmpa + mult*typea(i)
                  tmpz = tmpz + mult*typez(i)
                  CALL DELSTR(tmpmode,1,LEN_TRIM(type(i))+1)
                  GO TO 10
               END IF
            END DO
            CALL NUCID(tmpmode(1:LEN_TRIM(Mode)),tmpa,tmpz)
            IF(tmpa.GT.0.AND.tmpz.GT.0) THEN
               tmpmode = ' '
               CYCLE
            ELSE
               RETURN
            END IF
         ELSE
            DO i = 1, ntypes
               IF(tmpmode(1:LEN_TRIM(type(i))).EQ.TRIM(type(i))) THEN
                  tmpa = tmpa + typea(i)
                  tmpz = tmpz + typez(i)
                  tmpmode = tmpmode(LEN_TRIM(type(i))+1:)
                  GO TO 10
               END IF
            END DO
         END IF
         RETURN
   10 END DO
!
      RETURN
      END SUBROUTINE GETDECAZ
!
!***********************************************************************
!
      SUBROUTINE CKLABEL(Work,Labfnd)
!
!     Checks if a label is allowed for a symbol or record type and
!     sets up for later checking if allowed
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Work
      LOGICAL(KIND=4) :: Labfnd
!
!     Functions used
!
      INTEGER(KIND=4), INTRINSIC :: LEN_TRIM, INDEX
!
!     Local variables
!
      CHARACTER(LEN=80) :: tmpstr
      INTEGER(KIND=4) :: compos, i
!
      LOGICAL(KIND=4), DIMENSION(nalab) :: aallow
      DATA aallow/.TRUE., .FALSE., .TRUE., .FALSE., .TRUE., 3*.FALSE./
      CHARACTER(LEN=3), DIMENSION(nalab) :: afield
      DATA afield/'E', 'DE', 'IA', 'DIA', 'HF', 'DHF', 'C', 'Q'/
      LOGICAL(KIND=4), DIMENSION(nblab) :: ballow
      DATA ballow/.TRUE., .FALSE., .TRUE., .FALSE., .TRUE., 4*.FALSE./
      CHARACTER(LEN=5), DIMENSION(nblab) :: bfield
      DATA bfield/'E', 'DE', 'IB', 'DIB', 'LOGFT', 'DFT', 'C', 'UN',    &
     &     'Q'/
      LOGICAL(KIND=4), DIMENSION(ndlab) :: dallow
      DATA dallow/.TRUE., .FALSE., .TRUE., .FALSE., 2*.TRUE., .FALSE.,  &
     &     .TRUE., 3*.FALSE./
      CHARACTER(LEN=4), DIMENSION(ndlab) :: dfield
      DATA dfield/'E', 'DE', 'IP', 'DIP', 'EI', 'T', 'DT', 'L', 'C',    &
     &     'COIN', 'Q'/
      LOGICAL(KIND=4), DIMENSION(nelab) :: eallow
      DATA eallow/.TRUE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE.,    &
     &     .TRUE., .FALSE., .TRUE., 4*.FALSE./
      CHARACTER(LEN=5), DIMENSION(nelab) :: efield
      DATA efield/'E', 'DE', 'IB', 'DIB', 'IE', 'DIE', 'LOGFT', 'DFT',  &
     &     'TI', 'DTI', 'C', 'UN', 'Q'/
      LOGICAL(KIND=4), DIMENSION(nglab) :: gallow
      DATA gallow/.TRUE., .FALSE., .TRUE., .FALSE., 2*.TRUE., .FALSE.,  &
     &     .TRUE., .FALSE., .TRUE., 4*.FALSE./
      CHARACTER(LEN=4), DIMENSION(nglab) :: gfield
      DATA gfield/'E', 'DE', 'RI', 'DRI', 'M', 'MR', 'DMR', 'CC', 'DCC',&
     &     'TI', 'DTI', 'C', 'COIN', 'Q'/
      LOGICAL(KIND=4), DIMENSION(nllab) :: lallow
      DATA lallow/.TRUE., .FALSE., 2*.TRUE., .FALSE., 2*.TRUE.,         &
     &     4*.FALSE./
      CHARACTER(LEN=2), DIMENSION(nllab) :: lfield
      DATA lfield/'E', 'DE', 'J', 'T', 'DT', 'L', 'S', 'DS', 'C', 'MS', &
     &     'Q'/
      INTEGER(KIND=4), PARAMETER :: cardn = 1, cardl = 4, carda = 5,    &
     &                              cardb = 6, carde = 7, cardg = 8,    &
     &                              cardd = 9
!
!     Note: D card usually rtype=11, here use 9.
!     corrections made at start and end of subroutine.
!
      Labfnd = .FALSE.
!
!     Make adjustment for D card, RTYPE from 11 to 9 (this subr only).
      IF(RTYpe.EQ.11) RTYpe = cardd
!
!     Make sure rtype is between CARDN and CARDD.
      IF(RTYpe.LT.cardn.OR.cardd.LT.RTYpe) THEN
         CALL WRTMSG('<E> INVALID RECORD TYPE FOR "LABEL="',0,0)
         GO TO 30
      END IF
!     Check for record types which do not allow labels
      IF(RTYpe.LT.cardl) THEN
         CALL WRTMSG('<E> INVALID RECORD TYPE FOR "LABEL="',0,0)
         GO TO 30
      END IF
!
      tmpstr = Work
      compos = INDEX(tmpstr,',')
      IF(compos.GT.0) THEN
         tmpstr = tmpstr(compos+1:)
         CALL WRTMSG('<W> TWO COLUMNS WITH SAME "LABEL="',10,           &
     &               16+LEN_TRIM(Work))
      ELSE
         tmpstr = ' '
      END IF
!     Check for L record symbols
   10 IF(RTYpe.EQ.cardl) THEN
         DO i = 1, nllab
            IF(Work.EQ.lfield(i)) THEN
               IF(lallow(i)) THEN
                  LLAbon(i) = .TRUE.
                  Labfnd = .TRUE.
               ELSE
                  CALL WRTMSG('<E> NO "LABEL=" ALLOWED FOR SYMBOL',10,  &
     &                        16+LEN_TRIM(Work))
               END IF
               GO TO 20
            END IF
         END DO
         CALL WRTMSG('<E> INVALID "LABEL=" FOUND',10,16+LEN_TRIM(Work))
!        Check for A record symbols
      ELSE IF(RTYpe.EQ.carda) THEN
         DO i = 1, nalab
            IF(Work.EQ.afield(i)) THEN
               IF(aallow(i)) THEN
                  ALAbon(i) = .TRUE.
                  Labfnd = .TRUE.
               ELSE
                  CALL WRTMSG('<E> NO "LABEL=" ALLOWED FOR SYMBOL',10,  &
     &                        16+LEN_TRIM(Work))
               END IF
               GO TO 20
            END IF
         END DO
         CALL WRTMSG('<E> INVALID "LABEL=" FOUND',10,16+LEN_TRIM(Work))
!        Check for B record symbols
      ELSE IF(RTYpe.EQ.cardb) THEN
         DO i = 1, nblab
            IF(Work.EQ.bfield(i)) THEN
               IF(ballow(i)) THEN
                  BLAbon(i) = .TRUE.
                  Labfnd = .TRUE.
               ELSE
                  CALL WRTMSG('<E> NO "LABEL=" ALLOWED FOR SYMBOL',10,  &
     &                        16+LEN_TRIM(Work))
               END IF
               GO TO 20
            END IF
         END DO
         CALL WRTMSG('<E> INVALID "LABEL=" FOUND',10,16+LEN_TRIM(Work))
!        Check for EC record symbols
      ELSE IF(RTYpe.EQ.carde) THEN
         DO i = 1, nelab
            IF(Work.EQ.efield(i)) THEN
               IF(eallow(i)) THEN
                  ELAbon(i) = .TRUE.
                  Labfnd = .TRUE.
               ELSE
                  CALL WRTMSG('<E> NO "LABEL=" ALLOWED FOR SYMBOL',10,  &
     &                        16+LEN_TRIM(Work))
               END IF
               GO TO 20
            END IF
         END DO
         CALL WRTMSG('<E> INVALID "LABEL=" FOUND',10,16+LEN_TRIM(Work))
!        Check for G record symbols
      ELSE IF(RTYpe.EQ.cardg) THEN
         DO i = 1, nglab
            IF(Work.EQ.gfield(i)) THEN
               IF(gallow(i)) THEN
                  GLAbon(i) = .TRUE.
                  Labfnd = .TRUE.
               ELSE
                  CALL WRTMSG('<E> NO "LABEL=" ALLOWED FOR SYMBOL',10,  &
     &                        16+LEN_TRIM(Work))
               END IF
               GO TO 20
            END IF
         END DO
         CALL WRTMSG('<E> INVALID "LABEL=" FOUND',10,16+LEN_TRIM(Work))
!        Check for D record symbols
      ELSE IF(RTYpe.EQ.cardd) THEN
         DO i = 1, ndlab
            IF(Work.EQ.dfield(i)) THEN
               IF(dallow(i)) THEN
                  DLAbon(i) = .TRUE.
                  Labfnd = .TRUE.
               ELSE
                  CALL WRTMSG('<E> NO "LABEL=" ALLOWED FOR SYMBOL',10,  &
     &                        16+LEN_TRIM(Work))
               END IF
               GO TO 20
            END IF
         END DO
         CALL WRTMSG('<E> INVALID "LABEL=" FOUND',10,16+LEN_TRIM(Work))
!        Fell through everything - must be an error
      ELSE
         CALL WRTMSG('<E> INVALID "LABEL=" FOUND',10,16+LEN_TRIM(Work))
      END IF
      GO TO 30
!     More processing since multiple symbols
   20 IF(tmpstr.EQ.' ') GO TO 30
      compos = INDEX(tmpstr,',')
      IF(compos.GT.0) THEN
         tmpstr = tmpstr(compos+1:)
      ELSE
         tmpstr = ' '
      END IF
      GO TO 10
!
   30 IF(RTYpe.EQ.9) RTYpe = 11
!
      RETURN
      END SUBROUTINE CKLABEL
!
!***********************************************************************
!
      SUBROUTINE INITLAB
!
!     Initialize COMMON's for LABEL reset
!
      IMPLICIT NONE
!
!     Local variables
!
      INTEGER(KIND=4) :: i
!
      DO i = 1, nllab
         LLAbon(i) = .FALSE.
      END DO
      DO i = 1, nalab
         ALAbon(i) = .FALSE.
      END DO
      DO i = 1, nblab
         BLAbon(i) = .FALSE.
      END DO
      DO i = 1, nelab
         ELAbon(i) = .FALSE.
      END DO
      DO i = 1, nglab
         GLAbon(i) = .FALSE.
      END DO
      DO i = 1, ndlab
         DLAbon(i) = .FALSE.
      END DO
!
      RETURN
      END SUBROUTINE INITLAB
!
!***********************************************************************
!
      SUBROUTINE CKISPIN(Field,From,To,Odd)
!
!     Checks ISPIN on an L record continuation
!     Patterned after the CKJ subroutine
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Field
      INTEGER(KIND=4) :: From, To
      LOGICAL(KIND=4) :: Odd
!
!     Functions used
!
      CHARACTER(LEN=1), INTRINSIC :: CHAR
      INTEGER(KIND=4), INTRINSIC :: INDEX, LEN, LEN_TRIM
      INTEGER(KIND=4), EXTERNAL :: BREAK, INDEXF, SPAN
!
!     Local variables
!
      CHARACTER(LEN=38) :: message
      CHARACTER(LEN=68) :: work
      INTEGER(KIND=4) :: i, j, l, n, r
!
      r = LEN_TRIM(Field)
      IF(r.EQ.0) THEN
         CALL WRTMSG('<E> INVALID ISPIN FIELD',From,To)
         RETURN
      END IF
!
      work = Field
!
      CALL SQZSTR(work,' ')
      r = LEN_TRIM(work)
!
      n = 0
      DO i = 1, r
         IF(work(i:i).EQ.'(') n = n + 1
         IF(work(i:i).EQ.')') n = n - 1
      END DO
      IF(n.NE.0) CALL WRTMSG('<E> UNBALANCED PARENTHESES',From,To)
      CALL SQZSTR(work(1:r),'(')
      CALL SQZSTR(work(1:r),')')
      r = LEN_TRIM(work)
!
      n = 0
      DO i = 1, r
         IF(work(i:i).EQ.'[') n = n + 1
         IF(work(i:i).EQ.']') n = n - 1
      END DO
      IF(n.NE.0) CALL WRTMSG('<E> UNBALANCED SQUARE BRACKETS',From,To)
      CALL SQZSTR(work(1:r),'[')
      CALL SQZSTR(work(1:r),']')
      r = LEN_TRIM(work)
!
!     Allow all connectors for J plus "+"
!
      CALL REPSTR(work,'OR',',')
      CALL REPSTR(work,'AND',',')
      CALL REPSTR(work,'TO',',')
      CALL REPSTR(work,'&',',')
      CALL REPSTR(work,':',',')
      CALL REPSTR(work,'+',',')
!
!     Check even/oddness of A before removing "/2"
      IF(Odd) THEN
         message = '<E> ODD A, INTEGER ISPIN'
         i = INDEX(work,'/2')
         IF(i.EQ.0.AND.BREAK(work,1,'0123456789').LE.LEN(work)) THEN
            CALL WRTMSG(message,From,To)
         ELSE
            j = INDEX(work,',')
            DO WHILE (.TRUE.)
               IF(j.EQ.0) THEN
                  EXIT
               END IF
               IF(i.GT.j) THEN
                  CALL WRTMSG(message,From,To)
                  EXIT
               ELSE
                  i = INDEXF(work,j,'/2')
                  IF(i.EQ.0) THEN
                     CALL WRTMSG(message,From,To)
                     EXIT
                  END IF
                  j = INDEXF(work,i,',')
               END IF
            END DO
         END IF
      ELSE IF(INDEX(work,'/2').GT.0) THEN
         message = '<E> EVEN A, HALF-INTEGER ISPIN'
         CALL WRTMSG(message,From,To)
      END IF
!
!     Remove "1/2"
      CALL REPSTR(work,'/2',CHAR(0))
      DO WHILE (.TRUE.)
!
!        Replace digit string with J
         i = 1
         i = BREAK(work(1:r),i,'0123456789')
         IF(i.LE.r) THEN
            l = SPAN(work(1:r),i,'0123456789')
            work(i:i) = 'J'
            l = l - i
            IF(l.GT.1) THEN
               CALL DELSTR(work(i:r),2,l-1)
               r = r - l + 1
            END IF
            CYCLE
         END IF
!
!        If "NOTJ" as stand alone, done
         IF(work(1:r).EQ.'NOTJ') RETURN
         EXIT
      END DO
      DO WHILE (.TRUE.)
!
!        Replace "J,J" with "J"
         i = INDEX(work(1:r),'J,J')
         IF(i.GT.0) THEN
            CALL DELSTR(work(i:r),2,2)
            r = r - 2
            CYCLE
         END IF
!
!        Should only be "J" left
         IF(work(1:r).NE.'J')CALL WRTMSG('<E> INVALID ISPIN FIELD',From,&
     &      To)
!
         RETURN
      END DO
!
      RETURN
      END SUBROUTINE CKISPIN
!
!***********************************************************************
!
      SUBROUTINE CLDZERO(Field,From,Type)
!
!     Checks if the leading character is a zero followed by an integer
!     - This is a probable typing error
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Field, Type
      INTEGER(KIND=4) :: From
!
!     Functions used
!
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
!
!     Local variables
!
      CHARACTER(LEN=38) :: message
!
      message = ' PROBABLE TYPING ERROR'
!
      If(LEN_TRIM(Field) .EQ. 0)Return
      If(Field(1:1).EQ.'0' .AND. Field(2:2).EQ.' ' .AND. .NOT.contin)   &
     &  Then
         Call Wrtmsg('<W> Possible typing error',from,from)
      Else
         IF(LEN_TRIM(Field).LE.1) RETURN
      EndIf
!
      IF(Field(1:1).NE.'0') RETURN
      IF(Field(2:2).LT.'0'.OR.Field(2:2).GT.'9') RETURN
!
      CALL ADDSTR(message,1,TRIM(Type))
      CALL WRTMSG(message,From,From)
!
      RETURN
      END SUBROUTINE CLDZERO
!
!***********************************************************************
!
      LOGICAL(KIND=4) FUNCTION CGRI100(Field)
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Field
!
!     Functions used
!
      INTEGER(KIND=4) :: INDEX, LEN_TRIM
      REAL(KIND=4), EXTERNAL :: VALSTR
!
!     Local variables
!
      CHARACTER(LEN=10) :: work
      INTEGER(KIND=4) :: i
!
!     Checks to see if field value is "exactly" 100
!
      CGRI100 = .TRUE.
      work = Field
!
      CALL LBSUP(work)
      IF(work(1:1).EQ.'(') work = work(2:)
      i = LEN_TRIM(work)
      IF(i.LE.1) RETURN
      IF(work(i:i).EQ.')') work = work(1:i-1)
!
      IF(INDEX(work,'100').EQ.1) THEN
         IF(work(4:4).EQ.' ') THEN
            CGRI100 = .FALSE.
         ELSE IF(work(4:4).EQ.'.') THEN
            i = 5
            DO WHILE (work(i:i).EQ.'0')
               i = i + 1
            END DO
            IF(work(i:).EQ.' ') CGRI100 = .FALSE.
         END IF
         RETURN
      END IF
      IF(INDEX(work,'E').GT.0) THEN
         IF(VALSTR(work).EQ.100.) CGRI100 = .FALSE.
      END IF
!
      RETURN
      END FUNCTION CGRI100
!
!***********************************************************************
!
      SUBROUTINE CKFLVAL(Flstr,From,To)
!
!     Checks the value of a final level on a GAMMA continuation record t
!     see if it is consistent with E(level)-Egamma
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Flstr
      INTEGER(KIND=4) :: From, To
!
!     Functions used
!
      INTEGER(KIND=4), INTRINSIC :: INDEX, LEN_TRIM
      REAL(KIND=4), INTRINSIC :: ABS, FLOAT, SQRT
!
!     Local variables
!
      CHARACTER(LEN=2) :: dummy
      CHARACTER(LEN=38) :: message
      CHARACTER(LEN=10) :: test
      LOGICAL(KIND=4) :: isoff
      INTEGER(KIND=4) :: i, j
      REAL(KIND=4) :: clval, dclval, deg, delev, dflval, eg, elev,      &
     &                emass, flval, grec
!
      If(Flstr .EQ. FLVl(NFLvl))Then
         CALL WRTMSG('<E> FL= is same as current E(level)',From,To)
	 Return
      EndIf
      DO i = 1, NFLvl-1
         IF(Flstr.EQ.FLVl(i)) GO TO 10
      END DO
      RETURN
   10 CALL CNVS2U(FLVl(i),DFLvl(i),flval,dflval)
      CALL CNVS2U(FLVl(NFLvl),DFLvl(NFLvl),elev,delev)
      CALL CNVS2U(DXGe(NDXg),DXGde(NDXg),eg,deg)
      isoff = .FALSE.
      IF(NOFf.GT.0) THEN
         DO j = 1, NOFf
            IF(INDEX(FLVl(i),OFFchr(j)).GT.0) THEN
               flval = flval + OFFamt(j)
               isoff = .TRUE.
            END IF
            IF(INDEX(FLVl(NFLvl),OFFchr(j)).GT.0) THEN
               elev = elev + OFFamt(j)
               isoff = .TRUE.
            END IF
            IF(INDEX(DXGe(NDXg),OFFchr(j)).GT.0) eg = eg + OFFamt(j)
         END DO
      END IF
      IF(DXGde(NDXg)(1:1).EQ.'L') THEN
         eg = eg/2.
         deg = eg
      END IF
      emass = 931501.6*FLOAT(DSA)
      grec = eg**2/(2.0*emass)
      clval = elev - (eg+grec)
      dclval = SQRT(delev**2+deg**2)
      CALL CNVU2S(clval,dclval,test,10,dummy,2)
      CALL LBSUP(test)
      IF(test.EQ.Flstr) RETURN
      If(INDEX(FLVl(NFLvl),'SN')+INDEX(FLVl(NFLvl),'SP').GT.0)Return
      IF(ABS(flval-clval).LT.2.0*SQRT(dflval**2+dclval**2)) RETURN
      message = 'NOT CONS. WITH EL-EG='
      IF(isoff) THEN
         DO j = 1, NOFf
            IF(INDEX(FLVl(NFLvl),OFFchr(j)).GT.0) THEN
               clval = clval - OFFamt(j)
               CALL CNVU2S(clval,dclval,test,10,dummy,2)
               CALL LBSUP(test)
               CALL ADDSTR(message,LEN_TRIM(message)+1,test)
               CALL ADDSTR(message,LEN_TRIM(message)+1,'+')
               CALL ADDSTR(message,LEN_TRIM(message)+1,OFFchr(j))
               GO TO 20
            END IF
         END DO
      END IF
      CALL ADDSTR(message,LEN_TRIM(message)+1,test)
   20 CALL ADDSTR(message,LEN_TRIM(message)+2,dummy)
      If(ABS(flval-clval).LT.5.0*SQRT(dflval**2+dclval**2)) THEN
         Return
      ElseIF(ABS(flval-clval).LT.7.5*SQRT(dflval**2+dclval**2)) THEN
         CALL ADDSTR(message,1,'<W> ')
      ELSE
         CALL ADDSTR(message,1,'<E> ')
      END IF
      CALL WRTMSG(message,From,To)
!
      RETURN
      END SUBROUTINE CKFLVAL
!
!***********************************************************************
!
      SUBROUTINE CKSPEC(Card)
!
!     Check for codes which cause a backspace when Comment records are
!     translated
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Card
!
!     Functions used
!
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
      INTEGER(KIND=4), INTRINSIC :: INDEX, LEN_TRIM
!
!     Local variables
!
      CHARACTER(LEN=38) :: message
      INTEGER(KIND=4) :: i, k, l, m
      INTEGER(KIND=4) :: INDEXF
      INTEGER(KIND=4), DIMENSION(3) :: j
!
      INTEGER(KIND=4), PARAMETER :: nspeccod=2
      CHARACTER(LEN=5), DIMENSION(nspeccod) :: speccod
      DATA speccod/'DELTA', 'SUMOF'/
!
      k = 0
      DO i = 1, nspeccod
         j(i) = INDEX(Card,TRIM(speccod(i)))
         k = k + j(i)
      END DO
      DO WHILE (.TRUE.)
         IF(k.EQ.0) THEN
            RETURN
         END IF
         k = 100
         DO i = 1, nspeccod
            IF(j(i).GT.0.AND.j(i).LE.k) THEN
               k = j(i)
               l = i
            END IF
         END DO
         m = k + LEN_TRIM(speccod(l))
         IF(m.LT.LEN_TRIM(Card)) THEN
            IF(Card(m:m).NE.' ') THEN
               message = '"'//TRIM(speccod(l))
               message = TRIM(message)//'" must be followed by " "'
               IF(Card(m:m).EQ.'('.OR.Card(m:m).EQ.')') THEN
                  TOTfat = TOTfat + 1
                  message = '<F> '//message
               ELSE
                  TOTerr = TOTerr + 1
                  IF(NOErr) THEN
                     message = ' '
                  ELSE
                     message = '<E> '//message
                  END IF
               END IF
               IF(message.NE.' ') CALL WRTMSG(TRIM(message),k,m)
            END IF
         END IF
         k = 0
         DO i = 1, nspeccod
            j(i) = INDEXF(Card,m,TRIM(speccod(i)))
            k = k + j(i)
         END DO
      END DO
!
      RETURN
      END SUBROUTINE CKSPEC
!
!***********************************************************************
!
      SUBROUTINE CKECCONS
!
!     Check for consistency between electron-capture fractions on "S E"
!     records and IE/TI from EC/B+ record
!
      IMPLICIT NONE
!
!     Functions used
!
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
      INTEGER(KIND=4), INTRINSIC :: LEN
      INTEGER(KIND=4), EXTERNAL :: TYPSTR
      REAL(KIND=4), INTRINSIC :: ABS, AMAX1, SQRT
!
!     Local variables
!
      CHARACTER(LEN=20) :: dxstr, xstr
      CHARACTER(LEN=38) :: message
      LOGICAL(KIND=4) :: b_eout, sb_eout
      INTEGER(KIND=4) :: i
      REAL(KIND=4) :: die2ti, dx, dy, ie2ti, test1, test2, x, y
!
      b_eout = .TRUE.
      sb_eout = .TRUE.
      IF(ECCapt.EQ.0.0) THEN
         IF(IEStr.EQ.' ') THEN
            GO TO 20
         END IF
         TOTwar = TOTwar + 1
         IF(.NOT.NOWarn) THEN
            WRITE(out,'(1X,I5,2A)') ENUm, '.  ', ECArd
            IF(SECard.NE.' ') THEN
               WRITE(out,'(1X,I5,2A)') SENum, '.  ', SECard
            END IF
            WRITE(out,'(91X,A)')'<W> IE>0 but no capt. fract.'
         END IF
         GO TO 10
      END IF
      IF(IEStr.EQ.' ') THEN
         IF(ECCapt.LE.0.0) THEN
            GO TO 20
         END IF
         TOTwar = TOTwar + 1
         IF(.NOT.NOWarn) THEN
            WRITE(out,'(1X,I5,2A)') ENUm, '.  ', ECArd
            WRITE(out,'(1X,I5,2A)') SENum, '.  ', SECard
            WRITE(out,'(91X,A)')'<W> IE=" " but CK,... given'
         END IF
         GO TO 10
      END IF
      CALL LBSUP(IEStr)
      CALL LBSUP(DIEstr)
      CALL LBSUP(TIStr)
      CALL LBSUP(DTIstr)
      IF(TIStr.EQ.' ') THEN
         ie2ti = 1.0
         die2ti = 0.01
      ELSE IF((IEStr.EQ.TIStr).AND.(DIEstr.EQ.DTIstr)) THEN
         IF(ECArd(22:29).EQ.' ') THEN
            ie2ti = 1.0
         ELSE
            CALL CNVS2U(IEStr,DIEstr,x,dx)
            CALL CNVS2U(ECArd(22:29),ECArd(30:31),y,dy)
            ie2ti = (x-y)/x
         END IF
         die2ti = 0.01*ie2ti
      ELSE
         CALL CNVS2U(IEStr,DIEstr,x,dx)
         IF(dx.EQ.0.0) dx = 0.05*x
         CALL CNVS2U(TIStr,DTIstr,y,dy)
         IF(dy.EQ.0) dy = 0.05*y
         ie2ti = x/y
         die2ti = 0.0
         IF(TYPSTR(DIEstr).EQ.2.OR.TYPSTR(DTIstr).EQ.2) THEN
            die2ti = 0.5*ie2ti
         ELSE IF(x.GT.0.0) THEN
            die2ti = ie2ti*SQRT((dx/x)**2+(dy/y)**2)
         ELSE
            die2ti = ie2ti*(dy/y)
         END IF
      END IF
      test1 = ABS(ie2ti-ECCapt)
      test2 = SQRT(DECcapt**2+die2ti**2)
      IF(test1.GT.test2) THEN
         IF(test2.EQ.0.) THEN
            i = 0
            IF(test1.GT.0.03*AMAX1(ie2ti,ECCapt)) i = 3
            IF(test1.GT.0.10*AMAX1(ie2ti,ECCapt)) i = 5
         ELSE
            i = 1
            DO WHILE (test1.GT.i*test2)
               i = i + 1
               IF(i.LT.6) CYCLE
               EXIT
            END DO
         END IF
         IF(i.GT.2) THEN
            message = 'Capt. fract. incons.'
            IF(i.LE.4) THEN
               IF(NOWarn) THEN
                  message = ' '
               ELSE
                  message = '<W> '//message
               END IF
               TOTwar = TOTwar + 1
            END IF
            IF(i.GT.4) THEN
               IF(NOErr) THEN
                  message = ' '
               ELSE
                  message = '<E> '//message
               END IF
               TOTerr = TOTerr + 1
            END IF
            IF(message.NE.' ') THEN
               WRITE(out,'(1X,I5,2A)') ENUm, '.  ', ECArd
               WRITE(out,'(1X,I5,2A)') SENum, '.  ', SECard
               WRITE(out,'(91X,A)') TRIM(message)
               CALL CNVU2S(ie2ti,die2ti,xstr,LEN(xstr),dxstr,-2)
               CALL LBSUP(xstr)
               WRITE(out,'(91X,A)')' IE/TI='//TRIM(xstr)
               CALL CNVU2S(ECCapt,DECcapt,xstr,LEN(xstr),dxstr,-2)
               CALL LBSUP(xstr)
               WRITE(out,'(91X,A)') 'CK+...='//TRIM(xstr)
               GO TO 10
            END IF
         END IF
      END IF
      GO TO 20
   10 IF(DOEavchk) THEN
         IF(ECArd.EQ.B_Ecard) b_eout = .FALSE.
         IF(SECard.EQ.SB_ecard) sb_eout = .FALSE.
         CALL CKEAV(b_eout,sb_eout)
      END IF
!
!     Done - Reset
!
   20 DOEcchk = .FALSE.
      ECCapt = 0.0
      DECcapt = 0.0
      IEStr = ' '
      DIEstr = ' '
      TIStr = ' '
      DTIstr = ' '
      ECArd = ' '
      SECard = ' '
!
      RETURN
      END SUBROUTINE CKECCONS
!
!***********************************************************************
!
      SUBROUTINE ECSTORE(Card)
!
!     Store what has been found on the "S E" record
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      CHARACTER(LEN=*) :: Card
!
!     Functions used
!
      INTEGER(KIND=4), INTRINSIC :: INDEX, LEN_TRIM
      INTEGER(KIND=4), EXTERNAL :: INDEXF
      REAL(KIND=4), INTRINSIC :: SQRT
!
!     Local variables
!
      CHARACTER(LEN=20) :: dxstr, xstr
      CHARACTER(LEN=60) :: tmpstr
      LOGICAL(KIND=4) :: reset
      INTEGER(KIND=4) :: i, j, k
      REAL(KIND=4) :: dx, x
!
      IF(.NOT.DOEcchk) RETURN
      IF(SECard.NE.' ') THEN
         IF(SECard(10:).EQ.Card(10:)) THEN
            CALL WRTMSG('<E> Duplicates previous "S E" record',10,80)
            RETURN
         END IF
         IF(ECCapt.GT.0 .AND. Indexf(card,10,'$C').GT.0) THEN
            TOTwar = TOTwar + 1
            IF(.NOT.NOWarn) THEN
               WRITE(out,'(1X,I5,2A)') SENum, '.  ', SECard
               WRITE(out,'(1X,I5,2A)') NSEq, '.  ', Card
               WRITE(out,'(91X,A)')'<W> Possible duplication of "S E"'
            END IF
         END IF
      END IF
      reset = .FALSE.
      SENum = NSEq
      SECard = Card
      i = 10
      DO WHILE (.TRUE.)
         i = INDEXF(Card,i,'C')
         IF(i.EQ.0) THEN
            IF(reset) THEN
               GO TO 20
            END IF
            RETURN
         END IF
         IF(Card(i-1:i-1).NE.'$'.AND.Card(i-1:i-1).NE.' ') THEN
            i = i + 1
            CYCLE
         END IF
         tmpstr = Card(i:)
         j = INDEX(tmpstr,'$')
         IF(j.GT.0) THEN
            j = j - 1
         ELSE
            j = LEN_TRIM(tmpstr)
         END IF
         tmpstr = tmpstr(1:j)
         k = LEN_TRIM(tmpstr)
         i = i + 1
         IF(INDEX(tmpstr,'/T').GT.0) CYCLE
         j = INDEX(tmpstr,'=')
         IF(j.EQ.0) THEN
            reset = .TRUE.
            CYCLE
         END IF
         SENum = NSEq
         SECard = Card
         tmpstr = tmpstr(j+1:)
         CALL LBSUP(tmpstr)
         j = INDEX(tmpstr,' ')
         IF(j.EQ.0) THEN
            xstr = tmpstr
            dxstr = ' '
         ELSE
            xstr = tmpstr(1:j-1)
            dxstr = tmpstr(j+1:)
         END IF
         CALL LBSUP(xstr)
         CALL LBSUP(dxstr)
         CALL CNVS2U(xstr,dxstr,x,dx)
         IF(x.GT.1) CALL WRTMSG('<E> Capt. fract.>1',i-1,i-2+k)
         ECCapt = ECCapt + x
         DECcapt = SQRT(DECcapt**2+dx**2)
      END DO
      RETURN
!
!     Done - Reset
!
   20 DOEcchk = .FALSE.
      ECCapt = 0.0
      DECcapt = 0.0
      IEStr = ' '
      DIEstr = ' '
      TIStr = ' '
      DTIstr = ' '
      ECArd = ' '
      SECard = ' '
!
      RETURN
      END SUBROUTINE ECSTORE
!
!***********************************************************************
!
      SUBROUTINE CKEAV(B_eout,Sb_eout)
!
!     Check for consistency on IB given on B or E record and EAV given
!     on "S B" or "S E" record
!
      IMPLICIT NONE
!
!     Dummy arguments
!
      LOGICAL(KIND=4) :: B_eout, Sb_eout
!
!     Functions used
!
!2015zv      CHARACTER(LEN=*), INTRINSIC :: TRIM
!
!     Local variables
!
      CHARACTER(LEN=38) :: message
      REAL(KIND=4) :: dx, x
!
      IF(.NOT.IBPres.AND..NOT.EAVpres) GO TO 10
      message = ' '
      CALL CNVS2U(B_Ecard(22:29),B_Ecard(30:31),x,dx)
      IF(x.GT.0.0.AND..NOT.EAVpres) THEN
         TOTerr = TOTerr + 1
         IF(.NOT.NOErr) message = '<E> IB>0 but no EAV'
      END IF
      IF(EAVpres) THEN
         IF(.NOT.IBPres) THEN
            TOTwar = TOTwar + 1
            IF(.NOT.NOWarn) message = '<W> IB=" " but EAV given'
         ELSE IF(x.EQ.0) THEN
            TOTwar = TOTwar + 1
            IF(.NOT.NOWarn) message = '<W> IB=0 but EAV given'
         END IF
      END IF
      IF(message.NE.' ') THEN
         IF(B_eout) WRITE(out,'(1X,I5,2A)') B_Enum, '.  ', B_Ecard
         IF(SB_ecard.NE.' '.AND.Sb_eout) THEN
            WRITE(out,'(1X,I5,2A)') SB_enum, '.  ', SB_ecard
         END IF
         WRITE(out,'(91X,A)') TRIM(message)
      END IF
!
!     Done - reset
   10 DOEavchk = .FALSE.
      IBPres = .FALSE.
      EAVpres = .FALSE.
      B_Ecard = ' '
      SB_ecard = ' '
!
      RETURN
      END SUBROUTINE CKEAV
!
      Subroutine CHKUNP
!
!     Checks for duplicate unplaced gamma energies without proper
!       flags
!
      IMPLICIT NONE
!
!     Functions used
!
      LOGICAL(KIND=4), INTRINSIC :: INDEX
!
!     Local variables
!
      INTEGER(KIND=4) :: i,j,nlines
      CHARACTER(LEN=80) :: gcard1,gcard2
      CHARACTER(LEN=10) :: e1,e2
      CHARACTER(LEN=1) :: f1,f2
      CHARACTER(LEN=3) :: mltchr
      DATA mltchr/'*&@'/
!
      If(ndxg .LE. 1)Return
      nlines=3
      Do i=2,ndxg
         Read(UNIT=tmp,FMT='(A)',REC=recnum(i))gcard1
         e1=gcard1(10:19)
         Call Lbsup(e1)
	 f1=gcard2(77:77)
	 Do j=i-1,1,-1
            Read(UNIT=tmp,FMT='(A)',REC=recnum(j))gcard2
            e2=gcard2(10:19)
            f2=gcard2(77:77)
            Call Lbsup(e2)
	    If(e1 .EQ. e2)Then
               If(INDEX(mltchr,f1).EQ.0                                 &
     &           .OR. INDEX(mltchr,f2).EQ.0)Then
                  toterr=toterr+1
		  If(.NOT.noerr)Then
                     Call Wrtchk(nlines)
		     WRITE(out,'(1X,I5,2A)')RECdxg(j), '.  ', gcard2
		     WRITE(out,'(1X,I5,2A)')RECdxg(i), '.  ', gcard1
                     WRITE(out,'(18X,A,T92,A)')'**********',            &
     &                 '<E> Identical unplaced gamma energies'
		  EndIf
	       EndIF
	    EndIf
	 EndDo
      EndDo
!
      End SUBROUTINE CHKUNP
!
      Subroutine Ckrange(Field,From,To)
!
!     Checks for proper ordering of ranges on a continuation record
!
      IMPLICIT NONE
!
!     Dummy variables
!
      CHARACTER(LEN=*) :: Field
      INTEGER(KIND=4) :: From,To
!
!     Functions used
!
      INTEGER(KIND=4), INTRINSIC :: INDEX,LEN_TRIM,MIN0
      INTEGER(KIND=4), EXTERNAL :: RLSCN
!
!     Local variables
!
      INTEGER(KIND=4) :: i,j
      INTEGER(KIND=4) :: lowpos,higpos,endpos
      CHARACTER(LEN=10) :: quant 
      CHARACTER(LEN=3) :: units
      REAL(KIND=4), DIMENSION(2) :: value
!
      INTEGER(KIND=4), PARAMETER :: maxop=2
      CHARACTER(LEN=40), DIMENSION(maxop) :: valstr
      CHARACTER(LEN=2), DIMENSION(maxop) :: lop
      DATA lop/ 'LT','LE'/
      CHARACTER(LEN=2), DIMENSION(maxop) :: gop
      DATA gop/ 'GT','GE'/
!
      CHARACTER(LEN=3), DIMENSION(17) :: tunits
      DATA tunits/'AS ', 'FS ', 'PS ', 'NS ', 'US ', 'MS ', 'S  ',      &       
     &     'M  ', 'H  ', 'D  ', 'Y  ', 'KY ', 'MY ', 'GY ', 'EV ',      &       
     &     'KEV', 'MEV'/
!
!     1 YEAR IS 365.256 DAYS
!
!     T1/2(S)=LN(2)*HBAR/WIDTH(EV) WHERE
!     LN(2)*HBAR=C=4.5624*10**-16 EV*S --> 1/C=2.1918*10**15
!
      REAL(KIND=4), DIMENSION(17) :: tfact
      DATA tfact/1.E-18, 1.E-15, 1.E-12, 1.E-9, 1.E-6, 1.E-3, 1., 60.,  &       
     &     3600., 8.64E+4, 3.155812E+7, 3.155812E+10, 3.155812E+13,     &       
     &     3.155812E+16, 2.1918E+15, 2.1918E+18, 2.1918E+21/
!
      lowpos=0
      higpos=0
      Do i=1,maxop
         If(lowpos .EQ. 0)lowpos=INDEX(field,lop(i))
      EndDo
      Do i=1,maxop
         If(higpos .EQ. 0)higpos=INDEX(field,gop(i))
      EndDo
!
      If(.NOT.(lowpos.GT.0 .AND. higpos.GT.0))RETURN
!
      quant=Field(1:MIN0(lowpos,higpos)-1)
      If(lowpos .LT. higpos)Then
         endpos=higpos-1
      Else
         endpos=LEN_TRIM(Field)
      EndIf
      i=lowpos+3
      valstr(1)=Field(i:endpos)
      If(higpos .LT. lowpos)Then
         endpos=lowpos-1
      Else
         endpos=LEN_TRIM(Field)
      EndIf
      i=higpos+3
      valstr(2)=field(i:endpos)
      If(quant(1:1) .EQ. 'T')Then
         Do i=1,2
	    j=Rlscn(valstr(i),1,value(i))
	    units=valstr(i)(j:)
            Call Lbsup(units)
	    j=INDEX(units,' ')
	    If(j .GT. 0)units=units(1:j)
            Do j=1,17
               If(units .EQ. tunits(j))Then
	          value(i)=tfact(j)*value(i)
                  GoTo 100
               EndIf
	    EndDo
100         Continue
	 EndDo
      Else
         Do i=1,2
	    j=Rlscn(valstr(i),1,value(i))
         EndDo        
      EndIf
      If(value(1) .EQ. value(2))Then
         Call Wrtmsg('<W> Limits are equal',                            &
     &     From+MIN0(lowpos,higpos)-1,To)
      Elseif(value(1) .LT. value(2))Then
         Call Wrtmsg('<E> Upper limit less than lower limit',           &
     &     From+MIN0(lowpos,higpos)-1,To)
      EndIf
!
      Return
!
      End Subroutine Ckrange
!
      Subroutine GetOut(str,plerr,mierr)
!
!     Attempts to decode the outgoing in a reaction
!
      IMPLICIT NONE
!
!     Dummy variables
!
      CHARACTER(LEN=*) :: str
      LOGICAL(KIND=4) :: plerr,mierr
!
!     Functions used
!
      INTEGER(KIND=4), INTRINSIC :: INDEX, LEN_TRIM
      INTEGER(KIND=4), EXTERNAL :: Typstr
      REAL(KIND=4), EXTERNAL :: Valstr
!
!     Local variables
!
      INTEGER(KIND=4) :: i,j
      INTEGER(KIND=4) :: ibegin,iend
      INTEGER(KIND=4) :: savea,savez,tempa,tempz
      INTEGER(KIND=4) :: checka,checkz
      LOGICAL(KIND=4) :: sameio
      REAL(KIND=4) :: mult
      CHARACTER(LEN=39) :: work
!
      INTEGER(KIND=4), PARAMETER :: maxout=10
      INTEGER(KIND=4) :: nout
      CHARACTER(LEN=5), DIMENSION(maxout) :: outs
!
      outa=-9999
      outz=-9999
      plerr=.FALSE.
      mierr=.FALSE.
      sameio=.FALSE.
      tempa=0
      tempz=0
!
      work=str
      i=LEN_TRIM(work)
      If(work(i:i) .EQ. 'X')Return
      If(work.EQ.'XNG' .OR. work.EQ.'XG')Return
      i=LEN_TRIM(work)
      If(work(i-1:i) .EQ. '''G')work=work(1:i-2)
      i=LEN_TRIM(work)
      If(work(i:i) .EQ. '''')work=work(1:i-1)      
      i=LEN_TRIM(work)
      If(work(i:i).EQ.'G' .AND. i.GT.1)Then
         Call Izel(work(i-1:i),checkz)
         If(checkz .LT. 0)Then
            work=work(1:i-1)
	    i=LEN_TRIM(work)
	 ElseIf(i .GT. 2)Then
	    If(Typstr(work(i-2:i-2)) .EQ. 2)Then
               work=work(1:i-1)
	       i=LEN_TRIM(work)
            Else
	       ibegin=i-2
	       iend=i-2
               Do While (Typstr(work(ibegin:ibegin)).EQ.1                &
     &           .AND. ibegin.GT.0)
	          ibegin=ibegin-1
	       EndDo  
               ibegin=ibegin+1
	       checka=Valstr(work(ibegin:iend))
	       If(checka .LE. checkz)Then
	          work=work(1:i-1)
		  i=LEN_TRIM(work)
	       EndIf
	    EndIf
	 EndIf
      EndIf
      If(work(1:4) .EQ. 'POL ')work=work(5:)
      i=LEN_TRIM(work)
!
      j=LEN_TRIM(incnuc)
      If((incnuc(1:2).EQ.'PI' .OR. incnuc(1:2).EQ.'NU')                 &
     &  .AND. (work(1:j) .EQ. incnuc))Then
         sameio=.TRUE.
      ElseIf(work(1:j).EQ.incnuc                                        &
     &  .AND. .NOT.(work(1:2).EQ.'PI' .OR. work(1:2).EQ.'NU')           &
     &  .AND. .NOT.(work(j+1:j+1).EQ.'+' .OR. work(j+1:j+1).EQ.'-'))Then
         sameio=.TRUE.
      EndIf
      If(sameio)Then
         coma=inca
         comz=incz
         inca=0
	 incz=0
         If(i .EQ. j)Then
	    outa=0
	    outz=0
	    Return
	 Else
	    work=work(j+1:)
            If(work(1:1) .EQ. '''')work=work(2:)
	 EndIf
      EndIf      
      Call Lbsup(resnuc)
      j=LEN_TRIM(resnuc)
      If(work(1:j).EQ.resnuc(1:j)                                       &
     &  .AND. .NOT.(work(j+1:j+1).EQ.'+'.OR.work(j+1:j+1).EQ.'-'))Return
!
50    Continue
      IF(work.EQ.'X') THEN
         Return
      ELSE IF(work.EQ.'G') THEN
         outa=0
	 outz=0
      ELSE IF(work.EQ.'N') THEN
         If(outa .EQ. -9999)Then
            outa=1
	    outz=0
	 Else
            outa=tempa+1
            outz=tempz
         EndIf
      ELSE IF(work.EQ.'P') THEN
         If(outa .EQ. -9999)Then
            outa=1
	    outz=1
	 Else
            outa=tempa+1
            outz=tempz+1
         EndIf
      ELSE IF(work.EQ.'D') THEN
         If(outa .EQ. -9999)Then
            outa=2
	    outz=1
	 Else
            outa=tempa+2
            outz=tempz+1
         EndIf
      ELSE IF(work.EQ.'T') THEN
         If(outa .EQ. -9999)Then
            outa=3
	    outz=1
	 Else
            outa=tempa+3
            outz=tempz+1
         EndIf
      ELSE IF(work.EQ.'A') THEN
         If(outa .EQ. -9999)Then
            outa=4
	    outz=2
	 Else
            outa=tempa+4
            outz=tempz+2
         EndIf
      ELSE IF(work.EQ.'KAPPA') THEN
      ELSE IF(work.EQ.'KAPPA+') THEN
         outa=0
	 outz=1
      ELSE IF(work.EQ.'KAPPA-') THEN
         outa=0
	 outz=-1
      ELSE IF(work.EQ.'MU') THEN
      ELSE IF(work.EQ.'MU+') THEN
         outa=0
	 outz=1
      ELSE IF(work.EQ.'MU-') THEN
         outa=0
	 outz=-1
      ELSE IF(work.EQ.'MU0') THEN
         outa=0
	 outz=0
      ELSE IF(work.EQ.'PI') THEN
      ELSE IF(work.EQ.'PI+') THEN
         outa=0
	 outz=1
      ELSE IF(work.EQ.'PI-') THEN
         outa=0
	 outz=-1
      ELSE IF(work.EQ.'NU') THEN
         outa=0
	 outz=0
      ELSE IF(work.EQ.'K') THEN
      ELSE IF(work.EQ.'K+') THEN
         outa=0
	 outz=1
      ELSE IF(work.EQ.'K-') THEN
         outa=0
	 outz=-1
      ELSE IF(work.EQ.'E') THEN
      ELSE IF(work.EQ.'E+') THEN
         outa=0
	 outz=1
      ELSE IF(work.EQ.'E-') THEN
         outa=0
	 outz=-1
      ElseIf(INDEX(work,'+')+INDEX(work,'-') .GT. 0)Then
         i=INDEX(work,'+')
         If(i .EQ. 0)Then
	    mierr=.TRUE.
	    i=INDEX(work,'-')
	 ElseIf(INDEX(work,'-') .GT. 0)Then
	    If(INDEX(work,'-') .LT. i)Then
	       mierr=.TRUE.
	       i=INDEX(work,'-')
            Else
               plerr=.TRUE.
	    EndIf
         Else
            plerr=.TRUE.
	 EndIf
         If(i .GT. 1)i=i-1
         If(work(1:i) .EQ.'KAPPA')Then
	    tempa=0
            If(plerr)Then
               tempz=1
            Else
	       tempz=-1
	    EndIf
            work=work(i+2:)
            plerr=.FALSE.
            mierr=.FALSE.
	    GoTo 50
	 EndIf
         If(work(1:i) .EQ.'MU')Then
	    tempa=0
            If(plerr)Then
               tempz=1
            Else
	       tempz=-1
	    EndIf
            work=work(i+2:)
            plerr=.FALSE.
            mierr=.FALSE.
	    GoTo 50
	 EndIf
	 If(work(1:i) .EQ.'PI')Then
	    tempa=0
            If(plerr)Then
               tempz=1
            Else
	       tempz=-1
	    EndIf
            work=work(i+2:)
            plerr=.FALSE.
            mierr=.FALSE.
	    GoTo 50
	 EndIf
	 If(work(1:i) .EQ.'K')Then
	    tempa=0
            If(plerr)Then
               tempz=1
            Else
	       tempz=-1
	    EndIf
            work=work(i+2:)
            plerr=.FALSE.
            mierr=.FALSE.
	    GoTo 50
	 EndIf
	 If(work(1:i) .EQ.'E')Then
	    tempa=0
            If(plerr)Then
               tempz=1
            Else
	       tempz=-1
	    EndIf
            work=work(i+2:)
            plerr=.FALSE.
            mierr=.FALSE.
	    GoTo 50
	 EndIf
         Return
      ElseIf(Typstr(work) .EQ. 2)Then
         If(INDEX(work,'X').GT.0 .OR. INDEX(work,'Y').GT.0 .OR.         &
     &     INDEX(work,'Y').GT.0)RETURN
         Do i=1,LEN_TRIM(work)
            IF(work(i:i).EQ.'N') THEN
               tempa=1
               tempz=0
            ELSE IF(work(i:i).EQ.'P') THEN
               tempa=1
               tempz=1
            ELSE IF(work(i:i).EQ.'D') THEN
               tempa=2
	       tempz=1
            ELSE IF(work(i:i).EQ.'T') THEN
               tempa=3
               tempz=1
            ELSE IF(work(i:i).EQ.'A') THEN
               tempa=4
               tempz=2
	    Else
	       outa=-9999
               outz=-9999
	       Return
            EndIf
            If(outa .EQ. -9999)Then
	       outa=tempa
	    Else
	       outa=outa+tempa
	    EndIf
            If(outz .EQ. -9999)Then
	       outz=tempz
	    Else
	       outz=outz+tempz
	    EndIf
	 EndDo
      Else
         nout=0
         ibegin=LEN_TRIM(work)
	 iend=ibegin
	 Do While(iend .GE. 1)
            Do While(Typstr(work(ibegin:ibegin)) .EQ. 2)
	       ibegin=ibegin-1
	    EndDo
	    Do WHILE(Typstr(work(ibegin:ibegin)).EQ.1)
	       ibegin=ibegin-1
	    EndDo
	    nout=nout+1
	    outs(nout)=work(ibegin+1:iend)
            If(ibegin .LE. 0)Then
	       EXIT
	    Else
               iend=ibegin
	    EndIf
	 EndDo
         Do i=1,nout
            If(LEN_TRIM(outs(i)) .EQ. 1)Then
               IF(outs(i).EQ.'N') THEN
                  tempa=1
	          tempz=0
               ELSE IF(outs(i).EQ.'P') THEN
                  tempa=1
	          tempz=1
               ELSE IF(outs(i).EQ.'D') THEN
                  tempa=2
	          tempz=1
               ELSE IF(outs(i).EQ.'T') THEN
                  tempa=3
	          tempz=1
               ELSE IF(outs(i).EQ.'A') THEN
                  tempa=4
                  tempz=2
	       Else
	          outa=-9999
		  outz=-9999
		  Return
	       EndIf
	    Else
100            Continue
	       Call Nucid(outs(i)(1:LEN_TRIM(outs(i))),tempa,tempz)
               If(tempa.EQ.0)Then
	          outa=-9999
	          outz=-9999
	          Return
	       Endif
               If(tempz.LT.0)Then
                  j=LEN_TRIM(outs(i))
                  mult=1
		  If(Typstr(outs(i)(1:j-1)) .EQ. 1)Then
		     mult=Valstr(outs(i)(1:j-1))
		     outs(i)=outs(i)(j:j)
                     j=LEN_TRIM(outs(i))
		  EndIf
		  If(outs(i)(j:j) .EQ. 'N')Then
                     tempa=mult
		     tempz=0
		  ElseIf(outs(i)(j:j) .EQ. 'P')Then
                     tempa=mult
		     tempz=mult
		  ElseIf(outs(i)(j:j) .EQ. 'D')Then
                     tempa=2.0*mult
		     tempz=mult
		  ElseIf(outs(i)(j:j) .EQ. 'T')Then
                     tempa=3.0*mult
		     tempz=mult
		  ElseIf(outs(i)(j:j) .EQ. 'A')Then
                     tempa=4.0*mult
		     tempz=2.0*mult
                  Else
  	             outa=-9999
	             outz=-9999
	             Return
	          Endif
                  If(outa .EQ. -9999)Then
	             outa=tempa
	          Else
	             outa=outa+tempa
	          EndIf
                  If(outz .EQ. -9999)Then
	             outz=tempz
	          Else
	             outz=outz+tempz
	          EndIf
                  If(j-1 .GT. 1)Then
                     outs(i)=outs(i)(1:j-1)
                     GoTo 100
                  Else
		     GoTo 1000
		  EndIf
	       EndIf
	    EndIf
            If(tempz.EQ.15 .AND. tempa.LE.20)Then
	       tempz=tempa
            ElseIf(tempa .LE. tempz)Then
                savea=tempa
                savez=tempz
                mult=1
		iend=1
		Do While(Typstr(outs(i)(iend:iend)) .EQ. 1)
		   iend=iend+1
		EndDo
                ibegin=iend
		iend=iend-1
		If(iend .GT. 0)Then
		   mult=Valstr(outs(i)(1:iend))
                   If(outs(i)(ibegin:ibegin) .EQ. 'N')Then
                      tempa=mult
		      tempz=0
		   Elseif(outs(i)(ibegin:ibegin) .EQ. 'P')Then
                      tempa=mult
		      tempz=mult
		   Elseif(outs(i)(ibegin:ibegin) .EQ. 'D')Then
                      tempa=2.0*mult
		      tempz=mult
		   Elseif(outs(i)(ibegin:ibegin) .EQ. 'T')Then
                      tempa=3.0*mult
		      tempz=mult
		   Elseif(outs(i)(ibegin:ibegin) .EQ. 'A')Then
                      tempa=4.0*mult
		      tempz=2.0*mult
                   Else
                      GoTo 900
		   EndIf
                   outs(i)=outs(i)(ibegin+1:)
                   If(outs(i) .EQ. 'N')Then
                      tempa=tempa+1
		   Elseif(outs(i) .EQ. 'P')Then
                      tempa=tempa+1
		      tempz=tempz+1
		   Elseif(outs(i) .EQ. 'D')Then
                      tempa=tempa+2
		      tempz=tempz+1
		   Elseif(outs(i) .EQ. 'T')Then
                      tempa=tempa+3
		      tempz=tempz+1
		   Elseif(outs(i) .EQ. 'A')Then
                      tempa=tempa+4
		      tempz=tempz+2
                   Else
		      tempa=savea
		      tempz=savez
		   EndIf
		EndIf		  
	    EndIf
900         Continue
            If(outa .EQ. -9999)Then
	       outa=tempa
	    Else
	       outa=outa+tempa
	    EndIf
            If(outz .EQ. -9999)Then
	       outz=tempz
	    Else
	       outz=outz+tempz
	    EndIf
1000        Continue
	 EndDo
      EndIf
!
      End Subroutine GetOut
!
      END PROGRAM FMTCHK
