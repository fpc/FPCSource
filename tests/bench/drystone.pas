PROGRAM Dhrystone( input, output );

  uses
    timer;

{
 *   "DHRYSTONE" Benchmark Program
 *
 *   Version:   Mod2/1
 *   Date:      05/03/86
 *   Author:      Reinhold P. Weicker,  CACM Vol 27, No 10, 10/84 pg. 1013
 *         C version translated from ADA by Rick Richardson
 *         Every method to preserve ADA-likeness has been used,
 *         at the expense of C-ness.
 *         Modula-2 version translated from C by Kevin Northover.
 *         Again every attempt made to avoid distortions of the original.
 *   Machine Specifics:
 *         The LOOPS constant is initially set for 50000 loops.
 *         If you have a machine with large integers and is
 *         very fast, please change this number to 500000 to
 *         get better accuracy.
 *
 **************************************************************************
 *
 *   The following program contains statements of a high-level programming
 *   language (Modula-2) in a distribution considered representative:
 *
 *   assignments         53%
 *   control statements      32%
 *   procedure, function calls   15%
 *
 *   100 statements are dynamically executed.  The program is balanced with
 *   respect to the three aspects:
 *      - statement type
 *      - operand type (for simple data types)
 *      - operand access
 *         operand global, local, parameter, or constant.
 *
 *   The combination of these three aspects is balanced only approximately.
 *
 *   The program does not compute anything meaningfull, but it is
 *   syntactically and semantically correct.
 *
 }

{$R- range checking off}


CONST

{    Set LOOPS to specify how many thousand drystones to perform.
      LOOPS = 50 will perforum 50,000 drystones. Choose longer for
      better precision and for fast machines.
}

  LOOPS =  5000;      { Use this for slow or 16 bit machines }
  Ident1 = 1;
  Ident2 = 2;
  Ident3 = 3;
  Ident4 = 4;
  Ident5 = 5;

type integer = longint;
Type Enumeration = INTEGER;
{ TYPE Enumeration = (Ident1, Ident2, Ident3, Ident4, Ident5); }

TYPE   OneToThirty   = INTEGER;
TYPE   OneToFifty    = INTEGER;
TYPE   CapitalLetter = CHAR;
TYPE   String30      = STRING[30]; { ARRAY[0..30] OF CHAR; }
TYPE   Array1Dim     = ARRAY[0..50] OF INTEGER;
TYPE   Array2Dim     = ARRAY[0..50,0..50] OF INTEGER;

{ TYPE   RecordPtr     = ^RecordType; }
       RecordType    = RECORD
                         PtrComp    : integer;
                         Discr      : Enumeration;
                         EnumComp   : Enumeration;
                         IntComp    : OneToFifty;
                         StringComp : String30;
                       END;

{
 * Package 1
 }
VAR
  IntGlob    : INTEGER;
  BoolGlob   : BOOLEAN;
  Char1Glob  : CHAR;
  Char2Glob  : CHAR ;
  Array1Glob : Array1Dim;
  Array2Glob : Array2Dim;
  MyRec      : array[0..2] of RecordType;
{  PtrGlb     : RecordPtr; }
{  PtrGlbNext : RecordPtr; }

  Hour, Min, Sec, Hund : word;
  TStart, TEnd : real;

CONST
  PtrGlb     = 1;
  PtrGlbNext = 2;

PROCEDURE Proc7(IntParI1, IntParI2 : OneToFifty; VAR IntParOut : OneToFifty);
VAR
   IntLoc  : OneToFifty;
BEGIN
   IntLoc:= IntParI1 + 2;
   IntParOut:= IntParI2 + IntLoc;
END ;

PROCEDURE Proc3( var inRecIdx : integer );
BEGIN
   IF ( inRecIdx <> 0 ) THEN
      inRecIdx := MyRec[PtrGlb].PtrComp
   ELSE
      IntGlob:= 100;
   Proc7( 10, IntGlob, MyRec[PtrGlb].IntComp);
END ;

FUNCTION Func3(EnumParIn : Enumeration) : BOOLEAN;
  VAR EnumLoc: Enumeration;
BEGIN
   EnumLoc:= EnumParIn;
   Func3:= EnumLoc = Ident3;
END ;

PROCEDURE Proc6(EnumParIn : Enumeration; VAR EnumParOut : Enumeration);
BEGIN
   EnumParOut:= EnumParIn;
   IF (NOT Func3(EnumParIn) ) THEN
      EnumParOut:= Ident4;
   CASE EnumParIn OF
    Ident1:   EnumParOut:= Ident1 ;
    Ident2:   IF (IntGlob > 100) THEN EnumParOut:= Ident1
                                 ELSE EnumParOut:= Ident4;
    Ident3:   EnumParOut:= Ident2 ;
    Ident4:   ;
    Ident5:   EnumParOut:= Ident3;
   END;
END ;


PROCEDURE Proc1( inIdx : integer );
var
   i : integer;
BEGIN
   i := MyRec[inIdx].PtrComp;

   MyRec[i] := MyRec[PtrGlb];
   MyRec[inIdx].IntComp := 5;
   MyRec[i].IntComp:= MyRec[inIdx].IntComp;
   MyRec[i].PtrComp:= i;
   Proc3( MyRec[i].PtrComp );
   IF ( MyRec[i].Discr = Ident1 ) THEN
      BEGIN
         MyRec[i].IntComp:= 6;
         Proc6( MyRec[inIdx].EnumComp, MyRec[i].EnumComp );
         MyRec[i].PtrComp:= MyRec[PtrGlb].PtrComp;
         Proc7( MyRec[i].IntComp, 10, MyRec[i].IntComp );
      END
   ELSE
      MyRec[inIdx] := MyRec[i];
END;


PROCEDURE Proc2(VAR IntParIO : OneToFifty);
VAR
   IntLoc  : OneToFifty;
   EnumLoc : Enumeration;
BEGIN
   IntLoc:= IntParIO + 10;
   REPEAT
     IF (Char1Glob = 'A') THEN
      BEGIN
         IntLoc:= IntLoc - 1;
         IntParIO:= IntLoc - IntGlob;
         EnumLoc:= Ident1;
      END;
   UNTIL EnumLoc = Ident1;
END ;

PROCEDURE Proc4;
VAR
   BoolLoc : BOOLEAN;
BEGIN
   BoolLoc:= Char1Glob = 'A';
   BoolLoc:= BoolLoc OR BoolGlob;
   Char2Glob:= 'B';
END ;

PROCEDURE Proc5;
BEGIN
   Char1Glob:= 'A';
   BoolGlob:= FALSE;
END ;

PROCEDURE Proc8(VAR Array1Par : Array1Dim; VAR Array2Par : Array2Dim;
      IntParI1, IntParI2 : OneToFifty);
VAR
   IntLoc   : OneToFifty;
   IntIndex : OneToFifty;
BEGIN
   IntLoc:= IntParI1 + 5;
   Array1Par[IntLoc]:= IntParI2;
   Array1Par[IntLoc+1]:= Array1Par[IntLoc];
   Array1Par[IntLoc+30]:= IntLoc;
   FOR IntIndex:= IntLoc TO (IntLoc+1) DO
      Array2Par[IntLoc,IntIndex]:= IntLoc;
   { Array2Par[IntLoc,IntLoc-1]:= Array2Par[IntLoc,IntLoc-1] + 1; }
   Array2Par[IntLoc+20,IntLoc]:= Array1Par[IntLoc];
   IntGlob:= 5;
END ;

FUNCTION Func1(CharPar1, CharPar2 : CapitalLetter) : Enumeration;
VAR
   CharLoc1, CharLoc2 : CapitalLetter;
BEGIN
   CharLoc1:= CharPar1;
   CharLoc2:= CharLoc1;
   IF (CharLoc2 <> CharPar2) THEN
      Func1:= (Ident1)
   ELSE
      Func1:= (Ident2);
END ;

FUNCTION Func2(VAR StrParI1, StrParI2 : String30) : BOOLEAN;
VAR
   IntLoc   : OneToThirty;
   CharLoc  : CapitalLetter;
BEGIN
   IntLoc := 2;
   WHILE (IntLoc <= 2) DO
    BEGIN
     IF (Func1(StrParI1[IntLoc], StrParI2[IntLoc+1]) = Ident1) THEN
       BEGIN
         CharLoc := 'A';
         IntLoc:= IntLoc + 1;
       END;
    END;
   IF (CharLoc >= 'W') AND (CharLoc <= 'Z') THEN IntLoc:= 7;
   IF CharLoc = 'X' THEN
     Func2:= TRUE
   ELSE IF StrParI1 > StrParI2 THEN
    BEGIN
     IntLoc:= IntLoc + 7;
     Func2:= TRUE;
    END
   ELSE
     Func2:= FALSE;
END ;


PROCEDURE Proc0;
VAR
   IntLoc1    : OneToFifty;
   IntLoc2    : OneToFifty;
   IntLoc3    : OneToFifty;
   CharLoc    : CHAR;
   CharIndex  : CHAR;
   EnumLoc    : Enumeration;
   String1Loc,
   String2Loc : String30;
   i,
   j          : INTEGER;

BEGIN
{
   NEW(PtrGlbNext);
   NEW(PtrGlb);
}

   MyRec[PtrGlb].PtrComp:= PtrGlbNext;
   MyRec[PtrGlb].Discr:= Ident1;
   MyRec[PtrGlb].EnumComp:= Ident3;
   MyRec[PtrGlb].IntComp:= 40;
   MyRec[PtrGlb].StringComp := 'DHRYSTONE PROGRAM, SOME STRING';

   String1Loc := 'DHRYSTONE PROGRAM, 1''ST STRING';

FOR i := 1 TO LOOPS DO
  FOR j := 1 TO 1000 DO
  BEGIN
   Proc5;
   Proc4;
   IntLoc1:= 2;
   IntLoc2:= 3;
   String2Loc := 'DHRYSTONE PROGRAM, 2''ND STRING';
   EnumLoc:= Ident2;
   BoolGlob:= NOT Func2(String1Loc, String2Loc);
   WHILE (IntLoc1 < IntLoc2) DO
    BEGIN
      IntLoc3 := 5 * IntLoc1 - IntLoc2;
      Proc7(IntLoc1, IntLoc2, IntLoc3);
      IntLoc1:= IntLoc1 + 1;
    END;
   Proc8(Array1Glob, Array2Glob, IntLoc1, IntLoc3);
   Proc1(PtrGlb);
   CharIndex:= 'A';
   WHILE  CharIndex <= Char2Glob DO
     BEGIN
      IF (EnumLoc = Func1(CharIndex, 'C')) THEN
         Proc6(Ident1, EnumLoc);
      { CharIndex:= SUCC(CharIndex); }
      inc(byte(charindex));
     END;
   IntLoc3:= IntLoc2 * IntLoc1;
   IntLoc2:= IntLoc3 DIV IntLoc1;
   IntLoc2:= 7 * (IntLoc3 - IntLoc2) - IntLoc1;
   Proc2(IntLoc1);
 END;
END;

{ The Main Program is trivial }
BEGIN
   writeln( 'Start of Dhrystone benchmark' );
   start;
   Proc0;
   stop;
END.
