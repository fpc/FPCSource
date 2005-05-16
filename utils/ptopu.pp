Unit PtoPu;
{
    $Id: ptopu.pp,v 1.8 2005/02/21 07:59:10 michael Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt, member of
    the Free Pascal development team

    Pascal Pretty-Printer object implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
 This unit is based heavily on the code by

 Author:  Peter Grogono
   This program is based on a Pascal pretty-printer written by Ledgard,
   Hueras, and Singer.  See SIGPLAN Notices, Vol. 12, No. 7, July 1977,
   pages 101-105, and PP.DOC/HLP.
   This version of PP developed under Pascal/Z V4.0 or later.
   Very minor modifications for Turbo Pascal made by Willett Kempton
   March 1984 and Oct 84.  Runs under 8-bit Turbo or 16-bit Turbo.
   Toad Hall tweak, rewrite for TP 5, 28 Nov 89

The following was changed :
 - Object oriented
 - Uses streams
 - Run-time customizable.
}

Interface

Uses objects;

Const

  MAXSYMBOLSIZE = 65500;
  MAXSHOWSIZE = 40;

  MAXSTACKSIZE = 100;
  MAXKEYLENGTH = 15;     { The longest keywords are IMPLEMENTATION INITIALIZATION }
  MAXLINESIZE = 90;     { Maximum length of output line }

TYPE

  {Token    = String[MAXSYMBOLSIZE];}
  Token    = AnsiString;
{XXX this is not used  String0  = STRING[1];} {Pascal/z had 0}
  FileName = STRING;


  { Keysymbols }
  { If you add keysyms, adjust the definition of lastkey }
  keysymbol =  { keywords }
              (endsym,beginsym,ifsym,thensym,elsesym,procsym,varsym,ofsym,
               whilesym,dosym,casesym,withsym,forsym,repeatsym,untilsym,
               funcsym,labelsym,constsym,typesym,recordsym,stringsym,progsym,
               { TP and Delphi keywords}
               asmsym, trysym, finallysym,exceptsym,raisesym,classsym,objectsym,
               constructorsym,destructorsym,inheritedsym,propertysym,
               privatesym,publicsym,protectedsym,publishedsym,
               initializationsym,finalizationsym,
               inlinesym,librarysym,interfacesym,implementationsym,
               readsym,writesym,unitsym,
               { Not used for formatting }
               andsym,arrsym,divsym,downsym,filesym,gotosym,insym,modsym,
               notsym,nilsym,orsym,setsym,tosym,virtualsym,usessym,
               casevarsym,
               { other symbols }
               becomes,delphicomment,dopencomment,dclosecomment,opencomment,closecomment,semicolon,colon,equals,
               openparen,closeparen,period,endoffile,othersym);

  { Formatting options }
  { If you add options, adjust the definition of lastopt }
  options = (crsupp,crbefore,blinbefore,
             dindonkey,dindent,spbef,
             spaft,gobsym,inbytab,crafter,upper,lower,capital);

  optionset = SET OF options;
  keysymset = SET OF keysymbol;

  tableentry = RECORD
                 selected : optionset;
                 dindsym : keysymset;
                 terminators : keysymset
               END;

  { Character identification }

  charname = (letter,digit,space,quote,endofline,
              filemark,otherchar);

  charinfo = RECORD
               name : charname;
               Value : CHAR
             END;

  symbol = RECORD
             name : keysymbol;
             Value : Token;
             IsKeyWord : BOOLEAN;
             length, spacesbefore, crsbefore : INTEGER;
           END;

  symbolinfo = ^ symbol;

  stackentry = RECORD
                 indentsymbol : keysymbol;
                 prevmargin : INTEGER
               END;

  symbolstack = ARRAY [1..MAXSTACKSIZE] OF stackentry;

Const FirstOpt = crsupp;
      LastOpt = capital; { Adjust this if you add options }
      FirstKey = endsym;
      LastKey = othersym; { Adjust this if you add options }
      LastFormatsym = usessym;

Type
  tableptr = ^tableentry;
  optiontable = ARRAY [keysymbol] OF tableptr;
  OEntriesTable = Array [keysymbol] OF String[15];
  ONamesTable = Array [Options] of String[15];
  KeywordTable = ARRAY [endsym..lastFormatsym] OF String[MAXKEYLENGTH];
  SpecialChar = ARRAY [1..2] OF CHAR;
  dblcharset = SET OF endsym..othersym;
  DblCharTable = ARRAY [becomes..dclosecomment] OF SpecialChar;
  SglCharTable = ARRAY [opencomment..period] OF CHAR;

  TPrettyPrinter=Object(TObject)
    Private
      RecordSeen,
      CRPending : BOOLEAN;
      currchar,nextchar : charinfo;
      currsym,nextsym : symbolinfo;
      inlines,outlines : INTEGER;
      stack   : symbolstack;
      top,startpos,currlinepos,currmargin : Integer;
      option : OptionTable;
      Procedure Verbose (Const Msg : String);
      Procedure GetChar;
      Procedure StoreNextChar(VAR lngth: INTEGER;
                              VAR Value: Token);
      Procedure SkipBlanks(VAR spacesbefore, crsbefore: INTEGER);
      Procedure GetComment(sym: symbolinfo);
      Procedure GetDoubleComment(sym: symbolinfo);
      Procedure GetDelphiComment(sym: symbolinfo);
      Procedure GetNumber(sym: symbolinfo);
      Procedure GetCharLiteral(sym: symbolinfo);
      Function  char_Type: keysymbol;
      Procedure GetSpecialChar(sym: symbolinfo);
      Procedure GetNextSymbol(sym: symbolinfo);
      Procedure GetIdentifier(sym: symbolinfo);
      Procedure GetSymbol;
      Procedure PopStack(VAR indentsymbol: keysymbol;
                         VAR prevmargin: INTEGER);
      Procedure PushStack(indentsymbol: keysymbol;
                          prevmargin: INTEGER );
      Procedure WriteCRs(numberofcrs: INTEGER);
      Procedure InsertCR;
      Procedure InsertBlankLine;
      Procedure LShiftOn(dindsym: keysymset);
      Procedure LShift;
      Procedure InsertSpace(VAR symbol: symbolinfo);
      Procedure MoveLinePos(newlinepos: INTEGER);
      Procedure PrintSymbol;
      Procedure PPSymbol;
      Procedure Gobble(terminators: keysymset);
      Procedure RShift(currmsym: keysymbol);
      Function ReadConfigFile: Boolean;
    Public
      LineSize : longint;
      Indent : Integer;    { How many characters to indent ? }
      InS,
      OutS,
      DiagS,cfgS : PStream;
      Constructor Create;
      Function PrettyPrint : Boolean;
    end;

Procedure GenerateCfgFile(S: PStream);

Implementation

CONST
  version = '20 February 2005';  {was '11 October 1984','28 November 1989'; ..ancient stuff!}

  NUL = 0;      { ASCII null character }
  TAB = 9;      { ASCII tab character }
  FF = 12;      { ASCII formfeed character }
  CR = 13;      { ASCII carriage return }
  ESC = 27;     { ASCII escape character }
  Blank = ' ';
  MAXBYTE = 255;{ Largest value of 1 byte variable }

Type

  hashentry = RECORD
                Keyword : String[MAXKEYLENGTH];
                symtype : keysymbol
              END;

VAR
  sets : tableptr;
  dblch   : dblcharset;
  hashtable : ARRAY [Byte] OF hashentry;

CONST
  Keyword : KeywordTable =
     ('END', 'BEGIN', 'IF', 'THEN',
      'ELSE', 'PROCEDURE', 'VAR', 'OF',
      'WHILE', 'DO', 'CASE', 'WITH',
      'FOR', 'REPEAT', 'UNTIL', 'FUNCTION',
      'LABEL', 'CONST', 'TYPE', 'RECORD',
      'STRING', 'PROGRAM',
      'ASM','TRY','FINALLY','EXCEPT','RAISE','CLASS','OBJECT',
      'CONSTRUCTOR','DESCTRUCTOR','INHERITED','PROPERTY',
      'PRIVATE','PUBLIC','PROTECTED','PUBLISHED',
      'INITIALIZATION','FINALIZATION',
      'INLINE','LIBRARY','INTERFACE','IMPLEMENTATION',
      'READ','WRITE','UNIT',
      {keywords not used for formatting }
      'AND', 'ARRAY', 'DIV', 'DOWNTO',
      'FILE', 'GOTO', 'IN', 'MOD',
      'NOT', 'NIL', 'OR', 'SET','TO','VIRTUAL','USES'
     );


  EntryNames : OEntriesTable =
              ('end','begin','if','then','else','proc','var',
               'of','while','do','case','with','for','repeat','until',
               'func','label','const','type','record','string',
               'prog',
               'asm','try','finally','except','raise','class','object',
               'constructor','destructor','inherited','property',
               'private','public','protected','published',
               'initialization','finalization',
               'inline','library','interface','implementation',
               'read','write','unit',

               'and','arr','div','down','file','goto',
               'in','mod','not','nil','or','set','to','virtual','uses',
               'casevar',
               'becomes','delphicomment','dopencomment','dclosecomment',
               'opencomment','closecomment','semicolon',
               'colon','equals',
               'openparen','closeparen','period','endoffile','other');

  OptionNames : ONamesTable =
       ('crsupp','crbefore','blinbefore',
        'dindonkey','dindent','spbef','spaft',
        'gobsym','inbytab','crafter','upper',
        'lower','capital');


  DblChar : DblCharTable =
     ( ':=', '//','(*','*)' );

  SglChar : SglCharTable =
    ('{', '}', ';', ':', '=', '(', ')', '.' );

{ ---------------------------------------------------------------------
    General functions, not part of the object.
  ---------------------------------------------------------------------}

  function upperStr(const s : string) : string;
  var
    i  : longint;
  begin
     setLength(upperStr,length(s));
     for i:=1 to length(s) do
      if s[i] in ['a'..'z'] then
       upperStr[i]:=char(byte(s[i])-32)
      else
       upperStr[i]:=s[i];
  end;

  function LowerStr(const s : string) : string;
  var
    i  : longint;
  begin
     setLength(LowerStr,length(s));
     for i:=1 to length(s) do
      if s[i] in ['A'..'Z'] then
       LowerStr[i]:=char(byte(s[i])+32)
      else
       LowerStr[i]:=s[i];
  end;



Function IntToStr(I : LongInt) : String;

var
 s : string;
begin
  str(I,s);
  IntToStr := s;
end;

Function StrToInt(Const S : String) : Integer;

Var Code : integer;
    Res : Integer;

begin
  Val(S, Res, Code);
  StrToInt := Res;
  If Code<>0 then StrToInt:=0;
end;

Procedure Strip (Var S : String);

Const WhiteSpace =  [#32,#9,#10,#13];

Var I,J : Longint;

begin
  If length(s)=0 then exit;
  I:=1;
  While (S[I] in whitespace) and (I<Length(S)) do inc(i);
  J:=length(S);
  While (S[J] in whitespace) and (J>1) do dec(j);
  If I<=J then
    S:=Copy(S,i,j-i+1)
  else
    S:='';
end;

{ ---------------------------------------------------------------------
    Hash table related functions
  ---------------------------------------------------------------------}


Function hash(Symbol: String): Byte;
  { Hashing function for identifiers.  The formula gives a unique value
    in the range 0..255 for each Pascal/Z keyword.  Note that range and
    overflow checking must be turned off for this function even if they
    are enabled for the rest of the program.  }
  BEGIN
{$R-}
    hash := (ORD(Symbol[1]) * 5 + ORD(Symbol[length(Symbol)])) * 5 + length(Symbol);
{$R+}
  END; { of hash }

Procedure CreateHash;

Var psn : Byte;
    sym : keysymbol;

begin
  FOR psn := 0 TO MAXBYTE DO BEGIN
    hashtable[psn].Keyword := '         ';
    hashtable[psn].symtype := othersym
  END;
  FOR sym := endsym TO lastformatsym DO BEGIN
    psn := hash(Keyword[sym]);
    hashtable[psn].Keyword := Keyword[sym];
    hashtable[psn].symtype := sym
  END; { for }
end;


Procedure ClassID(Value: Token;
                  lngth: INTEGER;
                  VAR idtype: keysymbol;
                  VAR IsKeyWord: BOOLEAN);
  { Classify an identifier.  We are only interested
    in it if it is a keyword, so we use the hash table. }
  VAR
    Keyvalue: String[MAXKEYLENGTH];
    tabent: INTEGER;
  BEGIN
    IF lngth > MAXKEYLENGTH THEN BEGIN
      idtype := othersym;
      IsKeyWord := FALSE
    END
    ELSE BEGIN
      KeyValue:= UpperStr(Value);
      tabent := hash(Keyvalue);
      IF Keyvalue = hashtable[tabent].Keyword THEN BEGIN
        idtype := hashtable[tabent].symtype;
        IsKeyWord := TRUE;
      END
      ELSE BEGIN
        idtype := othersym;
        IsKeyWord := FALSE;
      END
    END
  END; { of ClassID }

{ ---------------------------------------------------------------------
    Functions to create options and set defaults.
  ---------------------------------------------------------------------}

Procedure CreateOptions (Var Option : OptionTable);

Var Sym : KeySymbol;

begin
  FOR sym := endsym TO othersym DO BEGIN
    NEW(option[sym]);
    option[sym]^.selected := [];
    option[sym]^.dindsym := [];
    option[sym]^.terminators := []
  END;
end;

Procedure SetTerminators(Var Option : OptionTable);

begin
  option[casesym]^.terminators    := [ofsym];
  option[casevarsym]^.terminators := [ofsym];
  option[forsym]^.terminators     := [dosym];
  option[whilesym]^.terminators   := [dosym];
  option[withsym]^.terminators    := [dosym];
  option[ifsym]^.terminators      := [thensym];
  option[untilsym]^.terminators   := [endsym, untilsym, elsesym, semicolon];
  option[becomes]^.terminators    := [endsym, untilsym, elsesym, semicolon];
  option[openparen]^.terminators  := [closeparen];
  option[usessym]^.terminators    := [semicolon];
end;

Procedure SetDefaultIndents (Var Option : OptionTable);

begin
  option[recordsym]^.dindsym    := [endsym];
  option[funcsym]^.dindsym      := [labelsym, constsym, typesym, varsym];
  option[procsym]^.dindsym      := [labelsym, constsym, typesym, varsym];
  option[constsym]^.dindsym     := [labelsym, constsym, typesym, varsym];
  option[typesym]^.dindsym      := [labelsym, constsym, typesym, varsym];
  option[varsym]^.dindsym       := [labelsym, constsym, typesym, varsym];
  option[beginsym]^.dindsym     := [labelsym, constsym, typesym, varsym];
  option[publicsym]^.dindsym    := [protectedsym,privatesym,publicsym,publishedsym];
  option[privatesym]^.dindsym   := [protectedsym,privatesym,publicsym,publishedsym];
  option[protectedsym]^.dindsym := [protectedsym,privatesym,publicsym,publishedsym];
  option[publishedsym]^.dindsym := [protectedsym,privatesym,publicsym,publishedsym];
  option[finallysym]^.dindsym   := [trysym];
  option[exceptsym]^.dindsym   := [trysym];
  option[elsesym]^.dindsym      := [ifsym, thensym, elsesym];
  option[untilsym]^.dindsym     := [ifsym, thensym, elsesym, forsym, whilesym,
                                    withsym, colon, equals];
  option[endsym]^.dindsym       := [ifsym, thensym, elsesym, forsym, whilesym,
                                    withsym, casevarsym, colon, equals, recordsym,
                                    classsym,objectsym];
  option[semicolon]^.dindsym    := [ifsym, thensym, elsesym, forsym,
                                    whilesym, withsym, colon, equals];
end;

Procedure SetDefaults (Var Option : OptionTable);

{ Sets default values for the formatting rules. }

begin
  option[progsym]^.selected         := [capital,blinbefore, spaft];
  option[unitsym]^.selected         := [capital,blinbefore, spaft];
  option[librarysym]^.selected      := [capital,blinbefore, spaft];
  option[funcsym]^.selected         := [capital,blinbefore, dindonkey, spaft];
  option[procsym]^.selected         := [capital,blinbefore, dindonkey, spaft];
  option[labelsym]^.selected        := [capital,blinbefore, spaft, inbytab];
  option[constsym]^.selected        := [capital,blinbefore, dindonkey, spaft, inbytab];
  option[typesym]^.selected         := [capital,blinbefore, dindonkey, spaft, inbytab];
  option[varsym]^.selected          := [capital,blinbefore, dindonkey, spaft, inbytab];
  option[beginsym]^.selected        := [capital,dindonkey, crbefore, crafter, inbytab];
  option[repeatsym]^.selected       := [capital,inbytab, crafter];
  option[recordsym]^.selected       := [capital,inbytab, crafter];
  option[objectsym]^.selected       := [capital,inbytab, crafter];
  option[classsym]^.selected        := [capital,inbytab, crafter];
  option[publicsym]^.selected       := [capital,crbefore, dindonkey, spaft, inbytab];
  option[publishedsym]^.selected    := [capital,crbefore, dindonkey, spaft, inbytab];
  option[protectedsym]^.selected    := [capital,crbefore, dindonkey, spaft, inbytab];
  option[privatesym]^.selected      := [capital,crbefore, dindonkey, spaft, inbytab];
  option[trysym]^.Selected          := [capital,crbefore,crafter,inbytab];
  option[finallysym]^.selected      := [capital,crbefore,dindonkey,crafter,inbytab];
  option[exceptsym]^.selected       := [capital,crbefore,dindonkey,crafter,inbytab];
  option[casesym]^.selected         := [capital,spaft, inbytab, gobsym, crafter];
  option[casevarsym]^.selected      := [capital,spaft, inbytab, gobsym, crafter];
  option[ofsym]^.selected           := [capital,crsupp, spbef];
  option[forsym]^.selected          := [capital,spaft, inbytab, gobsym, crafter];
  option[whilesym]^.selected        := [capital,spaft, inbytab, gobsym, crafter];
  option[withsym]^.selected         := [capital,spaft, inbytab, gobsym, crafter];
  option[dosym]^.selected           := [capital,crsupp, spbef];
  option[ifsym]^.selected           :=  [capital,spaft, inbytab, gobsym];
  option[thensym]^.selected         := [capital];
  option[elsesym]^.selected         := [capital,crbefore, dindonkey, inbytab];
  option[endsym]^.selected          := [capital,crbefore, crafter,dindonkey,dindent];
  option[untilsym]^.selected        := [capital,crbefore, dindonkey, dindent, spaft,
                                        gobsym, crafter];
  option[becomes]^.selected         := [capital,spbef, spaft, gobsym];
  option[Delphicomment]^.Selected   := [crafter];
  option[opencomment]^.selected     := [capital,crsupp];
  option[closecomment]^.selected    := [capital,crsupp];
  option[semicolon]^.selected       := [capital,crsupp, dindonkey, crafter];
  option[colon]^.selected           := [capital,inbytab];
  option[equals]^.selected          := [capital,spbef, spaft, inbytab];
  option[openparen]^.selected       := [capital,gobsym];
  option[period]^.selected          := [capital,crsupp];
end;

{ ---------------------------------------------------------------------
    Stream handling routines
  ---------------------------------------------------------------------}

Function ReadChar (S : PStream) : Char;

Var C : Char;

begin
  repeat
    if S^.GetPos = S^.GetSize then
      C:=#0
    else
      S^.Read(C,1);
  Until C<>#13;
  ReadChar:=C;
end;

Function EoSLn (S : PStream) : Char;

Const WhiteSpace = [' ', #9, #13 ];

Var C : Char;

begin
  Repeat
    if S^.GetPos = S^.GetSize then
      C:=#0
    else
      S^.Read(C,1);
  Until (Not (C in WhiteSpace)) or ((C=#10));
  EoSln:=C;
end;

Function ReadString (S: PStream): String;

Var Buffer : String;
    I : Byte;

begin
  Buffer:='';
  I:=0;
  Repeat
    S^.Read(Buffer[I+1],1);
    Inc(I);
  until (I=255) or (Buffer[I]=#10) Or (S^.Status=StReadError);
  If S^.Status=stReadError then Dec(I);
  If Buffer[i]=#10 Then Dec(I);
  If Buffer[I]=#13 then Dec(I);
  Buffer[0] := chr(I);
  ReadString:=Buffer;
end;

Procedure WriteString (S : PStream; ST : String);

begin
  S^.Write(St[1],length(St));
end;

Procedure WriteAnsiString (S : PStream; ST : AnsiString);

begin
  S^.Write(St[1],length(St));
end;


Procedure WriteCR (S: PStream);

Const
  Newline = System.LineEnding;

begin
  WriteString(S,Newline);
end;


Procedure WriteLnString (S : PStream; ST : String);

begin
  WriteString(S,ST);
  WriteCR(S);
end;


{ ---------------------------------------------------------------------
    TPrettyPrinter object
  ---------------------------------------------------------------------}

Procedure TPrettyPrinter.Verbose (Const Msg : String);

begin
  If Assigned (DiagS) then
    WriteLnString (DiagS,Msg);
end;

Procedure TPrettyPrinter.GetChar;
{ Read the next character and classify it }
  VAR  Ch: CHAR;
  BEGIN
    currchar := nextchar;
    WITH nextchar DO
      begin
      Ch:=ReadCHar(Ins);
      If Ch=#0 then
        BEGIN
        name := filemark;
        Value := Blank
        END
      ELSE If (Ch=#10) THEN
        BEGIN
        name := endofline;
        Value := Ch;
        Inc(inlines);
        END
      ELSE
        BEGIN
        Value := Ch;
        IF Ch IN ['a'..'z', 'A'..'Z', '_'] THEN name := letter
        ELSE IF Ch IN ['0'..'9'] THEN name := digit
        ELSE IF Ch = '''' THEN name := quote
        ELSE IF Ch in [#13,' ',#9] THEN name := space
        ELSE name := otherchar
        END
      end;
  END; { of GetChar }


Procedure TPrettyPrinter.StoreNextChar(VAR lngth: INTEGER;
                        VAR Value: Token);
  { Store a character in the current symbol }
  BEGIN
    GetChar;
    IF lngth < MAXSYMBOLSIZE THEN BEGIN {XXX - should there be a limit at all?}
      Inc(lngth);
      setlength(Value,lngth);
      Value[lngth] := currchar.Value;
    END;
  END; { of StoreNextChar }


Procedure TPrettyPrinter.SkipBlanks(VAR spacesbefore, crsbefore: INTEGER);
  { Count the spaces between symbols }
  BEGIN
    spacesbefore := 0;
    crsbefore := 0;
    WHILE nextchar.name IN [space, endofline] DO BEGIN
      GetChar;
      CASE currchar.name OF
        space:      Inc(spacesbefore);
        endofline:  BEGIN
                      Inc(crsbefore);
                      spacesbefore := 0;
                    END;
      END;  {case}
    END;
  END; { of SkipBlanks }


Procedure TPrettyPrinter.GetComment(sym: symbolinfo);
  { Process comments using brace notation }
  BEGIN
    sym^.name := opencomment;
    WHILE NOT ((currchar.Value = '}') 
    OR (nextchar.name = filemark)) DO
      StoreNextChar(sym^.length, sym^.Value);
    IF currchar.Value = '}' THEN sym^.name := closecomment;
  END; { of GetCommment }

Procedure TPrettyPrinter.GetDoubleComment(sym: symbolinfo);
  { Process comments using parenthesis notation }
  BEGIN
    sym^.name := dopencomment;
    WHILE NOT (((currchar.Value = '*') AND (nextchar.Value = ')'))
    OR (nextchar.name = filemark)) DO
      StoreNextChar(sym^.length, sym^.Value);
    IF (currchar.Value = '*') AND (nextchar.Value = ')') THEN BEGIN
      StoreNextChar(sym^.length, sym^.Value);
      sym^.name := dclosecomment;
    END;
  END; { of GetDoubleCommment }

Procedure TPrettyPrinter.GetDelphiComment(sym: symbolinfo);
  { Process comments using either brace or parenthesis notation }
  BEGIN
    sym^.name := Delphicomment;
    WHILE NOT ((nextchar.name = endofline) OR (nextchar.name = filemark)) DO
      StoreNextChar(sym^.length, sym^.Value);

  END; { of GetDelphiCommment }



Procedure TPrettyPrinter.GetIdentifier(sym: symbolinfo);
  { Read an identifier and classify it }
  BEGIN
    WHILE nextchar.name IN [letter, digit] DO
      StoreNextChar(sym^.length, sym^.Value);
    ClassID(sym^.Value, sym^.length, sym^.name, sym^.IsKeyWord);
    IF sym^.name IN [recordsym, casesym, endsym] THEN
      CASE sym^.name OF
        recordsym : RecordSeen := TRUE;
        casesym   : IF RecordSeen THEN sym^.name := casevarsym;
        endsym    : RecordSeen := FALSE;
      END;  {case}
  END; { of GetIdentifier }


{ Read a number and store it as a string }
Procedure TPrettyPrinter.GetNumber(sym: symbolinfo);
  BEGIN
    WHILE nextchar.name = digit DO StoreNextChar(sym^.length, sym^.Value);
    sym^.name := othersym;
  END; { of GetNumber }


PROCEDURE TPrettyPrinter.GetCharLiteral(sym: symbolinfo);
  { Read a quoted string }
  BEGIN
    WHILE nextchar.name = quote DO BEGIN
      StoreNextChar(sym^.length, sym^.Value);
      WHILE NOT (nextchar.name IN [quote, endofline, filemark]) DO
        StoreNextChar(sym^.length, sym^.Value);
      IF nextchar.name = quote THEN StoreNextChar(sym^.length, sym^.Value);
    END;
    sym^.name := othersym;
  END; { of GetCharLiteral }


FUNCTION TPrettyPrinter.char_Type: keysymbol;

  { Classify a character pair }

  VAR
    NextTwoChars: SpecialChar;
    Hit: BOOLEAN;
    thischar: keysymbol;
  BEGIN
    NextTwoChars[1] := currchar.Value;
    NextTwoChars[2] := nextchar.Value;
    thischar := becomes;
    Hit := FALSE;
    WHILE NOT (Hit OR (thischar = opencomment)) DO BEGIN
      IF NextTwoChars = DblChar[thischar] THEN Hit := TRUE
      ELSE Inc(thischar);
    END;
    IF NOT Hit THEN BEGIN
      thischar := opencomment;
      WHILE NOT (Hit OR (PRED(thischar) = period)) DO BEGIN
        IF currchar.Value = SglChar[thischar] THEN Hit := TRUE
        ELSE Inc(thischar);
      END;
    END;
    IF Hit THEN char_Type := thischar
    ELSE char_Type := othersym;
  END; { of char_Type }


Procedure TPrettyPrinter.GetSpecialChar(sym: symbolinfo);
   { Read special characters }
  BEGIN
    StoreNextChar(sym^.length, sym^.Value);
    sym^.name := char_Type;
    IF sym^.name IN dblch THEN StoreNextChar(sym^.length, sym^.Value)
  END; { of GetSpecialChar }


Procedure TPrettyPrinter.GetNextSymbol(sym: symbolinfo);
  { Read a symbol using the appropriate procedure }
  BEGIN
    CASE nextchar.name OF
      letter:     GetIdentifier(sym);
      digit:      GetNumber(sym);
      quote:      GetCharLiteral(sym);
      otherchar:  BEGIN
                    GetSpecialChar(sym);
                    IF sym^.name = opencomment THEN GetComment(sym)
                    else IF sym^.name = dopencomment THEN GetDoubleComment(sym)
                    else IF sym^.name= DelphiComment then GetDelphiComment(Sym)
                  END;
      filemark:   sym^.name := endoffile;
      ELSE {:} {Turbo}
        WRITELN('Unknown character type: ', ORD(nextchar.name));
    END;  {case}
  END; { of GetNextSymbol }


Procedure TprettyPrinter.GetSymbol;
{ Store the next symbol in NEXTSYM }
  VAR
    dummy: symbolinfo;
  BEGIN
    dummy := currsym;
    currsym := nextsym;
    nextsym := dummy;
    SkipBlanks(nextsym^.spacesbefore, nextsym^.crsbefore);
    nextsym^.length := 0;
    nextsym^.IsKeyWord := FALSE;
    IF currsym^.name = opencomment THEN GetComment(nextsym)
    ELSE IF currsym^.name = dopencomment THEN GetDoubleComment(nextsym)
    ELSE GetNextSymbol(nextsym);
  END;  {of GetSymbol}


Procedure TprettyPrinter.PopStack(VAR indentsymbol: keysymbol;
                                  VAR prevmargin: INTEGER);
  { Manage stack of indentation symbols and margins }
  BEGIN
    IF top > 0 THEN BEGIN
      indentsymbol := stack[top].indentsymbol;
      prevmargin := stack[top].prevmargin;
      Dec(top);
    END
    ELSE BEGIN
      indentsymbol := othersym;
      prevmargin := 0;
    END;
  END; { of PopStack }


Procedure TPrettyPrinter.PushStack(indentsymbol: keysymbol;
                                   prevmargin: INTEGER );
  BEGIN
    Inc(top);
    stack[top].indentsymbol := indentsymbol;
    stack[top].prevmargin := prevmargin;
  END; { of PushStack }


Procedure TPrettyPrinter.WriteCRs(numberofcrs: INTEGER);
  VAR
    i: INTEGER;
  BEGIN
    IF numberofcrs > 0 THEN BEGIN
      FOR i := 1 TO numberofcrs DO
        WriteCr(OutS);
      Inc(outlines,numberofcrs);
      currlinepos := 0;
    END;
  END; { of WriteCRs }


Procedure TPrettyPrinter.InsertCR;
  BEGIN
    IF currsym^.crsbefore = 0 THEN BEGIN
      WriteCRs(1);
      currsym^.spacesbefore := 0;
    END;
  END; { of InsertCR }


Procedure TPrettyPrinter.InsertBlankLine;
  BEGIN
    IF currsym^.crsbefore = 0 THEN BEGIN
      IF currlinepos = 0 THEN WriteCRs(1)
      ELSE WriteCRs(2);
      currsym^.spacesbefore := 0;
    END
    ELSE IF currsym^.crsbefore = 1 THEN
      IF currlinepos > 0 THEN WriteCRs(1);
  END; { of InsertBlankLine }


Procedure TPrettyPrinter.LShiftOn(dindsym: keysymset);
  { Move margin left according to stack configuration and current symbol }
  VAR
    indentsymbol: keysymbol;
    prevmargin: INTEGER;
  BEGIN
    IF top > 0 THEN BEGIN
      REPEAT
        PopStack(indentsymbol, prevmargin);
        IF indentsymbol IN dindsym THEN currmargin := prevmargin;
      UNTIL NOT (indentsymbol IN dindsym) OR (top = 0);
      IF NOT (indentsymbol IN dindsym) THEN
        PushStack(indentsymbol, prevmargin);
    END;
  END; { of LShiftOn }


Procedure TprettyPrinter.LShift;
{ Move margin left according to stack top }
  VAR
    indentsymbol: keysymbol;
    prevmargin: INTEGER;
  BEGIN
    IF top > 0 THEN BEGIN
      PopStack(indentsymbol, prevmargin);
      currmargin := prevmargin;
(* maybe PopStack(indentsymbol,currmargin); *)
    END;
  END; { of LShift }


Procedure TPrettyPrinter.InsertSpace(VAR symbol: symbolinfo);
  { Insert space if room on line }
  BEGIN
    IF currlinepos < LineSize THEN BEGIN
      WriteString(OutS, Blank);
      Inc(currlinepos);
      IF (symbol^.crsbefore = 0) AND (symbol^.spacesbefore > 0)
      THEN Dec(symbol^.spacesbefore);
    END;
  END; { of InsertSpace }


Procedure TPrettyPrinter.MoveLinePos(newlinepos: INTEGER);
  { Insert spaces until correct line position reached }
  VAR  i: INTEGER;
  BEGIN
    FOR i := SUCC(currlinepos) TO newlinepos DO
      WriteString(OutS, Blank);
    currlinepos := newlinepos;
  END; { of MoveLinePos }


Procedure TPrettyPrinter.PrintSymbol;

  BEGIN
    IF (currsym^.IsKeyWord) then
      begin
      If upper in sets^.selected Then
        WriteString (OutS,UpperStr(currsym^.value))
      else if lower in sets^.selected then
        WriteString (OutS,LowerStr(currsym^.value))
      else if capital in sets^.selected then
        begin
        WriteString(OutS,UpCase(CurrSym^.Value[1]));
        WriteString(OutS,LowerStr(Copy(CurrSym^.Value,2,MAXSYMBOLSIZE)));{XXX - ?should it be length?}
        end
      else
        WriteString(OutS,Currsym^.Value);
      end
    ELSE
      WriteAnsiString(OutS, currsym^.Value);
    startpos := currlinepos;
    Inc(currlinepos,currsym^.length);
  END; { of PrintSymbol }


Procedure TPrettyPrinter.PPSymbol;
{ Find position for symbol and then print it }
  VAR  newlinepos: INTEGER;
  BEGIN
    WriteCRs(currsym^.crsbefore);
    IF (currlinepos + currsym^.spacesbefore > currmargin)
    OR (currsym^.name IN [opencomment, closecomment,dopencomment, dclosecomment])
    THEN newlinepos := currlinepos + currsym^.spacesbefore
    ELSE newlinepos := currmargin;

    IF newlinepos + currsym^.length > LINESIZE THEN BEGIN {XXX - this needs to be cleaned for case of long symbol values}
      WriteCRs(1);
      IF currmargin + currsym^.length <= LINESIZE
      THEN newlinepos := currmargin
      ELSE IF currsym^.length < LINESIZE
      THEN newlinepos := LINESIZE - currsym^.length
      ELSE newlinepos := 0;
    END;
    MoveLinePos(newlinepos);
    PrintSymbol;
  END; { of PPSymbol }


Procedure TPrettyPrinter.Gobble(terminators: keysymset);
  { Print symbols which follow a formatting symbol but which do not
    affect layout }
  BEGIN
    IF top < MAXSTACKSIZE THEN PushStack(currsym^.name, currmargin);
    currmargin := currlinepos;
    WHILE NOT ((nextsym^.name IN terminators)
    OR (nextsym^.name = endoffile)) DO BEGIN
      GetSymbol;
      PPSymbol;
    END;
    LShift;
  END; { of Gobble }


Procedure TprettyPrinter.RShift(currmsym: keysymbol);
  { Move right, stacking margin positions }
  BEGIN
    IF top < MAXSTACKSIZE THEN PushStack(currmsym, currmargin);
    IF startpos > currmargin THEN currmargin := startpos;
    Inc(currmargin,INDENT);
  END; { of RShift }


Function TPrettyPrinter.ReadConfigFile : Boolean;

Var I,J : Longint;

  Procedure SetOption(TheKey : KeySymbol;Var OptionList : String);

  Var TheOpt  : Options;
      Found : Boolean;
      K : longint;
      opt : string;

  begin
    Repeat
      K:=pos(',',optionlist);
      If k>0 then
        begin
        opt:=Copy(OptionList,1,k-1);
        strip(opt);
        Delete(OptionList,1,k);
        end
      else
        opt:=OptionList;
      If Length(Opt)>0 then
        begin
        Found:=False;
        for TheOpt :=firstopt to lastopt do
          begin
          found:=opt=OptionNames[Theopt];
          If found then break;
          end;
        If not found then
          Verbose ('Unknown option on line '+inttostr(i)+': '+Opt)
        else
          Option[TheKey]^.Selected:=Option[TheKey]^.Selected+[TheOpt];
        end;
    until k=0;
  end;

  Procedure SetIndent(TheKey : KeySymbol; Var OptionList : String);

  Var
      TheIndent : Keysymbol;
      Found : Boolean;
      K : longint;
      opt : string;

  begin
    Repeat
      K:=pos(',',optionlist);
      If k>0 then
        begin
        opt:=Copy(OptionList,1,k-1);
        strip(opt);
        Delete(OptionList,1,k);
        end
      else
        opt:=OptionList;
      If Length(Opt)>0 then
        begin
        Found:=False;
        for TheIndent :=firstKey to lastKey do
          begin
          found:=opt=EntryNames[Theindent];
          If found then break;
          end;
        If not found then
          begin
          Verbose ('Unknown indent keysym on line '+inttostr(i)+': '+Opt);
          exit;
          end;
        Option[TheKey]^.dindsym:=Option[TheKey]^.dindsym+[Theindent];
        end;
    until k=0;
  end;

Var TheKey : KeySymbol;
    Found,DoIndent : Boolean;
    Line, Name : String;

begin
  ReadConfigFile:=false;
  I:=0;
  while not (CfgS^.Status=stReadError) do
    begin
    inc(i);
    Line:='';
    Line:=ReadString(cfgS);
    { Strip comment }
    If pos('#',Line)<>0 then
      Line:=Copy(Line,1,Pos('#',Line)-1);
    If length(Line)<>0 then
      begin
      J:=Pos('=',Line);
      If J>0 then
        begin
        Line:=LowerStr(Line);
        Name:=Copy(Line,1,j-1);
        Delete(Line,1,J);
        { indents or options ? }
        If (Name[1]='[') and
           (Name[Length(Name)]=']') then
           begin
           Name:=Copy(Name,2,Length(Name)-2);
           Doindent:=True;
           end
        else
           DoIndent:=False;
        Strip(Name);
        found:=false;
        for thekey:=firstkey to lastkey do
          begin
          found:=Name=EntryNames[thekey];
          If Found then break;
          end;
        If not found then
          Verbose ('Unknown keyword on line '+inttostr(i)+': '+Name)
        else
          If DoIndent then
            SetIndent(TheKey,Line)
          else
            SetOption(TheKey,Line)
        end
      else
        verbose ('Error in config file on line '+IntToStr(i));
      end;
    end;
  Verbose ('Processed configfile: read '+IntToStr(I)+' lines');
  ReadConfigFile:=true;
end;

Procedure GenerateCfgFile(S : PStream);

Var TheKey,TheIndent : KeySymbol;
    TheOpt : Options;
    Written : Boolean;
    Option : OptionTable;

begin
  CreateOptions(option);
  SetDefaults(option);
  SetDefaultIndents(option);
  For TheKey:=Firstkey to lastkey do
    begin
    { Write options }
    WriteString (S,EntryNames[TheKey]+'=');
    Written:=False;
    for TheOpt:=FirstOpt to LastOpt do
      If TheOpt in Option[TheKey]^.Selected then
        begin
        if written then
           WriteString (S,',')
        else
           Written:=True;
        writeString (S,OptionNames[TheOpt]);
        end;
    WriteCr (S);
    { Write de-indent keysyms, if any }
    If Option[TheKey]^.dindsym<>[] then
      begin
      WriteString (S,'['+EntryNames[TheKey]+']=');
      Written:=False;
      For TheIndent:=FirstKey to lastkey do
      If TheIndent in Option[TheKey]^.dindsym then
        begin
        if written then
           WriteString (S,',')
        else
           Written:=True;
        WriteString (S,EntryNames[Theindent]);
        end;
      WriteCr (S);
      end;
    end;
end;

Function trimMiddle ( a:ansistring; lnght: integer; size: integer):string;
var
    half:Integer;
begin
    if lnght > size 
    then
    begin
      half := (size - 3) div 2;
      trimMiddle := copy(a,1,half) + '...' + copy(a,lnght-half+1,half);
    end
    else
      trimMiddle := a;
end;

Function TPrettyPrinter.PrettyPrint : Boolean;

Begin
  PrettyPrint:=False;
  If Not Assigned(Ins) or Not Assigned(OutS) then
    exit;
  If Not Assigned(CfgS) then
    begin
    SetDefaults(Option);
    SetDefaultIndents(Option);
    end
  else
    ReadConfigFile;
  { Initialize variables }
  top := 0;
  currlinepos := 0;
  currmargin := 0;
  inlines := 0;
  outlines := 0;
  CrPending := FALSE;
  RecordSeen := FALSE;
  GetChar;
  NEW(currsym);
  NEW(nextsym);
  GetSymbol;
  WHILE nextsym^.name <> endoffile DO BEGIN
    GetSymbol;
    Verbose('line in-'+IntToStr(inlines)+' out-'+IntToStr(outlines)+
            ' symbol "'+EntryNames[currsym^.name]+'" = "'+ 
            trimMiddle(currsym^.value,length(currsym^.value),MAXSHOWSIZE)+'"');
    sets := option[currsym^.name];
    IF (CrPending AND NOT (crsupp IN sets^.selected))
    OR (crbefore IN sets^.selected) THEN BEGIN
      InsertCR;
      CrPending := FALSE
    END;
    IF blinbefore IN sets^.selected THEN BEGIN
      InsertBlankLine;
      CrPending := FALSE
    END;
    IF dindonkey IN sets^.selected THEN LShiftOn(sets^.dindsym);
    IF dindent IN sets^.selected THEN LShift;
    IF spbef IN sets^.selected THEN InsertSpace(currsym);
    PPSymbol;
    IF spaft IN sets^.selected THEN InsertSpace(nextsym);
    IF inbytab IN sets^.selected THEN RShift(currsym^.name);
    IF gobsym IN sets^.selected THEN Gobble(sets^.terminators);
    IF crafter IN sets^.selected THEN CrPending := TRUE
  END;
  IF CrPending THEN WriteCRs(1);
  Verbose(IntToStr(inlines)+' lines read, '+IntToStr(outlines)+' lines written.');
  PrettyPrint:=True;
end;

Constructor TPrettyPrinter.Create;

Begin
  LineSize:=MaxLineSize;
  CreateOptions (Option);
  SetTerminators(Option);
  DiagS:=Nil;
  InS:=Nil;
  OutS:=Nil;
  CfgS:=Nil;
End;

{ ---------------------------------------------------------------------
    Unit initialization
  ---------------------------------------------------------------------}


Begin
  CreateHash;
  dblch := [becomes, opencomment];
end.

{
  $Log: ptopu.pp,v $
  Revision 1.8  2005/02/21 07:59:10  michael
  - keywords 'virtual' and 'uses' were added.
  - '{}' and '(**)' comment types were separated.
  - tokens now AnsiStrings
  - the comments are now handled better, ptop now does multi line comments.
  - added debug prints to verbose option
          'line in-<number> out-<number> symbol "<name>" = "<value>"'
    the <value> is truncated in the middle. this means visible beginning and
    the end.

  Revision 1.7  2003/11/24 22:39:25  michael
  + set maxsymbolsize to 255

  Revision 1.6  2003/03/27 14:23:00  michael
  + Fixed use of linesize property, reported by Wolfgang Waffenschmidt

  Revision 1.5  2002/09/07 15:40:31  peter
    * old logs removed and tabs fixed

  Revision 1.4  2002/07/14 13:39:45  carl
    * use special symbols for portability's sake

  Revision 1.3  2002/06/01 18:39:14  marco
   * Renamefest

  Revision 1.2  2002/02/27 16:35:31  carl
  * bugfix of stream errors - would always give error!

}
