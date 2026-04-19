{
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

Unit PtoPu;
{$mode objfpc}
{$h+}

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

uses
  {$ifdef FPC_DOTTEDUNITS}
  System.Classes,
  System.Sysutils
  {$else}
  Classes,
  Sysutils
  {$endif}
  ;

Const

  MAXSYMBOLSIZE = 65500;
  MAXSHOWSIZE = 40;
  MAXSTACKSIZE = 100;
  MAXKEYLENGTH = 15;     { The longest keywords are IMPLEMENTATION and INITIALIZATION }
  DEFLINESIZE = 100;
  DEFINDENT = 2;

TYPE
  Token    = AnsiString;
  FileName = STRING;

  TTokenScope = (tsInterface,tsImplementation);

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
               casevarsym,ofobjectsym,
               { other symbols }
               becomes,notequal,lessorequal,greaterorequal,
               plusequals, minusequals, divideequals, timesequals, exponential,
               delphicomment,dopencomment,dclosecomment,opencomment,closecomment,semicolon,colon,equals,
               openparen,closeparen,period,endoffile,othersym);

  { Formatting options }
  { If you add options, adjust the definition of lastopt }
  options = (crsupp,crbefore,blinbefore,
             dindonkey,dindent,spbef,
             spaft,gobsym,inbytab,inbyindent,crafter,upper,lower,capital);

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
                 prevmargin : INTEGER;
               END;

  symbolstack = ARRAY [1..MAXSTACKSIZE] OF stackentry;

Const FirstOpt = crsupp;
      LastOpt = capital; { Adjust this if you add options }
      FirstKey = endsym;
      LastKey = othersym; { Adjust this if you add options }
      LastFormatsym = usessym;

Type
  TablePtr = ^tableentry;
  OptionTable = ARRAY [Ttokenscope,keysymbol] OF tableptr;
  KeywordTable = ARRAY [endsym..lastFormatsym] OF String[MAXKEYLENGTH];
  SpecialChar = ARRAY [1..2] OF CHAR;
  dblcharset = SET OF endsym..othersym;
  DblCharTable = ARRAY [becomes..dclosecomment] OF SpecialChar;
  SglCharTable = ARRAY [opencomment..period] OF CHAR;

  { TPrettyPrinter }

  TPrettyPrinter=Class(TObject)
  Private
    FTokenScope: TTokenScope;
{$ifdef debug}
    GobbleLevel : Integer;
{$endif debug}
    PreviousSymbol : keysymbol;
    RecordLevel : Integer;
    ClassSeen,ObjectSeen : Boolean;
    LastStruct : KeySymbol;
    CRPending : BOOLEAN;
    currchar,nextchar : charinfo;
    currsym,nextsym : symbolinfo;
    inlines,outlines : INTEGER;
    stack   : symbolstack;
    top,startpos,currlinepos,currmargin : Integer;
    option : OptionTable;
    FVerbose :  Boolean;
    FirstWordStackPos,
    FirstWordPos,
    FLineSize,
    FIndent : Integer;
    ins,outs,cfgs : TStream;
    Procedure Verbose (Const Msg : String);
    Procedure GetChar;
    Procedure StoreNextChar(Var lngth: INTEGER;
                            var Value: Token);
    Procedure SkipBlanks(Out spacesbefore, crsbefore: INTEGER);
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
    Procedure PopStack(Out indentsymbol: keysymbol;
                       Out prevmargin: INTEGER);
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
    Procedure RShiftIndent{$ifdef debug}(currmsym: keysymbol){$endif debug};
    Function ReadConfigFile: Boolean;
  Public
    Constructor Create;
    Function PrettyPrint : Boolean;
    Property BeVerbose : Boolean Read FVerbose Write FVerbose;
    Property LineSize : Integer Read FLineSize Write FLineSize;
    Property Indent : Integer Read FIndent Write FIndent;    { How many characters to indent ? }
    Property Source : TStream Read Ins Write Ins;
    Property Dest : TStream Read OutS Write Outs;
    Property Config : Tstream Read cfgS Write cfgs;
    Property CurrentScope : TTokenScope Read FTokenScope Write FTokenScope;
  end;

Procedure GenerateCfgFile(S: TStream);

resourcestring
  E_UNKNOWN_KEYWORD = 'Unknown config keyword on line %d : %s';
  E_CONFIG_FILE     = 'Error reading config file on line %d';
  E_UNKNOWN_INDENT  = 'Unknown indent %s on line %d: %s';
  S_PROCESSED_CFG   = 'Processed config file: read %d lines';
  S_PRETTY_PRINTED  = 'Read %d lines of input, %d lines written.';