unit ptop.types;

{$mode objfpc}{$h+}

interface

uses
  {$ifdef FPC_DOTTEDUNITS}
  system.typinfo
    {$else}
    typinfo
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

CONST
  Keyword : KeywordTable =
     ('END', 'BEGIN', 'IF', 'THEN',
      'ELSE', 'PROCEDURE', 'VAR', 'OF',
      'WHILE', 'DO', 'CASE', 'WITH',
      'FOR', 'REPEAT', 'UNTIL', 'FUNCTION',
      'LABEL', 'CONST', 'TYPE', 'RECORD',
      'STRING', 'PROGRAM',
      'ASM','TRY','FINALLY','EXCEPT','RAISE','CLASS','OBJECT',
      'CONSTRUCTOR','DESTRUCTOR','INHERITED','PROPERTY',
      'PRIVATE','PUBLIC','PROTECTED','PUBLISHED',
      'INITIALIZATION','FINALIZATION',
      'INLINE','LIBRARY','INTERFACE','IMPLEMENTATION',
      'READ','WRITE','UNIT',
      {keywords not used for formatting }
      'AND', 'ARRAY', 'DIV', 'DOWNTO',
      'FILE', 'GOTO', 'IN', 'MOD',
      'NOT', 'NIL', 'OR', 'SET','TO','VIRTUAL','USES'
     );

  DblChar : DblCharTable =
    ( ':=', '<>', '<=', '>=', '+=', '-=', '/=', '*=', '**', '//', '(*', '*)' );

  SglChar : SglCharTable =
    ('{', '}', ';', ':', '=', '(', ')', '.' );

var
  OptionTypInfo : PTypeInfo;
  EntryTableTypInfo : PTypeInfo;

implementation

initialization
  OptionTypInfo := TypeInfo(Options);
  EntryTableTypInfo := TypeInfo(KeySymbol);
end.
