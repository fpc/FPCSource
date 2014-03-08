unit jstoken;

{$mode objfpc}{$H+}

interface

type

  TJSToken = (tjsUnknown,
     // Specials
     tjsEOF,tjsWhiteSpace,tjsChar,tjsString, tjsIdentifier,tjsNumber, tjsComment,tjsREGEX, tjsRESERVED,
     tjsANDAND, tjsANDEQ,
     tjsBraceOpen,tjsBraceClose,tjsSQuaredBraceOpen,tjsSQuaredBraceClose,tjsCurlyBraceOpen,tjsCurlyBraceClose,
     tjsCOMMA,tjsCOLON,  tjsDOT,tjsSEMICOLON, tjsASSIGN,tjsGT,tjsLT, tjsConditional,
     tjsPLUS,tjsMINUS,tjsMUL,tjsDIV,tjsAnd,tjsOR, tjsInv, tjsMod, tjsXOR, tjsNot,
     tjsEQ,
     tjsGE,
     tjsLE, tjsLSHIFT, tjsLSHIFTEQ,
     tjsMINUSEQ, tjsMINUSMINUS, tjsMODEQ,tjsDIVEQ,tjsXOREq,
     tjsNE,
     tjsOREQ, tjsOROR,
     tjsPLUSEQ, tjsPLUSPLUS,
     tjsURSHIFT, tjsURSHIFTEQ,
     tjsRSHIFT, tjsRSHIFTEQ,
     tjsSEQ, tjsSNE, tjsMULEQ,
     { Reserved words start here. They must be last }
     tjsBREAK,tjsCASE, tjsCATCH, tjsCONTINUE,
     tjsDEFAULT, tjsDELETE, tjsDO,
     tjsELSE,
     tjsFalse, tjsFINALLY, tjsFOR, tjsFUNCTION,
     tjsIF, tjsIN, tjsINSTANCEOF,
     tjsNEW,tjsNULL,
     tjsRETURN,
     tjsSWITCH,
     tjsTHIS, tjsTHROW, tjsTrue, tjsTRY, tjsTYPEOF,
     tjsVAR, tjsVOID,
     tjsWHILE, tjsWITH
   );

const
  FirstKeyword = tjsBreak;
  LastKeyWord = tJSWith;

  TokenInfos: array[TJSToken] of string = ('unknown',
       // Specials
        'EOF','whitespace','Char','String', 'identifier','number','comment','regular expression', 'reserved word',
        '&&','&=',
        '(',')','[',']','{','}',
        ',',':','.',';','=','>','<','?',
        '+','-','*','/','&','|','~','%','^','!',
        '==',
        '>=',
        '<=', '<<', '<<=',
        '-=', '--', '%=', '/=','^=',
        '!=',
        '|=', '||',
        '+=', '++',
        '>>>', '>>>=',
        '>>', '>>=',
        '===', '!==', '*=',
        // Identifiers last
        'break','case','catch', 'continue',
     'default','delete', 'do',
     'else',
     'false','finally', 'for', 'function',
     'if', 'in', 'instanceof',
     'new','null',
     'return',
     'switch',
     'this', 'throw', 'true', 'try', 'typeof',
     'var', 'void',
     'while', 'with'
    );


implementation

end.

