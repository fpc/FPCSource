{ ********************************************************************* 
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2016 Michael Van Canneyt.
       
    Javascript token definitions
            
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
                   
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
                                
  **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit jstoken;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

type

  TJSToken = (tjsUnknown,
     // Specials
     tjsEOF,tjsWhiteSpace,tjsChar,tjsString{this bites TJSString}, tjsIdentifier,tjsNumber, tjsComment,tjsREGEX, tjsRESERVED,
     tjsANDAND, tjsANDEQ,
     tjsBraceOpen,tjsBraceClose,tjsSQuaredBraceOpen,tjsSQuaredBraceClose,tjsCurlyBraceOpen,tjsCurlyBraceClose,
     tjsCOMMA,tjsCOLON,  tjsDOT,tjsSEMICOLON, tjsASSIGN,tjsGT,tjsLT, tjsConditional,
     tjsPLUS,tjsMINUS,tjsMUL,tjsDIV,tjsPower, tjsAnd,tjsOR, tjsInv, tjsMod, tjsXOR, tjsNot,
     tjsEQ,
     tjsGE,
     tjsLE, tjsLSHIFT, tjsLSHIFTEQ,
     tjsMINUSEQ, tjsMINUSMINUS, tjsMODEQ,tjsDIVEQ,tjsXOREq,
     tjsNE,
     tjsOREQ, tjsOROR,
     tjsPLUSEQ, tjsPLUSPLUS,
     tjsURSHIFT, tjsURSHIFTEQ,
     tjsRSHIFT, tjsRSHIFTEQ,
     tjsSEQ, tjsSNE, tjsMULEQ, tjsArrow, tjsEllipsis,
     { Reserved words start here. They must be last }
     tjsAWAIT, tjsBREAK, tjsCASE, tjsCATCH, tjsCLASS, tjsCONST, tjsCONTINUE,
     tjsDEBUGGER, tjsDEFAULT, tjsDELETE, tjsDO,
     tjsELSE, tjsENUM, tjsEXPORT, tjsEXTENDS,
     tjsFALSE, tjsFINALLY, tjsFOR, tjsFUNCTION,
     tjsIF, tjsIMPORT, tjsIN, tjsINSTANCEOF,
     tjsLet,
     tjsNEW, tjsNULL,
     tjsRETURN,
     tjsSUPER, tjsSWITCH,
     tjsTHIS, tjsTHROW, tjsTrue, tjsTRY, tjsTYPEOF,
     tjsVAR, tjsVOID,
     tjsWHILE, tjsWITH,
     tjsYield
   );
   TJSTokens = Set of TJSToken;

const
  FirstKeyword = tjsAwait;
  LastKeyWord = tJSYield;

  TokenInfos: array[TJSToken] of String = ('unknown',
       // Specials
        'EOF','whitespace','AnsiChar','String', 'identifier','number','comment','regular expression', 'reserved word',
        '&&','&=',
        '(',')','[',']','{','}',
        ',',':','.',';','=','>','<','?',
        '+','-','*','/','**','&','|','~','%','^','!',
        '==',
        '>=',
        '<=', '<<', '<<=',
        '-=', '--', '%=', '/=','^=',
        '!=',
        '|=', '||',
        '+=', '++',
        '>>>', '>>>=',
        '>>', '>>=',
        '===', '!==', '*=', '=>', '...',
        // Identifiers last
        'await', 'break','case','catch', 'class','const','continue',
     'debugger','default','delete', 'do',
     'else','enum','export','extends',
     'false','finally', 'for', 'function',
     'if', 'import', 'in', 'instanceof',
     'let',
     'new','null',
     'return',
     'super', 'switch',
     'this', 'throw', 'true', 'try', 'typeof',
     'var', 'void',
     'while', 'with',
     'yield'
    );


implementation

end.

