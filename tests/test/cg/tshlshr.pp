{***********************************************************}
{ CODE GENERATOR TEST SUITE                                *}
{***********************************************************}
{ NODE TESTED : secondshlshr()                             *}
{***********************************************************}
{ PRE-REQUISITES: secondload()                              }
{                 secondassign()                            }
{                 secondtypeconv()                          }
{***********************************************************}
{ DEFINES  :  VERBOSE if testing should be verbose          }
{             FPC if target is FreePascal compiler          }
{***********************************************************}
{ REMARKS: None                                             }
{***********************************************************}
Program tshlshr;

{----------------------------------------------------}
{ Cases to test:                                     }
{   RIGHT NODE (shift count value)                   }
{     - LOC_CREGISTER                                }
{     - LOC_REFERENCE / LOC_MEM                      }
{     - LOC_REGISTER                                 }
{     - numeric constant                             }
{   LEFT NODE (value to shift)                       }
{     - LOC_CREGISTER                                }
{     - LOC_REFERENCE / LOC_MEM                      }
{     - LOC_REGISTER                                 }
{----------------------------------------------------}


var
 longres :  longint;
 longcnt : longint;
 bytecnt : shortint;
 byteres : shortint;
{$IFDEF FPC}
 int64res : int64;
 int64cnt : int64;
{$ENDIF}
Begin
   WriteLn('------------------------------ LONGINT --------------------------------');

   { left : LOC_REFERENCE     }
   { right : numeric constant }
   WriteLn('(left) : LOC_REFERENCE; (right) : ordinal constant');
   longres:=1;
   longres := longres shl 15;
   WriteLn('(SHL) Value should be 32768...',longres);
   longres:=-1;
   longres := longres shl 15;
   WriteLn('(SHL) Value should be -32768...',longres);
   longres:=1;
   longres := longres shl 33;
   WriteLn('(SHL) Value should be 2...',longres);

   longres:=$8000;
   longres := longres shr 15;
   WriteLn('(SHR) Value should be 1...',longres);
   longres:=-1;
   longres := longres shr 15;
   WriteLn('(SHR) Value should be 131071...',longres);
   longres:=$FFFF;
   longres := longres shr 33;
   WriteLn('(SHR) Value should be 32767...',longres);

   { left : LOC_REFERENCE }
   { right : LOC_REFERENCE }
   WriteLn('(left) : LOC_REFERENCE; (right) : LOC_REFERENCE');
   longres := 1;
   longcnt := -2;
   longres:=longres shl longcnt ;
   WriteLn('(SHL) Value should be 1073741824...',longres);

   longres:=1;
   longcnt:=15;
   longres := longres shl longcnt;
   WriteLn('(SHL) Value should be 32768...',longres);
   longres:=-1;
   longcnt := 15;
   longres := longres shl longcnt;
   WriteLn('(SHL) Value should be -32768...',longres);

   longres := 1;
   longcnt := -2;
   longres:=longres shr longcnt ;
   WriteLn('(SHR) Value should be 0...',longres);

   longres:=32768;
   longcnt:=15;
   longres := longres shr longcnt;
   WriteLn('(SHR) Value should be 1...',longres);
   longres:=-1;
   longcnt := 15;
   longres := longres shl longcnt;
   WriteLn('(SHR) Value should be -32768...',longres);


   { left : LOC_REFERENCE }
   { right : LOC_REGISRER }

   WriteLn('(left) : LOC_REFERENCE; (right) : LOC_REGISTER');
   longres := 1;
   bytecnt := -2;
   longres:=longres shl bytecnt ;
   WriteLn('(SHL) Value should be 1073741824...',longres);

   longres:=1;
   bytecnt:=15;
   longres := longres shl bytecnt;
   WriteLn('(SHL) Value should be 32768...',longres);
   longres:=-1;
   bytecnt := 15;
   longres := longres shl bytecnt;
   WriteLn('(SHL) Value should be -32768...',longres);

   longres := 1;
   bytecnt := -2;
   longres:=longres shr bytecnt ;
   WriteLn('(SHR) Value should be 0...',longres);

   longres:=32768;
   bytecnt:=15;
   longres := longres shr bytecnt;
   WriteLn('(SHR) Value should be 1...',longres);
   longres:=-1;
   bytecnt := 15;
   longres := longres shr bytecnt;
   WriteLn('(SHR) Value should be 131071...',longres);


   WriteLn('(left) : LOC_REGISTER; (right) : LOC_REGISTER');
   byteres := 1;
   bytecnt := 2;
   byteres := byteres shl bytecnt;
   WriteLn('(SHL) Value should be 4...',byteres);

   byteres := 4;
   bytecnt := 2;
   byteres := byteres shr bytecnt;
   WriteLn('(SHR) Value should be 1...',byteres);

{$IFDEF FPC}
   WriteLn('------------------------------  INT64  --------------------------------');

   { left : LOC_REFERENCE     }
   { right : numeric constant }
   WriteLn('(left) : LOC_REFERENCE; (right) : ordinal constant');
   int64res:=1;
   int64res := int64res shl 15;
   WriteLn('(SHL) Value should be 32768...',int64res);
   int64res:=-1;
   int64res := int64res shl 15;
   WriteLn('(SHL) Value should be -32768...',int64res);
   int64res:=1;
   int64res := int64res shl 65;
   WriteLn('(SHL) Value should be 2...',int64res);

   int64res:=$8000;
   int64res := int64res shr 15;
   WriteLn('(SHR) Value should be 1...',int64res);
{   int64res:=-1;
   int64res := int64res shr 15;
   WriteLn('(SHR) Value should be 131071...',int64res);}
   int64res:=$FFFF;
   int64res := int64res shr 65;
   WriteLn('(SHR) Value should be 0...',int64res);

   { left : LOC_REFERENCE }
   { right : LOC_REFERENCE }
   WriteLn('(left) : LOC_REFERENCE; (right) : LOC_REFERENCE');
   int64res := 1;
   int64cnt := -2;
   int64res:=int64res shl int64cnt ;
   WriteLn('(SHL) Value should be 1073741824...',int64res);

   int64res:=1;
   int64cnt:=15;
   int64res := int64res shl int64cnt;
   WriteLn('(SHL) Value should be 32768...',int64res);
   int64res:=-1;
   int64cnt := 15;
   int64res := int64res shl int64cnt;
   WriteLn('(SHL) Value should be -32768...',int64res);

   int64res := 1;
   int64cnt := -2;
   int64res:=int64res shr int64cnt ;
   WriteLn('(SHR) Value should be 0...',int64res);

   int64res:=32768;
   int64cnt:=15;
   int64res := int64res shr int64cnt;
   WriteLn('(SHR) Value should be 1...',int64res);
   int64res:=-1;
   int64cnt := 15;
   int64res := int64res shl int64cnt;
   WriteLn('(SHR) Value should be -32768...',int64res);


   { left : LOC_REFERENCE }
   { right : LOC_REGISRER }

   WriteLn('(left) : LOC_REFERENCE; (right) : LOC_REGISTER');
   int64res := 1;
   bytecnt := -2;
   int64res:=int64res shl bytecnt ;
   WriteLn('(SHL) Value should be 1073741824...',int64res);

   int64res:=1;
   bytecnt:=15;
   int64res := int64res shl bytecnt;
   WriteLn('(SHL) Value should be 32768...',int64res);
   int64res:=-1;
   bytecnt := 15;
   int64res := int64res shl bytecnt;
   WriteLn('(SHL) Value should be -32768...',int64res);

   int64res := 1;
   bytecnt := -2;
   int64res:=int64res shr bytecnt ;
   WriteLn('(SHR) Value should be 0...',int64res);

   int64res:=32768;
   bytecnt:=15;
   int64res := int64res shr bytecnt;
   WriteLn('(SHR) Value should be 1...',int64res);
{   int64res:=-1;
   bytecnt := 15;
   int64res := int64res shr bytecnt;
   WriteLn('(SHR) Value should be 131071...',int64res);}

{$ENDIF}
end.

