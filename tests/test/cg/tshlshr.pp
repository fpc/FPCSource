{***********************************************************}
{ CODE GENERATOR TEST SUITE                                *}
{***********************************************************}
{ NODE TESTED : secondshlshr()                             *}
{***********************************************************}
{ PRE-REQUISITES: secondload()                              }
{                 secondassign()                            }
{                 secondtypeconv()                          }
{                 secondinline() with strings only!         }
{                 secondadd() comparison                    }
{                 secondifn()                               }
{***********************************************************}
{ DEFINES  :  FPC if target is Free Pascal compiler         }
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
   Write('(SHL) Value should be 32768...');
   if longres = 32768 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');
   longres:=-1;
   longres := longres shl 15;
   Write('(SHL) Value should be -32768...');
   if longres = -32768 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');
   longres:=1;
   longres := longres shl 33;
   Write('(SHL) Value should be 2...');
   if longres = 2 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');

   longres:=$8000;
   longres := longres shr 15;
   Write('(SHR) Value should be 1...');
   if longres = 1 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');

   longres:=-1;
   longres := longres shr 15;
   Write('(SHR) Value should be 131071...');
   if longres = 131071 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');

   longres:=$FFFF;
   longres := longres shr 33;
   Write('(SHR) Value should be 32767...');
   if longres = 32767 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');

   { left : LOC_REFERENCE }
   { right : LOC_REFERENCE }
   WriteLn('(left) : LOC_REFERENCE; (right) : LOC_REFERENCE');

   longres := 1;
   longcnt := -2;
   longres:=longres shl longcnt ;
   Write('(SHL) Value should be 1073741824...');
   if longres = 1073741824 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');

   longres:=1;
   longcnt:=15;
   longres := longres shl longcnt;
   Write('(SHL) Value should be 32768...');
   if longres = 32768 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');

   longres:=-1;
   longcnt := 15;
   longres := longres shl longcnt;
   Write('(SHL) Value should be -32768...');
   if longres = -32768 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');

   longres := 1;
   longcnt := -2;
   longres:=longres shr longcnt ;
   Write('(SHR) Value should be 0...');
   if longres = 0 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');

   longres:=32768;
   longcnt:=15;
   longres := longres shr longcnt;
   Write('(SHR) Value should be 1...');
   if longres = 1 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');
   longres:=-1;
   longcnt := 15;
   longres := longres shl longcnt;
   Write('(SHR) Value should be -32768...');
   if longres = -32768 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');

   { left : LOC_REFERENCE }
   { right : LOC_REGISRER }
   WriteLn('(left) : LOC_REFERENCE; (right) : LOC_REGISTER');
   longres := 1;
   bytecnt := -2;
   longres:=longres shl bytecnt ;
   Write('(SHL) Value should be 1073741824...');
   if longres = 1073741824 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');

   longres:=1;
   bytecnt:=15;
   longres := longres shl bytecnt;
   Write('(SHL) Value should be 32768...');
   if longres = 32768 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');

   longres:=-1;
   bytecnt := 15;
   longres := longres shl bytecnt;
   Write('(SHL) Value should be -32768...');
   if longres = -32768 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');

   longres := 1;
   bytecnt := -2;
   longres:=longres shr bytecnt ;
   Write('(SHR) Value should be 0...');
   if longres = 0 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');

   longres:=32768;
   bytecnt:=15;
   longres := longres shr bytecnt;
   Write('(SHR) Value should be 1...');
   if longres = 1 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');

   longres:=-1;
   bytecnt := 15;
   longres := longres shr bytecnt;
   Write('(SHR) Value should be 131071...');
   if longres = 131071 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');

   WriteLn('(left) : LOC_REGISTER; (right) : LOC_REGISTER');
   byteres := 1;
   bytecnt := 2;
   byteres := byteres shl bytecnt;
   Write('(SHL) Value should be 4...');
   if longres = 4 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');


   byteres := 4;
   bytecnt := 2;
   byteres := byteres shr bytecnt;
   Write('(SHR) Value should be 1...');
   if longres = 1 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');

{$IFDEF FPC}
   WriteLn('------------------------------  INT64  --------------------------------');
   { left : LOC_REFERENCE     }
   { right : numeric constant }
   WriteLn('(left) : LOC_REFERENCE; (right) : ordinal constant');
   int64res:=1;
   int64res := int64res shl 15;
   Write('(SHL) Value should be 32768...');
   if int64res = 32768 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');

   int64res:=-1;
   int64res := int64res shl 15;
   Write('(SHL) Value should be -32768...');
   if int64res = -32768 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');


   int64res:=1;
   int64res := int64res shl 65;
   Write('(SHL) Value should be 2...');
   if int64res = 2 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');

   int64res:=$8000;
   int64res := int64res shr 15;
   Write('(SHR) Value should be 1...');
   if int64res = 1 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');

{   int64res:=-1;
   int64res := int64res shr 15;
   Write('(SHR) Value should be 131071...');}
   int64res:=$FFFF;
   int64res := int64res shr 65;
   Write('(SHR) Value should be 0...');
   if int64res = 0 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');

   { left : LOC_REFERENCE }
   { right : LOC_REFERENCE }
   WriteLn('(left) : LOC_REFERENCE; (right) : LOC_REFERENCE');
   int64res := 1;
   int64cnt := -2;
   int64res:=int64res shl int64cnt ;
   Write('(SHL) Value should be 1073741824...');
   if int64res = 1073741824 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');

   int64res:=1;
   int64cnt:=15;
   int64res := int64res shl int64cnt;
   Write('(SHL) Value should be 32768...');
   if int64res = 32768 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');


   int64res:=-1;
   int64cnt := 15;
   int64res := int64res shl int64cnt;
   Write('(SHL) Value should be -32768...');
   if int64res = -32768 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');


   int64res := 1;
   int64cnt := -2;
   int64res:=int64res shr int64cnt ;
   Write('(SHR) Value should be 0...');
   if int64res = 0 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');

   int64res:=32768;
   int64cnt:=15;
   int64res := int64res shr int64cnt;
   Write('(SHR) Value should be 1...');
   if int64res = 1 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');

   int64res:=-1;
   int64cnt := 15;
   int64res := int64res shl int64cnt;
   Write('(SHR) Value should be -32768...');
   if int64res = -32768 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');

   { left : LOC_REFERENCE }
   { right : LOC_REGISRER }
   WriteLn('(left) : LOC_REFERENCE; (right) : LOC_REGISTER');
   int64res := 1;
   bytecnt := -2;
   int64res:=int64res shl bytecnt ;
   Write('(SHL) Value should be 1073741824...');
   if int64res = 1073741824 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');


   int64res:=1;
   bytecnt:=15;
   int64res := int64res shl bytecnt;
   Write('(SHL) Value should be 32768...');
   if int64res = 32768 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');


   int64res:=-1;
   bytecnt := 15;
   int64res := int64res shl bytecnt;
   Write('(SHL) Value should be -32768...');
   if int64res = -32768 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');


   int64res := 1;
   bytecnt := -2;
   int64res:=int64res shr bytecnt ;
   Write('(SHR) Value should be 0...');
   if int64res = 0 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');


   int64res:=32768;
   bytecnt:=15;
   int64res := int64res shr bytecnt;
   Write('(SHR) Value should be 1...');
   if int64res = 1 then
     WriteLn('Success.')
   else
     WriteLn('Failure.');
{   int64res:=-1;
   bytecnt := 15;
   int64res := int64res shr bytecnt;
   WriteLn('(SHR) Value should be 131071...',int64res);}

{$ENDIF}
end.

