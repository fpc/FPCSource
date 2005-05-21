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
procedure test(value, required: {$ifndef fpc}longint{$else fpc}int64{$endif fpc});
begin
  if value <> required then
    begin
      writeln('Got ',value,' instead of ',required);
      halt(1);
    end
  else
    writeln('Passed!');
end;

type
tint64record = packed record
{$ifdef ENDIAN_BIG}
   highval : longint;
   lowval  : longint;
{$else}
   lowval  : longint;
   highval : longint;
{$endif}
end;


var
 longres :  longint;
 longcnt : longint;
 bytecnt : shortint;
 byteres : shortint;
{$IFDEF FPC}
 int64res : int64;
 int64cnt : int64;
 int64rec : tint64record;
{$ENDIF}
Begin
   WriteLn('------------------------------ LONGINT --------------------------------');
   { left : LOC_REFERENCE     }
   { right : numeric constant }
   WriteLn('(left) : LOC_REFERENCE; (right) : ordinal constant');
   longres:=1;
   longres := longres shl 15;
   Write('(SHL) Value should be 32768...');
   test(longres, 32768);

   longres:=-1;
   longres := longres shl 15;
   Write('(SHL) Value should be -32768...');
   test(longres, -32768);

   longres:=1;
   longres := longres shl 33;
   Write('(SHL) Value should be 2...');
   test(longres, 2);

   longres:=$8000;
   longres := longres shr 15;
   Write('(SHR) Value should be 1...');
   test(longres, 1);

   longres:=-1;
   longres := longres shr 15;
   Write('(SHR) Value should be 131071...');
   test(longres, 131071);

   longres:=$FFFF;
   longres := longres shr 33;
   Write('(SHR) Value should be 32767...');
   test(longres, 32767);

   { left : LOC_REFERENCE }
   { right : LOC_REFERENCE }
   WriteLn('(left) : LOC_REFERENCE; (right) : LOC_REFERENCE');

{
   longres := 1;
   longcnt := -2;
   longres:=longres shl longcnt ;
   Write('(SHL) Value should be 1073741824...');
   test(longres, 1073741824);
}

   longres:=1;
   longcnt:=15;
   longres := longres shl longcnt;
   Write('(SHL) Value should be 32768...');
   test(longres, 32768);

   longres:=-1;
   longcnt := 15;
   longres := longres shl longcnt;
   Write('(SHL) Value should be -32768...');
   test(longres, -32768);

{
   longres := 1;
   longcnt := -2;
   longres:=longres shr longcnt ;
   Write('(SHR) Value should be 0...');
   test(longres, 0);
}

   longres:=32768;
   longcnt:=15;
   longres := longres shr longcnt;
   Write('(SHR) Value should be 1...');
   test(longres, 1);

   longres:=-1;
   longcnt := 15;
   longres := longres shl longcnt;
   Write('(SHR) Value should be -32768...');
   test(longres, -32768);

   { left : LOC_REFERENCE }
   { right : LOC_REGISRER }
{
   WriteLn('(left) : LOC_REFERENCE; (right) : LOC_REGISTER');
   longres := 1;
   bytecnt := -2;
   longres:=longres shl bytecnt ;
   Write('(SHL) Value should be 1073741824...');
   test(longres, 1073741824);
}

   longres:=1;
   bytecnt:=15;
   longres := longres shl bytecnt;
   Write('(SHL) Value should be 32768...');
   test(longres, 32768);

   longres:=-1;
   bytecnt := 15;
   longres := longres shl bytecnt;
   Write('(SHL) Value should be -32768...');
   test(longres, -32768);

{
   longres := 1;
   bytecnt := -2;
   longres:=longres shr bytecnt ;
   Write('(SHR) Value should be 0...');
   test(longres, 0);
}

   longres:=32768;
   bytecnt:=15;
   longres := longres shr bytecnt;
   Write('(SHR) Value should be 1...');
   test(longres, 1);

   longres:=-1;
   bytecnt := 15;
   longres := longres shr bytecnt;
   Write('(SHR) Value should be 131071...');
   test(longres, 131071);

   WriteLn('(left) : LOC_REGISTER; (right) : LOC_REGISTER');
   byteres := 1;
   bytecnt := 2;
   byteres := byteres shl bytecnt;
   Write('(SHL) Value should be 4...');
   test(byteres, 4);


   byteres := 4;
   bytecnt := 2;
   byteres := byteres shr bytecnt;
   Write('(SHR) Value should be 1...');
   test(byteres, 1);

{$IFDEF FPC}
   WriteLn('------------------------------  INT64  --------------------------------');
   { left : LOC_REFERENCE     }
   { right : numeric constant }
   WriteLn('(left) : LOC_REFERENCE; (right) : ordinal constant');
   int64res:=1;
   int64res := int64res shl 15;
   Write('(SHL) Value should be 32768...');
   test(int64res, 32768);

   int64res:=-1;
   int64res := int64res shl 15;
   Write('(SHL) Value should be -32768...');
   test(int64res, -32768);


   int64res:=1;
   int64res := int64res shl 65;
   Write('(SHL) Value should be 2...');
   test(int64res, 2);

   int64res:=$8000;
   int64res := int64res shr 15;
   Write('(SHR) Value should be 1...');
   test(int64res, 1);

   int64res:=$FFFF;
   int64res := int64res shr 65;
   Write('(SHR) Value should be 32767...');
   test(int64res, 32767);

   { left : LOC_REFERENCE }
   { right : LOC_REFERENCE }
{
   WriteLn('(left) : LOC_REFERENCE; (right) : LOC_REFERENCE');
   int64res := 1;
   int64cnt := -2;
   int64res:=int64res shl int64cnt ;
   Write('(SHL) Value should be 1073741824...');
   test(int64res, 1073741824);
}

   int64res:=1;
   int64cnt:=15;
   int64res := int64res shl int64cnt;
   Write('(SHL) Value should be 32768...');
   test(int64res, 32768);


   int64res:=-1;
   int64cnt := 15;
   int64res := int64res shl int64cnt;
   Write('(SHL) Value should be -32768...');
   test(int64res, -32768);

   int64res := 1;
   int64cnt := 33;
   int64res := int64res shl int64cnt;
   Write('(SHL) Value should be 2 in high longint (85899345)...');
   move(int64res,int64rec, sizeof(int64));
   test(int64rec.highval, 2);
{   test(int64res, 8589934592);}


{
   int64res := 1;
   int64cnt := -2;
   int64res:=int64res shr int64cnt ;
   Write('(SHR) Value should be 0...');
   test(int64res and $FFFFFFFF, 0);
}
   int64res:=32768;
   int64cnt:=15;
   int64res := int64res shr int64cnt;
   Write('(SHR) Value should be 1...');
   test(int64res, 1);

   int64res:=-1;
   int64cnt := 15;
   int64res := int64res shl int64cnt;
   Write('(SHR) Value should be -32768...');
   test(int64res, -32768);

   { left : LOC_REFERENCE }
   { right : LOC_REGISRER }
{
  WriteLn('(left) : LOC_REFERENCE; (right) : LOC_REGISTER');
   int64res := 1;
   bytecnt := -2;
   int64res:=int64res shl bytecnt ;
   Write('(SHL) Value should be 1073741824...');
   test(int64res, 1073741824);
}

   int64res:=1;
   bytecnt:=15;
   int64res := int64res shl bytecnt;
   Write('(SHL) Value should be 32768...');
   test(int64res, 32768);


   int64res:=-1;
   bytecnt := 15;
   int64res := int64res shl bytecnt;
   Write('(SHL) Value should be -32768...');
   test(int64res, -32768);

{
   int64res := 1;
   bytecnt := -2;
   int64res:=int64res shr bytecnt ;
   Write('(SHR) Value should be 0...');
   test(int64res and $FFFFFFFF, 0);
}

   int64res:=32768;
   bytecnt:=15;
   int64res := int64res shr bytecnt;
   Write('(SHR) Value should be 1...');
   test(int64res, 1);

   int64res := 1;
   bytecnt := 33;
   int64res := int64res shl bytecnt;
   Write('(SHL) Value should be 2 in high longint (85899345)...');
   move(int64res,int64rec, sizeof(int64));
   test(int64rec.highval, 2);

{   int64res:=-1;
   bytecnt := 15;
   int64res := int64res shr bytecnt;
   WriteLn('(SHR) Value should be 131071...',int64res);}

{$ENDIF}
end.
