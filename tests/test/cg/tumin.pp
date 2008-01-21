{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{****************************************************************}
{ NODE TESTED : secondunaryminus()                               }
{****************************************************************}
{ PRE-REQUISITES: secondload()                                   }
{                 secondassign()                                 }
{****************************************************************}
{ DEFINES:   VERBOSE = Write test information to screen          }
{            FPC     = Target is FreePascal compiler             }
{****************************************************************}
{ REMARKS:                                                       }
{                                                                }
{                                                                }
{                                                                }
{****************************************************************}
{$mode objfpc}

Program tumin;

{----------------------------------------------------}
{ Cases to test:                                     }
{   CURRENT NODE (result)                            }
{     - LOC_REGISTER                                 }
{     - LOC_FLAGS                                    }
{   LEFT NODE (value to complement)                  }
{     possible cases : int64,byte,word,longint       }
{                      boolean                       }
{     - LOC_CREGISTER                                }
{     - LOC_REFERENCE / LOC_MEM                      }
{     - LOC_REGISTER                                 }
{     - LOC_FLAGS                                    }
{     - LOC_JUMP                                     }
{----------------------------------------------------}

uses
  SysUtils;

{$IFNDEF FPC}
type  smallint = integer;
{$ENDIF}

function getintres : smallint;
begin
 getintres := $7F7F;
end;

function getbyteboolval : boolean;
begin
  getbyteboolval := TRUE;
end;

procedure test(value, required: longint);
begin
  if value <> required then
    begin
      writeln('Got ',value,' instead of ',required);
      halt(1);
    end
  else
    writeln('Passed!');
end;


{$Q+}
{$R+}

var
 caught: boolean;
 longres :  longint;
 cardres : cardinal;
 intres : smallint;
 byteboolval : bytebool;
 wordboolval : wordbool;
 longboolval : longbool;
 byteboolres : bytebool;
 wordboolres : wordbool;
 longboolres : longbool;
{$ifdef fpc}
 int64res : int64;
 qwordres : qword;
{$endif}
Begin
   WriteLn('------------------------------ LONGINT --------------------------------');
   { CURRENT NODE: REGISTER }
   { LEFT NODE : REFERENCE  }
   WriteLn('(current) : LOC_REGISTER; (left) : LOC_REFERENCE');
   longres := $7F7F7F7F;
   longres := -longres;
   Write('Value should be $80808081...');

   { the following test give range check errors }
   test(longres,longint($80808081));

   { CURRENT NODE : REGISTER }
   { LEFT NODE : REGISTER    }
   WriteLn('(current) : LOC_REGISTER; (left) : LOC_REGISTER');
   longres := - getintres;
   Write('Value should be $FFFF8081...');
   test(longres, longint($FFFF8081));


   Writeln('Overflow tests');
   Write('-0...');
   longres:=0;
   longres:=-longres;
   test(longres,0);
   longres:=high(longint);
   longres:=-longres;
   Write('-',high(longint),'...');
   test(longres,longint($80000001));

   Write('-(',low(longint),')...');
   longres:=low(longint);
   caught:=false;
   try
     longres:=-longres;
   except
{$ifdef cpu64}
     on erangeerror do
{$else cpu64}
     on eintoverflow do
{$endif cpu64}
       caught:=true;
   end;
   if not caught then
     begin
       Writeln('Overflow -$80000000 not caught');
       halt(1);
     end
   else
     writeln('Passed!');


   WriteLn('------------------------------  CARDINAL  ----------------------------------');

   Writeln('Overflow/Rangecheck tests');
   Write('-0...');
   cardres:=0;
   longres:=-cardres;
   test(longres,0);
   cardres:=high(longint);
   longres:=-cardres;
   Write('-',high(longint),'...');
   test(longres,longint($80000001));

   Write('-',high(cardinal),'...');
   cardres:=high(cardinal);
   caught:=false;
   try
     longres:=-cardres;
   except
     on erangeerror do
       caught:=true;
   end;
   if not caught then
     begin
       Writeln('Rangecheck -high(cardinal) not caught');
       halt(1);
     end
   else
     writeln('Passed!');

{$ifndef cpu64}
   { this is calculated in 64 bit on 64 bit cpus -> no range error }

   Write('-',cardinal($80000000),'...');
   cardres:=cardinal($80000000);
   caught:=false;
   try
     longres:=-cardres;
   except
     on erangeerror do
       caught:=true;
   end;
   if not caught then
     begin
       Writeln('Rangecheck -cardinal($80000000) not caught');
       halt(1);
     end
   else
     writeln('Passed!');
{$endif cpu64}

{$IFDEF FPC}
   WriteLn('------------------------------  INT64  ----------------------------------');
   { CURRENT NODE: REGISTER }
   { LEFT NODE : REFERENCE  }
   WriteLn('(current) : LOC_REGISTER; (left) : LOC_REFERENCE');
   int64res := $7F7F7F7F;
   int64res := - int64res;
   Write('Value should be $80808081...');
   test(longint(int64res and $FFFFFFFF),longint($80808081));

   { CURRENT NODE : REGISTER }
   { LEFT NODE : REGISTER    }
   WriteLn('(current) : LOC_REGISTER; (left) : LOC_REGISTER');
   int64res := - (word(getintres));
   Write('Value should be $8081...');
   test(longint(int64res and $FFFFFFFF),longint($FFFF8081));

   Writeln('Overflow tests');
   Write('-0...');
   int64res:=0;
   int64res:=-int64res;
   test(hi(int64res) or lo(int64res),0);
   int64res:=high(int64);
   int64res:=-int64res;
   Write('-',high(int64),'... (2 tests)');
   test(longint(hi(int64res)),longint($80000000));
   test(longint(lo(int64res)),1);

   Writeln('-(',low(int64),')...');
   int64res:=low(int64);
   caught:=false;
   try
     int64res:=-int64res;
   except
     on eintoverflow do
       caught:=true;
   end;
   if not caught then
     begin
       Writeln('Overflow -$8000000000000000 not caught');
       halt(1);
     end
   else
     writeln('Passed!');


   WriteLn('------------------------------  QWORD  ----------------------------------');

   Writeln('Overflow/Rangecheck tests');
   Write('-0...');
   qwordres:=0;
   int64res:=-qwordres;
   test(hi(int64res) or lo(int64res),0);
   qwordres:=high(int64);
   int64res:=-qwordres;
   Write('-',high(int64),'... (2 tests)');
   test(longint(hi(int64res)),longint($80000000));
   test(longint(lo(int64res)),1);

   Write('-',high(qword),'...');
   qwordres:=high(qword);
   caught:=false;
   try
     int64res:=-qwordres;
   except
     on erangeerror do
       caught:=true;
   end;
   if not caught then
     begin
       Writeln('Rangecheck -high(qword) not caught');
       halt(1);
     end
   else
     writeln('Passed!');

   Write('-',qword($8000000000000000),'...');
   qwordres:=qword($8000000000000000);
   caught:=false;
   try
     int64res:=-qwordres;
   except
     on erangeerror do
       caught:=true;
   end;
   if not caught then
     begin
       Writeln('Rangecheck -qword($8000000000000000) not caught');
       halt(1);
     end
   else
     writeln('Passed!');
{$ENDIF}


end.
