{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{****************************************************************}
{ NODE TESTED : secondunaryminus()                               }
{****************************************************************}
{ PRE-REQUISITES: secondload()                                   }
{                 secondassign()                                 }
{                 secondtypeconv()                               }
{****************************************************************}
{ DEFINES:                                                       }
{            FPC     = Target is FreePascal compiler             }
{****************************************************************}
{ REMARKS:                                                       }
{                                                                }
{                                                                }
{                                                                }
{****************************************************************}

Program tneg;

{----------------------------------------------------}
{ Cases to test:                                     }
{   CURRENT NODE (result value)                      }
{     - LOC_REGISTER                                 }
{     - LOC_FPU                                      }
{   LEFT NODE (value to negate)                      }
{     - LOC_CREGISTER                                }
{     - LOC_REFERENCE / LOC_MEM                      }
{     - LOC_REGISTER                                 }
{     - LOC_FPU                                      }
{----------------------------------------------------}

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

procedure fail;
 begin
   writeln('Failure.');
   halt(1);
 end;


  function getreal: real;
   begin
     getreal := 1.0;
   end;

var
 longval :  longint;
 realval : real;
 byteval : longint;
{$IFDEF FPC}
 int64val : int64;
{$ENDIF}
Begin
   WriteLn('------------------------------ LONGINT --------------------------------');
   { CURRENT NODE: REGISTER }
   { LEFT NODE : REFERENCE  }
   longval := 1;
   longval := - longval;
   Write('Value should be -1...');
   test(longval, -1);

   { CURRENT NODE : REGISTER }
   { LEFT NODE: REGISTER     }
   byteval := 2;
   longval := - byteval;
   Write('Value should be -2...');
   test(longval, -2);

   { CURRENT NODE: LOC_FPU }
   { LEFT NODE : LOC_REFERENCE }
   realval := -1.0;
   realval := - realval;
   Write('Value should 1.0...');
   if realval - 1.0 = 0.0 then
      WriteLn('Passed!')
   else
      Fail;

   { LEFT NODE : LOC_FPU }
   { CURRENT NODE : LOC_FPU }
   realval := -1.0;
   realval := -(getreal*(realval));
   Write('Value should 1.0...');
   if realval - 1.0 = 0.0 then
      WriteLn('Passed!')
   else
      Fail;

{$IFDEF FPC}
   WriteLn('------------------------------  INT64  --------------------------------');
   { CURRENT NODE: REGISTER }
   { LEFT NODE : REFERENCE  }
   int64val := 1;
   int64val := - int64val;
   Write('Value should be -1...');

   { the following test give range check errors }
   {$R-}
   test(int64val and $FFFFFFFF, -1);

   { CURRENT NODE : REGISTER }
   { LEFT NODE: REGISTER     }
   byteval := 2;
   int64val := - byteval;
   Write('Value should be -2...');
   test(int64val and $FFFFFFFF, -2);
{$ENDIF}
end.
