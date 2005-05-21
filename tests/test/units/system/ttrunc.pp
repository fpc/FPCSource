{ this tests the trunc routine }
program ttrunc;

{$ifdef VER1_0}
  {$define SKIP_CURRENCY_TEST}
{$endif }

{$ifndef MACOS}
{$APPTYPE CONSOLE}
{$else}
{$APPTYPE TOOL}
{$endif}

const
  RESULT_ONE = 1234;
  VALUE_ONE = 1234.5678;
  RESULT_CONST_ONE = trunc(VALUE_ONE);
  RESULT_TWO = -1234;
  VALUE_TWO = -1234.5678;
  RESULT_CONST_TWO = trunc(VALUE_TWO);


 procedure fail;
  begin
    WriteLn('Failed!');
    halt(1);
  end;

procedure test_trunc_real;
var
 r: real;
 _success : boolean;
 l: longint;
Begin
 Write('Trunc() real testing...');
 _success := true;
 r:=VALUE_ONE;
 if Trunc(r)<>RESULT_ONE then
   _success:=false;
 if Trunc(VALUE_ONE)<>RESULT_ONE then
   _success:=false;
 r:=VALUE_ONE;
 if Trunc(r)<>RESULT_CONST_ONE then
   _success := false;
 r:=VALUE_ONE;
 l:=Trunc(r);
 if l<>RESULT_ONE then
   _success:=false;
 l:=Trunc(VALUE_ONE);
 if l<>RESULT_ONE then
   _success:=false;


 r:=VALUE_TWO;
 if Trunc(r)<>RESULT_TWO then
   _success:=false;
 if Trunc(VALUE_TWO)<>RESULT_TWO then
   _success:=false;
 r:=VALUE_TWO;
 if Trunc(r)<>RESULT_CONST_TWO then
   _success := false;
 r:=VALUE_TWO;
 l:=Trunc(r);
 if l<>RESULT_TWO then
   _success:=false;
 l:=Trunc(VALUE_TWO);
 if l<>RESULT_TWO then
   _success:=false;


 if not _success then
   fail;
 WriteLn('Success!');
end;

procedure test_trunc_single;
var
 r: single;
 _success : boolean;
 l: longint;
Begin
 Write('Trunc() single testing...');
 _success := true;
 r:=VALUE_ONE;
 if Trunc(r)<>RESULT_ONE then
   _success:=false;
 if Trunc(VALUE_ONE)<>RESULT_ONE then
   _success:=false;
 r:=VALUE_ONE;
 if Trunc(r)<>RESULT_CONST_ONE then
   _success := false;
 r:=VALUE_ONE;
 l:=Trunc(r);
 if l<>RESULT_ONE then
   _success:=false;
 l:=Trunc(VALUE_ONE);
 if l<>RESULT_ONE then
   _success:=false;


 r:=VALUE_TWO;
 if Trunc(r)<>RESULT_TWO then
   _success:=false;
 if Trunc(VALUE_TWO)<>RESULT_TWO then
   _success:=false;
 r:=VALUE_TWO;
 if Trunc(r)<>RESULT_CONST_TWO then
   _success := false;
 r:=VALUE_TWO;
 l:=Trunc(r);
 if l<>RESULT_TWO then
   _success:=false;
 l:=Trunc(VALUE_TWO);
 if l<>RESULT_TWO then
   _success:=false;


 if not _success then
   fail;
 WriteLn('Success!');
end;


procedure test_trunc_double;
var
 r: double;
 _success : boolean;
 l: longint;
Begin
 Write('Trunc() double testing...');
 _success := true;
 r:=VALUE_ONE;
 if Trunc(r)<>RESULT_ONE then
   _success:=false;
 if Trunc(VALUE_ONE)<>RESULT_ONE then
   _success:=false;
 r:=VALUE_ONE;
 if Trunc(r)<>RESULT_CONST_ONE then
   _success := false;
 r:=VALUE_ONE;
 l:=Trunc(r);
 if l<>RESULT_ONE then
   _success:=false;
 l:=Trunc(VALUE_ONE);
 if l<>RESULT_ONE then
   _success:=false;


 r:=VALUE_TWO;
 if Trunc(r)<>RESULT_TWO then
   _success:=false;
 if Trunc(VALUE_TWO)<>RESULT_TWO then
   _success:=false;
 r:=VALUE_TWO;
 if Trunc(r)<>RESULT_CONST_TWO then
   _success := false;
 r:=VALUE_TWO;
 l:=Trunc(r);
 if l<>RESULT_TWO then
   _success:=false;
 l:=Trunc(VALUE_TWO);
 if l<>RESULT_TWO then
   _success:=false;


 if not _success then
   fail;
 WriteLn('Success!');
end;


{$ifndef SKIP_CURRENCY_TEST}
procedure test_trunc_currency;
var
 r: currency;
 _success : boolean;
 l: longint;
Begin
 Write('Trunc() currency testing...');
 _success := true;
 r:=VALUE_ONE;
 if Trunc(r)<>RESULT_ONE then
   _success:=false;
 if Trunc(VALUE_ONE)<>RESULT_ONE then
   _success:=false;
 r:=VALUE_ONE;
 if Trunc(r)<>RESULT_CONST_ONE then
   _success := false;
 r:=VALUE_ONE;
 l:=Trunc(r);
 if l<>RESULT_ONE then
   _success:=false;
 l:=Trunc(VALUE_ONE);
 if l<>RESULT_ONE then
   _success:=false;


 r:=VALUE_TWO;
 if Trunc(r)<>RESULT_TWO then
   _success:=false;
 if Trunc(VALUE_TWO)<>RESULT_TWO then
   _success:=false;
 r:=VALUE_TWO;
 if Trunc(r)<>RESULT_CONST_TWO then
   _success := false;
 r:=VALUE_TWO;
 l:=Trunc(r);
 if l<>RESULT_TWO then
   _success:=false;
 l:=Trunc(VALUE_TWO);
 if l<>RESULT_TWO then
   _success:=false;


 if not _success then
   fail;
 WriteLn('Success!');
end;
{$endif SKIP_CURRENCY_TEST}


Begin
  test_trunc_real;
  test_trunc_single;
  test_trunc_double;
{$ifdef SKIP_CURRENCY_TEST}
  Writeln('Skipping currency test because its not supported by theis compiler');
{$else SKIP_CURRENCY_TEST}
  test_trunc_currency;
{$endif SKIP_CURRENCY_TEST}
end.
