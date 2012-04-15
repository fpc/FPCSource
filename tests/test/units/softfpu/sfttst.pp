{****************************************************************}
{  Softfloat module testsuit                                     }
{****************************************************************}
{  Copyright (c) 2002 Carl Eric Codere                           }
{****************************************************************}

{$E+}

program sfttst;

uses softfpu;

const
  softresult_ok : boolean = false;
  softresult_error_count : longint = 0;

procedure fail;
begin
  WriteLn('Failed!');
  inc(softresult_error_count);
end;

 function singletofloat32(r: single):float32rec;
  var
   _result: float32rec;
  begin
    move(r,_result, sizeof(r));
    singletofloat32 := _result;
  end;

function float32tosingle(r: float32rec): single;
 var
  _result : single;
 begin
   move(r, _result, sizeof(r));
   float32tosingle := _result;
 end;


 function doubletofloat64(r: double):float64;
  var
   _result: float64;
  begin
    move(r,_result, sizeof(r));
    doubletofloat64 := _result;
  end;

function float64todouble(r: float64): double;
 var
  _result : double;
 begin
   move(r, _result, sizeof(r));
   float64todouble := _result;
 end;


{******************************************************************************}
{*                            single arithmetic                               *}
{******************************************************************************}
 Procedure float32TestSub;
 var
  i : single;
  j : single;
  val1,val2 : float32rec;
 Begin
  Write('single - single test...');
  softresult_ok := true;
  i:=99.9;
  j:=10.0;
  val1:=singletofloat32(i);
  val2:=singletofloat32(j);
  { i:=i-j }
  val1:= float32_sub(val1,val2);
  i:=float32tosingle(val1);
  j:=float32tosingle(val2);
  if trunc(i) <> trunc(89.9) then
    softresult_ok := false;
  WriteLn('Result (89.9) :',i);
  val1:=singletofloat32(i);
  val2:=singletofloat32(j);
  { i:=j-i }
  val1:= float32_sub(val2,val1);
  i:=float32tosingle(val1);
  j:=float32tosingle(val2);
  if trunc(i) <> trunc(-79.9) then
    softresult_ok := false;
  WriteLn('Result (-79.9) :',i);
  val1:=singletofloat32(j);
  val2:=singletofloat32(10.0);
  { j:=j-10.0 }
  val1:= float32_sub(val1,val2);
  j:=float32tosingle(val1);
  if j <> 0.0 then
    softresult_ok := false;
  WriteLn('Result (0.0) :',j);
  if not softresult_ok then
    Fail
  else
    WriteLn('Success.');
 end;

 procedure float32TestAdd;
 var
  i : single;
  j : single;
  val1, val2 : float32rec;
 Begin
   WriteLn('single + single test...');
   softresult_ok := true;
   i:= 9;
 {  i:=i+1.5;}
   val1:=float32_add(singletofloat32(i),singletofloat32(1.5));
   i:=float32tosingle(val1);
   if trunc(i) <> trunc(10.5) then
     softresult_ok := false;
   WriteLn('Result (10.5) :',i);
   i := 326788.12345;
   j := 100.0;
{   i := i + j + 12.5;}
   val1 := singletofloat32(i);
   val2 := singletofloat32(j);
   val1:=float32_add(val1,val2);   { i:=i+j }
   val1:=float32_add(val1,singletofloat32(12.5));
   i:=float32tosingle(val1);
   if trunc(i) <> trunc(326900.12345) then
     softresult_ok := false;
   WriteLn('Result (326900.12345) :',i);
   if not softresult_ok then
    Fail
   else
    WriteLn('Success.');
 end;


 procedure float32testmul;
 var
  i : single;
  j : single;
  val1 : float32rec;
 begin
  WriteLn('single * single test...');
  softresult_ok := true;
  i:= 21111.0;
  j:= 11.1;
{  i := i * j * i; }
  val1:=float32_mul(singletofloat32(i),singletofloat32(j));
  i:=float32tosingle(val1);
  if trunc(i) <> trunc(234332.1) then
    softresult_ok := false;
  WriteLn('Result (234332.1) :',i);
  i := 10.0;
  j := -12.0;
{  i := i * j * 10.0;}
  val1:=float32_mul(float32_mul(singletofloat32(i),singletofloat32(j)),singletofloat32(10.0));
  i:=float32tosingle(val1);
  if trunc(i) <> trunc(-1200.0) then
    softresult_ok := false;
  WriteLn('Result (-1200.0) :',i);
  if not softresult_ok then
    Fail
  else
    WriteLn('Success.');
 end;



 Procedure float32TestDiv;
 var
  i : single;
  j : single;
  val1 : float32rec;
 Begin
  softresult_ok := true;
  WriteLn('single / single test...');
  i:=-99.9;
  j:=10.0;
{  i:=i / j; }
  val1:=float32_div(singletofloat32(i),singletofloat32(j));
  i:=float32tosingle(val1);
  if trunc(i) <> trunc(-9.9) then
    softresult_ok := false;
  WriteLn('Result (-9.9) :',i);
  {i:=j / i;}
  val1:=float32_div(singletofloat32(j),singletofloat32(i));
  i:=float32tosingle(val1);
  if trunc(i) <> trunc(-1.01) then
    softresult_ok := false;
  WriteLN('Result (-1.01) :',i);
{  j:=i / 10.0; }
  val1:=float32_div(singletofloat32(i),singletofloat32(10.0));
  j:=float32tosingle(val1);
  if trunc(j) <> trunc(-0.1001) then
    softresult_ok := false;
  WriteLn('Result (-0.1001) :',j);
  if not softresult_ok then
    Fail
  else
    WriteLn('Success.');
 end;

 procedure float32testequal;
 var
  i : single;
  j : single;
  val1,val2 : float32rec;
 begin
  softresult_ok := true;
  Write('single = single test...');
  i := 1000.0;
  j := 1000.0;
  val1 := singletofloat32(i);
  val2 := singletofloat32(j);
  if (float32_eq(val1,val2)=0) then
    softresult_ok := false;
  i := -112345.1;
  j := -112345.1;
  val1 := singletofloat32(i);
  val2 := singletofloat32(j);
  if (float32_eq(val1,val2)=0) then
    softresult_ok := false;
  i := 4502020.1125E+03;
  j := 4502020.1125E+03;
  val1 := singletofloat32(i);
  val2 := singletofloat32(j);
  if (float32_eq(val1,val2)=0) then
    softresult_ok := false;
  i := -4502028.1125E+03;
  j := -4502028.1125E+03;
  val1 := singletofloat32(i);
  val2 := singletofloat32(j);
  if (float32_eq(val1,val2)=0) then
    softresult_ok := false;
  i := -4502030.1125E+03;
  j := -4502028.1125E+03;
  val1 := singletofloat32(i);
  val2 := singletofloat32(j);
  if (float32_eq(val1,val2)<>0) then
    softresult_ok := false;
  if not softresult_ok then
    Fail
  else
    WriteLn('Success.');
 end;

 procedure float32testle;
 var
  i : single;
  j : single;
  val1,val2: float32rec;
 begin
  softresult_ok := true;
  Write('single <= single test...');
  i := 1000.0;
  j := 1000.0;
  val1 := singletofloat32(i);
  val2 := singletofloat32(j);
  if (float32_le(val1,val2)=0) then
    softresult_ok := false;
  i := 10000.0;
  j := 999.0;
  val1 := singletofloat32(i);
  val2 := singletofloat32(j);
  if (float32_le(val2,val1)=0) then
    softresult_ok := false;
  i := -10000.0;
  j := -999.0;
  val1 := singletofloat32(i);
  val2 := singletofloat32(j);
  if (float32_le(val2,val1)<>0) then
    softresult_ok := false;
  if not softresult_ok then
    Fail
  else
    WriteLn('Success.');
 end;


 procedure float32testlt;
 var
  i : single;
  j : single;
  val1,val2 : float32rec;
 begin
  softresult_ok := true;
  Write('single < single test...');
  i := 1000.0;
  j := 1000.0;
  val1 := singletofloat32(i);
  val2 := singletofloat32(j);
  if (float32_lt(val1,val2)<>0) then
    softresult_ok := false;
  i := 999.0;
  j := 1000.0;
  val1 := singletofloat32(i);
  val2 := singletofloat32(j);
  if (float32_lt(val1,val2)=0) then
    softresult_ok := false;
  i := -10000.0;
  j := -999.0;
  val1 := singletofloat32(i);
  val2 := singletofloat32(j);
  if (float32_lt(val2,val1)<>0) then
    softresult_ok := false;
  if not softresult_ok then
    Fail
  else
    WriteLn('Success.');
 end;

procedure Float32TestInt;
 var
  _result : longint;
 begin
  softresult_ok := true;
  Write('Single to Longint test...');
  _result:=float32_to_int32(singletofloat32(-12.12345));
  if _result <> -12 then
    softresult_ok := false;
  _result:=float32_to_int32(singletofloat32(12.52345));
  if _result <> 13 then
    softresult_ok := false;
  _result:=float32_to_int32(singletofloat32(-0.01));
  if _result <> 0 then
    softresult_ok := false;
  if not softresult_ok then
    Fail
  else
    WriteLn('Success.');
 end;

{Procedure int32_to_float32( a: int32; var c: float32rec ); }
procedure IntTestFloat32;
 var
  val1 : float32rec;
 begin
  softresult_ok := true;
  Write('Longint to single test...');
  val1:=int32_to_float32($8000);
  if float32tosingle(val1) <> $8000 then
    softresult_ok := false;
  val1:=int32_to_float32(-1);
  if float32tosingle(val1) <> -1 then
    softresult_ok := false;
  val1:=int32_to_float32(0);
  if (float32tosingle(val1)) <> 0.0 then
    softresult_ok := false;
  val1:=int32_to_float32(-217000000);
  if float32tosingle(val1) <> -217000000 then
    softresult_ok := false;
  if not softresult_ok then
    Fail
  else
    WriteLn('Success.');
 end;

{******************************************************************************}
{*                            double arithmetic                               *}
{******************************************************************************}
 Procedure float64TestSub;
 var
  i : double;
  j : double;
  val1,val2 : float64;
 Begin
  Write('Double - Double test...');
  softresult_ok := true;
  i:=99.9;
  j:=10.0;
  val1:=doubletofloat64(i);
  val2:=doubletofloat64(j);
  { i:=i-j }
  val1:=float64_sub(val1,val2);
  i:=float64todouble(val1);
  j:=float64todouble(val2);
  if trunc(i) <> trunc(89.9) then
    softresult_ok := false;
  WriteLn('Result (89.9) :',i);
  val1:=doubletofloat64(i);
  val2:=doubletofloat64(j);
  { i:=j-i }
  val1:=float64_sub(val2,val1);
  i:=float64todouble(val1);
  j:=float64todouble(val2);
  if trunc(i) <> trunc(-79.9) then
    softresult_ok := false;
  WriteLn('Result (-79.9) :',i);
  val1:=doubletofloat64(j);
  val2:=doubletofloat64(10.0);
  { j:=j-10.0 }
  val1:=float64_sub(val1,val2);
  j:=float64todouble(val1);
  if j <> 0.0 then
    softresult_ok := false;
  WriteLn('Result (0.0) :',j);
  if not softresult_ok then
    Fail
  else
    WriteLn('Success.');
 end;

 procedure float64TestAdd;
 var
  i : double;
  j : double;
  val1, val2 : float64;
 Begin
   WriteLn('Double + Double test...');
   softresult_ok := true;
   i:= 9;
 {  i:=i+1.5;}
   val1:=float64_add(doubletofloat64(i),doubletofloat64(1.5));
   i:=float64todouble(val1);
   if trunc(i) <> trunc(10.5) then
     softresult_ok := false;
   WriteLn('Result (10.5) :',i);
   i := 326788.12345;
   j := 100.0;
{   i := i + j + 12.5;}
   val1 := doubletofloat64(i);
   val2 := doubletofloat64(j);
   val1:=float64_add(val1,val2);   { i:=i+j }
   val1:=float64_add(val1,doubletofloat64(12.5));
   i:=float64todouble(val1);
   if trunc(i) <> trunc(326900.12345) then
     softresult_ok := false;
   WriteLn('Result (326900.12345) :',i);
   if not softresult_ok then
    Fail
   else
    WriteLn('Success.');
 end;


 procedure float64testmul;
 var
  i : double;
  j : double;
  val1 : float64;
 begin
  WriteLn('Double * Double test...');
  softresult_ok := true;
  i:= 21111.0;
  j:= 11.1;
{  i := i * j * i; }
  val1:=float64_mul(doubletofloat64(i),doubletofloat64(j));
  i:=float64todouble(val1);
  if trunc(i) <> trunc(234332.1) then
    softresult_ok := false;
  WriteLn('Result (234332.1) :',i);
  i := 10.0;
  j := -12.0;
{  i := i * j * 10.0;}
  val1:=float64_mul(doubletofloat64(i),doubletofloat64(j));
  val1:=float64_mul(val1,doubletofloat64(10.0));
  i:=float64todouble(val1);
  if trunc(i) <> trunc(-1200.0) then
    softresult_ok := false;
  WriteLn('Result (-1200.0) :',i);
  if not softresult_ok then
    Fail
  else
    WriteLn('Success.');
 end;



 Procedure float64TestDiv;
 var
  i : double;
  j : double;
  val1 : float64;
 Begin
  softresult_ok := true;
  WriteLn('Double / Double test...');
  i:=-99.9;
  j:=10.0;
{  i:=i / j; }
  val1:=float64_div(doubletofloat64(i),doubletofloat64(j));
  i:=float64todouble(val1);
  if trunc(i) <> trunc(-9.9) then
    softresult_ok := false;
  WriteLn('Result (-9.9) :',i);
  {i:=j / i;}
  val1:=float64_div(doubletofloat64(j),doubletofloat64(i));
  i:=float64todouble(val1);
  if trunc(i) <> trunc(-1.01) then
    softresult_ok := false;
  WriteLN('Result (-1.01) :',i);
{  j:=i / 10.0; }
  val1:=float64_div(doubletofloat64(i),doubletofloat64(10.0));
  j:=float64todouble(val1);
  if trunc(j) <> trunc(-0.1001) then
    softresult_ok := false;
  WriteLn('Result (-0.1001) :',j);
  if not softresult_ok then
    Fail
  else
    WriteLn('Success.');
 end;

 procedure float64testequal;
 var
  i : double;
  j : double;
  val1,val2 : float64;
 begin
  softresult_ok := true;
  Write('Double = Double test...');
  i := 1000.0;
  j := 1000.0;
  val1 := doubletofloat64(i);
  val2 := doubletofloat64(j);
  if (float64_eq(val1,val2)=0) then
    softresult_ok := false;
  i := -112345.1;
  j := -112345.1;
  val1 := doubletofloat64(i);
  val2 := doubletofloat64(j);
  if (float64_eq(val1,val2)=0) then
    softresult_ok := false;
  i := 4502020.1125E+03;
  j := 4502020.1125E+03;
  val1 := doubletofloat64(i);
  val2 := doubletofloat64(j);
  if (float64_eq(val1,val2)=0) then
    softresult_ok := false;
  i := -4502028.1125E+03;
  j := -4502028.1125E+03;
  val1 := doubletofloat64(i);
  val2 := doubletofloat64(j);
  if (float64_eq(val1,val2)=0) then
    softresult_ok := false;
  i := -4502030.1125E+03;
  j := -4502028.1125E+03;
  val1 := doubletofloat64(i);
  val2 := doubletofloat64(j);
  if (float64_eq(val1,val2)<>0) then
    softresult_ok := false;
  if not softresult_ok then
    Fail
  else
    WriteLn('Success.');
 end;

 procedure float64testle;
 var
  i : double;
  j : double;
  val1,val2: float64;
 begin
  softresult_ok := true;
  Write('Double <= Double test...');
  i := 1000.0;
  j := 1000.0;
  val1 := doubletofloat64(i);
  val2 := doubletofloat64(j);
  if (float64_le(val1,val2)=0) then
    softresult_ok := false;
  i := 10000.0;
  j := 999.0;
  val1 := doubletofloat64(i);
  val2 := doubletofloat64(j);
  if (float64_le(val2,val1)=0) then
    softresult_ok := false;
  i := -10000.0;
  j := -999.0;
  val1 := doubletofloat64(i);
  val2 := doubletofloat64(j);
  if (float64_le(val2,val1)<>0) then
    softresult_ok := false;
  if not softresult_ok then
    Fail
  else
    WriteLn('Success.');
 end;


 procedure float64testlt;
 var
  i : double;
  j : double;
  val1,val2 : float64;
 begin
  softresult_ok := true;
  Write('Double < Double test...');
  i := 1000.0;
  j := 1000.0;
  val1 := doubletofloat64(i);
  val2 := doubletofloat64(j);
  if (float64_lt(val1,val2)<>0) then
    softresult_ok := false;
  i := 999.0;
  j := 1000.0;
  val1 := doubletofloat64(i);
  val2 := doubletofloat64(j);
  if (float64_lt(val1,val2)=0) then
    softresult_ok := false;
  i := -10000.0;
  j := -999.0;
  val1 := doubletofloat64(i);
  val2 := doubletofloat64(j);
  if (float64_lt(val2,val1)<>0) then
    softresult_ok := false;
  if not softresult_ok then
    Fail
  else
    WriteLn('Success.');
 end;

procedure Float64TestInt;
 var
  _result : longint;
 begin
  softresult_ok := true;
  Write('double to Longint test...');
  _result:=float64_to_int32(doubletofloat64(-12.12345));
  if _result <> -12 then
    softresult_ok := false;
  _result:=float64_to_int32(doubletofloat64(12.52345));
  if _result <> 13 then
    softresult_ok := false;
  _result:=float64_to_int32(doubletofloat64(-0.01));
  if _result <> 0 then
    softresult_ok := false;
  if not softresult_ok then
    Fail
  else
    WriteLn('Success.');
 end;

{Procedure int32_to_float64( a: int32; var c: float64 ); }
procedure IntTestFloat64;
 var
  val1 : float64;
 begin
  softresult_ok := true;
  Write('Longint to double test...');
  val1:=int32_to_float64($8000);
  if float64todouble(val1) <> $8000 then
    softresult_ok := false;
  val1:=int32_to_float64(-1);
  if float64todouble(val1) <> -1 then
    softresult_ok := false;
  val1:=int32_to_float64(0);
  if (float64todouble(val1)) <> 0.0 then
    softresult_ok := false;
  val1:=int32_to_float64(-217000000);
  if float64todouble(val1) <> -217000000 then
    softresult_ok := false;
  if not softresult_ok then
    Fail
  else
    WriteLn('Success.');
 end;

{ test procedure int64_to_float32 }
procedure Int64TestFloat32;
 var
  val1 : float32rec;
  a : int64;
  sgl : single;
 begin
  softresult_ok := true;
  Write('int64 to single test...');
  { cases to test : a = 0; a < 0; a > 0 }
  a:=0;
  { reset floating point exceptions flag }
  softfloat_exception_flags := 0;
  sgl:=float32tosingle(int64_to_float32(a));
  if trunc(sgl) <> 0 then
    softresult_ok := false;
  a:=-32768;
  softfloat_exception_flags := 0;
  sgl:=float32tosingle(int64_to_float32(a));
  if trunc(sgl) <> -32768 then
    softresult_ok := false;
  a:=-1000001;
  softfloat_exception_flags := 0;
  sgl:=float32tosingle(int64_to_float32(a));
  if trunc(sgl) <> -1000001 then
    softresult_ok := false;
  a:=12567;
  softfloat_exception_flags := 0;
  sgl:=float32tosingle(int64_to_float32(a));
  if trunc(sgl) <> 12567 then
    softresult_ok := false;
  a:=high(longint);
  softfloat_exception_flags := 0;
  sgl:=float32tosingle(int64_to_float32(a));
  { the result might be inexact, so can't really test }
  if (trunc(sgl) <> high(longint)) and
     ((softfloat_exception_flags and float_flag_inexact)=0) then
    softresult_ok := false;
  a:=low(longint);
  softfloat_exception_flags := 0;
  sgl:=float32tosingle(int64_to_float32(a));
  if (trunc(sgl) <> low(longint)) and
     ((softfloat_exception_flags and float_flag_inexact)=0) then
    softresult_ok := false;
{$ifndef ver1_0}
  { version 1.0 returns a longint for trunc   }
  { so these routines will automatically fail }
  a:=1 shl 33;
  softfloat_exception_flags := 0;
  sgl:=float32tosingle(int64_to_float32(a));
  if (int64(trunc(sgl)) <> int64(1) shl 33) and
     ((softfloat_exception_flags and float_flag_inexact)=0) then
    softresult_ok := false;
  a:=1 shl 33;
  a:=-a;
  softfloat_exception_flags := 0;
  sgl:=float32tosingle(int64_to_float32(a));
  if (int64(trunc(sgl)) <> -(int64(1) shl 33)) and
    ((softfloat_exception_flags and float_flag_inexact)=0)   then
    softresult_ok := false;
{$endif}
  if not softresult_ok then
    Fail
  else
    WriteLn('Success.');
end;

{ test procedure int64_to_float32 }
procedure Int64TestFloat64;
 var
  val1 : float32rec;
  a : int64;
  float : double;
 begin
  softresult_ok := true;
  Write('int64 to double test...');
  { cases to test : a = 0; a < 0; a > 0 }
  a:=0;
  { reset floating point exceptions flag }
  softfloat_exception_flags := 0;
  float:=float64todouble(int64_to_float64(a));
  if trunc(float) <> 0 then
    softresult_ok := false;
  a:=-32768;
  softfloat_exception_flags := 0;
  float:=float64todouble(int64_to_float64(a));
  if trunc(float) <> -32768 then
    softresult_ok := false;
  a:=-1000001;
  softfloat_exception_flags := 0;
  float:=float64todouble(int64_to_float64(a));
  if trunc(float) <> -1000001 then
    softresult_ok := false;
  a:=12567;
  softfloat_exception_flags := 0;
  float:=float64todouble(int64_to_float64(a));
  if trunc(float) <> 12567 then
    softresult_ok := false;
  a:=high(longint);
  softfloat_exception_flags := 0;
  float:=float64todouble(int64_to_float64(a));
  { the result might be inexact, so can't really test }
  if (trunc(float) <> high(longint)) and
     ((softfloat_exception_flags and float_flag_inexact)=0) then
    softresult_ok := false;
  a:=low(longint);
  softfloat_exception_flags := 0;
  float:=float64todouble(int64_to_float64(a));
  if (trunc(float) <> low(longint)) and
     ((softfloat_exception_flags and float_flag_inexact)=0) then
    softresult_ok := false;
{$ifndef ver1_0}
  { version 1.0 returns a longint for trunc   }
  { so these routines will automatically fail }
  a:=1 shl 33;
  softfloat_exception_flags := 0;
  float:=float64todouble(int64_to_float64(a));
  if (int64(trunc(float)) <> int64(1) shl 33) and
     ((softfloat_exception_flags and float_flag_inexact)=0) then
    softresult_ok := false;
  a:=1 shl 33;
  a:=-a;
  softfloat_exception_flags := 0;
  float:=float64todouble(int64_to_float64(a));
  if (int64(trunc(float)) <> -(int64(1) shl 33)) and
    ((softfloat_exception_flags and float_flag_inexact)=0)   then
    softresult_ok := false;
{$endif}
  if not softresult_ok then
    Fail
  else
    WriteLn('Success.');
end;


Begin
 Float32TestEqual;
 Float32TestLE;
 Float32TestLT;
 Float32TestSub;
 Float32TestAdd;
 Float32TestDiv;
 Float32TestMul;
 Float32TestInt;
 IntTestFloat32;

 float64TestEqual;
 float64TestLE;
 float64TestLT;
 float64TestSub;
 float64TestAdd;
 float64TestDiv;
 float64TestMul;
 float64TestInt;
 IntTestfloat64;
 { int64 conversion routines }
 int64testfloat32;
 int64testfloat64;
 if (softresult_error_count>0) then
   begin
     WriteLn(softresult_error_count,' failures!');
     halt(1);
   end;
end.
