{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{  By Carl Eric Codere                                           }
{****************************************************************}
{ NODE TESTED : secondcalln()                                    }
{****************************************************************}
{ PRE-REQUISITES: secondload()                                   }
{                 secondassign()                                 }
{                 secondtypeconv()                               }
{                 secondtryexcept()                              }
{****************************************************************}
{ DEFINES:                                                       }
{            FPC     = Target is FreePascal compiler             }
{****************************************************************}
{ REMARKS: This tests secondcalln(), genentrycode() and          }
{ genexitcode().                                                 }
{                                                                }
{                                                                }
{****************************************************************}
program tcall;

{$ifdef fpc}{$mode objfpc}{$endif}
uses SysUtils;

{
class:
  class constructor
   1a  - success
   1b  - failure
  2 class destructor
  3 class method
  4 virtual method
  5 abstract method
  6 static method
object:
  object constructor
  7a  - success
  7b  - failure
  8 object destructor
  9 method
  10 virtual method
standard:
  11 function
  12 procedure
  13 procedure variable

modifiers:
  no parameters                    1a  1b
  parameters
     - const                       1a
     - value                       1a
     - variable                    1a
     - mixed                       1a

  explicit self parameter
  operator directive
  assembler directive
  interrupt directive
  inline directive
  cdecl directive
  pascal directive
  safecall directive
  stdcall directive
  oldfpcall directive
  register directive
}

const
  GLOBAL_RESULT = $55;

var
 globalresult : integer;
 failed : boolean;

type

  tclass1 = class
    constructor create_none;               { class constructor }
    constructor create_value(l:longint;b: byte);
    constructor create_var(var l:longint;var b: byte);
    constructor create_const(const l:longint; const b: byte);
    constructor create_mixed(var a: byte; b: byte; var c: byte; const d: byte);
  end;

  tclass2 = class
    constructor create_none;               { class constructor }
  public
    b: array[1..{$ifdef cpu16}32763{$else}$66666666{$endif}] of byte;
  end;


  constructor tclass1.create_none;
   begin
     Inherited create;
     globalresult:=GLOBAL_RESULT;
   end;


  constructor tclass1.create_value(l:longint;b: byte);
   begin
     Inherited create;
     globalresult:=b;
   end;

  constructor tclass1.create_var(var l:longint;var b: byte);
    begin
     Inherited create;
      b:=GLOBAL_RESULT;
    end;

  constructor tclass1.create_const(const l:longint; const b: byte);
    begin
     Inherited create;
      globalresult := GLOBAL_RESULT;
    end;

  constructor tclass1.create_mixed(var a: byte; b: byte; var c: byte; const d: byte);
    begin
     Inherited create;
      globalresult := GLOBAL_RESULT;
    end;



  constructor tclass2.create_none;
   begin
     Inherited create;
     { the next line will normally not be reached, else
       it's a failure }
     globalresult:=0;
   end;



procedure fail;
begin
  WriteLn('Failure.');
  halt(1);
end;


function myheaperrornil(size : longint): integer;
  begin
    myheaperrornil:=1;
  end;

function myheaperrorexception(size : longint): integer;
  begin
    myheaperrorexception:=0;
  end;

var
 class_none: tclass1;
 class_value: tclass1;
 class_var: tclass1;
 class_const: tclass1;
 class_mixed: tclass1;
 class_none_fail : tclass2;
 a,b,c,d: byte;
 l:longint;
Begin
  { reset test variables }
  globalresult := 0;
  failed := false;

  write('class constructor testing...');
  { secondcalln : class constructor success }
  class_none:=tclass1.create_none;
  if globalresult <> GLOBAL_RESULT then
    failed:= true;

  globalresult := 0;
  class_value:=tclass1.create_value(0,GLOBAL_RESULT);
  if globalresult <> GLOBAL_RESULT then
    failed:= true;

  globalresult := 0;
  b:=0;
  class_var:=tclass1.create_var(l,b);
  globalresult:=b;
  if globalresult <> GLOBAL_RESULT then
    failed:= true;


  globalresult := 0;
  b:=GLOBAL_RESULT;
  class_const:=tclass1.create_const(l,b);
  if globalresult <> GLOBAL_RESULT then
    failed:= true;

  globalresult := 0;
  b:=0;
  a:=0;
  c:=0;
  d:=GLOBAL_RESULT;
  class_mixed:=tclass1.create_mixed(a,b,c,d);
  if globalresult <> GLOBAL_RESULT then
    failed:= true;

{$ifdef dummy}
  globalresult := GLOBAL_RESULT;
  { secondcalln : class constructor failure, when getmem returns 0,
    that will call class_help_fail and abort class construction }
  heaperror := @myheaperrornil;
  try
    class_none_fail:=tclass2.create_none;
   except
   on EOutOfMemory do globalresult:=0;
   end;
  if globalresult <> GLOBAL_RESULT then
    failed:= true;

  { secondcalln : class constructor failure, getmem gives a runtime error
    that will be translated to a exception and the exception shall be catched
    here }
  globalresult := 0;
  heaperror := @myheaperrorexception;
  try
    class_none_fail:=tclass2.create_none;
   except
   on EOutOfMemory do globalresult:=GLOBAL_RESULT;
   end;
  if globalresult <> GLOBAL_RESULT then
    failed:= true;
{$endif dummy}

  if failed then
    fail
  else
    WriteLn('Passed!');

end.
