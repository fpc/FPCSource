{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{  By Carl Eric Codere                                           }
{****************************************************************}
{ NODE TESTED : secondcallparan()                                }
{****************************************************************}
{ PRE-REQUISITES: secondload()                                   }
{                 secondassign()                                 }
{                 secondtypeconv()                               }
{                 secondtryexcept()                              }
{                 secondcalln()                                  }
{                 secondadd()                                    }
{****************************************************************}
{ DEFINES:                                                       }
{            FPC     = Target is FreePascal compiler             }
{****************************************************************}
{ REMARKS: This tests a subset of the secondcalln() node         }
{                                                                }
{****************************************************************}
program tparan;

 {$mode objfpc}
 { REAL should map to single or double }
 { so it is not checked, since single  }
 { double nodes are checked.           }

 { assumes that enumdef is the same as orddef (same storage format) }

 { test of value parameters }
 const
  RESULT_U8BIT = $55;
  RESULT_U16BIT = $500F;
  RESULT_S32BIT = $500F0000;
  RESULT_S64BIT = $500F0000;
  RESULT_S32REAL = 1777.12;
  RESULT_S64REAL = 3444.24;
  RESULT_BOOL8BIT = 1;
  RESULT_BOOL16BIT = 1;
  RESULT_BOOL32BIT = 1;
  RESULT_PCHAR = 'Hello world';
  
type 
  tclass1 = class
  end;
  
  tprocedure = procedure;
  
  tsmallrecord = packed record
    b: byte;
    w: word;
  end;
  
  tlargerecord = packed record
    b: array[1..33000] of byte;
  end;
  
  tsmallsetenum = 
  (A_A,A_B,A_C,A_D);
  
  tsmallset = set of tsmallsetenum;
  
  
  
  
  
var
 global_u8bit : byte;
 global_u16bit : word;
 global_s32bit : longint;
 global_s64bit : int64;
 global_s32real : single;
 global_s64real : double;
 global_ptr : pchar;
 global_proc : tprocedure;
 global_class : tclass1;
 value_u8bit : byte;
 value_u16bit : word;
 value_s32bit : longint;
 value_s64bit : int64;
 value_s32real : single;
 value_s64real  : double;
 value_proc : tprocedure;
 value_ptr : pchar;
 value_class : tclass1;
 value_smallrec : tsmallrecord;
 value_largerec : tlargerecord;
 value_smallset : tsmallset;
 
    procedure fail;
    begin
      WriteLn('Failure.');
{      halt(1);}
    end;
   
  procedure testprocedure;
   begin
   end;
 
   function getu8bit : byte;
    begin
      getu8bit:=RESULT_U8BIT;
    end;
    
   function getu16bit: word;
     begin
       getu16bit:=RESULT_U16BIT;
     end;
     
   function gets32bit: longint;
    begin
      gets32bit:=RESULT_S32BIT;
    end;
    
   function gets64bit: longint;
    begin
      gets64bit:=RESULT_S32BIT;
    end;
 

   function gets32real: single;
    begin
      gets32real:=RESULT_S32REAL;
    end;

   function gets64real: double;
    begin
      gets64real:=RESULT_S64REAL;
    end;

{ ***************************************************************** }
{                        VALUE PARAMETERS                           }
{ ***************************************************************** }

  procedure proc_value_u8bit(v: byte);
   begin
     global_u8bit := v;
   end;


  procedure proc_value_u16bit(v: word);
   begin
     global_u16bit := v;
   end;


  procedure proc_value_s32bit(v : longint);
   begin
     global_s32bit := v;
   end;
   
  procedure proc_value_s64bit(v: int64);
   begin
     global_s64bit:= v;
   end;
   
   
   
  procedure proc_value_bool8bit(v: boolean);
   begin
     { boolean should be 8-bit always! }
     if sizeof(boolean) <> 1 then RunError(255);
     global_u8bit := byte(v);
   end;


  procedure proc_value_bool16bit(v: wordbool);
   begin
     global_u16bit := word(v);
   end;


  procedure proc_value_bool32bit(v : longbool);
   begin
     global_s32bit := longint(v);
   end;


  procedure proc_value_s32real(v : single);
   begin
     global_s32real := v;
   end;
   
  procedure proc_value_s64real(v: double);
   begin
     global_s64real:= v;
   end;
   
   
  procedure proc_value_pointerdef(p : pchar);
   begin
     global_ptr:=p;
   end;


  procedure proc_value_procvardef(p : tprocedure);
   begin
     global_proc:=p;
   end;
   
   
  procedure proc_value_classrefdef(obj : tclass1);
   begin
     global_class:=obj;
   end;
   
   
  procedure proc_value_smallrecord(smallrec : tsmallrecord);
   begin
     if (smallrec.b = RESULT_U8BIT) and (smallrec.w = RESULT_U16BIT) then
       global_u8bit := RESULT_U8BIT;
   end;
   

  procedure proc_value_largerecord(largerec : tlargerecord);
   begin
     if (largerec.b[1] = RESULT_U8BIT) and (largerec.b[2] = RESULT_U8BIT) then
       global_u8bit := RESULT_U8BIT;
   end;
   
  procedure proc_value_smallset(smallset : tsmallset);
   begin
     if A_D in smallset then
       global_u8bit := RESULT_U8BIT;
   end;
    

{$IFDEF FPC}
{$INLINE ON}

  procedure proc_value_u8bit_inline(v: byte);inline;
   begin
     global_u8bit := v;
   end;


  procedure proc_value_u16bit_inline(v: word);inline;
   begin
     global_u16bit := v;
   end;


  procedure proc_value_s32bit_inline(v : longint);inline;
   begin
     global_s32bit := v;
   end;
   
  procedure proc_value_s64bit_inline(v: int64);inline;
   begin
     global_s64bit:= v;
   end;

  procedure proc_value_s32real_inline(v : single);inline;
   begin
     global_s32real := v;
   end;
   
  procedure proc_value_s64real_inline(v: double);inline;
   begin
     global_s64real:= v;
   end;

  procedure proc_value_pointerdef_inline(p : pchar);inline;
   begin
     global_ptr:=p;
   end;


  procedure proc_value_procvardef_inline(p : tprocedure);inline;
   begin
     global_proc:=p;
   end;
   
   
  procedure proc_value_classrefdef_inline(obj : tclass1);inline;
   begin
     global_class:=obj;
   end;

  procedure proc_value_bool8bit_inline(v: boolean);inline;
   begin
     { boolean should be 8-bit always! }
     if sizeof(boolean) <> 1 then RunError(255);
     global_u8bit := byte(v);
   end;

  procedure proc_value_smallrecord_inline(smallrec : tsmallrecord);inline;
   begin
     if (smallrec.b = RESULT_U8BIT) and (smallrec.w = RESULT_U16BIT) then
       global_u8bit := RESULT_U8BIT;
   end;


  procedure proc_value_largerecord_inline(largerec : tlargerecord);inline;
   begin
     if (largerec.b[1] = RESULT_U8BIT) and (largerec.b[2] = RESULT_U8BIT) then
       global_u8bit := RESULT_U8BIT;
   end;

  procedure proc_value_smallset_inline(smallset : tsmallset);inline;
   begin
     if A_D in smallset then
       global_u8bit := RESULT_U8BIT;
   end;

{$ENDIF}
  

var
 failed: boolean;
Begin
  global_u8bit := 0;
  global_u16bit := 0;
  global_s32bit := 0;
  global_s64bit := 0;
  global_s32real := 0.0;
  global_s64real := 0.0;
  failed:=false;
  
  { LOC_REGISTER }
  write('Value parameter test (src : LOC_REGISTER)...');
  proc_value_u8bit(getu8bit);
  if global_u8bit <> RESULT_U8BIT then
    failed:=true;
  proc_value_u16bit(getu16bit);
  if global_u16bit <> RESULT_U16BIT then
    failed:=true;
  proc_value_s32bit(gets32bit);
  if global_s32bit <> RESULT_S32BIT then
    failed:=true;
  proc_value_s64bit(gets64bit);
  if global_s64bit <> RESULT_S64BIT then
    failed:=true;
  if failed then
    fail
  else
    WriteLn('Passed!');
    
    
  { LOC_FPUREGISTER }  
  global_u8bit := 0;
  global_u16bit := 0;
  global_s32bit := 0;
  global_s64bit := 0;
  global_s32real := 0.0;
  global_s64real := 0.0;
  failed:=false;
  write('Value parameter test (src : LOC_FPUREGISTER)...');
  proc_value_s32real_inline(gets32real);
  if trunc(global_s32real) <> trunc(RESULT_S32REAL) then
    failed:=true;
  proc_value_s64real_inline(gets64real);
  if trunc(global_s64real) <> trunc(RESULT_S64REAL) then
    failed:=true;
  if failed then
    fail
  else
    WriteLn('Passed!');
    

{$ifdef fpc}    
  write('(Inline) Value parameter test (src : LOC_REGISTER)...');
  global_u8bit := 0;
  global_u16bit := 0;
  global_s32bit := 0;
  global_s64bit := 0;
  global_s32real := 0.0;
  global_s64real := 0.0;
  failed:=false;
  proc_value_u8bit_inline(getu8bit);
  if global_u8bit <> RESULT_U8BIT then
    failed:=true;
  proc_value_u16bit_inline(getu16bit);
  if global_u16bit <> RESULT_U16BIT then
    failed:=true;
  proc_value_s32bit_inline(gets32bit);
  if global_s32bit <> RESULT_S32BIT then
    failed:=true;
  proc_value_s64bit_inline(gets64bit);
  if global_s64bit <> RESULT_S64BIT then
    failed:=true;
  if failed then
    fail
  else
    WriteLn('Passed!');


  global_u8bit := 0;
  global_u16bit := 0;
  global_s32bit := 0;
  global_s64bit := 0;
  global_s32real := 0.0;
  global_s64real := 0.0;
  failed:=false;
  write('(Inline) Value parameter test (src : LOC_FPUREGISTER)...');
  proc_value_s32real_inline(gets32real);
  if trunc(global_s32real) <> trunc(RESULT_S32REAL) then
    failed:=true;
  proc_value_s64real_inline(gets64real);
  if trunc(global_s64real) <> trunc(RESULT_S64REAL) then
    failed:=true;
    
  if failed then
    fail
  else
    WriteLn('Passed!');
{$endif}    

  { LOC_MEM, LOC_REFERENCE orddef } 
  value_u8bit := RESULT_U8BIT;
  value_u16bit := RESULT_U16BIT;
  value_s32bit := RESULT_S32BIT;
  value_s64bit := RESULT_S64BIT;
  value_s32real := RESULT_S32REAL;
  value_s64real  := RESULT_S64REAL;

  global_u8bit := 0;
  global_u16bit := 0;
  global_s32bit := 0;
  global_s64bit := 0;
  global_s32real := 0.0;
  global_s64real := 0.0;
  failed:=false;
  
  { LOC_REFERENCE }
  write('Value parameter test (src : LOC_REFERENCE (orddef/enumdef)))...');
  proc_value_u8bit(value_u8bit);
  if global_u8bit <> RESULT_U8BIT then
    failed:=true;
  proc_value_u16bit(value_u16bit);
  if global_u16bit <> RESULT_U16BIT then
    failed:=true;
  proc_value_s32bit(value_s32bit);
  if global_s32bit <> RESULT_S32BIT then
    failed:=true;
  proc_value_s64bit(value_s64bit);
  if global_s64bit <> RESULT_S64BIT then
    failed:=true;
  if failed then
    fail
  else
    WriteLn('Passed!');
    
    
  { LOC_REFERENCE }  
  global_u8bit := 0;
  global_u16bit := 0;
  global_s32bit := 0;
  global_s64bit := 0;
  global_s32real := 0.0;
  global_s64real := 0.0;
  failed:=false;
  write('Value parameter test (src : LOC_REFERENCE (floatdef))...');
  proc_value_s32real_inline(value_s32real);
  if trunc(global_s32real) <> trunc(RESULT_S32REAL) then
    failed:=true;
  proc_value_s64real_inline(value_s64real);
  if trunc(global_s64real) <> trunc(RESULT_S64REAL) then
    failed:=true;
  if failed then
    fail
  else
    WriteLn('Passed!');
    

{$ifdef fpc}    
  write('(Inline) Value parameter test (src : LOC_REFERENCE (orddef/enumdef))...');
  global_u8bit := 0;
  global_u16bit := 0;
  global_s32bit := 0;
  global_s64bit := 0;
  global_s32real := 0.0;
  global_s64real := 0.0;
  failed:=false;
  proc_value_u8bit_inline(value_u8bit);
  if global_u8bit <> RESULT_U8BIT then
    failed:=true;
  proc_value_u16bit_inline(value_u16bit);
  if global_u16bit <> RESULT_U16BIT then
    failed:=true;
  proc_value_s32bit_inline(value_s32bit);
  if global_s32bit <> RESULT_S32BIT then
    failed:=true;
  proc_value_s64bit_inline(value_s64bit);
  if global_s64bit <> RESULT_S64BIT then
    failed:=true;
  if failed then
    fail
  else
    WriteLn('Passed!');


  global_u8bit := 0;
  global_u16bit := 0;
  global_s32bit := 0;
  global_s64bit := 0;
  global_s32real := 0.0;
  global_s64real := 0.0;
  failed:=false;
  write('(Inline) Value parameter test (src : LOC_REFERENCE (floatdef))...');
  proc_value_s32real_inline(value_s32real);
  if trunc(global_s32real) <> trunc(RESULT_S32REAL) then
    failed:=true;
  proc_value_s64real_inline(value_s64real);
  if trunc(global_s64real) <> trunc(RESULT_S64REAL) then
    failed:=true;
    
  if failed then
    fail
  else
    WriteLn('Passed!');
{$endif}    



  write('Value parameter test (src : LOC_REFERENCE (pointer))...');
  value_ptr:=nil;
  value_class:=nil;
  value_proc := nil;
  global_ptr := nil;
  global_proc := nil;
  global_class := nil;
  failed:=false;
  value_ptr := RESULT_PCHAR;
  proc_value_pointerdef(value_ptr);
  if global_ptr <> value_ptr then
    failed := true;


  value_proc := @testprocedure;
  proc_value_procvardef(value_proc);
  if value_proc <> global_proc then
    failed := true;

  value_class := tclass1.create;
  proc_value_classrefdef(value_class);
  if value_class <> global_class then
    failed := true;
  value_class.destroy;
  if failed then
    fail
  else
    WriteLn('Passed!');


  write('(Inline) Value parameter test (src : LOC_REFERENCE (pointer))...');
  value_ptr:=nil;
  value_class:=nil;
  value_proc := nil;
  global_ptr := nil;
  global_proc := nil;
  global_class := nil;
  value_ptr := RESULT_PCHAR;
  failed:=false;
  proc_value_pointerdef_inline(value_ptr);
  if global_ptr <> value_ptr then
    failed := true;


  value_proc := @testprocedure;
  proc_value_procvardef_inline(value_proc);
  if value_proc <> global_proc then
    failed := true;

  value_class := tclass1.create;
  proc_value_classrefdef_inline(value_class);
  if value_class <> global_class then
    failed := true;
  value_class.destroy;
  if failed then
    fail
  else
    WriteLn('Passed!');



  { LOC_REFERENCE }
  global_u8bit := 0;
  global_u16bit := 0;
  global_s32bit := 0;
  global_s64bit := 0;
  global_s32real := 0.0;
  global_s64real := 0.0;
  failed:=false;
  value_u8bit := 0;
  write('Value parameter test (src : LOC_FLAGS (orddef)))...');
  proc_value_bool8bit(value_u8bit = 0);
  if global_u8bit <> RESULT_BOOL8BIT then
    failed:=true;
(* IMPOSSIBLE TO GENERATE LOC_FLAGS WITH SIZE <> S_B ON VERSION 1.0.x 
  proc_value_bool16bit(value_s64bit < 0);
  if global_u16bit <> RESULT_BOOL16BIT then
    failed:=true;
  proc_value_bool32bit(bool1 and bool2);
  if global_s32bit <> RESULT_BOOL32BIT then
    failed:=true;*)
  if failed then
    fail
  else
    WriteLn('Passed!');


  write('(Inline) Value parameter test (src : LOC_FLAGS (orddef))...');
  global_u8bit := 0;
  global_u16bit := 0;
  global_s32bit := 0;
  global_s64bit := 0;
  global_s32real := 0.0;
  global_s64real := 0.0;
  failed:=false;
  value_u8bit := 0;
  failed:=false;
  proc_value_bool8bit_inline(value_u8bit = 0);
  if global_u8bit <> RESULT_BOOL8BIT then
    failed:=true;
  if failed then
    fail
  else
    WriteLn('Passed!');
    
    
    
  global_u8bit := 0;
  global_u16bit := 0;
  global_s32bit := 0;
  global_s64bit := 0;
  global_s32real := 0.0;
  global_s64real := 0.0;
  failed:=false;
  value_u8bit := 0;
  write('Value parameter test (src : LOC_JUMP (orddef)))...');
  proc_value_bool8bit(value_s64bit = 0);
  if global_u8bit <> RESULT_BOOL8BIT then
    failed:=true;
(* IMPOSSIBLE TO GENERATE LOC_JUMP WITH SIZE <> S_B ON VERSION 1.0.x 
  proc_value_bool16bit(value_s64bit < 0);
  if global_u16bit <> RESULT_BOOL16BIT then
    failed:=true;
  proc_value_bool32bit(bool1 and bool2);
  if global_s32bit <> RESULT_BOOL32BIT then
    failed:=true;*)
  if failed then
    fail
  else
    WriteLn('Passed!');
     
  { arraydef,
    recorddef,
    objectdef,
    stringdef,
    setdef : all considered the same by code generator.
  }
  write('Value parameter test (src : LOC_REFERENCE (recorddef)))...');
  global_u8bit := 0;
  failed := false;
  fillchar(value_smallrec,sizeof(value_smallrec),#0);

  value_smallrec.b := RESULT_U8BIT;
  value_smallrec.w := RESULT_U16BIT;
  proc_value_smallrecord(value_smallrec);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;
    
  global_u8bit := 0;
  fillchar(value_largerec,sizeof(value_largerec),RESULT_U8BIT);
  proc_value_largerecord(value_largerec);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;
    
  if failed then
    fail
  else
    WriteLn('Passed!');

  write('(Inline) Value parameter test (src : LOC_REFERENCE (recorddef)))...');
  global_u8bit := 0;
  failed := false;
  fillchar(value_smallrec,sizeof(value_smallrec),#0);

  value_smallrec.b := RESULT_U8BIT;
  value_smallrec.w := RESULT_U16BIT;
  proc_value_smallrecord_inline(value_smallrec);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;
    
  global_u8bit := 0;
  fillchar(value_largerec,sizeof(value_largerec),RESULT_U8BIT);
  proc_value_largerecord_inline(value_largerec);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;
    
  if failed then
    fail
  else
    WriteLn('Passed!');
    
    
  write('Value parameter test (src : LOC_REFERENCE (setdef)))...');
  global_u8bit := 0;
  failed := false;
  value_smallset := [A_A,A_D];

  proc_value_smallset(value_smallset);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;
  if failed then
    fail
  else
    WriteLn('Passed!');


  write('(Inline) Value parameter test (src : LOC_REFERENCE (setdef)))...');
  global_u8bit := 0;
  failed := false;
  value_smallset := [A_A,A_D];

  proc_value_smallset_inline(value_smallset);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;
  if failed then
    fail
  else
    WriteLn('Passed!');
    
  { NEED TO FINISH : PUSH_PARA_VALUE!! }
end.


{
  $Log$
  Revision 1.1  2002-03-30 23:18:43  carl
  + callparan node testing (only 60% finished!)

}