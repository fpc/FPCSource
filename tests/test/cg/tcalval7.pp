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
{          (value parameters with register calling convention)   }
{****************************************************************}
program tcalval7;

{$ifdef fpc}
{$mode objfpc}
{$INLINE ON}
{$endif}
{$R+}
{$P-}

{$ifdef VER70}
  {$define tp}
{$endif}


{$ifdef cpu68k}
  {$define cpusmall}
{$endif}
{$ifdef cpu8086}
  {$define cpusmall}
{$endif}

 { REAL should map to single or double }
 { so it is not checked, since single  }
 { double nodes are checked.           }

 { assumes that enumdef is the same as orddef (same storage format) }

 const
{ should be defined depending on CPU target }
{$ifdef fpc}
  {$ifdef cpusmall}
    BIG_INDEX = 8000;
    SMALL_INDEX  = 13;
  {$else}
    BIG_INDEX = 33000;
    SMALL_INDEX = 13;     { value should not be aligned! }
  {$endif}
{$else}
  BIG_INDEX = 33000;
  SMALL_INDEX = 13;     { value should not be aligned! }
{$endif}
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
  RESULT_BIGSTRING = 'Hello world';
  RESULT_SMALLSTRING = 'H';
  RESULT_CHAR = 'I';
  RESULT_BOOLEAN = TRUE;

type
{$ifndef tp}
  tclass1 = class
  end;
{$else}
  shortstring = string;
{$endif}

  tprocedure = procedure;

  tsmallrecord =
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
  packed
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
  record
    b: byte;
    w: word;
  end;

  tlargerecord = packed record
    b: array[1..BIG_INDEX] of byte;
  end;

  tsmallarray = packed array[1..SMALL_INDEX] of byte;

  tsmallsetenum =
  (A_A,A_B,A_C,A_D);

  tsmallset = set of tsmallsetenum;
  tlargeset = set of char;

  tsmallstring = string[2];





var
 global_u8bit : byte;
 global_u16bit : word;
 global_s32bit : longint;
 global_s32real : single;
 global_s64real : double;
 global_ptr : pchar;
 global_proc : tprocedure;
 global_bigstring : shortstring;
 global_boolean : boolean;
 global_char : char;
{$ifndef tp}
 global_class : tclass1;
 global_s64bit : int64;
 value_s64bit : int64;
 value_class : tclass1;
{$endif}
 value_u8bit : byte;
 value_u16bit : word;
 value_s32bit : longint;
 value_s32real : single;
 value_s64real  : double;
 value_proc : tprocedure;
 value_ptr : pchar;
 value_smallrec : tsmallrecord;
 value_largerec : tlargerecord;
 value_smallset : tsmallset;
 value_smallstring : tsmallstring;
 value_bigstring   : shortstring;
 value_largeset : tlargeset;
 value_smallarray : tsmallarray;
 value_boolean : boolean;
 value_char : char;

    procedure fail;
    begin
      WriteLn('Failure.');
      halt(1);
    end;


    procedure clear_globals;
     begin
      global_u8bit := 0;
      global_u16bit := 0;
      global_s32bit := 0;
      global_s32real := 0.0;
      global_s64real := 0.0;
      global_ptr := nil;
      global_proc := nil;
      global_bigstring := '';
      global_boolean := false;
      global_char := #0;
{$ifndef tp}
      global_s64bit := 0;
      global_class := nil;
{$endif}
     end;


    procedure clear_values;
     begin
      value_u8bit := 0;
      value_u16bit := 0;
      value_s32bit := 0;
      value_s32real := 0.0;
      value_s64real  := 0.0;
      value_proc := nil;
      value_ptr := nil;
      fillchar(value_smallrec, sizeof(value_smallrec), #0);
      fillchar(value_largerec, sizeof(value_largerec), #0);
      value_smallset := [];
      value_smallstring := '';
      value_bigstring   := '';
      value_largeset := [];
      fillchar(value_smallarray, sizeof(value_smallarray), #0);
      value_boolean := false;
      value_char:=#0;
{$ifndef tp}
      value_s64bit := 0;
      value_class := nil;
{$endif}
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

   function gets64bit: int64;
    begin
      gets64bit:=RESULT_S64BIT;
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

  procedure proc_value_u8bit(v: byte);register;
   begin
     global_u8bit := v;
   end;


  procedure proc_value_u16bit(v: word);register;
   begin
     global_u16bit := v;
   end;


  procedure proc_value_s32bit(v : longint);register;
   begin
     global_s32bit := v;
   end;




  procedure proc_value_bool8bit(v: boolean);register;
   begin
     { boolean should be 8-bit always! }
     if sizeof(boolean) <> 1 then RunError(255);
     global_u8bit := byte(v);
   end;


  procedure proc_value_bool16bit(v: wordbool);register;
   begin
     global_u16bit := word(v);
   end;


  procedure proc_value_bool32bit(v : longbool);register;
   begin
     global_s32bit := longint(v);
   end;


  procedure proc_value_s32real(v : single);register;
   begin
     global_s32real := v;
   end;

  procedure proc_value_s64real(v: double);register;
   begin
     global_s64real:= v;
   end;


  procedure proc_value_pointerdef(p : pchar);register;
   begin
     global_ptr:=p;
   end;


  procedure proc_value_procvardef(p : tprocedure);register;
   begin
     global_proc:=p;
   end;




  procedure proc_value_smallrecord(smallrec : tsmallrecord);register;
   begin
     if (smallrec.b = RESULT_U8BIT) and (smallrec.w = RESULT_U16BIT) then
       global_u8bit := RESULT_U8BIT;
   end;


  procedure proc_value_largerecord(largerec : tlargerecord);register;
   begin
     if (largerec.b[1] = RESULT_U8BIT) and (largerec.b[2] = RESULT_U8BIT) then
       global_u8bit := RESULT_U8BIT;
   end;

  procedure proc_value_smallset(smallset : tsmallset);register;
   begin
     if A_D in smallset then
       global_u8bit := RESULT_U8BIT;
   end;


  procedure proc_value_largeset(largeset : tlargeset);register;
   begin
     if 'I' in largeset then
       global_u8bit := RESULT_U8BIT;
   end;

  procedure proc_value_smallstring(s:tsmallstring);register;
   begin
     if s = RESULT_SMALLSTRING then
       global_u8bit := RESULT_u8BIT;
   end;


  procedure proc_value_bigstring(s:shortstring);register;
   begin
     if s = RESULT_BIGSTRING then
       global_u8bit := RESULT_u8BIT;
   end;


  procedure proc_value_smallarray(arr : tsmallarray);register;
  begin
    if arr[SMALL_INDEX] = RESULT_U8BIT then
      global_u8bit := RESULT_U8BIT;
  end;

  procedure proc_value_smallarray_open(arr : array of byte);register;
  begin
    { form 0 to N-1 indexes in open arrays }
    if arr[SMALL_INDEX-1] = RESULT_U8BIT then
      global_u8bit := RESULT_U8BIT;
  end;

{$ifndef tp}
  procedure proc_value_classrefdef(obj : tclass1);register;
   begin
     global_class:=obj;
   end;


  procedure proc_value_smallarray_const_1(arr : array of const);register;
  var
   i: integer;
  begin
    for i:=0 to high(arr) do
     begin
       case arr[i].vtype of
        vtInteger : global_u8bit := arr[i].vinteger and $ff;
        vtBoolean : global_boolean := arr[i].vboolean;
        vtChar : global_char := arr[i].vchar;
        vtExtended : global_s64real := arr[i].VExtended^;
        vtString :  global_bigstring := arr[i].VString^;
        vtPointer : ;
        vtPChar : global_ptr := arr[i].VPchar;
        vtObject : ;
{        vtClass : global_class := (arr[i].VClass) as tclass1;}
        vtAnsiString : ;
        vtInt64 :  global_s64bit := arr[i].vInt64^;
        else
          RunError(255);
       end;
     end; {endfor}
  end;


  procedure proc_value_smallarray_const_2(arr : array of const);register;
  var
   i: integer;
  begin
     if high(arr)<0 then
       global_u8bit := RESULT_U8BIT;
  end;

  procedure proc_value_s64bit(v: int64);register;
   begin
     global_s64bit:= v;
   end;
{$endif}

 {********************************* MIXED PARAMETERS *************************}

  procedure proc_value_u8bit_mixed(b1 : byte; v: byte; b2: byte);register;
   begin
     global_u8bit := v;
     value_u8bit := b2;
   end;


  procedure proc_value_u16bit_mixed(b1: byte; v: word; b2: byte);register;
   begin
     global_u16bit := v;
     value_u8bit := b2;
   end;


  procedure proc_value_s32bit_mixed(b1 : byte; v : longint; b2: byte);register;
   begin
     global_s32bit := v;
     value_u8bit := b2;
   end;




  procedure proc_value_bool8bit_mixed(b1: byte; v: boolean; b2: byte);register;
   begin
     { boolean should be 8-bit always! }
     if sizeof(boolean) <> 1 then RunError(255);
     global_u8bit := byte(v);
     value_u8bit := b2;
   end;


  procedure proc_value_bool16bit_mixed(b1 : byte; v: wordbool; b2: byte);register;
   begin
     global_u16bit := word(v);
     value_u8bit := b2;
   end;


  procedure proc_value_bool32bit_mixed(b1 : byte; v : longbool; b2: byte);register;
   begin
     global_s32bit := longint(v);
     value_u8bit := b2;
   end;


  procedure proc_value_s32real_mixed(b1: byte; v : single; b2: byte);register;
   begin
     global_s32real := v;
     value_u8bit := b2;
   end;

  procedure proc_value_s64real_mixed(b1: byte; v: double; b2: byte);register;
   begin
     global_s64real:= v;
     value_u8bit := b2;
   end;


  procedure proc_value_pointerdef_mixed(b1: byte; p : pchar; b2: byte);register;
   begin
     global_ptr:=p;
     value_u8bit := b2;
   end;


  procedure proc_value_procvardef_mixed(b1: byte; p : tprocedure; b2: byte);register;
   begin
     global_proc:=p;
     value_u8bit := b2;
   end;




  procedure proc_value_smallrecord_mixed(b1: byte; smallrec : tsmallrecord; b2: byte);register;
   begin
     if (smallrec.b = RESULT_U8BIT) and (smallrec.w = RESULT_U16BIT) then
       global_u8bit := RESULT_U8BIT;
     value_u8bit := b2;
   end;


  procedure proc_value_largerecord_mixed(b1: byte; largerec : tlargerecord; b2: byte);register;
   begin
     if (largerec.b[1] = RESULT_U8BIT) and (largerec.b[2] = RESULT_U8BIT) then
       global_u8bit := RESULT_U8BIT;
     value_u8bit := b2;
   end;

  procedure proc_value_smallset_mixed(b1: byte; smallset : tsmallset; b2: byte);register;
   begin
     if A_D in smallset then
       global_u8bit := RESULT_U8BIT;
     value_u8bit := b2;
   end;


  procedure proc_value_largeset_mixed(b1: byte; largeset : tlargeset; b2: byte);register;
   begin
     if 'I' in largeset then
       global_u8bit := RESULT_U8BIT;
     value_u8bit := b2;
   end;

  procedure proc_value_smallstring_mixed(b1: byte; s:tsmallstring; b2: byte);register;
   begin
     if s = RESULT_SMALLSTRING then
       global_u8bit := RESULT_u8BIT;
     value_u8bit := b2;
   end;


  procedure proc_value_bigstring_mixed(b1: byte; s:shortstring; b2: byte);register;
   begin
     if s = RESULT_BIGSTRING then
       global_u8bit := RESULT_u8BIT;
     value_u8bit := b2;
   end;


  procedure proc_value_smallarray_mixed(b1: byte; arr : tsmallarray; b2: byte);register;
  begin
    if arr[SMALL_INDEX] = RESULT_U8BIT then
      global_u8bit := RESULT_U8BIT;
     value_u8bit := b2;
  end;

  procedure proc_value_smallarray_open_mixed(b1: byte; arr : array of byte; b2: byte);register;
  begin
    { form 0 to N-1 indexes in open arrays }
    if arr[SMALL_INDEX-1] = RESULT_U8BIT then
      global_u8bit := RESULT_U8BIT;
     value_u8bit := b2;
  end;

{$ifndef tp}
  procedure proc_value_classrefdef_mixed(b1: byte; obj : tclass1; b2: byte);register;
   begin
     global_class:=obj;
     value_u8bit := b2;
   end;


  procedure proc_value_s64bit_mixed(b1 : byte; v: int64; b2: byte);register;
   begin
     global_s64bit:= v;
     value_u8bit := b2;
   end;


  procedure proc_value_smallarray_const_1_mixed(b1: byte; arr : array of const; b2: byte);register;
  var
   i: integer;
  begin
    for i:=0 to high(arr) do
     begin
       case arr[i].vtype of
        vtInteger : global_u8bit := arr[i].vinteger and $ff;
        vtBoolean : global_boolean := arr[i].vboolean;
        vtChar : global_char := arr[i].vchar;
        vtExtended : global_s64real := arr[i].VExtended^;
        vtString :  global_bigstring := arr[i].VString^;
        vtPointer : ;
        vtPChar : global_ptr := arr[i].VPchar;
        vtObject : ;
{        vtClass : global_class := (arr[i].VClass) as tclass1;}
        vtAnsiString : ;
        vtInt64 :  global_s64bit := arr[i].vInt64^;
        else
          RunError(255);
       end;
     end; {endfor}
     value_u8bit := b2;
  end;


  procedure proc_value_smallarray_const_2_mixed(b1: byte; arr : array of const; b2: byte);register;
  var
   i: integer;
  begin
     if high(arr)<0 then
       global_u8bit := RESULT_U8BIT;
     value_u8bit := b2;
  end;
{$endif}



var
 failed: boolean;
Begin
  {***************************** NORMAL TESTS *******************************}
  clear_globals;
  clear_values;

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
{$ifndef tp}
  proc_value_s64bit(gets64bit);
  if global_s64bit <> RESULT_S64BIT then
    failed:=true;
{$endif}
  if failed then
    fail
  else
    WriteLn('Passed!');


  { LOC_FPUREGISTER }
  clear_globals;
  clear_values;
  failed:=false;
  write('Value parameter test (src : LOC_FPUREGISTER)...');
  proc_value_s32real(gets32real);
  if trunc(global_s32real) <> trunc(RESULT_S32REAL) then
    failed:=true;
  proc_value_s64real(gets64real);
  if trunc(global_s64real) <> trunc(RESULT_S64REAL) then
    failed:=true;
  if failed then
    fail
  else
    WriteLn('Passed!');


  { LOC_MEM, LOC_REFERENCE orddef }
  clear_globals;
  clear_values;
  value_u8bit := RESULT_U8BIT;
  value_u16bit := RESULT_U16BIT;
  value_s32bit := RESULT_S32BIT;
{$ifndef tp}
  value_s64bit := RESULT_S64BIT;
{$endif}
  value_s32real := RESULT_S32REAL;
  value_s64real  := RESULT_S64REAL;

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
{$ifndef tp}
  proc_value_s64bit(value_s64bit);
  if global_s64bit <> RESULT_S64BIT then
    failed:=true;
{$endif}
  if failed then
    fail
  else
    WriteLn('Passed!');


  { LOC_REFERENCE }
  clear_globals;
  failed:=false;
  write('Value parameter test (src : LOC_REFERENCE (floatdef))...');
  proc_value_s32real(value_s32real);
  if trunc(global_s32real) <> trunc(RESULT_S32REAL) then
    failed:=true;
  proc_value_s64real(value_s64real);
  if trunc(global_s64real) <> trunc(RESULT_S64REAL) then
    failed:=true;
  if failed then
    fail
  else
    WriteLn('Passed!');



  write('Value parameter test (src : LOC_REFERENCE (pointer))...');
  clear_globals;
  clear_values;
  failed:=false;
  value_ptr := RESULT_PCHAR;
  proc_value_pointerdef(value_ptr);
  if global_ptr <> value_ptr then
    failed := true;


  value_proc := {$ifndef tp}@{$endif}testprocedure;
  proc_value_procvardef(value_proc);
  if {$ifndef fpc}@{$endif}value_proc <> {$ifndef fpc}@{$endif}global_proc then
    failed := true;

{$ifndef tp}
  value_class := tclass1.create;
  proc_value_classrefdef(value_class);
  if value_class <> global_class then
    failed := true;
  value_class.destroy;
{$endif}
  if failed then
    fail
  else
    WriteLn('Passed!');




  { LOC_REFERENCE }
  clear_globals;
  clear_values;
  failed:=false;
  value_u8bit := 0;
  write('Value parameter test (src : LOC_FLAGS (orddef)))...');
  proc_value_bool8bit(value_u8bit = 0);
  if global_u8bit <> RESULT_BOOL8BIT then
    failed:=true;
{* IMPOSSIBLE TO GENERATE LOC_FLAGS WITH SIZE <> S_B ON VERSION 1.0.x
  proc_value_bool16bit(value_s64bit < 0);
  if global_u16bit <> RESULT_BOOL16BIT then
    failed:=true;
  proc_value_bool32bit(bool1 and bool2);
  if global_s32bit <> RESULT_BOOL32BIT then
    failed:=true;*}
  if failed then
    fail
  else
    WriteLn('Passed!');



{$ifndef tp}
  clear_globals;
  clear_values;
  failed:=false;
  write('Value parameter test (src : LOC_JUMP (orddef)))...');
  proc_value_bool8bit(value_s64bit = 0);
  if global_u8bit <> RESULT_BOOL8BIT then
    failed:=true;
{* IMPOSSIBLE TO GENERATE LOC_JUMP WITH SIZE <> S_B ON VERSION 1.0.x
  proc_value_bool16bit(value_s64bit < 0);
  if global_u16bit <> RESULT_BOOL16BIT then
    failed:=true;
  proc_value_bool32bit(bool1 and bool2);
  if global_s32bit <> RESULT_BOOL32BIT then
    failed:=true;*}
  if failed then
    fail
  else
    WriteLn('Passed!');
{$endif}

  { arraydef,
    recorddef,
    objectdef,
    stringdef,
    setdef : all considered the same by code generator.
  }
  write('Value parameter test (src : LOC_REFERENCE (recorddef)))...');
  clear_globals;
  clear_values;
  failed := false;

  value_smallrec.b := RESULT_U8BIT;
  value_smallrec.w := RESULT_U16BIT;
  proc_value_smallrecord(value_smallrec);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;

  clear_globals;
  clear_values;
  fillchar(value_largerec,sizeof(value_largerec),RESULT_U8BIT);
  proc_value_largerecord(value_largerec);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;

  if failed then
    fail
  else
    WriteLn('Passed!');



  write('Value parameter test (src : LOC_REFERENCE (setdef)))...');
  clear_globals;
  clear_values;
  failed := false;

  value_smallset := [A_A,A_D];
  proc_value_smallset(value_smallset);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;

  clear_globals;
  clear_values;
  value_largeset := ['I'];
  proc_value_largeset(value_largeset);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;

  if failed then
    fail
  else
    WriteLn('Passed!');





  write('Value parameter test (src : LOC_REFERENCE (stringdef)))...');
  clear_globals;
  clear_values;
  failed := false;
  value_smallstring := RESULT_SMALLSTRING;

  proc_value_smallstring(value_smallstring);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;

  clear_globals;
  clear_values;
  value_bigstring := RESULT_BIGSTRING;
  proc_value_bigstring(value_bigstring);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;

  if failed then
    fail
  else
    WriteLn('Passed!');



  { open array by value with cdecl modifier!!!!!!!!!!!!!!!!!!!!!!!!!!!}
  { DON'T KNOW WHY/HOW TO TEST!!!!!                                   }


  write('Value parameter test (src : LOC_REFERENCE (arraydef)))...');

  clear_globals;
  clear_values;
  failed:=false;

  fillchar(value_smallarray,sizeof(value_smallarray),#0);
  value_smallarray[SMALL_INDEX] := RESULT_U8BIT;
  proc_value_smallarray(value_smallarray);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;

  clear_globals;
  clear_values;

  fillchar(value_smallarray,sizeof(value_smallarray),#0);
  value_smallarray[SMALL_INDEX] := RESULT_U8BIT;
  proc_value_smallarray_open(value_smallarray);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;

{$ifndef tp}
  clear_globals;
  clear_values;

  value_u8bit := RESULT_U8BIT;
  value_ptr := RESULT_PCHAR;
  value_s64bit := RESULT_S64BIT;
  value_smallstring := RESULT_SMALLSTRING;
  value_class := tclass1.create;
  value_boolean := RESULT_BOOLEAN;
  value_char := RESULT_CHAR;
  value_s64real:=RESULT_S64REAL;
  proc_value_smallarray_const_1([value_u8bit,value_ptr,value_s64bit,value_char,value_smallstring,value_s64real,
    value_boolean,value_class]);

  if global_u8bit <> RESULT_U8BIT then
    failed := true;

  if global_char <> RESULT_CHAR then
    failed := true;
  if global_boolean <> RESULT_BOOLEAN then
    failed:=true;
  if trunc(global_s64real) <> trunc(RESULT_S64REAL) then
     failed := true;
  if global_bigstring <> RESULT_SMALLSTRING then
     failed := true;
  if global_ptr <> value_ptr then
     failed := true;
{  if value_class <> global_class then
     failed := true;!!!!!!!!!!!!!!!!!!!!}
  if global_s64bit <> RESULT_S64BIT then
     failed := true;
  if assigned(value_class) then
    value_class.destroy;

  global_u8bit := 0;
  proc_value_smallarray_const_2([]);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;
{$endif fpc}

  if failed then
    fail
  else
    WriteLn('Passed!');

  {***************************** MIXED  TESTS *******************************}
  clear_globals;
  clear_values;

  failed:=false;

  { LOC_REGISTER }
  write('Mixed value parameter test (src : LOC_REGISTER)...');
  proc_value_u8bit_mixed(byte(NOT RESULT_U8BIT),getu8bit,RESULT_U8BIT);
  if global_u8bit <> RESULT_U8BIT then
    failed:=true;
  if value_u8bit <> RESULT_U8BIT then
    failed := true;
  proc_value_u16bit_mixed(byte(NOT RESULT_U8BIT),getu16bit,RESULT_U8BIT);
  if global_u16bit <> RESULT_U16BIT then
    failed:=true;
  if value_u8bit <> RESULT_U8BIT then
    failed := true;
  proc_value_s32bit_mixed(byte(NOT RESULT_U8BIT),gets32bit, RESULT_U8BIT);
  if global_s32bit <> RESULT_S32BIT then
    failed:=true;
  if value_u8bit <> RESULT_U8BIT then
    failed := true;
{$ifndef tp}
  proc_value_s64bit_mixed(byte(NOT RESULT_U8BIT),gets64bit,RESULT_U8BIT);
  if global_s64bit <> RESULT_S64BIT then
    failed:=true;
{$endif}
  if value_u8bit <> RESULT_U8BIT then
    failed := true;

  if failed then
    fail
  else
    WriteLn('Passed!');


  { LOC_FPUREGISTER }
  clear_globals;
  clear_values;
  failed:=false;
  write('Mixed value parameter test (src : LOC_FPUREGISTER)...');
  proc_value_s32real_mixed(byte(NOT RESULT_U8BIT), gets32real, RESULT_U8BIT);
  if value_u8bit <> RESULT_U8BIT then
    failed := true;
  if trunc(global_s32real) <> trunc(RESULT_S32REAL) then
    failed:=true;
  proc_value_s64real_mixed(byte(NOT RESULT_U8BIT),gets64real,RESULT_U8BIT);
  if value_u8bit <> RESULT_U8BIT then
    failed := true;
  if trunc(global_s64real) <> trunc(RESULT_S64REAL) then
    failed:=true;
  if failed then
    fail
  else
    WriteLn('Passed!');


  { LOC_MEM, LOC_REFERENCE orddef }
  clear_globals;
  clear_values;
  value_u8bit := RESULT_U8BIT;
  value_u16bit := RESULT_U16BIT;
  value_s32bit := RESULT_S32BIT;
{$ifndef tp}
  value_s64bit := RESULT_S64BIT;
{$endif}
  value_s32real := RESULT_S32REAL;
  value_s64real  := RESULT_S64REAL;

  failed:=false;

  { LOC_REFERENCE }
  write('Mixed value parameter test (src : LOC_REFERENCE (orddef/enumdef)))...');
  proc_value_u8bit_mixed(byte(NOT RESULT_U8BIT),value_u8bit, RESULT_U8BIT);
  if global_u8bit <> RESULT_U8BIT then
    failed:=true;
  if value_u8bit <> RESULT_U8BIT then
    failed := true;
  proc_value_u16bit_mixed(byte(NOT RESULT_U8BIT),value_u16bit, RESULT_U8BIT);
  if global_u16bit <> RESULT_U16BIT then
    failed:=true;
  if value_u8bit <> RESULT_U8BIT then
    failed := true;
  proc_value_s32bit_mixed(byte(NOT RESULT_U8BIT),value_s32bit, RESULT_U8BIT);
  if global_s32bit <> RESULT_S32BIT then
    failed:=true;
  if value_u8bit <> RESULT_U8BIT then
    failed := true;
{$ifndef tp}
  proc_value_s64bit_mixed(byte(NOT RESULT_U8BIT), value_s64bit, RESULT_U8BIT);
  if global_s64bit <> RESULT_S64BIT then
    failed:=true;
{$endif}
  if value_u8bit <> RESULT_U8BIT then
    failed := true;

  if failed then
    fail
  else
    WriteLn('Passed!');


  { LOC_REFERENCE }
  clear_globals;
  failed:=false;
  write('Mixed value parameter test (src : LOC_REFERENCE (floatdef))...');
  proc_value_s32real_mixed(byte(NOT RESULT_U8BIT), value_s32real, RESULT_U8BIT);
  if trunc(global_s32real) <> trunc(RESULT_S32REAL) then
    failed:=true;
  if value_u8bit <> RESULT_U8BIT then
    failed := true;
  proc_value_s64real_mixed(byte(NOT RESULT_U8BIT), value_s64real, RESULT_U8BIT);
  if trunc(global_s64real) <> trunc(RESULT_S64REAL) then
    failed:=true;
  if value_u8bit <> RESULT_U8BIT then
    failed := true;

  if failed then
    fail
  else
    WriteLn('Passed!');



  write('Mixed value parameter test (src : LOC_REFERENCE (pointer))...');
  clear_globals;
  clear_values;
  failed:=false;
  value_ptr := RESULT_PCHAR;
  proc_value_pointerdef_mixed(byte(NOT RESULT_U8BIT), value_ptr, RESULT_U8BIT);
  if global_ptr <> value_ptr then
    failed := true;
  if value_u8bit <> RESULT_U8BIT then
    failed := true;


  value_proc := {$ifndef tp}@{$endif}testprocedure;
  proc_value_procvardef_mixed(byte(NOT RESULT_U8BIT), value_proc, RESULT_U8BIT);
  if {$ifndef fpc}@{$endif}value_proc <> {$ifndef fpc}@{$endif}global_proc then
    failed := true;

{$ifndef tp}
  value_class := tclass1.create;
  proc_value_classrefdef_mixed(byte(NOT RESULT_U8BIT), value_class, RESULT_U8BIT);
  if value_class <> global_class then
    failed := true;
  if value_u8bit <> RESULT_U8BIT then
    failed := true;
  value_class.destroy;
{$endif}
  if failed then
    fail
  else
    WriteLn('Passed!');




  { LOC_REFERENCE }
  clear_globals;
  clear_values;
  failed:=false;
  value_u8bit := 0;
  write('Mixed value parameter test (src : LOC_FLAGS (orddef)))...');
  proc_value_bool8bit_mixed(byte(NOT RESULT_U8BIT), value_u8bit = 0, RESULT_U8BIT);
  if global_u8bit <> RESULT_BOOL8BIT then
    failed:=true;
  if value_u8bit <> RESULT_U8BIT then
    failed := true;
{* IMPOSSIBLE TO GENERATE LOC_FLAGS WITH SIZE <> S_B ON VERSION 1.0.x
  proc_value_bool16bit(value_s64bit < 0);
  if global_u16bit <> RESULT_BOOL16BIT then
    failed:=true;
  proc_value_bool32bit(bool1 and bool2);
  if global_s32bit <> RESULT_BOOL32BIT then
    failed:=true;*}
  if failed then
    fail
  else
    WriteLn('Passed!');



{$ifndef tp}
  clear_globals;
  clear_values;
  failed:=false;
  write('Mixed value parameter test (src : LOC_JUMP (orddef)))...');
  proc_value_bool8bit_mixed(byte(NOT RESULT_U8BIT), value_s64bit = 0, RESULT_U8BIT);
  if global_u8bit <> RESULT_BOOL8BIT then
    failed:=true;
  if value_u8bit <> RESULT_U8BIT then
    failed := true;
{* IMPOSSIBLE TO GENERATE LOC_JUMP WITH SIZE <> S_B ON VERSION 1.0.x
  proc_value_bool16bit(value_s64bit < 0);
  if global_u16bit <> RESULT_BOOL16BIT then
    failed:=true;
  proc_value_bool32bit(bool1 and bool2);
  if global_s32bit <> RESULT_BOOL32BIT then
    failed:=true;*}
  if failed then
    fail
  else
    WriteLn('Passed!');
{$endif}

  { arraydef,
    recorddef,
    objectdef,
    stringdef,
    setdef : all considered the same by code generator.
  }
  write('Mixed value parameter test (src : LOC_REFERENCE (recorddef)))...');
  clear_globals;
  clear_values;
  failed := false;

  value_smallrec.b := RESULT_U8BIT;
  value_smallrec.w := RESULT_U16BIT;
  proc_value_smallrecord_mixed(byte(NOT RESULT_U8BIT), value_smallrec, RESULT_U8BIT);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;
  if value_u8bit <> RESULT_U8BIT then
    failed := true;

  clear_globals;
  clear_values;
  fillchar(value_largerec,sizeof(value_largerec),RESULT_U8BIT);
  proc_value_largerecord_mixed(byte(NOT RESULT_U8BIT), value_largerec, RESULT_U8BIT);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;
  if value_u8bit <> RESULT_U8BIT then
    failed := true;

  if failed then
    fail
  else
    WriteLn('Passed!');



  write('Mixed value parameter test (src : LOC_REFERENCE (setdef)))...');
  clear_globals;
  clear_values;
  failed := false;

  value_smallset := [A_A,A_D];
  proc_value_smallset_mixed(byte(NOT RESULT_U8BIT), value_smallset, RESULT_U8BIT);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;
  if value_u8bit <> RESULT_U8BIT then
    failed := true;

  clear_globals;
  clear_values;
  value_largeset := ['I'];
  proc_value_largeset_mixed(byte(NOT RESULT_U8BIT), value_largeset, RESULT_U8BIT);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;
  if value_u8bit <> RESULT_U8BIT then
    failed := true;

  if failed then
    fail
  else
    WriteLn('Passed!');





  write('Mixed value parameter test (src : LOC_REFERENCE (stringdef)))...');
  clear_globals;
  clear_values;
  failed := false;
  value_smallstring := RESULT_SMALLSTRING;

  proc_value_smallstring_mixed(byte(NOT RESULT_U8BIT), value_smallstring, RESULT_U8BIT);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;
  if value_u8bit <> RESULT_U8BIT then
    failed := true;

  clear_globals;
  clear_values;
  value_bigstring := RESULT_BIGSTRING;
  proc_value_bigstring_mixed(byte(NOT RESULT_U8BIT), value_bigstring, RESULT_U8BIT);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;
  if value_u8bit <> RESULT_U8BIT then
    failed := true;

  if failed then
    fail
  else
    WriteLn('Passed!');



  { open array by value with cdecl modifier!!!!!!!!!!!!!!!!!!!!!!!!!!!}
  { DON'T KNOW WHY/HOW TO TEST!!!!!                                   }


  write('Mixed value parameter test (src : LOC_REFERENCE (arraydef)))...');

  clear_globals;
  clear_values;
  failed:=false;

  fillchar(value_smallarray,sizeof(value_smallarray),#0);
  value_smallarray[SMALL_INDEX] := RESULT_U8BIT;
  proc_value_smallarray_mixed(byte(NOT RESULT_U8BIT), value_smallarray, RESULT_U8BIT);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;
  if value_u8bit <> RESULT_U8BIT then
    failed := true;

  clear_globals;
  clear_values;

  fillchar(value_smallarray,sizeof(value_smallarray),#0);
  value_smallarray[SMALL_INDEX] := RESULT_U8BIT;
  proc_value_smallarray_open_mixed(byte(NOT RESULT_U8BIT), value_smallarray, RESULT_U8BIT);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;
  if value_u8bit <> RESULT_U8BIT then
    failed := true;

{$ifndef tp}
  clear_globals;
  clear_values;

  value_u8bit := RESULT_U8BIT;
  value_ptr := RESULT_PCHAR;
  value_s64bit := RESULT_S64BIT;
  value_smallstring := RESULT_SMALLSTRING;
  value_class := tclass1.create;
  value_boolean := RESULT_BOOLEAN;
  value_char := RESULT_CHAR;
  value_s64real:=RESULT_S64REAL;
  proc_value_smallarray_const_1_mixed(byte(NOT RESULT_U8BIT), [value_u8bit,value_ptr,value_s64bit,value_char,
   value_smallstring,value_s64real,value_boolean,value_class],
     RESULT_U8BIT);
  if value_u8bit <> RESULT_U8BIT then
    failed := true;

  if global_u8bit <> RESULT_U8BIT then
    failed := true;

  if global_char <> RESULT_CHAR then
    failed := true;
  if global_boolean <> RESULT_BOOLEAN then
    failed:=true;
  if trunc(global_s64real) <> trunc(RESULT_S64REAL) then
     failed := true;
  if global_bigstring <> RESULT_SMALLSTRING then
     failed := true;
  if global_ptr <> value_ptr then
     failed := true;
{  if value_class <> global_class then
     failed := true;!!!!!!!!!!!!!!!!!!!!}
  if global_s64bit <> RESULT_S64BIT then
     failed := true;
  if assigned(value_class) then
    value_class.destroy;

  global_u8bit := 0;
  proc_value_smallarray_const_2_mixed(byte(NOT RESULT_U8BIT), [], RESULT_U8BIT);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;
  if value_u8bit <> RESULT_U8BIT then
    failed := true;
{$endif}

  if failed then
    fail
  else
    WriteLn('Passed!');

end.
