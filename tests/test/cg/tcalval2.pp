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
{          (value parameters with inline calls)                  }
{****************************************************************}
program tcalval2;

{$mode objfpc}
{$INLINE ON}
{$R+}
{$P-}

{$ifdef VER70}
  {$define tp}
{$endif}


 { REAL should map to single or double }
 { so it is not checked, since single  }
 { double nodes are checked.           }

 { assumes that enumdef is the same as orddef (same storage format) }

 const
{ should be defined depending on CPU target }
{$ifdef fpc}
  {$ifdef cpu68k}
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
  tclass1 = class
  end;

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
 global_s64bit : int64;
 global_s32real : single;
 global_s64real : double;
 global_ptr : pchar;
 global_proc : tprocedure;
 global_class : tclass1;
 global_bigstring : shortstring;
 global_boolean : boolean;
 global_char : char;
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
      global_s64bit := 0;
      global_s32real := 0.0;
      global_s64real := 0.0;
      global_ptr := nil;
      global_proc := nil;
      global_class := nil;
      global_bigstring := '';
      global_boolean := false;
      global_char := #0;
     end;


    procedure clear_values;
     begin
      value_u8bit := 0;
      value_u16bit := 0;
      value_s32bit := 0;
      value_s64bit := 0;
      value_s32real := 0.0;
      value_s64real  := 0.0;
      value_proc := nil;
      value_ptr := nil;
      value_class := nil;
      fillchar(value_smallrec, sizeof(value_smallrec), #0);
      fillchar(value_largerec, sizeof(value_largerec), #0);
      value_smallset := [];
      value_smallstring := '';
      value_bigstring   := '';
      value_largeset := [];
      fillchar(value_smallarray, sizeof(value_smallarray), #0);
      value_boolean := false;
      value_char:=#0;
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


  procedure proc_value_largeset_inline(largeset : tlargeset);inline;
   begin
     if 'I' in largeset then
       global_u8bit := RESULT_U8BIT;
   end;


  procedure proc_value_smallstring_inline(s:tsmallstring);inline;
   begin
     if s = RESULT_SMALLSTRING then
       global_u8bit := RESULT_u8BIT;
   end;


  procedure proc_value_bigstring_inline(s:shortstring);inline;
   begin
     if s = RESULT_BIGSTRING then
       global_u8bit := RESULT_u8BIT;
   end;


  procedure proc_value_smallarray_inline(arr : tsmallarray);inline;
  begin
    if arr[SMALL_INDEX] = RESULT_U8BIT then
      global_u8bit := RESULT_U8BIT;
  end;

  procedure proc_value_smallarray_open_inline(arr : array of byte);inline;
  begin
    { form 0 to N-1 indexes in open arrays }
    if arr[SMALL_INDEX-1] = RESULT_U8BIT then
      global_u8bit := RESULT_U8BIT;
  end;


  procedure proc_value_smallarray_const_1_inline(arr : array of const);inline;
  var
   i: integer;
  begin
        global_u8bit := arr[0].vinteger and $ff;
        global_ptr := arr[1].VPchar;
        global_s64bit := arr[2].vInt64^;
        global_char := arr[3].vchar;
        global_bigstring := arr[4].VString^;
        global_s64real := arr[5].VExtended^;

        global_boolean := arr[6].vboolean;
(*
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
{        vtClass : global_class := tclass1(arr[i].VClass);}
        vtAnsiString : ;
        vtInt64 :  global_s64bit := arr[i].vInt64^;
        else
          RunError(255);
       end;
     end; {endfor}
*)
  end;


  procedure proc_value_smallarray_const_2_inline(arr : array of const);inline;
  var
   i: integer;
  begin
     if high(arr)<0 then
       global_u8bit := RESULT_U8BIT;
  end;


var
 failed: boolean;
begin

  {***************************** INLINE TESTS *******************************}

  write('(Inline) Value parameter test (src : LOC_REGISTER)...');
  clear_globals;
  clear_values;
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

  clear_globals;
  clear_values;
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

  { LOC_REFERENCE }
  write('(Inline) Value parameter test (src : LOC_REFERENCE (orddef/enumdef))...');
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


  clear_globals;
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

  write('(Inline) Value parameter test (src : LOC_REFERENCE (pointer))...');
  clear_globals;
  clear_values;
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

  write('(Inline) Value parameter test (src : LOC_FLAGS (orddef))...');
  clear_globals;
  clear_values;
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

  write('(Inline) Value parameter test (src : LOC_REFERENCE (recorddef)))...');
  failed := false;

  clear_globals;
  clear_values;
  value_smallrec.b := RESULT_U8BIT;
  value_smallrec.w := RESULT_U16BIT;
  proc_value_smallrecord_inline(value_smallrec);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;

  clear_globals;
  clear_values;
  fillchar(value_largerec,sizeof(value_largerec),RESULT_U8BIT);
  proc_value_largerecord_inline(value_largerec);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;

  if failed then
    fail
  else
    WriteLn('Passed!');

  write('(Inline) Value parameter test (src : LOC_REFERENCE (setdef)))...');
  clear_globals;
  clear_values;
  failed := false;
  value_smallset := [A_A,A_D];

  proc_value_smallset_inline(value_smallset);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;

  clear_globals;
  clear_values;
  value_largeset := ['I'];
  proc_value_largeset_inline(value_largeset);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;

  if failed then
    fail
  else
    WriteLn('Passed!');

  write('(Inline) Value parameter test (src : LOC_REFERENCE (stringdef)))...');
  clear_globals;
  clear_values;
  failed := false;
  value_smallstring := RESULT_SMALLSTRING;

  proc_value_smallstring_inline(value_smallstring);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;

  clear_globals;
  clear_values;
  value_bigstring := RESULT_BIGSTRING;
  proc_value_bigstring_inline(value_bigstring);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;

  if failed then
    fail
  else
    WriteLn('Passed!');

  write('(Inline) value parameter test (src : LOC_REFERENCE (arraydef)))...');
  clear_globals;
  clear_values;
  failed:=false;

  value_smallarray[SMALL_INDEX] := RESULT_U8BIT;
  proc_value_smallarray_inline(value_smallarray);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;

  clear_globals;
  clear_values;

  value_smallarray[SMALL_INDEX] := RESULT_U8BIT;
  proc_value_smallarray_open_inline(value_smallarray);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;

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
  proc_value_smallarray_const_1_inline([value_u8bit,value_ptr,value_s64bit,value_char,value_smallstring,value_s64real,value_boolean,value_class]);

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
  proc_value_smallarray_const_2_inline([]);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;

  if failed then
    fail
  else
    WriteLn('Passed!');
end.
