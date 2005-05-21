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
 {          (function return values with pascal   calling cnvs)   }
 {          (also tests nested routines up to 2 level deep)       }
 {****************************************************************}
 program tcalfun2;

 {$ifdef fpc}
 {$mode objfpc}
 {$INLINE ON}
 {$endif}
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
 {$ifndef tp}
   tclass1 = class
   end;
 {$else}
   shortstring = string;
 {$endif}

   tprocedure = procedure;

   tsmallrecord = packed record
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
  value_ansistring : ansistring;
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
       global_s64bit := 0;
       global_class := nil;
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
       value_ansistring := '';
 {$ifndef tp}
       value_s64bit := 0;
       value_class := nil;
 {$endif}
      end;



 {********************************* FUNCTION RESULTS *************************}

{ LOC_MEM return values }
function func_array: tsmallarray;pascal;
 var
  smallarray: tsmallarray;
 begin
  fillchar(smallarray, sizeof(smallarray), #0);
  smallarray[1] := RESULT_U8BIT;
  smallarray[SMALL_INDEX] := RESULT_U8BIT;
  func_array := smallarray;
 end;

function func_largerecord: tlargerecord;pascal;
 var
   largerecord : tlargerecord;
 begin
   fillchar(largerecord, sizeof(largerecord), #0);
   largerecord.b[1] := RESULT_U8BIT;
   largerecord.b[BIG_INDEX] := RESULT_U8BIT;
   func_largerecord := largerecord;
 end;

function func_shortstring: shortstring;pascal;
 begin
   func_shortstring := RESULT_BIGSTRING;
 end;

function func_largeset : tlargeset;pascal;
 var
  largeset : tlargeset;
 begin
  largeset := ['I'];
  func_largeset := largeset;
 end;

function func_u8bit : byte;pascal;
 begin
   func_u8bit := RESULT_U8BIT;
 end;

function func_u16bit : word;pascal;
 begin
   func_u16bit := RESULT_U16BIT;
 end;

function func_s32bit : longint;pascal;
 begin
   func_s32bit := RESULT_S32BIT;
 end;

function func_s64bit : int64;pascal;
 begin
   func_s64bit := RESULT_S64BIT;
 end;

function func_s32real : single;pascal;
 begin
   func_s32real := RESULT_S32REAL;
 end;

function func_s64real : double;pascal;
 begin
   func_s64real := RESULT_S64REAl;
 end;

function func_ansistring : ansistring;pascal;
 begin
   func_ansistring := RESULT_BIGSTRING;
 end;

function func_pchar : pchar;pascal;
 begin
   func_pchar := RESULT_PCHAR;
 end;

 {************************** FUNCTION RESULT WITH PARAMS ******************}
{ LOC_MEM return values }
function func_array_mixed(b: byte): tsmallarray;pascal;
 var
  local_b: byte;
  smallarray: tsmallarray;
 begin
  fillchar(smallarray, sizeof(smallarray), #0);
  smallarray[1] := RESULT_U8BIT;
  smallarray[SMALL_INDEX] := RESULT_U8BIT;
  func_array_mixed := smallarray;
  local_b:=b;
  global_u8bit := b;
 end;

function func_largerecord_mixed(b: byte): tlargerecord;pascal;
 var
   local_b: byte;
   largerecord : tlargerecord;
 begin
   fillchar(largerecord, sizeof(largerecord), #0);
   largerecord.b[1] := RESULT_U8BIT;
   largerecord.b[BIG_INDEX] := RESULT_U8BIT;
   func_largerecord_mixed := largerecord;
   local_b:=b;
   global_u8bit := b;
 end;

function func_shortstring_mixed(b: byte): shortstring;pascal;
 var
  local_b: byte;
 begin
   func_shortstring_mixed := RESULT_BIGSTRING;
   local_b:=b;
   global_u8bit := b;
 end;

function func_largeset_mixed(b: byte) : tlargeset;pascal;
 var
  local_b: byte;
  largeset : tlargeset;
 begin
  largeset := ['I'];
  func_largeset_mixed := largeset;
   local_b:=b;
   global_u8bit := b;
 end;

function func_u8bit_mixed(b: byte) : byte;pascal;
 var
  local_b: byte;
 begin
   func_u8bit_mixed := RESULT_U8BIT;
   local_b:=b;
   global_u8bit := b;
 end;

function func_u16bit_mixed(b: byte) : word;pascal;
 var
  local_b: byte;
 begin
   func_u16bit_mixed := RESULT_U16BIT;
   local_b:=b;
   global_u8bit := b;
 end;

function func_s32bit_mixed(b: byte) : longint;pascal;
 var
  local_b: byte;
 begin
   func_s32bit_mixed := RESULT_S32BIT;
   local_b:=b;
   global_u8bit := b;
 end;

function func_s64bit_mixed(b: byte) : int64;pascal;
 var
  local_b: byte;
 begin
   func_s64bit_mixed := RESULT_S64BIT;
   local_b:=b;
   global_u8bit := b;
 end;

function func_s32real_mixed(b: byte) : single;pascal;
 var
  local_b: byte;
 begin
   func_s32real_mixed := RESULT_S32REAL;
   local_b:=b;
   global_u8bit := b;
 end;

function func_s64real_mixed(b: byte) : double;pascal;
 var
  local_b: byte;
 begin
   func_s64real_mixed := RESULT_S64REAl;
   local_b:=b;
   global_u8bit := b;
 end;

function func_ansistring_mixed(b: byte) : ansistring;pascal;
 var
  local_b: byte;
 begin
   func_ansistring_mixed := RESULT_BIGSTRING;
   local_b:=b;
   global_u8bit := b;
 end;

function func_pchar_mixed(b: byte) : pchar;pascal;
 var
  local_b: byte;
 begin
   func_pchar_mixed := RESULT_PCHAR;
   local_b:=b;
   global_u8bit := b;
 end;

 {********************* FUNCTION RESULT WITH PARAMS (NESTED) ******************}
{ LOC_MEM return values }
function func_array_mixed_nested(b: byte): tsmallarray;pascal;

    procedure nested_one_proc(l: longint);
     begin
       global_u16bit := b;
       global_s32bit := l;
     end;

    procedure nested_two_proc(l : longint);
     begin
       global_s64bit := l;
     end;



   function nested_one_func(level1_b : byte; s: shortstring): byte;
     var
      s1 : shortstring;

      function nested_two_func(level2_b : byte; s :shortstring): byte;
        begin
          nested_two_func:=level2_b;
          global_bigstring := s;
          nested_one_proc(RESULT_S32BIT);
        end;

    begin
      s1:=s;
      nested_one_func := nested_two_func(level1_b,s1);
      nested_two_proc(level1_b);
    end;


 var
  local_b: byte;
  smallarray: tsmallarray;
 begin
  fillchar(smallarray, sizeof(smallarray), #0);
  smallarray[1] := RESULT_U8BIT;
  smallarray[SMALL_INDEX] := RESULT_U8BIT;
  func_array_mixed_nested := smallarray;
  local_b:=b;
  global_u8bit := nested_one_func(local_b, RESULT_BIGSTRING);
{  nested_one_proc(RESULT_S32BIT);}
 end;

function func_largerecord_mixed_nested(b: byte): tlargerecord;pascal;

    procedure nested_one_proc(l: longint);
     begin
       global_u16bit := b;
       global_s32bit := l;
     end;

    procedure nested_two_proc(l : longint);
     begin
       global_s64bit := l;
     end;



   function nested_one_func(level1_b : byte; s: shortstring): byte;
     var
      s1 : shortstring;

      function nested_two_func(level2_b : byte; s :shortstring): byte;
        begin
          nested_two_func:=level2_b;
          global_bigstring := s;
          nested_one_proc(RESULT_S32BIT);
        end;

    begin
      s1:=s;
      nested_one_func := nested_two_func(level1_b,s1);
      nested_two_proc(level1_b);
    end;


 var
   local_b: byte;
   largerecord : tlargerecord;
 begin
   fillchar(largerecord, sizeof(largerecord), #0);
   largerecord.b[1] := RESULT_U8BIT;
   largerecord.b[BIG_INDEX] := RESULT_U8BIT;
   func_largerecord_mixed_nested := largerecord;
   local_b:=b;
   global_u8bit := nested_one_func(local_b, RESULT_BIGSTRING);
 end;

function func_shortstring_mixed_nested(b: byte): shortstring;pascal;

    procedure nested_one_proc(l: longint);
     begin
       global_u16bit := b;
       global_s32bit := l;
     end;

    procedure nested_two_proc(l : longint);
     begin
       global_s64bit := l;
     end;



   function nested_one_func(level1_b : byte; s: shortstring): byte;
     var
      s1 : shortstring;

      function nested_two_func(level2_b : byte; s :shortstring): byte;
        begin
          nested_two_func:=level2_b;
          global_bigstring := s;
          nested_one_proc(RESULT_S32BIT);
        end;

    begin
      s1:=s;
      nested_one_func := nested_two_func(level1_b,s1);
      nested_two_proc(level1_b);
    end;

 var
  local_b: byte;
 begin
   func_shortstring_mixed_nested := RESULT_BIGSTRING;
   local_b:=b;
   global_u8bit := nested_one_func(local_b, RESULT_BIGSTRING);
 end;

function func_largeset_mixed_nested(b: byte) : tlargeset;pascal;

    procedure nested_one_proc(l: longint);
     begin
       global_u16bit := b;
       global_s32bit := l;
     end;

    procedure nested_two_proc(l : longint);
     begin
       global_s64bit := l;
     end;



   function nested_one_func(level1_b : byte; s: shortstring): byte;
     var
      s1 : shortstring;

      function nested_two_func(level2_b : byte; s :shortstring): byte;
        begin
          nested_two_func:=level2_b;
          global_bigstring := s;
          nested_one_proc(RESULT_S32BIT);
        end;

    begin
      s1:=s;
      nested_one_func := nested_two_func(level1_b,s1);
      nested_two_proc(level1_b);
    end;


 var
  local_b: byte;
  largeset : tlargeset;
 begin
  largeset := ['I'];
  func_largeset_mixed_nested := largeset;
  local_b:=b;
  global_u8bit := nested_one_func(local_b, RESULT_BIGSTRING);
 end;

function func_u8bit_mixed_nested(b: byte) : byte;pascal;

    procedure nested_one_proc(l: longint);
     begin
       global_u16bit := b;
       global_s32bit := l;
     end;

    procedure nested_two_proc(l : longint);
     begin
       global_s64bit := l;
     end;



   function nested_one_func(level1_b : byte; s: shortstring): byte;
     var
      s1 : shortstring;

      function nested_two_func(level2_b : byte; s :shortstring): byte;
        begin
          nested_two_func:=level2_b;
          global_bigstring := s;
          nested_one_proc(RESULT_S32BIT);
        end;

    begin
      s1:=s;
      nested_one_func := nested_two_func(level1_b,s1);
      nested_two_proc(level1_b);
    end;


 var
  local_b: byte;
 begin
   func_u8bit_mixed_nested := RESULT_U8BIT;
   local_b:=b;
   global_u8bit := nested_one_func(local_b, RESULT_BIGSTRING);
 end;

function func_u16bit_mixed_nested(b: byte) : word;pascal;

    procedure nested_one_proc(l: longint);
     begin
       global_u16bit := b;
       global_s32bit := l;
     end;

    procedure nested_two_proc(l : longint);
     begin
       global_s64bit := l;
     end;



   function nested_one_func(level1_b : byte; s: shortstring): byte;
     var
      s1 : shortstring;

      function nested_two_func(level2_b : byte; s :shortstring): byte;
        begin
          nested_two_func:=level2_b;
          global_bigstring := s;
          nested_one_proc(RESULT_S32BIT);
        end;

    begin
      s1:=s;
      nested_one_func := nested_two_func(level1_b,s1);
      nested_two_proc(level1_b);
    end;


 var
  local_b: byte;
 begin
   func_u16bit_mixed_nested := RESULT_U16BIT;
   local_b:=b;
   global_u8bit := nested_one_func(local_b, RESULT_BIGSTRING);
 end;

function func_s32bit_mixed_nested(b: byte) : longint;pascal;

    procedure nested_one_proc(l: longint);
     begin
       global_u16bit := b;
       global_s32bit := l;
     end;

    procedure nested_two_proc(l : longint);
     begin
       global_s64bit := l;
     end;



   function nested_one_func(level1_b : byte; s: shortstring): byte;
     var
      s1 : shortstring;

      function nested_two_func(level2_b : byte; s :shortstring): byte;
        begin
          nested_two_func:=level2_b;
          global_bigstring := s;
          nested_one_proc(RESULT_S32BIT);
        end;

    begin
      s1:=s;
      nested_one_func := nested_two_func(level1_b,s1);
      nested_two_proc(level1_b);
    end;


 var
  local_b: byte;
 begin
   func_s32bit_mixed_nested := RESULT_S32BIT;
   local_b:=b;
   global_u8bit := nested_one_func(local_b, RESULT_BIGSTRING);
 end;

function func_s64bit_mixed_nested(b: byte) : int64;pascal;

    procedure nested_one_proc(l: longint);
     begin
       global_u16bit := b;
       global_s32bit := l;
     end;

    procedure nested_two_proc(l : longint);
     begin
       global_s64bit := l;
     end;



   function nested_one_func(level1_b : byte; s: shortstring): byte;
     var
      s1 : shortstring;

      function nested_two_func(level2_b : byte; s :shortstring): byte;
        begin
          nested_two_func:=level2_b;
          global_bigstring := s;
          nested_one_proc(RESULT_S32BIT);
        end;

    begin
      s1:=s;
      nested_one_func := nested_two_func(level1_b,s1);
      nested_two_proc(level1_b);
    end;


 var
  local_b: byte;
 begin
   func_s64bit_mixed_nested := RESULT_S64BIT;
   local_b:=b;
   global_u8bit := nested_one_func(local_b, RESULT_BIGSTRING);
 end;

function func_s32real_mixed_nested(b: byte) : single;pascal;

    procedure nested_one_proc(l: longint);
     begin
       global_u16bit := b;
       global_s32bit := l;
     end;

    procedure nested_two_proc(l : longint);
     begin
       global_s64bit := l;
     end;



   function nested_one_func(level1_b : byte; s: shortstring): byte;
     var
      s1 : shortstring;

      function nested_two_func(level2_b : byte; s :shortstring): byte;
        begin
          nested_two_func:=level2_b;
          global_bigstring := s;
          nested_one_proc(RESULT_S32BIT);
        end;

    begin
      s1:=s;
      nested_one_func := nested_two_func(level1_b,s1);
      nested_two_proc(level1_b);
    end;


 var
  local_b: byte;
 begin
   func_s32real_mixed_nested := RESULT_S32REAL;
   local_b:=b;
   global_u8bit := nested_one_func(local_b, RESULT_BIGSTRING);
 end;

function func_s64real_mixed_nested(b: byte) : double;pascal;

    procedure nested_one_proc(l: longint);
     begin
       global_u16bit := b;
       global_s32bit := l;
     end;

    procedure nested_two_proc(l : longint);
     begin
       global_s64bit := l;
     end;



   function nested_one_func(level1_b : byte; s: shortstring): byte;
     var
      s1 : shortstring;

      function nested_two_func(level2_b : byte; s :shortstring): byte;
        begin
          nested_two_func:=level2_b;
          global_bigstring := s;
          nested_one_proc(RESULT_S32BIT);
        end;

    begin
      s1:=s;
      nested_one_func := nested_two_func(level1_b,s1);
      nested_two_proc(level1_b);
    end;


 var
  local_b: byte;
 begin
   func_s64real_mixed_nested := RESULT_S64REAl;
   local_b:=b;
   global_u8bit := nested_one_func(local_b, RESULT_BIGSTRING);
 end;

function func_ansistring_mixed_nested(b: byte) : ansistring;pascal;

    procedure nested_one_proc(l: longint);
     begin
       global_u16bit := b;
       global_s32bit := l;
     end;

    procedure nested_two_proc(l : longint);
     begin
       global_s64bit := l;
     end;



   function nested_one_func(level1_b : byte; s: shortstring): byte;
     var
      s1 : shortstring;

      function nested_two_func(level2_b : byte; s :shortstring): byte;
        begin
          nested_two_func:=level2_b;
          global_bigstring := s;
          nested_one_proc(RESULT_S32BIT);
        end;

    begin
      s1:=s;
      nested_one_func := nested_two_func(level1_b,s1);
      nested_two_proc(level1_b);
    end;


 var
  local_b: byte;
 begin
   func_ansistring_mixed_nested := RESULT_BIGSTRING;
   local_b:=b;
   global_u8bit := nested_one_func(local_b, RESULT_BIGSTRING);
 end;

function func_pchar_mixed_nested(b: byte) : pchar;pascal;

    procedure nested_one_proc(l: longint);
     begin
       global_u16bit := b;
       global_s32bit := l;
     end;

    procedure nested_two_proc(l : longint);
     begin
       global_s64bit := l;
     end;



   function nested_one_func(level1_b : byte; s: shortstring): byte;
     var
      s1 : shortstring;

      function nested_two_func(level2_b : byte; s :shortstring): byte;
        begin
          nested_two_func:=level2_b;
          global_bigstring := s;
          nested_one_proc(RESULT_S32BIT);
        end;

    begin
      s1:=s;
      nested_one_func := nested_two_func(level1_b,s1);
      nested_two_proc(level1_b);
    end;


 var
  local_b: byte;
 begin
   func_pchar_mixed_nested := RESULT_PCHAR;
   local_b:=b;
   global_u8bit := nested_one_func(local_b, RESULT_BIGSTRING);
 end;


var
 failed: boolean;
Begin
 {************************************* SIMPLE TESTS ***********************************}
 write('Testing function results (LOC_REFERENCE)...');

 clear_globals;
 clear_values;
 failed := false;

 value_smallarray := func_array;
 if (value_smallarray[1] <> RESULT_U8BIT) or (value_smallarray[SMALL_INDEX] <> RESULT_U8BIT) then
   failed := true;

 clear_globals;
 clear_values;
 value_largerec := func_largerecord;
 if (value_largerec.b[1] <> RESULT_U8BIT) or (value_largerec.b[BIG_INDEX] <> RESULT_U8BIT) then
    failed:=true;

 clear_globals;
 clear_values;
 value_bigstring := func_shortstring;
 if value_bigstring <> RESULT_BIGSTRING then
   failed := true;

 clear_globals;
 clear_values;
 value_largeset := func_largeset;
 if not ('I' in value_largeset) then
   failed := true;

 if failed then
   fail
 else
   WriteLn('Passed!');

 write('Testing orddef/enumdef function results (LOC_REGISTER)...');

 clear_globals;
 clear_values;
 failed := false;


 value_u8bit := func_u8bit;
 if value_u8bit <> RESULT_U8BIT then
     failed := true;

 clear_globals;
 clear_values;
 value_u16bit := func_u16bit;
 if value_u16bit <> RESULT_U16BIT then
     failed := true;

 clear_globals;
 clear_values;
 value_s32bit := func_s32bit;
 if value_s32bit <> RESULT_S32BIT then
     failed := true;

 clear_globals;
 clear_values;
 value_s64bit := func_s64bit;
 if value_s64bit <> RESULT_S64BIT then
    failed := true;

 if failed then
   fail
 else
   WriteLn('Passed!');


 write('Testing floatdef function results...');

 clear_globals;
 clear_values;
 failed := false;

 clear_globals;
 clear_values;
 value_s32real := func_s32real;
 if trunc(value_s32real) <> trunc(RESULT_S32REAL) then
   failed:=true;

 clear_globals;
 clear_values;
 value_s64real := func_s64real;
 if trunc(value_s64real) <> trunc(RESULT_S64REAL) then
   failed:=true;

 if failed then
   fail
 else
   WriteLn('Passed!');

 write('Testing ansistring function result...');

 clear_globals;
 clear_values;
 failed := false;


value_ansistring := func_ansistring;
if value_ansistring <> RESULT_BIGSTRING then
   failed:=true;

 if failed then
   fail
 else
   WriteLn('Passed!');

 write('Testing pointer function result (LOC_REGISTER)...');

 clear_globals;
 clear_values;
 failed := false;

 value_ptr := func_pchar;
 if value_ptr <> RESULT_PCHAR then
    failed := true;

 if failed then
   fail
 else
   WriteLn('Passed!');

 {*********************************** TESTS W/PARAMS ***********************************}
 write('Testing function results with parameter (LOC_REFERENCE)...');

 clear_globals;
 clear_values;
 failed := false;

 value_smallarray := func_array_mixed(RESULT_U8BIT);
 if (value_smallarray[1] <> RESULT_U8BIT) or (value_smallarray[SMALL_INDEX] <> RESULT_U8BIT) then
   failed := true;
 if global_u8bit <> RESULT_U8BIT then
   failed := true;

 clear_globals;
 clear_values;
 value_largerec := func_largerecord_mixed(RESULT_U8BIT);
 if (value_largerec.b[1] <> RESULT_U8BIT) or (value_largerec.b[BIG_INDEX] <> RESULT_U8BIT) then
    failed:=true;
 if global_u8bit <> RESULT_U8BIT then
   failed := true;

 clear_globals;
 clear_values;
 value_bigstring := func_shortstring_mixed(RESULT_U8BIT);
 if value_bigstring <> RESULT_BIGSTRING then
   failed := true;
 if global_u8bit <> RESULT_U8BIT then
   failed := true;

 clear_globals;
 clear_values;
 value_largeset := func_largeset_mixed(RESULT_U8BIT);
 if not ('I' in value_largeset) then
   failed := true;
 if global_u8bit <> RESULT_U8BIT then
   failed := true;

 if failed then
   fail
 else
   WriteLn('Passed!');

 write('Testing orddef/enumdef function results with parameter (LOC_REGISTER)...');

 clear_globals;
 clear_values;
 failed := false;


 value_u8bit := func_u8bit_mixed(RESULT_U8BIT);
 if value_u8bit <> RESULT_U8BIT then
     failed := true;
 if global_u8bit <> RESULT_U8BIT then
   failed := true;

 clear_globals;
 clear_values;
 value_u16bit := func_u16bit_mixed(RESULT_U8BIT);
 if value_u16bit <> RESULT_U16BIT then
     failed := true;
 if global_u8bit <> RESULT_U8BIT then
   failed := true;

 clear_globals;
 clear_values;
 value_s32bit := func_s32bit_mixed(RESULT_U8BIT);
 if value_s32bit <> RESULT_S32BIT then
     failed := true;
 if global_u8bit <> RESULT_U8BIT then
   failed := true;

 clear_globals;
 clear_values;
 value_s64bit := func_s64bit_mixed(RESULT_U8BIT);
 if value_s64bit <> RESULT_S64BIT then
    failed := true;
 if global_u8bit <> RESULT_U8BIT then
   failed := true;

 if failed then
   fail
 else
   WriteLn('Passed!');


 write('Testing floatdef function results with parameter...');

 clear_globals;
 clear_values;
 failed := false;

 value_s32real := func_s32real_mixed(RESULT_U8BIT);
 if trunc(value_s32real) <> trunc(RESULT_S32REAL) then
   failed:=true;
 if global_u8bit <> RESULT_U8BIT then
   failed := true;

 clear_globals;
 clear_values;
 value_s64real := func_s64real_mixed(RESULT_U8BIT);
 if trunc(value_s64real) <> trunc(RESULT_S64REAL) then
   failed:=true;
 if global_u8bit <> RESULT_U8BIT then
   failed := true;

 if failed then
   fail
 else
   WriteLn('Passed!');

 write('Testing ansistring function result with parameter...');

 clear_globals;
 clear_values;
 failed := false;


 value_ansistring := func_ansistring_mixed(RESULT_U8BIT);
 if value_ansistring <> RESULT_BIGSTRING then
   failed:=true;
 if global_u8bit <> RESULT_U8BIT then
   failed := true;

 if failed then
   fail
 else
   WriteLn('Passed!');

 write('Testing pointer function result with parameter (LOC_REGISTER)...');

 clear_globals;
 clear_values;
 failed := false;

 value_ptr := func_pchar_mixed(RESULT_U8BIT);
 if value_ptr <> RESULT_PCHAR then
    failed := true;
 if global_u8bit <> RESULT_U8BIT then
    failed := true;

 if failed then
   fail
 else
   WriteLn('Passed!');

 {******************************NESTED TESTS W/PARAMS **********************************}
 write('Testing function (w/nesting) results with parameter (LOC_REFERENCE)...');

 clear_globals;
 clear_values;
 failed := false;

 value_smallarray := func_array_mixed_nested(RESULT_U8BIT);
 if (value_smallarray[1] <> RESULT_U8BIT) or (value_smallarray[SMALL_INDEX] <> RESULT_U8BIT) then
   failed := true;
 if global_u8bit <> RESULT_U8BIT then
   failed := true;
 if global_bigstring <> RESULT_BIGSTRING then
   failed := true;
 if global_u16bit <> RESULT_U8BIT then
   failed := true;
 if global_s32bit <> RESULT_S32BIT then
   failed := true;
 if global_s64bit <> RESULT_U8BIT then
   failed := true;

 clear_globals;
 clear_values;
 value_largerec := func_largerecord_mixed_nested(RESULT_U8BIT);
 if (value_largerec.b[1] <> RESULT_U8BIT) or (value_largerec.b[BIG_INDEX] <> RESULT_U8BIT) then
    failed:=true;
 if global_u8bit <> RESULT_U8BIT then
   failed := true;
 if global_bigstring <> RESULT_BIGSTRING then
   failed := true;
 if global_u16bit <> RESULT_U8BIT then
   failed := true;
 if global_s64bit <> RESULT_U8BIT then
   failed := true;
 if global_s32bit <> RESULT_S32BIT then
   failed := true;

 clear_globals;
 clear_values;
 value_bigstring := func_shortstring_mixed_nested(RESULT_U8BIT);
 if value_bigstring <> RESULT_BIGSTRING then
   failed := true;
 if global_u8bit <> RESULT_U8BIT then
   failed := true;
 if global_bigstring <> RESULT_BIGSTRING then
   failed := true;
 if global_u16bit <> RESULT_U8BIT then
   failed := true;
 if global_s32bit <> RESULT_S32BIT then
   failed := true;
 if global_s64bit <> RESULT_U8BIT then
   failed := true;

 clear_globals;
 clear_values;
 value_largeset := func_largeset_mixed_nested(RESULT_U8BIT);
 if not ('I' in value_largeset) then
   failed := true;
 if global_u8bit <> RESULT_U8BIT then
   failed := true;
 if global_bigstring <> RESULT_BIGSTRING then
   failed := true;
 if global_u16bit <> RESULT_U8BIT then
   failed := true;
 if global_s32bit <> RESULT_S32BIT then
   failed := true;
 if global_s64bit <> RESULT_U8BIT then
   failed := true;

 if failed then
   fail
 else
   WriteLn('Passed!');

 write('Testing orddef/enumdef function (w/nesting) results with parameter (LOC_REGISTER)...');

 clear_globals;
 clear_values;
 failed := false;


 value_u8bit := func_u8bit_mixed_nested(RESULT_U8BIT);
 if value_u8bit <> RESULT_U8BIT then
     failed := true;
 if global_u8bit <> RESULT_U8BIT then
   failed := true;
 if global_bigstring <> RESULT_BIGSTRING then
   failed := true;
 if global_u16bit <> RESULT_U8BIT then
   failed := true;
 if global_s32bit <> RESULT_S32BIT then
   failed := true;
 if global_s64bit <> RESULT_U8BIT then
   failed := true;

 clear_globals;
 clear_values;
 value_u16bit := func_u16bit_mixed_nested(RESULT_U8BIT);
 if value_u16bit <> RESULT_U16BIT then
     failed := true;
 if global_u8bit <> RESULT_U8BIT then
   failed := true;
 if global_bigstring <> RESULT_BIGSTRING then
   failed := true;
 if global_u16bit <> RESULT_U8BIT then
   failed := true;
 if global_s64bit <> RESULT_U8BIT then
   failed := true;
 if global_s32bit <> RESULT_S32BIT then
   failed := true;

 clear_globals;
 clear_values;
 value_s32bit := func_s32bit_mixed_nested(RESULT_U8BIT);
 if value_s32bit <> RESULT_S32BIT then
     failed := true;
 if global_u8bit <> RESULT_U8BIT then
   failed := true;
 if global_bigstring <> RESULT_BIGSTRING then
   failed := true;
 if global_u16bit <> RESULT_U8BIT then
   failed := true;
 if global_s64bit <> RESULT_U8BIT then
   failed := true;
 if global_s32bit <> RESULT_S32BIT then
   failed := true;

 clear_globals;
 clear_values;
 value_s64bit := func_s64bit_mixed_nested(RESULT_U8BIT);
 if value_s64bit <> RESULT_S64BIT then
    failed := true;
 if global_u8bit <> RESULT_U8BIT then
   failed := true;
 if global_bigstring <> RESULT_BIGSTRING then
   failed := true;
 if global_u16bit <> RESULT_U8BIT then
   failed := true;
 if global_s64bit <> RESULT_U8BIT then
   failed := true;
 if global_s32bit <> RESULT_S32BIT then
   failed := true;

 if failed then
   fail
 else
   WriteLn('Passed!');


 write('Testing floatdef function (w/nesting) results with parameter...');

 clear_globals;
 clear_values;
 failed := false;

 value_s32real := func_s32real_mixed_nested(RESULT_U8BIT);
 if trunc(value_s32real) <> trunc(RESULT_S32REAL) then
   failed:=true;
 if global_u8bit <> RESULT_U8BIT then
   failed := true;
 if global_bigstring <> RESULT_BIGSTRING then
   failed := true;
 if global_u16bit <> RESULT_U8BIT then
   failed := true;
 if global_s64bit <> RESULT_U8BIT then
   failed := true;
 if global_s32bit <> RESULT_S32BIT then
   failed := true;

 clear_globals;
 clear_values;
 value_s64real := func_s64real_mixed_nested(RESULT_U8BIT);
 if trunc(value_s64real) <> trunc(RESULT_S64REAL) then
   failed:=true;
 if global_u8bit <> RESULT_U8BIT then
   failed := true;
 if global_bigstring <> RESULT_BIGSTRING then
   failed := true;
 if global_u16bit <> RESULT_U8BIT then
   failed := true;
 if global_s64bit <> RESULT_U8BIT then
   failed := true;
 if global_s32bit <> RESULT_S32BIT then
   failed := true;

 if failed then
   fail
 else
   WriteLn('Passed!');

 write('Testing ansistring function (w/nesting) result with parameter...');

 clear_globals;
 clear_values;
 failed := false;


 value_ansistring := func_ansistring_mixed_nested(RESULT_U8BIT);
 if value_ansistring <> RESULT_BIGSTRING then
   failed:=true;
 if global_u8bit <> RESULT_U8BIT then
   failed := true;
 if global_bigstring <> RESULT_BIGSTRING then
   failed := true;
 if global_u16bit <> RESULT_U8BIT then
   failed := true;
 if global_s64bit <> RESULT_U8BIT then
   failed := true;
 if global_s32bit <> RESULT_S32BIT then
   failed := true;

 if failed then
   fail
 else
   WriteLn('Passed!');

 write('Testing pointer function (w/nesting) result with parameter (LOC_REGISTER)...');

 clear_globals;
 clear_values;
 failed := false;

 value_ptr := func_pchar_mixed_nested(RESULT_U8BIT);
 if value_ptr <> RESULT_PCHAR then
    failed := true;
 if global_u8bit <> RESULT_U8BIT then
    failed := true;
 if global_bigstring <> RESULT_BIGSTRING then
   failed := true;
 if global_u16bit <> RESULT_U8BIT then
   failed := true;
 if global_s64bit <> RESULT_U8BIT then
   failed := true;
 if global_s32bit <> RESULT_S32BIT then
   failed := true;

 if failed then
   fail
 else
   WriteLn('Passed!');
end.
