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
{          (var   parameters)                                    }
{****************************************************************}
program tparan3;
{$mode objfpc}
{$INLINE ON}
{$P-}
{$V+}



 { REAL should map to single or double }
 { so it is not checked, since single  }
 { double nodes are checked.           }

 { assumes that enumdef is the same as orddef (same storage format) }

 const
{ should be defined depending on CPU target }
{$ifdef cpu68k}
  BIG_INDEX = 8000;
  SMALL_INDEX  = 13;
{$endif}
{$ifdef cpui386}
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
{      halt(1);}
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

  {************************************************************************}
  {                           VAR   PARAMETERS                             }
  {************************************************************************}
  procedure proc_var_s32bit(var v : longint);
   begin
     v:=RESULT_S32BIT;
   end;
   
  procedure proc_var_s64bit(var v: int64);
   begin
     v:=RESULT_S64BIT;
   end;


  procedure proc_var_u8bit(var v: byte);
   begin
     v:=RESULT_U8BIT;
   end;
   
  procedure proc_var_smallrecord(var smallrec : tsmallrecord);
   begin
     smallrec.b := RESULT_U8BIT;
     smallrec.w := RESULT_U16BIT;
   end;


  procedure proc_var_largerecord(var largerec : tlargerecord);
   begin
     largerec.b[1] := RESULT_U8BIT;
     largerec.b[2] := RESULT_U8BIT;
   end;


  procedure proc_var_smallset(var smallset : tsmallset);
   begin
     smallset := [A_A,A_D];
   end;
   
   
  procedure proc_var_largeset(var largeset : tlargeset);
   begin
     largeset:= largeset + ['I'];
   end;
   

  procedure proc_var_smallstring(var s:tsmallstring);
   begin
     s:=RESULT_SMALLSTRING;
   end;
    

  procedure proc_var_bigstring(var s:shortstring);
   begin
     s:=RESULT_BIGSTRING;
   end;
   

  procedure proc_var_openstring(var s: OpenString);
   begin
    global_u8bit := high(s);
    s:=RESULT_SMALLSTRING;
   end;
   
  procedure proc_const_smallarray(const arr : tsmallarray);
  begin
    if arr[SMALL_INDEX] = RESULT_U8BIT then
      global_u8bit := RESULT_U8BIT;
  end;
  
  procedure proc_const_smallarray_open(const arr : array of byte);
  begin
    { form 0 to N-1 indexes in open arrays }
    if arr[SMALL_INDEX-1] = RESULT_U8BIT then
      global_u8bit := RESULT_U8BIT;
  end;
  
  
  procedure proc_const_smallarray_const_1(const arr : array of const);
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
  

  procedure proc_const_smallarray_const_2(const arr : array of const);
  var
   i: integer;
  begin
     if high(arr)<0 then
       global_u8bit := RESULT_U8BIT;
  end;
   

  procedure proc_var_formaldef_array(var buf);
  var
   p: ^byte;
  begin
    { array is indexed from 1 }
    p := @buf;
    p[SMALL_INDEX-1] := RESULT_U8BIT;
    p[0] := RESULT_U8BIT;
  end;


procedure proc_var_formaldef_string(var buf);
  var
   p: ^byte;
  begin
    { array is indexed from 1 }
    p := @buf;
    p[SMALL_INDEX-1] := RESULT_U8BIT;
    p[0] := RESULT_U8BIT;
  end;
   
  {************************************************************************}
  {                      CONST PARAMETERS (INLINE)                         }
  {************************************************************************}

  procedure proc_var_smallrecord_inline(var smallrec : tsmallrecord);inline;
   begin
     smallrec.b := RESULT_U8BIT;
     smallrec.w := RESULT_U16BIT;
   end;


  procedure proc_var_largerecord_inline(var largerec : tlargerecord);inline;
   begin
     largerec.b[1] := RESULT_U8BIT;
     largerec.b[2] := RESULT_U8BIT;
   end;

  procedure proc_const_smallset_inline(const smallset : tsmallset);inline;
   begin
     if A_D in smallset then
       global_u8bit := RESULT_U8BIT;
   end;
   
   
  procedure proc_const_largeset_inline(const largeset : tlargeset);inline;
   begin
     if 'I' in largeset then
       global_u8bit := RESULT_U8BIT;
   end;
   

  procedure proc_const_smallstring_inline(const s:tsmallstring);inline;
   begin
     if s = RESULT_SMALLSTRING then
       global_u8bit := RESULT_u8BIT;
   end;
    

  procedure proc_const_bigstring_inline(const s:shortstring);inline;
   begin
     if s = RESULT_BIGSTRING then
       global_u8bit := RESULT_u8BIT;
   end;
   

  procedure proc_const_smallarray_inline(const arr : tsmallarray);inline;
  begin
    if arr[SMALL_INDEX] = RESULT_U8BIT then
      global_u8bit := RESULT_U8BIT;
  end;
  
  procedure proc_const_smallarray_open_inline(const arr : array of byte);inline;
  begin
    { form 0 to N-1 indexes in open arrays }
    if arr[SMALL_INDEX-1] = RESULT_U8BIT then
      global_u8bit := RESULT_U8BIT;
  end;
  
  
  procedure proc_const_smallarray_const_1_inline(const arr : array of const);inline;
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
  

  procedure proc_const_smallarray_const_2_inline(const arr : array of const);inline;
  var
   i: integer;
  begin
     if high(arr)<0 then
       global_u8bit := RESULT_U8BIT;
  end;


  procedure proc_const_formaldef_array_inline(const buf);inline;
  var
   p: ^byte;
  begin
    { array is indexed from 1 }
    p := @buf;
    global_u8bit := p[SMALL_INDEX-1];
  end;

var
  failed: boolean;
  pp : ^pchar;
begin
  {************************************************************************}
  {                           CONST  PARAMETERS                            }
  {************************************************************************}
  clear_globals;
  clear_values;
  failed:=false;
  
  write('Var parameter test (src : LOC_REFERENCE (orddef)))...');
  proc_var_s32bit(global_s32bit);
  if global_s32bit <> RESULT_S32BIT then
    failed:=true;

  clear_globals;
  clear_values;
  proc_var_s64bit(global_s64bit);
  if global_s64bit <> RESULT_S64BIT then
    failed:=true;

  clear_globals;
  clear_values;
  proc_var_u8bit(global_u8bit);
  if global_u8bit <> RESULT_U8BIT then
    failed:=true;


  if failed then
    fail
  else
    WriteLn('Passed!');
  
  write('Var parameter test (src : LOC_REFERENCE (recorddef)))...');
  clear_globals;
  clear_values;
  failed := false;

  proc_var_smallrecord(value_smallrec);
  if (value_smallrec.b <> RESULT_U8BIT) or (value_smallrec.w <> RESULT_U16BIT) then
    failed := true;
    
  clear_globals;
  clear_values;
  proc_var_largerecord(value_largerec);
  if (value_largerec.b[1] <> RESULT_U8BIT) or (value_largerec.b[2] <> RESULT_U8BIT) then
    failed := true;
    
  if failed then
    fail
  else
    WriteLn('Passed!');

  write('(Inline) var parameter test (src : LOC_REFERENCE (recorddef)))...');
  clear_globals;
  clear_values;
  failed := false;

  proc_var_smallrecord_inline(value_smallrec);
  if (value_smallrec.b <> RESULT_U8BIT) or (value_smallrec.w <> RESULT_U16BIT) then
    failed := true;
 
  clear_globals;
  clear_values;
  proc_var_largerecord_inline(value_largerec);
  if (value_largerec.b[1] <> RESULT_U8BIT) or (value_largerec.b[2] <> RESULT_U8BIT) then
    failed := true;
    
  if failed then
    fail
  else
    WriteLn('Passed!');
    
  write('var parameter test (src : LOC_REFERENCE (setdef)))...');
  clear_globals;
  clear_values;
  failed := false;
  
  proc_var_smallset(value_smallset);
  if (not (A_A in value_smallset)) or (not (A_D in value_smallset)) then
    failed := true;
    
  clear_globals;
  clear_values;
  proc_var_largeset(value_largeset);
  if not ('I' in value_largeset) then
    failed := true;
    
  if failed then
    fail
  else
    WriteLn('Passed!');

(*    

  write('(Inline) const parameter test (src : LOC_REFERENCE (setdef)))...');
  clear_globals;
  clear_values;
  failed := false;
  value_smallset := [A_A,A_D];

  proc_const_smallset_inline(value_smallset);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;
    
  clear_globals;
  clear_values;
  value_largeset := ['I'];
  proc_const_largeset_inline(value_largeset);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;
    
  if failed then
    fail
  else
    WriteLn('Passed!');
*)    


  write('var parameter test (src : LOC_REFERENCE (stringdef)))...');
  clear_globals;
  clear_values;
  failed := false;
  proc_var_smallstring(value_smallstring);
  if value_smallstring <> RESULT_SMALLSTRING then
    failed := true;

  clear_globals;
  clear_values;
  proc_var_bigstring(value_bigstring);
  if value_bigstring <> RESULT_BIGSTRING then
    failed := true;

  clear_globals;
  clear_values;
  proc_var_openstring(value_smallstring);
  if (value_smallstring <> RESULT_SMALLSTRING) or (global_u8bit <> high(value_smallstring)) then
    failed := true;

    
  if failed then
    fail
  else
    WriteLn('Passed!');

(*
  clear_globals;
  clear_values;
  write('(Inline) const parameter test (src : LOC_REFERENCE (stringdef)))...');
  failed := false;
  value_smallstring := RESULT_SMALLSTRING;

  proc_const_smallstring_inline(value_smallstring);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;
    
  clear_globals;
  clear_values;
  value_bigstring := RESULT_BIGSTRING;
  proc_const_bigstring_inline(value_bigstring);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;
    
  if failed then
    fail
  else
    WriteLn('Passed!');
*)    
    
  write('Var parameter test (src : LOC_REFERENCE (formaldef)))...');
  clear_globals;
  clear_values;
  failed:=false;
  
  proc_var_formaldef_array(value_smallarray);
  if (value_smallarray[SMALL_INDEX] <> RESULT_U8BIT) or (value_smallarray[1] <> RESULT_U8BIT) then
    failed := true;


  if failed then
    fail
  else
    WriteLn('Passed!');

(*  

  write('(Inline) Const parameter test (src : LOC_REFERENCE (formaldef)))...');
  clear_globals;
  clear_values;
  failed:=false;
  value_smallarray[SMALL_INDEX] := RESULT_U8BIT;
  proc_const_formaldef_array_inline(value_smallarray);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;


  if failed then
    fail
  else
    WriteLn('Passed!');
    
  write('Var parameter test (src : LOC_REFERENCE (arraydef)))...');
  
  clear_globals;
  clear_values;
  failed:=false;
  
  value_smallarray[SMALL_INDEX] := RESULT_U8BIT;
  proc_const_smallarray(value_smallarray);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;

  clear_globals;
  clear_values;

  value_smallarray[SMALL_INDEX] := RESULT_U8BIT;
  proc_const_smallarray_open(value_smallarray);
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
  proc_const_smallarray_const_1([value_u8bit,value_ptr,value_s64bit,value_char,value_smallstring,value_s64real,value_boolean,value_class]);
  
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
  proc_const_smallarray_const_2([]);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;


  if failed then
    fail
  else
    WriteLn('Passed!');
    

  write('Inline const parameter test (src : LOC_REFERENCE (arraydef)))...');
  
  clear_globals;
  clear_values;
  failed:=false;
  
  value_smallarray[SMALL_INDEX] := RESULT_U8BIT;
  proc_const_smallarray_inline(value_smallarray);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;

  clear_globals;
  clear_values;

  value_smallarray[SMALL_INDEX] := RESULT_U8BIT;
  proc_const_smallarray_open_inline(value_smallarray);
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
  proc_const_smallarray_const_1_inline([value_u8bit,value_ptr,value_s64bit,value_char,value_smallstring,value_s64real,value_boolean,value_class]);
  
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
  proc_const_smallarray_const_2_inline([]);
  if global_u8bit <> RESULT_U8BIT then
    failed := true;


  if failed then
    fail
  else
    WriteLn('Passed!');
*)    
end.


{
  $Log$
  Revision 1.1  2002-04-01 19:57:34  carl
  + var parameter code generator testing (also crashes under version 1.0.x!)

}  