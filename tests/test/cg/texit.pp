{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{  By Carl Eric Codere                                           }
{****************************************************************}
{ NODE TESTED : secondexitn()                                    }
{****************************************************************}
{ PRE-REQUISITES: secondload()                                   }
{                 secondassign()                                 }
{                 secondtypeconv()                               }
{                 secondcalln()                                  }
{                 secondin()                                     }
{****************************************************************}
{ DEFINES:                                                       }
{            FPC     = Target is FreePascal compiler             }
{****************************************************************}
{ REMARKS:                                                       }
{                                                                }
{                                                                }
{                                                                }
{****************************************************************}

procedure fail;
begin
  WriteLn('Failure.');
  halt(1);
end;


function simple_exit : longint;
 var
  z : longint;
 begin
  z:=0;
  exit;
  z:=12;
 end;


const
  RET_S64BIT = $100000;
  RET_S32BIT = -123456;
  RET_U32BIT = 2000000;
  RET_S16BIT = -30000;
  RET_U16BIT = $5555;
  RET_S8BIT  = -80;
  RET_U8BIT = $AA;
  RET_SINGLE = 57689.15;
  RET_DOUBLE = 100012.345;
  PCHAR_STRING: pchar = 'HELLO STRING';

type
  t24bitarray = packed array[1..3] of byte;

var
 global_var : longint;


 function getu8bit : byte;
  begin
    getu8bit := RET_U8BIT;
  end;

 function gets8bit : shortint;
  begin
    gets8bit := RET_S8BIT;
  end;


 function getu16bit : word;
  begin
    getu16bit := RET_U16BIT;
  end;

 function gets16bit : smallint;
  begin
    gets16bit := RET_S16BIT;
  end;


 function getu32bit : cardinal;
  begin
    getu32bit := RET_U32BIT;
  end;

 function gets32bit : longint;
  begin
    gets32bit := RET_S32BIT;
  end;


  function gets64bit : int64;
   begin
     gets64bit := RET_S64BIT;
   end;


   function gets32real : single;
    begin
     gets32real := RET_SINGLE;
    end;


   function gets64real : double;
    begin
     gets64real := RET_DOUBLE;
    end;

   function getpchar: pchar;
    begin
     getpchar := PCHAR_STRING;
    end;


function exit_loc_mem_ordinal_1 : longint;
 var
  z : longint;
 begin
  global_var:=0;
  global_var:=RET_S32BIT;
  exit(global_var);
  global_var:=RET_S32BIT;
 end;


function exit_loc_reg_pointerdef  : pchar;
 begin
  exit(getpchar);
 end;

function exit_loc_ref_pointerdef  : pchar;
 var
  p: pchar;
 begin
  p:=PCHAR_STRING;
  exit(p);
 end;

function exit_loc_reg_ordinal_s64bit  : int64;
 begin
  exit(gets64bit);
 end;

function exit_loc_reg_ordinal_s32bit  :longint;
 begin
  exit(gets32bit);
 end;

function exit_loc_reg_ordinal_u32bit  :cardinal;
 begin
  exit(getu32bit);
 end;

function exit_loc_reg_ordinal_s16bit  : smallint;
 begin
  exit(gets16bit);
 end;

function exit_loc_reg_ordinal_u16bit  :word;
 begin
  exit(getu16bit)
 end;

function exit_loc_reg_ordinal_s8bit  : shortint;
 begin
  exit(gets8bit);
 end;

function exit_loc_reg_ordinal_u8bit  : byte;
 begin
   exit(getu8bit);
 end;




function exit_loc_ref_constant : word;
 begin
  exit(RET_U16BIT);
 end;



function exit_loc_ref_ordinal_s64bit  : int64;
 var
   s : int64;
 begin
  s := RET_S64BIT;
  exit(s);
 end;

function exit_loc_ref_ordinal_s32bit  :longint;
 var
   s : longint;
 begin
  s := RET_S32BIT;
  exit(s);
 end;

function exit_loc_ref_ordinal_u32bit  :cardinal;
 var
  c: cardinal;
 begin
  c := RET_U32BIT;
  exit(c);
 end;

function exit_loc_ref_ordinal_s16bit  : smallint;
 var
  s : smallint;
 begin
  s := RET_S16BIT;
  exit(s);
 end;

function exit_loc_ref_ordinal_u16bit  :word;
 var
  w: word;
 begin
  w := RET_U16BIT;
  exit(w);
 end;

function exit_loc_ref_ordinal_s8bit  : shortint;
 var
  s : shortint;
 begin
  s := RET_S8BIT;
  exit(s);
 end;

function exit_loc_ref_ordinal_u8bit  : byte;
 var
  b: byte;
 begin
   b:=RET_U8BIT;
   exit(b);
 end;


 function exit_loc_ref_ordinal_24bit : t24bitarray;
  var
   r : t24bitarray;
  begin
    r[1]:=12;
    r[2]:=13;
    r[3]:=14;
    exit(r);
  end;



function exit_loc_ref_float_s32real : single;
 var
  s: single;
 begin
   s:=RET_SINGLE;
   exit(s);
 end;


function exit_loc_ref_float_s64real : double;
 var
  s: double;
 begin
   s:=RET_DOUBLE;
   exit(s);
 end;


function exit_loc_reg_float_s32real : single;
 begin
   exit(gets32real);
 end;


function exit_loc_reg_float_s64real : double;
 begin
   exit(gets64real);
 end;


function exit_loc_flags : boolean;
 var
   c: char;
 begin
  c:='A';
  exit(c in ['a'..'z']);
 end;

function exit_loc_jump : boolean;
 var
   b,c: boolean;
 begin
  b:=TRUE;
  c:=FALSE;
  exit(b and c);
 end;


 function exit_loc_ansi(w: word) : ansistring;
  var d: ansistring;
 begin
   str(w,d);
   exit(d);
 end;



var
 failed : boolean;
 array_24bits : t24bitarray;
Begin

 { simple exit }
 write('Testing secondexitn() simple case...');
 failed := false;
 simple_exit;
 if failed then
   fail
 else
   writeln('Passed!');

 write('Testing secondexitn() with reference (simple case)...');
 failed := false;
 array_24bits := exit_loc_ref_ordinal_24bit;
 if (array_24bits[1]<>12) or (array_24bits[2]<>13) or (array_24bits[3]<>14) then
   failed := true;
 if failed then
   fail
 else
   writeln('Passed!');

 write('secondexitn() LOC_CONSTANT case...');
 failed := false;
 if exit_loc_ref_constant <> RET_U16BIT then
   failed := true;
 if failed then
   fail
 else
   writeln('Passed!');



 write('secondexitn() LOC_MEM case...');
 failed := false;
 if exit_loc_mem_ordinal_1 <> RET_S32BIT then
   failed := true;
 if failed then
   fail
 else
   writeln('Passed!');

 writeln('Testing secondexitn() LOC_REFERENCE case...');

 write('    ordinal/enumdef return value...');
 failed := false;
 if exit_loc_ref_ordinal_s64bit <> RET_S64BIT then
   failed := true;
 if exit_loc_ref_ordinal_s32bit <> RET_S32BIT then
   failed := true;
 if exit_loc_ref_ordinal_u32bit <> RET_U32BIT then
   failed := true;
 if exit_loc_ref_ordinal_s16bit <> RET_S16BIT then
   failed := true;
 if exit_loc_ref_ordinal_u16bit <> RET_U16BIT then
   failed := true;
 if exit_loc_ref_ordinal_s8bit <> RET_S8BIT then
   failed := true;
 if exit_loc_ref_ordinal_u8bit <> RET_U8BIT then
   failed := true;
 if failed then
   fail
 else
   writeln('Passed!');

 write('    floating point return value...');
 failed := false;
 if (trunc(exit_loc_ref_float_s32real) <>  trunc(RET_SINGLE)) then
  failed := true;
 if trunc(exit_loc_ref_float_s64real) <> trunc(RET_DOUBLE) then
  failed := true;
 if failed then
   fail
 else
   writeln('Passed!');

 { procvardef is not tested since it is the same as pointer return value...}
 write('    pointer/procedure variable return value...');
 failed := false;
 { compare the actual pointer not the values inside the string ! }
 if (exit_loc_ref_pointerdef  <> PCHAR_STRING) then
   failed:=true;
 if failed then
   fail
 else
   writeln('Passed!');



 writeln('Testing secondexitn() LOC_REGISTER case...');

 write('    ordinal/enumdef return value...');
 failed := false;
 if exit_loc_reg_ordinal_s64bit <> RET_S64BIT then
   failed := true;
 if exit_loc_reg_ordinal_s32bit <> RET_S32BIT then
   failed := true;
 if exit_loc_reg_ordinal_u32bit <> RET_U32BIT then
   failed := true;
 if exit_loc_reg_ordinal_s16bit <> RET_S16BIT then
   failed := true;
 if exit_loc_reg_ordinal_u16bit <> RET_U16BIT then
   failed := true;
 if exit_loc_reg_ordinal_s8bit <> RET_S8BIT then
   failed := true;
 if exit_loc_reg_ordinal_u8bit <> RET_U8BIT then
   failed := true;
 if failed then
   fail
 else
   writeln('Passed!');

 write('    floating point return value...');
 failed := false;
 if (trunc(exit_loc_reg_float_s32real) <>  trunc(RET_SINGLE)) then
  failed := true;
 if trunc(exit_loc_reg_float_s64real) <> trunc(RET_DOUBLE) then
  failed := true;
 if failed then
   fail
 else
   writeln('Passed!');

 { procvardef is not tested since it is the same as pointer return value...}
 write('    pointer/procedure variable return value...');
 failed := false;
 { compare the actual pointer not the values inside the string ! }
 if (exit_loc_reg_pointerdef  <> PCHAR_STRING) then
   failed:=true;
 if failed then
   fail
 else
   writeln('Passed!');


 write('Testing secondexitn() LOC_FLAGS case...');
 failed := false;
 { check for false, since having zero in register is rarer   }
 { then having non-zero (just in case everything is corrupt) }
 if exit_loc_flags then
   failed := true;
 if failed then
   fail
 else
   writeln('Passed!');

 write('Testing secondexitn() LOC_JUMP case...');
 failed := false;
 { check for false, since having zero in register is rarer   }
 { then having non-zero (just in case everything is corrupt) }
 if exit_loc_jump then
   failed := true;
 if failed then
   fail
 else
   writeln('Passed!');

 write('Testing secondexitn() ansistring case...');
 failed := false;
 if exit_loc_ansi(10) <> '10' then
   failed := true;
 if failed then
   fail
 else
   writeln('Passed!');

end.
