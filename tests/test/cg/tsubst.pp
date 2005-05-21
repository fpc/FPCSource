{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{****************************************************************}
{ NODE TESTED : secondsubscriptn(), partial secondload()         }
{****************************************************************}
{ PRE-REQUISITES: secondload()                                   }
{                 secondassign()                                 }
{****************************************************************}
{ DEFINES:   VERBOSE = Write test information to screen          }
{            FPC     = Target is FreePascal compiler             }
{****************************************************************}
{ REMARKS:                                                       }
{                                                                }
{                                                                }
{                                                                }
{****************************************************************}
Program tsubst1;
{$mode objfpc}


{$IFNDEF FPC}
type  smallint = integer;
{$ENDIF}
const
 { Should be equal to the maximum offset possible in indirect addressing
   mode with displacement. (CPU SPECIFIC) }

{$ifdef cpu68k}
 MAX_DISP = 32767;
{$else}
 MAX_DISP = 65535;
{$endif}

{ These different alignments are described in the PowerPC ABI
  supplement, they should represent most possible cases.
}
type
tlevel1rec = record
 c: byte;
end;

tlevel2rec = record
 c: byte;
 d: byte;
 s: word;
 n: longint;
end;

tlevel3rec = record
 c: byte;
 s: word;
end;

tlevel4rec = record
 c: byte;
 i : int64;
 s: word;
end;

tlevel5rec = record
 c: byte;
 s: word;
 j: longint;
end;

tlevel1rec_big = record
 fill : array[1..MAX_DISP] of byte;
 c: byte;
end;

tlevel2rec_big = record
 fill : array[1..MAX_DISP] of byte;
 c: byte;
 d: byte;
 s: word;
 n: longint;
end;

tlevel3rec_big = record
 fill : array[1..MAX_DISP] of byte;
 c: byte;
 s: word;
end;

tlevel4rec_big = record
 fill : array[1..MAX_DISP] of byte;
 c: byte;
 i : int64;
 s: word;
end;

tlevel5rec_big = record
 fill : array[1..MAX_DISP] of byte;
 c: byte;
 s: word;
 j: longint;
end;

{ packed record, for testing misaligned access }
tlevel1rec_packed = packed record
 c: byte;
end;

tlevel2rec_packed = packed record
 c: byte;
 d: byte;
 s: word;
 n: longint;
end;

tlevel3rec_packed = packed record
 c: byte;
 s: word;
end;

tlevel4rec_packed = packed record
 c: byte;
 i : int64;
 s: word;
end;

tlevel5rec_packed = packed record
 c: byte;
 s: word;
 j: longint;
end;

tclass1 = class
 fill : array[1..MAX_DISP] of byte;
 c: byte;
 s: word;
 j: longint;
end;

tclass2 = class
 c: byte;
 s: word;
 i: int64;
end;


 { test with global variables }
 const
  RESULT_U8BIT = $55;
  RESULT_U16BIT = $500F;
  RESULT_S32BIT = $500F0000;
  RESULT_S64BIT = $500F0000;





 level1rec : tlevel1rec =
 (
  c: RESULT_U8BIT
 );

 level2rec : tlevel2rec =
 (
   c: RESULT_U8BIT;
   d: RESULT_U8BIT;
   s: RESULT_U16BIT;
   n: RESULT_S32BIT;
 );

 level3rec : tlevel3rec =
 (
  c: RESULT_U8BIT;
  s: RESULT_U16BIT;

 );

 level4rec : tlevel4rec =
 (
  c: RESULT_U8BIT;
  i : RESULT_S64BIT;
  s : RESULT_U16BIT
 );

 level5rec : tlevel5rec =
 (
   c: RESULT_U8BIT;
   s: RESULT_U16BIT;
   j: RESULT_S32BIT;
 );

 level1rec_packed : tlevel1rec_packed =
 (
  c: RESULT_U8BIT
 );

 level2rec_packed : tlevel2rec_packed =
 (
   c: RESULT_U8BIT;
   d: RESULT_U8BIT;
   s: RESULT_U16BIT;
   n: RESULT_S32BIT;
 );

 level3rec_packed : tlevel3rec_packed =
 (
  c: RESULT_U8BIT;
  s: RESULT_U16BIT;
 );

 level4rec_packed : tlevel4rec_packed =
 (
  c: RESULT_U8BIT;
  i : RESULT_S64BIT;
  s : RESULT_U16BIT
 );

 level5rec_packed : tlevel5rec_packed =
 (
   c: RESULT_U8BIT;
   s: RESULT_U16BIT;
   j: RESULT_S32BIT;
 );

    procedure fail;
    begin
      WriteLn('Failure.');
      halt(1);
    end;

var
 c,d: byte;
 s: word;
 n,j: longint;
 i: int64;
 failed : boolean;
 class1 : tclass1;
 class2 : tclass2;

 procedure clear_globals;
  begin
    c:=0;
    d:=0;
    s:=0;
    n:=0;
    j:=0;
    i:=0;
    class1:=nil;
    class2:=nil
  end;


 function getclass : tclass1;
  begin
    getclass := class1;
  end;

 function getclass2: tclass2;
  begin
    getclass2 := class2;
  end;

{$ifndef cpu68k}
 procedure testlocal_big_1;
 var
   local1rec_big : tlevel1rec_big;
  begin
     clear_globals;
     local1rec_big.c := RESULT_U8BIT;
     c:= local1rec_big.c;
     if c <> RESULT_U8BIT then
       failed := true;
  end;


  procedure testlocal_big_2;
   var
    local2rec_big : tlevel2rec_big;
   begin
     clear_globals;
     { setup values - assign }
     local2rec_big.c := RESULT_U8BIT;
     local2rec_big.d := RESULT_U8BIT;
     local2rec_big.s := RESULT_U16BIT;
     local2rec_big.n := RESULT_S32BIT;
     { load values - load }
     c:= local2rec_big.c;
     if c <> RESULT_U8BIT then
       failed := true;
     d:= local2rec_big.d;
     if d <> RESULT_U8BIT then
       failed := true;
     s:= local2rec_big.s;
     if s <> RESULT_U16BIT then
       failed := true;
     n:= local2rec_big.n;
     if n <> RESULT_S32BIT then
       failed := true;
   end;


   procedure testlocal_big_3;
    var
     local3rec_big : tlevel3rec_big;
    begin
     clear_globals;
     { setup values - assign }
     local3rec_big.c := RESULT_U8BIT;
     local3rec_big.s := RESULT_U16BIT;
     c:= local3rec_big.c;
     if c <> RESULT_U8BIT then
       failed := true;
     s:= local3rec_big.s;
     if s <> RESULT_U16BIT then
       failed := true;
    end;

    procedure testlocal_big_4;
    var
     local4rec_big : tlevel4rec_big;
     begin
         clear_globals;
         { setup values - assign }
         local4rec_big.c := RESULT_U8BIT;
         local4rec_big.i := RESULT_S64BIT;
         local4rec_big.s := RESULT_U16BIT;

         c:= local4rec_big.c;
         if c <> RESULT_U8BIT then
           failed := true;
         i:= local4rec_big.i;
         if i <> RESULT_S64BIT then
           failed := true;
         s:= local4rec_big.s;
         if s <> RESULT_U16BIT then
           failed := true;
     end;


     procedure testlocal_big_5;
     var
      local5rec_big : tlevel5rec_big;
      begin
       clear_globals;
       { setup values - assign }
       local5rec_big.c := RESULT_U8BIT;
       local5rec_big.s := RESULT_U16BIT;
       local5rec_big.j := RESULT_S32BIT;
       c:= local5rec_big.c;
       if c <> RESULT_U8BIT then
        failed := true;
       s:= local5rec_big.s;
       if s <> RESULT_U16BIT then
        failed := true;
       j:= local5rec_big.j;
       if j <> RESULT_S32BIT then
        failed := true;
     end;
{$endif}

procedure testlocals;
var
 local1rec : tlevel1rec_packed;
 local2rec : tlevel2rec_packed;
 local3rec : tlevel3rec_packed;
 local4rec : tlevel4rec_packed;
 local5rec : tlevel5rec_packed;
begin
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
 { normal record access }
 Write('Non-Aligned simple local record access (secondvecn())...');
 failed := false;

 clear_globals;

 clear_globals;
 local1rec.c := RESULT_U8BIT;
 c:= local1rec.c;
 if c <> RESULT_U8BIT then
   failed := true;

 clear_globals;
 { setup values - assign }
 local2rec.c := RESULT_U8BIT;
 local2rec.d := RESULT_U8BIT;
 local2rec.s := RESULT_U16BIT;
 local2rec.n := RESULT_S32BIT;
 { load values - load }
 c:= local2rec.c;
 if c <> RESULT_U8BIT then
   failed := true;
 d:= local2rec.d;
 if d <> RESULT_U8BIT then
   failed := true;
 s:= local2rec.s;
 if s <> RESULT_U16BIT then
   failed := true;
 n:= local2rec.n;
 if n <> RESULT_S32BIT then
   failed := true;


 clear_globals;
 { setup values - assign }
 local3rec.c := RESULT_U8BIT;
 local3rec.s := RESULT_U16BIT;
 c:= local3rec.c;
 if c <> RESULT_U8BIT then
   failed := true;
 s:= local3rec.s;
 if s <> RESULT_U16BIT then
   failed := true;

 clear_globals;
 { setup values - assign }
 local4rec.c := RESULT_U8BIT;
 local4rec.i := RESULT_S64BIT;
 local4rec.s := RESULT_U16BIT;

 c:= local4rec.c;
 if c <> RESULT_U8BIT then
   failed := true;
 i:= local4rec.i;
 if i <> RESULT_S64BIT then
   failed := true;
 s:= local4rec.s;
 if s <> RESULT_U16BIT then
   failed := true;

 clear_globals;
 { setup values - assign }
 local5rec.c := RESULT_U8BIT;
 local5rec.s := RESULT_U16BIT;
 local5rec.j := RESULT_S32BIT;

 c:= local5rec.c;
 if c <> RESULT_U8BIT then
   failed := true;
 s:= local5rec.s;
 if s <> RESULT_U16BIT then
   failed := true;
 j:= local5rec.j;
 if j <> RESULT_S32BIT then
   failed := true;

 if failed then
   fail
 else
   WriteLN('Passed!');
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
end;
{---------------------------}

var
 level1rec_big : tlevel1rec_big;
 level2rec_big : tlevel2rec_big;
 level3rec_big : tlevel3rec_big;
 level4rec_big : tlevel4rec_big;
 level5rec_big : tlevel5rec_big;
begin
 { normal record access }
 Write('Aligned simple global record access (secondvecn())...');
 failed := false;

 clear_globals;
 c:= level1rec.c;
 if c <> RESULT_U8BIT then
   failed := true;

 clear_globals;
 c:= level2rec.c;
 if c <> RESULT_U8BIT then
   failed := true;
 d:= level2rec.d;
 if d <> RESULT_U8BIT then
   failed := true;
 s:= level2rec.s;
 if s <> RESULT_U16BIT then
   failed := true;
 n:= level2rec.n;
 if n <> RESULT_S32BIT then
   failed := true;


 clear_globals;
 c:= level3rec.c;
 if c <> RESULT_U8BIT then
   failed := true;
 s:= level3rec.s;
 if s <> RESULT_U16BIT then
   failed := true;


 clear_globals;
 c:= level4rec.c;
 if c <> RESULT_U8BIT then
   failed := true;
 i:= level4rec.i;
 if i <> RESULT_S64BIT then
   failed := true;
 s:= level4rec.s;
 if s <> RESULT_U16BIT then
   failed := true;

 clear_globals;
 c:= level5rec.c;
 if c <> RESULT_U8BIT then
   failed := true;
 s:= level5rec.s;
 if s <> RESULT_U16BIT then
   failed := true;
 j:= level5rec.j;
 if j <> RESULT_S32BIT then
   failed := true;

 if failed then
   fail
 else
   WriteLN('Passed!');

{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
 Write('Non-Aligned simple global record access (secondvecn())...');

 clear_globals;
 c:= level1rec_packed.c;
 if c <> RESULT_U8BIT then
   failed := true;

 clear_globals;
 c:= level2rec_packed.c;
 if c <> RESULT_U8BIT then
   failed := true;
 d:= level2rec_packed.d;
 if d <> RESULT_U8BIT then
   failed := true;
 s:= level2rec_packed.s;
 if s <> RESULT_U16BIT then
   failed := true;
 n:= level2rec_packed.n;
 if n <> RESULT_S32BIT then
   failed := true;


 clear_globals;
 c:= level3rec_packed.c;
 if c <> RESULT_U8BIT then
   failed := true;
 s:= level3rec_packed.s;
 if s <> RESULT_U16BIT then
   failed := true;


 clear_globals;
 c:= level4rec_packed.c;
 if c <> RESULT_U8BIT then
   failed := true;
 i:= level4rec_packed.i;
 if i <> RESULT_S64BIT then
   failed := true;
 s:= level4rec_packed.s;
 if s <> RESULT_U16BIT then
   failed := true;

 clear_globals;
 c:= level5rec_packed.c;
 if c <> RESULT_U8BIT then
   failed := true;
 s:= level5rec_packed.s;
 if s <> RESULT_U16BIT then
   failed := true;
 j:= level5rec_packed.j;
 if j <> RESULT_S32BIT then
   failed := true;

 if failed then
   fail
 else
   WriteLN('Passed!');

 Write('Non-Aligned big global record access (secondvecn())...');

 clear_globals;
 level1rec_big.c := RESULT_U8BIT;
 c:= level1rec_big.c;
 if c <> RESULT_U8BIT then
   failed := true;

 clear_globals;
 { setup values - assign }
 level2rec_big.c := RESULT_U8BIT;
 level2rec_big.d := RESULT_U8BIT;
 level2rec_big.s := RESULT_U16BIT;
 level2rec_big.n := RESULT_S32BIT;
 { load values - load }
 c:= level2rec_big.c;
 if c <> RESULT_U8BIT then
   failed := true;
 d:= level2rec_big.d;
 if d <> RESULT_U8BIT then
   failed := true;
 s:= level2rec_big.s;
 if s <> RESULT_U16BIT then
   failed := true;
 n:= level2rec_big.n;
 if n <> RESULT_S32BIT then
   failed := true;


 clear_globals;
 { setup values - assign }
 level3rec_big.c := RESULT_U8BIT;
 level3rec_big.s := RESULT_U16BIT;
 c:= level3rec_big.c;
 if c <> RESULT_U8BIT then
   failed := true;
 s:= level3rec_big.s;
 if s <> RESULT_U16BIT then
   failed := true;

 clear_globals;
 { setup values - assign }
 level4rec_big.c := RESULT_U8BIT;
 level4rec_big.i := RESULT_S64BIT;
 level4rec_big.s := RESULT_U16BIT;

 c:= level4rec_big.c;
 if c <> RESULT_U8BIT then
   failed := true;
 i:= level4rec_big.i;
 if i <> RESULT_S64BIT then
   failed := true;
 s:= level4rec_big.s;
 if s <> RESULT_U16BIT then
   failed := true;

 clear_globals;
 { setup values - assign }
 level5rec_big.c := RESULT_U8BIT;
 level5rec_big.s := RESULT_U16BIT;
 level5rec_big.j := RESULT_S32BIT;

 c:= level5rec_big.c;
 if c <> RESULT_U8BIT then
   failed := true;
 s:= level5rec_big.s;
 if s <> RESULT_U16BIT then
   failed := true;
 j:= level5rec_big.j;
 if j <> RESULT_S32BIT then
   failed := true;

 if failed then
   fail
 else
   WriteLN('Passed!');

{$endif FPC_REQUIRES_PROPER_ALIGNMENT}

 testlocals;

{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
{$ifndef cpu68k}
 Write('Non-Aligned big local record access (secondvecn())...');
 failed := false;

 testlocal_big_1;
 testlocal_big_2;
 testlocal_big_3;
 testlocal_big_4;
 testlocal_big_5;
 if failed then
   fail
 else
   WriteLN('Passed!');
{$endif}
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}

 Write('Aligned class big field access (secondvecn())...');
 clear_globals;
 failed := false;


 { LOC_REFERENCE }
 class1:=tclass1.create;
 class1.c:= RESULT_U8BIT;
 class1.j:= RESULT_S32BIT;
 class1.s:= RESULT_U16BIT;
 c:=class1.c;
 if c <> RESULT_U8BIT then
   failed := true;
 j:=class1.j;
 if j <> RESULT_S32BIT then
   failed := true;
 s:=class1.s;
 if s <> RESULT_U16BIT then
   failed := true;

 class1.destroy;
 clear_globals;

 { LOC_REGISTER }
 class1:=tclass1.create;
 class1.c:= RESULT_U8BIT;
 class1.j:= RESULT_S32BIT;
 class1.s:= RESULT_U16BIT;
 c:=(getclass).c;
 if c <> RESULT_U8BIT then
   failed := true;
 j:=(getclass).j;
 if j <> RESULT_S32BIT then
   failed := true;
 s:=(getclass).s;
 if s <> RESULT_U16BIT then
   failed := true;

 class1.destroy;


 if failed then
   fail
 else
   WriteLN('Passed!');
 {----------------------------------------------------------------------------}
 Write('Aligned class simple field access (secondvecn())...');
 clear_globals;
 failed := false;


 { LOC_REFERENCE }
 class2:=tclass2.create;
 class2.c:= RESULT_U8BIT;
 class2.i:= RESULT_S64BIT;
 class2.s:= RESULT_U16BIT;
 c:=class2.c;
 if c <> RESULT_U8BIT then
   failed := true;
 i:=class2.i;
 if i <> RESULT_S64BIT then
   failed := true;
 s:=class2.s;
 if s <> RESULT_U16BIT then
   failed := true;

 class2.destroy;
 clear_globals;

 { LOC_REGISTER }
 class2:=tclass2.create;
 class2.c:= RESULT_U8BIT;
 class2.i:= RESULT_S64BIT;
 class2.s:= RESULT_U16BIT;
 c:=(getclass2).c;
 if c <> RESULT_U8BIT then
   failed := true;
 i:=(getclass2).i;
 if i <> RESULT_S64BIT then
   failed := true;
 s:=(getclass2).s;
 if s <> RESULT_U16BIT then
   failed := true;

 class2.destroy;


 if failed then
   fail
 else
   WriteLN('Passed!');


end.
