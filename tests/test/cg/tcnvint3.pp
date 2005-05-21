{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{****************************************************************}
{ NODE TESTED : secondtypeconvert() -> second_int_to_int         }
{****************************************************************}
{ PRE-REQUISITES: secondload()                                   }
{                 secondassign()                                 }
{                 secondcalln()                                  }
{                 secondinline()                                 }
{                 secondadd()                                    }
{****************************************************************}
{ DEFINES:                                                       }
{****************************************************************}
{ REMARKS:                                                       }
{****************************************************************}
program tcnvint3;

{$ifdef VER70}
  {$define tp}
{$endif}

{$R-}

{$ifdef tp}
type
  smallint = integer;
{$endif}

procedure fail;
begin
  WriteLn('Failure.');
  halt(1);
end;


const
 ABSOLUTE_GETS8BIT_RESULT    = 63;
 GETS8BIT_RESULT             = -63;
 GETU8BIT_RESULT             = $55;
 ABSOLUTE_GETS16BIT_RESULT   = 16384;
 GETS16BIT_RESULT            = -16384;
 GETU16BIT_RESULT            = 32767;
 GETS32BIT_RESULT            = -1000000;
 GETU32BIT_RESULT            =  2000000;


{$ifndef tp}
   function gets64bit : int64;
    begin
      gets64bit := 12;
    end;
{$endif}

   function gets32bit : longint;
    begin
      gets32bit := GETS32BIT_RESULT;
    end;


  { return an 8-bit signed value }
  function gets8bit : shortint;
    begin
      gets8bit := GETS8BIT_RESULT;
    end;

  { return an 8-bit unsigned value }
  function getu8bit : byte;
   begin
     getu8bit := GETU8BIT_RESULT;
   end;


  function gets16bit : smallint;
    begin
      gets16bit := GETS16BIT_RESULT;
    end;

  function getu16bit : word;
    begin
      getu16bit := GETU16BIT_RESULT;
    end;


{$ifndef tp}
   function getu32bit : longword;
    begin
      getu32bit := GETU32BIT_RESULT;
    end;
{$endif tp}

var
 s8bit : shortint;
 s16bit : smallint;
 s32bit : longint;
 u16bit : word;
 u8bit : byte;
 failed : boolean;
{$ifndef tp}
 s64bit : int64;
 u32bit : cardinal;
{$endif}
begin
  {--------------------- dst_size < src_size -----------------------}
  { Actually the destination is always a natural register  }
  { either 32-bit / 64-bit, therefore not really important }
  { to do extensive checking on these nodes.               }
  { src : LOC_REGISTER }
  { dst : LOC_REGISTER }
  writeln('type conversion src_size > dst_size');
  writeln('dst : LOC_REGISTER src : LOC_REGISTER ');
{$ifndef tp}
  write('Testing dst : s32bit src : s64bit...');
  { s64bit -> s32bit  }
  s32bit:=gets64bit;
  if s32bit <> 12 then
    Fail
  else
    WriteLn('Passed.');
  { s64bit -> s8bit }
  write('Testing dst : s8bit src : s64bit...');
  s8bit:=gets64bit;
  if s8bit <> 12 then
    Fail
  else
    WriteLn('Passed.');
{$endif}
  { s32bit -> s16bit }
  write('Testing dst : s16bit src : s32bit...');
  s16bit := gets32bit;
  if s16bit <> smallint(GETS32BIT_RESULT AND $FFFF) then
    Fail
  else
    WriteLn('Passed.');

  { Here we will check each possible case of src, to test also the load }
  { of different memory sizes cases.                                    }
  { src : LOC_REFERENCE }
  { dst : LOC_REGISTER  }
{$ifndef tp}
  writeln('dst : LOC_REGISTER src : LOC_REFERENCE ');
  write('Testing dst : s32bit src : s64bit...');
  s64bit:=$FF0000;
  s32bit:=s64bit;
  if s32bit <> $FF0000 then
    Fail
  else
    WriteLn('Passed.');
{$endif}
  write('Testing dst : s16bit src : s32bit...');
  s32bit:=$FF00;
  s16bit:=s32bit;
  if s16bit <> smallint($FF00) then
    Fail
  else
    WriteLn('Passed.');
  { try a signed value }
  write('Testing dst : s16bit src : s32bit...');
  s32bit:=-14;
  s16bit:=s32bit;
  if s16bit <> smallint(-14) then
    Fail
  else
    WriteLn('Passed.');
  s16bit:=$FF;
  write('Testing dst : s8bit src : s16bit...');
  s8bit:=s16bit;
  if s8bit <> shortint($FF) then
    Fail
  else
    WriteLn('Passed.');
{$ifndef tp}
  write('Testing dst : u16bit src : u32bit...');
  u32bit:=$F001;
  u16bit := u32bit;
  if u16bit <> $F001 then
    Fail
  else
    WriteLn('Passed.');
{$endif}
  write('Testing dst : u8bit src : u16bit...');
  u16bit := $10;
  u8bit := u16bit;
  if u8bit <> $10 then
    Fail
  else
    WriteLn('Passed.');

  { That was the easy part... now : dst_size > src_size    }
  { here we must take care of sign extension               }

  { src : LOC_REGISTER }
  { dst : LOC_REGISTER }
  writeln('type conversion dst_size > src_size');
  writeln('dst : LOC_REGISTER src : LOC_REGISTER ');

  failed := false;
  write('Testing dst : u16bit  src : s8bit, u8bit... ');
  u16bit:=gets8bit;
  if u16bit <> word(GETS8BIT_RESULT) then
     failed := true;
  u16bit:=getu8bit;
  if u16bit <> GETU8BIT_RESULT then
     failed := true;
  if failed then
    Fail
  else
    WriteLn('Passed.');

{$ifndef tp}
  failed := false;
  write('Testing dst : u32bit  src : s8bit, u8bit, s16bit, u16bit... ');
  u32bit:=gets8bit;
  if u32bit <> cardinal(GETS8BIT_RESULT) then
     failed := true;
  u32bit:=getu8bit;
  if u32bit <> GETU8BIT_RESULT then
     failed := true;
  u32bit:=gets16bit;
  if u32bit <> cardinal(GETS16BIT_RESULT) then
     failed := true;
  u32bit:=getu16bit;
  if u32bit <> GETU16BIT_RESULT then
     failed := true;

  if failed then
    Fail
  else
    WriteLn('Passed.');
{$endif}


  failed := false;
  write('Testing dst : s16bit  src : s8bit, u8bit...');
  s16bit := gets8bit;
  if s16bit <> GETS8BIT_RESULT then
    failed := true;
  s16bit := getu8bit;
  if s16bit <> GETU8BIT_RESULT then
    failed := true;
  if failed then
    Fail
  else
    WriteLn('Passed.');


  failed := false;
  write('Testing dst : s32bit  src : s8bit, u8bit. s16bit, u16bit...');

  s32bit := gets8bit;
  if s32bit <> GETS8BIT_RESULT then
    failed := true;
  s32bit := getu8bit;
  if s32bit <> GETU8BIT_RESULT then
    failed := true;
  s32bit := gets16bit;
  if s32bit <> GETS16BIT_RESULT then
    failed := true;
  s32bit := getu16bit;
  if s32bit <> GETU16BIT_RESULT then
    failed := true;
  if failed then
    Fail
  else
    WriteLn('Passed.');

{$ifndef tp}
  failed := false;
  write('Testing dst : s64bit  src : s8bit, u8bit. s16bit, u16bit, s32bit, u32bit...');

  s64bit := gets8bit;
  if s64bit <> GETS8BIT_RESULT then
    failed := true;
  s64bit := getu8bit;
  if s64bit <> GETU8BIT_RESULT then
    failed := true;
  s64bit := gets16bit;
  if s64bit <> GETS16BIT_RESULT then
    failed := true;
  s64bit := getu16bit;
  if s64bit <> GETU16BIT_RESULT then
    failed := true;
  s64bit := gets32bit;
  if s64bit <> GETS32BIT_RESULT then
    failed := true;
  s64bit := getu32bit;
  if s64bit <> GETU32BIT_RESULT then
    failed := true;
  if failed then
    Fail
  else
    WriteLn('Passed.');
{$endif}
  { src : LOC_REFERENCE }
  { dst : LOC_REGISTER }
  writeln('type conversion dst_size > src_size');
  writeln('dst : LOC_REGISTER src : LOC_REFERENCE ');

  failed := false;
  write('Testing dst : u16bit  src : s8bit, u8bit... ');
  s8bit := GETS8BIT_RESULT;
  u16bit:=s8bit;
  if u16bit <> word(GETS8BIT_RESULT) then
     failed := true;
  u8bit := GETU8BIT_RESULT;
  u16bit:=u8bit;
  if u16bit <> GETU8BIT_RESULT then
     failed := true;
  if failed then
    Fail
  else
    WriteLn('Passed.');

{$ifndef tp}
  failed := false;
  write('Testing dst : u32bit  src : s8bit, u8bit, s16bit, u16bit... ');
  s8bit := GETS8BIT_RESULT;
  u32bit:=s8bit;
  if u32bit <> cardinal(GETS8BIT_RESULT) then
     failed := true;
  u8bit := GETU8BIT_RESULT;
  u32bit:=u8bit;
  if u32bit <> GETU8BIT_RESULT then
     failed := true;
  s16bit := GETS16BIT_RESULT;
  u32bit:=s16bit;
  if u32bit <> cardinal(GETS16BIT_RESULT) then
     failed := true;
  u16bit := GETU16BIT_RESULT;
  u32bit:=u16bit;
  if u32bit <> GETU16BIT_RESULT then
     failed := true;

  if failed then
    Fail
  else
    WriteLn('Passed.');
{$endif}

  failed := false;
  write('Testing dst : s16bit  src : s8bit, u8bit...');
  s8bit := GETS8BIT_RESULT;
  s16bit := s8bit;
  if s16bit <> GETS8BIT_RESULT then
    failed := true;
  u8bit := GETU8BIT_RESULT;
  s16bit := u8bit;
  if s16bit <> GETU8BIT_RESULT then
    failed := true;
  if failed then
    Fail
  else
    WriteLn('Passed.');


  failed := false;
  write('Testing dst : s32bit  src : s8bit, u8bit. s16bit, u16bit...');

  s8bit := GETS8BIT_RESULT;
  s32bit := s8bit;
  if s32bit <> GETS8BIT_RESULT then
    failed := true;
  u8bit := GETU8BIT_RESULT;
  s32bit := u8bit;
  if s32bit <> GETU8BIT_RESULT then
    failed := true;
  s16bit := GETS16BIT_RESULT;
  s32bit := s16bit;
  if s32bit <> GETS16BIT_RESULT then
    failed := true;
  u16bit := GETU16BIT_RESULT;
  s32bit := u16bit;
  if s32bit <> GETU16BIT_RESULT then
    failed := true;
  if failed then
    Fail
  else
    WriteLn('Passed.');


{$ifndef tp}
  failed := false;
  write('Testing dst : s64bit  src : s8bit, u8bit. s16bit, u16bit, s32bit, u32bit...');

  s8bit := GETS8BIT_RESULT;
  s64bit := s8bit;
  if s64bit <> GETS8BIT_RESULT then
    failed := true;
  u8bit := GETU8BIT_RESULT;
  s64bit := u8bit;
  if s64bit <> GETU8BIT_RESULT then
    failed := true;
  s16bit := GETS16BIT_RESULT;
  s64bit := s16bit;
  if s64bit <> GETS16BIT_RESULT then
    failed := true;
  u16bit := GETU16BIT_RESULT;
  s64bit := u16bit;
  if s64bit <> GETU16BIT_RESULT then
    failed := true;
  s32bit := GETS32BIT_RESULT;
  s64bit := s32bit;
  if s64bit <> GETS32BIT_RESULT then
    failed := true;
  u32bit := GETU32BIT_RESULT;
  s64bit := u32bit;
  if s64bit <> GETU32BIT_RESULT then
    failed := true;
  if failed then
    Fail
  else
    WriteLn('Passed.');
{$endif}
end.
