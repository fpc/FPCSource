unit system;

{$mode objfpc}

interface

Type
  dword = longword;
  integer = smallint;

   jmp_buf = packed record
     f,a,b,c,e,d,l,h,ixlo,ixhi,iylo,iyhi,splo,sphi,pclo,pchi : byte;
   end;
   pjmp_buf = ^jmp_buf;

  PExceptAddr = ^TExceptAddr;
  TExceptAddr = record
  end;

      PGuid = ^TGuid;
      TGuid = packed record
         case integer of
            1 : (
                 Data1 : DWord;
                 Data2 : word;
                 Data3 : word;
                 Data4 : array[0..7] of byte;
                );
            2 : (
                 D1 : DWord;
                 D2 : word;
                 D3 : word;
                 D4 : array[0..7] of byte;
                );
            3 : ( { uuid fields according to RFC4122 }
                 time_low : dword;                     // The low field of the timestamp
                 time_mid : word;                      // The middle field of the timestamp
                 time_hi_and_version : word;           // The high field of the timestamp multiplexed with the version number
                 clock_seq_hi_and_reserved : byte;     // The high field of the clock sequence multiplexed with the variant
                 clock_seq_low : byte;                 // The low field of the clock sequence
                 node : array[0..5] of byte;           // The spatially unique node identifier
                );
      end;

  HRESULT = Byte;

  TTypeKind = (tkUnknown,tkInteger,tkChar,tkEnumeration,tkFloat,
              tkSet,tkMethod,tkSString,tkLString,tkAString,
              tkWString,tkVariant,tkArray,tkRecord,tkInterface,
              tkClass,tkObject,tkWChar,tkBool,tkInt64,tkQWord,
              tkDynArray,tkInterfaceRaw,tkProcVar,tkUString,tkUChar,
              tkHelper,tkFile,tkClassRef,tkPointer);

procedure fpc_InitializeUnits;compilerproc;
Procedure fpc_do_exit;compilerproc;

procedure PrintChar(Ch: Char);
procedure PrintLn;
procedure PrintHexDigit(const d: byte);
procedure PrintHexByte(const b: byte);
procedure PrintHexWord(const w: word);

implementation

var
  save_iy: Word; public name 'FPC_SAVE_IY';

procedure fpc_InitializeUnits;[public,alias:'FPC_INITIALIZEUNITS']; compilerproc;
begin
end;

Procedure fpc_do_exit;[Public,Alias:'FPC_DO_EXIT']; compilerproc;
begin
  repeat
  until false;
end;

procedure PrintChar(Ch: Char);
begin
  asm
    ld iy,(save_iy)

    ld a, 2
    push ix
    call 5633
    pop ix

    ld a, (Ch)
    push ix
    rst 16
    pop ix
    ld (save_iy),iy
  end;
end;

procedure PrintLn;
begin
  PrintChar(#13);
end;

procedure PrintHexDigit(const d: byte);
begin
  { the code generator is still to broken to compile this, so we do it in a stupid way }
{  if (d >= 0) or (d <= 9) then
    PrintChar(Char(d + Ord('0')))
  else if (d >= 10) and (d <= 15) then
    PrintChar(Char(d + (Ord('A') - 10)));}
  if d=0 then
    PrintChar('0')
  else if d=1 then
    PrintChar('1')
  else if d=2 then
    PrintChar('2')
  else if d=3 then
    PrintChar('3')
  else if d=4 then
    PrintChar('4')
  else if d=5 then
    PrintChar('5')
  else if d=6 then
    PrintChar('6')
  else if d=7 then
    PrintChar('7')
  else if d=8 then
    PrintChar('8')
  else if d=9 then
    PrintChar('9')
  else if d=10 then
    PrintChar('A')
  else if d=11 then
    PrintChar('B')
  else if d=12 then
    PrintChar('C')
  else if d=13 then
    PrintChar('D')
  else if d=14 then
    PrintChar('E')
  else if d=15 then
    PrintChar('F')
  else
    PrintChar('?');
end;

procedure PrintHexByte(const b: byte);
begin
  PrintHexDigit(b shr 4);
  PrintHexDigit(b and $F);
end;

procedure PrintHexWord(const w: word);
begin
  PrintHexByte(Byte(w shr 8));
  PrintHexByte(Byte(w));
end;

end.
