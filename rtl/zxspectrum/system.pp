unit system;

interface

{$ifdef FULL_RTL}

{$define FPC_IS_SYSTEM}

{$I systemh.inc}

{$else FULL_RTL}

{$mode objfpc}
Type
  dword = longword;
  integer = smallint;
  sizeint = smallint;
  sizeuint = word;
  ptrint = smallint;
  ptruint = word;

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
procedure Move(const source;var dest;count:SizeInt);
Procedure FillChar(var x;count:SizeInt;value:byte);
function get_frame:pointer;
function get_caller_addr(framebp:pointer;addr:pointer=nil):pointer;
function get_caller_frame(framebp:pointer;addr:pointer=nil):pointer;
Function Sptr : pointer;

{$endif FULL_RTL}

{ OpenChannel(2) opens the upper screen
  OpenChannel(1) opens the lower screen
  OpenChannel(3) opens the ZX Printer }
procedure OpenChannel(Chan: Byte);
procedure PrintChar(Ch: Char);
procedure PrintLn;
procedure PrintHexDigit(const d: byte);
procedure PrintHexByte(const b: byte);
procedure PrintHexWord(const w: word);
procedure Ink(colour: Byte);
procedure Paper(colour: Byte);
procedure GotoXY(X, Y: Byte);
function ReadKey: Char;
function KeyPressed: Boolean;

implementation

{$ifdef FULL_RTL}
{$I system.inc}
{$else FULL_RTL}
{$I z80.inc}
{$endif FULL_RTL}

var
  save_iy: Word; public name 'FPC_SAVE_IY';
  LastKey: Char absolute 23560;

function ReadKey: Char;
begin
  repeat
    ReadKey:=LastKey;
  until ReadKey<>#0;
  LastKey:=#0;
end;

function KeyPressed: Boolean;
begin
  KeyPressed:=LastKey<>#0;
end;

procedure fpc_InitializeUnits;[public,alias:'FPC_INITIALIZEUNITS']; compilerproc;
begin
end;

Procedure fpc_do_exit;[Public,Alias:'FPC_DO_EXIT']; compilerproc;
begin
  repeat
  until false;
end;

procedure OpenChannel(Chan: Byte);
begin
  asm
    ld iy,(save_iy)
    ld a, (Chan)
    push ix
    call 5633
    pop ix
    ld (save_iy),iy
  end;
end;

procedure PrintChar(Ch: Char);
begin
  asm
    ld iy,(save_iy)
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

procedure Ink(colour: Byte);
begin
  PrintChar(#16);
  PrintChar(Char(colour));
end;

procedure Paper(colour: Byte);
begin
  PrintChar(#17);
  PrintChar(Char(colour));
end;

procedure GotoXY(X, Y: Byte);
begin
  PrintChar(#22);
  PrintChar(Char(X-1));
  PrintChar(Char(Y-1));
end;

end.
