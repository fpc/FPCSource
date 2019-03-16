{ %target=win32,win64,linux }
{ %cpu=i386,x86_64 }
{ %opt=-O1 }

program tw35187;

{ NOTE: SIGSEGV won't trigger if GetMem is used because it allocates pages from a large pre-reserved heap. [Kit] }

{$IFDEF TARGET_VALID}
{$UNDEF TARGET_VALID}
{$ENDIF TARGET_VALID}

{$IFDEF WINDOWS}
uses
  Windows;
{$DEFINE TARGET_VALID}
{$ENDIF}
{$IFDEF UNIX}
uses
  BaseUnix, SysCall;

function fpmprotect(Addr: Pointer; Len: PtrUInt; Prot: LongInt): LongInt; inline;
begin
  fpmprotect := Do_SysCall(syscall_nr_mprotect, TSysParam(Addr), Len, Prot);
end;
{$DEFINE TARGET_VALID}
{$ENDIF}

{$IFNDEF TARGET_VALID}
{$ERROR No memory allocation routine available }
{$ENDIF TARGET_VALID}

const
  TestBlock: packed array[0..127] of Char = 'The quick brown fox jumps over the lazy dog, Victor jagt zw'#148'lf Boxk'#132'mpfer quer '#129'ber den gro'#225'en Sylter Deich.'#10#13'0123456789?!"#%'#251#253#252;

  Expected: packed array[0..255] of Char = '54686520717569636B2062726F776E20666F78206A756D7073206F76657220746865206C617A7920646F672C20566963746F72206A616774207A77946C6620426F786B846D70666572207175657220816265722064656E2067726FE1656E2053796C7465722044656963682E0A0D303132333435363738393F21222325FBFDFC';

  HexDigits: packed array[0..$F] of Char = '0123456789ABCDEF';

var
  Buf: packed array[0..255] of Char;
  HexPtr: PChar;
  P: PByte;
  I: DWord;
  HeapBlock, HeapMarker: PByte;
begin
  WriteLn(TestBlock);
  FillChar(Buf, SizeOf(Buf), 0);

  { Reserve two 4K memory pages: one that is read-write followed by one that
    has no access rights at all and will trigger SIGSEGV if encroached }
{$IFDEF WINDOWS}
  HeapBlock := VirtualAlloc(
                 VirtualAlloc(nil, 8192, MEM_RESERVE, PAGE_READWRITE),
                 4096,
                 MEM_COMMIT,
                 PAGE_READWRITE
               );
  if not Assigned(HeapBlock) then
    begin
      WriteLn('Memory allocation failure');
      Halt(1);
    end;
  HeapMarker := HeapBlock + 3968; { 4096 - 128 }
{$ENDIF WINDOWS}
{$IFDEF UNIX}
  HeapBlock := fpmmap(nil, 8192, PROT_NONE, MAP_ANON or MAP_PRIVATE, -1, 0);
  if not Assigned(HeapBlock) or (fpmprotect(HeapBlock, 4096, PROT_READ or PROT_WRITE) <> 0) then
    begin
      WriteLn('Memory allocation failure');
      Halt(1);
    end;

  HeapMarker := HeapBlock + 3968; { 4096 - 128 }
{$ENDIF UNIX}

  Move(TestBlock, HeapMarker^, SizeOf(TestBlock));
  HexPtr := @Buf;

  for I := 0 to SizeOf(TestBlock) - 1 do
  begin
    P := HeapMarker + I;

    HexPtr^ := HexDigits[P^ shr 4]; { first nybble }
    Write(HexPtr^);
    Inc(HexPtr);

    { #35187: This instruction causes an access violation on the last byte
        because it tries to read a word instead of a byte. }

    HexPtr^ := HexDigits[P^ and $F]; { second nybble }
    Write(HexPtr^);
    Inc(HexPtr);
  end;

{$IFDEF WINDOWS}
  VirtualFree(HeapBlock, 0, MEM_RELEASE);
{$ENDIF WINDOWS}
{$IFDEF UNIX}
  fpmunmap(HeapBlock, 8192);
{$ENDIF UNIX}

  WriteLn();

  for I := 0 to SizeOf(TestBlock) - 1 do
    if Buf[I] <> Expected[I] then
    begin
      WriteLn('Error at index ', I, '; expected ', Expected[I], ' got ', Buf[I]);
      Halt(1);
    end;

  WriteLn('ok');
end.
