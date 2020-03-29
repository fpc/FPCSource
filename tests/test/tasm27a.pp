{ %CPU=i386 }

{$IFDEF FPC}
{$MODE DELPHI}
{$PIC OFF}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}
program tasm27a;

{$S-}

type
{$IFNDEF FPC}
  CodePointer = Pointer;
{$ENDIF}
  TRec = record
    Str: String[8];
    Arr: array [-5..10] of SmallInt;
  end;

var
  Rec: TRec;

const
  x_size = 6*8*4+1;
procedure x; assembler;
asm
  dd Rec.Str                    { dd Rec.Str }
  dd Rec.Str[0]                 { dd Rec.Str }
  dd Rec.Arr                    { dd Rec.Arr }
  dd Rec.Arr[2]                 { dd Rec.Arr+2 }
  dd 5[7]                       { dd 12 }
  dd 5+[7]                      { dd 12 }
  dd 5-[7]                      { dd -2 }
  dd [5]                        { dd 5 }

  dd byte ptr Rec.Str           { dd Rec.Str }
  dd byte ptr Rec.Str[0]        { dd Rec.Str }
  dd byte ptr Rec.Arr           { dd Rec.Arr }
  dd byte ptr Rec.Arr[2]        { dd Rec.Arr+2 }
  dd byte ptr 5[7]              { dd 12 }
  dd byte ptr 5+[7]             { dd 12 }
  dd byte ptr 5-[7]             { dd -2 }
  dd byte ptr [5]               { dd 5 }

  dd word ptr Rec.Str           { dd Rec.Str }
  dd word ptr Rec.Str[0]        { dd Rec.Str }
  dd word ptr Rec.Arr           { dd Rec.Arr }
  dd word ptr Rec.Arr[2]        { dd Rec.Arr+2 }
  dd word ptr 5[7]              { dd 12 }
  dd word ptr 5+[7]             { dd 12 }
  dd word ptr 5-[7]             { dd -2 }
  dd word ptr [5]               { dd 5 }

  dd dword ptr Rec.Str          { dd Rec.Str }
  dd dword ptr Rec.Str[0]       { dd Rec.Str }
  dd dword ptr Rec.Arr          { dd Rec.Arr }
  dd dword ptr Rec.Arr[2]       { dd Rec.Arr+2 }
  dd dword ptr 5[7]             { dd 12 }
  dd dword ptr 5+[7]            { dd 12 }
  dd dword ptr 5-[7]            { dd -2 }
  dd dword ptr [5]              { dd 5 }

  dd qword ptr Rec.Str          { dd Rec.Str }
  dd qword ptr Rec.Str[0]       { dd Rec.Str }
  dd qword ptr Rec.Arr          { dd Rec.Arr }
  dd qword ptr Rec.Arr[2]       { dd Rec.Arr+2 }
  dd qword ptr 5[7]             { dd 12 }
  dd qword ptr 5+[7]            { dd 12 }
  dd qword ptr 5-[7]            { dd -2 }
  dd qword ptr [5]              { dd 5 }

  dd tbyte ptr Rec.Str          { dd Rec.Str }
  dd tbyte ptr Rec.Str[0]       { dd Rec.Str }
  dd tbyte ptr Rec.Arr          { dd Rec.Arr }
  dd tbyte ptr Rec.Arr[2]       { dd Rec.Arr+2 }
  dd tbyte ptr 5[7]             { dd 12 }
  dd tbyte ptr 5+[7]            { dd 12 }
  dd tbyte ptr 5-[7]            { dd -2 }
  dd tbyte ptr [5]              { dd 5 }
end;
procedure x_verify; assembler;
asm
  dd Rec.Str                    { dd Rec.Str }
  dd Rec.Str                    { dd Rec.Str[0] }
  dd Rec.Arr                    { dd Rec.Arr }
  dd Rec.Arr+2                  { dd Rec.Arr[2] }
  dd 12                         { dd 5[7] }
  dd 12                         { dd 5+[7] }
  dd -2                         { dd 5-[7] }
  dd 5                          { dd [5] }

  dd Rec.Str                    { dd byte ptr Rec.Str }
  dd Rec.Str                    { dd byte ptr Rec.Str[0] }
  dd Rec.Arr                    { dd byte ptr Rec.Arr }
  dd Rec.Arr+2                  { dd byte ptr Rec.Arr[2] }
  dd 12                         { dd byte ptr 5[7] }
  dd 12                         { dd byte ptr 5+[7] }
  dd -2                         { dd byte ptr 5-[7] }
  dd 5                          { dd byte ptr [5] }

  dd Rec.Str                    { dd word ptr Rec.Str }
  dd Rec.Str                    { dd word ptr Rec.Str[0] }
  dd Rec.Arr                    { dd word ptr Rec.Arr }
  dd Rec.Arr+2                  { dd word ptr Rec.Arr[2] }
  dd 12                         { dd word ptr 5[7] }
  dd 12                         { dd word ptr 5+[7] }
  dd -2                         { dd word ptr 5-[7] }
  dd 5                          { dd word ptr [5] }

  dd Rec.Str                    { dd dword ptr Rec.Str }
  dd Rec.Str                    { dd dword ptr Rec.Str[0] }
  dd Rec.Arr                    { dd dword ptr Rec.Arr }
  dd Rec.Arr+2                  { dd dword ptr Rec.Arr[2] }
  dd 12                         { dd dword ptr 5[7] }
  dd 12                         { dd dword ptr 5+[7] }
  dd -2                         { dd dword ptr 5-[7] }
  dd 5                          { dd dword ptr [5] }

  dd Rec.Str                    { dd qword ptr Rec.Str }
  dd Rec.Str                    { dd qword ptr Rec.Str[0] }
  dd Rec.Arr                    { dd qword ptr Rec.Arr }
  dd Rec.Arr+2                  { dd qword ptr Rec.Arr[2] }
  dd 12                         { dd qword ptr 5[7] }
  dd 12                         { dd qword ptr 5+[7] }
  dd -2                         { dd qword ptr 5-[7] }
  dd 5                          { dd qword ptr [5] }

  dd Rec.Str                    { dd tbyte ptr Rec.Str }
  dd Rec.Str                    { dd tbyte ptr Rec.Str[0] }
  dd Rec.Arr                    { dd tbyte ptr Rec.Arr }
  dd Rec.Arr+2                  { dd tbyte ptr Rec.Arr[2] }
  dd 12                         { dd tbyte ptr 5[7] }
  dd 12                         { dd tbyte ptr 5+[7] }
  dd -2                         { dd tbyte ptr 5-[7] }
  dd 5                          { dd tbyte ptr [5] }
end;

function CompareCode(cp, cp2: CodePointer; sz: Integer): Boolean;
var
  I: Integer;
  lastbyte: Byte;
begin
{$IFDEF FPC}
  if CompareByte(cp^, cp2^, sz) <> 0 then
  begin
    CompareCode := False;
    exit;
  end;
{$ELSE}
  for I := 0 to sz - 1 do
    if PChar(cp)[I] <> PChar(cp2)[I] then
    begin
      CompareCode := False;
      exit;
    end;
{$ENDIF}
  { check also that the last byte is a retn instruction }
  { size differs at least between linux and windows, so leave the ret check away
  lastbyte:=PByte(cp)[sz-1];
  if lastbyte<>$C3 then
  begin
    CompareCode := False;
    exit;
  end;
  }
  CompareCode := True;
end;

procedure Error(N: Integer);
begin
  Writeln('Error! ', N);
  Halt(1);
end;

begin
  if not CompareCode(CodePointer(@x), CodePointer(@x_verify), x_size) then
    Error(1);

  Writeln('Ok!');
end.
