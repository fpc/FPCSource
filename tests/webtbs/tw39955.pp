{ %cpu=i386,i8086 }
{ %opt=-Op80386 -O2 }
const
  StrLen = 11 {5 or 11};
  Str: array [0..15] of string[StrLen] = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15');
var
  I: Integer;
  S: ^string;
  hs: string;
begin
  WriteLn('@Str=', HexStr(PtrUInt(@Str), 8));
  hs:='';
  for I := Low(Str) to High(Str) div 4 do
  begin
    {$IFDEF CPU16}
      asm jmp @1; nop; int3; @1: end;
    {$ELSE}
      asm jmp .L1; nop; int3; .L1: end;
    {$ENDIF}
    S := @Str[I];
    WriteLn('Str[', I, ']=@', HexStr(PtrUInt(S), 8), '=@Str+', PtrUInt(S) - PtrUInt(@Str), '=''', S^, '''');
    hs:=hs+S^;
  end;
  if hs<>'0123' then
    halt(1);
  writeln('ok');
end.
