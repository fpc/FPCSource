{ %cpu=i8086 }

{ far pointer arithmetic tests }

{ far pointer arithmetic should work only on the offset,
  without changing the segment }

{$R-}

var
  ErrorCode: Integer;

procedure Error(Code: Integer);
begin
  Writeln('Error: ', code);
  ErrorCode := Code;
end;

var
  FarPtr: FarPointer;
  FarPtr2: FarPointer;
  int16: Integer;
  int32: LongInt;
begin
  ErrorCode := 0;

  Writeln('farptr + int16_var');
  FarPtr := Ptr($1234, $5678);
  int16 := $F0AD;
  FarPtr2 := nil;
  FarPtr2 := FarPtr + int16;
  if FarPtr2 <> Ptr($1234, $4725) then
    Error(1);
  Writeln('int16_var + farptr');
  FarPtr := Ptr($1234, $5678);
  int16 := $F0AD;
  FarPtr2 := nil;
  FarPtr2 := int16 + FarPtr;
  if FarPtr2 <> Ptr($1234, $4725) then
    Error(1);
  Writeln('farptr + int32_var');
  FarPtr := Ptr($1234, $5678);
  int32 := $55AAF0AD;
  FarPtr2 := nil;
  FarPtr2 := FarPtr + int32;
  if FarPtr2 <> Ptr($1234, $4725) then
    Error(1);
  Writeln('int32_var + farptr');
  FarPtr := Ptr($1234, $5678);
  int32 := $55AAF0AD;
  FarPtr2 := nil;
  FarPtr2 := int32 + FarPtr;
  if FarPtr2 <> Ptr($1234, $4725) then
    Error(1);
  Writeln('farptr + int16_const');
  FarPtr := Ptr($1234, $5678);
  FarPtr2 := nil;
  FarPtr2 := FarPtr + $F0AD;
  if FarPtr2 <> Ptr($1234, $4725) then
    Error(1);
  Writeln('int16_const + farptr');
  FarPtr := Ptr($1234, $5678);
  FarPtr2 := nil;
  FarPtr2 := $F0AD + FarPtr;
  if FarPtr2 <> Ptr($1234, $4725) then
    Error(1);
  Writeln('farptr + int32_const');
  FarPtr := Ptr($1234, $5678);
  FarPtr2 := nil;
  FarPtr2 := FarPtr + $55AAF0AD;
  if FarPtr2 <> Ptr($1234, $4725) then
    Error(1);
  Writeln('int32_const + farptr');
  FarPtr := Ptr($1234, $5678);
  FarPtr2 := nil;
  FarPtr2 := $55AAF0AD + FarPtr;
  if FarPtr2 <> Ptr($1234, $4725) then
    Error(1);
  { const }
  Writeln('farptr_const + int16_var');
  int16 := $F0AD;
  FarPtr2 := nil;
  FarPtr2 := FarPointer($12345678) + int16;
  if FarPtr2 <> Ptr($1234, $4725) then
    Error(1);
  Writeln('int16_var + farptr_const');
  int16 := $F0AD;
  FarPtr2 := nil;
  FarPtr2 := int16 + FarPointer($12345678);
  if FarPtr2 <> Ptr($1234, $4725) then
    Error(1);
  Writeln('farptr_const + int32_var');
  int32 := $55AAF0AD;
  FarPtr2 := nil;
  FarPtr2 := FarPointer($12345678) + int32;
  if FarPtr2 <> Ptr($1234, $4725) then
    Error(1);
  Writeln('int32_var + farptr_const');
  int32 := $55AAF0AD;
  FarPtr2 := nil;
  FarPtr2 := int32 + FarPointer($12345678);
  if FarPtr2 <> Ptr($1234, $4725) then
    Error(1);
  Writeln('farptr_const + int16_const');
  FarPtr2 := nil;
  FarPtr2 := FarPointer($12345678) + $F0AD;
  if FarPtr2 <> Ptr($1234, $4725) then
    Error(1);
  Writeln('int16_const + farptr_const');
  FarPtr2 := nil;
  FarPtr2 := $F0AD + FarPointer($12345678);
  if FarPtr2 <> Ptr($1234, $4725) then
    Error(1);
  Writeln('farptr_const + int32_const');
  FarPtr2 := nil;
  FarPtr2 := FarPointer($12345678) + $55AAF0AD;
  if FarPtr2 <> Ptr($1234, $4725) then
    Error(1);
  Writeln('int32_const + farptr_const');
  FarPtr2 := nil;
  FarPtr2 := $55AAF0AD + FarPointer($12345678);
  if FarPtr2 <> Ptr($1234, $4725) then
    Error(1);

  Writeln('farptr - int16_var');
  FarPtr := Ptr($1234, $5678);
  int16 := $F0AD;
  FarPtr2 := nil;
  FarPtr2 := FarPtr - int16;
  if FarPtr2 <> Ptr($1234, $65CB) then
    Error(1);
  Writeln('farptr - int32_var');
  FarPtr := Ptr($1234, $5678);
  int32 := $55AAF0AD;
  FarPtr2 := nil;
  FarPtr2 := FarPtr - int32;
  if FarPtr2 <> Ptr($1234, $65CB) then
    Error(1);
  Writeln('farptr - int16_const');
  FarPtr := Ptr($1234, $5678);
  FarPtr2 := nil;
  FarPtr2 := FarPtr - $F0AD;
  if FarPtr2 <> Ptr($1234, $65CB) then
    Error(1);
  Writeln('farptr - int32_const');
  FarPtr := Ptr($1234, $5678);
  FarPtr2 := nil;
  FarPtr2 := FarPtr - $55AAF0AD;
  if FarPtr2 <> Ptr($1234, $65CB) then
    Error(1);
  Writeln('farptr_const - int16_var');
  FarPtr := Ptr($1234, $5678);
  int16 := $F0AD;
  FarPtr2 := nil;
  FarPtr2 := FarPointer($12345678) - int16;
  if FarPtr2 <> Ptr($1234, $65CB) then
    Error(1);
  Writeln('farptr_const - int32_var');
  FarPtr := Ptr($1234, $5678);
  int32 := $55AAF0AD;
  FarPtr2 := nil;
  FarPtr2 := FarPointer($12345678) - int32;
  if FarPtr2 <> Ptr($1234, $65CB) then
    Error(1);
  Writeln('farptr_const - int16_const');
  FarPtr := Ptr($1234, $5678);
  FarPtr2 := nil;
  FarPtr2 := FarPointer($12345678) - $F0AD;
  if FarPtr2 <> Ptr($1234, $65CB) then
    Error(1);
  Writeln('farptr_const - int32_const');
  FarPtr := Ptr($1234, $5678);
  FarPtr2 := nil;
  FarPtr2 := FarPointer($12345678) - $55AAF0AD;
  if FarPtr2 <> Ptr($1234, $65CB) then
    Error(1);

  if ErrorCode = 0 then
    Writeln('Success!')
  else
    Halt(ErrorCode);
end.
