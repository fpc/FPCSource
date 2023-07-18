constructor TMarshal.Create;
begin
  System.Error(reInvalidPtr);
end;

class function TMarshal.AllocMem(Size: SizeInt): TPtrWrapper;
begin
  Result.Value := System.AllocMem(Size);
end;

class function TMarshal.ReallocMem(OldPtr: TPtrWrapper; NewSize: SizeInt): TPtrWrapper;
var
  P: Pointer;
begin
  P := OldPtr.Value;
  Result.Value := System.ReallocMem(P, NewSize);
end;

class procedure TMarshal.FreeMem(Ptr: TPtrWrapper);
begin
  System.FreeMem(Ptr.Value);
end;

class procedure TMarshal.Move(Src, Dest: TPtrWrapper; Count: SizeInt); static;
begin
  System.Move(Src.Value^, Dest.Value^, Count);
end;

class function TMarshal.AsAnsi(const S: UnicodeString): AnsiString;
begin
  Result := AnsiString(S);
end;

class function TMarshal.AsAnsi(S: PUnicodeChar): AnsiString;
begin
  result := AnsiString(S);
end;

class function TMarshal.InOutString(const S: UnicodeString): PUnicodeChar;
begin
  Result := PUnicodeChar(S);
end;

class function TMarshal.InString(const S: UnicodeString): PUnicodeChar;
begin
  Result := PUnicodeChar(S);
end;

class function TMarshal.OutString(const S: UnicodeString): PUnicodeChar;
begin
  Result := PUnicodeChar(S);
end;

class function TMarshal.UnsafeAddrOf(var Value): TPtrWrapper;
begin
  Result.Value := @Value;
end;

class function TMarshal.AllocStringAsAnsi(const Str: UnicodeString): TPtrWrapper;
begin
  Result := AllocStringAsAnsi(PUnicodeChar(Pointer(Str)), Length(Str), DefaultSystemCodePage);
end;

class function TMarshal.AllocStringAsAnsi(const Str: UnicodeString; CodePage: Word): TPtrWrapper;
begin
  Result := AllocStringAsAnsi(PUnicodeChar(Pointer(Str)), Length(Str), CodePage);
end;

class function TMarshal.AllocStringAsAnsi(S: PUnicodeChar): TPtrWrapper;
begin
  Result := AllocStringAsAnsi(S, Length(S), DefaultSystemCodePage);
end;

class function TMarshal.AllocStringAsAnsi(S: PUnicodeChar; CodePage: Word): TPtrWrapper;
begin
  Result := AllocStringAsAnsi(S, Length(S), CodePage);
end;

class function TMarshal.AllocStringAsUnicode(const Str: UnicodeString): TPtrWrapper;
var
  NBytes: SizeUint;
begin
  NBytes := (Length(Str) + 1) * SizeOf(UnicodeChar);
  Result.Value := System.GetMem(NBytes);
  System.Move(PUnicodeChar(Str)^, Result.Value^, NBytes);
end;

class function TMarshal.AllocStringAsUtf8(const Str: UnicodeString): TPtrWrapper;
begin
  Result := AllocStringAsAnsi(PUnicodeChar(Pointer(Str)), Length(Str), CP_UTF8);
end;

class function TMarshal.AllocStringAsUtf8(S: PUnicodeChar): TPtrWrapper;
begin
  Result := AllocStringAsAnsi(S, Length(S), CP_UTF8);
end;

class function TMarshal.AllocStringAsAnsi(S: PUnicodeChar; Len: SizeInt; CodePage: Word): TPtrWrapper;
var
  U2ARes: AnsiString;
  NBytes: SizeInt;
begin
  U2ARes := ''; { Suppress warning. }
  WideStringManager.Unicode2AnsiMoveProc(S, U2ARes, CodePage, Len);
  if Length(U2ARes) = 0 then
  begin
    Result.Value := nil;
    Exit;
  end;
  { Could instead avoid the second allocation, assuming U2ARes.RefCount = 1:
    System.Move(Pointer(U2ARes)^, (Pointer(U2ARes) - AnsiStringHeaderSize)^, (Length(U2ARes) + 1) * SizeOf(AnsiChar));
    Result.FValue := Pointer(U2ARes) - AnsiStringHeaderSize;
    Pointer(U2ARes) := nil; }
  NBytes := (Length(U2ARes) + 1) * SizeOf(AnsiChar);
  Result.Value := System.GetMem(NBytes);
  System.Move(PAnsiChar(U2ARes)^, Result.Value^, NBytes);
end;

class procedure TMarshal.Copy(const Src: specialize TArray<UnicodeChar>; StartIndex: SizeInt; Dest: TPtrWrapper; Count: SizeInt);
begin
  System.Move(PUnicodeChar(Src)[StartIndex], Dest.Value^, Count * SizeOf(UnicodeChar));
end;

class procedure TMarshal.Copy(Src: TPtrWrapper; var Dest: specialize TArray<UnicodeChar>; StartIndex: SizeInt; Count: SizeInt);
begin
  System.Move(Src.Value^, PUnicodeChar(Dest)[StartIndex], Count * SizeOf(UnicodeChar));
end;

class procedure TMarshal.Copy(const Src: specialize TArray<UInt8>; StartIndex: SizeInt; Dest: TPtrWrapper; Count: SizeInt);
begin
  System.Move(PUInt8(Src)[StartIndex], Dest.Value^, Count * SizeOf(UInt8));
end;

class procedure TMarshal.Copy(Src: TPtrWrapper; var Dest: specialize TArray<UInt8>; StartIndex: SizeInt; Count: SizeInt);
begin
  System.Move(Src.Value^, PUInt8(Dest)[StartIndex], Count * SizeOf(UInt8));
end;

class procedure TMarshal.Copy(const Src: specialize TArray<Int8>; StartIndex: SizeInt; Dest: TPtrWrapper; Count: SizeInt);
begin
  System.Move(PInt8(Src)[StartIndex], Dest.Value^, Count * SizeOf(Int8));
end;

class procedure TMarshal.Copy(Src: TPtrWrapper; var Dest: specialize TArray<Int8>; StartIndex: SizeInt; Count: SizeInt);
begin
  System.Move(Src.Value^, PInt8(Dest)[StartIndex], Count * SizeOf(Int8));
end;

class procedure TMarshal.Copy(const Src: specialize TArray<UInt16>; StartIndex: SizeInt; Dest: TPtrWrapper; Count: SizeInt);
begin
  System.Move(PUInt16(Src)[StartIndex], Dest.Value^, Count * SizeOf(UInt16));
end;

class procedure TMarshal.Copy(Src: TPtrWrapper; var Dest: specialize TArray<UInt16>; StartIndex: SizeInt; Count: SizeInt);
begin
  System.Move(Src.Value^, PUInt16(Dest)[StartIndex], Count * SizeOf(UInt16));
end;

class procedure TMarshal.Copy(const Src: specialize TArray<Int16>; StartIndex: SizeInt; Dest: TPtrWrapper; Count: SizeInt);
begin
  System.Move(PInt16(Src)[StartIndex], Dest.Value^, Count * SizeOf(Int16));
end;

class procedure TMarshal.Copy(Src: TPtrWrapper; var Dest: specialize TArray<Int16>; StartIndex: SizeInt; Count: SizeInt);
begin
  System.Move(Src.Value^, PInt16(Dest)[StartIndex], Count * SizeOf(Int16));
end;

class procedure TMarshal.Copy(const Src: specialize TArray<Int32>; StartIndex: SizeInt; Dest: TPtrWrapper; Count: SizeInt);
begin
  System.Move(PInt32(Src)[StartIndex], Dest.Value^, Count * SizeOf(Int32));
end;

class procedure TMarshal.Copy(Src: TPtrWrapper; var Dest: specialize TArray<Int32>; StartIndex: SizeInt; Count: SizeInt);
begin
  System.Move(Src.Value^, PInt32(Dest)[StartIndex], Count * SizeOf(Int32));
end;

class procedure TMarshal.Copy(const Src: specialize TArray<Int64>; StartIndex: SizeInt; Dest: TPtrWrapper; Count: SizeInt);
begin
  System.Move(PInt64(Src)[StartIndex], Dest.Value^, Count * SizeOf(Int64));
end;

class procedure TMarshal.Copy(Src: TPtrWrapper; var Dest: specialize TArray<Int64>; StartIndex: SizeInt; Count: SizeInt);
begin
  System.Move(Src.Value^, PInt64(Dest)[StartIndex], Count * SizeOf(Int64));
end;

class procedure TMarshal.Copy(const Src: specialize TArray<TPtrWrapper>; StartIndex: SizeInt; Dest: TPtrWrapper; Count: SizeInt);
begin
  System.Move(PPointer(Src)[StartIndex], Dest.Value^, Count * SizeOf(TPtrWrapper));
end;

class procedure TMarshal.Copy(Src: TPtrWrapper; var Dest: specialize TArray<TPtrWrapper>; StartIndex: SizeInt; Count: SizeInt);
begin
  System.Move(Src.Value^, PPointer(Dest)[StartIndex], Count * SizeOf(TPtrWrapper));
end;

generic class function TMarshal.FixArray<T>(const Arr: specialize TArray<T>): TPtrWrapper;
begin
  Result.Value := nil;
  specialize TArray<T>(Result) := Arr;
end;

generic class procedure TMarshal.UnfixArray<T>(ArrPtr: TPtrWrapper);
begin
  Finalize(specialize TArray<T>(ArrPtr));
end;

class function TMarshal.FixString(var Str: UnicodeString): TPtrWrapper;
begin
  UniqueString(Str);
  Result := UnsafeFixString(Str);
end;

class procedure TMarshal.UnfixString(Ptr: TPtrWrapper);
begin
  if Ptr.Value <> PUnicodeChar('') then
    Finalize(UnicodeString(Ptr));
end;

class function TMarshal.UnsafeFixString(const Str: UnicodeString): TPtrWrapper;
begin
  if Length(Str) = 0 then
  begin
    Result.Value := PUnicodeChar('');
    Exit;
  end;
  Result.Value := nil;
  UnicodeString(Result) := Str;
end;

class function TMarshal.ReadByte(Ptr: TPtrWrapper; Ofs: SizeInt = 0): Byte;
begin
  Result := PByte(Ptr.Value + Ofs)^;
end;

class procedure TMarshal.WriteByte(Ptr: TPtrWrapper; Ofs: SizeInt; Value: Byte);
begin
  PByte(Ptr.Value + Ofs)^ := Value;
end;

class procedure TMarshal.WriteByte(Ptr: TPtrWrapper; Value: Byte);
begin
  PByte(Ptr.Value)^ := Value;
end;

class function TMarshal.ReadInt16(Ptr: TPtrWrapper; Ofs: SizeInt = 0): Int16;
begin
  Result := PInt16(Ptr.Value + Ofs)^;
end;

class procedure TMarshal.WriteInt16(Ptr: TPtrWrapper; Ofs: SizeInt; Value: Int16);
begin
  PInt16(Ptr.Value + Ofs)^ := Value;
end;

class procedure TMarshal.WriteInt16(Ptr: TPtrWrapper; Value: Int16);
begin
  PInt16(Ptr.Value)^ := Value;
end;

class function TMarshal.ReadInt32(Ptr: TPtrWrapper; Ofs: SizeInt = 0): Int32;
begin
  Result := PInt32(Ptr.Value + Ofs)^;
end;

class procedure TMarshal.WriteInt32(Ptr: TPtrWrapper; Ofs: SizeInt; Value: Int32);
begin
  PInt32(Ptr.Value + Ofs)^ := Value;
end;

class procedure TMarshal.WriteInt32(Ptr: TPtrWrapper; Value: Int32);
begin
  PInt32(Ptr.Value)^ := Value;
end;

class function TMarshal.ReadInt64(Ptr: TPtrWrapper; Ofs: SizeInt = 0): Int64;
begin
  Result := PInt64(Ptr.Value + Ofs)^;
end;

class procedure TMarshal.WriteInt64(Ptr: TPtrWrapper; Ofs: SizeInt; Value: Int64);
begin
  PInt64(Ptr.Value + Ofs)^ := Value;
end;

class procedure TMarshal.WriteInt64(Ptr: TPtrWrapper; Value: Int64);
begin
  PInt64(Ptr.Value)^ := Value;
end;

class function TMarshal.ReadPtr(Ptr: TPtrWrapper; Ofs: SizeInt = 0): TPtrWrapper;
begin
  Result.Value := PPointer(Ptr.Value + Ofs)^;
end;

class procedure TMarshal.WritePtr(Ptr: TPtrWrapper; Ofs: SizeInt; Value: TPtrWrapper);
begin
  PPointer(Ptr.Value + Ofs)^ := Value.Value;
end;

class procedure TMarshal.WritePtr(Ptr, Value: TPtrWrapper);
begin
  PPointer(Ptr.Value)^ := Value.Value;
end;

class function TMarshal.ReadStringAsAnsi(Ptr: TPtrWrapper; Len: SizeInt = -1): UnicodeString;
begin
  Result := ReadStringAsAnsi(DefaultSystemCodePage, Ptr, Len);
end;

class function TMarshal.ReadStringAsAnsi(CodePage: Word; Ptr: TPtrWrapper; Len: SizeInt = -1): UnicodeString;
begin
  { Here and below, IndexByte/Word assume that, when Len >= 0, either:
    - Up to Len characters are accessible in Ptr;
    - IndexByte/Word cannot access invalid memory past the searched character
      (e.g. i386.inc and x86_64.inc IndexByte/Word versions are specifically designed not to). }
  if Len < 0 then
    Len := IndexByte(Ptr.Value^, Len, 0);
  Result := ''; { Suppress warning. }
  WideStringManager.Ansi2UnicodeMoveProc(Ptr.Value, CodePage, Result, Len);
end;

class function TMarshal.ReadStringAsAnsiUpTo(CodePage: Word; Ptr: TPtrWrapper; MaxLen: SizeInt): UnicodeString;
var
  Len: SizeInt;
begin
  Len := IndexByte(Ptr.Value^, MaxLen, 0);
  if Len < 0 then
    Len := MaxLen;
  Result := ReadStringAsAnsi(CodePage, Ptr, Len);
end;

class procedure TMarshal.WriteStringAsAnsi(Ptr: TPtrWrapper; const Value: UnicodeString; MaxCharsIncNull: SizeInt);
begin
  WriteStringAsAnsi(Ptr, 0, Value, MaxCharsIncNull, DefaultSystemCodePage);
end;

class procedure TMarshal.WriteStringAsAnsi(Ptr: TPtrWrapper; const Value: UnicodeString; MaxCharsIncNull: SizeInt; CodePage: Word);
begin
  WriteStringAsAnsi(Ptr, 0, Value, MaxCharsIncNull, CodePage);
end;

class procedure TMarshal.WriteStringAsAnsi(Ptr: TPtrWrapper; Ofs: SizeInt; const Value: UnicodeString; MaxCharsIncNull: SizeInt);
begin
  WriteStringAsAnsi(Ptr, Ofs, Value, MaxCharsIncNull, DefaultSystemCodePage);
end;

class procedure TMarshal.WriteStringAsAnsi(Ptr: TPtrWrapper; Ofs: SizeInt; const Value: UnicodeString; MaxCharsIncNull: SizeInt; CodePage: Word);
var
  U2ARes: AnsiString;
  ValueLen, U2AResLen: SizeInt;
begin
  U2ARes := ''; { Suppress warning. }
  ValueLen := Length(Value);
  { Delphi null-terminates iff MaxCharsIncNull < 0, so MaxCharsIncNull is actually just MaxChars. }
  if (MaxCharsIncNull > 0) and (MaxCharsIncNull < ValueLen) then
    ValueLen := MaxCharsIncNull; { UTF-16 → ANSI should never shrink element count, so limit the number of characters analyzed. }
  WideStringManager.Unicode2AnsiMoveProc(PUnicodeChar(Pointer(Value)), U2ARes, CodePage, ValueLen);
  U2AResLen := Length(U2ARes);
  if (MaxCharsIncNull >= 0) and (MaxCharsIncNull < U2AResLen) then
    U2AResLen := MaxCharsIncNull;
  System.Move(PAnsiChar(Pointer(U2ARes))^, (Ptr.Value + Ofs)^, U2AResLen * SizeOf(AnsiChar));
  if MaxCharsIncNull < 0 then
    PAnsiChar(Ptr.Value + Ofs)[U2AResLen] := #0;
end;

class function TMarshal.ReadStringAsUnicode(Ptr: TPtrWrapper; Len: SizeInt = -1): UnicodeString;
begin
  if Len < 0 then
    Len := Length(PUnicodeChar(Ptr.Value));
  Result := ''; { Suppress warning. }
  SetLength(Result, Len);
  System.Move(Ptr.Value^, Pointer(Result)^, Len * SizeOf(UnicodeChar));
end;

class function TMarshal.ReadStringAsUnicodeUpTo(Ptr: TPtrWrapper; MaxLen: SizeInt): UnicodeString;
var
  Len: SizeInt;
begin
  Len := IndexWord(Ptr.Value^, MaxLen, 0);
  if Len < 0 then
    Len := MaxLen;
  Result := ReadStringAsUnicode(Ptr, Len);
end;

class procedure TMarshal.WriteStringAsUnicode(Ptr: TPtrWrapper; const Value: UnicodeString; MaxCharsIncNull: SizeInt);
begin
  WriteStringAsUnicode(Ptr, 0, Value, MaxCharsIncNull);
end;

class procedure TMarshal.WriteStringAsUnicode(Ptr: TPtrWrapper; Ofs: SizeInt; const Value: UnicodeString; MaxCharsIncNull: SizeInt);
var
  Len: SizeInt;
begin
  { Again, Delphi null-terminates iff MaxCharsIncNull < 0, so MaxCharsIncNull is actually just MaxChars. }
  Len := Length(Value);
  if (MaxCharsIncNull >= 0) and (MaxCharsIncNull < Len) then
    Len := MaxCharsIncNull;
  System.Move(Pointer(Value)^, (Ptr.Value + Ofs)^, Len * SizeOf(UnicodeChar));
  if MaxCharsIncNull < 0 then
    PUnicodeChar(Ptr.Value + Ofs)[Len] := #0;
end;

class function TMarshal.ReadStringAsUtf8(Ptr: TPtrWrapper; Len: SizeInt = -1): UnicodeString;
begin
  Result := ReadStringAsAnsi(CP_UTF8, Ptr, Len);
end;

class function TMarshal.ReadStringAsUtf8UpTo(Ptr: TPtrWrapper; MaxLen: SizeInt): UnicodeString;
begin
  Result := ReadStringAsAnsiUpTo(CP_UTF8, Ptr, MaxLen);
end;

class procedure TMarshal.WriteStringAsUtf8(Ptr: TPtrWrapper; const Value: UnicodeString; MaxCharsIncNull: SizeInt);
begin
  WriteStringAsAnsi(Ptr, Value, MaxCharsIncNull, CP_UTF8);
end;

class procedure TMarshal.WriteStringAsUtf8(Ptr: TPtrWrapper; Ofs: SizeInt; const Value: UnicodeString; MaxCharsIncNull: SizeInt);
begin
  WriteStringAsAnsi(Ptr, Ofs, Value, MaxCharsIncNull, CP_UTF8);
end;

constructor TMarshaller.TState.Create;
begin
  inherited Create;
  DeferHead.Alloc := sizeof(TDeferQueueNode.StaticStore);
  DeferTail := @DeferHead;
end;

destructor TMarshaller.TState.Destroy;
begin
  Flush;
  inherited Destroy;
end;

procedure TMarshaller.TState.Flush;
begin
  try
    FlushQueue;
  finally
    ClearQueue;
  end;
end;

procedure TMarshaller.TState.FlushQueue;
var
  Qn: PDeferQueueNode;
  D: PDeferBase;
  Pos: SizeInt;
begin
  Qn := @DeferHead;
  repeat
    Pos := 0;
    while Pos < Qn^.Used do
    begin
      Pointer(D) := Pointer(PByte(Qn^.Mem)) + Pos;
      Pos := Pos + SizeOf(D^); { This is runtime SizeOf of the actual instance that accesses VMT. }
      D^.Done;
    end;
    Qn := Qn^.Next;
  until not Assigned(Qn);
end;

procedure TMarshaller.TState.ClearQueue;
var
  Qn, Nx: PDeferQueueNode;
begin
  Qn := DeferHead.Next;
  DeferHead.Next := nil;
  DeferHead.Used := 0;
  while Assigned(Qn) do
  begin
    Nx := Qn^.Next;
    System.FreeMem(Qn);
    Qn := Nx;
  end;
end;

procedure TMarshaller.TState.NotePointerChanged(OldPtr, NewPtr: TPtrWrapper);
var
  Qn: PDeferQueueNode;
  D: PDeferBase;
  Pos: SizeInt;
begin
  Qn := @DeferHead;
  repeat
    Pos := 0;
    while Pos < Qn^.Used do
    begin
      Pointer(D) := Pointer(PByte(Qn^.Mem)) + Pos;
      Pos := Pos + SizeOf(D^); { This is runtime SizeOf of the actual instance that accesses VMT. }
      D^.NotePointerChanged(OldPtr, NewPtr);
    end;
    Qn := Qn^.Next;
  until not Assigned(Qn);
end;

constructor TMarshaller.TDeferBase.Init;
begin
end;

procedure TMarshaller.TDeferBase.NotePointerChanged(OldPtr, NewPtr: TPtrWrapper);
begin
end;

destructor TMarshaller.TDeferFreeMem.Done;
begin
  TMarshal.FreeMem(P);
  P := TPtrWrapper.NilValue;
end;

procedure TMarshaller.TDeferFreeMem.NotePointerChanged(OldPtr, NewPtr: TPtrWrapper);
begin
  if P = OldPtr then
    P := NewPtr;
end;

class procedure TMarshaller.TAddressableUnfixArraySpecialization.UnfixArray(ArrPtr: TPtrWrapper);
begin
  TMarshal.specialize UnfixArray<T>(ArrPtr);
end;

destructor TMarshaller.TDeferUnfix.Done;
begin
  if Assigned(P.Value) then
    Unfix(P);
  P := TPtrWrapper.NilValue;
end;

destructor TMarshaller.TDeferMoveToSBAndFree.Done;
begin
  try
    if Assigned(SB) then
    begin
      SB.Clear;
      SB.Append(TMarshal.ReadStringAsUnicodeUpTo(Src, MaxLen));
    end;
  finally
    TMarshal.FreeMem(Src);
    Src := TPtrWrapper.NilValue;
  end;
end;

function TMarshaller.PushDefer(InstanceSize: SizeInt): PDeferBase;
var
  Qn: PDeferQueueNode;
  Alloc: SizeInt;
begin
  { Careful: FState starts uninitialized, Assigned(FStateLife) must be used rather than Assigned(FState). }
  if not Assigned(FStateLife) then
  begin
    FState := TState.Create;
    FStateLife := FState;
  end;
  Qn := FState.DeferTail;
  if InstanceSize <= Qn^.Alloc - Qn^.Used then
  begin
    { Enough space. }
    Result := Pointer(PByte(Qn^.Mem)) + Qn^.Used;
    Qn^.Used := Qn^.Used + InstanceSize;
  end else
  begin
    { Not enough space; allocate new node. }
    Alloc := InstanceSize + Qn^.Alloc + SizeInt(SizeUint(Qn^.Alloc) div 2);
    Qn := GetMem(SizeOf(TDeferQueueNode) - SizeOf(TDeferQueueNode.StaticStore) + Alloc);
    Qn^.Used := InstanceSize;
    Qn^.Alloc := Alloc;
    Qn^.Next := nil;
    FState.DeferTail^.Next := Qn;
    FState.DeferTail := Qn;
    Result := Pointer(PByte(Qn^.Mem));
  end;
end;

procedure TMarshaller.Flush;
begin
  if Assigned(FStateLife) then
    FState.Flush;
end;

function TMarshaller.AllocMem(Size: SizeInt): TPtrWrapper;
var
  D: PDeferFreeMem;
begin
  Pointer(D) := PushDefer(SizeOf(TDeferFreeMem));
  D^.Init;
  Result := TMarshal.AllocMem(Size);
  D^.P := Result;
end;

function TMarshaller.ReallocMem(OldPtr: TPtrWrapper; NewSize: NativeInt): TPtrWrapper;
begin
  if not Assigned(OldPtr.Value) then
    Exit(AllocMem(NewSize));
  Result := TMarshal.ReallocMem(OldPtr, NewSize);
  if (Result <> OldPtr) and Assigned(FStateLife) then
    FState.NotePointerChanged(OldPtr, Result);
end;

function TMarshaller.AllocStringAsAnsi(const Str: UnicodeString): TPtrWrapper;
begin
  Result := AllocStringAsAnsi(PUnicodeChar(Pointer(Str)), Length(Str), DefaultSystemCodePage);
end;

function TMarshaller.AllocStringAsAnsi(const Str: UnicodeString; CodePage: Word): TPtrWrapper;
begin
  Result := AllocStringAsAnsi(PUnicodeChar(Pointer(Str)), Length(Str), CodePage);
end;

function TMarshaller.AllocStringAsUnicode(const Str: UnicodeString): TPtrWrapper;
var
  D: PDeferFreeMem;
begin
  Pointer(D) := PushDefer(SizeOf(TDeferFreeMem));
  D^.Init;
  Result := TMarshal.AllocStringAsUnicode(Str);
  D^.P := Result;
end;

function TMarshaller.AllocStringAsUtf8(const Str: UnicodeString): TPtrWrapper;
begin
  Result := AllocStringAsAnsi(Str, CP_UTF8);
end;

function TMarshaller.AsAnsi(const S: UnicodeString): TPtrWrapper;
begin
  Result := AllocStringAsAnsi(PUnicodeChar(Pointer(S)), Length(S), DefaultSystemCodePage);
end;

function TMarshaller.AsAnsi(S: PUnicodeChar): TPtrWrapper;
begin
  Result := AllocStringAsAnsi(S, Length(S), DefaultSystemCodePage);
end;

function TMarshaller.AsAnsi(const S: UnicodeString; CodePage: Word): TPtrWrapper;
begin
  Result := AllocStringAsAnsi(PUnicodeChar(Pointer(S)), Length(S), CodePage);
end;

function TMarshaller.AsAnsi(S: PUnicodeChar; CodePage: Word): TPtrWrapper;
begin
  Result := AllocStringAsAnsi(S, Length(S), CodePage);
end;

function TMarshaller.AsUtf8(const S: UnicodeString): TPtrWrapper;
begin
  Result := AllocStringAsAnsi(PUnicodeChar(Pointer(S)), Length(S), CP_UTF8);
end;

function TMarshaller.AsUtf8(S: PUnicodeChar): TPtrWrapper;
begin
  Result := AllocStringAsAnsi(S, Length(S), CP_UTF8);
end;

function TMarshaller.AllocStringAsAnsi(S: PUnicodeChar; Len: SizeInt; CodePage: Word): TPtrWrapper;
var
  D: PDeferFreeMem;
begin
  Pointer(D) := PushDefer(SizeOf(TDeferFreeMem));
  D^.Init;
  Result := TMarshal.AllocStringAsAnsi(S, Len, CodePage);
  D^.P := Result;
end;

function TMarshaller.AsRaw(const B: TBytes): TPtrWrapper;
begin
  Result := specialize FixArray<Byte>(B);
end;

generic function TMarshaller.FixArray<T>(const Arr: specialize TArray<T>): TPtrWrapper;
var
  D: PDeferUnfix;
begin
  Pointer(D) := PushDefer(SizeOf(TDeferUnfix));
  D^.Init;
  Result := TMarshal.specialize FixArray<T>(Arr);
  D^.Unfix := @specialize TAddressableUnfixArraySpecialization<T>.UnfixArray;
  D^.P := Result;
end;

function TMarshaller.FixString(var Str: UnicodeString): TPtrWrapper;
var
  D: PDeferUnfix;
begin
  Pointer(D) := PushDefer(SizeOf(TDeferUnfix));
  D^.Init;
  Result := TMarshal.FixString(Str);
  D^.Unfix := @TMarshal.UnfixString;
  D^.P := Result;
end;

function TMarshaller.UnsafeFixString(const Str: UnicodeString): TPtrWrapper;
var
  D: PDeferUnfix;
begin
  Pointer(D) := PushDefer(SizeOf(TDeferUnfix));
  D^.Init;
  Result := TMarshal.UnsafeFixString(Str);
  D^.Unfix := @TMarshal.UnfixString;
  D^.P := Result;
end;

function TMarshaller.InString(SB: TUnicodeStringBuilder; MaxLen: SizeInt): TPtrWrapper;
var
  D: PDeferMoveToSBAndFree;
begin
  Pointer(D) := PushDefer(SizeOf(TDeferMoveToSBAndFree));
  D^.Init;
  Result := TMarshal.AllocMem((MaxLen + 1) * SizeOf(UnicodeChar));
  D^.Src := Result;
  D^.SB := SB;
  D^.MaxLen := MaxLen;
end;

function TMarshaller.OutString(const S: UnicodeString): TPtrWrapper;
var
  TS: UnicodeString;
begin
  TS := S;
  Result := FixString(TS);
end;

function TMarshaller.InOutString(SB: TUnicodeStringBuilder; MaxLen: SizeInt): TPtrWrapper;
var
  D: PDeferMoveToSBAndFree;
  NCopy: SizeInt;
begin
  Pointer(D) := PushDefer(SizeOf(TDeferMoveToSBAndFree));
  D^.Init;
  Result := TMarshal.AllocMem((MaxLen + 1) * SizeOf(UnicodeChar));
  D^.Src := Result;
  NCopy := SB.Length;
  if MaxLen < NCopy then
    NCopy := MaxLen;
  TMarshal.WriteStringAsUnicode(Result, SB.ToString(0, NCopy), NCopy);
  D^.SB := SB;
  D^.MaxLen := MaxLen;
end;
