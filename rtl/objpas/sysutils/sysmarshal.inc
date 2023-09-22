
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
