{ Thread management routines }

constructor TThread.Create(CreateSuspended: Boolean;
                           const StackSize: SizeUInt = DefaultStackSize);
begin
  inherited Create;
end;


destructor TThread.Destroy;
begin
  inherited Destroy;
end;

procedure TThread.CallOnTerminate;
begin
  FOnTerminate(Self);
end;

procedure TThread.DoTerminate;
begin
  if Assigned(FOnTerminate) then
    Synchronize(@CallOnTerminate);
end;

function TThread.GetPriority: TThreadPriority;
begin
  Result := tpNormal;
end;

procedure TThread.SetPriority(Value: TThreadPriority);
begin
end;


procedure TThread.SetSuspended(Value: Boolean);
begin
end;

procedure TThread.Suspend;
begin
end;

procedure TThread.Resume;
begin
end;

procedure TThread.Terminate;
begin
  FTerminated := True;
end;

function TThread.WaitFor: Integer;
begin
  Result := -1;
end;
