{
  Dummy process.inc
}

{
  prevent compilation error for the versions mentioned below
} 
{$if defined(VER2_4) or defined(VER2_5_1)}
{$warning Temporary workaround - unit does nothing}
{$else}
{$fatal Proper implementation of TProcess for version of this target needed}
{$endif}

procedure TProcess.CloseProcessHandles;
begin
end;

Function TProcess.PeekExitStatus : Boolean;
begin
end;

Procedure TProcess.Execute;
begin
end;

Function TProcess.WaitOnExit : Boolean;
begin
  Result:=False;
end;

Function TProcess.Suspend : Longint;
begin
  Result:=0;
end;

Function TProcess.Resume : LongInt;

begin
  Result:=0;
end;

Function TProcess.Terminate(AExitCode : Integer) : Boolean;
begin
  Result:=False;
end;

Procedure TProcess.SetShowWindow (Value : TShowWindowOptions);
begin
end;


