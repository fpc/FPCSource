{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2014 by Olivier Coursière
    
    Based on nativeNT target by Sven Barth

    Classes unit for UEFI

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ Thread management routines }

procedure TThread.SysCreate(CreateSuspended: Boolean;
                            const StackSize: SizeUInt);
begin

end;


procedure TThread.SysDestroy;
begin

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
