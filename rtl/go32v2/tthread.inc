{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{****************************************************************************}
{*                             TThread                                      *}
{****************************************************************************}


procedure TThread.CallOnTerminate;

begin
end;


function TThread.GetPriority: TThreadPriority;

begin
  GetPriority:=tpNormal;
end;


procedure TThread.SetPriority(Value: TThreadPriority);

begin
end;


procedure TThread.SetSuspended(Value: Boolean);

begin
end;


procedure TThread.DoTerminate;

begin
end;


constructor TThread.Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);

begin
 {IsMultiThread := TRUE; }
end;


destructor TThread.Destroy;

begin
end;


procedure TThread.Resume;

begin
end;


procedure TThread.Suspend;

begin
end;


procedure TThread.Terminate;

begin
end;


function TThread.WaitFor: Integer;

begin
  WaitFor:=0;
end;


