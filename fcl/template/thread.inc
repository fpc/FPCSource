{
    $Id$
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


procedure TThread.Synchronize(Method: TThreadMethod);

begin
end;


constructor TThread.Create(CreateSuspended: Boolean);

begin
  { Should set the following variable to TRUE before }
  { creating the new thread:                         }
  IsMultiThread := TRUE;
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


{
  $Log$
  Revision 1.3  2001-10-09 02:19:25  carl
  * bugfix #1639 (IsMultiThread varialbe setting)

  Revision 1.2  2000/07/13 11:33:03  michael
  + removed logs
 
}
