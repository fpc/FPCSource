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
  Revision 1.4  2000-01-07 01:24:34  peter
    * updated copyright to 2000

  Revision 1.3  2000/01/06 01:20:34  peter
    * moved out of packages/ back to topdir

  Revision 1.1  2000/01/03 19:33:09  peter
    * moved to packages dir

  Revision 1.1  1999/05/30 10:46:43  peter
    * start of tthread for linux,win32

  Revision 1.2  1999/04/08 10:18:57  peter
    * makefile updates

}
