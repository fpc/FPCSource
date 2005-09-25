{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


constructor TMultiReadExclusiveWriteSynchronizer.Create;
begin
  System.InitCriticalSection(Crit);
end;

destructor TMultiReadExclusiveWriteSynchronizer.Destroy;
begin
  System.DoneCriticalSection(Crit);
end;

function  TMultiReadExclusiveWriteSynchronizer.Beginwrite : boolean;
begin
  System.EnterCriticalSection(Crit);
  result:=true;
end;

procedure  TMultiReadExclusiveWriteSynchronizer.Endwrite;
begin
  System.LeaveCriticalSection(Crit);
end;

procedure  TMultiReadExclusiveWriteSynchronizer.Beginread;
begin
  System.EnterCriticalSection(Crit);
end;

procedure  TMultiReadExclusiveWriteSynchronizer.Endread;
begin
  System.LeaveCriticalSection(Crit);
end;
