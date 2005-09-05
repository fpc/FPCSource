{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure TSynchroObject.Acquire;
begin
end;

procedure TSynchroObject.Release;
begin
end;

procedure TCriticalSection.Enter;
begin
  Acquire;
end;


procedure TCriticalSection.Leave;
begin
  Release;
end;

constructor TMultiReadExclusiveWriteSynchronizer.Create;

begin
  Crit:=tcriticalsection.create;
end;

destructor TMultiReadExclusiveWriteSynchronizer.Destroy;
begin
  Crit.free;
end;

procedure  TMultiReadExclusiveWriteSynchronizer.Beginwrite;
begin
  Crit.acquire;
end;

procedure  TMultiReadExclusiveWriteSynchronizer.Endwrite;
begin
  Crit.release;
end;

procedure  TMultiReadExclusiveWriteSynchronizer.Beginread;
begin
  Crit.acquire;
end;

procedure  TMultiReadExclusiveWriteSynchronizer.Endread;
begin
  Crit.acquire;
end;

