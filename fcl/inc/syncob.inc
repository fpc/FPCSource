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

procedure TSyncroObject.Acquire;
  begin
  end;
  
  
procedure TSyncroObject.Release;
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
