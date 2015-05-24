{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2015 by Karoly Balogh,
    member of the Free Pascal development team.

    m68k/Amiga atomic operations implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ The Amiga hardware doesn't support the m68k CPU's atomic operations
  like TAS, CAS, CAS2 and so on. Therefore we must "emulate" them from
  software. The easiest way is the Forbid()/Permit() OS call pair around
  the ops themselves. It of course won't be hardware-atomic, but should
  be safe for multithreading. (KB) }

function InterLockedDecrement (var Target: longint) : longint;
  begin
    Forbid();
    Dec(Target);
    Result := Target;
    Permit();
  end;


function InterLockedIncrement (var Target: longint) : longint;
  begin
    Forbid()
    Inc(Target);
    Result := Target;
    Permit();
  end;


function InterLockedExchange (var Target: longint;Source : longint) : longint;
  begin
    Forbid();
    Result := Target;
    Target := Source;
    Permit();
  end;


function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint;
  begin
    Forbid();
    Result := Target;
    Target := Target + Source;
    Permit();
  end;


function InterlockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint;
  begin
    Forbid();
    Result := Target;
    if Target = Comperand then
      Target := NewValue;
    Permit();
  end;
