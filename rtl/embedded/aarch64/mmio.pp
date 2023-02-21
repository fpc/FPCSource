{
    Copyright (c) 1998-2022 by the Free Pascal development team

    Memory mapped IO helpers

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
{$IFNDEF FPC_DOTTEDUNITS}
unit mmio;
{$ENDIF FPC_DOTTEDUNITS}

interface

procedure DUMMY(Count: DWord);
procedure PUT32(Address: DWord; Value: DWord); inline;
function GET32(Address: DWord) : DWord; inline;

implementation

procedure DUMMY(Count: DWord);
var
    i : DWord;
begin
    for i := 0 to Count do
    begin
        asm
            nop
        end;
    end;
end;

procedure PUT32(Address: DWord; Value: DWord); inline;
VAR
    p: ^DWord;
begin
    p := POINTER (Address);
    p^ := Value;
end;

function GET32(Address: DWord) : DWord; inline;
VAR
    p: ^DWord;
begin
    p := POINTER (Address);
    GET32 := p^;
end;

procedure PUT64(Address: QWord; Value: QWord); inline;
VAR
    p: ^QWord;
begin
    p := POINTER (Address);
    p^ := Value;
end;

function GET64(Address: QWord) : QWord; inline;
VAR
    p: ^QWord;
begin
    p := POINTER (Address);
    GET64 := p^;
end;

end.
