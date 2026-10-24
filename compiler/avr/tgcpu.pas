{
    Copyright (C) 1998-2000 by Florian Klaempfl

    This unit handles the temporary variables stuff for AVR.

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
unit tgcpu;

{$i fpcdefs.inc}

  interface

    uses
      tgobj,globtype,aasmdata,cgutils,symtype;

    type
      ttgavr = class(ttgobj)
      public
        procedure setfirsttemp(l: asizeint); override;
      end;

implementation

uses
  globals,
  verbose,
  cpubase,
  cutils;

procedure ttgavr.setfirsttemp(l: asizeint);
  begin
    { this is a negative value normally }
    if l*direction<0 then
      internalerror(20260929);
    firsttemp:=l;
    lasttemp:=l;
{$ifdef EXTDEBUG}
    Comment(V_Note,'ttavr: (SetFirstTempGen) set to '+tostr(l));
{$endif}
  end;

begin
  tgobjclass:=ttgavr;
end.
