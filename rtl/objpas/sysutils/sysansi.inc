{
    *********************************************************************
    $Id$
    Copyright (C) 2002 by Florian Klaempfl

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
    *********************************************************************
}

function AnsiCompareFileName(const S1, S2 : Ansistring) : Longint;
  begin
  end;

function AnsiLowerCaseFileName(const s : string) : Ansistring;
  begin
  end;

function AnsiUpperCaseFileName(const s : string) : Ansistring;
  begin
  end;

function AnsiPos(const substr,s : string) : Longint;
  begin
  end;

function AnsiStrPos(str,substr : PChar) : PChar;
  begin
  end;

function AnsiStrRScan(Str : PChar;Chr : Char) : PChar;
  begin
  end;

function AnsiStrScan(Str : PChar;Chr: Char) : PChar;
  begin
  end;

{
  $Log$
  Revision 1.1  2003-10-06 21:01:06  peter
    * moved classes unit to rtl

  Revision 1.1  2002/10/07 19:43:24  florian
    + empty prototypes for the AnsiStr* multi byte functions added
}
