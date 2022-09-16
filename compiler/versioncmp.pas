{
    Copyright (c) 2022 by Jonas Maebe

    Target OS version comparisons

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
unit versioncmp;

{$i fpcdefs.inc}

interface

  type
    tversion = object
     private
      fstr: string;
      fnum: cardinal;
     public
      { initialise with string and numerical representation of the version }
      constructor init(const str: string; major: byte; minor: word; patch: byte);
      constructor invalidate;
      function relationto(const other: tversion): shortint;
      function relationto(major: byte; minor: word; patch: byte): shortint;
      function isvalid: boolean;
      property str: string read fstr;
     private
      function tonum(major: byte; minor: word; patch: byte): cardinal; inline;
    end;

implementation

  function tversion.tonum(major: byte; minor: word; patch: byte): cardinal;
    begin
      result:=(major shl 24) or (minor shl 8) or patch;
    end;


  constructor tversion.init(const str: string; major: byte; minor: word; patch: byte);
    begin
      fstr:=str;
      fnum:=tonum(major,minor,patch);
    end;


  constructor tversion.invalidate;
    begin
      fstr:='';
      fnum:=0;
    end;


  function tversion.relationto(const other: tversion): shortint;
    begin
      if fnum>other.fnum then
        result:=1
      else if fnum<other.fnum then
        result:=-1
      else
        result:=0;
    end;


  function tversion.relationto(major: byte; minor: word; patch: byte): shortint;
    var
      othernum: cardinal;
    begin
      othernum:=tonum(major,minor,patch);
      if fnum>othernum then
        result:=1
      else if fnum<othernum then
        result:=-1
      else
        result:=0;
    end;


  function tversion.isvalid: boolean;
    begin
      result:=fstr<>'';
    end;



end.

