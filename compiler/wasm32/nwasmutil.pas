{
    Copyright (c) 2021 by Nikolay Nikolov

    WebAssembly version of some node tree helper routines

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
unit nwasmutil;

{$i fpcdefs.inc}

interface

  uses
    ngenutil;

  type

    { twasmnodeutils }

    twasmnodeutils = class(tnodeutils)
    public
      class procedure InsertObjectInfo; override;
    end;

implementation

  { twasmnodeutils }

  class procedure twasmnodeutils.InsertObjectInfo;
    begin
      inherited;
    end;

begin
  cnodeutils:=twasmnodeutils;
end.
