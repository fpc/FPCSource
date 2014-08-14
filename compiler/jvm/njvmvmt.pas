{
    Copyright (c) 2014 by Jonas Maebe

    Generate JVM bytecode for in set/case nodes

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
unit njvmvmt;

{$i fpcdefs.inc}

interface

    uses
      ncgvmt;

    type
      tjvmvmtwriter = class(TVMTWriter)
        class function use_vmt_writer: boolean; override;
      end;


implementation


{*****************************************************************************
                             TJVMVMTWRITER
*****************************************************************************}


  class function tjvmvmtwriter.use_vmt_writer: boolean;
    begin
      result:=false;
    end;

begin
  CVMTWriter:=tjvmvmtwriter;
end.
