{
    $Id: aoptcpu.pas,v 1.7 2005/02/26 01:27:00 jonas Exp $
    Copyright (c) 1998-2002 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit implements the PowerPC optimizer object

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


Unit aoptcpu;

Interface

{$i fpcdefs.inc}

uses cpubase, aoptobj, aoptcpub, aopt;

Type
  TCpuAsmOptimizer = class(TAsmOptimizer)
    { uses the same constructor as TAopObj }
  End;

Implementation

begin
  casmoptimizer:=TCpuAsmOptimizer;
End.
{
 $Log: aoptcpu.pas,v $
 Revision 1.7  2005/02/26 01:27:00  jonas
   * fixed generic jumps optimizer and enabled it for ppc (the label table
     was not being initialised -> getfinaldestination always failed, which
     caused wrong optimizations in some cases)
   * changed the inverse_cond into a function, because tasmcond is a record
     on ppc
   + added a compare_conditions() function for the same reason

 Revision 1.6  2005/02/14 17:13:10  peter
   * truncate log

}
