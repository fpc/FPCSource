{******************************************************************************
    $Id$
    Copyright (c) 2000 by Florian Klaempfl

    Includes the iSPARC code generator

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

 *****************************************************************************}
unit CpuNode;
{$INCLUDE fpcdefs.inc}
interface
{This unit is used to define the specific CPU implementations. All needed
actions are included in the INITALIZATION part of these units. This explains
the behaviour of such a unit having just a USES clause!}
implementation
uses
  ncgbas,ncgflw,ncgcnv,ncgld,ncgmem,ncgcon,{ncgset,}
  ncpuadd,ncpucall,{n386con,n386cnv,n386flw,}ncpumat,{n386mem,}
  {n386set,}ncpuinln,{n386opt,}ncpucnv,
  { this not really a node }
  {ncpuobj,}rgcpu;
end.
{
    $Log$
    Revision 1.6  2002-12-21 23:21:47  mazen
    + added support for the shift nodes
    + added debug output on screen with -an command line option

    Revision 1.5  2002/11/30 20:03:29  mazen
    + ncpuinln node

}
