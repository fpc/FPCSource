{*****************************************************************************}
{ File                   : cpunode.pas                                        }
{ Author                 : Mazen NEIFER                                       }
{ Project                : Free Pascal Compiler (FPC)                         }
{ Creation date          : 2002\26\26                                         }
{ Last modification date : 2002\07\14                                         }
{ Licence                : GPL                                                }
{ Bug report             : mazen.neifer.01@supaero.org                        }
{*****************************************************************************}
{
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

 ****************************************************************************}
UNIT cpunode;
{$INCLUDE fpcdefs.inc}
INTERFACE
{This unit is used to define the specific CPU implementations. All needed
actions are included in the INITALIZATION part of these units. This explains
the behaviour of such a unit having just a USES clause!}
IMPLEMENTATION
USES
  ncgbas,ncgflw,ncgcnv,ncgld,ncgmem,ncgcon,{ncgset,}
  naddcpu,ncpucall,{n386con,n386cnv,n386flw,n386mat,n386mem,}
  {n386set,n386inl,n386opt,}ncpucnv,
  { this not really a node }
  {nSPARCobj,}rgcpu;
END.
