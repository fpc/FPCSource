{*****************************************************************************}
{ File                   : cpuinfo.pas                                        }
{ Author                 : Mazen NEIFER                                       }
{ Project                : Free Pascal Compiler (FPC)                         }
{ Creation date          : 2002\26\26                                         }
{ Last modification date : 2002\08\20                                         }
{ Licence                : GPL                                                }
{ Bug report             : mazen.neifer.01@supaero.org                        }
{*****************************************************************************}
{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Basic Processor information

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
UNIT cpuinfo;
{$INCLUDE fpcdefs.inc}
INTERFACE
TYPE
{# Natural integer register type and size for the target machine }
  AWord=Cardinal;
  PAWord=^AWord;
{ the ordinal type used when evaluating constant integer expressions }
  TConstExprInt=int64;
{ this must be an ordinal type with the same size as a pointer }
{ Note: must be unsigned!! Otherwise, ugly code like           }
{ pointer(-1) will result in a pointer with the value          }
{ $fffffffffffffff on a 32bit machine if the compiler uses     }
{ int64 constants internally (JM)                              }
  TConstPtrUInt=cardinal;
  bestreal = extended;
  ts32real = single;
  ts64real = double;
  ts80real = extended;
  ts64comp = extended;
  pbestreal=^bestreal;
{ possible supported processors for this target }
  tprocessors=(no_processor,SPARC_V8,SPARC_V9);
CONST
{# Size of native extended floating point type }
  extended_size = 10;
{# Size of a pointer                           }
  pointer_size  = 4;
{# Size of a multimedia register               }
  mmreg_size = 8;
{ target cpu string (used by compiler options) }
  target_cpu_string = 'SPARC';
{ size of the buffer used for setjump/longjmp
  the size of this buffer is deduced from the
  jmp_buf structure in setjumph.inc file }
  jmp_buf_size = 24;
IMPLEMENTATION
END.
