{
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

 ****************************************************************************
}
Unit cpuinfo;

{$i fpcdefs.inc}

Interface

  uses
    globtype;

Type
   bestreal = extended;
   ts32real = single;
   ts64real = double;
   ts80real = extended;
   ts128real = type extended;
   ts64comp = type extended;

   pbestreal=^bestreal;

   tprocessors =
      (no_processor,
       ClassAthlon64
      );

   tfputype =
     (no_fpuprocessor,
      fpu_sse64
     );

Const
   { Size of native extended type }
   extended_size = 10;
   { Size of a multimedia register }
   mmreg_size = 16;
   { target cpu string (used by compiler options) }
   target_cpu_string = 'x86_64';

   { calling conventions supported by the code generator }
   supported_calling_conventions : tproccalloptions = [
     pocall_internproc,
{     pocall_compilerproc,
     pocall_inline,}
     pocall_register,
     pocall_safecall,
     pocall_stdcall,
     pocall_cdecl,
     pocall_cppdecl
   ];

   processorsstr : array[tprocessors] of string[10] = ('',
     'ATHLON64'
   );

   fputypestr : array[tfputype] of string[6] = ('',
     'SSE64'
   );

   sse_singlescalar : set of tfputype = [fpu_sse64];
   sse_doublescalar : set of tfputype = [fpu_sse64];

Implementation

end.
