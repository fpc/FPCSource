{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998-2000 by the Free Pascal development team

    Basic Processor information about the Alpha

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
  Basic Processor information about the Alpha
}
Unit CPUInfo;

{$i fpcdefs.inc}

Interface

Type
   { Natural integer register type and size for the target machine }
{$ifdef FPC}
   AWord = Qword;
{$else FPC}
   AWord = Longint;
{$endif FPC}
   PAWord = ^AWord;

   { This must be an ordinal type with the same size as a pointer
     Note: Must be unsigned! Otherwise, ugly code like
     pointer(-1) will result in a pointer with the value
     $fffffffffffffff on a 32bit machine if the compiler uses
     int64 constants internally (JM)                              }
   TConstPtrUInt = qword;

   bestreal = extended;
   ts32real = single;
   ts64real = double;
   ts80real = extended;
   ts64comp = extended;

   pbestreal=^bestreal;

   { possible supported processors for this target }
   tcputype =
      (cpu_none,
       ClassEV7,
       ClassEV8
      );

Const
   { Size of native extended type }
   extended_size = 16;
   {# Size of a pointer                           }
   sizeof(aint)  = 8;
   {# Size of a multimedia register               }
   mmreg_size = 8;

   { target cpu string (used by compiler options) }
   target_cpu_string = 'alpha';

Implementation

end.
