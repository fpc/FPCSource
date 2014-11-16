{
    Copyright (c) 1998-2002 by the Free Pascal development team

    Basic Processor information for the virtual instruction set

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Unit CPUInfo;

Interface

uses
   globtype;

Type
   { Architecture word - Native unsigned type }
   AWord  = Longword;
   PAWord = ^AWord;

   { this must be an ordinal type with the same size as a pointer }
   { to allow some dirty type casts for example when using        }
   { tconstsym.value                                              }
   { Note: must be unsigned!! Otherwise, ugly code like           }
   { pointer(-1) will result in a pointer with the value          }
   { $fffffffffffffff on a 32bit machine if the compiler uses     }
   { int64 constants internally (JM)                              }
   TConstPtrUInt = Longword;

   bestreal = double;
{$if FPC_FULLVERSION>20700}
   bestrealrec = TDoubleRec;
{$endif FPC_FULLVERSION>20700}
   ts32real = single;
   ts64real = double;
   ts80real = extended;
   ts64comp = comp;

   pbestreal=^bestreal;

   { possible supported processors for this target }
   tcputype = (cpu_none);

   tcontrollertype =
     (ct_none
     );


Const
   { Is there support for dealing with multiple microcontrollers available }
   { for this platform? }
   ControllerSupport = false;
   {# Size of native extended floating point type }
   extended_size = 8;
   {# Size of a pointer                           }
   sizeof(aint)  = 4;
   {# Size of a multimedia register               }
   mmreg_size = 8;
   { target cpu string (used by compiler options) }
   target_cpu_string = 'vis';

   { We know that there are fields after sramsize
     but we don't care about this warning }
   {$PUSH}
    {$WARN 3177 OFF}
   embedded_controllers : array [tcontrollertype] of tcontrollerdatatype =
   (
      (controllertypestr:''; controllerunitstr:''; flashbase:0; flashsize:0; srambase:0; sramsize:0));
   {$POP}

Implementation

end.
