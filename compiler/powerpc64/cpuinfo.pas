{
    Copyright (c) 1998-2002 by the Free Pascal development team

    Basic Processor information for the PowerPC

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit CPUInfo;

interface

uses
  globtype;

type
  bestreal = double;
  ts32real = single;
  ts64real = double;
  ts80real = extended;
  ts128real = extended;
  ts64comp = comp;

  pbestreal = ^bestreal;

  { possible supported processors for this target }
  tprocessors =
    (no_processor,
    ppc970
    );

  tfputype =
    (no_fpuprocessor,
    fpu_soft,
    fpu_standard
    );

const
  { calling conventions supported by the code generator }
  supported_calling_conventions: tproccalloptions = [
    pocall_internproc,
    pocall_stdcall,
    { the difference to stdcall is only the name mangling }
    pocall_cdecl,
    { the difference to stdcall is only the name mangling }
    pocall_cppdecl
    ];

  processorsstr: array[tprocessors] of string[10] = ('',
    '970'
    );

  fputypestr: array[tfputype] of string[8] = ('',
    'SOFT',
    'STANDARD'
    );

implementation

end.

