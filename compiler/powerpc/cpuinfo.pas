{
    $Id: cpuinfo.pas,v 1.24 2005/03/27 14:10:53 jonas Exp $
    Copyright (c) 1998-2002 by the Free Pascal development team

    Basic Processor information for the PowerPC

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
   bestreal = double;
   ts32real = single;
   ts64real = double;
   ts80real = extended;
   ts128real = extended;
   ts64comp = comp;

   pbestreal=^bestreal;

   { possible supported processors for this target }
   tprocessors =
      (no_processor,
       ppc601,
       ppc604
      );

   tfputype =
     (no_fpuprocessor,
      fpu_soft,
      fpu_standard
     );


Const
   { calling conventions supported by the code generator }
   supported_calling_conventions : tproccalloptions = [
     pocall_internproc,
     pocall_compilerproc,
     pocall_inline,
     pocall_stdcall,
     { the difference to stdcall is only the name mangling }
     pocall_cdecl,
     { the difference to stdcall is only the name mangling }
     pocall_cppdecl,
     { pass all const records by reference }
     pocall_mwpascal
   ];

   processorsstr : array[tprocessors] of string[10] = ('',
     '603',
     '604'
   );

   fputypestr : array[tfputype] of string[8] = ('',
     'SOFT',
     'STANDARD'
   );

Implementation

end.
{
  $Log: cpuinfo.pas,v $
  Revision 1.24  2005/03/27 14:10:53  jonas
    * const record parameters > 8 bytes are now passed by reference for non
      cdecl/cppdecl procedures on Mac OS/Mac OS X to fix compatibility with
      GPC (slightly more efficient than Metrowerks behaviour below, but
      less efficient in most cases than our previous scheme)
    + "mwpascal" procedure directive to support the const record parameter
      behaviour of Metrowerks Pascal, which passes all const records by
      reference

  Revision 1.23  2005/03/25 21:55:43  jonas
    * removed some unused variables

  Revision 1.22  2005/02/14 17:13:10  peter
    * truncate log

}
