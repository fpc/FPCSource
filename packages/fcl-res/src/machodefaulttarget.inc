{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Default Mach-O target parameters

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

  fMachineType:=mmtpowerpc; //default
  {$IFDEF CPUPOWERPC32}
  fMachineType:=mmtpowerpc;
  {$ENDIF}
  {$IFDEF CPUPOWERPC64}
  fMachineType:=mmtpowerpc64;
  {$ENDIF}
  {$IFDEF CPUI386}
  fMachineType:=mmti386;
  {$ENDIF}
  {$IFDEF CPUX86_64}
  fMachineType:=mmtx86_64;
  {$ENDIF}
  {$IFDEF CPUAARCH64}
  fMachineType:=mmtarm64;
  {$ENDIF}

  fBits:=MACH_ERRBIT;
  {$IFDEF CPU32}
  fBits:=MACH_32BIT;
  {$ENDIF}
  {$IFDEF CPU64}
  fBits:=MACH_64BIT;
  {$ENDIF}

  {$IFDEF ENDIAN_BIG}
  fNativeEndianess:=MACH_BIG_ENDIAN;
  {$ELSE}
  fNativeEndianess:=MACH_LITTLE_ENDIAN;
  {$ENDIF}
  fEndianess:=fNativeEndianess;
  fOppositeEndianess:=false;

