{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Default ELF target parameters

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

  fMachineType:=emtnone;
  {$IFDEF CPUI386}
  fMachineType:=emti386;
  {$ENDIF}
  {$IFDEF CPUX86_64}
  fMachineType:=emtx86_64;
  {$ENDIF}
  {$IFDEF CPUPOWERPC32}
  fMachineType:=emtppc;;
  {$ENDIF}
  {$IFDEF CPUPOWERPC64}
  fMachineType:=emtppc64;;
  {$ENDIF}
  {$IFDEF CPUARM}
    {$IFDEF ENDIAN_LITTLE}
    fMachineType:=emtarm;
    {$ELSE}
    fMachineType:=emtarmeb;
    {$ENDIF}
  {$ENDIF}
  {$IFDEF CPU68K}
  fMachineType:=emtm68k;
  {$ENDIF}
  {$IFDEF CPUSPARC}
  fMachineType:=emtsparc;
  {$ENDIF}
  {$IFDEF CPUALPHA}
  fMachineType:=emtalpha;
  {$ENDIF}
  {$IFDEF CPUIA64}
  fMachineType:=emtia64;
  {$ENDIF}
  {$IFDEF CPUMIPS}
    {$IFDEF ENDIAN_LITTLE}
    fMachineType:=emtmipsel;
    {$ELSE}
    fMachineType:=emtmips;
    {$ENDIF}
  {$ENDIF}

  fBits:=ELFCLASSNONE;
  {$IFDEF CPU32}
  fBits:=ELFCLASS32;
  {$ENDIF}
  {$IFDEF CPU64}
  fBits:=ELFCLASS64;
  {$ENDIF}

  fNativeOrder:=ELFDATANONE;
  {$IFDEF ENDIAN_LITTLE}
  fNativeOrder:=ELFDATA2LSB;
  {$ENDIF}
  {$IFDEF ENDIAN_BIG}
  fNativeOrder:=ELFDATA2MSB;
  {$ENDIF}
  fOrder:=fNativeOrder;

  fOppositeEndianess:=false;

