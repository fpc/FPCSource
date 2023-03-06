{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Types used by Mach-O resource reader and writer

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit machotypes;
{$ENDIF FPC_DOTTEDUNITS}

{$MODE OBJFPC}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.CTypes;
{$ELSE FPC_DOTTEDUNITS}
uses
  ctypes;
{$ENDIF FPC_DOTTEDUNITS}

{$packrecords c}

type
  TMachOMachineType = (mmtpowerpc, mmtpowerpc64, mmti386, mmtx86_64, mmtarm, mmtarm64);
  TMachOSubMachineTypePowerPC = (msmppc_all, msmppc_7400, msmppc_7450, msmppc_970);
  TMachOSubMachineTypePowerPC64 = (msmppc64_all);
  TMachOSubMachineType386 = (msm386_all);
  TMachOSubMachineTypex64 = (msmx64_all, msmx64_haswell);
  TMachOSubMachineTypeArm = (msmarm_all,msmarm_v4t,msmarm_v6,msmarm_v5tej,msmarm_xscale,msmarm_v7);
  TMachOSubMachineTypeAarch64 = (msmaarch64_all, msmaarch64_v8, msmaarch64_e);
  TSegSectName = array[0..15] of AnsiChar;

  TMachOSubMachineType = record
    case TMachOMachineType of
      mmtpowerpc: (fPpcSubType: TMachOSubMachineTypePowerPC);
      mmtpowerpc64: (fPpc64SubType: TMachOSubMachineTypePowerPC64);
      mmti386: (f386SubType: TMachOSubMachineType386);
      mmtx86_64: (fX64SubType: TMachOSubMachineTypex64);
      mmtarm: (fArmSubType: TMachOSubMachineTypeArm);
      mmtarm64: (fArm64SubType: TMachOSubMachineTypeAarch64);
  end;

  tmach_integer = cint;
  tmach_cpu_type = tmach_integer;
  tmach_cpu_subtype = tmach_integer;
  tmach_cpu_threadtype = tmach_integer;
  tmach_vm_prot = cint;

type
  TMachFatHdr = record
    magic: cuint32;
    nfatarch: cuint32;
  end;

  TMachFarArch = record
    cputype: tmach_cpu_type;
    cpusubtype: tmach_cpu_subtype;
    offset: cuint32;
    size: cuint32;
    align: cuint32;
  end;

  TMachHdr = record
    magic: cuint32;
    cputype: tmach_cpu_type;
    cpusubtype: tmach_cpu_subtype;
    filetype: cuint32;
    ncmds: cuint32;
    sizeofcmds: cuint32;
    flags: cuint32;
    {$IFDEF CPU64}
    reserved: cuint32;
    {$ENDIF}
  end;

  TLoadCommand = record
    cmd: cuint32;
    cmdsize: cuint32;
  end;

  //note: all commands don't include first two longwords

  TSegmentCommand32 = record
    name     : TSegSectName;
    vmaddr   : cuint32;
    vmsize   : cuint32;
    fileoff  : cuint32;
    filesize : cuint32;
    maxprot  : tmach_vm_prot;
    initprot : tmach_vm_prot;
    nsects   : cuint32;
    flags    : cuint32;
  end;

  TSegmentCommand64 = record
    name     : TSegSectName;
    vmaddr   : cuint64;
    vmsize   : cuint64;
    fileoff  : cuint64;
    filesize : cuint64;
    maxprot  : tmach_vm_prot;
    initprot : tmach_vm_prot;
    nsects   : cuint32;
    flags    : cuint32;
  end;
  
  TSection32 = record
    sectname : TSegSectName;
    segname  : TSegSectName;
    addr : cuint32;
    size : cuint32;
    offset : cuint32;
    align : cuint32;
    reloff : cuint32;
    nreloc : cuint32;
    flags : cuint32;
    reserved1 : cuint32;
    reserved2 : cuint32;
  end;

  TSection64 = record
    sectname : TSegSectName;
    segname  : TSegSectName;
    addr : cuint64;
    size : cuint64;
    offset : cuint32;
    align : cuint32;
    reloff : cuint32;
    nreloc : cuint32;
    flags : cuint32;
    reserved1 : cuint32;
    reserved2 : longword;
    reserved3 : cuint32;
  end;

  TSymtabCommand = record
    symoff : cuint32;
    nsyms : cuint32;
    stroff : cuint32;
    strsize : cuint32;
  end;
  
  TDySymtabCommand = record
    ilocalsym : cuint32;
    nlocalsym : cuint32;
    iextdefsym : cuint32;
    nextdefsym : cuint32;
    iundefsym : cuint32;
    nundefsym : cuint32;
    tocoff : cuint32;
    ntoc : cuint32;
    modtaboff : cuint32;
    nmodtab : cuint32;
    extrefsymoff : cuint32;
    nextrefsyms : cuint32;
    indirectsymoff : cuint32;
    nindirectsyms : cuint32;
    extreloff : cuint32;
    nextrel : cuint32;
    locreloff : cuint32;
    nlocrel : cuint32;
  end;
  
  TNList32 = record
    strx : cuint32;
    _type : cuint8;
    sect : cuint8;
    desc : cuint16;
    value : cuint32;
  end;
  PNList32 = ^TNList32;
  
  TNList64 = record
    strx : cuint32;
    _type : cuint8;
    sect : cuint8;
    desc : cuint16;
    value : cuint64;
  end;
  PNList64 = ^TNList64;

  TRelocationInfo = record
    address : cuint32;
    flags : cuint32;
  end;
  PRelocationInfo = ^TRelocationInfo;

  TMachOSubMachineTypeCompatible = (smc_incompatible, smc_compatible, smc_exact);

  function MachOMachineTypesToPas(mach: tmach_cpu_type; sub: tmach_cpu_subtype; out machPas: TMachOMachineType; out subPas: TMachOSubMachineType): boolean;
  function MachOSubMachineTypesEqual(mach: TMachOMachineType; const wantedSubType, fileSubType: TMachOSubMachineType): TMachOSubMachineTypeCompatible;

implementation

{$IFDEF FPC_DOTTEDUNITS}
  uses
    System.Resources.Macho.Consts;
{$ELSE FPC_DOTTEDUNITS}
  uses
    machoconsts;
{$ENDIF FPC_DOTTEDUNITS}

  function MachOMachineTypesToPas(mach: tmach_cpu_type; sub: tmach_cpu_subtype; out machPas: TMachOMachineType; out subPas: TMachOSubMachineType): boolean;
    begin
      result:=true;
      case mach of
        CPU_TYPE_POWERPC:
          begin
            machpas:=mmtpowerpc;
            case sub and not(CPU_SUBTYPE_MASK) of
              CPU_SUBTYPE_POWERPC_7400:
                subPas.fPpcSubType:=msmppc_7400;
              CPU_SUBTYPE_POWERPC_7450:
                subPas.fPpcSubType:=msmppc_7450;
              CPU_SUBTYPE_POWERPC_970:
                subPas.fPpcSubType:=msmppc_970;
              else
                subPas.fPpcSubType:=msmppc_all;
            end;
            exit;
          end;
        CPU_TYPE_POWERPC64:
          begin
            machpas:=mmtpowerpc64;
            subPas.fPpc64SubType:=msmppc64_all;
            exit;
          end;
        CPU_TYPE_I386:
          begin
            machpas:=mmti386;
            subPas.f386SubType:=msm386_all;
            exit;
          end;
        CPU_TYPE_X86_64:
          begin
            machpas:=mmtx86_64;
            case sub and not(CPU_SUBTYPE_MASK) of
              CPU_SUBTYPE_X86_64_H:
                subPas.fX64SubType:=msmx64_haswell;
              else
                subPas.fX64SubType:=msmx64_all;
            end;
            exit;
          end;
        CPU_TYPE_ARM:
          begin
            machpas:=mmtarm;
            subPas.fArmSubType:=msmarm_all;
            exit;
          end;
        CPU_TYPE_ARM64     :
          begin
            machpas:=mmtarm64;
            case sub and not(CPU_SUBTYPE_MASK) of
              CPU_SUBTYPE_ARM64_V8:
                subPas.fArm64SubType:=msmaarch64_v8;
              CPU_SUBTYPE_ARM64E:
                subPas.fArm64SubType:=msmaarch64_e;
              else
                subPas.fArm64SubType:=msmaarch64_all;
            end;
            exit;
          end;
      end;
      result:=false;
    end;


  function MachOSubMachineTypesEqual(mach: TMachOMachineType; const wantedSubType, fileSubType: TMachOSubMachineType): TMachOSubMachineTypeCompatible;
    begin
      result:=smc_incompatible;
      case mach of
        mmtpowerpc:
          begin
            if wantedSubType.fPpcSubType=fileSubType.fPpcSubType then
              exit(smc_exact)
            else if wantedSubType.fPpcSubType>fileSubType.fPpcSubType then
              exit(smc_compatible);
          end;
        mmtpowerpc64:
          begin
            if wantedSubType.fPpc64SubType=fileSubType.fPpc64SubType then
              exit(smc_exact)
          end;
        mmti386:
          begin
            if wantedSubType.f386SubType=fileSubType.f386SubType then
              exit(smc_exact)
          end;
        mmtx86_64:
          begin
            if wantedSubType.fX64SubType=fileSubType.fX64SubType then
              exit(smc_exact)
            else if wantedSubType.fX64SubType>fileSubType.fX64SubType then
              exit(smc_compatible);
          end;
        mmtarm:
          begin
            if wantedSubType.fArmSubType=fileSubType.fArmSubType then
              exit(smc_exact)
            else if wantedSubType.fArmSubType>fileSubType.fArmSubType then
              exit(smc_compatible);
          end;
        mmtarm64:
          begin
            if wantedSubType.fArm64SubType=fileSubType.fArm64SubType then
              exit(smc_exact)
            else if (wantedSubType.fArm64SubType in [msmaarch64_all,msmaarch64_v8]) = (fileSubType.fArm64SubType in [msmaarch64_all,msmaarch64_v8]) then
              exit(smc_exact)
            // msmaarch64_e means pointer authentication etc which are different from plain ARMv8 code
          end;
      end;
    end;

end.
