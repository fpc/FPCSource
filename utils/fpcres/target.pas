{

    FPCRes - Free Pascal Resource Converter
    Part of the Free Pascal distribution
    Copyright (C) 2008 by Giulio Bernardi

    Target selection and definitions

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit target;

{$MODE OBJFPC}

interface

type
  TMachineType = (mtnone, mti386,mtx86_64,mtppc,mtppc64,mtarm,mtarmeb,mtm68k,
                  mtsparc,mtalpha,mtia64,mtmips,mtmipsel,mtaarch64,
                  mtBigEndian,mtLittleEndian);
  TMachineTypes = set of TMachineType;

  TSubMachineTypeArm = (smtarm_all,smtarm_v4t,smtarm_v6,smtarm_v5tej,smtarm_xscale,smtarm_v7);
  TSubMachineTypeGeneric = (smtgen_all);

  TSubMachineType = record
    case TMachineType of
      mtarm,mtarmeb:
        (subarm: TSubMachineTypeArm);
      mtnone, mti386,mtx86_64,mtppc,mtppc64,mtm68k,
      mtsparc,mtalpha,mtia64,mtmips,mtmipsel,mtaarch64,
      mtBigEndian,mtLittleEndian:
        (subgen: TSubMachineTypeGeneric);
  end;

  TObjFormat = (ofNone, ofRes, ofElf, ofCoff, ofXCoff, ofMachO, ofExt);
  TObjFormats = set of TObjFormat;
  

  TMachineInfo = record
    name : string;
    formats : TObjFormats;
    alias : string;
  end;
  
  TFormatInfo = record
    name : string;
    ext : string;
    machines : TMachineTypes;
  end;

  TResTarget = record
    machine : TMachineType;
    submachine : TSubMachineType;
    objformat : TObjFormat;
  end;

function GetDefaultMachineForFormat(aFormat : TObjFormat) : TMachineType;
function GetDefaultSubMachineForMachine(aMachine: TMachineType) : TSubMachineType;
function TargetToStr(const aTarget : TResTarget) : string;
function MachineToStr(const aMachine : TMachineType) : string;
function ObjFormatToStr(const aFormat : TObjFormat) : string;

var
  Machines : array[TMachineType] of TMachineInfo =
  (
    (name : '';             formats : [ofRes]),                   //mtnone
    (name : 'i386';         formats : [ofElf, ofCoff, ofMachO]),  //mti386
    (name : 'x86_64';       formats : [ofElf, ofCoff, ofMachO]),  //mtx86_64
    (name : 'powerpc';      formats : [ofElf, ofXCoff, ofMachO]), //mtppc
    (name : 'powerpc64';    formats : [ofElf, {ofXCoff,} ofMachO]), //mtppc64
    (name : 'arm';          formats : [ofElf, ofCoff, ofMachO]),  //mtarm
    (name : 'armeb';        formats : [ofElf]),                   //mtarmeb
    (name : 'm68k';         formats : [ofElf]),                   //mtm68k
    (name : 'sparc';        formats : [ofElf]),                   //mtsparc
    (name : 'alpha';        formats : [ofElf]),                   //mtalpha
    (name : 'ia64';         formats : [ofElf]),                   //mtia64
    (name : 'mips';         formats : [ofElf]; alias : 'mipseb'), //mtmips
    (name : 'mipsel';       formats : [ofElf]),                   //mtmipsel
    (name : 'aarch64';      formats : [ofMachO]),                 //mtaarch64
    (name : 'bigendian';    formats : [ofExt]),                   //mtBigEndian
    (name : 'littleendian'; formats : [ofExt])                    //mtLittleEndian
  );

  SubMachinesArm: array[TSubMachineTypeArm] of string[8] =
    ('all','armv4','armv6','armv5tej','xscale','armv7');
  SubMachinesGen: array[TSubMachineTypeGeneric] of string[3] =
    ('all');
  
  ObjFormats : array[TObjFormat] of TFormatInfo =
  (
    (name : '';         ext : '';        machines : []),
    (name : 'res';      ext : '.res';    machines : [mtnone]),
    (name : 'elf';      ext : '.or';     machines : [mti386,mtx86_64,mtppc,
                                                     mtppc64,mtarm,mtarmeb,
                                                     mtm68k,mtsparc,mtalpha,
                                                     mtia64,mtmips,mtmipsel]),
    (name : 'coff';     ext : '.o';      machines : [mti386,mtx86_64,mtarm,
                                                     mtppc,mtppc64]),
    (name : 'xcoff';    ext : '.o';      machines : [mtppc{,mtppc64}]),
    (name : 'mach-o';   ext : '.or';     machines : [mti386,mtx86_64,mtppc,
                                                     mtppc64,mtarm,mtaarch64]),
    (name : 'external'; ext : '.fpcres'; machines : [mtBigEndian,mtLittleEndian])
  );


  CurrentTarget : TResTarget =
  (
  {$if defined(CPUI386)}
    machine : mti386;
    submachine : (subgen: smtgen_all);
  {$elseif defined(CPUX86_64)}
    machine : mtx86_64;
    submachine : (subgen: smtgen_all);
  {$elseif defined(CPUPOWERPC32)}
    machine : mtppc;
    submachine : (subgen: smtgen_all);
  {$elseif defined(CPUPOWERPC64)}
    machine : mtppc64;
    submachine : (subgen: smtgen_all);
  {$elseif defined(CPUARM)}
    {$IFDEF ENDIAN_LITTLE}
    machine : mtarm;
    submachine : (subarm: smtarm_all);
    {$ELSE}
    machine : mtarmeb;
    submachine : (subarm: smtarm_all);
    {$ENDIF}
  {$elseif defined(CPU68K)}
    machine : mtm68k;
    submachine : (subgen: smtgen_all);
  {$elseif defined(CPUSPARC)}
    machine : mtsparc;
    submachine : (subgen: smtgen_all);
  {$elseif defined(CPUALPHA)}
    machine : mtalpha;
    submachine : (subgen: smtgen_all);
  {$elseif defined(CPUIA64)}
    machine : mtia64;
    submachine : (subgen: smtgen_all);
  {$elseif defined(CPUMIPSEL)}
    machine : mtmipsel;
    submachine : (subgen: smtgen_all);
  {$elseif defined(CPUMIPS)}
    machine : mtmips;
    submachine : (subgen: smtgen_all);
  {$elseif defined(CPUAARCH64)}
    machine : mtaarch64;
    submachine : (subgen: smtgen_all);
  {$else}
    machine : mti386;  //default i386
    submachine : (subgen: smtgen_all);
  {$endif}

  {$IFDEF WINDOWS}
    objformat : ofCoff;
  {$ELSE}
    {$IF defined(DARWIN)}
      objformat : ofMachO;
    {$ELSEIF defined(AIX)}
      objformat : ofXCoff;
    {$ELSE}
      objformat : ofElf;
    {$ENDIF}
  {$ENDIF}
  );


implementation

function GetDefaultMachineForFormat(aFormat : TObjFormat) : TMachineType;
begin
  case aFormat of
    ofNone : Result:=mtnone;
    ofRes  : Result:=mtnone;
    ofElf  : Result:=mti386;
    ofCoff : Result:=mti386;
    ofXCoff: Result:=mtppc;
    ofMachO: Result:=mti386;
    {$IFDEF ENDIAN_BIG}
    ofExt  : Result:=mtBigEndian;
    {$ELSE}
    ofExt  : Result:=mtLittleEndian;
    {$ENDIF}
  end;
end;

function MachineToStr(const aMachine : TMachineType) : string;
begin
  Result:=Machines[aMachine].name;
end;

function SubMachineToStr(const aMachine : TMachineType; const aSubMachine : TSubMachineType) : string;
begin
  case aMachine of
    mtarm,mtarmeb:
      result:=SubMachinesArm[aSubMachine.subarm];
    else
      // no need to confuse people with the "all" suffix, it doesn't do
      // anything anyway
      result:='';
  end;
end;

function ObjFormatToStr(const aFormat : TObjFormat) : string;
begin
  Result:=ObjFormats[aFormat].name;
end;

function GetDefaultSubMachineForMachine(aMachine: TMachineType): TSubMachineType;
begin
  case aMachine of
    mtarm,mtarmeb:
      result.subarm:=smtarm_all;
    else
      result.subgen:=smtgen_all;
  end;
end;

function TargetToStr(const aTarget : TResTarget) : string;
var s1, s2, s3 : string;
begin
  s1:=MachineToStr(aTarget.machine);
  s2:=ObjFormatToStr(aTarget.objformat);
  s3:=SubMachineToStr(aTarget.Machine,aTarget.submachine);
  if (s1='') or (s2='') then Result:=s1+s2
  else Result:=s1+' - '+s2;
  if s3<>'' then
    Result:=Result+'-'+s3;
end;

end.
