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
                  mtsparc,mtalpha,mtia64,mtBigEndian,mtLittleEndian);
  TMachineTypes = set of TMachineType;

  TObjFormat = (ofNone, ofRes, ofElf, ofCoff, ofMachO, ofExt);
  TObjFormats = set of TObjFormat;
  

  TMachineInfo = record
    name : string;
    formats : TObjFormats;
  end;
  
  TFormatInfo = record
    name : string;
    ext : string;
    machines : TMachineTypes;
  end;

  TResTarget = record
    machine : TMachineType;
    objformat : TObjFormat;
  end;

function GetDefaultMachineForFormat(aFormat : TObjFormat) : TMachineType;
function TargetToStr(const aTarget : TResTarget) : string;
function MachineToStr(const aMachine : TMachineType) : string;
function ObjFormatToStr(const aFormat : TObjFormat) : string;

var
  Machines : array[TMachineType] of TMachineInfo =
  (
    (name : '';             formats : [ofRes]),                  //mtnone
    (name : 'i386';         formats : [ofElf, ofCoff, ofMachO]), //mti386
    (name : 'x86_64';       formats : [ofElf, ofCoff, ofMachO]), //mtx86_64
    (name : 'powerpc';      formats : [ofElf, ofMachO]),         //mtppc
    (name : 'powerpc64';    formats : [ofElf, ofMachO]),         //mtppc64
    (name : 'arm';          formats : [ofElf, ofCoff]),          //mtarm
    (name : 'armeb';        formats : [ofElf]),                  //mtarmeb
    (name : 'm68k';         formats : [ofElf]),                  //mtm68k
    (name : 'sparc';        formats : [ofElf]),                  //mtsparc
    (name : 'alpha';        formats : [ofElf]),                  //mtalpha
    (name : 'ia64';         formats : [ofElf]),                  //mtia64
    (name : 'bigendian';    formats : [ofExt]),                  //mtBigEndian
    (name : 'littleendian'; formats : [ofExt])                   //mtLittleEndian
  );
  
  ObjFormats : array[TObjFormat] of TFormatInfo =
  (
    (name : '';         ext : '';        machines : []),
    (name : 'res';      ext : '.res';    machines : [mtnone]),
    (name : 'elf';      ext : '.or';     machines : [mti386,mtx86_64,mtppc,
                                                     mtppc64,mtarm,mtarmeb,
                                                     mtm68k,mtsparc,mtalpha,
                                                     mtia64]),
    (name : 'coff';     ext : '.o';      machines : [mti386,mtx86_64,mtarm]),
    (name : 'mach-o';   ext : '.or';     machines : [mti386,mtx86_64,mtppc,
                                                     mtppc64]),
    (name : 'external'; ext : '.fpcres'; machines : [mtBigEndian,mtLittleEndian])
  );


  CurrentTarget : TResTarget =
  (
  {$IFDEF CPUI386}
    machine : mti386;
  {$ELSE}
  {$IFDEF CPUX86_64}
    machine : mtx86_64;
  {$ELSE}
  {$IFDEF CPUPOWERPC32}
    machine : mtppc;
  {$ELSE}
  {$IFDEF CPUPOWERPC64}
    machine : mtppc64;
  {$ELSE}
  {$IFDEF CPUARM}
    {$IFDEF ENDIAN_LITTLE}
    machine : mtarm;
    {$ELSE}
    machine : mtarmeb;
    {$ENDIF}
  {$ELSE}
  {$IFDEF CPU68K}
    machine : mtm68k;
  {$ELSE}
  {$IFDEF CPUSPARC}
    machine : mtsparc;
  {$ELSE}
  {$IFDEF CPUALPHA}
    machine : mtalpha;
  {$ELSE}
  {$IFDEF CPUIA64}
    machine : mtia64;
  {$ELSE}
    machine : mti386;  //default i386
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}

  {$IFDEF WINDOWS}
    objformat : ofCoff;
  {$ELSE}
    {$IFDEF DARWIN}
      objformat : ofMachO;
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

function ObjFormatToStr(const aFormat : TObjFormat) : string;
begin
  Result:=ObjFormats[aFormat].name;
end;

function TargetToStr(const aTarget : TResTarget) : string;
var s1, s2 : string;
begin
  s1:=MachineToStr(aTarget.machine);
  s2:=ObjFormatToStr(aTarget.objformat);
  if (s1='') or (s2='') then Result:=s1+s2
  else Result:=s1+' - '+s2;
end;

end.
