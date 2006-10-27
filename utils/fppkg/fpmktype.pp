{$mode objfpc}
{$h+}
unit fpmktype;

interface

uses sysutils;

Type
  TFileType = (ftSource,ftUnit,ftObject,ftResource,ftExecutable,ftStaticLibrary,
               ftSharedLibrary);
  TFileTypes = set of TFileType;

  TOS = (Amiga,Atari,Darwin,FreeBSD,Go32v2,Linux,MacOS,MorphOS,NetBSD,
         Netware,NetwLibc,OpenBSD,OS2,PalmOS,Solaris,Win32,wince,Emx);
  TOSes = Set of TOS;
  
  TCPU = (Arm,I386,PPC,SPARC,X86_64,m68k);
  TCPUS = Set of TCPU;
  
  TCompilerMode = (FPC,TP,ObjFPC,Delphi,MacPas);
  TCompilerModes = Set of TCompilerMode;
  
  TTargetType = (ttUnit,ttProgram,ttExampleUnit,ttExampleProgram);
  TTargetTypes = set of TTargetType;
  
  TTargetState = (tsNeutral,tsCompiling,tsCompiled,tsInstalled);
  TTargetStates = Set of TTargetState;

  TVerboseLevel = (vlError,vlWarning,vlInfo,vlCompare,vlCommand,vldebug);
  TVerboseLevels = Set of TVerboseLevel;

  TCommandAt = (caBeforeCompile,caAfterCompile,
                caBeforeInstall,caAfterInstall,
                caBeforeArchive,caAfterArchive,
                caBeforeClean,caAfterClean,
                caBeforeDownload,caAfterDownload);
  
  TLogEvent = Procedure (Level : TVerboseLevel; Const Msg : String) of Object;

  EInstallerError = Class(Exception);

{$i fpmkcnst.inc}

Function OSToString(OS: TOS) : String;
Function OSesToString(OSes: TOSes) : String;
Function CPUToString(CPU: TCPU) : String;
Function CPUSToString(CPUS: TCPUS) : String;
Function StringToOS(S : String) : TOS;
Function OSesToString(S : String) : TOSes;
Function StringToCPU(S : String) : TCPU;
Function StringToCPUS(S : String) : TCPUS;
Function ModeToString(Mode: TCompilerMode) : String;
Function StringToMode(S : String) : TCompilerMode;
Function MakeTargetString(CPU : TCPU;OS: TOS) : String;
Procedure StringToCPUOS(S : String; Var CPU : TCPU; Var OS: TOS);

implementation

uses typinfo;

resourcestring
  SErrInvalidCPU        = 'Invalid CPU name : "%s"';
  SErrInvalidOS         = 'Invalid OS name : "%s"';
  SErrInvalidMode       = 'Invalid compiler mode : "%s"';
  SErrInvalidTarget     = 'Invalid compiler target: %s';


Function OSToString(OS: TOS) : String;

begin
  Result:=LowerCase(GetenumName(TypeInfo(TOS),Ord(OS)));
end;

Function OSesToString(OSes: TOSes) : String;

begin
  Result:=LowerCase(SetToString(PtypeInfo(TypeInfo(TOSes)),Integer(OSes),False));
end;

Function CPUToString(CPU: TCPU) : String;

begin
  Result:=LowerCase(GetenumName(TypeInfo(TCPU),Ord(CPU)));
end;

Function CPUSToString(CPUS: TCPUS) : String;

begin
  Result:=LowerCase(SetToString(PTypeInfo(TypeInfo(TCPUS)),Integer(CPUS),False));
end;

Function StringToOS(S : String) : TOS;

Var
  I : Integer;

begin
  I:=GetEnumValue(TypeInfo(TOS),S);
  if (I=-1) then
    Raise EInstallerError.CreateFmt(SErrInvalidOS,[S]);
  Result:=TOS(I);
end;


Function OSesToString(S : String) : TOSes;

begin
  Result:=TOSes(StringToSet(PTypeInfo(TypeInfo(TOSes)),S));
end;

Function StringToCPU(S : String) : TCPU;

Var
  I : Integer;

begin
  I:=GetEnumValue(TypeInfo(TCPU),S);
  if (I=-1) then
    Raise EInstallerError.CreateFmt(SErrInvalidCPU,[S]);
  Result:=TCPU(I);
end;

Function StringToCPUS(S : String) : TCPUS;

begin
  Result:=TCPUS(StringToSet(PTypeInfo(TypeInfo(TCPUS)),S));
end;

Function ModeToString(Mode: TCompilerMode) : String;

begin
  Result:=LowerCase(GetenumName(TypeInfo(TCompilerMode),Ord(Mode)));
end;

Function StringToMode(S : String) : TCompilerMode;

Var
  I : Integer;

begin
  I:=GetEnumValue(TypeInfo(TCompilerMode),S);
  if (I=-1) then
    Raise EInstallerError.CreateFmt(SErrInvalidMode,[S]);
  Result:=TCompilerMode(I);
end;


Function MakeTargetString(CPU : TCPU;OS: TOS) : String;

begin
  Result:=CPUToString(CPU)+'-'+OSToString(OS);
end;

Procedure StringToCPUOS(S : String; Var CPU : TCPU; Var OS: TOS);

Var
  P : integer;

begin
  P:=Pos('-',S);
  If (P=0) then
    Raise EInstallerError.CreateFmt(SErrInvalidTarget,[S]);
  CPU:=StringToCPU(Copy(S,1,P-1));
  OS:=StringToOs(Copy(S,P+1,Length(S)-P));
end;

end.