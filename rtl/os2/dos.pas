{****************************************************************************


                         Free Pascal Runtime-Library
                              DOS unit for OS/2
                   Copyright (c) 1997,1999-2000 by Daniel Mantione,
                   member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ****************************************************************************}

unit dos;

{$ASMMODE ATT}

{***************************************************************************}

interface

{***************************************************************************}

{$PACKRECORDS 1}

uses    Strings, DosCalls;

Type
   {Search record which is used by findfirst and findnext:}
   SearchRec = record
            case boolean of
             false: (Handle: THandle;     {Used in os_OS2 mode}
                     FStat: PFileFindBuf3;
                     Fill: array [1..21 - SizeOf (THandle) - SizeOf (pointer)]
                                                                       of byte;
                     Attr: byte;
                     Time: longint;
                     Size: longint;
                     Name: string);      {Filenames can be long in OS/2!}
             true:  (Fill2: array [1..21] of byte;
                     Attr2: byte;
                     Time2: longint;
                     Size2: longint;
                     Name2: string);       {Filenames can be long in OS/2!}
        end;

        {Flags for the exec procedure:
        }

threadvar
(* For compatibility with VP/2, used for runflags in Exec procedure. *)
    ExecFlags: cardinal;
(* Note that the TP/BP compatible method for retrieval of exit codes    *)
(* is limited to only one (the last) execution! Including the following *)
(* two variables in the interface part allows querying the status of    *)
(* of asynchronously started programs using DosWaitChild with dtNoWait  *)
(* parameter, i.e. without waiting for the final program result (as     *)
(* opposed to calling DosExitCode which would wait for the exit code).  *)
    LastExecRes: TResultCodes;
    LastExecFlags: cardinal;

{$i dosh.inc}

{OS/2 specific functions}

function GetEnvPChar (EnvVar: string): PChar;

function DosErrorModuleName: string;
(* In case of an error in Dos.Exec returns the name of the module *)
(* causing the problem - e.g. name of a missing or corrupted DLL. *)
(* It may also contain a queue name in case of a failed attempt *)
(* to create queue for reading results of started sessions.     *)



implementation

{$DEFINE HAS_GETMSCOUNT}
{$DEFINE HAS_DOSEXITCODE}

{$DEFINE FPC_FEXPAND_UNC} (* UNC paths are supported *)
{$DEFINE FPC_FEXPAND_DRIVES} (* Full paths begin with drive specification *)
{$DEFINE FPC_FEXPAND_GETENV_PCHAR}

{$I dos.inc}

threadvar
  LastDosErrorModuleName: string;


const   FindResvdMask = $00003737; {Allowed bits in attribute
                                    specification for DosFindFirst call.}


function GetMsCount: int64;
var
  L: cardinal;
begin
  DosQuerySysInfo (svMsCount, svMsCount, L, 4);
  GetMsCount := L;
end;


function fsearch(path:pathstr;dirlist:string):pathstr;
Var
  A: array [0..255] of char;
  D, P: AnsiString;
begin
  P:=Path;
  D:=DirList;
  DosError := DosSearchPath (dsIgnoreNetErrs, PChar(D), PChar(P), @A, 255);
  if DosError <> 0 then
   OSErrorWatch (DosError);
  fsearch := StrPas (@A);
end;


procedure getftime(var f;var time:longint);
var
  FStat: TFileStatus3;
begin
  DosError := DosQueryFileInfo (FileRec (F).Handle, ilStandard, @FStat,
                                                               SizeOf (FStat));
  if DosError=0 then
   begin
    Time := FStat.TimeLastWrite + longint (FStat.DateLastWrite) shl 16;
    if Time = 0 then
      Time := FStat.TimeCreation + longint (FStat.DateCreation) shl 16;
   end
  else
   begin
    Time:=0;
    OSErrorWatch (DosError);
   end;
end;


procedure SetFTime (var F; Time: longint);
var FStat: TFileStatus3;
    RC: cardinal;
begin
  RC := DosQueryFileInfo (FileRec (F).Handle, ilStandard, @FStat,
                                                               SizeOf (FStat));
  if RC = 0 then
   begin
    FStat.DateLastAccess := Hi (Time);
    FStat.DateLastWrite := Hi (Time);
    FStat.TimeLastAccess := Lo (Time);
    FStat.TimeLastWrite := Lo (Time);
    RC := DosSetFileInfo (FileRec (F).Handle, ilStandard, @FStat,
                                                               SizeOf (FStat));
    if RC <> 0 then
     OSErrorWatch (RC);
   end
  else
   OSErrorWatch (RC);
  DosError := integer (RC);
end;


function DosExitCode: word;
var
  Res: TResultCodes;
  PPID: cardinal;
  RC: cardinal;
begin
  if (LastExecFlags = deAsyncResult) or (LastExecFlags = deAsyncResultDb) then
   begin
    RC := DosWaitChild (DCWA_PROCESS, dtWait, Res, PPID, LastExecRes.PID);
    if RC = 0 then
(* If we succeeded, the process is finished - possible future querying
   of DosExitCode shall return the result immediately as with synchronous
   execution. *)
     begin
      LastExecFlags := deSync;
      LastExecRes := Res;
     end
    else
     begin
      LastExecRes.ExitCode := RC shl 16;
      OSErrorWatch (RC);
     end;
   end;
  if LastExecRes.ExitCode > high (word) then
    DosExitCode := high (word)
  else
    DosExitCode := LastExecRes.ExitCode and $FFFF;
end;


procedure Exec (const Path: PathStr; const ComLine: ComStr);
{Execute a program.}
var
  Args0, Args: PByteArray;
  ArgSize: word;
  ObjName: string;
  Res: TResultCodes;
  RC, RC2: cardinal;
  ExecAppType: cardinal;
  HQ: THandle;
  SPID, STID, SCtr, QName: string;
  SID, PID: cardinal;
  SD: TStartData;
  RD: TRequestData;
  PCI: PChildInfo;
  CISize: cardinal;
  Prio: byte;
  DSS: boolean;
  SR: SearchRec;

const
  MaxArgsSize = 3072; (* Amount of memory reserved for arguments in bytes. *)

begin
{  LastDosExitCode := Exec (Path, ExecRunFlags (ExecFlags), efDefault, ComLine);}
  ObjName := '';
(* FExpand should be used only for the DosStartSession part
   and only if the executable is in the current directory.  *)
  FindFirst (Path, AnyFile, SR);
  if DosError = 0 then
   QName := FExpand (Path)
  else
   QName := Path;
  FindClose (SR);
  if ComLine = '' then
   begin
    Args0 := nil;
    Args := nil;
   end
  else
   begin
    GetMem (Args0, MaxArgsSize);
    Args := Args0;
(* Work around a bug in OS/2 - argument to DosExecPgm *)
(* should not cross a 64K boundary. *)
    if ((PtrUInt (Args) + 1024) and $FFFF) < 1024 then
     Inc (pointer (Args), 1024);
    ArgSize := 0;
    Move (QName [1], Args^ [ArgSize], Length (QName));
    Inc (ArgSize, Length (QName));
    Args^ [ArgSize] := 0;
    Inc (ArgSize);
    {Now do the real arguments.}
    Move (ComLine [1], Args^ [ArgSize], Length (ComLine));
    Inc (ArgSize, Length (ComLine));
    Args^ [ArgSize] := 0;
    Inc (ArgSize);
    Args^ [ArgSize] := 0;
   end;

  RC := DosQueryAppType (PChar (Args), ExecAppType);
  if RC <> 0 then
   OSErrorWatch (RC)
  else
   if (ApplicationType and 3 = ExecAppType and 3) then
(* DosExecPgm should work... *)
    begin
     DSS := false;
     Res.ExitCode := $FFFFFFFF;
     RC := DosExecPgm (ObjName, cardinal (ExecFlags), Args, nil, Res, Path);
     if RC = 0 then
      begin
       LastExecFlags := ExecFlags;
       LastExecRes := Res;
       LastDosErrorModuleName := '';
      end
     else
      begin
       if (RC = 190) or (RC = 191) then
        DSS := true;
       OSErrorWatch (RC);
      end;
   end
  else
   DSS := true;
  if DSS then
   begin
    Str (GetProcessID, SPID);
    Str (ThreadID, STID);
    QName := '\QUEUES\FPC_Dos_Exec_p' + SPID + 't' + STID + '.QUE'#0;
    FillChar (SD, SizeOf (SD), 0);
    SD.Length := SizeOf (SD);
    RC := 0;
    case ExecFlags of
     deSync:
      begin
       SD.Related := ssf_Related_Child;
       LastExecFlags := ExecFlags;
       SD.TermQ := @QName [1];
       RC := DosCreateQueue (HQ, quFIFO or quConvert_Address, @QName [1]);
       if RC <> 0 then
        OSErrorWatch (RC);
      end;
     deAsync,
     deAsyncResult:
      begin
(* Current implementation of DosExitCode does not support retrieval *)
(* of result codes for other session types started asynchronously.  *)
       LastExecFlags := deAsync;
       SD.Related := ssf_Related_Independent;
      end;
     deBackground:
      begin
(* Current implementation of DosExitCode does not support retrieval *)
(* of result codes for other session types started asynchronously.  *)
       LastExecFlags := ExecFlags;
       SD.Related := ssf_Related_Independent;
       SD.FgBg := ssf_FgBg_Back;
      end;
     deAsyncResultDB:
      begin
(* Current implementation of DosExitCode does not support retrieval *)
(* of result codes for other session types started asynchronously.  *)
       LastExecFlags := ExecFlags;
       SD.Related := ssf_Related_Child;
       SD.TraceOpt := ssf_TraceOpt_Trace;
      end;
    end;
    if RC <> 0 then
     ObjName := Copy (QName, 1, Pred (Length (QName)))
    else
     begin
      if Args = nil then
(* No parameters passed, Args not allocated for DosExecPgm, so allocate now. *)
       begin
        GetMem (Args0, MaxArgsSize);
        Args := Args0;
        Move (QName [1], Args^ [0], Length (QName));
        Args^ [Length (QName)] := 0;
       end
      else
       SD.PgmInputs := PChar (@Args^ [Length (QName) + 1]);
      SD.PgmName := PChar (Args);
      SD.InheritOpt := ssf_InhertOpt_Parent;
      SD.ObjectBuffer := @ObjName [1];
      SD.ObjectBuffLen := SizeOf (ObjName) - 1;
      RC := DosStartSession (SD, SID, PID);
      if RC <> 0 then
       OSErrorWatch (RC);
      if (RC = 0) or (RC = 457) then
       begin
        LastExecRes.PID := PID;
        if ExecFlags = deSync then
         begin
          RC := DosReadQueue (HQ, RD, CISize, PCI, 0, 0, Prio, 0);
          if RC <> 0 then
           OSErrorWatch (RC);
          if (RC = 0) and (PCI^.SessionID = SID) then
           begin
            LastExecRes.ExitCode := PCI^.Return;
            RC2 := DosCloseQueue (HQ);
            if RC2 <> 0 then
             OSErrorWatch (RC2);
            RC2 := DosFreeMem (PCI);
            if RC2 <> 0 then
             OSErrorWatch (RC2);
           end
          else
           begin
            RC2 := DosCloseQueue (HQ);
            if RC2 <> 0 then
             OSErrorWatch (RC2);
           end;
         end;
       end
      else if ExecFlags = deSync then
       begin
        RC2 := DosCloseQueue (HQ);
        if RC2 <> 0 then
         OSErrorWatch (RC2);
       end;
     end;
   end;
  if RC <> 0 then
   begin
    LastDosErrorModuleName := ObjName;
    LastExecFlags := deSync;
    LastExecRes.ExitCode := 0; (* Needed for TP/BP compatibility *)
    LastExecRes.TerminateReason := $FFFFFFFF;
   end;
  DosError := RC;
  if Args0 <> nil then
   FreeMem (Args0, MaxArgsSize);
end;


function DosErrorModuleName: string;
begin
  DosErrorModuleName := LastDosErrorModuleName;
end;


function dosversion:word;
{Returns OS/2 version}
var
  Minor, Major: Cardinal;
begin
  DosQuerySysInfo(svMajorVersion, svMajorVersion, Major, 4);
  DosQuerySysInfo(svMinorVersion, svMinorVersion, Minor, 4);
  DosVersion:=Major or Minor shl 8;
end;


procedure GetDate (var Year, Month, MDay, WDay: word);
Var
  dt: TDateTime;
begin
  DosGetDateTime(dt);
  Year:=dt.year;
  Month:=dt.month;
  MDay:=dt.Day;
  WDay:=dt.Weekday;
end;


procedure SetDate (Year, Month, Day: word);
var
  DT: TDateTime;
  RC: cardinal;
begin
  DosGetDateTime (DT);
  DT.Year := Year;
  DT.Month := byte (Month);
  DT.Day := byte (Day);
  RC := DosSetDateTime (DT);
  if RC <> 0 then
   OSErrorWatch (RC);
end;


procedure GetTime (var Hour, Minute, Second, Sec100: word);
var
  dt: TDateTime;
begin
  DosGetDateTime(dt);
  Hour:=dt.Hour;
  Minute:=dt.Minute;
  Second:=dt.Second;
  Sec100:=dt.Hundredths;
end;


procedure SetTime (Hour, Minute, Second, Sec100: word);
var
  DT: TDateTime;
  RC: cardinal;
begin
  DosGetDateTime (DT);
  DT.Hour := byte (Hour);
  DT.Minute := byte (Minute);
  DT.Second := byte (Second);
  DT.Sec100 := byte (Sec100);
  DosSetDateTime (DT);
  if RC <> 0 then
   OSErrorWatch (RC);
end;

function DiskFree (Drive: byte): int64;
var FI: TFSinfo;
    RC: cardinal;
begin
  {In OS/2, we use the filesystem information.}
  RC := DosQueryFSInfo (Drive, 1, FI, SizeOf (FI));
  if RC = 0 then
      DiskFree := int64 (FI.Free_Clusters) *
         int64 (FI.Sectors_Per_Cluster) * int64 (FI.Bytes_Per_Sector)
  else
   begin
    DiskFree := -1;
    OSErrorWatch (RC);
   end;
end;


function DiskSize (Drive: byte): int64;
var FI: TFSinfo;
    RC: cardinal;
begin
  RC := DosQueryFSinfo (Drive, 1, FI, SizeOf (FI));
  if RC = 0 then
      DiskSize := int64 (FI.Total_Clusters) *
         int64 (FI.Sectors_Per_Cluster) * int64 (FI.Bytes_Per_Sector)
  else
   begin
    DiskSize := -1;
    OSErrorWatch (RC);
   end;
end;


procedure DosSearchRec2SearchRec (var F: SearchRec);
type
  TRec = record
    T, D: word;
  end;
begin
 with F do
    begin
        Name := FStat^.Name;
        Size := FStat^.FileSize;
        Attr := byte(FStat^.AttrFile and $FF);
        TRec (Time).T := FStat^.TimeLastWrite;
        TRec (Time).D := FStat^.DateLastWrite;
    end;
end;


procedure FindFirst (const Path: PathStr; Attr: word; var F: SearchRec);


var Count: cardinal;

begin
  {No error.}
  DosError := 0;
  New (F.FStat);
  F.Handle := THandle ($FFFFFFFF);
  Count := 1;
  DosError := integer (DosFindFirst (Path, F.Handle,
                     Attr and FindResvdMask, F.FStat, SizeOf (F.FStat^),
                                                           Count, ilStandard));
  if DosError <> 0 then
   OSErrorWatch (DosError)
  else if Count = 0 then
   DosError := 18;
  DosSearchRec2SearchRec (F);
end;


procedure FindNext (var F: SearchRec);
var
  Count: cardinal;
begin
    {No error}
    DosError := 0;
    Count := 1;
    DosError := integer (DosFindNext (F.Handle, F.FStat, SizeOf (F.FStat^),
                                                                       Count));
    if DosError <> 0 then
     OSErrorWatch (DosError)
    else if Count = 0 then
     DosError := 18;
    DosSearchRec2SearchRec (F);
end;


procedure FindClose (var F: SearchRec);
begin
  if F.Handle <> THandle ($FFFFFFFF) then
   begin
    DosError := integer (DosFindClose (F.Handle));
    if DosError <> 0 then
     OSErrorWatch (DosError);
   end;
  Dispose (F.FStat);
end;


function envcount:longint;
begin
  envcount:=envc;
end;


function envstr (index : longint) : string;

var hp:Pchar;

begin
    if (index<=0) or (index>envcount) then
        begin
            envstr:='';
            exit;
        end;
    hp:=EnvP[index-1];
    envstr:=strpas(hp);
end;


function GetEnvPChar (EnvVar: string): PChar;
(* The assembler version is more than three times as fast as Pascal. *)
var
 P: PChar;
begin
 EnvVar := UpCase (EnvVar);
{$ASMMODE INTEL}
 asm
  cld
  mov edi, Environment
  lea esi, EnvVar
  xor eax, eax
  lodsb
@NewVar:
  cmp byte ptr [edi], 0
  jz @Stop
  push eax        { eax contains length of searched variable name }
  push esi        { esi points to the beginning of the variable name }
  mov ecx, -1     { our character ('=' - see below) _must_ be found }
  mov edx, edi    { pointer to beginning of variable name saved in edx }
  mov al, '='     { searching until '=' (end of variable name) }
  repne
  scasb           { scan until '=' not found }
  neg ecx         { what was the name length? }
  dec ecx         { corrected }
  dec ecx         { exclude the '=' character }
  pop esi         { restore pointer to beginning of variable name }
  pop eax         { restore length of searched variable name }
  push eax        { and save both of them again for later use }
  push esi
  cmp ecx, eax    { compare length of searched variable name with name }
  jnz @NotEqual   { ... of currently found variable, jump if different }
  xchg edx, edi   { pointer to current variable name restored in edi }
  repe
  cmpsb           { compare till the end of variable name }
  xchg edx, edi   { pointer to beginning of variable contents in edi }
  jz @Equal       { finish if they're equal }
@NotEqual:
  xor eax, eax    { look for 00h }
  mov ecx, -1     { it _must_ be found }
  repne
  scasb           { scan until found }
  pop esi         { restore pointer to beginning of variable name }
  pop eax         { restore length of searched variable name }
  jmp @NewVar     { ... or continue with new variable otherwise }
@Stop:
  xor eax, eax
  mov P, eax      { Not found - return nil }
  jmp @End
@Equal:
  pop esi         { restore the stack position }
  pop eax
  mov P, edi      { place pointer to variable contents in P }
@End:
 end ['eax','ecx','edx','esi','edi'];
 GetEnvPChar := P;
end;
{$ASMMODE ATT}


Function GetEnv(envvar: string): string;
(* The assembler version is more than three times as fast as Pascal. *)
begin
 GetEnv := StrPas (GetEnvPChar (EnvVar));
end;


procedure GetFAttr (var F; var Attr: word);
var
  PathInfo: TFileStatus3;
  RC: cardinal;
{$ifndef FPC_ANSI_TEXTFILEREC}
  R: rawbytestring;
{$endif not FPC_ANSI_TEXTFILEREC}
  P: pchar;
begin
  Attr := 0;
{$ifdef FPC_ANSI_TEXTFILEREC}
  P := @FileRec (F).Name;
{$else FPC_ANSI_TEXTFILEREC}
  R := ToSingleByteFileSystemEncodedFileName (FileRec (F).Name);
  P := PChar (R);
{$endif FPC_ANSI_TEXTFILEREC}
  RC := DosQueryPathInfo (P, ilStandard, @PathInfo, SizeOf (PathInfo));
  DosError := integer (RC);
  if RC = 0 then
    Attr := PathInfo.AttrFile
  else
   OSErrorWatch (RC);
end;


procedure SetFAttr (var F; Attr: word);
var
  PathInfo: TFileStatus3;
  RC: cardinal;
{$ifndef FPC_ANSI_TEXTFILEREC}
  R: rawbytestring;
{$endif not FPC_ANSI_TEXTFILEREC}
  P: pchar;
begin
{$ifdef FPC_ANSI_TEXTFILEREC}
  P := @FileRec (F).Name;
{$else FPC_ANSI_TEXTFILEREC}
  R := ToSingleByteFileSystemEncodedFileName (FileRec (F).Name);
  P := PChar (R);
{$endif FPC_ANSI_TEXTFILEREC}
  RC := DosQueryPathInfo (P, ilStandard, @PathInfo, SizeOf (PathInfo));
  if RC = 0 then
   begin
    PathInfo.AttrFile := Attr;
    RC := DosSetPathInfo (P, ilStandard, @PathInfo, SizeOf (PathInfo),
                                                        doWriteThru);
    if RC <> 0 then
     OSErrorWatch (RC);
   end
  else
   OSErrorWatch (RC);
  DosError := integer (RC);
end;


{function  GetShortName(var p : String) : boolean;
begin
  GetShortName:=true;}
{$WARNING EA .shortname support (see FAT32 driver) should be probably added here!}
{end;

function  GetLongName(var p : String) : boolean;
begin
  GetLongName:=true;}
{$WARNING EA .longname support should be probably added here!}
{end;}



begin
 FillChar (LastExecRes, SizeOf (LastExecRes), 0);
 LastDosErrorModuleName := '';
 ExecFlags := 0;
 LastExecFlags := deSync;
end.
