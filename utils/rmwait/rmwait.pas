{
    rmwait - remove (delete) file(s) with optional retries
    Copyright (C) 2009 by Tomas Hajny, member of the Free Pascal team

    This tool tries to mimic behaviour of GNU rm, but it provides
    the additional feature of retries and it also fixes some issues
    appearing at least with the Win32 port of version 3.13.

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

program rmwait;
{$D
Remove (delete) file(s) with optional retries.
}

{ $DEFINE DONOTHING}

uses
{$IFDEF GO32V2}
  Go32,
{$ENDIF GO32V2}
{$IFDEF OS2}
  DosCalls,
{$ENDIF OS2}
{$IFDEF WINDOWS}
  Windows,
{$ENDIF WINDOWS}
{$IFDEF UNIX}
  BaseUnix,
{$ENDIF UNIX}
  Dos;

const
  OptDirectories: boolean = false;
  OptForce: boolean = false;
  OptInteractive: boolean = false;
  OptRecursive: boolean = false;
  OptVerbose: boolean = false;
  OptRetries: longint = 1;
  OptWait: longint = 5;
  OptsStop: boolean = false;


var
  OldExit: pointer;
  Deleted: cardinal;

procedure VerbLine (S: string); inline;
begin
  if OptVerbose then
    WriteLn (S);
end;


procedure ForceErrorLn (S: string); inline;
begin
  WriteLn (ParamStr (0), ': ', S);
end;


procedure ErrorLn (S: string); inline;
begin
{  if not (OptForce) then}
   ForceErrorLn (S);
end;


procedure GenericErrorLn (S: string; N: longint); inline;
begin
  if not (OptForce) then
   WriteLn (ParamStr (0), ': ', S, ' (', N, ')');
end;


procedure ClearIO; inline;
begin
  if IOResult <> 0 then ;
end;


procedure Wait (Seconds: Cardinal);
{$IFDEF GO32v2}
var
  R: Registers;
  T0, T1, T2: int64;
  DayOver: boolean;
begin
(* Sleep is supposed to give up time slice - DOS Idle Interrupt chosen
   because it should be supported in all DOS versions. *)
  R.AH := $2C;
  RealIntr($21, R);
  T0 := R.CH * 3600 + R.CL * 60 + R.DH;
  T2 := T0 + Seconds;
  DayOver := T2 > (24 * 3600);
  repeat
    Intr ($28, R);
(*    R.AH := $2C; - should be preserved. *)
    RealIntr($21, R);
    T1 := R.CH * 3600 + R.CL * 60 + R.DH;
    if DayOver and (T1 < T0) then
     Inc (T1, 24 * 3600);
  until T1 >= T2;
end;
{$ELSE GO32v2}
 {$IFDEF OS2}
begin
  DosSleep (Seconds * 1000);
end;
 {$ELSE OS2}
  {$IFDEF UNIX}
begin
  fpSleep (Seconds * 1000);
end;
  {$ELSE UNIX}
   {$IFDEF WINDOWS}
begin
  Sleep (Seconds * 1000);
end;
   {$ELSE WINDOWS}
var
  T0, T1, T2: int64;
begin
{$WARNING No sleeping is performed with this platform!}
  T0 := GetMSCount;
  T2 := T0 + Seconds * 1000;
  repeat
    T1 := GetMSCount;
(* GetMSCount returning lower value than in the first check indicates overflow
   and is treated as end of the waiting period due to undefined range. *)
  until (T1 >= T2) or (T1 < T0);
end;
   {$ENDIF WINDOWS}
  {$ENDIF UNIX}
 {$ENDIF OS2}
{$ENDIF GO32v2}


procedure ClearAttribs (var F: file); inline;
var
  W: word;
begin
{$I-}
  GetFAttr (F, W);
  if W and (ReadOnly or SysFile) <> 0 then
    SetFAttr (F, W and not ReadOnly and not SysFile);
  ClearIO;
{$I+}
end;


function StrF (U: cardinal): string; inline;
begin
  Str (U, StrF);
end;


function CheckOK (Msg: string; FN: PathStr): boolean;
var
  Resp: string;
begin
  Write (ParamStr (0), ': ', Msg, '''', FN, '''? ');
  ReadLn (Resp);
  CheckOK := (Length (Resp) > 0) and (UpCase (Resp [1]) = 'Y');
end;


procedure DelFile (FN: PathStr); inline;
var
  F: file;
  R, Tries: longint;
begin
  VerbLine ('removing ''' + FN + '''');
  Inc (Deleted);
  if not (OptInteractive) or CheckOK ('remove ', FN) then
    begin
      Assign (F, FN);
      if OptForce then
        ClearAttribs (F);
      Tries := 1;
      repeat
{$I-}
{$IFDEF DONOTHING}
        WriteLn ('Debug: ', FN);
{$ELSE DONOTHING}
        Erase (F);
{$ENDIF DONOTHING}
        R := IOResult;
{$I+}
        Inc (Tries);
        if (R = 5) and (Tries <= OptRetries) then
          Wait (OptWait);
      until (R <> 5) or (Tries > OptRetries);
      case R of
        0: ;
        2: ErrorLn (FN + ': No such file or directory');
        5: ErrorLn (FN + ': Permission denied');
      else
       GenericErrorLn (FN + ': Cannot be removed', R);
      end;
    end;
end;


procedure DelDir (FN: PathStr); inline;
var
  F: file;
  R, Tries: longint;
begin
  VerbLine ('removing ''' + FN + '''');
  Inc (Deleted);
  if not (OptInteractive) or CheckOK ('remove directory ', FN) then
    begin
      if OptForce then
        begin
          Assign (F, FN);
          ClearAttribs (F);
        end;
      Tries := 1;
      repeat
{$I-}
{$IFDEF DONOTHING}
        WriteLn ('Debug: Directory ', FN);
{$ELSE DONOTHING}
        RmDir (FN);
{$ENDIF DONOTHING}
        R := IOResult;
{$I+}
        Inc (Tries);
        if (R = 5) and (Tries <= OptRetries) then
         begin
          VerbLine ('Removal attempt failed, waiting ' + StrF (OptWait) + ' seconds before trying again...');
          Wait (OptWait);
         end;
      until (R <> 5) or (Tries > OptRetries);
      case R of
        0: ;
        5: ErrorLn (FN + ': Permission denied');
      else
       GenericErrorLn (FN + ': Cannot be removed', R);
      end;
    end;
end;


procedure Syntax;
begin
  WriteLn;
  WriteLn ('RMWait - remove (delete) file(s) with optional retries');
  WriteLn;
  WriteLn ('Syntax:');
  WriteLn (ParamStr (0) + ' [<options>...] [<file specifications>...]');
  WriteLn;
  WriteLn ('<file specifications> may contain wildcards ''*'' and ''?''.');
  WriteLn;
  WriteLn ('Options:');
  WriteLn (' -d, --directory                 remove directory. even if non-empty');
  WriteLn (' -f, --force                     ignore non-existent files, never prompt');
  WriteLn (' -i, --interactive               prompt before any removal');
  WriteLn (' -r, -R, --recursive             remove the contents of directories recursively');
  WriteLn (' -v, --verbose                   explain what is being done');
  WriteLn (' --version                       output version information and exit');
  WriteLn (' -h, -?, --help                  display this help and exit');
  WriteLn (' -t[<N>[,<T>]], --try[<N>[,<T>]] in case of errors, retry deleting N times');
  WriteLn ('                                 (default 3 times) waiting T seconds between');
  WriteLn ('                                 individual attempts (default 5 seconds)');
  WriteLn (' --                              stop processing of options');
  WriteLn;
  WriteLn ('To remove a file whose name starts with a ''-'', for example ''-file'',');
  WriteLn ('use one of these commands:');
  WriteLn (' rm -- -file');
  WriteLn (' rm ./-file');
  WriteLn;
  Halt;
end;


procedure ParError (S: string); inline;
begin
  ForceErrorLn (S);
  WriteLn;
  Syntax;
end;


procedure ProcessFSpec (FN: PathStr);
var
  SR: SearchRec;
  D, BaseDir: DirStr;
  N, BaseName: NameStr;
  E: ExtStr;
  RemFNDir: boolean;
begin
  RemFNDir := false;
{$IF NOT DEFINED (OS2) and NOT DEFINED (WINDOWS) and NOT DEFINED (DPMI) and NOT DEFINED (UNIX) and NOT DEFINED (MACOS) and NOT DEFINED (AMIGA) and NOT DEFINED (NETWARE)}
 {$WARNING Proper behaviour for this target platform has not been checked!}
{$ENDIF}
{$IF NOT DEFINED (MACOS) and NOT DEFINED (AMIGA)}
(* Special case - root directory needs to be treated in a special way. *)
 {$IFDEF UNIX}
 if (Length (FN) = 1)
 {$ELSE UNIX}
  {$IF DEFINED (OS2) or DEFINED (WINDOWS) or DEFINED (DPMI)}
 if (((Length (FN) = 3) and (FN [2] = DriveSeparator))
        or ((Length (FN) = 2) and (FN [1] = DirectorySeparator)))
(* Root of UNC path - nonsense, but changing it to root of current drive would be dangerous. *)
  {$ELSE}
   {$IFDEF NETWARE}
 if (Length (FN) = Pos (DirectorySeparator, FN))
   {$ENDIF NETWARE}
  {$ENDIF}
      and (FN [Length (FN)] = DirectorySeparator) then
 {$ENDIF UNIX}
  if OptRecursive then
    begin
      BaseDir := FN;
      BaseName := AllFilesMask;
    end
  else
    begin
      ErrorLn (FN + ': is a directory');
      Exit;
    end
 else
{$ENDIF}
  begin
(* Check if the specification directly corresponds to a directory *)
  if FN [Length (FN)] = DirectorySeparator then
    Delete (FN, Length (FN), 1);
  FSplit (FN, D, N, E);
  FindFirst (FN, (AnyFile or Directory) and not VolumeID, SR);
  if (DosError = 0) and (SR.Attr and Directory = Directory) and
                                                ((SR.Name = N + E) or
(* Checking equal names is not sufficient with case preserving file systems. *)
                              (Pos ('?', FN) = 0) and (Pos ('*', FN) = 0)) then
    if OptRecursive then
     begin
      BaseDir := FN;
      if BaseDir [Length (BaseDir)] <> DirectorySeparator then
       BaseDir := BaseDir + DirectorySeparator;
      BaseName := AllFilesMask;
      RemFNDir := true;
     end
    else
     if OptDirectories then
      RemFNDir := true
     else
      begin
       ErrorLn (FN + ': is a directory');
       Exit;
      end
  else
    begin
      BaseDir := D;
      BaseName := N + E;
    end;
  FindClose (SR);
 end;
  FindFirst (BaseDir + BaseName, AnyFile and not Directory and not VolumeID, SR);
  while DosError = 0 do
    begin
      DelFile (BaseDir + SR.Name);
      FindNext (SR);
    end;
  FindClose (SR);

  if OptRecursive then
    begin
      FindFirst (BaseDir + BaseName, (AnyFile or Directory) and not VolumeID, SR);
      while DosError = 0 do
        begin
          if (SR.Attr and Directory > 0) and
           ((Length (SR.Name) <> 1) or (SR.Name [1] <> '.')) and
           ((Length (SR.Name) <> 2) or (SR.Name [1] <> '.') or (SR.Name [2] <> '.')) and
           (not (OptInteractive) or CheckOK ('descend directory ', BaseDir + SR.Name)) then
            ProcessFSpec (BaseDir + SR.Name);
          FindNext (SR);
        end;
      FindClose (SR);
    end;
  if RemFNDir then
    DelDir (FN);
end;


procedure NewExit; far;
begin
  ExitProc := OldExit;
  if (ErrorAddr <> nil) or (ExitCode <> 0) then
    begin
      ErrorAddr := nil;
      case ExitCode of
        202: WriteLn ('Directory tree too deep!!');
        4: WriteLn ('Increase the FILES directive in CONFIG.SYS!!');
        5, 101, 150..152, 154, 156..158, 160..162: WriteLn ('I/O error (',
                                                              ExitCode, ')!!');
      else
        WriteLn ('Internal error (', ExitCode, ')!!');
      end;
      WriteLn;
    end;
end;


procedure AllowSlash (var S: string); inline;
var
  I: byte;
begin
  if DirectorySeparator <> '/' then
    for I := 1 to Length (S) do
      begin
        if S [I] = '/' then
         S [I] := DirectorySeparator;
      end;
end;


procedure ProcessOpts (S: string);
var
  I: longint;

  procedure ParseOptTries; inline;
  var
    SN: string;
    J, N, Err: longint;
  begin
    J := Succ (I);
    while (J <= Length (S)) and (S [J] in ['0'..'9']) do
     Inc (J);
    if J = Succ (I) then
     OptRetries := 3
    else
     begin
      SN := Copy (S, Succ (I), J - I - 1);
      Val (SN, N, Err);
      if Err <> 0 then
       ParError ('invalid value for retry attempts ''' + SN + '''');
      OptRetries := N;
      I := Pred (J);
      if (J < Length (S)) and (S [J] = ',') then
       begin
        Inc (J);
        Inc (I);
        while (J <= Length (S)) and (S [J] in ['0'..'9']) do
         Inc (J);
        if J > Succ (I) then
         begin
          SN := Copy (S, Succ (I), J - I - 1);
          Val (SN, N, Err);
          if Err <> 0 then
           ParError ('invalid value for retry wait time ''' + SN + '''');
          OptWait := N;
          I := Pred (J);
         end;
       end;
     end;
  end;

begin
  if S [2] = '-' then
   if Length (S) = 2 then
    OptsStop := true
   else
    begin
      Delete (S, 1, 2);
      for I := 1 to Length (S) do
       S [I] := Upcase (S [I]);
      if S = 'HELP' then Syntax;
      if S = 'DIRECTORY' then
       OptDirectories := true
      else if S = 'FORCE' then
       OptForce := true
      else if S = 'INTERACTIVE' then
       OptInteractive := true
      else if S = 'RECURSIVE' then
       OptRecursive := true
      else if S = 'VERBOSE' then
       OptVerbose := true
      else if S = 'VERSION' then
       begin
        WriteLn ('rmwait - version 20091101');
        Halt;
       end
      else if Copy (S, 1, 3) = 'TRY' then
       begin
        I := 3;
        ParseOptTries;
        if I <> Length (S) then
         ParError ('unrecognized option ''' + S + '''');
       end
      else
       ParError ('unrecognized option ''' + S + '''');
    end
  else
   begin
    I := 2;
    repeat
      case Upcase (S [I]) of
       'H', '?': Syntax;
       'D': OptDirectories := true;
       'F': OptForce := true;
       'I': OptInteractive := true;
       'R': OptRecursive := true;
       'V': OptVerbose := true;
       'T': ParseOptTries;
      else
       ParError ('invalid option -- ' + S [I])
      end;
      Inc (I);
    until (I > Length (S));
   end;
end;

var
  J, K: longint;
  Par: string;

begin
{$IFDEF OS2}
  DosCalls.DosError (0);
{$ENDIF}

  OldExit := ExitProc;
  ExitProc := @NewExit;

  J := ParamCount;
  if J = 0 then
    Syntax
  else
   begin
    K := 1;
    Par := ParamStr (K);

    while (K <= J) and (Par [1] = '-') and (Length (Par) > 1) and not OptsStop do
      begin
        ProcessOpts (Par);
        Inc (K);
        Par := ParamStr (K);
      end;

    if K > J then
     Syntax
    else
     repeat
       AllowSlash (Par);
       Deleted := 0;
       ProcessFSpec (FExpand (Par));
       if Deleted = 0 then
        ErrorLn (ParamStr (K) + ': No such file or directory');
       Inc (K);
       Par := ParamStr (K);
     until K > J;
   end;
end.
