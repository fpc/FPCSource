{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1997-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{****************************************************************************
                A platform independent FExpand implementation
****************************************************************************}

{$IFDEF FPC_FEXPAND_VOLUMES}
 {$IFNDEF FPC_FEXPAND_DRIVES}
  (* Volumes are just a special case of drives. *)
  {$DEFINE FPC_FEXPAND_DRIVES}
 {$ENDIF FPC_FEXPAND_DRIVES}
{$ENDIF FPC_FEXPAND_VOLUMES}

{$IFDEF FPC_FEXPAND_DIRSEP_IS_UPDIR}
 {$IFNDEF FPC_FEXPAND_DRIVESEP_IS_ROOT}
  (* If DriveSeparator is used for upper directory,       *)
  (* it cannot be used for marking root at the same time. *)
  {$DEFINE FPC_FEXPAND_DRIVESEP_IS_ROOT}
 {$ENDIF FPC_FEXPAND_DRIVESEP_IS_ROOT}
{$ENDIF FPC_FEXPAND_DIRSEP_IS_UPDIR}

procedure GetDirIO (DriveNr: byte; var Dir: OpenString);

(* GetDirIO is supposed to return the root of the given drive   *)
(* in case of an error for compatibility of FExpand with TP/BP. *)

var
  OldInOutRes: word;
begin
  OldInOutRes := InOutRes;
  InOutRes := 0;
  GetDir (DriveNr, Dir);
  InOutRes := OldInOutRes;
end;


{$IFDEF FPC_FEXPAND_VOLUMES}
{$IFNDEF FPC_FEXPAND_NO_DEFAULT_PATHS}
procedure GetDirIO (const VolumeName: OpenString; var Dir: OpenString);

var
  OldInOutRes: word;
begin
  OldInOutRes := InOutRes;
  InOutRes := 0;
  GetDir (VolumeName, Dir);
  InOutRes := OldInOutRes;
end;
{$ENDIF FPC_FEXPAND_NO_DEFAULT_PATHS}
{$ENDIF FPC_FEXPAND_VOLUMES}


function FExpand (const Path: PathStr): PathStr;

(* LFNSupport boolean constant, variable or function must be declared for all
   the platforms, at least locally in the Dos unit implementation part.
   In addition, FPC_FEXPAND_UNC, FPC_FEXPAND_DRIVES, FPC_FEXPAND_GETENV_PCHAR,
   FPC_FEXPAND_TILDE, FPC_FEXPAND_VOLUMES, FPC_FEXPAND_NO_DEFAULT_PATHS,
   FPC_FEXPAND_DRIVESEP_IS_ROOT, FPC_FEXPAND_NO_CURDIR,
   FPC_FEXPAND_NO_DOTS_UPDIR and FPC_FEXPAND_DIRSEP_IS_UPDIR conditionals might
   be defined to specify FExpand behaviour.
*)

{$IFDEF FPC_FEXPAND_DRIVES}
var
    PathStart: longint;
{$ELSE FPC_FEXPAND_DRIVES}
const
    PathStart = 1;
{$ENDIF FPC_FEXPAND_DRIVES}
{$IFDEF FPC_FEXPAND_UNC}
var
    RootNotNeeded: boolean;
{$ELSE FPC_FEXPAND_UNC}
const
    RootNotNeeded = false;
{$ENDIF FPC_FEXPAND_UNC}

var S, Pa, Dirs: PathStr;
    I, J: longint;

begin
{$IFDEF FPC_FEXPAND_UNC}
    RootNotNeeded := false;
{$ENDIF FPC_FEXPAND_UNC}

(* First convert the path to uppercase if appropriate for current platform. *)
    if FileNameCaseSensitive then
        Pa := Path
    else
        Pa := UpCase (Path);

(* Allow both '/' and '\' as directory separators *)
(* by converting all to the native one.           *)
    if DirectorySeparator = '\' then
    {Allow slash as backslash}
        begin
            for I := 1 to Length (Pa) do
                if Pa [I] = '/' then
                    Pa [I] := DirectorySeparator
        end
    else
    {Allow backslash as slash}
        begin
            for I := 1 to Length (Pa) do
                if Pa [I] = '\' then
                    Pa [I] := DirectorySeparator;
        end;

(* PathStart is amount of characters to strip to get beginning *)
(* of path without volume/drive specification.                 *)
{$IFDEF FPC_FEXPAND_DRIVES}
 {$IFDEF FPC_FEXPAND_VOLUMES}
  {$IFDEF FPC_FEXPAND_DRIVESEP_IS_ROOT}
    PathStart := Pos (DriveSeparator, Pa);
  {$ELSE FPC_FEXPAND_DRIVESEP_IS_ROOT}
    PathStart := Succ (Pos (DriveSeparator, Pa));
  {$ENDIF FPC_FEXPAND_DRIVESEP_IS_ROOT}
 {$ELSE FPC_FEXPAND_VOLUMES}
    PathStart := 3;
 {$ENDIF FPC_FEXPAND_VOLUMES}
{$ENDIF FPC_FEXPAND_DRIVES}

(* Expand tilde to home directory if appropriate. *)
{$IFDEF FPC_FEXPAND_TILDE}
    {Replace ~/ with $HOME/}
    if (Length (Pa) >= 1) and (Pa [1] = '~') and
                      ((Pa [2] = DirectorySeparator) or (Length (Pa) = 1)) then
        begin
 {$IFDEF FPC_FEXPAND_GETENV_PCHAR}
            S := StrPas (GetEnv ('HOME'));
 {$ELSE FPC_FEXPAND_GETENV_PCHAR}
            S := GetEnv ('HOME');
 {$ENDIF FPC_FEXPAND_GETENV_PCHAR}
            if (S = '') or (Length (S) = 1)
                                          and (S [1] = DirectorySeparator) then
                Delete (Pa, 1, 1)
            else
                if S [Length (S)] = DirectorySeparator then
                    Pa := S + Copy (Pa, 3, Length (Pa) - 2)
                else
                    Pa := S + Copy (Pa, 2, Pred (Length (Pa)));
        end;
{$ENDIF FPC_FEXPAND_TILDE}

(* Do we have a drive/volume specification? *)
{$IFDEF FPC_FEXPAND_VOLUMES}
    if PathStart > 1 then
{$ELSE FPC_FEXPAND_VOLUMES}
    if (Length (Pa) > 1) and (Pa [1] in ['A'..'Z', 'a'..'z']) and
                                                 (Pa [2] = DriveSeparator) then
{$ENDIF FPC_FEXPAND_VOLUMES}
        begin

(* We need to know current directory on given *)
(* volume/drive _if_ such a thing is defined. *)
{$IFDEF FPC_FEXPAND_DRIVES}
 {$IFNDEF FPC_FEXPAND_NO_DEFAULT_PATHS}
  {$IFDEF FPC_FEXPAND_VOLUMES}
            GetDirIO (Copy (Pa, 1, PathStart - 2), S);
  {$ELSE FPC_FEXPAND_VOLUMES}
            { Always uppercase driveletter }
            if (Pa [1] in ['a'..'z']) then
                Pa [1] := Chr (Ord (Pa [1]) and not ($20));
            GetDirIO (Ord (Pa [1]) - Ord ('A') + 1, S);
  {$ENDIF FPC_FEXPAND_VOLUMES}

(* Do we have more than just drive/volume specification? *)
            if Length (Pa) = Pred (PathStart) then

(* If not, just use the current directory for that drive/volume. *)
                Pa := S
            else

(* If yes, find out whether the following path is relative or absolute. *)
                if Pa [PathStart] <> DirectorySeparator then
  {$IFDEF FPC_FEXPAND_VOLUMES}
                    if Copy (Pa, 1, PathStart - 2) = Copy (S, 1, PathStart - 2)
                                                                           then
  {$ELSE FPC_FEXPAND_VOLUMES}
                    if Pa [1] = S [1] then
  {$ENDIF FPC_FEXPAND_VOLUMES}
                        begin
                            { remove ending slash if it already exists }
                            if S [Length (S)] = DirectorySeparator then
                                Dec (S [0]);
                            Pa := S + DirectorySeparator +
                              Copy (Pa, PathStart, Length (Pa) - PathStart + 1)
                        end
                    else
  {$IFDEF FPC_FEXPAND_VOLUMES}
                        Pa := Copy (Pa, 1, PathStart - 2) + DriveSeparator
                           + DirectorySeparator +
                              Copy (Pa, PathStart, Length (Pa) - PathStart + 1)
  {$ELSE FPC_FEXPAND_VOLUMES}
                        Pa := Pa [1] + DriveSeparator + DirectorySeparator +
                              Copy (Pa, PathStart, Length (Pa) - PathStart + 1)
  {$ENDIF FPC_FEXPAND_VOLUMES}
 {$ENDIF FPC_FEXPAND_NO_DEFAULT_PATHS}
        end
    else
{$ELSE FPC_FEXPAND_DRIVES}

(* If drives are not supported, but a drive *)
(* was supplied anyway, ignore (remove) it. *)
            Delete (Pa, 1, 2);
        end;
    {Check whether we don't have an absolute path already}
    if (Length (Pa) >= PathStart) and (Pa [PathStart] <> DirectorySeparator) or
                                                 (Length (Pa) < PathStart) then
{$ENDIF FPC_FEXPAND_DRIVES}
        begin

(* Get current directory on selected drive/volume. *)
            GetDirIO (0, S);
{$IFDEF FPC_FEXPAND_VOLUMES}
 {$IFDEF FPC_FEXPAND_DRIVESEP_IS_ROOT}
            PathStart := Pos (DriveSeparator, S);
 {$ELSE FPC_FEXPAND_DRIVESEP_IS_ROOT}
            PathStart := Succ (Pos (DriveSeparator, S));
 {$ENDIF FPC_FEXPAND_DRIVESEP_IS_ROOT}
{$ENDIF FPC_FEXPAND_VOLUMES}

(* Do we have an absolute path? *)
{$IFDEF FPC_FEXPAND_DRIVES}
            if (Length (Pa) > 0)
 {$IFDEF FPC_FEXPAND_DRIVESEP_IS_ROOT}
                                 and (Pa [1] = DriveSeparator)
 {$ELSE FPC_FEXPAND_DRIVESEP_IS_ROOT}
                                 and (Pa [1] = DirectorySeparator)
 {$ENDIF FPC_FEXPAND_DRIVESEP_IS_ROOT}
                                                                   then
                begin
 {$IFDEF FPC_FEXPAND_UNC}
                    {Do not touch network drive names}
                    if (Length (Pa) > 1) and (Pa [2] = DirectorySeparator)
                                                            and LFNSupport then
                        begin
                            PathStart := 3;
                            {Find the start of the string of directories}
                            while (PathStart <= Length (Pa)) and
                                      (Pa [PathStart] <> DirectorySeparator) do
                                Inc (PathStart);
                            if PathStart > Length (Pa) then
                            {We have just a machine name...}
                                if Length (Pa) = 2 then
                                {...or not even that one}
                                    PathStart := 2
                                else
                                    Pa := Pa + DirectorySeparator                            else
                                if PathStart < Length (Pa) then
                                {We have a resource name as well}
                                    begin
                                        RootNotNeeded := true;
                                        {Let's continue in searching}
                                        repeat
                                            Inc (PathStart);
                                        until (PathStart > Length (Pa)) or
                                         (Pa [PathStart] = DirectorySeparator);
                                    end;
                        end
                    else
 {$ENDIF FPC_FEXPAND_UNC}
 {$IFDEF FPC_FEXPAND_VOLUMES}
                        begin
                            I := Pos (DriveSeparator, S);
  {$IFDEF FPC_FEXPAND_DIRSEP_IS_UPDIR}
   {$IFDEF FPC_FEXPAND_DRIVESEP_IS_ROOT}
                            if (Pa [1] = DriveSeparator) then
                                Delete (Pa, 1, 1);
   {$ENDIF FPC_FEXPAND_DRIVESEP_IS_ROOT}
                            Pa := Copy (S, 1, I) + Pa;
                            PathStart := I;
  {$ELSE FPC_FEXPAND_DIRSEP_IS_UPDIR}
                            Pa := Copy (S, 1, Pred (I)) + DriveSeparator + Pa;
                            PathStart := Succ (I);
  {$ENDIF FPC_FEXPAND_DIRSEP_IS_UPDIR}
                        end;
 {$ELSE FPC_FEXPAND_VOLUMES}
                        Pa := S [1] + DriveSeparator + Pa;
 {$ENDIF FPC_FEXPAND_VOLUMES}
                end
            else
{$ENDIF FPC_FEXPAND_DRIVES}

                (* We already have a slash if root is the curent directory. *)
                if Length (S) = PathStart then
                    Pa := S + Pa
                else

                    (* We need an ending slash if FExpand was called  *)
                    (* with an empty string for compatibility, except *)
                    (* for platforms where this is invalid.           *)
                    if Length (Pa) = 0 then
{$IFDEF FPC_FEXPAND_DIRSEP_IS_UPDIR}
                        Pa := S
{$ELSE FPC_FEXPAND_DIRSEP_IS_UPDIR}
                        Pa := S + DirectorySeparator
{$ENDIF FPC_FEXPAND_DIRSEP_IS_UPDIR}
                    else
 {$IFDEF FPC_FEXPAND_DIRSEP_IS_UPDIR}
                        if Pa [1] = DirectorySeparator then
                            Pa := S + Pa
                        else
 {$ENDIF FPC_FEXPAND_DIRSEP_IS_UPDIR}
                        Pa := S + DirectorySeparator + Pa;
        end;

    {Get string of directories to only process relative references on this one}
    Dirs := Copy (Pa, Succ (PathStart), Length (Pa) - PathStart);

{$IFNDEF FPC_FEXPAND_NO_CURDIR}
    {First remove all references to '\.\'}
    I := Pos (DirectorySeparator + '.' + DirectorySeparator, Dirs);
    while I <> 0 do
        begin
            Delete (Dirs, I, 2);
            I := Pos (DirectorySeparator + '.' + DirectorySeparator, Dirs);
        end;
{$ENDIF FPC_FEXPAND_NO_CURDIR}

{$IFNDEF FPC_FEXPAND_NO_DOTS_UPDIR}
    {Now remove also all references to '\..\' + of course previous dirs..}
    I := Pos (DirectorySeparator + '..' + DirectorySeparator, Dirs);
    while I <> 0 do
        begin
            J := Pred (I);
            while (J > 0) and (Dirs [J] <> DirectorySeparator) do
                Dec (J);
            Delete (Dirs, Succ (J), I - J + 3);
            I := Pos (DirectorySeparator + '..' + DirectorySeparator, Dirs);
        end;
{$ENDIF FPC_FEXPAND_NO_DOTS_UPDIR}

{$IFDEF FPC_FEXPAND_DIRSEP_IS_UPDIR}
    (* Now remove all references to '//' plus previous directories... *)
    I := Pos (DirectorySeparator + DirectorySeparator, Dirs);
    while I <> 0 do
        begin
            J := Pred (I);
            while (J > 0) and (Dirs [J] <> DirectorySeparator) do
                Dec (J);
            Delete (Dirs, Succ (J), Succ (I - J));
            I := Pos (DirectorySeparator + DirectorySeparator, Dirs);
        end;
{$ENDIF FPC_FEXPAND_DIRSEP_IS_UPDIR}

{$IFNDEF FPC_FEXPAND_NO_DOTS_UPDIR}
    {Then remove also a reference to '\..' at the end of line
    + the previous directory, of course,...}
    I := Pos (DirectorySeparator + '..', Dirs);
    if (I <> 0) and (I = Length (Dirs) - 2) then
        begin
            J := Pred (I);
            while (J > 0) and (Dirs [J] <> DirectorySeparator) do
                Dec (J);
            if (J = 0) then
                Dirs := ''
            else
                Delete (Dirs, Succ (J), I - J + 2);
        end;
{$ENDIF FPC_FEXPAND_NO_DOTS_UPDIR}

{$IFDEF FPC_FEXPAND_DIRSEP_IS_UPDIR}
    (* Remove a possible reference to '/' at the *)
    (* end of line plus the previous directory.  *)
    I := Length (Dirs);
    if (I > 0) and (Dirs [I] = DirectorySeparator) then
        begin
            J := Pred (I);
            while (J > 0) and (Dirs [J] <> DirectorySeparator) do
                Dec (J);
            if (J = 0) then
                Dirs := ''
            else
                Delete (Dirs, J, Succ (I - J));
        end;
{$ENDIF FPC_FEXPAND_DIRSEP_IS_UPDIR}

{$IFNDEF FPC_FEXPAND_NO_CURDIR}
    {...and also a possible reference to '\.'}
    if (Length (Dirs) = 1) then
        begin
            if (Dirs [1] = '.') then
            {A special case}
                Dirs := ''
        end
    else
        if (Length (Dirs) <> 0) and (Dirs [Length (Dirs)] = '.') and
                        (Dirs [Pred (Length (Dirs))] = DirectorySeparator) then
            Dec (Dirs [0], 2);

    {Finally remove '.\' at the beginning of the string of directories...}
    while (Length (Dirs) >= 2) and (Dirs [1] = '.')
                                         and (Dirs [2] = DirectorySeparator) do
        Delete (Dirs, 1, 2);
{$ENDIF FPC_FEXPAND_NO_CURDIR}

{$IFDEF FPC_FEXPAND_DIRSEP_IS_UPDIR}
    (* Remove possible (invalid) references to '/' at the beginning. *)
    while (Length (Dirs) >= 1) and (Dirs [1] = '/') do
        Delete (Dirs, 1, 1);
{$ENDIF FPC_FEXPAND_DIRSEP_IS_UPDIR}

{$IFNDEF FPC_FEXPAND_NO_DOTS_UPDIR}
    {...and possible (invalid) references to '..\' as well}
    while (Length (Dirs) >= 3) and (Dirs [1] = '.') and (Dirs [2] = '.') and
                                             (Dirs [3] = DirectorySeparator) do
        Delete (Dirs, 1, 3);
{$ENDIF FPC_FEXPAND_NO_DOTS_UPDIR}

    {Two special cases - '.' and '..' alone}
{$IFNDEF FPC_FEXPAND_NO_CURDIR}
    if (Length (Dirs) = 1) and (Dirs [1] = '.') then
        Dirs := '';
{$ENDIF FPC_FEXPAND_NO_CURDIR}
{$IFNDEF FPC_FEXPAND_NO_DOTS_UPDIR}
    if (Length (Dirs) = 2) and (Dirs [1] = '.') and (Dirs [2] = '.') then
        Dirs := '';
{$ENDIF FPC_FEXPAND_NO_DOTS_UPDIR}

    {Join the parts back to create the complete path}
    if Length (Dirs) = 0 then
        begin
            Pa := Copy (Pa, 1, PathStart);
{$IFNDEF FPC_FEXPAND_DRIVESEP_IS_ROOT}
            if Pa [PathStart] <> DirectorySeparator then
                Pa := Pa + DirectorySeparator;
{$ENDIF FPC_FEXPAND_DRIVESEP_IS_ROOT}
        end
    else
        Pa := Copy (Pa, 1, PathStart) + Dirs;

{$IFNDEF FPC_FEXPAND_DIRSEP_IS_UPDIR}
    {Remove ending \ if not supplied originally, the original string
    wasn't empty (to stay compatible) and if not really needed}
    if (Pa [Length (Pa)] = DirectorySeparator)
         and ((Length (Pa) > PathStart) or
{A special case with UNC paths}
            (RootNotNeeded and (Length (Pa) = PathStart))) and
                     (Length (Path) <> 0)
                          and (Path [Length (Path)] <> DirectorySeparator) then
        Dec (Pa [0]);
{$ENDIF FPC_FEXPAND_DIRSEP_IS_UPDIR}

    FExpand := Pa;
end;

{
  $Log$
  Revision 1.15  2002-12-07 16:26:39  hajny
    * '//' behaviour for Amiga corrected

  Revision 1.14  2002/12/01 20:46:44  hajny
    * Amiga support hopefully finished

  Revision 1.13  2002/11/25 21:03:57  hajny
    * Amiga fixes (among others)

  Revision 1.12  2002/11/24 15:49:22  hajny
    * make use of constants available in the system unit

  Revision 1.11  2002/09/07 15:07:45  peter
    * old logs removed and tabs fixed

  Revision 1.10  2002/05/14 19:25:24  hajny
    * fix for bug 1964 merged

  Revision 1.9  2002/03/03 15:19:36  carl
  * fixes unix conversion of slashes

}
