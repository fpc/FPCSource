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
procedure GetDirIO (const VolumeName: OpenString; var Dir: OpenString);

var
  OldInOutRes: word;
begin
  OldInOutRes := InOutRes;
  InOutRes := 0;
  GetDir (VolumeName, Dir);
  InOutRes := OldInOutRes;
end;
{$ENDIF FPC_FEXPAND_VOLUMES}


function FExpand (const Path: PathStr): PathStr;

(* LFNSupport boolean constant, variable or function must be declared for all
   the platforms, at least locally in the Dos unit implementation part.
   In addition, FPC_FEXPAND_UNC, FPC_FEXPAND_DRIVES, FPC_FEXPAND_GETENV_PCHAR,
   FPC_FEXPAND_TILDE and FPC_FEXPAND_VOLUMES conditionals might be defined to
   specify FExpand behaviour. Only forward slashes are supported if UNIX
   conditional is defined, both forward and backslashes otherwise.
*)

const
{$IFDEF UNIX}
    DirSep = '/';
{$ELSE UNIX}
    DirSep = '\';
{$ENDIF UNIX}
    DriveSep = ':';
{$IFDEF FPC_FEXPAND_DRIVES}
    PathStart: longint = 3;
{$ELSE FPC_FEXPAND_DRIVES}
    PathStart = 1;
{$ENDIF FPC_FEXPAND_DRIVES}
{$IFDEF FPC_FEXPAND_UNC}
 RootNotNeeded: boolean = false;
{$ELSE FPC_FEXPAND_UNC}
 RootNotNeeded = false;
{$ENDIF FPC_FEXPAND_UNC}

var S, Pa, Dirs: PathStr;
    I, J: longint;

begin
{$IFDEF FPC_FEXPAND_UNC}
    RootNotNeeded := false;
{$ENDIF FPC_FEXPAND_UNC}
    if FileNameCaseSensitive then
        Pa := Path
    else
        Pa := UpCase (Path);
{$IFNDEF UNIX}
    {Allow slash as backslash}
    for I := 1 to Length (Pa) do
        if Pa [I] = '/' then
            Pa [I] := DirSep;
{$ELSE}            
    {Allow backslash as slash}
    for I := 1 to Length (Pa) do
        if Pa [I] = '\' then
            Pa [I] := DirSep;
{$ENDIF UNIX}
{$IFDEF FPC_FEXPAND_VOLUMES}
    PathStart := Succ (Pos (DriveSep, Pa));
{$ENDIF FPC_FEXPAND_VOLUMES}
{$IFDEF FPC_FEXPAND_TILDE}
    {Replace ~/ with $HOME/}
    if (Length (Pa) >= 1) and (Pa [1] = '~') and 
                                  ((Pa [2] = DirSep) or (Length (Pa) = 1)) then
        begin
 {$IFDEF FPC_FEXPAND_GETENV_PCHAR}
            S := StrPas (GetEnv ('HOME'));
 {$ELSE FPC_FEXPAND_GETENV_PCHAR}
            S := GetEnv ('HOME');
 {$ENDIF FPC_FEXPAND_GETENV_PCHAR}
            if (S = '') or (Length (S) = 1) and (S [1] = DirSep) then
                Delete (Pa, 1, 1)
            else
                if S [Length (S)] = DirSep then
                    Pa := S + Copy (Pa, 3, Length (Pa) - 2)
                else
                    Pa := S + Copy (Pa, 2, Pred (Length (Pa)));
        end;
{$ENDIF FPC_FEXPAND_TILDE}
{$IFDEF FPC_FEXPAND_VOLUMES}
    if PathStart > 1 then
{$ELSE FPC_FEXPAND_VOLUMES}
    if (Length (Pa) > 1) and (Pa [1] in ['A'..'Z', 'a'..'z']) and
                                                       (Pa [2] = DriveSep) then
{$ENDIF FPC_FEXPAND_VOLUMES}
        begin
{$IFDEF FPC_FEXPAND_DRIVES}
 {$IFDEF FPC_FEXPAND_VOLUMES}
            GetDirIO (Copy (Pa, 1, PathStart - 2), S);
 {$ELSE FPC_FEXPAND_VOLUMES}
            { Always uppercase driveletter }
            if (Pa [1] in ['a'..'z']) then
                Pa [1] := Chr (Ord (Pa [1]) and not ($20));
            GetDirIO (Ord (Pa [1]) - Ord ('A') + 1, S);
 {$ENDIF FPC_FEXPAND_VOLUMES}
            if Length (Pa) = Pred (PathStart) then
                Pa := S
            else
                if Pa [PathStart] <> DirSep then
 {$IFDEF FPC_FEXPAND_VOLUMES}
                    if Copy (Pa, 1, PathStart - 2) = Copy (S, 1, PathStart - 2)
                                                                           then
 {$ELSE FPC_FEXPAND_VOLUMES}
                    if Pa [1] = S [1] then
 {$ENDIF FPC_FEXPAND_VOLUMES}
                        begin
                            { remove ending slash if it already exists }
                            if S [Length (S)] = DirSep then
                                Dec (S [0]);
                            Pa := S + DirSep +
                              Copy (Pa, PathStart, Length (Pa) - PathStart + 1)
                        end
                    else
 {$IFDEF FPC_FEXPAND_VOLUMES}
                        Pa := Copy (Pa, 1, PathStart - 2) + DriveSep + DirSep +
                              Copy (Pa, PathStart, Length (Pa) - PathStart + 1)
 {$ELSE FPC_FEXPAND_VOLUMES}
                        Pa := Pa [1] + DriveSep + DirSep +
                              Copy (Pa, PathStart, Length (Pa) - PathStart + 1)
 {$ENDIF FPC_FEXPAND_VOLUMES}
        end
    else
{$ELSE FPC_FEXPAND_DRIVES}
            Delete (Pa, 1, 2);
        end;
    {Check whether we don't have an absolute path already}
    if (Length (Pa) >= PathStart) and (Pa [PathStart] <> DirSep) or
                                                 (Length (Pa) < PathStart) then
{$ENDIF FPC_FEXPAND_DRIVES}
        begin
            GetDirIO (0, S);
{$IFDEF FPC_FEXPAND_DRIVES}
            if (Length (Pa) > 0) and (Pa [1] = DirSep) then
                begin
 {$IFDEF FPC_FEXPAND_UNC}
                    {Do not touch network drive names}
                    if (Length (Pa) > 1) and (Pa [2] = DirSep)
                                                            and LFNSupport then
                        begin
                            PathStart := 3;
                            {Find the start of the string of directories}
                            while (PathStart <= Length (Pa)) and
                                                  (Pa [PathStart] <> DirSep) do
                                Inc (PathStart);
                            if PathStart > Length (Pa) then
                            {We have just a machine name...}
                                if Length (Pa) = 2 then
                                {...or not even that one}
                                    PathStart := 2
                                else
                                    Pa := Pa + DirSep
                            else
                                if PathStart < Length (Pa) then
                                {We have a resource name as well}
                                    begin
                                        RootNotNeeded := true;
                                        {Let's continue in searching}
                                        repeat
                                            Inc (PathStart);
                                        until (PathStart > Length (Pa)) or
                                                     (Pa [PathStart] = DirSep);
                                    end;
                        end
                    else
 {$ENDIF FPC_FEXPAND_UNC}
 {$IFDEF FPC_FEXPAND_VOLUMES}
                        begin
                            I := Pos (DriveSep, S);
                            Pa := Copy (S, 1, Pred (I)) + DriveSep + Pa;
                            PathStart := Succ (I);
                        end;
 {$ELSE FPC_FEXPAND_VOLUMES}
                        Pa := S [1] + DriveSep + Pa;
 {$ENDIF FPC_FEXPAND_VOLUMES}
                end
            else
{$ENDIF FPC_FEXPAND_DRIVES}
                (* We already have a slash if root is the curent directory. *)
                if Length (S) = PathStart then
                    Pa := S + Pa
                else
                    (* We need an ending slash if FExpand was called
                    with an empty string for compatibility. *)
                    if Length (Pa) = 0 then
                        Pa := S + DirSep
                    else
                        Pa := S + DirSep + Pa;
        end;
    {Get string of directories to only process relative references on this one}
    Dirs := Copy (Pa, Succ (PathStart), Length (Pa) - PathStart);
    {First remove all references to '\.\'}
    I := Pos (DirSep + '.' + DirSep, Dirs);
    while I <> 0 do
        begin
            Delete (Dirs, I, 2);
            I := Pos (DirSep + '.' + DirSep, Dirs);
        end;
    {Now remove also all references to '\..\' + of course previous dirs..}
    I := Pos (DirSep + '..' + DirSep, Dirs);
    while I <> 0 do
        begin
            J := Pred (I);
            while (J > 0) and (Dirs [J] <> DirSep) do
                Dec (J);
            Delete (Dirs, Succ (J), I - J + 3);
            I := Pos (DirSep + '..' + DirSep, Dirs);
        end;
    {Then remove also a reference to '\..' at the end of line
    + the previous directory, of course,...}
    I := Pos (DirSep + '..', Dirs);
    if (I <> 0) and (I = Length (Dirs) - 2) then
        begin
            J := Pred (I);
            while (J >= 0) and (Dirs [J] <> DirSep) do
                Dec (J);
            if (J = 0) then
                Dirs := ''
            else
                Delete (Dirs, Succ (J), I - J + 2);
        end;
    {...and also a possible reference to '\.'}
    if (Length (Dirs) = 1) then
        begin
            if (Dirs [1] = '.') then
            {A special case}
                Dirs := ''
        end
    else
        if (Length (Dirs) <> 0) and (Dirs [Length (Dirs)] = '.') and
                                    (Dirs [Pred (Length (Dirs))] = DirSep) then
            Dec (Dirs [0], 2);
    {Finally remove '.\' at the beginning of the string of directories...}
    while (Length (Dirs) >= 2) and (Dirs [1] = '.') and (Dirs [2] = DirSep) do
        Delete (Dirs, 1, 2);
    {...and possible (invalid) references to '..\' as well}
    while (Length (Dirs) >= 3) and (Dirs [1] = '.') and (Dirs [2] = '.') and
                                                         (Dirs [3] = DirSep) do
        Delete (Dirs, 1, 3);
    {Two special cases - '.' and '..' alone}
    if (Length (Dirs) = 1) and (Dirs [1] = '.') or
             (Length (Dirs) = 2) and (Dirs [1] = '.') and (Dirs [2] = '.') then
        Dirs := '';
    {Join the parts back to create the complete path}
    if Length (Dirs) = 0 then
        begin
            Pa := Copy (Pa, 1, PathStart);
            if Pa [PathStart] <> DirSep then
                Pa := Pa + DirSep;
        end
    else
        Pa := Copy (Pa, 1, PathStart) + Dirs;
    {Remove ending \ if not supplied originally, the original string
    wasn't empty (to stay compatible) and if not really needed}
    if (Pa [Length (Pa)] = DirSep) and ((Length (Pa) > PathStart) or
{A special case with UNC paths}
            (RootNotNeeded and (Length (Pa) = PathStart))) and
                 (Length (Path) <> 0) and (Path [Length (Path)] <> DirSep) then
        Dec (Pa [0]);
    FExpand := Pa;
end;

{
  $Log$
  Revision 1.9  2002-03-03 15:19:36  carl
  * fixes unix conversion of slashes

  Revision 1.8  2001/04/09 19:28:40  hajny
    * yet another fix for FExpand under Unix

  Revision 1.5  2001/03/21 21:08:20  hajny
    * GetDir fixed

  Revision 1.4  2001/03/19 21:09:30  hajny
    * one more problem in the Unix part

  Revision 1.3  2001/03/19 21:05:42  hajny
    * mistyping in the Unix part fixed

  Revision 1.2  2001/03/10 09:57:51  hajny
    * FExpand without IOResult change, remaining direct asm removed

 
}
