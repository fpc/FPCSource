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


function GetDirIO (DriveNr: byte; var Dir: OpenString): word;
                                                [external name 'FPC_GETDIRIO'];

(* GetDirIO is supposed to return the root of the given drive   *)
(* in case of an error for compatibility of FExpand with TP/BP. *)
(* Dir must be specified as OpenString since System has $P+.    *)


function FExpand (const Path: PathStr): PathStr;

(* LFNSupport boolean constant, variable or function must be declared for all
   the platforms, at least locally in the Dos unit implementation part.
   In addition, FPC_FEXPAND_UNC, FPC_FEXPAND_DRIVES, FPC_FEXPAND_GETENV_PCHAR
   and FPC_FEXPAND_TILDE conditionals might be defined to specify FExpand
   behaviour. Only forward slashes are supported if UNIX conditional
   is defined, both forward and backslashes otherwise.
*)

const
{$IFDEF UNIX}
    DirSep = '/';
{$ELSE UNIX}
    DirSep = '\';
{$ENDIF UNIX}
{$IFDEF FPC_FEXPAND_DRIVES}
    PathStart = 3;
{$ELSE FPC_FEXPAND_DRIVES}
    PathStart = 1;
{$ENDIF FPC_FEXPAND_DRIVES}

var S, Pa: PathStr;
    I, J: longint;

begin
    if FileNameCaseSensitive then
        Pa := Path
    else
        Pa := UpCase (Path);
{$IFNDEF UNIX}
    {Allow slash as backslash}
    for I := 1 to Length (Pa) do
        if Pa [I] = '/' then
            Pa [I] := DirSep;
{$ENDIF}
{$IFDEF FPC_FEXPAND_TILDE}
    {Replace ~/ with $HOME}
    if (Length (Pa) >= 1) and (Pa [1] ='~') and 
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
                    Pa := S + Copy (Pa, 3, Length (Pa - 2))
                else
                    Pa := S + Copy (Pa, 2, Pred (Length (Pa)));
   end;
{$ENDIF FPC_FEXPAND_TILDE}
    if (Length (Pa) > 1) and (Pa [1] in ['A'..'Z', 'a'..'z']) and
                                                            (Pa [2] = ':') then
        begin
{$IFDEF FPC_FEXPAND_DRIVES}
            { Always uppercase driveletter }
            if (Pa [1] in ['a'..'z']) then
                Pa [1] := Chr (Ord (Pa [1]) and not ($20));
            if GetDirIO (Ord (Pa [1]) - Ord ('A') + 1, S) = 0 then ;
            case Length (Pa) of
                2: Pa := S;
            else
                if Pa [3] <> DirSep then
                    if Pa [1] = S [1] then
                        begin
                            { remove ending slash if it already exists }
                            if S [Length (S)] = DirSep then
                                Dec (S [0]);
                            Pa := S + DirSep + Copy (Pa, 3, Length (Pa))
                        end
                    else
                        Pa := Pa [1] + ':' + DirSep + Copy (Pa, 3, Length (Pa))
            end;
        end
    else
{$ELSE FPC_FEXPAND_DRIVES}
            Delete (Path, 1, 2);
            Delete (Pa, 1, 2);
        end;
{$ENDIF FPC_FEXPAND_DRIVES}
        begin
            if GetDirIO (0, S) = 0 then ;
{$IFDEF FPC_FEXPAND_DRIVES}
            if (Length (Pa) > 0) and (Pa [1] = DirSep) then
                begin
 {$IFDEF FPC_FEXPAND_UNC}
                    { Do not touch Network drive names }
                    if not ((Length (Pa) > 1) and (Pa [2] = Pa [1])
                                                           and LFNSupport) then
 {$ENDIF FPC_FEXPAND_UNC}
                        Pa := S [1] + ':' + Pa
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
    {First remove all references to '\.\'}
    I := Pos (DirSep + '.' + DirSep, Pa);
    while I <> 0 do
        begin
            Delete (Pa, I, 2);
            I := Pos (DirSep + '.' + DirSep, Pa);
        end;
    {Now remove also all references to '\..\' + of course previous dirs..}
    I := Pos (DirSep + '..' + DirSep, Pa);
    while I <> 0 do
        begin
            J := Pred (I);
            while (J > 0) and (Pa [J] <> DirSep) do
                Dec (J);
            if (J = 0)
{$IFDEF FPC_FEXPAND_UNC}
                       or (J = 1) and (I = 2)
{$ENDIF FPC_FEXPAND_UNC}
                                              then
                Delete (Pa, Succ (I), 3)
            else
                Delete (Pa, Succ (J), I - J + 3);
            I := Pos (DirSep + '..' + DirSep, Pa);
        end;
    {Now remove also any reference to '\..' at the end of line
    + of course previous dir..}
    I := Pos (DirSep + '..', Pa);
    if (I <> 0) and (I = Length (Pa) - 2) then
        begin
            J := Pred (I);
            while (J >= 1) and (Pa [J] <> DirSep) do
                Dec (J);
            if (J = 0)
{$IFDEF FPC_FEXPAND_UNC}
                       or (J = 1) and (I = 2)
{$ENDIF FPC_FEXPAND_UNC}
                                              then
                Delete (Pa, Succ (I), 2)
            else
                Delete (Pa, Succ (J), I - J + 2);
        end;
    {Now remove also any reference to '\.' at the end of line}
    I := Pos (DirSep + '.', Pa);
    if (I <> 0) and (I = Pred (Length (Pa))) then
{$IFDEF FPC_FEXPAND_DRIVES}
        if (I = 3) and (Pa [2] = ':')
{$ELSE FPC_FEXPAND_DRIVES}
        if (I = 1)
{$ENDIF FPC_FEXPAND_DRIVES}
{$IFDEF FPC_FEXPAND_UNC}
                                      or (I = 2) and (Pa [1] = '\')
{$ENDIF FPC_FEXPAND_UNC}
                                                                    then
            Dec (Pa [0])
        else
            Delete (Pa, I, 2);
    {Remove ending \ if not supplied originally, the original string
    wasn't empty (to stay compatible) and if not really needed}
    if (Length (Pa) > PathStart) and (Pa [Length (Pa)] = DirSep)
             and (Length (Path) <> 0) and (Path [Length (Path)] <> DirSep) then
        Dec (Pa [0]);
    FExpand := Pa;
end;

{
  $Log$
  Revision 1.2  2001-03-10 09:57:51  hajny
    * FExpand without IOResult change, remaining direct asm removed

 
}
