(* LFNSupport boolean constant, variable or function must be declared for all
   the platforms, at least locally in the Dos unit implementation part.
   In addition, FEXPAND_UNC, FEXPAND_DRIVES, FEXPAND_GETENV_PCHAR
   and FEXPAND_TILDE conditionals might be defined to specify FExpand
   behaviour. Only forward slashes are supported if UNIX conditional
   is defined, both forward and backslashes otherwise.
*)

(* TODO: GetDir replacement function should appear here to remove
   the incorrect setting of IOResult within FExpand.
*)
{
    function get_current_drive:byte;assembler;
    asm
        movb $0x19,%ah
        call syscall
    end;
}
const
{$IFDEF UNIX}
    DirSep = '/';
{$ELSE UNIX}
    DirSep = '\';
{$ENDIF UNIX}
{$IFDEF FEXPAND_DRIVES}
    PathStart = 3;
{$ELSE FEXPAND_DRIVES}
    PathStart = 1;
{$ENDIF FEXPAND_DRIVES}

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
{$IFDEF FEXPAND_TILDE}
    {Replace ~/ with $HOME}
    if (Length (Pa) > 1) and (Pa [1] ='~') and (Pa [2] = DirSep) then
        begin
 {$IFDEF FEXPAND_GETENV_PCHAR}
            S := StrPas (GetEnv ('HOME'));
 {$ELSE FEXPAND_GETENV_PCHAR}
            S := GetEnv ('HOME');
 {$ENDIF FEXPAND_GETENV_PCHAR}
            if (S = '') or (Length (S) = 1) and (S [1] = DirSep) then
                Delete (Pa, 1, 1)
            else
                if S [Length (S)] = DirSep then
                    Pa := S + Copy (Pa, 3, Length (Pa - 2))
                else
                    Pa := S + Copy (Pa, 2, Pred (Length (Pa)));
   end;
{$ENDIF FEXPAND_TILDE}
    if (Length (Pa) > 1) and (Pa [1] in ['A'..'Z', 'a'..'z']) and
                                                            (Pa [2] = ':') then
        begin
{$IFDEF FEXPAND_DRIVES}
            { Always uppercase driveletter }
            if (Pa [1] in ['a'..'z']) then
                Pa [1] := Chr (Ord (Pa [1]) and not ($20));
            {We must get the right directory (should be changed to avoid
            touching IOResult)}
 {$IFOPT I+}
  {$DEFINE FEXPAND_WAS_I}
  {$I-}
 {$ENDIF}
            I := IOResult;
            GetDir (Ord (Pa [1]) - Ord ('A') + 1, S);
            I := IOResult;
 {$IFDEF FEXPAND_WAS_I}
  {$I+}
  {$UNDEF FEXPAND_WAS_I}
 {$ENDIF FEXPAND_WAS_I}
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
{$ELSE FEXPAND_DRIVES}
            Delete (Path, 1, 2);
            Delete (Pa, 1, 2);
        end;
{$ENDIF FEXPAND_DRIVES}
        begin
{$IFOPT I+}
 {$DEFINE FEXPAND_WAS_I}
 {$I-}
{$ENDIF}
            I := IOResult;
            GetDir (0, S);
            I := IOResult;
{$IFDEF FEXPAND_WAS_I}
 {$I+}
 {$UNDEF FEXPAND_WAS_I}
{$ENDIF FEXPAND_WAS_I}
{$IFDEF FEXPAND_DRIVES}
            if (Length (Pa) > 0) and (Pa [1] = DirSep) then
                begin
 {$IFDEF FEXPAND_UNC}
                    { Do not touch Network drive names }
                    if not ((Length (Pa) > 1) and (Pa [2] = Pa [1])
                                                           and LFNSupport) then
 {$ENDIF FEXPAND_UNC}
                        Pa := S [1] + ':' + Pa
                end
            else
{$ENDIF FEXPAND_DRIVES}
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
{$IFDEF FEXPAND_UNC}
                       or (J = 1) and (I = 2)
{$ENDIF FEXPAND_UNC}
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
{$IFDEF FEXPAND_UNC}
                       or (J = 1) and (I = 2)
{$ENDIF FEXPAND_UNC}
                                              then
                Delete (Pa, Succ (I), 2)
            else
                Delete (Pa, Succ (J), I - J + 2);
        end;
    {Now remove also any reference to '\.' at the end of line}
    I := Pos (DirSep + '.', Pa);
    if (I <> 0) and (I = Pred (Length (Pa))) then
        if (I = PathStart)
{$IFDEF FEXPAND_DRIVES}
                           and (Pa [2] = ':')
{$ENDIF FEXPAND_DRIVES}
{$IFDEF FEXPAND_UNC}
                                              or (I = 2) and (Pa [1] = '\')
{$ENDIF FEXPAND_UNC}
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
