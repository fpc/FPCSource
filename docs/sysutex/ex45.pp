Program Example45;

{ This program demonstrates the GetDirs function }
{$H+}

Uses sysutils;

Var Dirs : Array[0..127] of pchar;
    I,Count : longint;
    Dir,NewDir : String;

Begin
  Dir:=GetCurrentDir;
  Writeln ('Dir : ',Dir);
  NewDir:='';
  count:=GetDirs(Dir,Dirs);
  For I:=0 to Count-1 do
    begin
    NewDir:=NewDir+'/'+StrPas(Dirs[I]);
    Writeln (NewDir);
    end;
End.
