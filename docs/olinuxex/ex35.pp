Program Example35;

{ Program to demonstrate the
  OpenDir,ReadDir, SeekDir and TellDir functions. }

Uses oldlinux;

Var TheDir : PDir;
    ADirent : PDirent;
    Entry : Longint;

begin
  TheDir:=OpenDir('./.');
  Repeat
    Entry:=TellDir(TheDir);
    ADirent:=ReadDir (TheDir);
    If ADirent<>Nil then
      With ADirent^ do
        begin
        Writeln ('Entry No : ',Entry);
        Writeln ('Inode    : ',ino);
        Writeln ('Offset   : ',off);
        Writeln ('Reclen   : ',reclen);
        Writeln ('Name     : ',pchar(@name[0]));
        end;
  Until ADirent=Nil;
  Repeat
    Write ('Entry No. you would like to see again (-1 to stop): ');
    ReadLn (Entry);
    If Entry<>-1 then
      begin
      SeekDir (TheDir,Entry);
      ADirent:=ReadDir (TheDir);
      If ADirent<>Nil then
        With ADirent^ do
          begin
          Writeln ('Entry No : ',Entry);
          Writeln ('Inode    : ',ino);
          Writeln ('Offset   : ',off);
          Writeln ('Reclen   : ',reclen);
          Writeln ('Name     : ',pchar(@name[0]));
          end;
    end;
  Until Entry=-1;
  CloseDir (TheDir);
end.
