Program Example35;

{ Program to demonstrate the
  OpenDir,ReadDir, SeekDir and TellDir functions. }

Uses BaseUnix;

Var TheDir : PDir;
    ADirent : PDirent;
    Entry : Longint;

begin
  TheDir:=fpOpenDir('./.');
  Repeat
//    Entry:=fpTellDir(TheDir);
    ADirent:=fpReadDir (TheDir^);
    If ADirent<>Nil then
      With ADirent^ do
        begin
        Writeln ('Entry No : ',Entry);
        Writeln ('Inode    : ',d_fileno);
//        Writeln ('Offset   : ',d_off);
        Writeln ('Reclen   : ',d_reclen);
        Writeln ('Name     : ',pchar(@d_name[0]));
        end;
  Until ADirent=Nil;
  Repeat
    Write ('Entry No. you would like to see again (-1 to stop): ');
    ReadLn (Entry);
    If Entry<>-1 then
      begin
//      fpSeekDir (TheDir,Entry);               // not implemented for various platforms
      ADirent:=fpReadDir (TheDir^);
      If ADirent<>Nil then
        With ADirent^ do
          begin
          Writeln ('Entry No : ',Entry);
          Writeln ('Inode    : ',d_fileno);
//          Writeln ('Offset   : ',off);
          Writeln ('Reclen   : ',d_reclen);
          Writeln ('Name     : ',pchar(@d_name[0]));
          end;
    end;
  Until Entry=-1;
  fpCloseDir (TheDir^);
end.
