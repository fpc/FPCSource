{ Make DB directory and all subdirectories }
{ FULLDB will be defined if included from db directory }
{ FULLFCL will be defined if included from main FCL directory }

{ DBase, only for I386}
Targets.ResetDefaults;  
Targets.DefaultCPU:=[i386];
Targets.DefaultOS:=[linux,win32,freebsd];
Targets.DefaultDir:='db/dbase';
T:=Targets.AddUnit('dbf');
T:=Targets.Addexampleunit('testdbf');
If (Defaults.CPU=i386) and (Defaults.OS in [linux,win32,freebsd]) then
  begin
  { Install files. }
  InstallFiles.add('dbf_common.o');
  InstallFiles.add('dbf_common.ppu');
  InstallFiles.add('dbf_cursor.o');
  InstallFiles.add('dbf_cursor.ppu');
  InstallFiles.add('dbf_dbffile.o');
  InstallFiles.add('dbf_dbffile.ppu');
  InstallFiles.add('dbf_fields.o');
  InstallFiles.add('dbf_fields.ppu');
  InstallFiles.add('dbf_idxcur.o');
  InstallFiles.add('dbf_idxcur.ppu');
  InstallFiles.add('dbf_idxfile.o');
  InstallFiles.add('dbf_idxfile.ppu');
  InstallFiles.add('dbf_lang.o');
  InstallFiles.add('dbf_lang.ppu');
  InstallFiles.add('dbf_memo.o');
  InstallFiles.add('dbf_memo.ppu');
  InstallFiles.add('dbf_parser.o');
  InstallFiles.add('dbf_parser.ppu');
  InstallFiles.add('dbf_pgfile.o');
  InstallFiles.add('dbf_pgfile.ppu');
  InstallFiles.add('dbf_prscore.o');
  InstallFiles.add('dbf_prscore.ppu');
  InstallFiles.add('dbf_prsdef.o');
  InstallFiles.add('dbf_prsdef.ppu');
  InstallFiles.add('dbf_prssupp.o');
  InstallFiles.add('dbf_prssupp.ppu');
  InstallFiles.add('dbf_str.o');
  InstallFiles.add('dbf_str.ppu');
  if (Defaults.OS<>win32) then
    begin
    InstallFiles.add('dbf_wtil.ppu');
    InstallFiles.add('dbf_wtil.o');
    end;
  { Clean files }
  CleanFiles.add('dbf_common.o');
  CleanFiles.add('dbf_common.ppu');
  CleanFiles.add('dbf_cursor.o');
  CleanFiles.add('dbf_cursor.ppu');
  CleanFiles.add('dbf_dbffile.o');
  CleanFiles.add('dbf_dbffile.ppu');
  CleanFiles.add('dbf_fields.o');
  CleanFiles.add('dbf_fields.ppu');
  CleanFiles.add('dbf_idxcur.o');
  CleanFiles.add('dbf_idxcur.ppu');
  CleanFiles.add('dbf_idxfile.o');
  CleanFiles.add('dbf_idxfile.ppu');
  CleanFiles.add('dbf_lang.o');
  CleanFiles.add('dbf_lang.ppu');
  CleanFiles.add('dbf_memo.o');
  CleanFiles.add('dbf_memo.ppu');
  CleanFiles.add('dbf_parser.o');
  CleanFiles.add('dbf_parser.ppu');
  CleanFiles.add('dbf_pgfile.o');
  CleanFiles.add('dbf_pgfile.ppu');
  CleanFiles.add('dbf_prscore.o');
  CleanFiles.add('dbf_prscore.ppu');
  CleanFiles.add('dbf_prsdef.o');
  CleanFiles.add('dbf_prsdef.ppu');
  CleanFiles.add('dbf_prssupp.o');
  CleanFiles.add('dbf_prssupp.ppu');
  CleanFiles.add('dbf_str.o');
  CleanFiles.add('dbf_str.ppu');
  if (Defaults.OS<>win32) then
    begin
    Cleanfiles.add('dbf_wtil.ppu');
    CleanFiles.add('dbf_wtil.o');
    end;
  end;
