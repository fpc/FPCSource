
{ checks if the correct RTE's are generated for invalid io operations }

{$i-}

const
 TMP_DIRECTORY = 'temp2';
 has_fails : boolean = false;

procedure test(value, required: longint);
begin
  if value <> required then
    begin
      writeln('Got ',value,' instead of ',required);
      has_fails:=true;
      {halt(1);}
    end;
end;

procedure test_read_text;
var
  f: text;
  s: string;
begin
  { to avoid influence of previous runs/procedures }
  fillchar(f,sizeof(f),0);
  write('Reading from not opened text file...');
  read(f,s);
  test(ioresult,103);
  readln(f);
  test(ioresult,103);
  writeln(' Passed!');

  write('Seekeoln from not opened text file...');
  seekeoln(f);
  test(ioresult,103);
  writeln(' Passed!');

  write('Seekeof from not opened text file...');
  seekeof(f);
  test(ioresult,103);
  writeln(' Passed!');

  assign(f,'inoutrte.$$$');
  rewrite(f);
  test(ioresult,0);

  write('Reading from write-only (rewritten) text file...');
  read(f,s);
  test(ioresult,104);
  readln(f);
  test(ioresult,104);
  writeln(' Passed!');

  write('Seekeoln from write-only (rewritten) text file...');
  seekeoln(f);
  test(ioresult,104);
  writeln(' Passed!');

  write('Seekeof from write-only (rewritten) text file...');
  seekeof(f);
  test(ioresult,104);
  writeln(' Passed!');

  close(f);
  test(ioresult,0);
  append(f);
  test(ioresult,0);

  write('Reading from write-only (appended) text file...');
  read(f,s);
  test(ioresult,104);
  readln(f);
  test(ioresult,104);
  writeln(' Passed!');

  write('Seekeoln from write-only (appended) text file...');
  seekeoln(f);
  test(ioresult,104);
  writeln(' Passed!');

  write('Seekeof from write-only (appended) text file...');
  seekeof(f);
  test(ioresult,104);
  writeln(' Passed!');

  close(f);
  test(ioresult,0);
  erase(f);
  test(ioresult,0);
end;

procedure test_read_typed;
var
  f: file of byte;
  s: byte;
begin
  { to avoid influence of previous runs/procedures }
  fillchar(f,sizeof(f),0);

  write('Reading from not opened typed file...');
  read(f,s);
  test(ioresult,103);
  writeln(' Passed!');

  { with filemode 2, the file is read-write }
  filemode := 1;
  assign(f,'inoutrte.$$$');
  rewrite(f);
  test(ioresult, 0);
  write(f,s);
  test(ioresult, 0);
  close(f);
  test(ioresult, 0);
  reset(f);
  test(ioresult, 0);
  write('Reading from write-only typed file...');
  read(f,s);
  test(ioresult,104);
  writeln(' Passed!');

  filemode := 2;
  close(f);
  test(ioresult, 0);
  erase(f);
  test(ioresult, 0);
end;

procedure test_read_untyped;
var
  f: file;
  r: longint;
  s: byte;
begin
  { to avoid influence of previous runs/procedures }
  fillchar(f,sizeof(f),0);

  write('Reading from not opened untyped file...');
  blockread(f,s,1,r);
  test(ioresult,103);
  writeln(' Passed!');

  { with filemode 2, the file is read-write }
  filemode := 1;
  assign(f,'inoutrte.$$$');
  rewrite(f);
  test(ioresult, 0);
  blockwrite(f,s,1);
  test(ioresult, 0);
  close(f);
  test(ioresult, 0);
  reset(f);
  test(ioresult, 0);
  write('Reading from write-only utyped file...');
  blockread(f,s,1,r);
  test(ioresult,104);
  writeln(' Passed!');

  filemode := 2;
  close(f);
  test(ioresult, 0);
  erase(f);
  test(ioresult, 0);
end;


procedure test_write_text;
var f: text;
    s: string;
begin
  { to avoid influence of previous runs/procedures }
  fillchar(f,sizeof(f),0);

  write('Writing to not opened text file...');
  write(f,s);
  test(ioresult,103);
  writeln(f);
  test(ioresult,103);
  writeln(' Passed!');

  assign(f,'inoutrte.$$$');
  rewrite(f);
  close(f);
  test(ioresult,0);
  reset(f);
  test(ioresult,0);

  write('Writing to read-only text file...');
  write(f,s);
  test(ioresult,105);
  writeln(f);
  test(ioresult,105);
  Writeln(' Passed!');

  close(f);
  test(ioresult,0);
  erase(f);
  test(ioresult,0);
end;

procedure test_write_typed;
var f: file of byte;
    s: byte;
begin
  { to avoid influence of previous runs/procedures }
  fillchar(f,sizeof(f),0);

  write('Writing to not opened typed file...');
  write(f,s);
  test(ioresult,103);
  writeln(' Passed!');

  assign(f,'inoutrte.$$$');
  rewrite(f);
  close(f);
  test(ioresult,0);
  filemode := 0;
  reset(f);
  test(ioresult,0);

  write('Writing to read-only typed file...');
  write(f,s);
  test(ioresult,105);
  Writeln(' Passed!');

  filemode := 2;
  close(f);
  test(ioresult,0);
  erase(f);
  test(ioresult,0);
end;

procedure test_write_untyped;
var f: file;
    r: longint;
    s: byte;
begin
  { to avoid influence of previous runs/procedures }
  fillchar(f,sizeof(f),0);

  write('Writing to not opened untyped file...');
  blockwrite(f,s,1,r);
  test(ioresult,103);
  writeln(' Passed!');

  assign(f,'inoutrte.$$$');
  rewrite(f);
  close(f);
  test(ioresult,0);
  filemode := 0;
  reset(f);
  test(ioresult,0);

  write('Writing to read-only untyped file...');
  blockwrite(f,s,1,r);
  test(ioresult,105);
  Writeln(' Passed!');

  filemode := 2;
  close(f);
  test(ioresult,0);
  erase(f);
  test(ioresult,0);
end;


procedure test_close_text;
var f: text;
begin
  { to avoid influence of previous runs/procedures }
  fillchar(f,sizeof(f),0);

  write('Testing closing of not opened text file...');
  close(f);
  test(ioresult,103);
  writeln(' Passed!');
end;

procedure test_close_typed;
var f: file of byte;
begin
  { to avoid influence of previous runs/procedures }
  fillchar(f,sizeof(f),0);

  write('Testing closing of not opened typed file...');
  close(f);
  test(ioresult,103);
  writeln(' Passed!');
end;

procedure test_close_untyped;
var f: file;
begin
  { to avoid influence of previous runs/procedures }
  fillchar(f,sizeof(f),0);

  write('Testing closing of not opened untyped file...');
  close(f);
  test(ioresult,103);
  writeln(' Passed!');
end;



procedure test_fileroutines;
var
 F: File;
 L: longint;
begin
 { get the file position of a non-existent file }
 write('Testing Filepos on non initialized file...');
 l:=FilePos(F);
 test(IOresult,103);
 writeln(' Passed!');
 write('Testing Filesize on non initialized file...');
 l:=FileSize(F);
 test(IOresult,103);
 writeln(' Passed!');
end;

procedure test_directory;
var
 F: File;
{ test directory I/O }
begin
  { test on non-existant directory }
  write('Testing change directory on non-existent file...');
  ChDir('notexist');
  test(IOResult,3);
  { test on a file }
  ChDir('testdir.pas');
  test(IOResult,3);
  Writeln(' Passed!');
  { test on non-existant directory }
{$ifdef go32v2}
  ChDir('Y:\test.dir');
  test(IOResult,15);
{$endif}
  { make a stub directory for testing purposes }
  Mkdir(TMP_DIRECTORY);
  test(IOResult,0);
  { try to recreate the directory .... }
  write('Testing make directory on already existent dir...');
  MkDir(TMP_DIRECTORY);
  test(IOResult,5);
  Writeln(' Passed!');

  { try to erase the directory, using file access }
  write('Testing erase of directory...');
  Assign(F,TMP_DIRECTORY);
  Erase(F);
  test(IOResult,2);
  Writeln(' Passed!');
  { now really remove the directory }
  RmDir(TMP_DIRECTORY);
  test(IOResult,0);
  { remove non-existant directory }
  write('Testing remove directory of non-existent file...');
  RmDir('testdir.exe');
  { TP here returns 5 , not 2 }
  test(IOResult,2);
  Writeln(' Passed!');
  { erase non-existant file }
  write('Testing erase of non-existent file...');
  Assign(F,'notexist.txt');
  Erase(F);
  test(IOResult,2);
  WriteLn(' Passed!');
  { try to erase the current directory }
  write('Trying to erase current directory...');
  RmDir('.');
{$ifndef macos}
  test(IOResult, 16);
{$else}
  test(IOResult, 5); {..since the system is not aware of current dir}
{$endif}
  WriteLn(' Passed!');
  { try to erase the previous directory }
  write('Trying to erase parent directory...');
  RmDir('..');
  test(IOResult, 5);
  WriteLn(' Passed!');
end;


begin
{$ifdef macos}
  pathTranslation:= true;
{$endif}
  test_read_text;
  test_read_typed;
  test_read_untyped;
  test_write_text;
  test_write_typed;
  test_write_untyped;
  test_close_text;
  test_close_typed;
  test_close_untyped;
  test_directory;
  test_fileroutines;
  if has_fails then
    halt(1);
end.
