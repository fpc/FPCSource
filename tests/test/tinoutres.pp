{ checks if the correct RTE's are generated for invalid io operations }

{$i-}

procedure test(value, required: longint); 
begin
  if value <> required then
    begin
      writeln('Got ',value,' instead of ',required);
      halt(1);
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

begin
  test_read_text;
  test_read_typed;
  test_read_untyped;
  test_write_text;
  test_write_typed;
  test_write_untyped;
  test_close_text;
  test_close_typed;
  test_close_untyped;
end.
