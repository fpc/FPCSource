{ Program to test OS-specific features of the system unit }
{ routines to test:                                       }
{   do_open()                                             }
{   do_read()                                             }
{   do_write()                                            }
{   do_close()                                            }
{   do_filesize()                                         }
{   do_seek()                                             }
{   do_truncate()                                         }

{ This routine overwrites/creates a filename called test.tmp }
{ fills it up with values, checks its file size, reads the   }
{ data back in,                                              }

Program tio;
{$I-}

{$IFDEF TP}
type
  shortstring = string;
{$ENDIF}


var
 F: File;


procedure test(value, required: longint);
begin
  if value <> required then
    begin
      writeln('Got ',value,' instead of ',required);
      halt(1);
    end;
end;


const
  FILE_NAME = 'test.tmp';
  FILE_NAME2 = 'test1.tmp';
  DATA_SIZE = 17;

  MODE_RESET = 0;
  MODE_REWRITE = 1;

  DATA: array[1..DATA_SIZE] of byte =
  ($01,$02,$03,$04,$05,$06,$07,$08,
   $09,$A,$B,$C,$D,$E,$F,$10,
   $11
  );


procedure test_do_open(name : shortstring; mode: word);
begin
  Write('opening file...');
  Assign(F,name);
  test(IOResult, 0);
  if mode = MODE_REWRITE then
    Rewrite(F,1)
  else
    Reset(F,1);
  test(IOResult, 0);
  WriteLn('Passed!');
end;

procedure test_do_write(var buf; BytesToWrite : longint);
var
  BytesWritten : word;
begin
  Write('writing to file...');
  BlockWrite(F,buf,BytesToWrite,BytesWritten);
  test(IOResult, 0);
  if BytesWritten<>DATA_SIZE then
    RunError(255);
  Writeln('Passed!');
end;

procedure test_do_filesize(size : longint);
begin
  Write('getting filesize...');
  { verifying if correct filesize }
  test(FileSize(F),size);
  { verify if IOError }
  test(IOResult, 0);
  WriteLn('Passed!');
end;

procedure test_do_seek(_pos : longint);
begin
  { Seek to beginning of file }
  Write('seek to beginning of file...');
  Seek(F, _pos);
  test(IOResult, 0);
  WriteLn('Passed!');
end;


procedure test_do_read(var buf; BytesToRead : word);
var
 BytesRead : word;
begin
  Write('reading from file...');
  BlockRead(F,buf,BytesToRead,BytesRead);
  test(BytesToRead, BytesRead);
  test(IOResult, 0);
  WriteLn('Passed!');
end;

procedure test_filepos(_pos : longint);
var
 BytesRead : word;
begin
  write('verifying file position...');
  test(FilePos(F),_pos);
  test(IOResult, 0);
  WriteLn('Passed!');
end;

procedure test_do_close;
begin
  Write('closing file...');
  Close(F);
  test(IOResult, 0);
  WriteLn('Passed!');
end;

procedure test_already_closed_close;
begin
  Write('closing already closed file...(IOResult=103 expected) ');
  Close(F);
  test(IOResult, 103);
  WriteLn('Passed!');
end;

procedure test_not_yet_open_close(name : string);
begin
  Write('closing assigned only file...(IOResult=103 expected) ');
  Assign(F,name);
  test(IOResult,0);
  Close(F);
  test(IOResult, 103);
  WriteLn('Passed!');
end;


procedure test_rename(oldname, newname : shortstring);
begin
  Assign(F,oldname);
  Write('renaming file...');
  ReName(F,newname);
  test(IOResult, 0);
  WriteLn('Passed!');
end;

procedure test_erase(name : shortstring);
begin
  Assign(F,name);
  Write('erasing file...');
  Erase(F);
  test(IOResult, 0);
  WriteLn('Passed!');
end;

var
 I: Integer;
 readData : array[1..DATA_SIZE] of byte;
Begin
  {------------------------ create and play with a new file --------------------------}
  FillChar(readData,DATA_SIZE,0);

  test_not_yet_open_close(FILE_NAME);
  test_do_open(FILE_NAME, MODE_REWRITE);
  test_do_write(DATA, DATA_SIZE);
  test_do_filesize(DATA_SIZE);
  test_do_seek(0);
  test_do_read(readData, DATA_SIZE);


  for i:=1 to DATA_SIZE do
   Begin
       test(readData[i], data[i]);
   end;

  test_do_seek(5);

  test_filepos(5);
(*
  test_do_truncate()
  WriteLn('truncating file...');
  Truncate(F);
  WriteLn(FileSize(F));
  if FileSize(F) <> 5 then
   RunError(255);
*)
  test_do_close;
  test_already_closed_close;
  {------------------------ create and play with an old file --------------------------}
  FillChar(readData,DATA_SIZE,0);
  test_do_open(FILE_NAME2, MODE_REWRITE);
  test_do_write(DATA, DATA_SIZE);
  test_do_close;

  FillChar(readData,DATA_SIZE,0);
  test_do_open(FILE_NAME2, MODE_RESET);
  test_do_write(DATA, DATA_SIZE);

  test_do_filesize(DATA_SIZE);
  test_do_seek(0);
  test_do_read(readData, DATA_SIZE);


  for i:=1 to DATA_SIZE do
   Begin
       test(readData[i], data[i]);
   end;

  test_do_close;

  test_rename(FILE_NAME2, 'test3.tmp');
  test_erase(FILE_NAME);
  test_erase('test3.tmp');
end.
