{ Program to test OS-specific features of the system unit }
{ routines to test:                                       }
{   do_open()                                             }
{   do_read()                                             }
{   do_write()                                            }
{   do_close()                                            }
{   do_filesize()                                         }
{   do_seek()                                             }
{   do_truncate()                                         }

{ This routine overwrites/creates a filename called test.dat }
{ fills it up with values, checks its file size, reads the   }
{ data back in,                                              }

Program tio;

const
  FILE_NAME = 'test.dat';
  FILE_NAME2 = 'test1.dat';
  DATA_SIZE = 17;

  DATA: array[1..DATA_SIZE] of byte =
  ($01,$02,$03,$04,$05,$06,$07,$08,
   $09,$A,$B,$C,$D,$E,$F,$10,
   $11
  );
 
  

{$I+}
var
 F: File;
 I: Integer;
 b: byte;
 readData : array[1..DATA_SIZE] of byte;
 BytesRead, BytesWritten : word;
Begin
  {------------------------ create and play with a new file --------------------------}
  BytesWritten := 0;
  FillChar(readData,DATA_SIZE,0);
  WriteLn('opening file...');
  Assign(F,FILE_NAME);
  Rewrite(F,1);
  WriteLn('writing to file...');
  BlockWrite(F,DATA,DATA_SIZE,BytesWritten);
  if BytesWritten<>DATA_SIZE then
    RunError(255);
  WriteLn('getting filesize...');
  if FileSize(F) <> DATA_SIZE then
     RunError(255);
  { Seek to beginning of file }
  WriteLn('seek to beginning of file...');
  Seek(F, 0);
  WriteLn('reading from file...');
  BlockRead(F,readData,DATA_SIZE,BytesRead);
  for i:=1 to DATA_SIZE do
   Begin
     if readData[i] <> data[i] then
       RunError(255);
   end;
  WriteLn('seeking in file...');
  Seek(f,5);
  WriteLn('getting file position...');
  if filepos(f) <> 5 then
    RunError(255);
  WriteLn('truncating file...');
{
  Truncate(F);
  WriteLn(FileSize(F));
  if FileSize(F) <> 5 then
   RunError(255);   }
  WriteLn('closing file...');
  Close(F);
  {------------------------ create and play with an old file --------------------------}
  BytesWritten := 0;
  FillChar(readData,DATA_SIZE,0);
  WriteLn('opening file...');
  Assign(F,FILE_NAME2);
  Rewrite(F,1);
  WriteLn('writing to file...');
  BlockWrite(F,DATA,DATA_SIZE,BytesWritten);
  if BytesWritten<>DATA_SIZE then
    RunError(255);
  WriteLn('closing file...');
  Close(F);
  BytesWritten := 0;
  FillChar(readData,DATA_SIZE,0);
  WriteLn('opening already created file...');
  Assign(F,FILE_NAME2);
  Reset(F,1);
  WriteLn('writing to file...');
  BlockWrite(F,DATA,DATA_SIZE,BytesWritten);
  if BytesWritten<>DATA_SIZE then
    RunError(255);
  WriteLn('closing file...');
  Close(F);
  Assign(F,FILE_NAME2);
  WriteLn('renaming file...');
  ReName(F,'test3.dat');
  WriteLn('erasing file....');
  Erase(F);
end.

{
 $Log$
 Revision 1.1  2001-07-14 04:25:00  carl
 system unit testing : basic I/O

}
