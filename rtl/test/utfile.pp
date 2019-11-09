unit utfile;

{$mode objfpc}
{$h+}

interface

uses
  SysUtils;

Implementation

uses punit, utrtl;

Function File1 : String;

var
  l,l2: longint;
begin
  try
    try
      l:=filecreate('tfile2.dat');
      if (l<0) then
        FailExit('unable to create file');
      fileclose(l);
      l:=fileopen('tfile2.dat',fmopenread);
      if (filewrite(l,l,sizeof(l))>0) then
        FailExit('writing to read-only file succeeded');
      fileclose(l);
      deletefile('tfile2.dat');


      l:=filecreate('tfile2.dat');
      if (l<0) then
        FailExit('unable to create file (2)');
      fileclose(l);
      l:=fileopen('tfile2.dat',fmopenwrite);
      if (filewrite(l,l,sizeof(l))<>sizeof(l)) then
        FailExit('writing to write-only file failed');
      if (fileseek(l,0,fsFromBeginning)<>0) then
        FailExit('seeking write-only file failed');
      if (fileread(l,l2,sizeof(l))>=0) then
        FailExit('reading from write-only file succeeded');
      fileclose(l);

      l:=fileopen('tfile2.dat',fmopenread or fmShareDenyWrite);
      if (l<0) then
        FailExit('unable to open file in read-only mode and fmShareDenyWrite mode');
      l2:=fileopen('tfile2.dat',fmopenread or fmShareDenyWrite);
      if (l2 < 0) then
        FailExit('opening two files as read-only with fmShareDenyWrite failed');
      fileclose(l2);
      l2:=fileopen('tfile2.dat',fmopenread or fmShareExclusive);
      if (l2 >= 0) then
        begin
          fileclose(l2);
          FailExit('opening file first as read-only with fmShareDenyWrite, and then again as fmopenread with fmShareExclusive succeeded');
        end;
      fileclose(l);


      l:=fileopen('tfile2.dat',fmopenwrite or fmShareExclusive);
      if (l<0) then
        FailExit('unable to open file in write-only and fmShareExclusive mode');
      l2:=fileopen('tfile2.dat',fmopenwrite or fmShareExclusive);
      if (l2 >= 0) then
        begin
          fileclose(l2);
          FailExit('opening two files as write-only with fmShareExclusive succeeded');
        end;
      l2:=fileopen('tfile2.dat',fmopenwrite or fmShareDenyWrite);
      if (l2 >= 0) then
        begin
          fileclose(l2);
          FailExit('opening file first as write-only with fmShareExclusive, and then again as fmopenwrite with fmShareDenyWrite succeeded');
        end;
      fileclose(l);


      l:=fileopen('tfile2.dat',fmopenread or fmShareExclusive);
      if (l<0) then
        FailExit('unable to open file in read-only and fmShareExclusive mode');
      l2:=fileopen('tfile2.dat',fmopenread or fmShareExclusive);
      if (l2 >= 0) then
        begin
          fileclose(l2);
          FailExit('opening two files as read-only with fmShareExclusive succeeded');
        end;
      l2:=fileopen('tfile2.dat',fmopenread or fmShareDenyWrite);
      if (l2 >= 0) then
        begin
          fileclose(l2);
          FailExit('opening file first as read-only with fmShareExclusive, and then again as fmopenread with fmShareDenyWrite succeeded');
        end;
      fileclose(l);


      l:=fileopen('tfile2.dat',fmopenread);
      if (l<0) then
        FailExit('unable to open file in read-only mode (2)');
      l2:=fileopen('tfile2.dat',fmopenread);
      if (l2 >= 0) then
        begin
          fileclose(l2);
          FailExit('opening two files as read-only without sharing specified succeeded (should not, file is by default locked)');
        end;
      l2:=fileopen('tfile2.dat',fmopenread or fmShareDenyWrite);
      if (l2 >= 0) then
        begin
          fileclose(l2);
          FailExit('opening two files as read-only with fmShareDenyWrite succeeded (should not, file is by default locked)');
        end;
      fileclose(l);


      { should be same as no locking specified }
      l:=fileopen('tfile2.dat',fmopenread or fmShareCompat);
      if (l<0) then
        FailExit('unable to open file in read-only mode (3)');
      l2:=fileopen('tfile2.dat',fmopenread or fmShareCompat);
      if (l2 >= 0) then
        begin
          fileclose(l2);
          FailExit('opening two files as read-only with fmShareCompat succeeded (should be locked)');
        end;
      l2:=fileopen('tfile2.dat',fmopenread or fmShareDenyWrite);
      if (l2 >= 0) then
        begin
          fileclose(l2);
          FailExit('opening file first as read-only fmShareCompat (should not have any effect), and then again as fmopenread with fmShareDenyWrite succeeded');
        end;
      fileclose(l);


      l:=fileopen('tfile2.dat',fmopenread or fmShareDenyNone);
      if (l<0) then
        FailExit('unable to open file in read-only mode and fmShareDenyNone mode');
      l2:=fileopen('tfile2.dat',fmopenread or fmShareDenyNone);
      if (l2 < 0) then
        FailExit('opening two files as read-only with fmShareDenyNone failed');
      fileclose(l2);
      l2:=fileopen('tfile2.dat',fmopenread or fmShareDenyWrite);
      if (l2 < 0) then
        FailExit('opening two files as read-only with fmShareDenyNone and then fmShareDenyWrite failed');
      fileclose(l2);
{ on Windows, fmShareExclusive checks whether the file is already open in any way by the current
  or another process. On Unix, that is not the case, and we also cannot check against a
  fmShareDenyNone mode
}
{$ifndef unix}
      l2:=fileopen('tfile2.dat',fmopenread or fmShareExclusive);
      if (l2 >= 0) then
        begin
          fileclose(l2);
          FailExit('opening two files as read-only with fmShareDenyNone and then fmShareExclusive succeeded');
        end;
{$endif}
      fileclose(l);

      l:=fileopen('tfile2.dat',fmopenread or fmShareDenyWrite);
      if (l<0) then
        FailExit('unable to open file in read-only mode and fmShareDenyWrite mode (2)');
      l2:=fileopen('tfile2.dat',fmopenread or fmShareDenyNone);
      if (l2 < 0) then
        FailExit('opening files as read-only with fmShareDenyWrite and then fmShareDenyNone failed');
      fileclose(l2);
      fileclose(l);


      l:=fileopen('tfile2.dat',fmopenwrite or fmShareDenyNone);
      if (l<0) then
        FailExit('unable to open file in write-only mode and fmShareDenyNone mode');
      l2:=fileopen('tfile2.dat',fmopenread or fmShareDenyNone);
      if (l2 < 0) then
        FailExit('opening two files as read/write-only with fmShareDenyNone failed');
      fileclose(l2);

    except
      on e: exception do
        begin
          writeln(e.message);
          exitcode:=1;
        end;
    end;
  finally
    if (l>=0) then
      fileclose(l);
    deletefile('tfile2.dat');
  end;
end;

Function file2 : string;

VAR
  dateTime: TDateTime;
  f : file;

BEGIN
  if FileExists('datetest.dat') then
    begin
    Assign(f,'datetest.dat');
    Erase(f);
    end;
  if FileExists('datetest.dat') then
    Exit('Error at 1000');
  FileClose(FileCreate('datetest.dat'));
  if not(FileExists('datetest.dat')) then
    Exit('Error at 1001');
  dateTime := IncMonth(Now, -1);
  if FileSetDate('datetest.dat', DateTimeToFileDate(dateTime))<>0 then
    Exit('Error at 1002');
  if FileExists('datetest.dat') then
    begin
    Assign(f,'datetest.dat');
    Erase(f);
    end;
end;

begin
  SysutilsTest('tfile1',@file1);
  SysutilsTest('tfile2',@file2);
end.
