{$ifdef fpc}
{$mode objfpc}
{$h+}
{$endif}

uses
  SysUtils;

{$ifndef fpc}
const
  fmsharecompat = cardinal(0);
  fsFromBeginning = cardinal(0);
{$endif}

var
  l,l2: longint;
begin
  try
    try
      l:=filecreate('tfile2.dat');
      if (l<0) then
        raise exception.create('unable to create file');
      fileclose(l);
      l:=fileopen('tfile2.dat',fmopenread);
      if (filewrite(l,l,sizeof(l))>0) then
        raise exception.create('writing to read-only file succeeded');
      fileclose(l);
      deletefile('tfile2.dat');


      l:=filecreate('tfile2.dat');
      if (l<0) then
        raise exception.create('unable to create file (2)');
      fileclose(l);
      l:=fileopen('tfile2.dat',fmopenwrite);
      if (filewrite(l,l,sizeof(l))<>sizeof(l)) then
        raise exception.create('writing to write-only file failed');
      if (fileseek(l,0,fsFromBeginning)<>0) then
        raise exception.create('seeking write-only file failed');
      if (fileread(l,l2,sizeof(l))>=0) then
        raise exception.create('reading from write-only file succeeded');
      fileclose(l);

      l:=fileopen('tfile2.dat',fmopenread or fmShareDenyWrite);
      if (l<0) then
        raise exception.create('unable to open file in read-only mode and fmShareDenyWrite mode');
      l2:=fileopen('tfile2.dat',fmopenread or fmShareDenyWrite);
      if (l2 < 0) then
        raise exception.create('opening two files as read-only with fmShareDenyWrite failed');
      fileclose(l2);
      l2:=fileopen('tfile2.dat',fmopenread or fmShareExclusive);
      if (l2 >= 0) then
        begin
          fileclose(l2);
          raise exception.create('opening file first as read-only with fmShareDenyWrite, and then again as fmopenread with fmShareExclusive succeeded');
        end;
      fileclose(l);


      l:=fileopen('tfile2.dat',fmopenwrite or fmShareExclusive);
      if (l<0) then
        raise exception.create('unable to open file in write-only and fmShareExclusive mode');
      l2:=fileopen('tfile2.dat',fmopenwrite or fmShareExclusive);
      if (l2 >= 0) then
        begin
          fileclose(l2);
          raise exception.create('opening two files as write-only with fmShareExclusive succeeded');
        end;
      l2:=fileopen('tfile2.dat',fmopenwrite or fmShareDenyWrite);
      if (l2 >= 0) then
        begin
          fileclose(l2);
          raise exception.create('opening file first as write-only with fmShareExclusive, and then again as fmopenwrite with fmShareDenyWrite succeeded');
        end;
      fileclose(l);


      l:=fileopen('tfile2.dat',fmopenread or fmShareExclusive);
      if (l<0) then
        raise exception.create('unable to open file in read-only and fmShareExclusive mode');
      l2:=fileopen('tfile2.dat',fmopenread or fmShareExclusive);
      if (l2 >= 0) then
        begin
          fileclose(l2);
          raise exception.create('opening two files as read-only with fmShareExclusive succeeded');
        end;
      l2:=fileopen('tfile2.dat',fmopenread or fmShareDenyWrite);
      if (l2 >= 0) then
        begin
          fileclose(l2);
          raise exception.create('opening file first as read-only with fmShareExclusive, and then again as fmopenread with fmShareDenyWrite succeeded');
        end;
      fileclose(l);


      l:=fileopen('tfile2.dat',fmopenread);
      if (l<0) then
        raise exception.create('unable to open file in read-only mode (2)');
      l2:=fileopen('tfile2.dat',fmopenread);
      if (l2 >= 0) then
        begin
          raise exception.create('opening two files as read-only without sharing specified succeeded (should not, file is by default locked)');
        end;
      fileclose(l2);
      l2:=fileopen('tfile2.dat',fmopenread or fmShareDenyWrite);
      if (l2 >= 0) then
        begin
          raise exception.create('opening two files as read-only with fmShareDenyWrite succeeded (should not, file is by default locked)');
        end;
      fileclose(l);
      fileclose(l2);


      { should be same as no locking specified }
      l:=fileopen('tfile2.dat',fmopenread or fmShareCompat);
      if (l<0) then
        raise exception.create('unable to open file in read-only mode (3)');
      l2:=fileopen('tfile2.dat',fmopenread or fmShareCompat);
      if (l2 >= 0) then
        raise exception.create('opening two files as read-only with fmShareCompat succeeded (should be locked)');
      fileclose(l2);
      l2:=fileopen('tfile2.dat',fmopenread or fmShareDenyWrite);
      if (l2 >= 0) then
        raise exception.create('opening file first as read-only fmShareCompat (should not have any effect), and then again as fmopenread with fmShareDenyWrite succeeded');
      fileclose(l2);
      fileclose(l);


      l:=fileopen('tfile2.dat',fmopenread or fmShareDenyNone);
      if (l<0) then
        raise exception.create('unable to open file in read-only mode and fmShareDenyNone mode');
      l2:=fileopen('tfile2.dat',fmopenread or fmShareDenyNone);
      if (l2 < 0) then
        raise exception.create('opening two files as read-only with fmShareDenyNone failed');
      fileclose(l2);
      { unix-specific that this fails? }
      l2:=fileopen('tfile2.dat',fmopenread or fmShareDenyWrite);
      if (l2 < 0) then
        raise exception.create('opening two files as read-only with fmShareDenyNone and then fmShareDenyWrite failed');
      fileclose(l2);
      { unix-specific that this fails? }
      l2:=fileopen('tfile2.dat',fmopenread or fmShareExclusive);
      if (l2 >= 0) then
        raise exception.create('opening two files as read-only with fmShareDenyNone and then fmShareExclusive succeeded');
      fileclose(l2);
      fileclose(l);

      l:=fileopen('tfile2.dat',fmopenread or fmShareDenyWrite);
      if (l<0) then
        raise exception.create('unable to open file in read-only mode and fmShareDenyWrite mode (2)');
      { unix-specific that this fails? }
      l2:=fileopen('tfile2.dat',fmopenread or fmShareDenyNone);
      if (l2 < 0) then
        raise exception.create('opening files as read-only with fmShareDenyWrite and then fmShareDenyNone failed');
      fileclose(l2);
      fileclose(l);


      l:=fileopen('tfile2.dat',fmopenwrite or fmShareDenyNone);
      if (l<0) then
        raise exception.create('unable to open file in write-only mode and fmShareDenyNone mode');
      l2:=fileopen('tfile2.dat',fmopenread or fmShareDenyNone);
      if (l2 < 0) then
        raise exception.create('opening two files as read/write-only with fmShareDenyNone failed');
      fileclose(l2);

    except
      on e: exception do
        begin
          writeln(e.message);
          exitcode:=1;
        end;
    end;
  finally
    fileclose(l);
    deletefile('tfile2.dat');
  end;
end.
