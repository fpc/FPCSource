{ Simple libtar test - creates a tar archive with a few entries }
program libtar_simple_test;

{$mode objfpc}{$H+}

uses
  SysUtils, libtar;

const
  TAR_FILENAME = 'tlibtar1.tar';
  CONTENT_1 = 'Hello, World!';
  CONTENT_2 = 'Some data in a subdirectory';
  CONTENT_3 = 'hello.txt';
  CONTENT_4 = 'hello.txt';

  TotalEntries = 4;

var
  TW: TTarWriter;
  TA: TTarArchive;
  DirRec: TTarDirRec;
  Content: RawByteString;
  EntryCount: Integer;
  Errors: Integer;
begin
  Errors := 0;

  WriteLn('Creating tar archive: ', TAR_FILENAME);

  try
    TW := TTarWriter.Create(TAR_FILENAME);
    try
        { Set default permissions }
        TW.Permissions := [tpReadByOwner, tpWriteByOwner, tpReadByGroup, tpReadByOther];

        { Add a simple text file }
        WriteLn('  Adding: hello.txt');
        TW.AddString(CONTENT_1, 'hello.txt', Now);

        { Add a file in a subdirectory }
        WriteLn('  Adding: subdir/data.txt');
        TW.AddString(CONTENT_2, 'subdir/data.txt', Now);

        { Add a directory entry }
        WriteLn('  Adding: emptydir/');
        TW.AddDir(CONTENT_4, Now);

        { Add a symbolic link }
        WriteLn('  Adding: link.txt -> hello.txt');
        TW.AddSymbolicLink('link.txt', CONTENT_3, Now);

        TW.Finalize;
    finally
        TW.Free;
    end;

    WriteLn('Done. Archive created successfully.');
    WriteLn;

    WriteLn('=== Reading and verifying archive ===');
    WriteLn;

    TA := TTarArchive.Create(TAR_FILENAME);
    try
        EntryCount := 0;

        while TA.FindNext(DirRec) do
        begin
        Inc(EntryCount);
        WriteLn('Entry ', EntryCount, ':');
        WriteLn('  Name: ', DirRec.Name);
        WriteLn('  Size: ', DirRec.Size);
        WriteLn('  Type: ', FILETYPE_NAME[DirRec.FileType]);

        if DirRec.FileType = ftSymbolicLink then
            begin
            if DirRec.LinkName <> CONTENT_3 then
                begin
                Inc(Errors);
                writeln('  Wrong link value');
                end;
            WriteLn('  Link: -> ', DirRec.LinkName);
            end;

        if DirRec.FileType = ftDirectory then
            begin
            if DirRec.Name <> CONTENT_4 then
                begin
                Inc(Errors);
                writeln('  Wrong directory Name');
                end;
            end;

        if DirRec.ChecksumOK then
            WriteLn('  Checksum: OK')
        else
        begin
            WriteLn('  Checksum: FAILED');
            Inc(Errors);
        end;

        { Verify content for regular files }
        if DirRec.FileType = ftNormal then
        begin
            Content := TA.ReadFile;

            if DirRec.Name = 'hello.txt' then
            begin
            if Content = CONTENT_1 then
                WriteLn('  Content: verified OK')
            else
            begin
                WriteLn('  Content: MISMATCH');
                Inc(Errors);
            end;
            end
            else if DirRec.Name = 'subdir/data.txt' then
            begin
            if Content = CONTENT_2 then
                WriteLn('  Content: verified OK')
            else
            begin
                WriteLn('  Content: MISMATCH');
                Inc(Errors);
            end;
            end;
        end;

        WriteLn;
        end;
    finally
        TA.Free;
    end;

    { === PART 3: Summary === }
    WriteLn('=== Summary ===');
    WriteLn;
    WriteLn('Total entries: ', EntryCount);

    if EntryCount <> TotalEntries then
    begin
        WriteLn('ERROR: Expected ',TotalEntries,' entries!');
        Inc(Errors);
    end;

    if Errors = 0 then
        WriteLn('All tests PASSED.')
    else
        WriteLn('FAILED with ', Errors, ' error(s).');

    WriteLn;

  finally
    { Cleanup }
    DeleteFile(TAR_FILENAME);
  end;

  Halt(Errors);
end.
