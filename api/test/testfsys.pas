{ test application for FileSys
  make sure a file called testfile.txt (here in FName) exists;
  it will be deleted ! }
program TestFS;

{$I platform.inc}

uses
  Common,
  Filesys;

const
  Passes = 12;
  FName  : string[30] = 'testfile.txt';
  DName  : string[30] = 'test';
  TestNames : array[0..Passes-1] of string[30] =
    ('CreateDir',
     'RenameDir',
     'ExpandName',
     'GetCurrentDir',
     'DeleteDir',

     'Check name',
     'Set time',
     'DateToString/TimeToString',
     'Set attribute',
     'FileAttrToString',
     'FileIntToString',
     'SplitName'
    );

var
  i           : byte;
  n, p, rn, e : TFileName;
  dt          : TDateTime;
  attr        : TFileAttr;
  fi          : TFileInt;

begin
  WriteLn ('TestFS - tests capabilities of unit FileSys');
  WriteLn ('---');
  i := 0;
  repeat
    { show topic }
    case i of
      0 : WriteLn ('<DIRECTORY>');
      5 : WriteLn ('<FILE>');
    end;
    { show name of action }
    Write (TestNames[i], ' ');
    { perform action }
    case i of
      0 :
        begin
          Write ('"', DName, '"');
          FileSys.CreateDir (DName);
        end;
      1 :
        begin
          Write (DName, '=> test2');
          RenameDir (DName, 'test2');
          GetErrorCode;
          RenameDir ('test2', DName);
        end;
      2 :
        begin
          Write ('"', ExpandName (DName),'"');
        end;
      3 :
        begin
          Write ('"', GetCurrentDir, '"');
        end;
      4 :
        begin
          Write ('"', DName, '"');
          FileSys.DeleteDir (DName);
        end;
      { FILE }
      5 :
        begin
          Write ('"', FName, '" : ');
          if (FileSys.CheckName (FName) = cnUnknown)
            then Write ('unknown')
            else Write ('known');
        end;
      6 :
        begin
          Write (' 22 Jul 1997 12:34:56');
          with dt do begin
            Day :=    22;
            Month :=  7;
            Year :=   1997;
            Hour :=   12;
            Minute := 34;
            Second := 56;
            CheckDateTime (dt);
          end;
          SetFTime (FName, dt);
        end;
      7 :
        begin
          Write (DateToString (dt), ' ', TimeToString (dt));
        end;
      8 :
        begin
          attr := 128;
          SetFAttr (FName, attr);
        end;
      9 :
        begin
          Write (FileAttrToString (attr));
        end;
     10 :
        begin
          fi := 12345678;
          Write (FileIntToString (fi));
        end;
     11 :
        begin
          {$ifdef OS_DOS}
          n := 'c:\sub1\sub3.ext.ext\name.gz';
          {$ELSE}
          n := '/sub1/sub3.ext.ext/name.gz';
          {$endif}
          SplitName (n, p, rn, e);
          Write ('"', n, '" => ',
                 'PATH = "', p, '", RAW NAME = "', rn,
                 '", EXTENSION = "', e, '"');
        end;

    end;
    if (Common.GetErrorCode = errOK)
      then WriteLn (' <NO ERROR>')
      else WriteLn (' <ERROR>');
    inc (i);
  until (i = Passes);
  WriteLn ('---');
end.
