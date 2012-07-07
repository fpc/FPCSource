{ Concatenates a number of text files. This code is in the public domain. }

program concat;

uses
  SysUtils, Classes;

var
  Dst: TextFile;
  FileList: TStringList;
  IgnoreNonExisting: boolean;


procedure usage;
  begin
    Writeln('Usage: concat [-i] <srcfile1> [<srcfile2> ..] <dstfile>');
    Writeln;
    Writeln('Options:');
    Writeln('  -i      Ignore non-existent files');
    Writeln;
    halt(1);
  end;


procedure DoConcat;
  var
    Src: TextFile;
    I: Longint;
    Line: Ansistring;
    OldFilemode: byte;
  begin
    OldFilemode:=FileMode;
    Filemode:=0;
    for I:=0 to FileList.Count-1 do
      begin
        Assign(Src,FileList[i]);
       {$i-}
        Reset(Src);
        while ioresult<>0 do
          begin
            { wait for lingering locks to disappear }
            Sleep(200);
            Reset(Src);
          end;
       {$i+}

        while not Eof(Src) do
          begin
            ReadLn(Src,Line);
            Writeln(Dst,Line);
          end;
        Close(Src);
      end;
    Filemode:=OldFilemode;
    Close(Dst);
  end;


procedure CheckParas;
  var
    FirstFile,
    I: Longint;
    Exists: boolean;
  begin
    { enough parameters? }
    if ParamCount<2 then
      Usage;

    FirstFile:=1;
    if UpperCase(ParamStr(1))='-i' then
        begin
          IgnoreNonExisting:=true;
          Inc(FirstFile);
        end;

    { check destination }
    if DirectoryExists(ParamStr(ParamCount)) then
      begin
        Writeln('Destination "',ParamStr(ParamCount),'" is a directory');
        halt(2);
      end;
    Assign(Dst,ParamStr(ParamCount));
{$i-}
    Rewrite(Dst);
{$i+}
    if IOResult<>0 then
      begin
        Writeln('Unable to create destination file "',ParamStr(ParamCount),'"');
        halt(2);
      end;
    { check source(s) }
    for I:=FirstFile to ParamCount-1 do
      begin
        Exists:=True;
        if not FileExists(ParamStr(I)) then
          begin
            if not IgnoreNonExisting then
              begin
                Writeln('File "',ParamStr(I),'" does not exist');
                halt(2);
              end;
            Exists:=False;
          end
        else if DirectoryExists(ParamStr(I)) then
          begin
            Writeln('"',ParamStr(I),'" is a directory');
            halt(2);
          end
        else if Exists then
          FileList.Add(ParamStr(I));
        end
  end;


begin
  FileList:=TStringList.Create;
  CheckParas;
  DoConcat;
  FileList.Free;
end.
