{ Concatenates a number of text files. This code is in the public domain. }

program concat;

uses
  SysUtils;

var
  Dst: TextFile;


procedure usage;
  begin
    Writeln('Usage: concat <srcfile1> [<srcfile2> ..] <dstfile>');
    Writeln;
    halt(1);
  end;


procedure DoConcat;
  var
    Src: TextFile;
    I: Longint;
    Line: Ansistring;
  begin
    for I:=1 to ParamCount-1 do
      begin
        Assign(Src,ParamStr(I));
        Reset(Src);
        while not Eof(Src) do
          begin
            ReadLn(Src,Line);
            Writeln(Dst,Line);
          end;
        Close(Src);
      end;
    Close(Dst);
  end;


procedure CheckParas;
  var
    I: Longint;
  begin
    { enough parameters? }
    if ParamCount<2 then
      Usage;
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
    for I:=1 to ParamCount-1 do
      begin
        if not FileExists(ParamStr(I)) then
          begin
            Writeln('File "',ParamStr(I),'" does not exist');
            halt(2);
          end;
        if DirectoryExists(ParamStr(I)) then
          begin
            Writeln('"',ParamStr(I),'" is a directory');
            halt(2);
          end;
        end;
  end;


begin
  CheckParas;
  DoConcat;
end.
