program testini;

{$mode objfpc}{$H+}

uses
  inifiles, classes;

var
  i: Integer;
  ini: TMemIniFile;
  lines: TStrings;

begin
  lines:=TStringList.Create();
  try
    lines.Add('[main]');
    lines.Add('key_a=1');
    lines.Add(';comment');
    lines.Add('key_b   =2');
    lines.Add('not_valid');
    lines.Add('key_c=   3');
    lines.Add('key_d="3"');
    WriteLn('ini file source:');
    for i:=0 to lines.Count-1 do 
      WriteLn('  ', lines[i]);
    ini:=TMemIniFile.Create('');
    try
      ini.options:=ini.options+[ifoStripQuotes];
      ini.SetStrings(lines);
      lines.Clear();
      ini.ReadSectionValues('main', lines,[]);
      WriteLn('ReadSectionValues (no options):');
      for i:=0 to lines.Count-1 do 
        WriteLn('  ', lines[i]);
      lines.Clear();
      ini.ReadSectionValues('main', lines,[svoIncludeComments]);
      WriteLn('ReadSectionValues (with comments, no invalid):');
      for i:=0 to lines.Count-1 do
        WriteLn('  ', lines[i]);
      lines.Clear();
      ini.ReadSectionValues('main', lines,[svoIncludeInvalid]);
      WriteLn('ReadSectionValues (without comments, with invalid):');
      for i:=0 to lines.Count-1 do
        WriteLn('  ', lines[i]);
      lines.Clear();
      ini.ReadSectionValues('main', lines,[svoIncludeComments,svoIncludeInvalid]);
      WriteLn('ReadSectionValues (with comments, with invalid):');
      for i:=0 to lines.Count-1 do
        WriteLn('  ', lines[i]);
      Lines.Clear;
      ini.ReadSectionValues('main', lines,[svoIncludeQuotes]);
      WriteLn('ReadSectionValues (with quotes):');
      for i:=0 to lines.Count-1 do
        WriteLn('  ', lines[i]);
    finally
      ini.Free();
    end;
  finally
    lines.Free();
  end
end.
