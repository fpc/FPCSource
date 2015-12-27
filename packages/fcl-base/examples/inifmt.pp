program inifmt;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, IniFiles, SysUtils
  { you can add units after this };

var
  ini: TCustomIniFile;
  x: Double;
  t: TTime;
  d: TDate;
  dt: TDateTime;
  iniName: String = 'test.ini';
  L: TStringList;
  i: Integer;

begin
  x := 1.2345;
  t := time();
  d := date();
  dt := now();

  ini := TMemIniFile.Create(iniName);
  ini.FormatSettings.DecimalSeparator := '|';
  ini.FormatSettingsActive := true;
  ini.WriteFloat('Data', 'float', 1.2345);
  ini.WriteTime('Data', 'time', t);
  ini.WriteDate('Data', 'date', d);
  ini.WriteDateTime('Data', 'datetime', dt);
  ini.Free;

  WriteLn('-----------------------------------------------------------');
  WriteLn('Ini file (direct file content)');
  WriteLn('-----------------------------------------------------------');
  L := TStringList.Create;
  L.LoadfromFile(ininame);
  for i:=0 to L.Count-1 do
    WriteLn(L[i]);
  L.Free;
  WriteLn;

  ini := TMemIniFile.Create(iniName);
  ini.FormatSettings.DecimalSeparator := '|';
  ini.FormatSettingsActive := true;
  x := ini.ReadFloat('Data', 'float', 0);
  t := ini.ReadTime('Data', 'time', 0);
  d := ini.ReadDate('Data', 'date', 0);
  dt := ini.ReadDateTime('Data', 'datetime', 0);
  ini.Free;

  WriteLn('------------------------------------------------------------------');
  WriteLn('Read input data from ini file (output using DefaultFormatSettings)');
  WriteLn('------------------------------------------------------------------');
  WriteLn('float = ', FloatToStr(x));
  WriteLn('time = ', TimeToStr(t));
  WriteLn('date = ', DateToStr(d));
  WriteLn('date/time = ', DateTimeToStr(dt));
  WriteLn;

  {$IFDEF MSWINDOWS}
  WriteLn('Press [ENTER] to quit...');
  ReadLn;
  {$ENDIF}
end.


