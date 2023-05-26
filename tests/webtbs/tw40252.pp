
type
  TLazLoggerLogEnabled = record end;

procedure Test(Log : TLazLoggerLogEnabled; const s : string);
begin
  writeln('Test: ',s);
end;

procedure Testv(var Log : TLazLoggerLogEnabled; const s : string);
begin
  writeln('Testv: ',s);
end;

procedure DebuglnStack(LogEnabled: TLazLoggerLogEnabled; const s: string);
begin
  Test(LogEnabled, s);
  Testv(LogEnabled, s);
end;

var
  LE : TLazLoggerLogEnabled;
begin
  DebuglnStack(LE,'Test string');
end.
