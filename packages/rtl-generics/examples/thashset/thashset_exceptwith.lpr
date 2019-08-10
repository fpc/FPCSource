// Generic types for NewPascal.org and FPC!
// by Maciej Izak (hnb), 2018
// sponsored by Sphere 10 Software (http://sphere10.com)

program thashset_exceptwith;

{$MODE DELPHI}
{$APPTYPE CONSOLE}

uses
  SysUtils, Generics.Collections;

function SetToStr(ASet: THashSet<Integer>): string;
var
  i: Integer;
begin
  Result := '(';
  for i in ASet do
    Result := Result + ' ' + IntToStr(i);
  Result := Result + ' )';
end;

procedure WriteLnHashSet(const AName: string; AHashSet: THashSet<Integer>);
begin
  WriteLn(Format('%0:s.Count = %1:d %0:s = %2:s', [AName, AHashSet.Count, SetToStr(AHashSet)]));
end;

var
  LowNumbers: THashSet<Integer>;
  HighNumbers: THashSet<Integer>;
  i: Integer;
begin
  LowNumbers := THashSet<Integer>.Create;
  HighNumbers := THashSet<Integer>.Create;

  for i := 0 to 5 do
    LowNumbers.Add(i);

  for i := 3 to 9 do
    HighNumbers.Add(i);

  WriteLnHashSet('LowNumbers', LowNumbers);
  WriteLnHashSet('HighNumbers', HighNumbers);

  WriteLn('< HighNumbers ExceptWith LowNumbers >');
  HighNumbers.ExceptWith(LowNumbers);
  WriteLnHashSet('HighNumbers', HighNumbers);

  HighNumbers.Free;
  LowNumbers.Free;

  ReadLn;
end.

