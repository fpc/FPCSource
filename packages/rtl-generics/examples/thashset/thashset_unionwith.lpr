// Generic types for NewPascal.org and FPC!
// by Maciej Izak (hnb), 2018
// sponsored by Sphere 10 Software (http://sphere10.com)

program thashset_unionwith;

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
  EvenNumbers: THashSet<Integer>;
  OddNumbers: THashSet<Integer>;
  Numbers: THashSet<Integer>;
  i: Integer;
begin
  EvenNumbers := THashSet<Integer>.Create;
  OddNumbers := THashSet<Integer>.Create;

  for i := 0 to 4 do
  begin
    EvenNumbers.Add(i * 2);
    OddNumbers.Add((i * 2) + 1);
  end;

  WriteLnHashSet('EvenNumbers', EvenNumbers);
  WriteLnHashSet('OddNumbers', OddNumbers);

  WriteLn('< Numbers UnionWith OddNumbers >');
  Numbers := THashSet<Integer>.Create(EvenNumbers);
  Numbers.UnionWith(OddNumbers);
  WriteLnHashSet('Numbers', Numbers);

  Numbers.Free;
  OddNumbers.Free;
  EvenNumbers.Free;

  ReadLn;
end.

