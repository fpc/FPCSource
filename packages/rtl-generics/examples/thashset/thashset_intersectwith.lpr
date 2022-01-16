// Generic types for NewPascal.org and FPC!
// by Maciej Izak (hnb), 2018
// sponsored by Sphere 10 Software (http://sphere10.com)

program thashset_intersectwith;

{$MODE DELPHI}
{$APPTYPE CONSOLE}

uses
  SysUtils, Generics.Collections;

function SetToStr(ASet: THashSet<string>): string;
var
  i: string;
begin
  Result := '(';
  for i in ASet do
    Result := Result + ' ' + i;
  Result := Result + ' )';
end;

procedure WriteLnHashSet(const AName: string; AHashSet: THashSet<string>);
begin
  WriteLn(Format('%0:s.Count = %1:d %0:s = %2:s', [AName, AHashSet.Count, SetToStr(AHashSet)]));
end;

var
  Group1: THashSet<string>;
  Group2: THashSet<string>;
  Group3: THashSet<string>;
begin
  Group1 := THashSet<string>.Create;
  Group2 := THashSet<string>.Create;

  Group1.Add('User1');
  Group1.Add('User2');
  Group1.Add('User3');
  Group2.Add('User3');
  Group2.Add('User4');
  Group2.Add('User5');

  WriteLnHashSet('Group1', Group1);
  WriteLnHashSet('Group2', Group2);

  WriteLn('< Group3 IntersectWith Group2 >');
  Group3 := THashSet<string>.Create(Group1);
  Group3.IntersectWith(Group2);
  WriteLnHashSet('Group3', Group3);

  Group3.Free;
  Group2.Free;
  Group1.Free;

  ReadLn;
end.

