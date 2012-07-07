{ %NORUN }

program tw21921;

{$mode Delphi}{$H+}

type

 { THashEntry }

 THashEntry<T> = record
   Key: string;
   Value: T;
   class function Create(const AKey: string; const AValue: T): THashEntry<T>; static; inline;
 end;

class function THashEntry<T>.Create(const AKey: string; const AValue: T): THashEntry<T>;
begin
  Result.Key := AKey;
  Result.Value := AValue;
end;

var
  Entry: THashEntry<Integer>;
begin
  Entry := THashEntry<Integer>.Create('One', 1);
end.

