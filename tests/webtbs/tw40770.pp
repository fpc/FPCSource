program tw40770;

{$ifdef FPC}{$mode DELPHI}{$endif}

type
  Wrapper<T> = record
    Value: T;
    function GetValue: T;
  end;

  Utils = record
  public
    class function Method1<T>(): Wrapper<T>; static;
    class function Method2<T>(): Wrapper<T>; static;
  end;

function Wrapper<T>.GetValue: T;
begin
  Result := Value;
end;

var
  Log: string;
procedure AppendToLog(const S: string);
begin
  if Log = '' then Log := S else Log := Log + ' ' + S;
end;

class function Utils.Method1<T>(): Wrapper<T>;
begin
  if TypeInfo(T) = TypeInfo(Integer) then
    AppendToLog('Method1<Integer>')
  else if TypeInfo(T) = TypeInfo(string) then
     AppendToLog('Method1<string>')
  else
    Halt(1);
end;

class function Utils.Method2<T>(): Wrapper<T>;
begin
  Result := Method1<T>();
end;

begin
  Utils.Method2<Integer>();
  Utils.Method2<string>();
  if Log <> 'Method1<Integer> Method1<string>' then
    Halt(2);
  WriteLn('OK');
end.