{ in mode Delphi a member that has generic and non-generic overloads and is
  compared with "<" is the member of its own structure, not a routine of the
  same name elsewhere }
program tgeneric133;

{$mode delphi}

type
  TUtil = class
    class function Cmp: Integer; overload; static;
    class function Cmp<T>: Integer; overload; static;
    function Val: Integer; overload; virtual;
    function Val<T>: Integer; overload;
  end;

  TDerived = class(TUtil)
    function Val: Integer; override;
    function Test: Boolean;
  end;

  TRec = record
    function Get: Integer; overload;
    function Get<T>: Integer; overload;
  end;

class function TUtil.Cmp: Integer;
begin
  Result := 2;
end;

class function TUtil.Cmp<T>: Integer;
begin
  Result := 5;
end;

function TUtil.Val: Integer;
begin
  Result := 2;
end;

function TUtil.Val<T>: Integer;
begin
  Result := 5;
end;

function TDerived.Val: Integer;
begin
  Result := 50;
end;

function TDerived.Test: Boolean;
begin
  Result := inherited Val < 3;
end;

function TRec.Get: Integer;
begin
  Result := 2;
end;

function TRec.Get<T>: Integer;
begin
  Result := 5;
end;

{ a routine of the same name as the members, which they must not reach }
function Cmp: Integer; overload;
begin
  Result := 100;
end;

function Cmp<T>: Integer; overload;
begin
  Result := 200;
end;

function Val: Integer; overload;
begin
  Result := 100;
end;

function Val<T>: Integer; overload;
begin
  Result := 200;
end;

function Get: Integer; overload;
begin
  Result := 100;
end;

function Get<T>: Integer; overload;
begin
  Result := 200;
end;

var
  u: TUtil;
  d: TDerived;
  r: TRec;
begin
  if not (TUtil.Cmp < 3) then
    Halt(1);
  u := TUtil.Create;
  if not (u.Val < 3) then
    Halt(2);
  if not (r.Get < 3) then
    Halt(3);
  d := TDerived.Create;
  if not d.Test then
    Halt(4);
  { the routine itself is still reached unqualified }
  if Cmp < 3 then
    Halt(5);
  if TUtil.Cmp<Integer> <> 5 then
    Halt(6);
  d.Free;
  u.Free;
  WriteLn('ok');
end.
