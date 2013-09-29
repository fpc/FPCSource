program tw23130;
{$MODE DELPHI}

type
  TFunction<TArgument, TResult> = function (const arg: TArgument): TResult;

  TWrapper = record
    class function Z(const arg: Integer): Boolean; static;
    class procedure W; static;
  end;

  TWrapper2 = class
    procedure ZZ(f: TFunction<Integer, Boolean>);
  end;

class function TWrapper.Z(const arg: Integer): Boolean;
begin
  Result := arg < 0;
end;

class procedure TWrapper.W;
begin
  with TWrapper2.Create do begin
    ZZ(@Z);  { Replace with @TWrapper.Z to get rid of the error }
    Free;
  end;
end;

procedure TWrapper2.ZZ(f: TFunction<Integer, Boolean>);
begin
end;

begin
end.
