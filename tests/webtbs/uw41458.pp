unit uw41458;
{$ifdef FPC}{$mode delphi}{$endif}

interface

type
  TUtils = record
    class procedure GenericMethod<T>; static;
  end;
  TMyObj = record
    FAnsiString: AnsiString;
    procedure method;
  end;

var
  GlobObj: TMyObj;

implementation

class procedure TUtils.GenericMethod<T>;
begin
end;

procedure impl_proc(a: Double; b: String);
begin
  TUtils.GenericMethod<byte>;
end;

procedure TMyObj.method;
begin
  impl_proc(1.2, '-');
end;

end.

