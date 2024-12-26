program tw40011;

{$mode delphi}
{$modeswitch nestedprocvars}
{$modeswitch anonymousfunctions}

type
  TFuncNested = function (const P1: String): String is nested;

  TMyClass = class
    class function CallFn(const A: String; const Fn: TFuncNested): String;
    class function Test(const A, B: String): String;
  end;

class function TMyClass.CallFn(const A: String; const Fn: TFuncNested): String;
begin
  Result := Fn(A);
end;

class function TMyClass.Test(const A, B: String): String;
begin
  Result := CallFn(A, function (const P1: String): String begin
      Result := P1 + B; // <-- SIGSEGV !!!
    end);
end;

var
  Sum: String;
begin
  Sum := TMyClass.Test('1', '2');
  if Sum <> '12' then
    Halt(1);
  WriteLn('OK');
end.

