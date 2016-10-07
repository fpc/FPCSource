{$mode objfpc}{$H+}

uses SysUtils;

type
  TMyMethod = procedure (A: Integer) of object;

  TMyClass = class
    class procedure Foo(A: Integer);
  end;

class procedure TMyClass.Foo(A: Integer);
begin
  Writeln('Got int ', A);
end;

procedure CallMethod(M: TMyMethod);
begin
  M(123);
end;

begin
  CallMethod({$ifdef FPC_OBJFPC} @ {$endif} TMyClass(nil).Foo);
end.
