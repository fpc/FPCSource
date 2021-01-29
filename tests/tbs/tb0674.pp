{ %NORUN }

{$mode objfpc}{$H+}

uses
  ub0674;

var
  LaunchRequest: TObject;
  c: TMyClass;
begin
  c:=TMyClass.Create;
  LaunchRequest := c.specialize CreateObjectFromJSONString<TObject>('qwe');
end.
