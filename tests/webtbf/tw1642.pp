{ %FAIL }
program TestDefaultProperty;

{$MODE OBJFPC}{$H+}

uses
  SysUtils;

type
  TMyClass = class
  private
    function GetItems(Index: integer): integer;
  public
    property Items[Index: integer]: integer read GetItems; default;
  end;

function TMyClass.GetItems(Index: integer): integer;
begin
  writeln('Get Index=',Index);
  Result:=Index;
end;

var MyClass: TMyClass;
  i: integer;

begin
  MyClass:=TMyClass.Create;
  i:=MyClass.Items;
  writeln('i=',i);
  MyClass.Free;
end.
