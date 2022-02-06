{ %NORUN }
{ .%FAIL }
{ Note: according to tests with Delphi this test *should* be FAIL, because if a
        method in a class does not have the overload directive it should hide
        the same methods introduced by a parent class, but since at least 2010
        we apply the overload flag to all methods of the same name if one of the
        parents had this flag as well }

program tfuncref13;

{$mode objfpc}{$H+}
{$modeswitch functionreferences}

uses
  ufuncref10;

var
  l: LongInt;
  i: ITestFunc5;
begin
  { only the overload with the parameter is available }
  l := i();
end.
