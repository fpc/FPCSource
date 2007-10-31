program project1;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

procedure DebugLn(Args: array of const);
begin
  if (high(args) <> 1) or
     (args[0].vtype <> vtboolean) or
     not args[0].vboolean or
     (args[1].vtype <> vtboolean) or
     args[1].vboolean then
    halt(1);
end;

procedure DoSomething(ALeft, ATop, AWidth, AHeight : integer);
begin
  DebugLn([
    (1<>ALeft) or (2<>ATop),
    (3<>AWidth) or (4<>AHeight)
    ]);
end;

begin
  dosomething(1,0,3,4);
end.

