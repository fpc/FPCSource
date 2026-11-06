{
  Program of the fpmkunit demo package: greets the name given on the command line.
}
program greet;

{$mode objfpc}{$H+}

uses
  DemoGreet;

begin
  Writeln(Greeting(ParamStr(1)));
end.
