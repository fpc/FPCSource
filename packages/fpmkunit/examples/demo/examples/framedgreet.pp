{
  Example program of the fpmkunit demo package, compiled only with fpmake -be.
  Uses the package unit DemoGreet and the example unit ExampleFrame.
}
program framedgreet;

{$mode objfpc}{$H+}

uses
  DemoGreet, ExampleFrame;

begin
  Writeln(Framed(Greeting(ParamStr(1))));
end.
