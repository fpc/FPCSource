{ %fail }
program compilercrash;
// This code is nonsense a.t.m. but it crashes the compiler.
{$mode objfpc}
uses classes;
type
{$M+}
Tmyclass = class(Tpersistent)
private
  FDetachable:Boolean;
  procedure setdetachable(const aValue:Boolean);
public
  property Detachable: Boolean read FDetachable write SetDetachable
  ['widgetsets:qt,gtk,win32', 'implementor:Vasya Pupkin', 'creation-date:01.01.2007'];
end;

begin
{ the initial purpose of this test was that the compiler shouldn't crash on the attribute above,
  however, as soon as attributes are implemented, it will succeed, so cause always an error at the end }
end;
