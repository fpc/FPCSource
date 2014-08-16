{ Source provided for Free Pascal Bug Report 3173 }
{ Submitted by "Dominik Zablotny" on  2004-06-18 }
{ e-mail: dominz@wp.pl }
program test;
{$ifdef fpc}
  {$mode delphi}
{$else}
type
  codepointer = pointer;
{$endif}
var
  p: procedure of object;

  function f:codepointer;
  begin
  end;

begin
  @p := f;
end.
