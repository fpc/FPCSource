{ %RESULT=217 }

{$ifdef fpc}
{$mode objfpc}
{$endif}
uses SysUtils;

type
  EWbcError = class of Exception;

Begin
  raise EwbcError.create('Hello');
end.
