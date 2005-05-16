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

{
  $Log: tb0449.pp,v $
  Revision 1.3  2005/02/14 17:13:35  peter
    * truncate log

}
