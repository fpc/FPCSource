{ %NORUN }


{$ifdef fpc}
{$mode objfpc}
{$endif}
uses sysutils;

type

  EWbcError = class of Exception;
  
  
Begin
  raise EwbcError.create('Hello');
end.

{
  $Log$
  Revision 1.1  2003-02-02 13:46:06  carl
   + exception testing

}  
