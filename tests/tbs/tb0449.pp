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
  $Log$
  Revision 1.2  2003-10-31 16:15:26  peter
    * test exitcode 217

  Revision 1.1  2003/02/02 13:46:06  carl
   + exception testing

}  
