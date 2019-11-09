unit utdos;

{$mode objfpc}

interface

uses
  Classes, SysUtils;

{ verifies that the DOSError variable is equal to }
{ the value requested.                            }

Function CheckDosError(Msg : String; err: Integer) : Boolean;

implementation

uses dos, punit;

Function CheckDosError(Msg : String; err: Integer) : Boolean;

 var
  x : integer;
  s :string;
 Begin
  x := DosError;
  case x of
  0 : s := '(0): No Error.';
  2 : s := '(2): File not found.';
  3 : s := '(3): Path not found.';
  5 : s := '(5): Access Denied.';
  6 : s := '(6): Invalid File Handle.';
  8 : s := '(8): Not enough memory.';
  10 : s := '(10) : Invalid Environment.';
  11 : s := '(11) : Invalid format.';
  18 : s := '(18) : No more files.';
  else
   begin
    Str (X, S);
    s := '(' + s + ') - INVALID DOSERROR';
   end
  end;
  Result:=AssertEquals(Msg+': Value of DOSError ('+S+')',Err,X);
 end;

end.

