{ Source provided for Free Pascal Bug Report 4487 }
{ Submitted by "Phil H." on  2005-11-02 }
{ e-mail: pjhess@purdue.edu }
program TestVarBug;

{$IFDEF FPC}
{$mode objfpc}
uses
  Variants;
{$ENDIF}

type
  TMyClass = class
  private
    function GetValue(AsInt : Boolean) : Variant;
  public
    property Value[AsInt : Boolean] : Variant read GetValue;
  end;

function TMyClass.GetValue(AsInt : Boolean) : Variant;
begin
  if AsInt then
    Result := 1
  else
    Result := True;
end;

var
  AClass : TMyClass;
begin
  AClass := TMyClass.Create;
  if (AClass.Value[True] = 1) and
     AClass.Value[False] then  //Throws exception with FPC (requires "= True")
    WriteLn('Value is True')
  else
    WriteLn('Value is False');
end.

