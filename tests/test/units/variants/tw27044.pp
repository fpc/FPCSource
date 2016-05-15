{ %norun }
{ Test that invoking methods on Variant that are function results, etc. 
  compiles without errors. }
{$mode delphi}{$H+}

uses SysUtils,variants;

type
  TTest = class
  private
   FObj: IDispatch;
   function GetObj: OleVariant;
  public
   property Obj: OleVariant read GetObj;
  end;

var tst: TTest;

function TTest.GetObj: OleVariant;
begin
  Result := FObj;
end;   

begin
  variant(0).foo;
  tst.Obj.bar;
end.

