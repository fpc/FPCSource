{ Source provided for Free Pascal Bug Report 4789 }
{ Submitted by "Andrew Haines" on  2006-02-09 }
{ e-mail: andrewd207@aol.com }
program Project1;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils
  { add your units here };

type

  { TSimpleObj }
  TSimpleSet = (ssOne, ssTwo, ssThree);

  TSimpleObj = class(TObject)
  private
    procedure SetTestVal(const AIndex: Integer; const AIsSet: Boolean);
    function GetTestVal(const AIndex: Integer): Boolean;
  public
    Property TestVal: Boolean index Ord(ssOne) read GetTestVal write SetTestVal;
  end;

{ TSimpleObj }

procedure TSimpleObj.SetTestVal(const AIndex: Integer; const AIsSet: Boolean);
begin

end;

function TSimpleObj.GetTestVal(const AIndex: Integer): Boolean;
begin

end;

begin
end.