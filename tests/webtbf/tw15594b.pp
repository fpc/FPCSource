{ %fail }

program project1;

{$mode objfpc}{$H+}

type
  
  { TBoolObject }

  TBoolObject = class(TObject)
  private
    fBool: Boolean;
    procedure SetBool(const AValue: Boolean);
  
  protected
    function GetBool: Boolean;
    property Bool: Boolean read GetBool write SetBool default True;
  end;
  
  TSubBoolObject = class(TBoolObject)
  published
    property Bool default True;
  end;

{ TBoolObject }

procedure TBoolObject.SetBool(const AValue: Boolean);
begin
  fBool:=AValue;
end;

function TBoolObject.GetBool: Boolean; 
begin
  Result:=fBool;
end;
  
var
  b: TSubBoolObject;
begin
  b:=TSubBoolObject.Create;
  b.Bool=False; // error: Illegal expression
  b.Free;
end.
