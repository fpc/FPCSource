{ %fail }

{ Source provided for Free Pascal Bug Report 3488 }
{ Submitted by "Jesus Reyes A." on  2004-12-28 }
{ e-mail: jesusrmx@yahoo.com.mx }
program opertest;

{$mode objfpc}{$H+}

type
  TBug=class
  private
    FField: Integer;
    procedure SetField(const AValue: Integer);
  public
    property Field: Integer read FField write SetField;
  end;

procedure TBug.SetField(const aValue: Integer);
begin
  WriteLn('SetField visited');
  FField := AValue;
end;

var
  Bug: TBug;
begin
  Bug := TBug.Create;
  Bug.Field := 10;

  { This is not allowed with properties }
  Bug.Field += 1;

  WriteLn('Bug.Field=',Bug.Field);
end.
