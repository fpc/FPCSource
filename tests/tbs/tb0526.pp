{%norun}
{$mode objfpc}
type
  TMyClass = class
  protected
    Field1 {$IFDEF DUMMY}, Field2 {$ENDIF} : Boolean;
    procedure Proc;
  end;

procedure TMyClass.Proc;
begin
  Field1:=True;
end;

begin
end.
