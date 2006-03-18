{ %OPT=-vn -Sen }

{ Source provided for Free Pascal Bug Report 4826 }
{ Submitted by "Ivo Steinmann" on  2006-02-20 }
{ e-mail: isteinmann@bluewin.ch }
program bug;

{$mode delphi}

type
  TTest = class
  private
    FFoobar: Integer;
  protected
    property Foobar: Integer read FFoobar write FFoobar;
  public
    constructor Create;
  end;

constructor TTest.Create;
begin
  inherited Create;
  Foobar := 0;
end;

var
  Test: TTest;
begin
  Test := TTest.Create;
  Test.Free;
end.
