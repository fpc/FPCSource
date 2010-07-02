{$ifdef fpc}
{$mode delphi}{$H+}
{$endif}

uses
  Classes, SysUtils;

type

  { TForm1 }

  TForm1 = class
  private
    { private declarations }
  public
    { public declarations }
    procedure test;
  end; 

  FNType = function(A, B: integer): integer;
var
  Form1: TForm1; 

function Add23(A, B: integer; C: cardinal): integer; overload; forward;
function Add23(A, B: integer): integer; overload; forward;

const
  FPArray: FNType = Add23;

function Add23(A, B: integer; C: cardinal): integer; overload;
  begin
    Result := A + B + C;
    halt(1);
  end;

function Add23(A, B: integer): integer; overload;
  begin
    Result := A - B;
  end;

{ TForm1 }

procedure TForm1.test;
  var
    a, b: integer;
  begin
    a := 3;
    b := 4;
    writeln(FParray(a, b));
end;

var
  f: tform1;
begin
  f:=tform1.create;
  f.test;
  f.free;
end.

