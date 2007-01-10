{ %opt=-Sen }
program notusedbug;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils
  { add your units here };
  
type
  TA = class
  private
    FC: integer;
  end;
  
  { TB }

  TB = class
  private FA: TA;
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TB }

constructor TB.Create;
begin
  FA := TA.Create;
  FA.FC := 4;
  writeln(FA.FC);
end;

destructor TB.Destroy;
begin
  FA.Free;
  inherited Destroy;
end;

var
  b: TB;

begin
  b := TB.Create;
  b.Free;
end.

