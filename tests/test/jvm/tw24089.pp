program tw24089;

{ %VERSION=1.1 }

{$ifdef fpc}
  {$mode objfpc}
{$endif}

{$ifdef cpujvm}
uses
  {$ifdef java}jdk15{$else}androidr14{$endif};

{$macro on}
{$define writeln:=jlsystem.fout.println}
{$define write:=jlsystem.fout.println}
{$endif}

type
  TRec1tw24089 = record
    a: integer;
  end;

  TRec2tw24089 = record
    i: integer;
    Rec1: TRec1tw24089;
  end;

  TMyClasstw24089 = class
  protected
    Data: TRec2tw24089;
  public
    procedure Test;
  end;

procedure TMyClasstw24089.Test;
var
  LocData: TRec2tw24089;
begin
{
  with LocData do
    Rec1.a := 1;
  writeln(LocData.Rec1.a);	// success it shows 1

  LocData.Rec1.a := 2;
  writeln(LocData.Rec1.a);  // success it shows 2
}
  with Data do
   Rec1.a := 3;
  writeln(Data.Rec1.a);		// !!FAIL!!, it shows 0
  if Data.Rec1.a <> 3 then
    halt(1);
{
  Data.Rec1.a := 4;
  writeln(Data.Rec1.a);   	// success it shows 4
}
end;

begin
  with TMyClasstw24089.Create do
   Test;  
end.
