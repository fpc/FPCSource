{ %version=1.1 }

{ Source provided for Free Pascal Bug Report 2696 }
{ Submitted by "Vincent Snijders" on  2003-09-28 }
{ e-mail: vslist@zonnet.nl }
{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  Classes;

type
  IBase = interface
    procedure a(i: integer); overload;
    procedure a(s: string); overload;
  end;
  IBase2 = interface(IBase)
    procedure c;
  end;
  TBase = class(TInterfacedObject, IBase)
  public
    procedure a(i: integer); overload;
    procedure a(s: string);  overload; virtual;
  end;
  TSubClass = class(TBase, IBase2)
    procedure a(s: string);  overload; override;
    procedure c;
  end;

{ TBase }

procedure TBase.a(i: integer);
begin

end;

procedure TBase.a(s: string);
begin

end;

{ TSubClass }

procedure TSubClass.a(s: string);
begin

end;

procedure TSubClass.c;
begin

end;

begin

end.
