{ Source provided for Free Pascal Bug Report 3489 }
{ Submitted by "Danny Milosavljevic" on  2004-12-28 }
{ e-mail: danny_milo@yahoo.com }
program itest;

{$ifdef fpc}{$mode delphi}{$endif}

var
  err : boolean;

type
  IGtkObject = interface
    ['{C4741871-56EB-4A5C-BDC8-018992345B24}']
    procedure Destroy1();
  end;
  IGtkWidget = interface(IGtkObject)
    ['{210BF9BD-232D-43D5-9ECA-94B8AB2734E5}']

     procedure Destroy1();
     procedure Show();
     procedure Hide();
  end;

  DGtkObject = class(TInterfacedObject, IGtkObject, IInterface)
  public
     procedure Destroy1();
  end;

  DGtkWidget = class(DGtkObject, IGtkWidget, IGtkObject, IInterface)
  public
    constructor CreateWrapped(a:Pointer);
     procedure Destroy1();
     procedure Show();
     procedure Hide();
  end;

procedure DGtkObject.Destroy1();
begin
end;

constructor DGtkWidget.CreateWrapped(a:Pointer);
begin
end;

procedure DGtkWidget.Destroy1();
begin
end;

procedure DGtkWidget.Show();
begin
  Writeln('Show');
  err:=false;
end;

procedure DGtkWidget.Hide();
begin
  Writeln('Hide');
end;

var
  w: IGtkWidget;
begin
  err:=true;
  w := DGtkWidget.CreateWrapped(nil);
  w.Show();
  if err then
    halt(1);
end.
