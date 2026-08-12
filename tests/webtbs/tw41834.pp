{ %NORUN }

program tw41834;
{$Mode objfpc}{$Interfaces CORBA}
type
  IFoo = interface ['{613ACB35-37AE-467E-979F-B27647E78AAF}']
    procedure x;
  end;

  TBar = class(TObject, IFoo)
    procedure x; virtual; abstract;
  end;

  generic TGen<I: IFoo> = class//(TBar)
    procedure g1;
  end;

procedure TGen.g1;
begin
  (self as I).x;
end;

begin
  (TBar(nil) as IFoo).x;
end.
