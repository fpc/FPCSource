{ demo_factory_component - TComponent descendants get automatic owner passing
  =============================================================================
  Demonstrates that TComponent descendants are automatically created with the
  correct owner when the factory context provides a TComponent as OwnerObject.

  No explicit factory registration is needed - the built-in TComponent handling
  in TOPClassFactory checks if the OwnerObject in context is a TComponent and
  passes it to the constructor.

  This demo creates TComponent-based objects directly via the factory to show
  the behavior, since the ORM's list loading uses TObject-based classes.

  Compile: fpc -Fu../../src demo_factory_component.lpr
  Run:     ./demo_factory_component
}
program demo_factory_component;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Opf.Factory;

type

  { TMyService - a TComponent descendant }

  TMyService = class(TComponent)
  private
    FServiceName: String;
  public
    property ServiceName: String read FServiceName write FServiceName;
  end;

  { TMyWidget - another TComponent descendant }

  TMyWidget = class(TComponent)
  end;

var
  AppOwner: TComponent;
  Ctx: TOPFactoryContext;
  Service: TMyService;
  Widget: TMyWidget;
  Obj: TObject;
  NonComp: TObject;

begin
  WriteLn('=== TComponent factory: automatic owner passing ===');
  WriteLn;

  AppOwner := TComponent.Create(nil);
  try
    { Case 1: TComponent owner in context - passed to constructor }
    WriteLn('Case 1: OwnerObject is a TComponent');
    Ctx := TOPFactoryContext.Create(frListItem);
    try
      Ctx.OwnerObject := AppOwner;
      Service := TMyService(OPFactory.CreateInstance(TMyService, Ctx));
      WriteLn('  Created: ', Service.ClassName);
      WriteLn('  Owner:   ', Service.Owner.ClassName, ' (passed automatically)');
      WriteLn('  ComponentCount on AppOwner: ', AppOwner.ComponentCount);
      // Don't free Service - AppOwner owns it
    finally
      Ctx.Free;
    end;
    WriteLn;

    { Case 2: Non-TComponent owner in context - created with nil owner }
    WriteLn('Case 2: OwnerObject is not a TComponent');
    NonComp := TObject.Create;
    try
      Ctx := TOPFactoryContext.Create(frChild);
      try
        Ctx.OwnerObject := NonComp;
        Widget := TMyWidget(OPFactory.CreateInstance(TMyWidget, Ctx));
        try
          WriteLn('  Created: ', Widget.ClassName);
          if Widget.Owner = nil then
            WriteLn('  Owner:   nil (non-TComponent owner ignored)')
          else
            WriteLn('  Owner:   ', Widget.Owner.ClassName);
        finally
          Widget.Free; // Must free manually - no owner
        end;
      finally
        Ctx.Free;
      end;
    finally
      NonComp.Free;
    end;
    WriteLn;

    { Case 3: Nil context - created with nil owner }
    WriteLn('Case 3: Nil context');
    Widget := TMyWidget(OPFactory.CreateInstance(TMyWidget, nil));
    try
      WriteLn('  Created: ', Widget.ClassName);
      if Widget.Owner = nil then
        WriteLn('  Owner:   nil (no context provided)')
      else
        WriteLn('  Owner:   ', Widget.Owner.ClassName);
    finally
      Widget.Free;
    end;
    WriteLn;

    { Case 4: Non-TComponent class - factory has no effect, uses default Create }
    WriteLn('Case 4: Non-TComponent class (regular TObject)');
    Ctx := TOPFactoryContext.Create(frListItem);
    try
      Ctx.OwnerObject := AppOwner;
      Obj := OPFactory.CreateInstance(TStringList, Ctx);
      try
        WriteLn('  Created: ', Obj.ClassName);
        WriteLn('  TStringList is not a TComponent - default constructor used');
      finally
        Obj.Free;
      end;
    finally
      Ctx.Free;
    end;
    WriteLn;

    WriteLn('AppOwner now has ', AppOwner.ComponentCount, ' owned component(s)');
    WriteLn('  (Service will be freed when AppOwner is freed)');
  finally
    AppOwner.Free; // Also frees Service
  end;

  WriteLn;
  WriteLn('Done.');
end.
