{$Mode objfpc}

{
  This unit introduces some basic classes as they are defined in Delphi.
  These classes should be source compatible to their Delphi counterparts:
    TPersistent
    TComponent
}

Unit tb0300;

{$M+}

Interface

Type

{ ---------------------------------------------------------------------
    Forward Declarations.
  ---------------------------------------------------------------------}

  TComponent = Class;
  TFiler = Class;
  TPersistent = Class;

{ ---------------------------------------------------------------------
    TFiler
  ---------------------------------------------------------------------}

  TFiler = Class (TObject)
    Protected
      FAncestor : TComponent;
      FIgnoreChildren : Boolean;
      FRoot : TComponent;
    Private
    Public
    Published
      { Methods }
      Constructor Create {(Stream : TStream; BufSize : Longint) };
      Destructor Destroy; override;
      Procedure FlushBuffer; virtual; abstract;
      { Properties }
      Property Root : TComponent Read FRoot Write FRoot;
      Property Ancestor : TComponent Read FAncestor Write FAncestor;
      Property IgnoreChildren : Boolean Read FIgnoreChildren Write FIgnoreChildren;
    end;

{ ---------------------------------------------------------------------
    TPersistent
  ---------------------------------------------------------------------}

  TPersistent = Class (TObject)
    Private
      Procedure AssignError (Source : TPersistent);
    Protected
      Procedure AssignTo (Dest : TPersistent);
      Procedure DefineProperties (Filer : TFiler); Virtual;
    Public
      { Methods }
      Destructor Destroy; Override;
      Procedure Assign (Source : TPersistent); virtual;
    Published
    end;

{ ---------------------------------------------------------------------
    TComponent
  ---------------------------------------------------------------------}

  TComponentState = Set of ( csLoading, csReading, CsWriting, csDestroying,
                             csDesigning, csAncestor, csUpdating, csFixups );
  TComponentStyle = set of ( csInheritable,csCheckPropAvail );
  TComponentName = String;

  TComponent = Class (TPersistent)
    Protected
      FComponentState : TComponentState;
      FComponentStyle : TComponentStyle;
      FName : TComponentName;

      FOwner : TComponent;
      Function GetComponent (Index : Longint) : TComponent;
      Function GetComponentCount : Longint;
      Function GetComponentIndex : Longint;
      Procedure SetComponentIndex (Value : Longint);
      Procedure Setname (Value : TComponentName);
    Private
    Public
      { Methods }
      { Properties }
      Property ComponentCount : Longint Read GetComponentCount; { RO  }
      Property ComponentIndex : Longint Read GetComponentIndex write SetComponentIndex; { R/W }
      // Property Components [Index : LongInt] : TComponent Read GetComponent; { R0 }
      Property ComponentState : TComponentState Read FComponentState; { RO }
      Property ComponentStyle : TcomponentStyle Read FComponentStyle; { RO }
      Property Owner : TComponent Read Fowner; { RO }
    Published
      Property Name : TComponentName Read FName Write Setname;
    end;




Implementation

{ ---------------------------------------------------------------------
    TComponent
  ---------------------------------------------------------------------}

Function TComponent.GetComponent (Index : Longint) : TComponent;

begin
end;



Function TComponent.GetComponentCount : Longint;

begin
end;



Function TComponent.GetComponentIndex : Longint;

begin
end;



Procedure TComponent.SetComponentIndex (Value : Longint);

begin
end;




Procedure TComponent.Setname (Value : TComponentName);

begin
end;



{ ---------------------------------------------------------------------
    TFiler
  ---------------------------------------------------------------------}

Constructor TFiler.Create {(Stream : TStream; BufSize : Longint) };

begin
end;




Destructor TFiler.Destroy;

begin
end;




{ ---------------------------------------------------------------------
    TPersistent
  ---------------------------------------------------------------------}

Procedure TPersistent.AssignError (Source : TPersistent);

begin
end;



Procedure TPersistent.AssignTo (Dest : TPersistent);

begin
end;



Procedure TPersistent.DefineProperties (Filer : TFiler);

begin
end;



Destructor TPersistent.Destroy;

begin
end;



Procedure TPersistent.Assign (Source : TPersistent);

begin
end;



end.
