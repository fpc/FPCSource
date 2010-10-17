{ %opt=-g-h }

program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, sysutils
  { you can add units after this };

type
  { TInterfacedObj }

  TInterfacedObj = class(TObject, IUnknown)
    private
      FOwner:TInterfacedObj;
      FDestructorCalled:boolean;

      function GetInterface(const iid: tguid; out obj): longint;
      procedure Log(const Str:string);
    protected
      FRefCount : longint;
    public
      function QueryInterface(constref iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
      function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
      function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};

      constructor Create;

      procedure AfterConstruction;override;
      procedure BeforeDestruction;override;
      class function NewInstance : TObject;override;

      property Owner:TInterfacedObj read FOwner write FOwner;
  end;


  IIntf1 = interface
    ['{EFB94FA8-4F38-4E44-8D12-74A84D07A78C}']
  end;

  IIntf2 = interface
   ['{EBC4A858-7BAC-4310-8426-E52B449D022A}']
    procedure Print;
    procedure SetI(const S:string);
  end;

  TClass1 = class(TInterfacedObj, IIntf1)

  end;

  { TClass2 }

  TClass2 = class(TInterfacedObj, IIntf2)
    i:string;
    procedure Print;
    procedure SetI(const S:string);
  end;

  TClass3 = class(TClass1, IIntf2)
    private
      FIntf2:IIntf2;
      property Intf2Prop:IIntf2 read FIntf2 implements IIntf2;
    public
      constructor Create;
  end;

{ TClass2 }

procedure TClass2.Print;
begin
  WriteLn('Print ', i);
end;

procedure TClass2.SetI(const S: string);
begin
  i:=S;
end;

  { TInterfacedObj }

  const Err = HResult($80004002);
  function TInterfacedObj.GetInterface(const iid: tguid; out obj): longint;
  begin
    if inherited GetInterface(IID, Obj) then
      Result:=0
    else
      Result:=Err;
  end;

  procedure TInterfacedObj.Log(const Str: string);
  begin
    WriteLn(Format('%s Obj=$%P class=%s RefCount=%d', [Str, Pointer(Self), ClassName, FRefCount]));
  end;

function TInterfacedObj.QueryInterface(constref iid: tguid; out obj): longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  begin
    Result:=GetInterface(iid, obj);

    //try to find interface in Owner
    if (FOwner <> nil) and (Result = Err) then
      Result:=FOwner.QueryInterface(iid, obj);
  end;

  function TInterfacedObj._AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};[public,alias:'TInterfacedObj_AddRef'];
  begin
    if not FDestructorCalled then
      begin
        _addref:=interlockedincrement(frefcount);
        Log('AddRef');

        if FOwner <> nil then
           FOwner._AddRef;
      end;
  end;

  function TInterfacedObj._Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  begin
    if FDestructorCalled then Exit;

    _Release:=interlockeddecrement(frefcount);
    Log('Release');
    if _Release=0 then
      begin
        FDestructorCalled:=True;

        Log('Destroy');
        self.destroy;
      end
      else
      if FOwner <> nil then
        FOwner._Release;
  end;

  procedure TInterfacedObj.AfterConstruction;
  begin
     { we need to fix the refcount we forced in newinstance }
     { further, it must be done in a thread safe way        }
     //declocked(frefcount);
    interlockeddecrement(frefcount);
    Log('AfterConstruction');
  end;

  procedure TInterfacedObj.BeforeDestruction;
  begin
     Log('BeforeDestruction');
     if frefcount<>0 then
       raise Exception.Create('Cannot free object still referenced.');
  end;

  class function TInterfacedObj.NewInstance : TObject;
  begin
     NewInstance:=inherited NewInstance;
     if NewInstance<>nil then
       TInterfacedObj(NewInstance).frefcount:=1;
  end;

  constructor TInterfacedObj.Create;
  begin
    FDestructorCalled:=false;
    inherited Create;
    FOwner:=nil;
  end;


{ TClass2 }

constructor TClass3.Create;
var O:TClass2;
begin
  inherited Create;
  O:=TClass2.Create;
  FIntf2:=O;
  O.Owner:=Self;

  FIntf2.SetI('AAA'); //this line is crucial for bug reproducing
end;

var O:TClass3;
    I1:IIntf1;
    I2:IIntf2;
begin
  HaltOnNotReleased := true;
  O:=TClass3.Create;
  I1:=O;

  //at this moment O object is already freed in rev.15156+ !!!
  I2:=I1 as IIntf2;
  I2.Print;
  Writeln('ok');
end.

