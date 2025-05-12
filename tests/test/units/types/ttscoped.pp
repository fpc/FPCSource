program ttscoped;

{$mode objFPC}

uses types;

type
      
//type
  TChildHandledObject = class
  private
    FStr : String;
  public
    class var InstanceCount : Integer;
  public
    constructor Create(const AStr:String);
    destructor Destroy(); override;
    procedure Display();
  end;

//type
  THandledObject = class
  private
    FStr : String;
    ScopedChild: specialize TScoped<TChildHandledObject>;
  public
    class var InstanceCount : Integer;
  public  
    constructor Create(const AStr:String);
    destructor Destroy(); override;
    procedure Display();
  end;

constructor TChildHandledObject.Create(const AStr:String);
begin
  FStr := AStr;
  Inc(InstanceCount);
end;

destructor TChildHandledObject.Destroy();
begin
  Dec(InstanceCount);
end;

procedure TChildHandledObject.Display();
begin
  writeln('ChildStr = ' + FStr);
end;

constructor THandledObject.Create(const AStr:String);
begin
  FStr := AStr;
  inc(InstanceCount);
  ScopedChild.Assign(TChildHandledObject.Create('"ChildStr - ' + AStr+'"'));
end;

destructor THandledObject.Destroy();
begin
  Dec(InstanceCount);
end;

procedure THandledObject.Display();
begin
  ScopedChild.Get.Display();
end;

procedure AssertEquals(aMsg : string; aExpected,aActual : Integer);
begin
  if aExpected<>aActual then
    begin
    Writeln(aMsg,' : Expected: ',aExpected,' Actual: ',aActual);
    Halt(1);
    end;
end;

procedure dotestsp;
var
  sc: specialize TScoped<THandledObject>;
begin
  AssertEquals('Initial THandledObject',0,THandledObject.InstanceCount);
  AssertEquals('Initial TChileHandledObject',0,TChildHandledObject.InstanceCount);
  sc.Assign(THandledObject.Create('Hello'));
  AssertEquals('Created THandledObject',1,THandledObject.InstanceCount);
  AssertEquals('Created TChildHandledObject',1,TChildHandledObject.InstanceCount);
  sc.Get().Display();
end;  

procedure dotestspassign;
var
  sc: specialize TScoped<THandledObject>;
begin
  AssertEquals('Initial THandledObject',0,THandledObject.InstanceCount);
  AssertEquals('Initial TChileHandledObject',0,TChildHandledObject.InstanceCount);
  sc:=THandledObject.Create('Hello');
  AssertEquals('Created THandledObject',1,THandledObject.InstanceCount);
  AssertEquals('Created TChildHandledObject',1,TChildHandledObject.InstanceCount);
  sc.Get().Display();
end;  

procedure dotestspassignto;
var
  sc: specialize TScoped<THandledObject>;
  v : THandledObject;
begin
  AssertEquals('Initial THandledObject',0,THandledObject.InstanceCount);
  AssertEquals('Initial TChileHandledObject',0,TChildHandledObject.InstanceCount);
  sc.assign(THandledObject.Create('Hello'));
  AssertEquals('Created THandledObject',1,THandledObject.InstanceCount);
  AssertEquals('Created TChildHandledObject',1,TChildHandledObject.InstanceCount);
  v:=sc;
  AssertEquals('Assigned THandledObject',1,THandledObject.InstanceCount);
  AssertEquals('Assigned TChildHandledObject',1,TChildHandledObject.InstanceCount);
  v.display;
  AssertEquals('Displayed THandledObject',1,THandledObject.InstanceCount);
  AssertEquals('Displayed TChildHandledObject',1,TChildHandledObject.InstanceCount);
end;  

begin
  Writeln('Simple');
  dotestsp;
  AssertEquals('Final THandledObject',0,THandledObject.InstanceCount);
  AssertEquals('Final TChileHandledObject',0,TChildHandledObject.InstanceCount);
  Writeln('Assign');
  dotestspassign;
  AssertEquals('Final THandledObject',0,THandledObject.InstanceCount);
  AssertEquals('Final TChileHandledObject',0,TChildHandledObject.InstanceCount);
  Writeln('Assign to');
  dotestspassignto;
  AssertEquals('Final THandledObject',0,THandledObject.InstanceCount);
  AssertEquals('Final TChileHandledObject',0,TChildHandledObject.InstanceCount);
end.


