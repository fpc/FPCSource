program project1;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$IFDEF MSWINDOWS}
  {$APPTYPE CONSOLE}
{$ENDIF}

{$r+}

uses SysUtils;

type
  TSomeClass = class
  public
    procedure LoadSomething;
  end;

  TA = class
  private
    FSomeObject: TSomeClass;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property SomeObject: TSomeClass read FSomeObject;
  end;

var
  i: longint;
  order: array[1..4] of longint;

procedure TSomeClass.LoadSomething;
begin
  raise Exception.Create('An exception loading something');
end;

constructor TA.Create;
begin
  order[i]:=1;
  inc(i);
  WriteLn(1);
  inherited Create;
end;

destructor TA.Destroy;
begin
  order[i]:=2;
  inc(i);
  WriteLn(2);
  inherited Destroy;
end;

procedure TA.AfterConstruction;
begin
  order[i]:=3;
  inc(i);
  WriteLn(3);
  FSomeObject := TSomeClass.Create;
  FSomeObject.LoadSomething;
end;

procedure TA.BeforeDestruction;
begin
  order[i]:=4;
  inc(i);
  WriteLn(4);
  FSomeObject.Free;
end;

var
  VA: TA;
  ok: boolean;
begin
  i:=1;
  ok:=false;
  try
    VA := TA.Create;
  except
    if order[1]<>1 then
      halt(1);
    if order[2]<>3 then
      halt(2);
    if order[3]<>4 then
      halt(3);
    if order[4]<>2 then
      halt(4);
    if i<>5 then
      halt(5);
    if assigned(va) then
      halt(6);
    ok:=true;
  end;
  if not ok then
    halt(7);
end.
