{ Source provided for Free Pascal Bug Report 3997 }
{ Submitted by "Dominique Louis" on  2005-05-21 }
{ e-mail: Dominique@SavageSoftware.com.au }

{$mode delphi}
program Project1;

{$APPTYPE CONSOLE}

uses
  SysUtils;

type
  TMyNotifyEvent =  procedure of object;

  TMyBaseWindow = class( TObject )
  private
    FOnRender: TMyNotifyEvent;
  public
    property OnRender: TMyNotifyEvent read FOnRender write FOnRender;
  end;

  TBaseInterface = class( TObject )
  protected
    procedure Render; virtual; abstract;
  public
    MainWindow : TMyBaseWindow;
    constructor Create;
    destructor Destroy; override;
    procedure ResetInputManager;
  end;

  TMyInterface = class( TBaseInterface )
  protected
    procedure Render; override;
  end;


{ TBaseInterface }
constructor TBaseInterface.Create;
begin
  inherited;
  WriteLn( 'TBaseInterface.Create' );
  MainWindow := TMyBaseWindow.Create;
  ResetInputManager;
end;

destructor TBaseInterface.Destroy;
begin
  MainWindow.Free;
  inherited;
end;

procedure TBaseInterface.ResetInputManager;
begin
  WriteLn( 'ResetInputManager' );
  MainWindow.OnRender := Render;
end;

{ TMyInterface }
procedure TMyInterface.Render;
begin
  WriteLn( 'Rendering' );
end;

var
  MyInterface : TMyInterface;

begin
  MyInterface := TMyInterface.Create;

  MyInterface.Render;

  MyInterface.Free;
end.
