unit tw6989;

{$mode objfpc}{$H+}
{$inline on}

interface

uses
  Classes, SysUtils;

const
  ltValue   = 3;



type
  TDebugClass = 0..31;


  { TLogger }

  TLogger = class
  private
    FDefaultClass: TDebugClass;
   // FChannels:TChannelList;
    procedure SendString(AMsgType: Integer;const AText:String);
  public
    ActiveClasses: set of TDebugClass;//Made a public field toallow use of include
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Send(const AText: String); inline;
    procedure Send(AClass: TDebugClass; const AText: String);
    procedure Send(const AText: String; Args: array of const); inline;
    procedure Send(AClass: TDebugClass; const AText: String; Args: array of const);
    procedure Send(const AText, AValue: String); inline;
    procedure Send(AClass: TDebugClass; const AText,AValue: String);
    procedure Send(const AText: String; AValue: Integer); inline;
    procedure Send(AClass: TDebugClass; const AText: String; AValue: Integer);
    procedure Send(const AText: String; AValue: Boolean); inline;
    procedure Send(AClass: TDebugClass; const AText: String; AValue: Boolean);
    procedure Send(const AText: String; ARect: TRect); inline;
    //if the next inline is commented no error occurs
    procedure Send(AClass: TDebugClass; const AText: String; ARect: TRect); inline;
  end;

implementation

{ TLogger }

procedure TLogger.SendString(AMsgType: Integer; const AText: String);
begin
end;

constructor TLogger.Create;
begin
end;

destructor TLogger.Destroy;
begin
end;

procedure TLogger.Clear;
begin
end;

procedure TLogger.Send(const AText: String);
begin
end;

procedure TLogger.Send(AClass: TDebugClass; const AText: String);
begin
end;

procedure TLogger.Send(const AText: String; Args: array of const);
begin
end;

procedure TLogger.Send(AClass: TDebugClass; const AText: String;
  Args: array of const);
begin
end;

procedure TLogger.Send(const AText, AValue: String);
begin
end;

procedure TLogger.Send(AClass: TDebugClass; const AText, AValue: String);
begin
end;

procedure TLogger.Send(const AText: String; AValue: Integer);
begin
end;

procedure TLogger.Send(AClass: TDebugClass; const AText: String; AValue: Integer);
begin
end;

procedure TLogger.Send(const AText: String; AValue: Boolean);
begin
end;

procedure TLogger.Send(AClass: TDebugClass; const AText: String; AValue: Boolean);
begin
end;

procedure TLogger.Send(const AText: String; ARect: TRect);
begin

end;

procedure TLogger.Send(AClass: TDebugClass; const AText: String; ARect: TRect);
begin
  //if this body procedure is commented also no error occurs
  if not (AClass in ActiveClasses) then Exit;
  with ARect do
    SendString(ltValue,Format('%s = (Left: %d; Top: %d; Right: %d; Bottom: %d)',[AText,Left,Top,Right,Bottom]));
end;
end.
