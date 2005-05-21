{ Source provided for Free Pascal Bug Report 1204 }
{ Submitted by "Marco van de Voort" on  2000-10-29 }
{ e-mail: marco@freepascal.org }

{$mode delphi}

Uses Sysutils,Classes;

type
  TICMPDisplay = procedure(Sender: TObject; Msg : String) of object;
  TICMPReply   = procedure(Sender: TObject; Error : Integer) of
object;

  // The object wich encapsulate the ICMP.DLL
  TICMP = class(TObject)
  private
    FOnDisplay :      TICMPDisplay;               // Event handler to display
  public
    constructor Create; virtual;
    destructor  Destroy; override;
    property OnDisplay     : TICMPDisplay   read  FOnDisplay write FOnDisplay;
  end;

  TPingDisplay   = procedure(Sender: TObject; Icmp: TObject; Msg : String) of object;


  TPing = class(TComponent)
  private
    FIcmp             : TICMP;
    FOnDisplay        : TPingDisplay;
  protected
     procedure   IcmpDisplay(Sender: TObject; Msg: String);

  public
    constructor Create(Owner : TComponent); override;
    destructor  Destroy; override;
    property    OnDisplay   : TPingDisplay   read  FOnDisplay
                                             write FOnDisplay;

  end;

constructor TICMP.Create;
begin
end;

destructor TICMP.Destroy;
begin
end;

constructor TPing.Create(Owner : TComponent);
begin
    Inherited Create(Owner);
    FIcmp               := TICMP.Create;
    FIcmp.OnDisplay     := IcmpDisplay;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* * *}
destructor TPing.Destroy;
begin
end;

procedure TPing.IcmpDisplay(Sender: TObject; Msg: String);
begin
    if Assigned(FOnDisplay) then
        FOnDisplay(Self, Sender, Msg);
end;

begin
end.
