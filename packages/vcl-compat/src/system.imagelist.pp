{$mode objfpc}
{$h+}
unit System.ImageList;

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.UITypes;
{$ELSE}
uses
  Classes, System.UITypes;
{$ENDIF}

type
  TImageLink = class;

  { TBaseImageList }

  TBaseImageList = class(TComponent)
  private
    FUpdateCount: Integer;
    FList: TFPList;
    FChanged: Boolean;
    function GetLinkCount: Integer;
    function GetLinks(const aIndex: Integer): TImageLink;
    Procedure ClearList;
  protected
    procedure AddLink(aLink: TImageLink);
    procedure DeleteLink(aLink: TImageLink);
    function LinkContains(const aLink: TImageLink; const aStartIndex: Integer = -1): Boolean;
    procedure DoChange; virtual; abstract;
    function GetCount: Integer; virtual; abstract;
    procedure Updated; override;
    procedure Loaded; override;
    property LinkCount: Integer read GetLinkCount;
    property Links[aIndex: Integer]: TImageLink read GetLinks;
  public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;
    procedure Change; virtual;
    procedure BeginUpdate;
    procedure EndUpdate;
    property Count: Integer read GetCount;
  end;

  { TImageLink }

  TImageLink = class
  private
    FImages: TBaseImageList;
    FImageIndex: TImageIndex;
    FIgnoreIndex: Boolean;
    FOnChange: TNotifyEvent;
    FIgnoreImages: Boolean;
    procedure SetImageList(aValue: TBaseImageList);
    procedure SetImageIndex(aValue: TImageIndex);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Change; virtual;
    property Images: TBaseImageList read FImages write SetImageList;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex;
    property IgnoreIndex: Boolean read FIgnoreIndex write FIgnoreIndex;
    property IgnoreImages: Boolean read FIgnoreImages write FIgnoreImages;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils;
{$ELSE}
  SysUtils;
{$ENDIF}

{ TBaseImageList }

function TBaseImageList.GetLinkCount: Integer;
begin
  Result:=FList.Count;
end;


function TBaseImageList.GetLinks(const aIndex: Integer): TImageLink;

begin
  Result:=TImageLink(FList[aIndex]);
end;


procedure TBaseImageList.AddLink(aLink: TImageLink);

begin
  if Not assigned(aLink) then
    exit;
  FList.Add(aLink);
end;

procedure TBaseImageList.DeleteLink(aLink: TImageLink);

begin
  if not Assigned(aLink) then
    exit;
  FList.Remove(aLink);
  aLink.FImages:=Nil;
end;


function TBaseImageList.LinkContains(const aLink: TImageLink; const aStartIndex: Integer): Boolean;

begin
  Result:=False;
  if (aStartIndex<0) or (aStartIndex>=LinkCount) then
    exit;
  Result:=FList.IndexOf(aLink)>=aStartIndex;
end;


procedure TBaseImageList.Updated;

begin
  inherited Updated;
  if FChanged then
    Change;
end;


procedure TBaseImageList.Loaded;

begin
  inherited Loaded;
  if FChanged then
     Change;
end;


procedure TBaseImageList.ClearList;

var
  aCount : integer;

begin
  aCount:=FList.Count-1;
  While aCount>=0 do
    begin
    TImageLink(FList[aCount]).FImages:=Nil;
    FList.Delete(aCount);
    aCount:=FList.Count-1;
    end;
end;


constructor TBaseImageList.Create(aOwner: TComponent);

begin
  inherited Create(aOwner);
  FList:=TFPList.Create;
end;


destructor TBaseImageList.Destroy;

begin
  ClearList;
  FreeAndNil(FList);
  inherited Destroy;
end;


procedure TBaseImageList.Change;

const
  NoChangeStates = [csLoading,csDestroying,csUpdating];

begin
  FChanged:=True;
  if ((ComponentState*NoChangeStates)=[]) then
    begin
    DoChange;
    FChanged:=False;
    end;
end;


procedure TBaseImageList.BeginUpdate;

begin
  if FUpdateCount = 0 then
    Updating;
  Inc(FUpdateCount);
end;


procedure TBaseImageList.EndUpdate;

begin
  if FUpdateCount<=0 then
    exit;
  Dec(FUpdateCount);
  if FUpdateCount=0 then
    Updated;
end;

{ TImageLink }

procedure TImageLink.SetImageList(aValue: TBaseImageList);

begin
  if aValue=FImages then
    exit;
  if Assigned(FImages) then
    FImages.DeleteLink(Self);
  FImages:=aValue;
  if Assigned(FImages) then
    FImages.AddLink(Self);
  if not FIgnoreImages then
    Change;
end;


procedure TImageLink.SetImageIndex(aValue: TImageIndex);

begin
  if aValue=FImageIndex then
    exit;
  FImageIndex:=aValue;
  If not IgnoreIndex then
    Change;
end;


constructor TImageLink.Create;

begin
  FImageIndex:=-1;
end;


destructor TImageLink.Destroy;

begin
  Images:=Nil;
  inherited Destroy;
end;


procedure TImageLink.Change;

begin
  if Assigned(FOnChange) then
    FOnChange(FImages);
end;


end.
