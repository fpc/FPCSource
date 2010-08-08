unit fpextjs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttp, db, httpdefs, fpwebdata;

Type


  { TExtJSDataFormatter }

  TExtJSDataFormatter = Class(TCustomHTTPDataContentProducer)
  private
    FMP: String;
    FPLP: String;
    FPSP: String;
    FRP: String;
    FSP: String;
    FTP: String;
    function IsMessageStored: boolean;
    function IsPageLimitStored: boolean;
    function IsPageStartStored: boolean;
    function IsRowsStored: boolean;
    Function IsSuccessStored : Boolean;
    function IsTotalStored: boolean;
  Public
    Constructor Create(AOwner : TComponent); override;
    Procedure DoReadRecords(Stream : TStream); override;
    Function ProduceContent : String; override;
    Property SuccessProperty : String Read FSP Write FSP stored IsSuccessStored;
    Property PageStartProperty : String Read FPSP Write FPSP stored IsPageStartStored;
    Property PageLimitProperty : String Read FPLP Write FPLP stored IsPageLimitStored;
    Property RowsProperty : String Read FRP Write FRP stored IsRowsStored;
    Property MessageProperty : String Read FMP Write FMP stored IsMessageStored;
    Property TotalProperty : String Read FTP Write FTP stored IsTotalStored;
  end;

Const
  // Do not localize these constants.
  DefRowsProperty      = 'rows';
  DefPageLimitProperty = 'limit';
  DefPageStartProperty = 'start';
  DefSuccessProperty   = 'success';
  DefMessageProperty   = 'message';
  DefTotalProperty     = 'total';

implementation

Resourcestring
  SErrNoAdaptor = 'No adaptor available';

{ TExtJSDataFormatter }

function TExtJSDataFormatter.IsSuccessStored: Boolean;
begin
  Result:=(SuccessProperty<>DefSuccessProperty);
end;

function TExtJSDataFormatter.IsTotalStored: boolean;
begin
  Result:=(TotalProperty<>DefTotalProperty);
end;

function TExtJSDataFormatter.IsMessageStored: boolean;
begin
  Result:=(MessageProperty<>DefMessageProperty);
end;

function TExtJSDataFormatter.IsPageLimitStored: boolean;
begin
  Result:=(PageLimitProperty<>DefPageLimitProperty);
end;

function TExtJSDataFormatter.IsPageStartStored: boolean;
begin
  Result:=(PageStartProperty<>DefPageStartProperty);
end;

function TExtJSDataFormatter.IsRowsStored: boolean;
begin
  Result:=(RowsProperty<>DefRowsProperty);
end;

constructor TExtJSDataFormatter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  RowsProperty:=DefRowsProperty;
  PageLimitProperty:=DefPageLimitProperty;
  PageStartProperty:=DefPageStartProperty;
  SuccessProperty:=DefSuccessProperty;
  MessageProperty:=DefMessageProperty;
  TotalProperty:=DefTotalProperty;
end;

procedure TExtJSDataFormatter.DoReadRecords(Stream: TStream);

Var
  I : Integer;
  L : TStrings;
  S : String;
begin

  If AllowPageSize then
    begin
    If Not Assigned(Adaptor) then
      Raise EFPHTTPError.Create(SErrNoAdaptor);
    if Adaptor.TryFieldValue(PageStartProperty,S) then
      begin
      I:=StrToIntDef(S,-1);
      If I<>-1 then
        PageStart:=I;
      end;
    if Adaptor.TryFieldValue(PageLimitProperty,S) then
      begin
      I:=StrToIntDef(S,-1);
      If I<>-1 then
        PageSize:=I;
      end;
    end;
  Inherited DoReadRecords(Stream);
end;

function TExtJSDataFormatter.ProduceContent: String;

Var
  S : TStringStream;

begin
  S:=TStringStream.Create('');
  try
    ContentToStream(S);
    Result:=S.DataString;
  finally
    FreeAndNil(S);
  end;
end;


end.

