{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    TFPReport descendent that stores it's design in a JSON structure. 
    Can be used in an IDE

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fplazreport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpreport, DOM, FPCanvas, fpTTF, fpreportdb, fpreportbarcode;

Type
  TCustomPropEvent = procedure(Sender: TObject;Data : TDOMNode) of object;
  TConvertLogEvent = Procedure(Sender: TOBject;Const Msg : String) of Object;
  TNameConvertEvent = Procedure(Sender: TOBject;Const aName : UnicodeString; Var aNewName : String) of Object;
  TFontSubstitutionEvent = Procedure(Sender: TOBject;Const aFontName : String; Const aBold,aItalic: Boolean; var aFont : TFPFontCacheItem) of Object;

  { TFPLazReport }
  TFPLazReport = class(TFPReport)
  private
    FData: TComponent;
    FMasterData: TFPReportDataBand;
    FDetailHeader : TFPReportDataHeaderBand;
    FDetailFooter : TFPReportDataFooterBand;
    FDetailBand: TFPReportDataBand;
    FMemoClass: TFPReportElementClass;
    FOnConvertName: TNameConvertEvent;
    FOnLog: TConvertLogEvent;
    FOnSetCustomProps: TCustomPropEvent;
    FOnSubstituteFont: TFontSubstitutionEvent;
    FCounter : Integer;
    FNullBand : TFPReportChildBand;
  Protected
    class function Red(rgb: Integer): BYTE; virtual;
    class function Green(rgb: Integer): BYTE; virtual;
    class function Blue(rgb: Integer): BYTE; virtual;
    function FindBand(aPage: TFPReportCustomPage; aTop: double; AElement: TFPReportElement=Nil): TFPReportCustomBand; virtual;
    class function GetProperty(aNode: TDOMNode; const aName: String; const aValue: string='Value'): UTF8String; virtual;
    function ApplyFrame(aDataNode: TDOMNode; aFrame: TFPReportFrame): Boolean; virtual;
    procedure ApplyObjectProperties(ObjNode: TDOMNode; aObj: TFPReportElement); virtual;
    procedure ConvertPageProperties(aPage: TFPReportPage; aPageNode: TDOMNode); virtual;
    procedure SetData(AValue: TComponent);virtual;
    procedure SizeToLayout(aDataNode: TDOMNode; aObj: TFPReportElement);virtual;
    Function ConvertComponentName(Const aName : UnicodeString;Const AClassName : String) : String; virtual;
    function ConvertFont(aDataNode: TDomNode): TFPFontCacheItem; virtual;
    function ConvertBand(aBandNode: TDomNode;aPage: TFPReportCustomPage): TFPReportCustomBand; virtual;
    function ConvertMemo(ObjNode: TDOMNode; aPage: TFPReportCustomPage): TFPReportMemo; virtual;
    function ConvertPage(aPageNode: TDOMNode): TFPReportPage; virtual;
    function ConvertLine(ObjNode: TDOMNode; APage: TFPReportCustomPage): TFPReportShape; virtual;
    function ConvertImage(ObjNode: TDOMNode; APage: TFPReportCustomPage): TFPReportImage; virtual;
    function ConvertBarcode(ObjNode : TDOMNode; APage : TFPReportCustomPage) : TFPReportBarcode; virtual;
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Procedure DoLog(Const Msg : String);
    Procedure DoLog(Const Fmt : String; Const Args : Array of const);
  Public
    constructor Create(AOwner: TComponent); override;
    function FixDataFields(aFieldName : string) : string;
    property MemoClass : TFPReportElementClass read FMemoClass write FmemoClass;
    Procedure LoadFromXML(LazReport : TXMLDocument);virtual;
    Procedure LoadFromFile(const aFileName : String);
  Published
    property DataContainer : TComponent read FData write SetData;
    property OnSetCustomproperties : TCustomPropEvent read FOnSetCustomProps write FOnSetCustomProps;
    Property OnLog : TConvertLogEvent Read FOnLog Write FOnLog;
    Property OnSubstituteFont : TFontSubstitutionEvent Read FOnSubstituteFont Write FOnSubstituteFont;
    Property OnConvertName : TNameConvertEvent Read FOnConvertName Write FOnConvertName;
  end;

  function MMToPixels(Const Dist: double) : Integer;
  function PixelsToMM(Const Dist: double) : TFPReportUnits;

implementation

uses dateutils, XMLRead,FPReadPNG,FPimage,FPReadGif,FPReadJPEG,fpbarcode;

Resourcestring
  SLogUnknownClass = 'Ignoring unknown lazreport class type for object "%s": "%s".';
  SErrUnknownBandType = 'Unknown band type: "%s", substituting child band';
  SErrWrongEncoding = 'Unknown image encoding at pos %d : %s';
  SFontSubstitution = 'FontSubstitution';
  SErrUnknownImageType = 'Unknown image type encountered: "%s"';
  SWarnConvertName = 'Name conversion: "%s" to "%s"';
  SUnknownBarcodeType = 'Unknown barcode type: "%s"';
  SIgnoringAngleOnBarcode = 'Igoring angle on barcode';
  SIgnoringShowTextOnBarcode = 'Igoring showtext on barcode';
  SLogNullBandAssigned = 'Null (child) band assigned for element of type "%s" at pos %5.3f';

function PixelsToMM(Const Dist: double) : TFPReportUnits;
begin
  Result:=Dist*(1/3.76);
end;
function MMToPixels(Const Dist: double) : Integer;
begin
  Result:=round(Dist*(3.76));
end;

function PageToMM(Const Dist: double) : TFPReportUnits;
begin
  Result:=Dist*(1/2.83);
end;

{ TFPLazReport }

procedure TFPLazReport.SetData(AValue: TComponent);
begin
  if FData=AValue then Exit;
  if Assigned(FData) then
    FData.RemoveFreeNotification(Self);
  FData:=AValue;
  if Assigned(FData) then
    FData.FreeNotification(Self);
end;

procedure TFPLazReport.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
    if (AComponent=FData) then
      FData:=Nil;
end;

procedure TFPLazReport.DoLog(const Msg: String);
begin
  If Assigned(FOnLog) then
    FOnLog(Self,Msg);
end;

procedure TFPLazReport.DoLog(const Fmt: String; const Args: array of const);

Var
  S : String;

begin
  try
    S:=Format(Fmt,Args);
  except
    on E : Exception do
      S:=Format('Failed to format error message "%s" with %d arguments',[Fmt,Length(Args)]);
  end;
  DoLog(S);
end;

constructor TFPLazReport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MemoClass := TFPReportMemo;
  DataContainer := Owner;
end;

function TFPLazReport.FixDataFields(aFieldName: string): string;

var
  k : Integer = 0;

begin
  Result := aFieldName;
  if Assigned(FData) then
    while k < FData.ComponentCount do
      begin
        if FData.Components[k] is TFPReportDatasetData then
          Result := StringReplace(Result,TFPReportDatasetData(FData.Components[k]).Name+'.',TFPReportDatasetData(FData.Components[k]).Name+'.',[rfReplaceAll,rfIgnoreCase]);
        inc(k);
      end;
  Result := StringReplace(Result,'PAGE#','PageNo',[rfReplaceAll,rfIgnoreCase]);
  Result := StringReplace(Result,'[DATE]','[TODAY]',[rfReplaceAll,rfIgnoreCase]);
end;

Class function TFPLazReport.Blue(rgb: Integer): BYTE;
begin
  Result := (rgb shr 16) and $000000ff;
end;

Class function TFPLazReport.Green(rgb: Integer): BYTE;
begin
  Result := (rgb shr 8) and $000000ff;
end;

Class function TFPLazReport.Red(rgb: Integer): BYTE;
begin
  Result := rgb and $000000ff;
end;

procedure TFPLazReport.LoadFromXML(LazReport: TXMLDocument);

var
  i: Integer;
  BaseNode,lPages : TDOMNode;
 aPage: TFPReportPage;

begin
  BaseNode := LazReport.DocumentElement.FindNode('LazReport');
  if not Assigned(BaseNode) then
    exit;
  lPages := BaseNode.FindNode('Pages');
  if Not Assigned(lPages) then
    exit;
  TwoPass:= GetProperty(lPages,'DoublePass') = 'True';
  with lPages.ChildNodes do
    for i := 0 to (Count - 1) do
      if (copy(Item[i].NodeName,0,4)='Page') and (Item[i].NodeName<>'PageCount') then
        begin
        aPage:=ConvertPage(Item[i]);
        AddPage(aPage);
        end;
end;

Class function TFPLazReport.GetProperty(aNode : TDOMNode;Const aName : String; Const aValue : string = 'Value') : UTF8String;

var
  bNode: TDOMNode;

begin
  Result := '';
  bNode := aNode.FindNode(aName);
  if Assigned(bNode) then
    if Assigned(bNode.Attributes.GetNamedItem(aValue)) then
      Result := UTF8Encode(bNode.Attributes.GetNamedItem(aValue).NodeValue);
end;

function TFPLazReport.FindBand(aPage : TFPReportCustomPage;aTop : double; AElement : TFPReportElement = Nil) : TFPReportCustomBand;
var
  b : Integer;
  S : String;

begin
  Result := nil;
  for b := 0 to aPage.BandCount-1 do
    begin
      if (aTop>=aPage.Bands[b].Layout.Top)
      and (aTop<=aPage.Bands[b].Layout.Top+aPage.Bands[b].Layout.Height) then
        begin
          Result := aPage.Bands[b];
          break;
        end;
    end;
  if (Result=Nil) then
    begin
    if (FNullBand=Nil) then
      begin
      FNullBand:=TFPReportChildBand.Create(Self);
      FNullBand.Name:='NullBand';
      FNullBand.Layout.Height:=aPage.Layout.Height;
      FNullBand.Parent:=aPage;
      end;
    Result:=FNullBand;
    end;
  if (Result=FNullBand) and Assigned(FOnLog) then
    begin
    if aElement = Nil then
      S:='<unknown>'
    else
      S:=aElement.ClassName;
    DoLog(SLogNullBandAssigned,[S,aTop]);
    end;
end;

Function TFPLazReport.ConvertBand(aBandNode : TDomNode;aPage: TFPReportCustomPage) : TFPReportCustomBand;

Var
  Tmp : String;
  aBand : TFPReportCustomBand;
  aData : TFPreportData;

begin
  tmp := GetProperty(aBandNode,'BandType');
  case tmp of
  'btReportTitle':
     aBand := TFPReportTitleBand.Create(Self);
  'btMasterData':
    begin
    aBand := TFPReportDataBand.Create(Self);
    tmp := GetProperty(aBandNode,'DatasetStr');
    if copy(tmp,1,1)='P' then
      tmp := copy(tmp,2,system.length(tmp));
    if Assigned(FData) then
      aData := TFPreportData(FData.FindComponent(tmp));
    if Assigned(aData) then
      TFPReportDataBand(aBand).Data := aData;
    FMasterData := TFPReportDataBand(aBand);
    end;
  'btMasterHeader':
    begin
      aBand := TFPReportDataHeaderBand.Create(Self);
    end;
  'btMasterFooter':
    begin
      aBand := TFPReportDataFooterBand.Create(Self);
    end;
  'btDetailData':
    begin
      aBand := TFPReportDataBand.Create(Self);
      tmp := GetProperty(aBandNode,'DatasetStr');
      if copy(tmp,1,1)='P' then
        tmp := copy(tmp,2,system.length(tmp));
      if Assigned(FData) and (FData.FindComponent(tmp) <> nil) then
        aData:=TFPreportData(FData.FindComponent(tmp));
      if Assigned(aData) and (ReportData.FindReportDataItem(aData)=Nil) then
        ReportData.AddReportData(aData);
      TFPReportDataBand(aBand).Data:=aData;
      TFPReportDataBand(aBand).MasterBand := FMasterData;
      FDetailBand := TFPReportDataBand(aBand);
      if Assigned(FDetailHeader) then
        begin
        FDetailHeader.Data := AData;
        FDetailHeader := nil;
        end;
      if Assigned(FDetailFooter) then
        begin
        FDetailFooter.Data:=aData;
        FDetailFooter:=nil;
        end;
    end;
  'btDetailHeader':
    begin
      aBand:=TFPReportDataHeaderBand.Create(Self);
      if Assigned(FDetailBand) then
        TFPReportDataHeaderBand(aBand).Data := FDetailBand.Data
      else
        FDetailHeader:=TFPReportDataHeaderBand(aBand);
    end;
  'btDetailFooter':
    begin
      aBand := TFPReportDataFooterBand.Create(Self);
      if Assigned(FDetailBand) then
        TFPReportDataFooterBand(aBand).Data := FDetailBand.Data
      else
        FDetailFooter := TFPReportDataFooterBand(aBand);
    end;
  'btPageHeader':
     aBand := TFPReportPageHeaderBand.Create(Self);
  'btPageFooter':
     aBand := TFPReportPageFooterBand.Create(Self);
  'btGroupHeader':
    begin
      aBand:=TFPReportGroupHeaderBand.Create(Self);
      tmp := GetProperty(aBandNode,'Condition');
      if copy(tmp,0,1)='[' then
        tmp := copy(tmp,2,system.length(tmp)-2);//remove []
      tmp := FixDataFields(tmp);
      TFPReportGroupHeaderBand(aBand).GroupCondition:=tmp;
    end;
  'btGroupFooter':
    aBand := TFPReportGroupFooterBand.Create(Self);
  else
    begin
    DoLog(SErrUnknownBandType,[Tmp]);
    aBand := TFPReportChildBand.Create(Self);
    end;
  end;
  if Assigned(aBand) then
    begin
    TFPReportDataBand(aBand).StretchMode:=smActualHeight;
    aBand.Parent:=aPage;
    end;
  Result:=aBand;
end;

Function TFPLazReport.ConvertFont(aDataNode : TDomNode) : TFPFontCacheItem;

Var
  i : Integer;
  FontFound, aBold, aItalic : Boolean;
  aFont : TFPFontCacheItem;
  RealFont,FontName : String;

begin
  aBold := pos('fsBold',GetProperty(aDataNode,'Style'))>0;
  aItalic := pos('fsItalic',GetProperty(aDataNode,'Style'))>0;
  FontName:=GetProperty(aDataNode,'Name');
  aFont := gTTFontCache.Find(FontName,aBold,aItalic);
  FontFound := not Assigned(aFont);
  if not Assigned(aFont) then
    aFont := gTTFontCache.Find('LiberationSans',aBold,aItalic);
  if not Assigned(aFont) then
    aFont := gTTFontCache.Find('Arial',aBold,aItalic);
  if not Assigned(aFont) then
    aFont := gTTFontCache.Find('DejaVu',aBold,aItalic);
  with gTTFontCache do
    begin
    i:=0;
    While (aFont=Nil) and (i<Count) do
      begin
      aFont := Items[i];
      if Not ((pos('sans',lowercase(aFont.FamilyName)) > 0) and (aFont.IsItalic = AItalic)
          and (aFont.IsBold = ABold)) then
         aFont:=nil;
      Inc(i);
      end;
    end;
  if Not FontFound then
    begin
    // Allow user to override
    If Assigned(FOnSubstituteFont) then
      FOnSubstituteFont(Self,FontName,aBold,aItalic,aFont);
    // Log it
    if Assigned(FOnLog) then
      begin
      if Assigned(aFont) then
        RealFont:=aFont.FamilyName
      else
        RealFont:='<nil>';
      if aBold then
        RealFont:=RealFont+'[Bold]';
      if aItalic then
        RealFont:=RealFont+'[Italic]';
      DoLog(SFontSubstitution,[FOntName,RealFont]);
      end;
    end;
  Result:=aFont;
end;

Function TFPLazReport.ConvertMemo(ObjNode : TDOMNode;aPage : TFPReportCustomPage) : TFPReportMemo;

Var
  aDataNode: TDOMNode;
  aBand: TFPReportCustomBand;
  aColor,aSize,aFlag : Integer;
  aFont: TFPFontCacheItem;

begin
  aDataNode := ObjNode.FindNode('Size');
  aBand := FindBand(aPage,PixelsToMM(StrToFloatDef(GetProperty(aDataNode,'Top'),0)));
  Result := MemoClass.Create(Self) as TFPReportMemo;
  Result.Parent:=aBand;
  aDataNode := ObjNode.FindNode('Data');
  if Assigned(FOnSetCustomProps) then
    FOnSetCustomProps(Result,aDataNode);
  aDataNode := ObjNode.FindNode('Size');
  case GetProperty(ObjNode,'Alignment') of
    'taRightJustify': Result.TextAlignment.Horizontal:=taRightJustified;
    'taCenter':       Result.TextAlignment.Horizontal:=taCentered;
  end;
  case GetProperty(ObjNode,'Layout') of
    'tlCenter': Result.TextAlignment.Vertical:=tlCenter;
    'tlTop':    Result.TextAlignment.Vertical:=tlTop;
    'tlBottom': Result.TextAlignment.Vertical:=tlBottom;
  end;
  Result.StretchMode:=smActualHeight;
  aFlag := StrToIntDef(GetProperty(ObjNode,'Flags'),0);
  if aFlag and 3 = 3 then
    Result.StretchMode:=smMaxHeight;
  Result.TextAlignment.TopMargin:=1;
  aDataNode := ObjNode.FindNode('Data');
  Result.Text:=FixDataFields(GetProperty(aDataNode,'Memo'));
  Result.UseParentFont := False;
  aDataNode := ObjNode.FindNode('Font');
  if Assigned(aDataNode) then
    aFont:=ConvertFont(aDataNode);
  if Assigned(aFont) then
    Result.Font.Name:=aFont.PostScriptName
  else
    Result.UseParentFont := true;
  aSize := StrToIntDef(GetProperty(aDataNode,'Size'),Result.Font.Size);
  if aSize>5 then
    dec(aSize);
  Result.Font.Size:=aSize;
  aColor := StrToIntDef(GetProperty(aDataNode,'Color'),0);
  Result.Font.Color:= RGBToReportColor(Red(aColor),Green(aColor),Blue(aColor));
end;

Function TFPLazReport.ConvertLine(ObjNode : TDOMNode; APage : TFPReportCustomPage) : TFPReportShape;

Var
  aDataNode: TDOMNode;
  aBand: TFPReportCustomBand;

begin
  aDataNode := ObjNode.FindNode('Size');
  aBand := FindBand(aPage,PixelsToMM(StrToFloatDef(GetProperty(aDataNode,'Top'),0)));
  Result := TFPReportShape.Create(Self);
  Result.Parent:=aBand;
  Result.ShapeType:=stLine;
  Result.Orientation:=orEast;
end;

Function TFPLazReport.ConvertImage(ObjNode : TDOMNode; APage : TFPReportCustomPage) : TFPReportImage;

Var
  aDataNode: TDOMNode;
  aBand: TFPReportCustomBand;
  tmp,e : String;
  SS: TStream;
  aReaderClass : TFPCustomImageReaderClass;
  B : Byte;
  I,CD : Integer;

begin
  aDataNode := ObjNode.FindNode('Size');
  aBand := FindBand(aPage,PixelsToMM(StrToFloatDef(GetProperty(aDataNode,'Top'),0)));
  Result := TFPReportImage.Create(aBand);
  aDataNode := ObjNode.FindNode('Picture');
  aReaderClass:=nil;
  tmp:=lowercase(GetProperty(aDataNode,'Type','Ext'));
  case tmp of
   'jpeg','jpg': aReaderClass := TFPReaderJPEG;
   'png': aReaderClass := TFPReaderPNG;
   'gif': aReaderClass := TFPReaderGif;
  end;
  if Not Assigned(aReaderClass) then
    begin
    DoLog(SErrUnknownImageType,[tmp]);
    exit;
    end;
  tmp:=GetProperty(aDataNode,'Data');
  if Tmp='' then
    Exit;
  ss:=TStringStream.Create('');
  try
    for i:=1 to (system.length(tmp) div 2) do
      begin
      e:=tmp[i*2-1]+tmp[i*2];
      Val('$'+E, B, cd);
      if cd<>0 then
        DoLog(SErrWrongEncoding,[i*2-1,E]);
      ss.Write(B, 1);
      end;
    ss.Position:=0;
    Result.LoadFromStream(ss,aReaderClass);
    Result.Stretched:=True;
  Finally
    ss.Free;
  end;
end;

function TFPLazReport.ConvertBarcode(ObjNode: TDOMNode; APage: TFPReportCustomPage): TFPReportBarcode;

  Function StringToEncoding (s : String): TBarcodeEncoding;

  begin
    Case s of
      'bcCode39' : Result:=be39;
      'bcCode93' : Result:=be93;
      'bcCodeCodabar' : Result:=beCodabar;
      'bcCode39Extended' : Result:=be39Extended;
      'bcCode128A' : Result:=be128A;
      'bcCode128B' : Result:=be128B;
      'bcCode128C' : Result:=be128C;
      'bcCodeEAN13' : Result:=beEAN13;
      'bcCodeEAN8' : Result:=beEAN8;
      'bcCode_2_5_interleaved' : Result:=be2of5interleaved;
      'bcCodeMSI' : Result:=beMSI;
    else
      DoLog(SUnknownBarcodeType,[s]);
    end;
  end;


Var
  aDataNode : TDomNode;
  cd : integer;
  D :double;

begin
  Result:=TFPReportBarcode.Create(Self);
  aDataNode:=ObjNode.FindNode('Size');
  Result.Parent:=FindBand(APage,PixelsToMM(StrToFloatDef(GetProperty(aDataNode,'Top'),0)));
  Result.Encoding:=StringToEncoding(GetProperty(ObjNode,'BarCode','BarType'));
  if GetProperty(ObjNode,'BarCode','Angle')<>'0' then
    DoLog(SIgnoringAngleOnBarcode);
  if GetProperty(ObjNode,'BarCode','ShowText')<>'0' then
    DoLog(SIgnoringShowTextOnBarcode);
  val(GetProperty(ObjNode,'BarCode','Zoom'),D,CD);
  if CD=0 then
    Result.Weight:=D
  else
    Result.Weight:=1;
  aDataNode:=ObjNode.FindNode('Data');
  if ADataNode<>Nil then
    Result.Expression:=GetProperty(aDataNode,'Memo');
end;

Procedure TFPLazReport.SizeToLayout(aDataNode : TDOMNode; aObj: TFPReportElement);

Var
  OffsetTop: TFPReportUnits;
  OffsetLeft: TFPReportUnits;

begin
  if Assigned(aObj.Band) then
    OffsetTop := aObj.Band.Layout.Top
  else
    OffsetTop := 0;
  OffsetLeft :=0;
  if not (aObj is TFPReportCustomBand) then
    if Assigned(aObj.Page) then
      OffsetLeft := aObj.Page.Margins.Left;
  With aObj.Layout do
    begin
    Top:=PixelsToMM(StrToFloatDef(GetProperty(aDataNode,'Top'),Top))-OffsetTop;
    Left:=PixelsToMM(StrToFloatDef(GetProperty(aDataNode,'Left'),Left))-OffsetLeft;
    Width:=PixelsToMM(StrToFloatDef(GetProperty(aDataNode,'Width'),Width));
    Height:=PixelsToMM(StrToFloatDef(GetProperty(aDataNode,'Height'),Height));
    end;
end;

function TFPLazReport.ConvertComponentName(const aName: UnicodeString; const AClassName: String): String;
begin
  if IsValidIdent(aName) then
    Result:=aName
  else
    begin
    Repeat
      Inc(FCounter);
      Result:=aClassName+IntToStr(FCounter);
    Until FindComponent(Result)=Nil;
    if Assigned(FOnConvertName) then
      FOnConvertName(Self,aName,Result);
    DoLog(SWarnConvertName,[aName,Result]);
    end;


end;

Function TFPLazReport.ApplyFrame(aDataNode : TDOMNode; aFrame: TFPReportFrame) : Boolean;

Var
  tmp : String;
  aColor : Integer;

begin
  Result:=False;
  aFrame.Shape:=fsNone;
  if GetProperty(aDataNode,'FrameColor')<>'' then
    begin
    aColor := StrToIntDef(GetProperty(aDataNode,'FrameColor'),0);
    aFrame.Color:= RGBToReportColor(Red(aColor),Green(aColor),Blue(aColor));
    end;
  aFrame.Width := Round(StrToIntDef(GetProperty(aDataNode,'FrameWidth'),0)/2);
  aFrame.Lines:=[];
  tmp := GetProperty(aDataNode,'FrameBorders');
  if tmp = '' then
    exit;
  if pos('frbBottom',tmp)>0 then
    aFrame.Lines := aFrame.Lines+[flBottom];
  if pos('frbTop',tmp)>0 then
    aFrame.Lines := aFrame.Lines+[flTop];
  if pos('frbLeft',tmp)>0 then
    aFrame.Lines := aFrame.Lines+[flLeft];
  if pos('frbRight',tmp)>0 then
    aFrame.Lines := aFrame.Lines+[flRight];
  Result:=aFrame.Lines<>[];
end;

Procedure TFPLazReport.ApplyObjectProperties(ObjNode : TDOMNode; aObj: TFPReportElement);

Var
  HasFrame : Boolean;
  FC: String;
  M : TFPReportMemo;
  aDataNode : TDOMNode;
  aColor : Integer;

begin
  aObj.Name:=ConvertComponentName(GetProperty(ObjNode,'Name'),aObj.ClassName);
  aDataNode := ObjNode.FindNode('Size');
  if Assigned(aDataNode) then
    SizeToLayout(aDataNode,aObj);
  HasFrame:=False;
  aDataNode := ObjNode.FindNode('Frames');
  if Assigned(aDataNode) then
    hasFrame:=ApplyFrame(aDataNode,aObj.Frame);
  if Not (aObj is TFPReportMemo) then
    exit;
  FC:=GetProperty(ObjNode,'FillColor');
  if (FC='clNone') or (FC='') then
    exit;
  M:=TFPReportMemo(aObj);
  aColor := StrToIntDef(FC,0);
  M.Frame.Pen:=psClear;
  M.Frame.BackgroundColor:= RGBToReportColor(Red(aColor),Green(aColor),Blue(aColor));
  M.Frame.Shape:=fsRectangle;
  if not HasFrame then
    begin
    M.Frame.Color:=RGBToReportColor(Red(aColor),Green(aColor),Blue(aColor));
    M.Frame.Pen:=psClear;
    end;
end;

procedure TFPLazReport.ConvertPageProperties(aPage: TFPReportPage; aPageNode: TDOMNode);

Var
  aDataNode: TDOMNode;

begin
  aPage.PageSize.PaperName:='A4';
  aPage.Font.Name:='ArialMT';
  if GetProperty(aPageNode,'Width')<>'' then
    aPage.PageSize.Width := round(PageToMM(StrToFloatDef(GetProperty(aPageNode,'Width'),aPage.PageSize.Width)));
  if GetProperty(aPageNode,'Height')<>'' then
    aPage.PageSize.Height := round(PageToMM(StrToFloatDef(GetProperty(aPageNode,'Height'),aPage.PageSize.Width)));
  if GetProperty(aPageNode,'Orientation') = 'poLandscape' then
    aPage.Orientation:=poLandscape;
  aDataNode := aPageNode.FindNode('Margins');
  if Assigned(aDataNode) then
    begin
    aPage.Margins.Top:=PixelsToMM(StrToFloatDef(GetProperty(aDataNode,'Top'),aPage.Margins.Top));
    aPage.Margins.Left:=PixelsToMM(StrToFloatDef(GetProperty(aDataNode,'left'),aPage.Margins.Left));
    aPage.Margins.Right:=PixelsToMM(StrToFloatDef(GetProperty(aDataNode,'Right'),aPage.Margins.Right));
    aPage.Margins.Bottom:=PixelsToMM(StrToFloatDef(GetProperty(aDataNode,'Bottom'),aPage.Margins.Bottom));
    end;
end;

Function TFPLazReport.ConvertPage(aPageNode : TDOMNode) : TFPReportPage;

var
  aPage: TFPReportPage;
  ObjNode : TDOMNode;
  aObj: TFPReportElement;
  J : Integer;
  NodeName,CT : String;

begin
  FMasterData := nil;
  FDetailBand := nil;
  FDetailHeader := nil;
  FDetailFooter := nil;
  aPage := TFPReportPage.Create(Self);
  Result:=aPage;
  ConvertPageProperties(aPage,aPageNode);
  for j := 0 to aPageNode.ChildNodes.Count-1 do
    begin
    ObjNode:=aPageNode.ChildNodes.Item[j];
    NodeName:=ObjNode.NodeName;
    if (copy(NodeName,0,6)='Object') and (NodeName<>'ObjectCount') then
      begin
      CT:=GetProperty(ObjNode,'ClassName');
      case CT of
      'TfrBandView':
        aObj:=ConvertBand(ObjNode,aPage);
      'TfrMemoView':
        aObj:=ConvertMemo(ObjNode,aPage);
      'TfrLineView':
        aObj:=ConvertLine(ObjNode,aPage);
      'TfrPictureView':
        aObj:=ConvertImage(ObjNode,aPage);
      'TfrBarCodeView':
        aObj:=ConvertBarcode(ObjNode,aPage);
      else
        DoLog(SLogUnknownClass,[NodeName,CT]);
        aObj:=Nil;
      end;
      if Assigned(aObj) then
        ApplyObjectProperties(ObjNode,aObj);
      end;
    end;
end;


procedure TFPLazReport.LoadFromFile(const aFileName: String);

var
  LazReport: TXMLDocument;

begin
  ReadXMLFile(LazReport, aFileName);
  try
    LoadFromXML(LazReport);
  finally
    LazReport.Free;
  end;
end;

end.

