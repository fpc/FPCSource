{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$H+}
unit dbwhtml;

Interface

uses sysutils,classes,db,whtml;

Type
  THTMLAlign = (haDefault,haLeft,haRight,haCenter); // Compatible with Delphi.
  
  TTableColumn = Class(TCollectionItem)
  private
    FActionUrl: String;
    FAlign: THTMLAlign;
    FBGColor: String;
    FCaptionURL: String;
    FFieldName : String;
    FCaption : String;
    FGetColumn: String;
    FImgUrl: String;
  Protected
    FField : TField; // Filled.
  Published
    Property FieldName : String Read FFieldName Write FFieldName;
    Property Caption : String Read FCaption Write FCaption;
    Property ImgUrl : String Read FImgUrl Write FImgUrl;
    Property ActionUrl : String Read FActionUrl Write FActionUrl;
    Property CaptionURL : String Read FCaptionURL Write FCaptionURL;
    Property BGColor : String Read FBGColor Write FBGColor;
    Property Align : THTMLAlign read FAlign Write Falign;
  end;
  
  TTableColumns = Class(TCollection)
    Constructor Create;
  end;

  THTMLProducer = Class(TComponent)
  Private
    FDataset : TDataset;
    FContents: TMemorySTream;
    Function GetContent : String;
  Protected
    Procedure CheckContents;
    Procedure WriteString(S : TStream; Const Value : String);
    Procedure WriteString(S : TStream; Const Fmt : String; Args : Array Of Const);
  Public
    Destructor Destroy; override;
    Procedure ClearContent;
    Procedure CreateContent; virtual; Abstract;
    Property Content : String Read GetContent;
  Published
    Property Dataset : TDataset Read FDataset Write FDataset;
  end;
  

  TTableProducer = Class(THTMLProducer)
  Private
    FTableColumns : TTableColumns;
    FBorder : Boolean;
    Procedure SetTableColumns(Value : TTableColumns);
  Protected
    Procedure BindColumns;
    Procedure CreateTableColumns; Virtual;
    Procedure CreateTableHeader(Stream : TStream);
    Procedure CreateHeaderCell(C : TTableColumn; Stream : TStream); virtual;
    Procedure CreateTableRow(Stream : TStream);virtual;
    Procedure StartTable(Stream : TStream); virtual;
    Procedure EndTable(Stream : TStream); virtual;
    Procedure EmitFieldCell(C : TTableColumn; Stream : TStream); virtual;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; virtual;
    Procedure Clear;  
    Procedure CreateColumns(FieldList : TStrings);
    Procedure CreateColumns(FieldList : String);
    Procedure CreateTable(Stream : TStream);
    Procedure CreateTable;
    Procedure CreateContent; override;
  Published
    Property Border : Boolean Read FBorder Write FBorder;
    Property TableColumns : TTableColumns Read FTableColumns Write SetTableColumns;
  end;
  
  TComboBoxProducer = Class(THTMLProducer)
  private
    FDatafield: String;
    FInputName: String;
    FValue: String;
    FValueField: String;
    function GetInputName: String;
  protected
    procedure CreateItem(Stream : TStream; VF,DF : TField; Selected : Boolean); virtual;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; virtual;
    Procedure CreateComboBox(Stream : TStream);
    Procedure CreateComboBox;
    Procedure CreateContent; override;
  Published
    Property ValueField : String Read FValueField Write FValueField;
    Property DataField : String Read FDatafield Write FDataField;
    Property Value : String Read FValue Write FValue;
    Property InputName : String Read GetInputName Write FInputName;
  end;

  TDBHtmlWriter = Class(THTMLWriter)
  Protected
    Function CreateTableProducer: TTableProducer; virtual;
  Public
    Procedure CreateTable(Dataset : TDataset);
    Procedure CreateTable(Dataset : TDataset; Producer : TTableProducer);
  end;
  

Implementation

{ TTableColumns }

constructor TTableColumns.Create;
begin
  inherited Create(TTableColumn);
end;

{ TTableProducer }


procedure TTableProducer.BindColumns;

Var
  I : Integer;

begin
  With FTableColumns do
    For I:=0 to Count-1 do
      With TTableColumn(Items[I]) do
        FField:=FDataset.FieldByName(FieldName);
end;

procedure TTableProducer.CreateTableColumns;
begin
  FTableColumns:=TTableColumns.Create;
end;

procedure TTableProducer.CreateTableHeader(Stream : TStream);

Var
  I : Integer;

begin
  WriteString(Stream,'<TR>');
  With FTableColumns do
    For I:=0 to Count-1 do
      CreateHeaderCell(TTableColumn(Items[I]),Stream);
  WriteString(Stream,'</TR>'#10);
end;

procedure TTableProducer.CreateHeaderCell(C: TTableColumn; Stream: TStream);

Var
  URL : String;

begin
  WriteString(Stream,'<TH>');
  With C do
    begin
    If (FCaptionURL<>'') then
      begin
      URL:=Format(FCaptionURL,[FieldName]);
      URL:=Format('<A HREF="%s">',[URL]);
      WriteString(Stream,URL);
      end;
    WriteString(Stream,Caption);
    If (FCaptionURL<>'') then
      WriteString(Stream,'</A>');
    If (FImgURL<>'') then
      begin
      if (FCaptionURL<>'') then
        WriteString(Stream,URL);
      WriteString(Stream,'<IMG SRC="%s">',[FImgURL]);
      If (FCaptionURL<>'') then
        WriteString(Stream,'</A>');
      end;
    end;
  WriteString(Stream,'</TH>');
end;

procedure TTableProducer.CreateTableRow(Stream : TStream);

Var
  I : Integer;

begin
  WriteString(Stream,'<TR>');
  With FTableColumns do
    For I:=0 to Count-1 do
      EmitFieldCell(TTableColumn(Items[I]),Stream);
  WriteString(Stream,'</TR>'#10);
end;

procedure TTableProducer.StartTable(Stream: TStream);

Var
  S : String;

begin
  S:='<TABLE';
  If Border then
    S:=S+' BORDER=1';
  S:=S+'>';
  WriteString(Stream,S);
end;

procedure TTableProducer.EndTable(Stream: TStream);
begin
  WriteString(Stream,'</TABLE>'#10);
end;

procedure TTableProducer.EmitFieldCell(C: TTableColumn; Stream: TStream);

Var
  URL : String;

begin
  WriteString(Stream,'<TD>');
  With C.FField Do
    begin
    URL:=C.ActionURL;
    If (URL<>'') then
      begin
      URL:=Format(C.ActionURL,[AsString]);
      WriteString(Stream,'<A HREF="%s">',[URL]);
      end;
    WriteString(Stream,AsString);
    If (URL<>'') then
      WriteString(Stream,'</A>');
    end;
  WriteString(Stream,'</TD>');
end;

constructor TTableProducer.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  CreateTableColumns;
end;

destructor TTableProducer.Destroy;
begin
  FTableColumns.Free;
  Inherited;
end;

procedure TTableProducer.Clear;
begin
  FTableColumns.Clear;
  If Assigned(FContents) then
    FreeAndNil(FContents);
  FBorder:=False;
  
end;

procedure TTableProducer.CreateColumns(FieldList: TStrings);

Var
  I : Integer;
  FN : String;
  
begin
  For I:=0 to FDataset.FieldCount-1 do
    begin
    FN:=FDataset.Fields[I].FieldName;
    If (FieldList=Nil) or (FieldList.IndexOf(FN)<>-1) then
      With FTableColumns.Add as TTableColumn do
        begin
        FieldName:=FN;
        Caption:=FDataset.Fields[i].DisplayName;
        end;
    end
end;

procedure TTableProducer.CreateColumns(FieldList: String);

Var
  L : TStringList;

begin
  If (FieldList='') then
    CreateColumns(Nil)
  else
    begin
    L:=TStringList.Create;
    try
      L.CommaText:=FieldList;
      CreateColumns(L);
    Finally
      L.Free;
    end;
    end;
end;

procedure TTableProducer.CreateTable(Stream: TStream);
begin
  If FTableColumns.Count=0 then
    CreateColumns(Nil);
  BindColumns;
  StartTable(Stream);
  Try
  CreateTableHeader(Stream);
  While Not Dataset.EOF do
    begin
    CreateTableRow(Stream);
    Dataset.Next;
    end;
  Finally
    EndTable(Stream);
  end;
end;

procedure TTableProducer.CreateTable;
begin
  CheckContents;
  CreateTable(FContents);
end;

procedure TTableProducer.CreateContent;
begin
  CreateTable;
end;

Procedure TTableProducer.SetTableColumns(Value : TTableColumns);

begin
  FTableColumns.Assign(Value);
end;


{ TComboBoxProducer }

function TComboBoxProducer.GetInputName: String;
begin
  If (FInputName='') then
    Result:=Name
  else
    Result:=FInputName;
end;

constructor TComboBoxProducer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TComboBoxProducer.Destroy;
begin
  Inherited;
end;

procedure TComboBoxProducer.CreateItem(Stream : TStream; VF,DF : TField; Selected : Boolean);

Const
  SOptions : Array[Boolean] of String = ('<OPTION','<OPTION SELECTED');

Var
  S : String;

begin
  WriteString(STream,SOptions[Selected]);
  If (VF<>Nil) and (VF<>DF) then
    WriteString(Stream,' VALUE="'+VF.AsString+'"');
  WriteString(Stream,'>'+DF.AsString+#10);
end;

procedure TComboBoxProducer.CreateComboBox(Stream: TStream);

Var
  VF,DF,SF : TField;

begin
  DF:=Dataset.FieldByNAme(DataField);
  if (ValueField<>'') then
    VF:=Dataset.FieldByName(ValueField)
  else
    VF:=Nil;
  If (Value='') then
    SF:=Nil
  else
    if VF<>NIl then
      SF:=VF
    else
      SF:=DF;
  WriteString(Stream,'<SELECT NAME="'+InputName+'">');
  Try
    While not Dataset.EOF do
      begin
      CreateItem(Stream,VF,DF,((SF<>Nil) and (SF.AsString=Value)));
      Dataset.Next;
      end;
  Finally
    WriteString(Stream,'</SELECT>');
  end;
end;

procedure TComboBoxProducer.CreateComboBox;
begin
  CheckContents;
  CreateComboBox(FContents);
end;

procedure TComboBoxProducer.CreateContent;
begin
  CreateComboBox;
end;

{ THTMLProceder }

function THTMLProducer.GetContent: String;

begin
  If Assigned(FContents) then
    begin
    SetLength(Result,FContents.Size);
    If (FContents.Size>0) then
      Move(FContents,Result[1],FContents.Size);
    end;
end;

procedure THTMLProducer.CheckContents;
begin
  If Assigned(FContents) then
    FContents.Clear
  else
    FContents:=TMemoryStream.Create;
end;

destructor THTMLProducer.Destroy;
begin
  If Assigned(FContents) then
    FreeAndNil(FContents);
  inherited Destroy;
end;

procedure THTMLProducer.ClearContent;
begin
  If Assigned(FContents) then
    FContents.Clear;
end;

procedure THTMLProducer.WriteString(S: TStream; const Value: String);

Var
  L : Integer;

begin
  L:=Length(Value);
  If L>0 then
    S.Write(Value[1],L);
end;

procedure THTMLProducer.WriteString(S: TStream; const Fmt: String;
  Args: array of const);
begin
  WriteString(S,Format(Fmt,Args));
end;

{ TDBHtmlWriter }

function TDBHtmlWriter.CreateTableProducer: TTableProducer;
begin
  Result:=TTableProducer.Create(Nil);
end;

procedure TDBHtmlWriter.CreateTable(Dataset: TDataset);

Var
  P : TTableProducer;

begin
  P:=CreateTableProducer;
  Try
    CreateTable(Dataset,P);
  Finally
    P.Free;
  end;
end;

procedure TDBHtmlWriter.CreateTable(Dataset: TDataset; Producer: TTableProducer);
begin
  Producer.Dataset:=Dataset;
  Producer.CreateTable(Self.Stream);
end;

end.
{
  $Log$
  Revision 1.4  2003-10-03 22:43:17  michael
  + Published tablecolumns property in tableproducer

  Revision 1.3  2003/10/03 08:42:22  michael
  + Form support.

  Revision 1.2  2003/10/01 21:07:48  michael
  + Added log/header

}
