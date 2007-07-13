{$mode objfpc}
{$h+}
unit inicol;

interface

Uses SysUtils,Classes,Inifiles;

Type

  { TIniCollectionItem }

  TIniCollectionItem = Class(TCollectionItem)
  protected
    function GetSectionName: String; virtual; abstract;
    procedure SetSectionName(const Value: String); virtual; abstract;
  Public
    Procedure SaveToIni(Ini: TCustomInifile; Section : String); Virtual; Abstract;
    Procedure LoadFromIni(Ini: TCustomInifile; Section : String); Virtual; Abstract;
    Procedure SaveToFile(FileName : String; Section : String);
    Procedure LoadFromFile(FileName : String; Section : String);
    Property SectionName : String Read GetSectionName Write SetSectionName;
  end;

  TIniCollection = Class(TCollection)
  private
    FFileName: String;
    FGlobalSection: String;
  protected
    FPrefix: String;  // Descendent must set this.
    FSectionPrefix : String;  // Descendent must set this too.
  Public
    Procedure Load;
    Procedure Save;
    Procedure SaveToIni(Ini: TCustomInifile; Section : String); virtual;
    Procedure SaveToFile(AFileName : String; Section : String);
    Procedure LoadFromIni(Ini: TCustomInifile; Section : String); virtual;
    Procedure LoadFromFile(AFileName : String; Section : String);
    Property Prefix : String Read FPrefix;
    Property SectionPrefix : String Read FSectionPrefix;
    Property FileName : String Read FFileName Write FFileName;
    Property GlobalSection : String Read FGlobalSection Write FGlobalSection;
  end;

  { TNamedIniCollectionItem }

  TNamedIniCollectionItem = Class(TIniCollectionItem)
  private
    procedure SetName(const AValue: String);
  Protected
    FName : String;
    FUserData : TObject;
  Protected
    Procedure SetCollection(Value : TCollection); override;
    function GetSectionName: String; override;
    procedure SetSectionName(const Value: String); override;
  Public
    Property UserData : TObject Read FUserData Write FUserData;
  Published
    Property Name : String Read FName Write SetName;
  end;

  { TNamedIniCollection }
  TNamedIniCollection = Class(TIniCollection)
  private
    function GetNamedItem(Index: Integer): TNamedIniCollectionItem;
    procedure SetNamedItem(Index: Integer; const AValue: TNamedIniCollectionItem);
  Public
    Function IndexOfUserData(UserData : TObject) : Integer;
    Function IndexOfName(Const AName : String) : Integer;
    Function FindByName(Const AName : string) : TNamedIniCollectionItem;
    Function FindByUserData(UserData : TObject) : TNamedIniCollectionItem;
    Property NamedItems [Index: Integer] : TNamedIniCollectionItem Read GetNamedItem Write SetNamedItem; default;
  end;

  EIniCol = Class(Exception);

Const
  KeyCount = 'Count';
  SGlobal  = 'Global';

implementation

{ TIniCollectionItem }

resourcestring
  SErrNoFileName = '%s: No filename specified.';
  SErrNoSection = '%s: No [global] section specified.';
  SErrDuplicateName = 'Duplicate names "%s" not allowed in collection';
  
procedure TIniCollectionItem.LoadFromFile(FileName, Section: String);

Var
  Ini : TMemInifile;

begin
  Ini:=TMemInifile.Create(FileName);
  Try
    LoadFromIni(Ini,Section);
  Finally
    Ini.Free;
  end;
end;

procedure TIniCollectionItem.SaveToFile(FileName, Section: String);

Var
  Ini : TMemInifile;

begin
  Ini:=TMemInifile.Create(FileName);
  Try
    SaveToIni(Ini,Section);
    Ini.UpdateFile;
  Finally
    Ini.Free;
  end;
end;


{ TIniCollection }

procedure TIniCollection.Load;
begin
  If (FFileName='') then
    Raise EIniCol.CreateFmt(SErrNoFileName,[ClassName]);
  If (GlobalSection='') then
    Raise EIniCol.CreateFmt(SErrNoSection,[ClassName]);
  LoadFromFile(FFileName,GlobalSection)
end;

procedure TIniCollection.LoadFromFile(AFileName, Section: String);

Var
  Ini  : TMemIniFile;

begin
  Ini:=TMemInifile.Create(AFileName);
  Try
    LoadFromIni(Ini,Section);
    FFileName:=AFileName;
    FGlobalSection:=Section;
  Finally
    ini.Free;
  end;
end;

procedure TIniCollection.LoadFromIni(Ini: TCustomInifile; Section: String);

Var
  ACount,I : Integer;
  N,SP : String;

begin
  Clear;
  SP:=FSectionPrefix;
  If (SP<>'') then
    SP:=SP+'_';
  ACount:=Ini.ReadInteger(Section,KeyCount,0);
  For I:=1 to ACount do
    begin
    N:=Ini.ReadString(Section,Prefix+IntToStr(I),'');
    If (N<>'') then
      With Add as TIniCollectionItem do
        begin
        SectionName:=N;
        LoadFromIni(Ini,SP+N);
        end;
    end;
end;

procedure TIniCollection.Save;
begin
  If (FFileName='') then
    Raise EIniCol.CreateFmt(SErrNoFileName,[ClassName]);
  If (GlobalSection='') then
    Raise EIniCol.CreateFmt(SErrNoSection,[ClassName]);
  SaveToFile(FFileName,GlobalSection)
end;

procedure TIniCollection.SaveToFile(AFileName, Section: String);

Var
  Ini : TMemIniFile;

begin
  Ini:=TMemInifile.Create(AFileName);
  Try
    Ini.CacheUpdates:=True;
    SaveToIni(Ini,Section);
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

procedure TIniCollection.SaveToIni(Ini: TCustomInifile; Section: String);

Var
  S,V,SP : String;
  I   : Integer;
  CI  : TIniCollectionItem;

begin
  SP:=FSectionPrefix;
  if (SP<>'') then
    SP:=SP+'_';
  Ini.WriteInteger(Section,KeyCount,Count);
  For I:=0 to Count-1 do
    begin
    CI:=(Items[i]) as TIniCollectionItem;
    With CI do
      begin
      V:=SectionName;
      S:=Prefix+IntToStr(I+1);
      Ini.WriteString(Section,S,V);
      CI.SaveToIni(Ini,SP+V);
      end;
    end;
end;

{ ---------------------------------------------------------------------
  TNamedIniCollectionItem
  ---------------------------------------------------------------------}

procedure TNamedIniCollectionItem.SetName(const AValue: String);
begin
  If (CompareText(AValue,FName)<>0) then
    begin
    If (AValue<>'') and (Collection<>Nil) and (Collection is TNamedIniCollection) then
      If TNamedIniCollection(Collection).IndexOfName(AValue)<>-1 then
        Raise EIniCol.CreateFmt(SErrDuplicateName,[AValue]);
    end;
  FName:=AValue;
end;

procedure TNamedIniCollectionItem.SetCollection(Value: TCollection);
begin
  If (Value<>Collection) then
    begin
    If (Value<>Nil) and (Value is TNamedIniCollection) Then
      If TNamedIniCollection(Value).IndexOfName(Self.Name)<>-1 then
        Raise EIniCol.CreateFmt(SErrDuplicateName,[Self.Name]);
    end;
  inherited SetCollection(Value);
end;

function TNamedIniCollectionItem.GetSectionName: String;
begin
  Result:=FName;
end;

procedure TNamedIniCollectionItem.SetSectionName(const Value: String);
begin
  FName:=Value; // Skip check. Ini files have only 1 named section
end;

{ ---------------------------------------------------------------------
  TNamedIniCollection
  ---------------------------------------------------------------------}

function TNamedIniCollection.GetNamedItem(Index: Integer): TNamedIniCollectionItem;
begin
  Result:=Items[Index] as TNamedIniCollectionItem;
end;

procedure TNamedIniCollection.SetNamedItem(Index: Integer; const AValue: TNamedIniCollectionItem);
begin
  Items[Index]:=AValue;
end;

function TNamedIniCollection.IndexOfUserData(UserData: TObject): Integer;
begin
  If (UserData=Nil) then
    Result:=-1
  else
    begin
    Result:=Count-1;
    While (Result>=0) and (GetNamedItem(Result).UserData<>UserData) do
      Dec(Result);
    end;
end;

function TNamedIniCollection.IndexOfName(const AName: String): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and (CompareText(GetNamedItem(Result).Name,AName)<>0) do
    Dec(Result);
end;

function TNamedIniCollection.FindByName(const AName : string): TNamedIniCollectionItem;

Var
  I : Integer;

begin
  I:=IndexOfName(AName);
  If (I=-1) then
    Result:=Nil
  else
    Result:=GetNamedItem(I);
end;

function TNamedIniCollection.FindByUserData(UserData: TObject): TNamedIniCollectionItem;

Var
  I : Integer;

begin
  I:=IndexOfUserData(UserData);
  If (I=-1) then
    Result:=Nil
  else
    Result:=GetNamedItem(I);
end;

end.

