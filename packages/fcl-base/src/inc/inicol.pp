{$mode objfpc}
{$h+}
unit inicol;

interface

Uses SysUtils,Classes,Inifiles;

Type

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

  EIniCol = Class(Exception);

Const
  KeyCount = 'Count';
  SGlobal  = 'Global';

implementation

{ TIniCollectionItem }

resourcestring
  SErrNoFileName = '%s: Geen bestandsnaam gespecifieerd.';
  SErrNoSection = '%s: Geen [global] sectie gespecifieerd.';

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

end.

