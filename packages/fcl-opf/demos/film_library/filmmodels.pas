unit filmmodels;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, contnrs, db, typinfo,
  Opf.Manager, Opf.Fields, Opf.Persister.Intf;

type
  TMediaFormat = (mfDVD, mfBluRay);
  TMediaFormats = set of TMediaFormat;

  { TFilm }

  TFilm = class(TObject, IOPFieldProvider, IOPPersistenceState)
  private
    FID: Integer;
    FTitle: String;
    FIMDBId: String;
    FIMDBIdStored: Boolean;
    FYear: Integer;
    FSynopsis: String;
    FSynopsisStored: Boolean;
    FRating: Integer;
    FRatingStored: Boolean;
    FMediaFormat: TMediaFormats;
    FDirectorList: TObjectList;
    FActorList: TObjectList;
    FGenreList: TObjectList;
    FLoanList: TObjectList;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    constructor Create;
    destructor Destroy; override;
    { IOPFieldProvider }
    procedure GetFieldDefs(aMap: TOPFieldMapList);
    function ApplyField(aFieldMap: TOPFieldMap; aField: TField): Boolean;
    function PersistField(aFieldMap: TOPFieldMap; aParam: TParam): Boolean;
  published
    property ID: Integer read FID write FID;
    property Title: String read FTitle write FTitle;
    property IMDBId: String read FIMDBId write FIMDBId;
    property IMDBIdStored: Boolean read FIMDBIdStored write FIMDBIdStored;
    property Year: Integer read FYear write FYear;
    property Synopsis: String read FSynopsis write FSynopsis;
    property SynopsisStored: Boolean read FSynopsisStored write FSynopsisStored;
    property Rating: Integer read FRating write FRating;
    property RatingStored: Boolean read FRatingStored write FRatingStored;
    property MediaFormat: TMediaFormats read FMediaFormat write FMediaFormat;
    property DirectorList: TObjectList read FDirectorList;
    property ActorList: TObjectList read FActorList;
    property GenreList: TObjectList read FGenreList;
    property LoanList: TObjectList read FLoanList;
  end;

  { TDirector }

  TDirector = class(TObject, IOPFieldProvider, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FCountry: String;
    FCountryStored: Boolean;
    FFilmList: TObjectList;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    constructor Create;
    destructor Destroy; override;
    { IOPFieldProvider }
    procedure GetFieldDefs(aMap: TOPFieldMapList);
    function ApplyField(aFieldMap: TOPFieldMap; aField: TField): Boolean;
    function PersistField(aFieldMap: TOPFieldMap; aParam: TParam): Boolean;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Country: String read FCountry write FCountry;
    property CountryStored: Boolean read FCountryStored write FCountryStored;
    property FilmList: TObjectList read FFilmList;
  end;

  { TActor }

  TActor = class(TObject, IOPFieldProvider, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FBirthYear: Integer;
    FBirthYearStored: Boolean;
    FFilmList: TObjectList;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    constructor Create;
    destructor Destroy; override;
    { IOPFieldProvider }
    procedure GetFieldDefs(aMap: TOPFieldMapList);
    function ApplyField(aFieldMap: TOPFieldMap; aField: TField): Boolean;
    function PersistField(aFieldMap: TOPFieldMap; aParam: TParam): Boolean;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property BirthYear: Integer read FBirthYear write FBirthYear;
    property BirthYearStored: Boolean read FBirthYearStored write FBirthYearStored;
    property FilmList: TObjectList read FFilmList;
  end;

  { TGenre }

  TGenre = class(TObject, IOPFieldProvider, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    { IOPFieldProvider }
    procedure GetFieldDefs(aMap: TOPFieldMapList);
    function ApplyField(aFieldMap: TOPFieldMap; aField: TField): Boolean;
    function PersistField(aFieldMap: TOPFieldMap; aParam: TParam): Boolean;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

  { TLoan }

  TLoan = class(TObject, IOPFieldProvider, IOPPersistenceState)
  private
    FID: Integer;
    FBorrowerName: String;
    FBorrowDate: String;
    FReturnDate: String;
    FReturnDateStored: Boolean;
    FFilmID: Integer;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    { IOPFieldProvider }
    procedure GetFieldDefs(aMap: TOPFieldMapList);
    function ApplyField(aFieldMap: TOPFieldMap; aField: TField): Boolean;
    function PersistField(aFieldMap: TOPFieldMap; aParam: TParam): Boolean;
  published
    property ID: Integer read FID write FID;
    property BorrowerName: String read FBorrowerName write FBorrowerName;
    property BorrowDate: String read FBorrowDate write FBorrowDate;
    property ReturnDate: String read FReturnDate write FReturnDate;
    property ReturnDateStored: Boolean read FReturnDateStored write FReturnDateStored;
    property FilmID: Integer read FFilmID write FFilmID;
  end;

  { TReportRow - for LoadSQL aggregate report queries }

  TReportRow = class(TObject, IOPFieldProvider, IOPPersistenceState)
  private
    FName: String;
    FFilmCount: Integer;
    FFilmTitles: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    { IOPFieldProvider }
    procedure GetFieldDefs(aMap: TOPFieldMapList);
    function ApplyField(aFieldMap: TOPFieldMap; aField: TField): Boolean;
    function PersistField(aFieldMap: TOPFieldMap; aParam: TParam): Boolean;
  published
    property Name: String read FName write FName;
    property FilmCount: Integer read FFilmCount write FFilmCount;
    property FilmTitles: String read FFilmTitles write FFilmTitles;
  end;

implementation

{ ========== TFilm ========== }

function TFilm.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TFilm.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

constructor TFilm.Create;
begin
  inherited Create;
  FDirectorList := TObjectList.Create(True);
  FActorList := TObjectList.Create(True);
  FGenreList := TObjectList.Create(True);
  FLoanList := TObjectList.Create(True);
end;

destructor TFilm.Destroy;
begin
  FreeAndNil(FDirectorList);
  FreeAndNil(FActorList);
  FreeAndNil(FGenreList);
  FreeAndNil(FLoanList);
  inherited Destroy;
end;

procedure TFilm.GetFieldDefs(aMap: TOPFieldMapList);
var
  F: TOPFieldMap;
begin
  F := aMap.AddField('ID', 'ID');
  F.Options := [foInKey, foAutoInc, foInSelect];
  F.PropertyTypeInfo := TypeInfo(Integer);

  F := aMap.AddField('Title', 'Title');
  F.PropertyTypeInfo := TypeInfo(String);

  F := aMap.AddField('IMDBId', 'IMDBId');
  F.Options := F.Options + [foNullable];
  F.PropertyTypeInfo := TypeInfo(String);

  F := aMap.AddField('Year', 'Year');
  F.PropertyTypeInfo := TypeInfo(Integer);

  F := aMap.AddField('Synopsis', 'Synopsis');
  F.Options := F.Options + [foNullable];
  F.PropertyTypeInfo := TypeInfo(String);

  F := aMap.AddField('Rating', 'Rating');
  F.Options := F.Options + [foNullable];
  F.PropertyTypeInfo := TypeInfo(Integer);

  F := aMap.AddField('MediaFormat', 'MediaFormat');
  F.PropertyTypeInfo := TypeInfo(TMediaFormats);
end;

function TFilm.ApplyField(aFieldMap: TOPFieldMap; aField: TField): Boolean;
begin
  Result := True;
  if aField.IsNull then
  begin
    if aFieldMap.PropertyName = 'ID' then FID := 0
    else if aFieldMap.PropertyName = 'Title' then FTitle := ''
    else if aFieldMap.PropertyName = 'IMDBId' then begin FIMDBId := ''; FIMDBIdStored := False; end
    else if aFieldMap.PropertyName = 'Year' then FYear := 0
    else if aFieldMap.PropertyName = 'Synopsis' then begin FSynopsis := ''; FSynopsisStored := False; end
    else if aFieldMap.PropertyName = 'Rating' then begin FRating := 0; FRatingStored := False; end
    else if aFieldMap.PropertyName = 'MediaFormat' then FMediaFormat := []
    else Result := False;
    Exit;
  end;
  if aFieldMap.PropertyName = 'ID' then FID := aField.AsInteger
  else if aFieldMap.PropertyName = 'Title' then FTitle := aField.AsString
  else if aFieldMap.PropertyName = 'IMDBId' then begin FIMDBId := aField.AsString; FIMDBIdStored := True; end
  else if aFieldMap.PropertyName = 'Year' then FYear := aField.AsInteger
  else if aFieldMap.PropertyName = 'Synopsis' then begin FSynopsis := aField.AsString; FSynopsisStored := True; end
  else if aFieldMap.PropertyName = 'Rating' then begin FRating := aField.AsInteger; FRatingStored := True; end
  else if aFieldMap.PropertyName = 'MediaFormat' then FMediaFormat := TMediaFormats(aField.AsInteger)
  else Result := False;
end;

function TFilm.PersistField(aFieldMap: TOPFieldMap; aParam: TParam): Boolean;
begin
  Result := True;
  if aFieldMap.PropertyName = 'ID' then aParam.AsInteger := FID
  else if aFieldMap.PropertyName = 'Title' then aParam.AsString := FTitle
  else if aFieldMap.PropertyName = 'IMDBId' then begin
    if not FIMDBIdStored then aParam.Clear else aParam.AsString := FIMDBId;
  end
  else if aFieldMap.PropertyName = 'Year' then aParam.AsInteger := FYear
  else if aFieldMap.PropertyName = 'Synopsis' then begin
    if not FSynopsisStored then aParam.Clear else aParam.AsString := FSynopsis;
  end
  else if aFieldMap.PropertyName = 'Rating' then begin
    if not FRatingStored then aParam.Clear else aParam.AsInteger := FRating;
  end
  else if aFieldMap.PropertyName = 'MediaFormat' then aParam.AsInteger := Integer(FMediaFormat)
  else Result := False;
end;

{ ========== TDirector ========== }

function TDirector.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TDirector.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

constructor TDirector.Create;
begin
  inherited Create;
  FFilmList := TObjectList.Create(True);
end;

destructor TDirector.Destroy;
begin
  FreeAndNil(FFilmList);
  inherited Destroy;
end;

procedure TDirector.GetFieldDefs(aMap: TOPFieldMapList);
var
  F: TOPFieldMap;
begin
  F := aMap.AddField('ID', 'ID');
  F.Options := [foInKey, foAutoInc, foInSelect];
  F.PropertyTypeInfo := TypeInfo(Integer);

  F := aMap.AddField('Name', 'Name');
  F.PropertyTypeInfo := TypeInfo(String);

  F := aMap.AddField('Country', 'Country');
  F.Options := F.Options + [foNullable];
  F.PropertyTypeInfo := TypeInfo(String);
end;

function TDirector.ApplyField(aFieldMap: TOPFieldMap; aField: TField): Boolean;
begin
  Result := True;
  if aField.IsNull then
  begin
    if aFieldMap.PropertyName = 'ID' then FID := 0
    else if aFieldMap.PropertyName = 'Name' then FName := ''
    else if aFieldMap.PropertyName = 'Country' then begin FCountry := ''; FCountryStored := False; end
    else Result := False;
    Exit;
  end;
  if aFieldMap.PropertyName = 'ID' then FID := aField.AsInteger
  else if aFieldMap.PropertyName = 'Name' then FName := aField.AsString
  else if aFieldMap.PropertyName = 'Country' then begin FCountry := aField.AsString; FCountryStored := True; end
  else Result := False;
end;

function TDirector.PersistField(aFieldMap: TOPFieldMap; aParam: TParam): Boolean;
begin
  Result := True;
  if aFieldMap.PropertyName = 'ID' then aParam.AsInteger := FID
  else if aFieldMap.PropertyName = 'Name' then aParam.AsString := FName
  else if aFieldMap.PropertyName = 'Country' then begin
    if not FCountryStored then aParam.Clear else aParam.AsString := FCountry;
  end
  else Result := False;
end;

{ ========== TActor ========== }

function TActor.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TActor.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

constructor TActor.Create;
begin
  inherited Create;
  FFilmList := TObjectList.Create(True);
end;

destructor TActor.Destroy;
begin
  FreeAndNil(FFilmList);
  inherited Destroy;
end;

procedure TActor.GetFieldDefs(aMap: TOPFieldMapList);
var
  F: TOPFieldMap;
begin
  F := aMap.AddField('ID', 'ID');
  F.Options := [foInKey, foAutoInc, foInSelect];
  F.PropertyTypeInfo := TypeInfo(Integer);

  F := aMap.AddField('Name', 'Name');
  F.PropertyTypeInfo := TypeInfo(String);

  F := aMap.AddField('BirthYear', 'BirthYear');
  F.Options := F.Options + [foNullable];
  F.PropertyTypeInfo := TypeInfo(Integer);
end;

function TActor.ApplyField(aFieldMap: TOPFieldMap; aField: TField): Boolean;
begin
  Result := True;
  if aField.IsNull then
  begin
    if aFieldMap.PropertyName = 'ID' then FID := 0
    else if aFieldMap.PropertyName = 'Name' then FName := ''
    else if aFieldMap.PropertyName = 'BirthYear' then begin FBirthYear := 0; FBirthYearStored := False; end
    else Result := False;
    Exit;
  end;
  if aFieldMap.PropertyName = 'ID' then FID := aField.AsInteger
  else if aFieldMap.PropertyName = 'Name' then FName := aField.AsString
  else if aFieldMap.PropertyName = 'BirthYear' then begin FBirthYear := aField.AsInteger; FBirthYearStored := True; end
  else Result := False;
end;

function TActor.PersistField(aFieldMap: TOPFieldMap; aParam: TParam): Boolean;
begin
  Result := True;
  if aFieldMap.PropertyName = 'ID' then aParam.AsInteger := FID
  else if aFieldMap.PropertyName = 'Name' then aParam.AsString := FName
  else if aFieldMap.PropertyName = 'BirthYear' then begin
    if not FBirthYearStored then aParam.Clear else aParam.AsInteger := FBirthYear;
  end
  else Result := False;
end;

{ ========== TGenre ========== }

function TGenre.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TGenre.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

procedure TGenre.GetFieldDefs(aMap: TOPFieldMapList);
var
  F: TOPFieldMap;
begin
  F := aMap.AddField('ID', 'ID');
  F.Options := [foInKey, foAutoInc, foInSelect];
  F.PropertyTypeInfo := TypeInfo(Integer);

  F := aMap.AddField('Name', 'Name');
  F.PropertyTypeInfo := TypeInfo(String);
end;

function TGenre.ApplyField(aFieldMap: TOPFieldMap; aField: TField): Boolean;
begin
  Result := True;
  if aField.IsNull then
  begin
    if aFieldMap.PropertyName = 'ID' then FID := 0
    else if aFieldMap.PropertyName = 'Name' then FName := ''
    else Result := False;
    Exit;
  end;
  if aFieldMap.PropertyName = 'ID' then FID := aField.AsInteger
  else if aFieldMap.PropertyName = 'Name' then FName := aField.AsString
  else Result := False;
end;

function TGenre.PersistField(aFieldMap: TOPFieldMap; aParam: TParam): Boolean;
begin
  Result := True;
  if aFieldMap.PropertyName = 'ID' then aParam.AsInteger := FID
  else if aFieldMap.PropertyName = 'Name' then aParam.AsString := FName
  else Result := False;
end;

{ ========== TLoan ========== }

function TLoan.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TLoan.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

procedure TLoan.GetFieldDefs(aMap: TOPFieldMapList);
var
  F: TOPFieldMap;
begin
  F := aMap.AddField('ID', 'ID');
  F.Options := [foInKey, foAutoInc, foInSelect];
  F.PropertyTypeInfo := TypeInfo(Integer);

  F := aMap.AddField('BorrowerName', 'BorrowerName');
  F.PropertyTypeInfo := TypeInfo(String);

  F := aMap.AddField('BorrowDate', 'BorrowDate');
  F.PropertyTypeInfo := TypeInfo(String);

  F := aMap.AddField('ReturnDate', 'ReturnDate');
  F.Options := F.Options + [foNullable];
  F.PropertyTypeInfo := TypeInfo(String);

  F := aMap.AddField('FilmID', 'FilmID');
  F.PropertyTypeInfo := TypeInfo(Integer);
end;

function TLoan.ApplyField(aFieldMap: TOPFieldMap; aField: TField): Boolean;
begin
  Result := True;
  if aField.IsNull then
  begin
    if aFieldMap.PropertyName = 'ID' then FID := 0
    else if aFieldMap.PropertyName = 'BorrowerName' then FBorrowerName := ''
    else if aFieldMap.PropertyName = 'BorrowDate' then FBorrowDate := ''
    else if aFieldMap.PropertyName = 'ReturnDate' then begin FReturnDate := ''; FReturnDateStored := False; end
    else if aFieldMap.PropertyName = 'FilmID' then FFilmID := 0
    else Result := False;
    Exit;
  end;
  if aFieldMap.PropertyName = 'ID' then FID := aField.AsInteger
  else if aFieldMap.PropertyName = 'BorrowerName' then FBorrowerName := aField.AsString
  else if aFieldMap.PropertyName = 'BorrowDate' then FBorrowDate := aField.AsString
  else if aFieldMap.PropertyName = 'ReturnDate' then begin FReturnDate := aField.AsString; FReturnDateStored := True; end
  else if aFieldMap.PropertyName = 'FilmID' then FFilmID := aField.AsInteger
  else Result := False;
end;

function TLoan.PersistField(aFieldMap: TOPFieldMap; aParam: TParam): Boolean;
begin
  Result := True;
  if aFieldMap.PropertyName = 'ID' then aParam.AsInteger := FID
  else if aFieldMap.PropertyName = 'BorrowerName' then aParam.AsString := FBorrowerName
  else if aFieldMap.PropertyName = 'BorrowDate' then aParam.AsString := FBorrowDate
  else if aFieldMap.PropertyName = 'ReturnDate' then begin
    if not FReturnDateStored then aParam.Clear else aParam.AsString := FReturnDate;
  end
  else if aFieldMap.PropertyName = 'FilmID' then aParam.AsInteger := FFilmID
  else Result := False;
end;

{ ========== TReportRow ========== }

function TReportRow.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TReportRow.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

procedure TReportRow.GetFieldDefs(aMap: TOPFieldMapList);
var
  F: TOPFieldMap;
begin
  F := aMap.AddField('Name', 'Name');
  F.Options := [foInKey, foInSelect];
  F.PropertyTypeInfo := TypeInfo(String);

  F := aMap.AddField('FilmCount', 'FilmCount');
  F.Options := [foInSelect];
  F.PropertyTypeInfo := TypeInfo(Integer);

  F := aMap.AddField('FilmTitles', 'FilmTitles');
  F.Options := [foInSelect];
  F.PropertyTypeInfo := TypeInfo(String);
end;

function TReportRow.ApplyField(aFieldMap: TOPFieldMap; aField: TField): Boolean;
begin
  Result := True;
  if aField.IsNull then
  begin
    if aFieldMap.PropertyName = 'Name' then FName := ''
    else if aFieldMap.PropertyName = 'FilmCount' then FFilmCount := 0
    else if aFieldMap.PropertyName = 'FilmTitles' then FFilmTitles := ''
    else Result := False;
    Exit;
  end;
  if aFieldMap.PropertyName = 'Name' then FName := aField.AsString
  else if aFieldMap.PropertyName = 'FilmCount' then FFilmCount := aField.AsInteger
  else if aFieldMap.PropertyName = 'FilmTitles' then FFilmTitles := aField.AsString
  else Result := False;
end;

function TReportRow.PersistField(aFieldMap: TOPFieldMap; aParam: TParam): Boolean;
begin
  Result := True;
  if aFieldMap.PropertyName = 'Name' then aParam.AsString := FName
  else if aFieldMap.PropertyName = 'FilmCount' then aParam.AsInteger := FFilmCount
  else if aFieldMap.PropertyName = 'FilmTitles' then aParam.AsString := FFilmTitles
  else Result := False;
end;

end.
