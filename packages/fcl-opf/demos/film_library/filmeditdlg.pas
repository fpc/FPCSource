unit filmeditdlg;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls,
  filmmodels;

type

  { TFilmEditDlg }

  TFilmEditDlg = class(TForm)
    BtnAddLoan: TButton;
    BtnAssignActor: TButton;
    BtnAssignDirector: TButton;
    BtnAssignGenre: TButton;
    BtnCancel: TButton;
    BtnDeleteLoan: TButton;
    BtnFetchIMDB: TButton;
    BtnOK: TButton;
    BtnRemoveActor: TButton;
    BtnRemoveDirector: TButton;
    BtnRemoveGenre: TButton;
    cbBluRay: TCheckBox;
    cbDVD: TCheckBox;
    EditIMDBId: TEdit;
    EditRating: TEdit;
    EditTitle: TEdit;
    EditYear: TEdit;
    gbActors: TGroupBox;
    gbDirectors: TGroupBox;
    gbGenres: TGroupBox;
    gbLoans: TGroupBox;
    lbActors: TListBox;
    lbDirectors: TListBox;
    lbGenres: TListBox;
    lbLoans: TListBox;
    LblIMDBId: TLabel;
    LblMediaFormat: TLabel;
    LblRating: TLabel;
    LblSynopsis: TLabel;
    LblTitle: TLabel;
    LblYear: TLabel;
    MemoSynopsis: TMemo;
    PnlActButtons: TPanel;
    PnlDirButtons: TPanel;
    PnlGenButtons: TPanel;
    PnlLoanButtons: TPanel;
    procedure BtnAssignDirectorClick(Sender: TObject);
    procedure BtnRemoveDirectorClick(Sender: TObject);
    procedure BtnAssignActorClick(Sender: TObject);
    procedure BtnRemoveActorClick(Sender: TObject);
    procedure BtnAssignGenreClick(Sender: TObject);
    procedure BtnRemoveGenreClick(Sender: TObject);
    procedure BtnAddLoanClick(Sender: TObject);
    procedure BtnDeleteLoanClick(Sender: TObject);
    procedure BtnFetchIMDBClick(Sender: TObject);
  private
    FFilm: TFilm;
    function GetOMDBApiKey: String;
    procedure FetchDirectors(const aNames: String);
    procedure FetchActors(const aNames: String);
    procedure FetchGenres(const aNames: String);
    procedure RefreshDirectorList;
    procedure RefreshActorList;
    procedure RefreshGenreList;
    procedure RefreshLoanList;
    procedure SetAssociationPanelsEnabled(aEnabled: Boolean);
  public
    procedure LoadFromFilm(aFilm: TFilm);
    procedure SaveToFilm(aFilm: TFilm);
  end;

var
  FilmEditDlgInstance: TFilmEditDlg;

implementation

uses
  contnrs, Dialogs, IniFiles,
  fphttpclient, fpjson, jsonparser,
  Opf.Manager, opfdatamodule;

{$R *.lfm}

{ TFilmEditDlg }

procedure TFilmEditDlg.SetAssociationPanelsEnabled(aEnabled: Boolean);
begin
  BtnAssignDirector.Enabled := aEnabled;
  BtnRemoveDirector.Enabled := aEnabled;
  BtnAssignActor.Enabled := aEnabled;
  BtnRemoveActor.Enabled := aEnabled;
  BtnAssignGenre.Enabled := aEnabled;
  BtnRemoveGenre.Enabled := aEnabled;
  BtnAddLoan.Enabled := aEnabled;
  BtnDeleteLoan.Enabled := aEnabled;
  if not aEnabled then
  begin
    lbDirectors.Items.Clear;
    lbDirectors.Items.Add('(Save film first)');
    lbActors.Items.Clear;
    lbActors.Items.Add('(Save film first)');
    lbGenres.Items.Clear;
    lbGenres.Items.Add('(Save film first)');
    lbLoans.Items.Clear;
    lbLoans.Items.Add('(Save film first)');
  end;
end;

procedure TFilmEditDlg.RefreshDirectorList;
var
  Dir: TDirector;
  I: Integer;
begin
  lbDirectors.Items.Clear;
  if (FFilm = nil) or (FFilm.ID <= 0) then Exit;
  OPManager.LoadAssociations(FFilm, 'DirectorList');
  for I := 0 to FFilm.DirectorList.Count - 1 do
  begin
    Dir := TDirector(FFilm.DirectorList[I]);
    lbDirectors.Items.AddObject(Dir.Name, Dir);
  end;
end;

procedure TFilmEditDlg.RefreshActorList;
var
  Act: TActor;
  I: Integer;
begin
  lbActors.Items.Clear;
  if (FFilm = nil) or (FFilm.ID <= 0) then Exit;
  OPManager.LoadAssociations(FFilm, 'ActorList');
  for I := 0 to FFilm.ActorList.Count - 1 do
  begin
    Act := TActor(FFilm.ActorList[I]);
    lbActors.Items.AddObject(Act.Name, Act);
  end;
end;

procedure TFilmEditDlg.RefreshGenreList;
var
  Genre: TGenre;
  I: Integer;
begin
  lbGenres.Items.Clear;
  if (FFilm = nil) or (FFilm.ID <= 0) then Exit;
  OPManager.LoadAssociations(FFilm, 'GenreList');
  for I := 0 to FFilm.GenreList.Count - 1 do
  begin
    Genre := TGenre(FFilm.GenreList[I]);
    lbGenres.Items.AddObject(Genre.Name, Genre);
  end;
end;

procedure TFilmEditDlg.RefreshLoanList;
var
  Loan: TLoan;
  S: String;
  I: Integer;
begin
  lbLoans.Items.Clear;
  if (FFilm = nil) or (FFilm.ID <= 0) then Exit;
  OPManager.LoadChildren(FFilm, 'LoanList');
  for I := 0 to FFilm.LoanList.Count - 1 do
  begin
    Loan := TLoan(FFilm.LoanList[I]);
    S := Loan.BorrowerName + ' - ' + Loan.BorrowDate;
    if Loan.ReturnDateStored then
      S := S + ' (returned ' + Loan.ReturnDate + ')'
    else
      S := S + ' (not returned)';
    lbLoans.Items.AddObject(S, Loan);
  end;
end;

procedure TFilmEditDlg.BtnAssignDirectorClick(Sender: TObject);
var
  AllDirs: TObjectList;
  Dir: TDirector;
  Dlg: TForm;
  LB: TListBox;
  Btn: TButton;
  I: Integer;
begin
  AllDirs := TObjectList.Create(True);
  try
    OPManager.Load(AllDirs, TDirector);
    Dlg := TForm.CreateNew(nil);
    try
      Dlg.Caption := 'Select Director';
      Dlg.Width := 300;
      Dlg.Height := 400;
      Dlg.Position := poScreenCenter;
      Dlg.BorderStyle := bsDialog;
      LB := TListBox.Create(Dlg);
      LB.Parent := Dlg;
      LB.Align := alClient;
      for I := 0 to AllDirs.Count - 1 do
      begin
        Dir := TDirector(AllDirs[I]);
        LB.Items.AddObject(Dir.Name, Dir);
      end;
      Btn := TButton.Create(Dlg);
      Btn.Parent := Dlg;
      Btn.Align := alBottom;
      Btn.Caption := 'Assign';
      Btn.ModalResult := mrOK;
      if (Dlg.ShowModal = mrOK) and (LB.ItemIndex >= 0) then
      begin
        Dir := TDirector(LB.Items.Objects[LB.ItemIndex]);
        OPManager.AddAssociation(FFilm, Dir);
        RefreshDirectorList;
      end;
    finally
      Dlg.Free;
    end;
  finally
    AllDirs.Free;
  end;
end;

procedure TFilmEditDlg.BtnRemoveDirectorClick(Sender: TObject);
var
  Dir: TDirector;
begin
  if lbDirectors.ItemIndex < 0 then
  begin
    ShowMessage('No director selected.');
    Exit;
  end;
  Dir := TDirector(lbDirectors.Items.Objects[lbDirectors.ItemIndex]);
  OPManager.RemoveAssociation(FFilm, Dir);
  RefreshDirectorList;
end;

procedure TFilmEditDlg.BtnAssignActorClick(Sender: TObject);
var
  AllActors: TObjectList;
  Act: TActor;
  Dlg: TForm;
  LB: TListBox;
  Btn: TButton;
  I: Integer;
begin
  AllActors := TObjectList.Create(True);
  try
    OPManager.Load(AllActors, TActor);
    Dlg := TForm.CreateNew(nil);
    try
      Dlg.Caption := 'Select Actor';
      Dlg.Width := 300;
      Dlg.Height := 400;
      Dlg.Position := poScreenCenter;
      Dlg.BorderStyle := bsDialog;
      LB := TListBox.Create(Dlg);
      LB.Parent := Dlg;
      LB.Align := alClient;
      for I := 0 to AllActors.Count - 1 do
      begin
        Act := TActor(AllActors[I]);
        LB.Items.AddObject(Act.Name, Act);
      end;
      Btn := TButton.Create(Dlg);
      Btn.Parent := Dlg;
      Btn.Align := alBottom;
      Btn.Caption := 'Assign';
      Btn.ModalResult := mrOK;
      if (Dlg.ShowModal = mrOK) and (LB.ItemIndex >= 0) then
      begin
        Act := TActor(LB.Items.Objects[LB.ItemIndex]);
        OPManager.AddAssociation(FFilm, Act);
        RefreshActorList;
      end;
    finally
      Dlg.Free;
    end;
  finally
    AllActors.Free;
  end;
end;

procedure TFilmEditDlg.BtnRemoveActorClick(Sender: TObject);
var
  Act: TActor;
begin
  if lbActors.ItemIndex < 0 then
  begin
    ShowMessage('No actor selected.');
    Exit;
  end;
  Act := TActor(lbActors.Items.Objects[lbActors.ItemIndex]);
  OPManager.RemoveAssociation(FFilm, Act);
  RefreshActorList;
end;

procedure TFilmEditDlg.BtnAssignGenreClick(Sender: TObject);
var
  AllGenres: TObjectList;
  Genre: TGenre;
  Dlg: TForm;
  LB: TListBox;
  Btn: TButton;
  I: Integer;
begin
  AllGenres := TObjectList.Create(True);
  try
    OPManager.Load(AllGenres, TGenre);
    Dlg := TForm.CreateNew(nil);
    try
      Dlg.Caption := 'Select Genre';
      Dlg.Width := 300;
      Dlg.Height := 400;
      Dlg.Position := poScreenCenter;
      Dlg.BorderStyle := bsDialog;
      LB := TListBox.Create(Dlg);
      LB.Parent := Dlg;
      LB.Align := alClient;
      for I := 0 to AllGenres.Count - 1 do
      begin
        Genre := TGenre(AllGenres[I]);
        LB.Items.AddObject(Genre.Name, Genre);
      end;
      Btn := TButton.Create(Dlg);
      Btn.Parent := Dlg;
      Btn.Align := alBottom;
      Btn.Caption := 'Assign';
      Btn.ModalResult := mrOK;
      if (Dlg.ShowModal = mrOK) and (LB.ItemIndex >= 0) then
      begin
        Genre := TGenre(LB.Items.Objects[LB.ItemIndex]);
        OPManager.AddAssociation(FFilm, Genre);
        RefreshGenreList;
      end;
    finally
      Dlg.Free;
    end;
  finally
    AllGenres.Free;
  end;
end;

procedure TFilmEditDlg.BtnRemoveGenreClick(Sender: TObject);
var
  Genre: TGenre;
begin
  if lbGenres.ItemIndex < 0 then
  begin
    ShowMessage('No genre selected.');
    Exit;
  end;
  Genre := TGenre(lbGenres.Items.Objects[lbGenres.ItemIndex]);
  OPManager.RemoveAssociation(FFilm, Genre);
  RefreshGenreList;
end;

procedure TFilmEditDlg.BtnAddLoanClick(Sender: TObject);
var
  Loan: TLoan;
  Dlg: TForm;
  EdName, EdBorrow, EdReturn: TEdit;
  LblName, LblBorrow, LblReturn: TLabel;
  BtnSave: TButton;
  Y: Integer;
begin
  Dlg := TForm.CreateNew(nil);
  try
    Dlg.Caption := 'Add Loan';
    Dlg.Width := 320;
    Dlg.Height := 220;
    Dlg.Position := poScreenCenter;
    Dlg.BorderStyle := bsDialog;

    Y := 12;
    LblName := TLabel.Create(Dlg);
    LblName.Parent := Dlg;
    LblName.SetBounds(12, Y, 80, 20);
    LblName.Caption := 'Borrower:';
    EdName := TEdit.Create(Dlg);
    EdName.Parent := Dlg;
    EdName.SetBounds(100, Y, 200, 24);

    Inc(Y, 36);
    LblBorrow := TLabel.Create(Dlg);
    LblBorrow.Parent := Dlg;
    LblBorrow.SetBounds(12, Y, 80, 20);
    LblBorrow.Caption := 'Borrow Date:';
    EdBorrow := TEdit.Create(Dlg);
    EdBorrow.Parent := Dlg;
    EdBorrow.SetBounds(100, Y, 200, 24);
    EdBorrow.Text := FormatDateTime('yyyy-mm-dd', Now);

    Inc(Y, 36);
    LblReturn := TLabel.Create(Dlg);
    LblReturn.Parent := Dlg;
    LblReturn.SetBounds(12, Y, 80, 20);
    LblReturn.Caption := 'Return Date:';
    EdReturn := TEdit.Create(Dlg);
    EdReturn.Parent := Dlg;
    EdReturn.SetBounds(100, Y, 200, 24);

    BtnSave := TButton.Create(Dlg);
    BtnSave.Parent := Dlg;
    BtnSave.SetBounds(120, Y + 40, 80, 30);
    BtnSave.Caption := 'Save';
    BtnSave.ModalResult := mrOK;

    if Dlg.ShowModal = mrOK then
    begin
      if (EdName.Text = '') or (EdBorrow.Text = '') then Exit;
      Loan := TLoan.Create;
      try
        Loan.FilmID := FFilm.ID;
        Loan.BorrowerName := EdName.Text;
        Loan.BorrowDate := EdBorrow.Text;
        if EdReturn.Text <> '' then
        begin
          Loan.ReturnDate := EdReturn.Text;
          Loan.ReturnDateStored := True;
        end
        else
          Loan.ReturnDateStored := False;
        OPManager.Save(Loan, opsNew);
        RefreshLoanList;
      finally
        Loan.Free;
      end;
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TFilmEditDlg.BtnDeleteLoanClick(Sender: TObject);
var
  Loan: TLoan;
  LoanID: Integer;
begin
  if lbLoans.ItemIndex < 0 then
  begin
    ShowMessage('No loan selected.');
    Exit;
  end;
  Loan := TLoan(lbLoans.Items.Objects[lbLoans.ItemIndex]);
  if MessageDlg('Delete Loan',
    'Are you sure you want to delete this loan for "' + Loan.BorrowerName + '"?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    LoanID := Loan.ID;
    Loan := TLoan.Create;
    try
      Loan.ID := LoanID;
      OPManager.Load(Loan);
      OPManager.Delete(Loan);
    finally
      Loan.Free;
    end;
    RefreshLoanList;
  end;
end;

function TFilmEditDlg.GetOMDBApiKey: String;
var
  IniPath: String;
  Ini: TIniFile;
begin
  Result := GetEnvironmentVariable('OMDB_API_KEY');
  if Result <> '' then Exit;
  IniPath := ExtractFilePath(ParamStr(0)) + 'omdb.ini';
  if FileExists(IniPath) then
  begin
    Ini := TIniFile.Create(IniPath);
    try
      Result := Ini.ReadString('OMDB', 'ApiKey', '');
    finally
      Ini.Free;
    end;
  end;
  if Result = '' then
    ShowMessage('OMDB API key not found. Set OMDB_API_KEY environment variable or create omdb.ini with [OMDB] ApiKey=yourkey');
end;

procedure TFilmEditDlg.FetchDirectors(const aNames: String);
var
  Names: TStringList;
  DirList: TObjectList;
  Dir: TDirector;
  I: Integer;
begin
  if (aNames = '') or (aNames = 'N/A') then Exit;
  if (FFilm = nil) or (FFilm.ID <= 0) then Exit;
  Names := TStringList.Create;
  try
    Names.Delimiter := ',';
    Names.StrictDelimiter := True;
    Names.DelimitedText := aNames;
    for I := 0 to Names.Count - 1 do
    begin
      if Trim(Names[I]) = '' then Continue;
      Dir := nil;
      DirList := TObjectList.Create(True);
      try
        OPManager.LoadSQL(DirList, TDirector,
          'SELECT ID, Name, Country FROM Director WHERE Name = :name',
          ['name', Trim(Names[I])]);
        if DirList.Count > 0 then
        begin
          Dir := TDirector.Create;
          Dir.ID := TDirector(DirList[0]).ID;
          Dir.Name := TDirector(DirList[0]).Name;
        end
        else
        begin
          Dir := TDirector.Create;
          Dir.Name := Trim(Names[I]);
          OPManager.Save(Dir, opsNew);
        end;
        OPManager.AddAssociation(FFilm, Dir);
      finally
        Dir.Free;
        DirList.Free;
      end;
    end;
  finally
    Names.Free;
  end;
  RefreshDirectorList;
end;

procedure TFilmEditDlg.FetchActors(const aNames: String);
var
  Names: TStringList;
  ActList: TObjectList;
  Act: TActor;
  I: Integer;
begin
  if (aNames = '') or (aNames = 'N/A') then Exit;
  if (FFilm = nil) or (FFilm.ID <= 0) then Exit;
  Names := TStringList.Create;
  try
    Names.Delimiter := ',';
    Names.StrictDelimiter := True;
    Names.DelimitedText := aNames;
    for I := 0 to Names.Count - 1 do
    begin
      if Trim(Names[I]) = '' then Continue;
      Act := nil;
      ActList := TObjectList.Create(True);
      try
        OPManager.LoadSQL(ActList, TActor,
          'SELECT ID, Name, BirthYear FROM Actor WHERE Name = :name',
          ['name', Trim(Names[I])]);
        if ActList.Count > 0 then
        begin
          Act := TActor.Create;
          Act.ID := TActor(ActList[0]).ID;
          Act.Name := TActor(ActList[0]).Name;
        end
        else
        begin
          Act := TActor.Create;
          Act.Name := Trim(Names[I]);
          OPManager.Save(Act, opsNew);
        end;
        OPManager.AddAssociation(FFilm, Act);
      finally
        Act.Free;
        ActList.Free;
      end;
    end;
  finally
    Names.Free;
  end;
  RefreshActorList;
end;

procedure TFilmEditDlg.FetchGenres(const aNames: String);
var
  Names: TStringList;
  GenList: TObjectList;
  Genre: TGenre;
  I: Integer;
begin
  if (aNames = '') or (aNames = 'N/A') then Exit;
  if (FFilm = nil) or (FFilm.ID <= 0) then Exit;
  Names := TStringList.Create;
  try
    Names.Delimiter := ',';
    Names.StrictDelimiter := True;
    Names.DelimitedText := aNames;
    for I := 0 to Names.Count - 1 do
    begin
      if Trim(Names[I]) = '' then Continue;
      Genre := nil;
      GenList := TObjectList.Create(True);
      try
        OPManager.LoadSQL(GenList, TGenre,
          'SELECT ID, Name FROM Genre WHERE Name = :name',
          ['name', Trim(Names[I])]);
        if GenList.Count > 0 then
        begin
          Genre := TGenre.Create;
          Genre.ID := TGenre(GenList[0]).ID;
          Genre.Name := TGenre(GenList[0]).Name;
        end
        else
        begin
          Genre := TGenre.Create;
          Genre.Name := Trim(Names[I]);
          OPManager.Save(Genre, opsNew);
        end;
        OPManager.AddAssociation(FFilm, Genre);
      finally
        Genre.Free;
        GenList.Free;
      end;
    end;
  finally
    Names.Free;
  end;
  RefreshGenreList;
end;

procedure TFilmEditDlg.BtnFetchIMDBClick(Sender: TObject);
var
  ApiKey, URL, Response, S: String;
  JSONData: TJSONData;
  JSONObj: TJSONObject;
  RatingVal: Double;
  DotSep: TFormatSettings;
begin
  DotSep := DefaultFormatSettings;
  DotSep.DecimalSeparator := '.';
  if Trim(EditIMDBId.Text) = '' then
  begin
    ShowMessage('Please enter an IMDB ID first (e.g., tt0133093).');
    Exit;
  end;

  ApiKey := GetOMDBApiKey;
  if ApiKey = '' then Exit;

  URL := 'http://www.omdbapi.com/?i=' + Trim(EditIMDBId.Text) +
         '&plot=full';
  if aPIkEY<>'' then
    URL:=URL+'&apikey=' + ApiKey;

  try
    Response := TFPHTTPClient.SimpleGet(URL);
  except
    on E: Exception do
    begin
      ShowMessage('OMDB fetch failed: ' + E.Message);
      Exit;
    end;
  end;

  JSONData := nil;
  try
    JSONData := GetJSON(Response);
    JSONObj := JSONData as TJSONObject;

    if JSONObj.Get('Response', '') = 'False' then
    begin
      ShowMessage('OMDB error: ' + JSONObj.Get('Error', 'Unknown error'));
      Exit;
    end;

    EditTitle.Text := JSONObj.Get('Title', '');
    EditYear.Text := JSONObj.Get('Year', '');
    MemoSynopsis.Text := JSONObj.Get('Plot', '');

    S := JSONObj.Get('imdbRating', '');
    if (S <> '') and (S <> 'N/A') then
    begin
      if TryStrToFloat(S, RatingVal, DotSep) then
        EditRating.Text := IntToStr(Round(RatingVal * 10));
    end;

    FetchDirectors(JSONObj.Get('Director', ''));
    FetchActors(JSONObj.Get('Actors', ''));
    FetchGenres(JSONObj.Get('Genre', ''));

    ShowMessage('OMDB data fetched successfully. Review the fields and click OK to save.');
  finally
    JSONData.Free;
  end;
end;

procedure TFilmEditDlg.LoadFromFilm(aFilm: TFilm);
begin
  FFilm := aFilm;

  EditTitle.Text := aFilm.Title;
  EditYear.Text := IntToStr(aFilm.Year);

  if aFilm.IMDBIdStored then
    EditIMDBId.Text := aFilm.IMDBId
  else
    EditIMDBId.Text := '';

  if aFilm.SynopsisStored then
    MemoSynopsis.Text := aFilm.Synopsis
  else
    MemoSynopsis.Text := '';

  if aFilm.RatingStored then
    EditRating.Text := IntToStr(aFilm.Rating)
  else
    EditRating.Text := '';

  cbDVD.Checked := mfDVD in aFilm.MediaFormat;
  cbBluRay.Checked := mfBluRay in aFilm.MediaFormat;

  if aFilm.ID > 0 then
  begin
    SetAssociationPanelsEnabled(True);
    RefreshDirectorList;
    RefreshActorList;
    RefreshGenreList;
    RefreshLoanList;
  end
  else
    SetAssociationPanelsEnabled(False);
end;

procedure TFilmEditDlg.SaveToFilm(aFilm: TFilm);
var
  Fmt: TMediaFormats;
begin
  aFilm.Title := EditTitle.Text;
  aFilm.Year := StrToIntDef(EditYear.Text, 0);

  if EditIMDBId.Text <> '' then
  begin
    aFilm.IMDBId := EditIMDBId.Text;
    aFilm.IMDBIdStored := True;
  end
  else
    aFilm.IMDBIdStored := False;

  if Trim(MemoSynopsis.Text) <> '' then
  begin
    aFilm.Synopsis := MemoSynopsis.Text;
    aFilm.SynopsisStored := True;
  end
  else
    aFilm.SynopsisStored := False;

  if EditRating.Text <> '' then
  begin
    aFilm.Rating := StrToIntDef(EditRating.Text, 0);
    aFilm.RatingStored := True;
  end
  else
    aFilm.RatingStored := False;

  Fmt := [];
  if cbDVD.Checked then Include(Fmt, mfDVD);
  if cbBluRay.Checked then Include(Fmt, mfBluRay);
  aFilm.MediaFormat := Fmt;
end;

end.
