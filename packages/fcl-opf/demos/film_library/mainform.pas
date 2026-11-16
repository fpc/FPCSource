unit mainform;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, contnrs, Forms, Controls, Grids, StdCtrls, ExtCtrls,
  ComCtrls, opfdatamodule;

type

  { TMainForm }

  TMainForm = class(TForm)
    BtnAdd: TButton;
    BtnBulkImport: TButton;
    BtnEdit: TButton;
    BtnDelete: TButton;
    BtnFilmsPerActor: TButton;
    BtnFilmsPerDirector: TButton;
    BtnManageDirectors: TButton;
    BtnManageActors: TButton;
    BtnManageGenres: TButton;
    BtnSearch: TButton;
    BtnShowAll: TButton;
    ButtonPanel: TPanel;
    EditSearch: TEdit;
    FilmGrid: TStringGrid;
    LblSearch: TLabel;
    SearchPanel: TPanel;
    StatusBar1: TStatusBar;
    procedure BtnAddClick(Sender: TObject);
    procedure BtnEditClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure BtnSearchClick(Sender: TObject);
    procedure BtnShowAllClick(Sender: TObject);
    procedure BtnBulkImportClick(Sender: TObject);
    procedure BtnFilmsPerDirectorClick(Sender: TObject);
    procedure BtnFilmsPerActorClick(Sender: TObject);
    procedure BtnManageDirectorsClick(Sender: TObject);
    procedure BtnManageActorsClick(Sender: TObject);
    procedure BtnManageGenresClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure PopulateGrid(aList: TObjectList);
    procedure RefreshGrid;
    procedure RefreshStatistics;
    function GetSelectedFilmID: Integer;
  end;

var
  MainFormInstance: TMainForm;

implementation

uses
  Dialogs,
  Opf.Manager,
  filmmodels, filmeditdlg, directorlistform, actorlistform, genrelistform,
  reportform;

{$R *.lfm}

const
  SEARCH_SQL = 'SELECT ID, Title, IMDBId, Year, Synopsis, Rating, MediaFormat ' +
               'FROM Film WHERE Title LIKE :term OR IMDBId LIKE :term2';

  FILMS_PER_DIRECTOR_SQL =
    'SELECT d.Name, COUNT(fd.FilmID) AS FilmCount, ' +
    'COALESCE(GROUP_CONCAT(f.Title, '', ''), '''') AS FilmTitles ' +
    'FROM Director d ' +
    'LEFT JOIN film_director fd ON d.ID = fd.DirectorID ' +
    'LEFT JOIN Film f ON fd.FilmID = f.ID ' +
    'GROUP BY d.ID, d.Name ' +
    'ORDER BY d.Name';

  FILMS_PER_ACTOR_SQL =
    'SELECT a.Name, COUNT(fa.FilmID) AS FilmCount, ' +
    'COALESCE(GROUP_CONCAT(f.Title, '', ''), '''') AS FilmTitles ' +
    'FROM Actor a ' +
    'LEFT JOIN film_actor fa ON a.ID = fa.ActorID ' +
    'LEFT JOIN Film f ON fa.FilmID = f.ID ' +
    'GROUP BY a.ID, a.Name ' +
    'ORDER BY a.Name';

function MediaFormatToStr(aFmt: TMediaFormats): String;
begin
  Result := '';
  if mfDVD in aFmt then Result := 'DVD';
  if mfBluRay in aFmt then
  begin
    if Result <> '' then Result := Result + ', ';
    Result := Result + 'BluRay';
  end;
end;

{ TMainForm }

function TMainForm.GetSelectedFilmID: Integer;
begin
  if (FilmGrid.Row < 1) or (FilmGrid.Row >= FilmGrid.RowCount) then
    Result := -1
  else
    Result := StrToIntDef(FilmGrid.Cells[0, FilmGrid.Row], -1);
end;

procedure TMainForm.PopulateGrid(aList: TObjectList);
var
  Film: TFilm;
  I: Integer;
begin
  FilmGrid.RowCount := aList.Count + 1;
  for I := 0 to aList.Count - 1 do
  begin
    Film := TFilm(aList[I]);
    FilmGrid.Cells[0, I + 1] := IntToStr(Film.ID);
    FilmGrid.Cells[1, I + 1] := Film.Title;
    FilmGrid.Cells[2, I + 1] := IntToStr(Film.Year);
    if Film.IMDBIdStored then
      FilmGrid.Cells[3, I + 1] := Film.IMDBId
    else
      FilmGrid.Cells[3, I + 1] := '';
    if Film.RatingStored then
      FilmGrid.Cells[4, I + 1] := IntToStr(Film.Rating)
    else
      FilmGrid.Cells[4, I + 1] := '';
    FilmGrid.Cells[5, I + 1] := MediaFormatToStr(Film.MediaFormat);
  end;
end;

procedure TMainForm.RefreshGrid;
var
  List: TObjectList;
begin
  List := TObjectList.Create(True);
  try
    opfdatamodule.OPManager.Load(List, TFilm);
    PopulateGrid(List);
  finally
    List.Free;
  end;
  RefreshStatistics;
end;

procedure TMainForm.RefreshStatistics;
var
  F, D, A, G, L: Integer;
begin
  OPFDataModuleInstance.GetStatistics(F, D, A, G, L);
  StatusBar1.Panels[0].Text := 'Films: ' + IntToStr(F);
  StatusBar1.Panels[1].Text := 'Directors: ' + IntToStr(D);
  StatusBar1.Panels[2].Text := 'Actors: ' + IntToStr(A);
  StatusBar1.Panels[3].Text := 'Genres: ' + IntToStr(G);
  StatusBar1.Panels[4].Text := 'Loans: ' + IntToStr(L);
end;

procedure TMainForm.BtnSearchClick(Sender: TObject);
var
  List: TObjectList;
  SearchText: String;
begin
  SearchText := Trim(EditSearch.Text);
  if SearchText = '' then
  begin
    RefreshGrid;
    Exit;
  end;
  List := TObjectList.Create(True);
  try
    opfdatamodule.OPManager.LoadSQL(List, TFilm, SEARCH_SQL,
      ['term', '%' + SearchText + '%', 'term2', '%' + SearchText + '%']);
    PopulateGrid(List);
  finally
    List.Free;
  end;
  RefreshStatistics;
end;

procedure TMainForm.BtnShowAllClick(Sender: TObject);
begin
  EditSearch.Text := '';
  RefreshGrid;
end;

procedure TMainForm.BtnAddClick(Sender: TObject);
var
  Film: TFilm;
  Dlg: TFilmEditDlg;
begin
  Dlg := TFilmEditDlg.Create(nil);
  try
    if Dlg.ShowModal = mrOK then
    begin
      Film := TFilm.Create;
      try
        Dlg.SaveToFilm(Film);
        opfdatamodule.OPManager.Save(Film, opsNew);
        RefreshGrid;
      finally
        Film.Free;
      end;
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TMainForm.BtnEditClick(Sender: TObject);
var
  Film: TFilm;
  Dlg: TFilmEditDlg;
  ID: Integer;
begin
  ID := GetSelectedFilmID;
  if ID < 0 then
  begin
    ShowMessage('No film selected.');
    Exit;
  end;
  Film := TFilm.Create;
  try
    Film.ID := ID;
    opfdatamodule.OPManager.Load(Film);
    Dlg := TFilmEditDlg.Create(nil);
    try
      Dlg.LoadFromFilm(Film);
      if Dlg.ShowModal = mrOK then
      begin
        Dlg.SaveToFilm(Film);
        opfdatamodule.OPManager.Save(Film, opsUpdate);
        RefreshGrid;
      end;
    finally
      Dlg.Free;
    end;
  finally
    Film.Free;
  end;
end;

procedure TMainForm.BtnDeleteClick(Sender: TObject);
var
  Film: TFilm;
  ID, LoanCount, AssocCount: Integer;
begin
  ID := GetSelectedFilmID;
  if ID < 0 then
  begin
    ShowMessage('No film selected.');
    Exit;
  end;
  Film := TFilm.Create;
  try
    Film.ID := ID;
    opfdatamodule.OPManager.Load(Film);

    opfdatamodule.OPManager.LoadAssociations(Film, 'DirectorList');
    opfdatamodule.OPManager.LoadAssociations(Film, 'ActorList');
    opfdatamodule.OPManager.LoadAssociations(Film, 'GenreList');
    opfdatamodule.OPManager.LoadChildren(Film, 'LoanList');

    LoanCount := Film.LoanList.Count;
    AssocCount := Film.DirectorList.Count + Film.ActorList.Count + Film.GenreList.Count;

    if MessageDlg('Delete Film',
      Format('Delete "%s"?' + LineEnding +
        'This will also remove %d loan(s) and clear %d association(s).',
        [Film.Title, LoanCount, AssocCount]),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      opfdatamodule.OPManager.Delete(Film);
      ShowMessage(Format('Deleted film + %d loan(s), cleared %d director/actor/genre association(s)',
        [LoanCount, AssocCount]));
      RefreshGrid;
    end;
  finally
    Film.Free;
  end;
end;

procedure TMainForm.BtnBulkImportClick(Sender: TObject);
var
  Film: TFilm;
  Dir: TDirector;
  Act: TActor;
  Genre: TGenre;
  Loan: TLoan;
  MatrixID, InceptionID, PulpID, DarkKnightID, InterstellarID: Integer;
  WachowskiID, NolanID, TarantinoID: Integer;
  ReevesID, DiCaprioID, JacksonID: Integer;
  SciFiID, ActionID, CrimeID, DramaID: Integer;

  procedure AddAssoc(aFilmID, aOtherID: Integer; aOtherClass: TClass);
  var
    F: TFilm;
    O: TObject;
  begin
    F := TFilm.Create;
    try
      F.ID := aFilmID;
      O := aOtherClass.Create;
      try
        if aOtherClass = TDirector then TDirector(O).ID := aOtherID
        else if aOtherClass = TActor then TActor(O).ID := aOtherID
        else if aOtherClass = TGenre then TGenre(O).ID := aOtherID;
        opfdatamodule.OPManager.AddAssociation(F, O);
      finally
        O.Free;
      end;
    finally
      F.Free;
    end;
  end;

begin
  opfdatamodule.OPManager.BeginTransaction;
  try
    { Films }
    Film := TFilm.Create;
    try
      Film.Title := 'The Matrix'; Film.Year := 1999;
      opfdatamodule.OPManager.Save(Film, opsNew); MatrixID := Film.ID;
    finally Film.Free; end;

    Film := TFilm.Create;
    try
      Film.Title := 'Inception'; Film.Year := 2010;
      opfdatamodule.OPManager.Save(Film, opsNew); InceptionID := Film.ID;
    finally Film.Free; end;

    Film := TFilm.Create;
    try
      Film.Title := 'Pulp Fiction'; Film.Year := 1994;
      opfdatamodule.OPManager.Save(Film, opsNew); PulpID := Film.ID;
    finally Film.Free; end;

    Film := TFilm.Create;
    try
      Film.Title := 'The Dark Knight'; Film.Year := 2008;
      opfdatamodule.OPManager.Save(Film, opsNew); DarkKnightID := Film.ID;
    finally Film.Free; end;

    Film := TFilm.Create;
    try
      Film.Title := 'Interstellar'; Film.Year := 2014;
      opfdatamodule.OPManager.Save(Film, opsNew); InterstellarID := Film.ID;
    finally Film.Free; end;

    { Directors }
    Dir := TDirector.Create;
    try
      Dir.Name := 'Lana Wachowski';
      opfdatamodule.OPManager.Save(Dir, opsNew); WachowskiID := Dir.ID;
    finally Dir.Free; end;

    Dir := TDirector.Create;
    try
      Dir.Name := 'Christopher Nolan';
      opfdatamodule.OPManager.Save(Dir, opsNew); NolanID := Dir.ID;
    finally Dir.Free; end;

    Dir := TDirector.Create;
    try
      Dir.Name := 'Quentin Tarantino';
      opfdatamodule.OPManager.Save(Dir, opsNew); TarantinoID := Dir.ID;
    finally Dir.Free; end;

    { Actors }
    Act := TActor.Create;
    try
      Act.Name := 'Keanu Reeves';
      opfdatamodule.OPManager.Save(Act, opsNew); ReevesID := Act.ID;
    finally Act.Free; end;

    Act := TActor.Create;
    try
      Act.Name := 'Leonardo DiCaprio';
      opfdatamodule.OPManager.Save(Act, opsNew); DiCaprioID := Act.ID;
    finally Act.Free; end;

    Act := TActor.Create;
    try
      Act.Name := 'Samuel L. Jackson';
      opfdatamodule.OPManager.Save(Act, opsNew); JacksonID := Act.ID;
    finally Act.Free; end;

    { Genres }
    Genre := TGenre.Create;
    try
      Genre.Name := 'Sci-Fi';
      opfdatamodule.OPManager.Save(Genre, opsNew); SciFiID := Genre.ID;
    finally Genre.Free; end;

    Genre := TGenre.Create;
    try
      Genre.Name := 'Action';
      opfdatamodule.OPManager.Save(Genre, opsNew); ActionID := Genre.ID;
    finally Genre.Free; end;

    Genre := TGenre.Create;
    try
      Genre.Name := 'Crime';
      opfdatamodule.OPManager.Save(Genre, opsNew); CrimeID := Genre.ID;
    finally Genre.Free; end;

    Genre := TGenre.Create;
    try
      Genre.Name := 'Drama';
      opfdatamodule.OPManager.Save(Genre, opsNew); DramaID := Genre.ID;
    finally Genre.Free; end;

    { Associations: Film <-> Director }
    AddAssoc(MatrixID, WachowskiID, TDirector);
    AddAssoc(InceptionID, NolanID, TDirector);
    AddAssoc(PulpID, TarantinoID, TDirector);
    AddAssoc(DarkKnightID, NolanID, TDirector);
    AddAssoc(InterstellarID, NolanID, TDirector);

    { Associations: Film <-> Actor }
    AddAssoc(MatrixID, ReevesID, TActor);
    AddAssoc(InceptionID, DiCaprioID, TActor);
    AddAssoc(PulpID, JacksonID, TActor);
    AddAssoc(InterstellarID, DiCaprioID, TActor);

    { Associations: Film <-> Genre }
    AddAssoc(MatrixID, SciFiID, TGenre);
    AddAssoc(MatrixID, ActionID, TGenre);
    AddAssoc(InceptionID, SciFiID, TGenre);
    AddAssoc(PulpID, CrimeID, TGenre);
    AddAssoc(DarkKnightID, ActionID, TGenre);
    AddAssoc(DarkKnightID, CrimeID, TGenre);
    AddAssoc(InterstellarID, SciFiID, TGenre);
    AddAssoc(InterstellarID, DramaID, TGenre);

    { Loans }
    Loan := TLoan.Create;
    try
      Loan.FilmID := MatrixID;
      Loan.BorrowerName := 'John';
      Loan.BorrowDate := '2026-01-15';
      Loan.ReturnDateStored := False;
      opfdatamodule.OPManager.Save(Loan, opsNew);
    finally Loan.Free; end;

    Loan := TLoan.Create;
    try
      Loan.FilmID := InceptionID;
      Loan.BorrowerName := 'Jane';
      Loan.BorrowDate := '2026-02-01';
      Loan.ReturnDate := '2026-02-15';
      Loan.ReturnDateStored := True;
      opfdatamodule.OPManager.Save(Loan, opsNew);
    finally Loan.Free; end;

    Loan := TLoan.Create;
    try
      Loan.FilmID := PulpID;
      Loan.BorrowerName := 'Bob';
      Loan.BorrowDate := '2026-03-01';
      Loan.ReturnDateStored := False;
      opfdatamodule.OPManager.Save(Loan, opsNew);
    finally Loan.Free; end;

    opfdatamodule.OPManager.Commit;
    RefreshGrid;
    ShowMessage('Bulk import complete: 5 films, 3 directors, 3 actors, 4 genres, 13 associations, 3 loans');
  except
    on E: Exception do
    begin
      opfdatamodule.OPManager.Rollback;
      ShowMessage('Import failed, all changes rolled back: ' + E.Message);
    end;
  end;
end;

procedure TMainForm.BtnFilmsPerDirectorClick(Sender: TObject);
var
  List: TObjectList;
  Dlg: TReportForm;
begin
  List := TObjectList.Create(True);
  try
    opfdatamodule.OPManager.LoadSQL(List, TReportRow, FILMS_PER_DIRECTOR_SQL);
    Dlg := TReportForm.Create(nil);
    try
      Dlg.LoadReport('Films per Director', List);
      Dlg.ShowModal;
    finally
      Dlg.Free;
    end;
  finally
    List.Free;
  end;
end;

procedure TMainForm.BtnFilmsPerActorClick(Sender: TObject);
var
  List: TObjectList;
  Dlg: TReportForm;
begin
  List := TObjectList.Create(True);
  try
    opfdatamodule.OPManager.LoadSQL(List, TReportRow, FILMS_PER_ACTOR_SQL);
    Dlg := TReportForm.Create(nil);
    try
      Dlg.LoadReport('Films per Actor', List);
      Dlg.ShowModal;
    finally
      Dlg.Free;
    end;
  finally
    List.Free;
  end;
end;

procedure TMainForm.BtnManageDirectorsClick(Sender: TObject);
var
  Dlg: TDirectorListForm;
begin
  Dlg := TDirectorListForm.Create(nil);
  try
    Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

procedure TMainForm.BtnManageActorsClick(Sender: TObject);
var
  Dlg: TActorListForm;
begin
  Dlg := TActorListForm.Create(nil);
  try
    Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

procedure TMainForm.BtnManageGenresClick(Sender: TObject);
var
  Dlg: TGenreListForm;
begin
  Dlg := TGenreListForm.Create(nil);
  try
    Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  OPFDataModuleInstance.SetupDatabase;
  RefreshGrid;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  OPFDataModuleInstance.TeardownDatabase;
end;

end.
