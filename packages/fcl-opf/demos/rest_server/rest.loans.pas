{ rest.loans - Loan CRUD route handlers (nested under /films/:filmId)
  ===================================================================
  Loans are 1:N children of Film. Uses SendData/SendError from rest.base.
}
unit rest.loans;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  httproute;

procedure RegisterLoanRoutes;

implementation

uses
  Classes, SysUtils, contnrs, fpjson, jsonparser, httpdefs,
  Opf.Manager,
  filmmodels,
  rest.setup,
  rest.base;

function GetFilmID(ARequest: TRequest; AResponse: TResponse; out FilmID: Integer): Boolean;
begin
  FilmID := StrToIntDef(ARequest.RouteParams['filmId'], 0);
  Result := FilmID <> 0;
  if not Result then
    SendError(AResponse, 400, 'Invalid film ID');
end;

function LoadFilm(FilmID: Integer; AResponse: TResponse): Boolean;
var
  Film: TFilm;
begin
  Film := TFilm.Create;
  try
    Film.ID := FilmID;
    try
      OPManager.Load(Film);
      Result := True;
    except
      SendError(AResponse, 404, 'Film not found');
      Result := False;
    end;
  finally
    Film.Free;
  end;
end;

procedure HandleListLoans(ARequest: TRequest; AResponse: TResponse);
var
  Film: TFilm;
  FilmID: Integer;
  Arr: TJSONArray;
begin
  if not GetFilmID(ARequest, AResponse, FilmID) then
    Exit;
  Film := TFilm.Create;
  try
    Film.ID := FilmID;
    try
      OPManager.Load(Film);
    except
      SendError(AResponse, 404, 'Film not found');
      Exit;
    end;
    OPManager.LoadChildren(Film, 'LoanList');
    Arr := ListToJSONArray(Film.LoanList, TLoan);
    try
      SendData(AResponse, 200, Arr);
    finally
      Arr.Free;
    end;
  finally
    Film.Free;
  end;
end;

procedure HandleGetLoan(ARequest: TRequest; AResponse: TResponse);
var
  Loan: TLoan;
  FilmID, LoanID: Integer;
  Obj: TJSONObject;
begin
  if not GetFilmID(ARequest, AResponse, FilmID) then
    Exit;
  LoanID := StrToIntDef(ARequest.RouteParams['id'], 0);
  if LoanID = 0 then
  begin
    SendError(AResponse, 400, 'Invalid loan ID');
    Exit;
  end;
  Loan := TLoan.Create;
  try
    Loan.ID := LoanID;
    try
      OPManager.Load(Loan);
    except
      SendError(AResponse, 404, 'Loan not found');
      Exit;
    end;
    if Loan.FilmID <> FilmID then
    begin
      SendError(AResponse, 404, 'Loan does not belong to this film');
      Exit;
    end;
    Obj := LoanToJSON(Loan);
    try
      SendData(AResponse, 200, Obj);
    finally
      Obj.Free;
    end;
  finally
    Loan.Free;
  end;
end;

procedure HandleCreateLoan(ARequest: TRequest; AResponse: TResponse);
var
  Loan: TLoan;
  IPS: IOPPersistenceState;
  FilmID: Integer;
  ReqJSON, RespObj: TJSONObject;
  Parser: TJSONParser;
begin
  if not GetFilmID(ARequest, AResponse, FilmID) then
    Exit;
  if ARequest.Content = '' then
  begin
    SendError(AResponse, 400, 'Request body is empty');
    Exit;
  end;
  if not LoadFilm(FilmID, AResponse) then
    Exit;
  Loan := TLoan.Create;
  try
    Parser := TJSONParser.Create(ARequest.Content);
    try
      ReqJSON := Parser.Parse as TJSONObject;
    finally
      Parser.Free;
    end;
    try
      JSONToLoan(ReqJSON, Loan);
    finally
      ReqJSON.Free;
    end;
    Loan.FilmID := FilmID;
    if Loan.GetInterface('{84DE4D96-0D52-0DC1-3129-1C576A55493A}', IPS) then
      IPS.SetPersistenceState(opsNew);
    OPManager.Save(Loan);
    RespObj := LoanToJSON(Loan);
    try
      SendData(AResponse, 201, RespObj);
    finally
      RespObj.Free;
    end;
  except
    on E: Exception do
    begin
      Loan.Free;
      SendError(AResponse, 400, E.Message);
      Exit;
    end;
  end;
  Loan.Free;
end;

procedure HandleUpdateLoan(ARequest: TRequest; AResponse: TResponse);
var
  Loan: TLoan;
  IPS: IOPPersistenceState;
  FilmID, LoanID: Integer;
  ReqJSON, RespObj: TJSONObject;
  Parser: TJSONParser;
begin
  if not GetFilmID(ARequest, AResponse, FilmID) then
    Exit;
  LoanID := StrToIntDef(ARequest.RouteParams['id'], 0);
  if LoanID = 0 then
  begin
    SendError(AResponse, 400, 'Invalid loan ID');
    Exit;
  end;
  Loan := TLoan.Create;
  try
    Loan.ID := LoanID;
    try
      OPManager.Load(Loan);
    except
      SendError(AResponse, 404, 'Loan not found');
      Loan.Free;
      Exit;
    end;
    if Loan.FilmID <> FilmID then
    begin
      SendError(AResponse, 404, 'Loan does not belong to this film');
      Loan.Free;
      Exit;
    end;
    Parser := TJSONParser.Create(ARequest.Content);
    try
      ReqJSON := Parser.Parse as TJSONObject;
    finally
      Parser.Free;
    end;
    try
      JSONToLoan(ReqJSON, Loan);
    finally
      ReqJSON.Free;
    end;
    if Loan.GetInterface('{84DE4D96-0D52-0DC1-3129-1C576A55493A}', IPS) then
      IPS.SetPersistenceState(opsUpdate);
    OPManager.Save(Loan);
    RespObj := LoanToJSON(Loan);
    try
      SendData(AResponse, 200, RespObj);
    finally
      RespObj.Free;
    end;
  except
    on E: Exception do
    begin
      Loan.Free;
      SendError(AResponse, 500, E.Message);
      Exit;
    end;
  end;
  Loan.Free;
end;

procedure HandleDeleteLoan(ARequest: TRequest; AResponse: TResponse);
var
  Loan: TLoan;
  FilmID, LoanID: Integer;
begin
  if not GetFilmID(ARequest, AResponse, FilmID) then
    Exit;
  LoanID := StrToIntDef(ARequest.RouteParams['id'], 0);
  if LoanID = 0 then
  begin
    SendError(AResponse, 400, 'Invalid loan ID');
    Exit;
  end;
  Loan := TLoan.Create;
  try
    Loan.ID := LoanID;
    try
      OPManager.Load(Loan);
    except
      SendError(AResponse, 404, 'Loan not found');
      Exit;
    end;
    if Loan.FilmID <> FilmID then
    begin
      SendError(AResponse, 404, 'Loan does not belong to this film');
      Exit;
    end;
    OPManager.Delete(Loan);
    AResponse.Code := 204;
  finally
    Loan.Free;
  end;
end;

procedure RegisterLoanRoutes;
begin
  HTTPRouter.RegisterRoute('/films/:filmId/loans', rmGet, @HandleListLoans);
  HTTPRouter.RegisterRoute('/films/:filmId/loans/:id', rmGet, @HandleGetLoan);
  HTTPRouter.RegisterRoute('/films/:filmId/loans', rmPost, @HandleCreateLoan);
  HTTPRouter.RegisterRoute('/films/:filmId/loans/:id', rmPut, @HandleUpdateLoan);
  HTTPRouter.RegisterRoute('/films/:filmId/loans/:id', rmDelete, @HandleDeleteLoan);
end;

end.
