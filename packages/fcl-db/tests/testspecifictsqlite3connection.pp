unit TestSpecificTSQLite3Connection;
{ Tests of TSQLite3Connection features that only SQLite has. }

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, DB, sqldb, sqlite3conn, sqlite3dyn, ctypes, ToolsUnit, TestSQLDB;

type

  { TTestSpecificTSQLite3Connection }

  TTestSpecificTSQLite3Connection = class(TSQLDBTestCase)
  private
    FAddend: Integer;
    function Connection: TSQLite3Connection;
    // Runs aSQL, which returns one row, with the unicode string parameters aParams (P1, P2, ...); the first field
    function Eval(const aSQL: String; const aParams: array of UnicodeString): UnicodeString;
    // Runs aSQL and returns the class and message of the exception it raises, or '' when it raises none
    function EvalError(const aSQL: String): String;
  protected
    procedure SetUp; override;
  published
    procedure TestCreateFunction;
    procedure TestCreateFunctionArg;
    procedure TestCreateFunctionAnyArgCount;
    procedure TestCreateFunctionWrongArgCount;
    procedure TestCreateFunctionError;
    procedure TestRemoveFunction;
    procedure TestCreateFunctionNotConnected;
    procedure TestReplaceLowerForLike;
  end;

implementation

uses
  testregistry;

// The text argument in lower case, using the widestring manager
procedure LowerFunc(ctx: psqlite3_context; N: cint; V: ppsqlite3_value); cdecl;

var
  lText: RawByteString;

begin
  SetString(lText, sqlite3_value_text(V[0]), sqlite3_value_bytes(V[0]));
  lText := UTF8Encode(WideLowerCase(UTF8Decode(lText)));
  sqlite3_result_text(ctx, PAnsiChar(lText), Length(lText), sqlite3_destructor_type(SQLITE_TRANSIENT));
end;


// The integer argument plus the integer the function's user data points to
procedure AddArgFunc(ctx: psqlite3_context; N: cint; V: ppsqlite3_value); cdecl;

begin
  sqlite3_result_int64(ctx, sqlite3_value_int64(V[0]) + PInteger(sqlite3_user_data(ctx))^);
end;


// The number of arguments
procedure ArgCountFunc(ctx: psqlite3_context; N: cint; V: ppsqlite3_value); cdecl;

begin
  sqlite3_result_int64(ctx, N);
end;


// Always an error
procedure FailFunc(ctx: psqlite3_context; N: cint; V: ppsqlite3_value); cdecl;

begin
  sqlite3_result_error(ctx, 'fpdev function failed', -1);
end;


{ TTestSpecificTSQLite3Connection }

function TTestSpecificTSQLite3Connection.Connection: TSQLite3Connection;

begin
  Result := SQLDBConnector.Connection as TSQLite3Connection;
end;


function TTestSpecificTSQLite3Connection.Eval(const aSQL: String; const aParams: array of UnicodeString): UnicodeString;

var
  I: Integer;

begin
  with SQLDBConnector.Query do
    begin
    Close;
    SQL.Text := aSQL;
    for I := 0 to High(aParams) do
      Params.ParamByName('P' + IntToStr(I + 1)).AsWideString := aParams[I];
    Open;
    try
      Result := Fields[0].AsWideString;
    finally
      Close;
    end;
    end;
end;


function TTestSpecificTSQLite3Connection.EvalError(const aSQL: String): String;

begin
  Result := '';
  try
    Eval(aSQL, []);
  except
    on E: Exception do
      Result := E.ClassName + ': ' + E.Message;
  end;
  SQLDBConnector.Query.Close;
end;


procedure TTestSpecificTSQLite3Connection.SetUp;

begin
  inherited SetUp;
  Connection.Connected := True;
end;


procedure TTestSpecificTSQLite3Connection.TestCreateFunction;

begin
  Connection.CreateFunction('fpdev_lower', 1, @LowerFunc, nil, SQLITE_UTF8 or SQLITE_DETERMINISTIC);
  AssertEquals('Unicode lower case', UnicodeString('äbc бог'),
    Eval('select fpdev_lower(:P1)', [UnicodeString('ÄBC БОГ')]));
end;


procedure TTestSpecificTSQLite3Connection.TestCreateFunctionArg;

begin
  FAddend := 40;
  Connection.CreateFunction('fpdev_addarg', 1, @AddArgFunc, @FAddend);
  AssertEquals('Argument plus the user data', '42', Eval('select fpdev_addarg(2)', []));
end;


procedure TTestSpecificTSQLite3Connection.TestCreateFunctionAnyArgCount;

begin
  Connection.CreateFunction('fpdev_argcount', -1, @ArgCountFunc);
  AssertEquals('No arguments', '0', Eval('select fpdev_argcount()', []));
  AssertEquals('Three arguments', '3', Eval('select fpdev_argcount(1, ''a'', null)', []));
end;


procedure TTestSpecificTSQLite3Connection.TestCreateFunctionWrongArgCount;

var
  lError: String;

begin
  Connection.CreateFunction('fpdev_onearg', 1, @ArgCountFunc);
  AssertEquals('One argument', '1', Eval('select fpdev_onearg(5)', []));
  lError := EvalError('select fpdev_onearg(5, 6)');
  AssertTrue('Two arguments are refused: ' + lError, Pos('fpdev_onearg', lError) > 0);
end;


procedure TTestSpecificTSQLite3Connection.TestCreateFunctionError;

var
  lError: String;

begin
  Connection.CreateFunction('fpdev_fail', 0, @FailFunc);
  lError := EvalError('select fpdev_fail()');
  AssertTrue('The error of the function is raised: ' + lError, Pos('fpdev function failed', lError) > 0);
end;


procedure TTestSpecificTSQLite3Connection.TestRemoveFunction;

var
  lError: String;

begin
  Connection.CreateFunction('fpdev_removed', -1, @ArgCountFunc);
  AssertEquals('Function registered', '0', Eval('select fpdev_removed()', []));
  Connection.CreateFunction('fpdev_removed', -1, nil);
  lError := EvalError('select fpdev_removed()');
  AssertTrue('A removed function is unknown: ' + lError, Pos('fpdev_removed', lError) > 0);
end;


procedure TTestSpecificTSQLite3Connection.TestCreateFunctionNotConnected;

var
  lError: String;

begin
  Connection.Connected := False;
  lError := '';
  try
    Connection.CreateFunction('fpdev_notconnected', 0, @ArgCountFunc);
  except
    on E: Exception do
      lError := E.ClassName;
  end;
  AssertEquals('Registering without a connection raises', 'EDatabaseError', lError);
end;


procedure TTestSpecificTSQLite3Connection.TestReplaceLowerForLike;

begin
  Connection.CreateFunction('lower', 1, @LowerFunc, nil, SQLITE_UTF8 or SQLITE_DETERMINISTIC);
  Connection.ExecuteDirect('create temp table fpdev_like (t text)');
  try
    with SQLDBConnector.Query do
      begin
      SQL.Text := 'insert into fpdev_like (t) values (:P1)';
      Params.ParamByName('P1').AsWideString := 'В начале сотворил Бог небо и землю.';
      ExecSQL;
      Params.ParamByName('P1').AsWideString := 'И сказал свет';
      ExecSQL;
      end;
    AssertEquals('LIKE on the replaced lower() is case-insensitive for Cyrillic', '1',
      Eval('select count(*) from fpdev_like where lower(t) like :P1', [UnicodeString('%бог%')]));
  finally
    Connection.ExecuteDirect('drop table fpdev_like');
  end;
end;


initialization
  if (UpperCase(dbconnectorname) = 'SQL') and (UpperCase(dbconnectorparams) = 'SQLITE3') then
    RegisterTest(TTestSpecificTSQLite3Connection);
end.
