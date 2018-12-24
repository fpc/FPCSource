library myext;

{$mode objfpc}{$h+}

uses
  sysutils,
  ctypes,
  sqlite3,
  sqlite3ext;

procedure mysum(ctx: psqlite3_context; n: cint; v: ppsqlite3_value); cdecl;
var
  a, b, r: cint;
begin
  a := sqlite3_value_int(v[0]);
  b := sqlite3_value_int(v[1]);
  r := a + b;
  sqlite3_result_int(ctx, r);
end;

procedure myconcat(ctx: psqlite3_context; n: cint; v: ppsqlite3_value); cdecl;
var
  a, b, r: ansistring;
begin
  a := sqlite3_value_text(v[0]);
  b := sqlite3_value_text(v[1]);
  r := a + b;
  sqlite3_result_text(ctx, @r[1], length(r), nil);
end;

function sqlite3_extension_init(db: Psqlite3; pzErrMsg: Ppcchar;
  const pApi: Psqlite3_api_routines): cint; cdecl; export;
var
  rc: cint;
begin
  SQLITE_EXTENSION_INIT2(pApi);
  rc := sqlite3_create_function(db, 'mysum', 2, SQLITE_UTF8, nil,
    @mysum, nil, nil);
  if rc = SQLITE_OK then
    Result := sqlite3_create_function(db, 'myconcat', 2, SQLITE_UTF8, nil,
      @myconcat, nil, nil);
  Result := rc;
end;

exports
  sqlite3_extension_init;

begin
end.
