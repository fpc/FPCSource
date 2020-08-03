unit watparser;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes, parseutils, wasmtext, wasmmodule, wasmbin, watscanner;

type
  TParseResult = record
    error : string;
  end;

//function ConsumeToken(sc: TWatScanner; tk: TWatToken): Boolean;
function ParseModule(sc: TWatScanner; dst: TWasmModule; var res: TParseResult): Boolean;
procedure ErrorUnexpected(var res: TParseResult; const tokenstr: string = '');
procedure ErrorExpectButFound(var res: TParseResult; const expected: string; const butfound: string = '');
procedure ErrorUnexpectedEof(var res: TParseResult);

implementation

procedure ConsumeAnyOpenToken(sc: TWatScanner; out tk: TWatToken;
  out hadOpenBrace: Boolean);
begin
  sc.Next;
  hadOpenBrace := sc.token = weOpenBrace;
  if hadOpenBrace then sc.Next;
  tk:=sc.token;
end;

function ConsumeOpenToken(sc: TWatScanner; tk: TWatToken): Boolean;
begin
  sc.Next;
  Result := (sc.token=weOpenBrace) or (sc.Token=tk);
  if Result and (sc.token=weOpenBrace) then begin
    sc.Next;
    Result := (sc.Token=tk);
  end;
end;

function ConsumeToken(sc: TWatScanner; tk: TWatToken; var res: TParseResult): Boolean;
begin
  Result:=sc.token =tk;
  if not Result then
    ErrorExpectButFound(res, 'some token','?')
  else
    sc.Next;
end;

function ParseNumOfId(sc: TWatScanner; out num: integer; out id: string; var res: TParseResult): Boolean;
begin
  num:=-1;
  id:='';
  Result := sc.Next;
  if not Result then begin
    ErrorUnexpectedEof(res);
    Exit;
  end;

  case sc.token of
    weNumber: num:=sc.GetInt32;
    weIdent: id:=sc.resText;
  else
    ErrorExpectButFound(res, 'index');
    Result := false;
  end;
  Result := true;
  if Result then sc.Next;
end;

function TokenTypeToValType(t: TWatToken; out tp: byte): Boolean;
begin
  Result:=true;
  case t of
    wei32: tp:=valtype_i32;
    wei64: tp:=valtype_i64;
    wef32: tp:=valtype_f32;
    wef64: tp:=valtype_f64;
  else
    tp:=0;
    Result:=false;
  end;
end;

function ParseParam(sc: TWatScanner; out id: string; out tp: byte; var res: TParseResult): Boolean;
begin
  tp:=0;
  id:='';
  if sc.token=weParam then sc.Next;

  if sc.token=weIdent then begin
    id:=sc.resText;
    sc.Next;
  end;

  if not TokenTypeToValType(sc.token, tp) then begin
    ErrorExpectButFound(res, 'type');
    Result:=false;
    Exit;
  end else
    Result:=true;
  sc.Next;
  Result := sc.token=weCloseBrace;
  if Result then sc.Next
  else ErrorExpectButFound(res, ')');
end;

function ParseFunc(sc: TWatScanner; dst: TWasmFunc; var res: TParseResult): Boolean;
var
  nm : integer;
  id : string;
  p  : TWasmParam;
begin
  if sc.token=weFunc then sc.Next;
  repeat
    if sc.token=weIdent then begin
      dst.id:=sc.resText;
      sc.Next;
    end;

    Result:=false;
    if sc.token=weOpenBrace then begin
      sc.Next;
      case sc.token of
        weType: begin
          if not ParseNumOfId(sc, nm, id, res) then Exit;
          if nm>=0 then dst.typeIdx:=nm
          else dst.typeId:=id;
        end;
        weParam: begin
          sc.Next;
          p:=dst.GetInlineType.AddParam;
          if not ParseParam(sc, p.id, p.tp, res) then Exit;
        end;
        weResult: begin
          sc.Next;
          p:=dst.GetInlineType.AddResult;
          if not ParseParam(sc, p.id, p.tp, res) then Exit;
        end;
        weLocal: begin
          sc.Next;
          p:=dst.AddLocal;
          if not ParseParam(sc, p.id, p.tp, res) then Exit;
        end;
      else
        ErrorUnexpected(res, 'booh');
        Exit;
      end;
      if not ConsumeToken(sc, weCloseBrace, res) then Exit;
    end;

  until sc.token=weCloseBrace;
  sc.Next;
end;

function ParseModule(sc: TWatScanner; dst: TWasmModule; var res: TParseResult): Boolean;
begin
  if not ConsumeOpenToken(sc, weModule) then begin
    Result := false;
    Exit;
  end;

  repeat
    sc.Next;
    if sc.token=weOpenBrace then begin
      sc.Next;

      if sc.token = weFunc then begin
        Result := ParseFunc(sc, dst.AddFunc, res);
        if not Result then Exit;
      end;

    end else if sc.token<>weCloseBrace then begin
      ErrorUnexpected(res);
      Result := false;
      exit;
    end;

  until sc.token=weCloseBrace;
  Result := true;
end;

procedure ErrorUnexpected(var res: TParseResult; const tokenstr: string);
begin
  res.error:='unexpected token '+tokenstr;
end;

procedure ErrorUnexpectedEof(var res: TParseResult);
begin
  res.error:='unexpected end of file';
end;

procedure ErrorExpectButFound(var res: TParseResult; const expected, butfound: string);
begin
  res.error:=expected +' is expected';
  if butfound<>'' then
    res.error:=res.error+', but '+butfound+ ' found';
end;

end.
