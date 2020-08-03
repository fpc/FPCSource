unit watscanner;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes, parseutils, wasmtext;

type
  TWatToken = (weNone, weError,
     weIdent,
     weString, weNumber, weOpenBrace, weCloseBrace,
     weAsmSymbol,

     weInstr,
     weFunc,
     weParam, weResult,
     weModule, weMut, weFuncRef,
     wei32, wei64,
     wef32, wef64,
     weType,
     weImport, weGlobal, weTable, weMemory, weLocal, weExport,
     weElem, weData, weOffset
   );

  { TWatScanner }

  TWatScanner = class(TObject)
  protected
    procedure DoComment(const cmt: string);
    function CommentIsSymbol(const cmt: string): Boolean;
  public
    buf       : string;
    idx       : integer;

    instrCode : byte;
    ofs       : integer;
    token     : TWatToken;
    resText   : string;
    procedure SetSource(const abuf: string);
    function Next: Boolean;

    function resInt32(const def: integer=-1): Integer;
    function resWasmString: string;
  end;

const
  // see Identifiers of Textual format
  IdStart  = '$';
  IdBody   = AlphaNumChars
             + [ '!' ,'#' ,'$' ,'%' ,'&' ,'''' ,'*'
                ,'+' ,'-' ,'.' ,'/' ,':' ,'<' ,'='
                ,'>' ,'?' ,'@' ,'\' ,'^' ,'_' ,'`'
                ,'|' ,'~'];
  GrammarChars  = AlphaNumChars+['.','_'];

procedure GetGrammar(const txt: string; out entity: TWatToken; out instByte: byte);

const
  KEY_MODULE = 'module';
  KEY_FUNC   = 'func';
  KEY_FUNCREF = 'funcref';
  KEY_I32    = 'i32';
  KEY_I64    = 'i64';
  KEY_F32    = 'f32';
  KEY_F64    = 'f64';
  KEY_PARAM  = 'param';
  KEY_RESULT = 'result';
  KEY_MUT    = 'mut';
  KEY_TYPE   = 'type';

  KEY_IMPORT = 'import';
  KEY_GLOBAL = 'global';
  KEY_TABLE  = 'table';
  KEY_MEMORY = 'memory';
  KEY_LOCAL  = 'local';
  KEY_EXPORT = 'export';
  KEY_ELEM   = 'elem';
  KEY_DATA   = 'data';
  KEY_OFFSET = 'offset';

function ScanString(const buf: string; var idx: integer): string;

implementation

procedure GetGrammar(const txt: string; out entity: TWatToken; out instByte: byte);
begin
  instByte:=0;
  entity:=weError;
  if txt='' then Exit;
  case txt[1] of
    'a':
      if txt='anyfunc' then entity:=weFuncRef
      else if TextToInst(txt, instByte) then entity:=weInstr;
    'd':
      if txt=KEY_DATA then entity:=weData
      else if TextToInst(txt, instByte) then entity:=weInstr;
    'e':
      if txt=KEY_EXPORT then entity:=weExport
      else if txt=KEY_ELEM then entity:=weElem
      else if TextToInst(txt, instByte) then entity:=weInstr;
    'i':
      if txt=KEY_I32 then entity:=wei32
      else if txt=KEY_I64 then entity:=wei64
      else if txt=KEY_IMPORT then entity:=weImport
      else if TextToInst(txt, instByte) then entity:=weInstr;
    'g':
      if txt=KEY_GLOBAL then entity:=weGlobal
      else if TextToInst(txt, instByte) then entity:=weInstr;
    'f':
      if txt=KEY_FUNC then entity:=weFunc
      else if txt=KEY_FUNCREF then entity:=weFuncRef
      else if txt=KEY_F32 then entity:=wef32
      else if txt=KEY_F64 then entity:=wef64
      else if TextToInst(txt, instByte) then entity:=weInstr;
    'l':
      if txt=KEY_LOCAL then entity:=weLocal
      else if TextToInst(txt, instByte) then entity:=weInstr;
    'm':
      if txt=KEY_MODULE then entity:=weModule
      else if txt = KEY_MUT then entity:=weMut
      else if txt = KEY_MEMORY then entity:=weMemory
      else if TextToInst(txt, instByte) then entity:=weInstr;
    'o':
      if txt=KEY_OFFSET then entity:=weOffset
      else if TextToInst(txt, instByte) then entity:=weInstr;
    'p':
      if txt=KEY_PARAM then entity:=weParam
      else if TextToInst(txt, instByte) then entity:=weInstr;
    'r':
      if txt=KEY_RESULT then entity:=weResult
      else if TextToInst(txt, instByte) then entity:=weInstr;
    't':
      if txt=KEY_TYPE then entity:=weType
      else if txt=KEY_TABLE then entity:=weTable
      else if TextToInst(txt, instByte) then entity:=weInstr;
  else
    if TextToInst(txt, instByte) then entity:=weInstr;
  end;
end;

{ TWatScanner }

procedure TWatScanner.DoComment(const cmt: string);
begin

end;

function TWatScanner.CommentIsSymbol(const cmt: string): Boolean;
begin
  Result := false;
end;

procedure TWatScanner.SetSource(const abuf: string);
begin
  buf:=abuf;
  idx:=1;
end;

function ScanString(const buf: string; var idx: integer): string;
var
  j : integer;
begin
  if buf[idx]<>'"' then begin
    Result:='';
    Exit;
  end;
  j:=idx;
  inc(idx);
  while (buf[idx]<>'"') and (idx<length(buf)) do begin
    if buf[idx]='\' then inc(idx);
    inc(idx);
  end;
  inc(idx);
  Result:=Copy(buf, j, idx-j);
end;

function TWatScanner.Next: Boolean;
var
  has2chars: Boolean;
  cmt : string;
  done: boolean;
begin
  Result := idx<=length(buf);
  if not Result then Exit;

  done:=false;
  resText:='';
  while not done do begin
    ScanWhile(buf, idx, SpaceEolnChars);
    Result := idx<=length(buf);
    if not Result then Exit;
    ofs:=idx;
    if (idx<length(buf)) and (buf[idx] in [';','(']) and (buf[idx+1]=';') then begin
      if (buf[idx]=';') then begin
        // comment until the end of the line
        cmt := ScanTo(buf, idx, EoLnChars);
        ScanWhile(buf, idx, EoLnChars);
      end else
        // comment until the ;)
        cmt := ScanToSubstr(buf, idx, ';)');

      if CommentIsSymbol(cmt) then begin
        token:=weAsmSymbol;
        done:=true;
      end else
        DoComment(cmt);
    end else begin
      done:=true;
      if buf[idx] = '(' then begin
        token:=weOpenBrace;
        inc(idx);
      end else if buf[idx]=')' then begin
        token:=weCloseBrace;
        inc(idx);
      end else if buf[idx]='"' then begin
        token:=weString;
        resText:=ScanString(buf, idx);
      end else if buf[idx] = IdStart then begin
        token:=weIdent;
        resText:=ScanWhile(buf, idx, IdBody);
      end else if buf[idx] in AlphaNumChars then begin
        resText:=ScanWhile(buf, idx, GrammarChars);
        GetGrammar(resText, token, instrCode);
        done:=true;
      end else if buf[idx] in NumericChars then begin
        token:=weNumber;
        resText:=ScanWhile(buf, idx, NumericChars);
      end else begin
        token:=weError;
        inc(idx);
        done:=true;
      end;
    end;
  end;

  if resText='' then
    resText := Copy(buf, ofs, idx-ofs);
end;

function TWatScanner.resInt32(const def: integer=-1): Integer;
var
  err: integer;
begin
  Val(resText, Result, err);
  if err<>0 then Result:=def;
end;

function TWatScanner.resWasmString: string;
var
  i : integer;
  j : integer;
begin
  if token<>weString then begin
    Result:='';
    Exit;
  end;
  Result:=Copy(resText, 2, length(resText)-2);
  if Result='' then Exit;

  i:=1;
  j:=1;
  while i<=length(Result) do begin
    if Result[i]='\' then begin
      inc(i);
      if i<=length(Result) then
        case Result[i] of
          'r': Result[j]:=#13;
          'n': Result[j]:=#10;
          '\': Result[j]:='\';
          '"': Result[j]:='"';
        end;
    end else
      if (j<i) then Result[j]:=Result[i];
    inc(j);
    inc(i);
  end;
  SetLength(Result, j-1);
end;



end.
