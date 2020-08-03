unit watparser;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes, parseutils;

type
  TWatEntity = (weNone, weError,
     weIdent, weString, weNumber, weSymbol, weOpenBrace, weCloseBrace);

  { TWatParser }

  TWatParser = class(TObject)
  protected
    procedure DoComment(const cmt: string);
    function CommentIsSymbol(const cmt: string): Boolean;
  public
    buf : string;
    idx : integer;

    ofs : integer;
    entity : TWatEntity;
    resText : string;
    procedure SetSource(const abuf: string);
    function Next: Boolean;
  end;

const
  SymbolChars : TCharSet = ['(',')'];
  AlphaNumCharsUnd : TCharSet = AlphaNumChars + ['_','$'];

implementation

{ TWatParser }

procedure TWatParser.DoComment(const cmt: string);
begin

end;

function TWatParser.CommentIsSymbol(const cmt: string): Boolean;
begin
  Result := false;
end;

procedure TWatParser.SetSource(const abuf: string);
begin
  buf:=abuf;
  idx:=1;
end;

function TWatParser.Next: Boolean;
var
  has2chars: Boolean;
  cmt : string;
  done: boolean;
  j: integer;
begin
  Result := idx<=length(buf);
  if not Result then Exit;

  done:=false;
  resText:='';
  while not done do begin
    ScanWhile(buf, idx, WhiteSpaceChars);
    Result := idx<=length(buf);
    if not Result then Exit;
    j:=idx;
    has2chars := idx<length(buf);
    if has2chars then begin
      if (buf[idx]=';') and (buf[idx+1]=';') then begin
        // comment until the end of the line
        cmt := ScanTo(buf, idx, EoLnChars);
        ScanWhile(buf, idx, EoLnChars);
      end else if (buf[idx]='(') and (buf[idx+1]=';') then
        // comment until the ;)
        cmt := ScanToSubstr(buf, idx, ';)');

      if CommentIsSymbol(cmt) then begin
        entity:=weSymbol;
        done:=true;
      end else
        DoComment(cmt);
    end;

    if not done then begin
      done:=true;
      if buf[idx] = '(' then begin
        entity:=weOpenBrace;
        inc(idx);
      end else if buf[idx]=')' then begin
        entity:=weCloseBrace;
        inc(idx);
      end else if buf[idx] in AlphabetChars then begin
        entity:=weIdent;
        resText:=ScanWhile(buf, idx, AlphaNumCharsUnd);
      end else if buf[idx] in NumericChars then begin
        entity:=weNumber;
        resText:=ScanWhile(buf, idx, NumericChars);
      end else begin
        entity:=weError;
        inc(idx);
        done:=true;
      end;
    end;
  end;

  if resText='' then
    resText := Copy(buf, j, idx-j);
end;

end.
