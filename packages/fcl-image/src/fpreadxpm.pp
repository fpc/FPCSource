{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    XPM reader class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
{$IFNDEF FPC_DOTTEDUNITS}
unit FPReadXPM;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses FpImage, System.Classes, System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses FpImage, classes, sysutils;
{$ENDIF FPC_DOTTEDUNITS}

type
  TFPReaderXPM = class (TFPCustomImageReader)
    private
      width, height, ncols, cpp, xhot, yhot : integer;
      xpmext : boolean;
      palette : TStringList;
      function HexToColor(s : AnsiString) : TFPColor;
      function NameToColor(s : AnsiString) : TFPColor;
      function DiminishWhiteSpace (s : AnsiString) : AnsiString;
    protected
      procedure InternalRead  (Str:TStream; Img:TFPCustomImage); override;
      function  InternalCheck (Str:TStream) : boolean; override;
    public
      constructor Create; override;
      destructor Destroy; override;
  end;

implementation

{$i x11colors.inc}

const
  WhiteSpace = ' '#9#10#13;

constructor TFPReaderXPM.create;
begin
  inherited create;
  palette := TStringList.Create;
end;

destructor TFPReaderXPM.Destroy;
begin
  Palette.Free;
  inherited destroy;
end;

function TFPReaderXPM.HexToColor(s : AnsiString) : TFPColor;
var l : integer;
  function CharConv (c : AnsiChar) : longword;
  begin
    if (c >= 'A') and (c <= 'F') then
      result := ord (c) - ord('A') + 10
    else if (c >= '0') and (c <= '9') then
      result := ord (c) - ord('0')
    else
      raise FPImageException.CreateFmt ('Wrong character (%s) in hexadecimal number', [c]);
  end;
  function convert (n : AnsiString) : word;
  var r: integer;
      v: longword;
  begin
    v := 0;
    for r := 1 to length(n) do
      v := (v shl 4) or CharConv(n[r]);
    // fill missing bits
    case length(n) of
      1: v := v * $1111;
      2: v := v * $101;
      3: v := (v shl 4) or (v shr 8);
    end;
    result := v;
  end;
begin
  s := uppercase (s);
  if (length(s) = 0) or (length(s) > 12) or (length(s) mod 3 <> 0) then
    raise FPImageException.CreateFmt ('Invalid hexadecimal color (#%s)',[s]);
  l := length(s) div 3;
  result.red   := (Convert(copy(s,1,l)));
  result.green := (Convert(copy(s,l+1,l)));
  result.blue  :=  Convert(copy(s,l+l+1,l));
  result.alpha := AlphaOpaque;
end;

// Looks up aName in the sorted X11 colour table; returns -1 when absent.
function FindX11Color(const aName : AnsiString) : integer;
var lo, hi, mid, cmp : integer;
begin
  lo := Low(X11Colors);
  hi := High(X11Colors);
  while lo <= hi do
    begin
    mid := (lo + hi) div 2;
    cmp := CompareStr(X11Colors[mid].Name, aName);
    if cmp = 0 then
      exit(mid)
    else if cmp < 0 then
      lo := mid + 1
    else
      hi := mid - 1;
    end;
  result := -1;
end;


function TFPReaderXPM.NameToColor(s : AnsiString) : TFPColor;
var i : integer;
begin
  s := StringReplace(lowercase(s), ' ', '', [rfReplaceAll]);
  if (s = 'none') or (s = 'transparent') then
    exit(colTransparent);
  i := FindX11Color(s);
  if i >= 0 then
    with X11Colors[i] do
      exit(FPColor(((RGB shr 16) and $FF) * 257, ((RGB shr 8) and $FF) * 257, (RGB and $FF) * 257));
  if s = 'ltgray' then
    result := colLtGray
  else if s = 'dkblue' then
    result := colDkBlue
  else if s = 'dkgreen' then
    result := colDkGreen
  else if s = 'dkcyan' then
    result := colDkCyan
  else if s = 'dkred' then
    result := colDkRed
  else if s = 'dkmagenta' then
    result := colDkMagenta
  else if s = 'dkyellow' then
    result := colDkYellow
  else if s = 'ltgreen' then
    result := colLtGreen
  else if s = 'olive' then
    result := colOlive
  else if s = 'teal' then
    result := colTeal
  else if s = 'silver' then
    result := colSilver
  else if s = 'lime' then
    result := colLime
  else if s = 'fuchsia' then
    result := colFuchsia
  else if s = 'aqua' then
    result := colAqua
  else
    result := colTransparent;
end;

function TFPReaderXPM.DiminishWhiteSpace (s : AnsiString) : AnsiString;
var r : integer;
    Doit : boolean;
begin
  Doit := true;
  result := '';
  for r := 1 to length(s) do
    if pos(s[r],WhiteSpace)>0 then
      begin
      if DoIt then
        result := result + ' ';
      DoIt := false;
      end
    else
      begin
      DoIt := True;
      result := result + s[r];
      end;
end;

procedure TFPReaderXPM.InternalRead  (Str:TStream; Img:TFPCustomImage);
var l : TStringList;

  procedure TakeInteger (var s : AnsiString; var i : integer);
  var r : integer;
      w : AnsiString;
  begin
    r := pos (' ', s);
    if r = 0 then
      r := length(s) + 1;
    w := copy(s,1,r-1);
    if not TryStrToInt(w, i) then
      raise FPImageException.CreateFmt ('Invalid number in XPM header: %s',[w]);
    delete (s, 1, r);
  end;

  procedure ParseFirstLine;
  var s : AnsiString;
  begin
    s := l[0];
    // diminish all whitespace to 1 blank
    s := DiminishWhiteSpace (trim(s));
    Takeinteger (s, width);
    Takeinteger (s, height);
    Takeinteger (s, ncols);
    Takeinteger (s, cpp);
    xhot := -1;
    yhot := -1;
    if (s <> '') and (comparetext(s, 'XPMEXT') <> 0) then
      begin
      Takeinteger (s, xhot);
      Takeinteger (s, yhot);
      end;
    xpmext := (comparetext(s, 'XPMEXT') = 0);
    if (s <> '') and not xpmext then
      Raise FPImageException.Create ('Wrong word for XPMEXT tag');
  end;

  procedure AddPalette (const code:AnsiString;const Acolor:TFPColor);
  var r : integer;
  begin
    r := Palette.Add(code);
    img.palette.Color[r] := Acolor;
  end;

  function IsKey(const aWord : AnsiString) : boolean;
  begin
    result := (aWord = 'c') or (aWord = 'm') or (aWord = 's') or (aWord = 'g') or (aWord = 'g4');
  end;

  procedure AddToPalette(s : AnsiString);
  var code, key, value : AnsiString;
      words : array of AnsiString;
      c : TFPColor;
      i, sp : integer;
  begin
    code := copy(s,1,cpp);
    s := trim(diminishWhiteSpace (copy(s,cpp+1,maxint)));
    if s = '' then
      raise FPImageException.Create('Empty color specification in XPM');
    // key/value pairs; a value runs until the next key word
    words := nil;
    repeat
      sp := pos(' ', s);
      if sp = 0 then
        sp := length(s) + 1;
      SetLength(words, Length(words) + 1);
      words[High(words)] := copy(s, 1, sp - 1);
      delete(s, 1, sp);
    until s = '';
    i := 0;
    while i < Length(words) do
      begin
      key := words[i];
      inc(i);
      value := '';
      while (i < Length(words)) and ((value = '') or not IsKey(words[i])) do
        begin
        if value <> '' then
          value := value + ' ';
        value := value + words[i];
        inc(i);
        end;
      if key = 'c' then
        s := value;
      end;
    // check if exists
    if s = '' then
      raise FPImageException.Create ('Only c-key is used for colors');
    // convert #hexadecimal value to integer and place in palette
    if s[1] = '#' then
      c := HexToColor(copy(s,2,maxint))
    else
      c := NameToColor(s);
    AddPalette(code,c);
  end;

  procedure ReadPalette;
  var r : integer;
  begin
    Palette.Clear;
    Img.Palette.Count := ncols;
    for r := 1 to ncols do
      AddToPalette (l[r]);
  end;

  procedure ReadLine (const s : AnsiString; imgindex : integer);
  var color, r, p : integer;
      code : AnsiString;
  begin
    p := 1;
    for r := 1 to width do
      begin
      code := copy(s, p, cpp);
      inc(p,cpp);
      for color := 0 to Palette.Count-1 do
        { Can't use indexof, as compare must be case sensitive }
        if code = Palette[color] then begin
          img.pixels[r-1,imgindex] := color;
          Break;
        end;
      end;
  end;

  procedure ReadData;
  var r : integer;
  begin
    for r := 1 to height do
      ReadLine (l[ncols+r], r-1);
  end;

var p, r : integer;
begin
  l := TStringList.Create;
  try
    l.LoadFromStream (Str);
    for r := l.count-1 downto 0 do
      begin
      p := pos ('"', l[r]);
      if p > 0 then
        l[r] := copy(l[r], p+1, lastdelimiter('"',l[r])-p-1)
      else
        l.delete(r);
      end;
    if l.Count = 0 then
      raise FPImageException.Create('Missing XPM header');
    ParseFirstLine;
    if (width <= 0) or (height <= 0) or (ncols <= 0) or (cpp <= 0) then
      raise FPImageException.Create('Invalid XPM header values');
    if (width > 65535) or (height > 65535) or (ncols > 65536) then
      raise FPImageException.Create('XPM dimensions too large');
    if l.Count <= ncols + height then
      raise FPImageException.Create('XPM data truncated');
    Img.SetSize (width, height);
    Img.UsePalette := True;
    ReadPalette;
    ReadData;
  finally
    l.Free;
  end;
end;

function  TFPReaderXPM.InternalCheck (Str:TStream) : boolean;
var s : String[9];
    l : integer;
begin
  try
    l := str.Read (s[1],9);
    s[0] := AnsiChar(l);
    if l <> 9 then
      result := False
    else
      result := (s = '/* XPM */');
  except
    result := false;
  end;
end;

initialization
  ImageHandlers.RegisterImageReader ('XPM Format', 'xpm', TFPReaderXPM);
end.
