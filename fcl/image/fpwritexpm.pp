{$mode objfpc}{$h+}
unit FPWriteXPM;

interface

uses FPImage, classes, sysutils;

type
  TFPWriterXPM = class (TFPCustomImageWriter)
    private
      FPalChars : string;
    protected
      procedure InternalWrite (Str:TStream; Img:TFPCustomImage); override;
    public
      constructor Create; override;
      property PalChars : string read FPalChars write FPalChars;
  end;


implementation

const
  DefPalChars = '.,-*abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789@#;:=+%$()[]';

constructor TFPWriterXPM.create;
begin
  inherited create;
  PalChars := DefPalChars;
end;

function ColorToHex (c:TFPColor; size:integer) : string;
var fmt : string;
    l : integer;
begin
  with c do
    write ('color=',red,',',green,',',blue,',',alpha);
  l := size div 3;
  fmt := inttostr(l);
  fmt := '%'+fmt+'.'+fmt+'x';
  fmt := fmt+fmt+fmt;
  with c do
    result := format(fmt,[red,green,blue]);
end;

procedure TFPWriterXPM.InternalWrite (Str:TStream; Img:TFPCustomImage);
var p, l : TStringList;
    c, len, r, t : integer;
  procedure BuildPaletteStrings;
  var r,c,e : integer;
    procedure MakeCodes (const head:string; charplace:integer);
    var r : integer;
    begin
      r := 1;
      dec (charplace);
      while (r <= e) and (c >= 0) do
        begin
        if Charplace = 1 then
          MakeCodes (head+PalChars[r],charplace)
        else
          p.Add (head+PalChars[r]);
        inc (r);
        dec(c);
        end;
    end;
  begin
    // Calculate length of codes
    len := 1;
    e := length(PalChars);
    r := e;
    c := img.palette.count;
    while (r <= c) do
      begin
      inc (len);
      r := r * e;
      end;
    MakeCodes ('',len);
  end;
var s : string;
begin
  l := TStringList.Create;
  p := TStringList.Create;
  try
    l.Add ('/* XPM */');
    l.Add ('static char *graphic[] = {');
    c := img.palette.count;
    BuildPaletteStrings;
    l.add (format('"%d %d %d %d",',[img.width,img.height,c,len]));
    for r := 0 to c-1 do
      begin
      if img.palette[r] <> colTransparent then
        l.Add (format('"%s c #%s",',[p[r],ColorToHex(img.palette.color[r],12)]))
      else
        l.Add (format('"%s c None",',[p[r]]));
      end;
    for r := 0 to img.Height-1 do
      begin
      s := p[img.pixels[0,r]];
      for t := 1 to img.Width-1 do
        s := s + p[img.pixels[t,r]];
      s := '"'+s+'"';
      if r < img.Height-1 then
        s := s + ',';
      l.Add (s);
      end;
    l.Add ('};');
  finally
    l.SaveToStream (Str);
    p.Free;
    l.Free;
  end;
end;

initialization
  ImageHandlers.RegisterImageWriter ('XPM Format', 'xpm', TFPWriterXPM);
end.
