unit ColorTxt;

{
  TColoredText is a descendent of TStaticText designed to allow the writing
  of colored text when color monitors are used.  With a monochrome or BW
  monitor, TColoredText acts the same as TStaticText.

  TColoredText is used in exactly the same way as TStaticText except that
  the constructor has an extra Byte parameter specifying the attribute
  desired.  (Do not use a 0 attribute, black on black).
}

{$i platform.inc}

{$ifdef PPC_FPC}
  {$H-}
{$else}
  {$F+,O+,E+,N+}
{$endif}
{$X+,R-,I-,Q-,V-}
{$ifndef OS_UNIX}
  {$S-}
{$endif}

interface

uses
  objects, drivers, views, dialogs, app, fvconsts;

type
  PColoredText = ^TColoredText;
  TColoredText = object(TStaticText)
    Attr : Byte;
    constructor Init(var Bounds: TRect; const AText: String; Attribute : Byte);
    constructor Load(var S: TStream);
    function GetTheColor : byte; virtual;
    procedure Draw; virtual;
    procedure Store(var S: TStream);
  end;

const
  RColoredText: TStreamRec = (
     ObjType: idColoredText;
     VmtLink: Ofs(TypeOf(TColoredText)^);
     Load:    @TColoredText.Load;
     Store:   @TColoredText.Store
  );

implementation

constructor TColoredText.Init(var Bounds: TRect; const AText: String;
                                  Attribute : Byte);
begin
TStaticText.Init(Bounds, AText);
Attr := Attribute;
end;

constructor TColoredText.Load(var S: TStream);
begin
TStaticText.Load(S);
S.Read(Attr, Sizeof(Attr));
end;

procedure TColoredText.Store(var S: TStream);
begin
TStaticText.Store(S);
S.Write(Attr, Sizeof(Attr));
end;

function TColoredText.GetTheColor : byte;
begin
if AppPalette = apColor then
  GetTheColor := Attr
else
  GetTheColor := GetColor(1);
end;

procedure TColoredText.Draw;
var
  Color: Byte;
  Center: Boolean;
  I, J, L, P, Y: Sw_Integer;
  B: TDrawBuffer;
  S: String;
begin
  Color := GetTheColor;
  GetText(S);
  L := Length(S);
  P := 1;
  Y := 0;
  Center := False;
  while Y < Size.Y do
  begin
    MoveChar(B, ' ', Color, Size.X);
    if P <= L then
    begin
      if S[P] = #3 then
      begin
        Center := True;
        Inc(P);
      end;
      I := P;
      repeat
        J := P;
        while (P <= L) and (S[P] = ' ') do Inc(P);
        while (P <= L) and (S[P] <> ' ') and (S[P] <> #13) do Inc(P);
      until (P > L) or (P >= I + Size.X) or (S[P] = #13);
      if P > I + Size.X then
        if J > I then P := J else P := I + Size.X;
      if Center then J := (Size.X - P + I) div 2 else J := 0;
      MoveBuf(B[J], S[I], Color, P - I);
      while (P <= L) and (S[P] = ' ') do Inc(P);
      if (P <= L) and (S[P] = #13) then
      begin
        Center := False;
        Inc(P);
        if (P <= L) and (S[P] = #10) then Inc(P);
      end;
    end;
    WriteLine(0, Y, Size.X, 1, B);
    Inc(Y);
  end;
end;


end.
