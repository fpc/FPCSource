{
    This file is part of the Free Vision package

    Copyright (c) 1999-2022 by Peter Vreman

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  TColoredText is a descendent of TStaticText designed to allow the writing
  of colored text when color monitors are used.  With a monochrome or BW
  monitor, TColoredText acts the same as TStaticText.

  TColoredText is used in exactly the same way as TStaticText except that
  the constructor has an extra Byte parameter specifying the attribute
  desired.  (Do not use a 0 attribute, black on black).
}
{$IFNDEF FPC_DOTTEDUNITS}
{$ifdef FV_UNICODE}
unit UColorTxt;
{$else FV_UNICODE}
unit ColorTxt;
{$endif FV_UNICODE}
{$ENDIF FPC_DOTTEDUNITS}


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

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Objects,
{$ifdef FV_UNICODE}
  FreeVision.Ufvcommon, FreeVision.Udrivers, FreeVision.Uviews,
  FreeVision.Udialogs, FreeVision.Uapp,
{$else}
  FreeVision.Fvcommon, FreeVision.Drivers, FreeVision.Views,
  FreeVision.Dialogs, FreeVision.App,
{$endif}
  FreeVision.Fvconsts;
{$ELSE FPC_DOTTEDUNITS}
uses
  Objects,
{$ifdef FV_UNICODE}
  UFvcommon, UDrivers, UViews, Udialogs, UApp,
{$else}
  Fvcommon, Drivers, Views, Dialogs, App,
{$endif}
  Fvconsts;
{$ENDIF FPC_DOTTEDUNITS}

type
  PColoredText = ^TColoredText;
  TColoredText = object(TStaticText)
    Attr : Byte;
    constructor Init(var Bounds: TRect; const AText: Sw_String; Attribute : Byte);
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

constructor TColoredText.Init(var Bounds: TRect; const AText: Sw_String;
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
  S: Sw_String;
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
        while (P <= L) and (S[P] <> ' ') and ((S[P] <> #13) and (S[P] <> #10)) do Inc(P);
      until (P > L) or (P >= I + Size.X) or ((S[P] = #13) or (S[P] = #10));  { line ending #13 or #10 }
      if P > I + Size.X then
        if J > I then P := J else P := I + Size.X;
      if Center then J := (Size.X - P + I) div 2 else J := 0;
      MoveBuf(B[J], S[I], Color, P - I);
      while (P <= L) and (S[P] = ' ') do Inc(P);
      if (P <= L) and ((S[P] = #13) or (S[P] = #10)) then
      begin
        Center := False;
        if (S[P] = #13) then
        begin
          Inc(P);
          if (P <= L) and (S[P] = #10) then Inc(P);  { line ending #13#10 }
        end else
        begin
          Inc(P);
          if (P <= L) and (S[P] = #13) then Inc(P);  { line ending #10#13 }
        end;
      end;
    end;
    WriteLine(0, Y, Size.X, 1, B);
    Inc(Y);
  end;
end;


end.
