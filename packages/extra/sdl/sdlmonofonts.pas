unit sdlmonofonts;
{******************************************************************}
{                                                                  }
{ SDL_MonoFonts unit by RÛbert KisnÈmeth (KiCHY)                   }
{ This unit is part of SDLGui by RÛbert KisnÈmeth, but works       }
{ without it. Use and distribute it freely in its unaltered state. }
{                                                                  }
{ If you wish supporting languages other than English & Hungarian  }
{ send me a letter and I try to implement it (but not Cyrillic or  }
{ Chinese or something exotic charset, please. Only a few letters.)}
{ I know p.e. the French or Spanish (or Finnish) have special      }
{ characters like us, Hungarians, but I'm very lazy...             }
{                                                                  }
{ E-mail: mikrobi@freemail.hu                                      }
{                                                                  }
{ Revision History                                                 }
{ ----------------                                                 }
{ September 21 2001 - RK : Initial v1.0 version                    }
{ October   28 2001 - RK : v1.01 Fixed a bug which found by        }
{                          Wojciech (brombapl@yahoo.com)           }
{                                                                  }
{******************************************************************}

{$I jedi-sdl.inc}

interface

uses
  Classes,
  SysUtils,
  sdl,
  sdl_image,
  sdlutils;

const
 CharSet = ' !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~·ÈÌÛˆı˙¸˚¡…Õ”÷’⁄‹€';

type
  TAlignment = (taLeftJustify, taRightJustify, taCenter);
  
  PFont = ^TFont;
  TFont = object
  private
   Image: PSDL_Surface;
   Rects: array[0..112] of PSDL_Rect;
   procedure WriteText2(x, y: integer; Txt: Pchar; TextLength: cardinal);
  public
   TransparentColor, TextColor: cardinal;
   Surface: PSDL_Surface;
   function Height: integer;
   constructor Initialize(const Filename: string);
   destructor Finalize;
   procedure LoadFont(const Fontfile: string);
   procedure FreeUpAll;
   procedure WriteText(x, y: integer; Txt: PChar; Align: TAlignment);
   procedure WriteTextWrapped(Rect: PSDL_Rect; Txt: PChar; Align: TAlignment);
   function TextWidth: integer;
   function WidthOf(Txt: PChar; Len: cardinal): integer; overload;
   function WidthOf(Txt: PChar): integer; overload;
  end;

implementation

constructor TFont.Initialize(const Filename: string);
begin
 LoadFont(Filename);
end;

procedure TFont.LoadFont(const Fontfile: string);
var
 i, x , width: integer;
 Separator: Cardinal;
begin
 FreeUpAll;
 if not fileexists(Fontfile) then exit;
 Image:=IMG_Load(pchar(Fontfile));
 if Image = nil then exit;
 Separator:=SDL_MapRGB(Image.format, 255, 0, 255);
 x:=0; i:=0;
 repeat
  // Search first/next separator
  while SDL_GetPixel(Image, x, 0)<>Separator do inc(x);
  // Determine character's width
  if x>=Image.w then break;
  Width:=1;
  while SDL_GetPixel(Image, x + Width, 0) = Separator do
    inc(Width);
  Rects[i]:=PSDLRect(x, 1, Width, Image.h-1);
  inc(i);
  inc(x, Width+1);
 until x>=Image.w;
 // Determine the transparent color
 TransparentColor:=SDL_GetPixel(Image, Rects[0].x+Rects[0].w, 0);
 SDL_SetColorKey(Image, SDL_SRCCOLORKEY, TransparentColor);
 TextColor:=SDL_MapRGB(Image.format, 0, 0, 0);
end;

procedure TFont.FreeUpAll;
var
 i: integer;
begin
 for i:=0 to 112 do
  if Rects[i]<>nil then Dispose(Rects[i]);
 if Image<>nil then SDL_FreeSurface(Image);
end;

destructor TFont.Finalize;
begin
 FreeUpAll;
end;

// Read a word from a string until its end or CRLF
procedure ReadWord(Txt: PChar; StartPos: cardinal; var FoundWord: PChar; var ItsLength: cardinal);
var
 WasLetter: boolean;
 ReadPos, TextLength: integer;
begin
 TextLength:=length(Txt);
 WasLetter:=false;
 ReadPos:=StartPos;
 repeat
  case Txt[ReadPos] of
   ' ': if WasLetter=true then break;
   #13: begin
         inc(ReadPos,1);
         break;
        end; 
   else WasLetter:=true
  end;
  inc(ReadPos);
 until ReadPos>=TextLength;
 FoundWord:=pointer(cardinal(Txt)+StartPos);
 ItsLength:=ReadPos-StartPos;
end;

function ContainsCR(Txt: PChar; Len: cardinal):boolean;
var
 i: integer;
begin
 result:=false;
 for i:=0 to Len-1 do
  if Txt[i]=#13 then begin
   Result:=true;
   exit;
  end;
end;

procedure TFont.WriteTextWrapped(Rect: PSDL_Rect; Txt: PChar; Align: TAlignment);
var
 Original_Clip_Rect: TSDL_Rect; // Store the original clipping rectangle
 ReadFrom: cardinal; // Reading position
 TextLength: cardinal; // The whole text's length
 FoundWord: PChar; //  The word we found
 WordLen: cardinal; // Length of the word we found
 Area: TSDL_Rect; // The rectangle we draw in
 RowLengthInPixels: cardinal; // Stores a row's length in pixels
 RowLengthInChars: cardinal; // Stores a row's length in chars
 FoundRow: PChar; // The row we will write out
 x, y: integer; // Drawing position
 NextWordsLengthInPixels: cardinal;
begin
 if (Surface=nil) or (Image=nil) or (Txt=nil) or (Txt='') then exit;
 Original_Clip_Rect:=Surface.Clip_Rect;
 if Rect=nil then Area:=Surface.Clip_Rect
  else Area:=Rect^;
 Surface.Clip_Rect:=Area;
 ReadFrom:=0;
 x:=Area.x;
 y:=Area.y;
 TextLength:=length(Txt);
 repeat
  // Collect words until it don't fit in Area's width
  // A row always contains minimum one word
  ReadWord(Txt, ReadFrom, FoundRow, WordLen); // Read a whole word from text
  RowLengthInPixels:=WidthOf(FoundRow, WordLen);
  RowLengthInChars:=WordLen;
  ReadFrom:=ReadFrom+WordLen+1; // Advance to next word
  // Read more words if it fits in Area's width
  repeat
   if ContainsCR(FoundRow, RowLengthInChars) then break; // We found a CR so break the line
   ReadWord(Txt, ReadFrom, FoundWord, WordLen); // Read a whole word from text
   NextWordsLengthInPixels:=WidthOf(FoundWord, WordLen);
   if RowLengthInPixels+Rects[0].w+1+NextWordsLengthInPixels<Area.w then begin
    RowLengthInPixels:=RowLengthInPixels+Rects[0].w+1+NextWordsLengthInPixels;
    RowLengthInChars:=RowLengthInChars+1+WordLen;
    ReadFrom:=ReadFrom+WordLen+1; // Advance to next word
   end else break;
  until (RowLengthInPixels>=Area.w) or (ReadFrom>=TextLength);
  // calculate alignment
  case Align of
   taLeftJustify:  x:=Area.x;
   taCenter:       x:=(Area.x+Area.w shr 1)-(WidthOf(FoundRow, RowLengthInChars)-1) shr 1;
   taRightJustify: x:=Area.x+Area.w-WidthOf(FoundRow, RowLengthInChars)+1;
  end;
  WriteText2(x, y, FoundRow, RowLengthInChars);
  y:=y+Rects[0].h;
 until (y>=Area.y+Area.h) or (ReadFrom>=TextLength);
 Surface.Clip_Rect:=Original_Clip_Rect;
end;

// Draw a text in a single line with clipping x & y
procedure TFont.WriteText(x, y: integer; Txt: Pchar; Align: TAlignment);
var
 i, len, ch, px, py: integer;
 TargetX: integer; // writing position after aligning
begin
 if (Surface=nil) or (Image=nil) or (Txt=nil) or (Txt='') then exit;
 SDL_LockSurface(Surface);
 SDL_LockSurface(Image);
 i:=0;
 len:=length(txt);
 case Align of
  taLeftJustify:  TargetX:=x;
  taCenter:       TargetX:=x-(WidthOf(Txt)-1) shr 1;
  taRightJustify: TargetX:=x-WidthOf(Txt)+1;
 end;
 while i<len do begin
  if x>=Surface.Clip_Rect.x+Surface.Clip_Rect.w then break; // We reached the right side
  ch:=pos(Txt[i], Charset)-1;
  if (ch>=0) and (ch<113) then begin
   for px:=0 to Rects[ch]^.w-1 do
    if (TargetX+px >= Surface.Clip_Rect.x) and // Clip from left
       (TargetX+px<Surface.Clip_Rect.x+Surface.Clip_Rect.w) then // Clip from right
     for py:=0 to Rects[ch]^.h-1 do
      if y+py<Surface.Clip_Rect.y+Surface.Clip_Rect.h then // if we don't reach the bottom border
       if SDL_GetPixel(Image, Rects[ch]^.x+px, Rects[ch]^.y+py)<>TransparentColor then
        SDL_PutPixel(Surface, TargetX+px, y+py, TextColor);
   TargetX:=TargetX+Rects[ch].w+1;
  end;
  inc(i);
 end;
 SDL_UnlockSurface(Surface);
 SDL_UnlockSurface(Image);
end;

// Draw a partial text in a single line without clipping x
procedure TFont.WriteText2(x, y: integer; Txt: Pchar; TextLength: cardinal);
var
 i, ch, px, py: integer;
begin
 if (Surface=nil) or (Image=nil) or (Txt=nil) or (Txt='') then exit;
 SDL_LockSurface(Surface);
 SDL_LockSurface(Image);
 i:=0;
 while i<TextLength do begin
  ch:=pos(Txt[i], CharSet)-1;
  if (ch>=0) and (ch<113) then begin
   for px:=0 to Rects[ch]^.w-1 do
    for py:=0 to Rects[ch]^.h-1 do
     if y+py<Surface.Clip_Rect.y+Surface.Clip_Rect.h then
      if SDL_GetPixel(Image, Rects[ch]^.x+px, Rects[ch]^.y+py)<>TransparentColor then
       SDL_PutPixel(Surface, x+px, y+py, TextColor);
   x:=x+Rects[ch].w+1;
  end;
  inc(i);
 end;
 SDL_UnlockSurface(Surface);
 SDL_UnlockSurface(Image);
end;

function TFont.TextWidth: integer;
begin
 Result:=0;
end;

function TFont.WidthOf(Txt: PChar; Len: cardinal): integer;
var
 i: cardinal;
 p: integer;
begin
 Result:=0;
 for i:=0 to Len-1 do begin
  p:=pos(Txt[i], CharSet)-1;
  if p>=0 then Result:=Result+Rects[p].w+1;
 end; 
end;

function TFont.WidthOf(Txt: PChar): integer;
var
 i, len: cardinal;
 p: integer;
begin
 Result:=0;
 Len:=Length(Txt);
 for i:=0 to Len-1 do begin
  p:=pos(Txt[i], CharSet)-1;
  if p>=0 then Result:=Result+Rects[p].w+1;
 end;
end;

function TFont.Height: integer;
begin
 if Image<>nil then Result:=Image.h else Result:=0;
end;

end.

