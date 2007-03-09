unit sfont;
{******************************************************************}
{
  $Id: sfont.pas,v 1.2 2004/03/31 09:04:31 savage Exp $

}
{                                                                  }
{       Borland Delphi SFont                                       }
{       Conversion of the Linux Games- SFont Library for SDL       }
{                                                                  }
{ Original work created by Karl Bartel  <karlb@gmx.net>            }
{ Copyright (C) 2003 Karl Bartel.                                  }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original files are : sfont.c                                 }
{                                                                  }
{ The original Pascal code is : SFont.pas                          }
{ The initial developer of the Pascal code is :                    }
{ Jason Farmer <jason@cerebral-bicycle.co.uk>                      }
{                                                                  }
{                                                                  }
{ Contributor(s)                                                   }
{ --------------                                                   }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                }
{                                                                  }
{ Obtained through:                                                }
{ Joint Endeavour of Delphi Innovators ( Project JEDI )            }
{                                                                  }
{ You may retrieve the latest version of this file at the Project  }
{ JEDI home page, located at http://delphi-jedi.org                }
{                                                                  }
{ The contents of this file are used with permission, subject to   }
{ the Mozilla Public License Version 1.1 (the "License"); you may  }
{ not use this file except in compliance with the License. You may }
{ obtain a copy of the License at                                  }
{ http://www.mozilla.org/NPL/NPL-1_1Final.html                     }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{                                                                  }
{ Description                                                      }
{ -----------                                                      }
{                                                                  }
{    SFONT - SDL Font Library by Karl Bartel <karlb@gmx.net>	   }
{                                                                  }
{  All functions are explained below.                              }
{  There are two versions of each                                  }
{  funtction. The first is the normal one,                         }
{  the function with the 2 at the end can be used when you         }
{  want to handle more than one font                               }
{  in your program.                                                }
{                                                                  }
{                                                                  }
{                                                                  }
{                                                                  }
{ Requires                                                         }
{ --------                                                         }
{   SDL runtime libary somewhere in your path                      }
{   The Latest SDL runtime can be found on http://www.libsdl.org   }
{                                                                  }
{ Programming Notes                                                }
{ -----------------                                                }
{                                                                  }
{                                                                  }
{                                                                  }
{                                                                  }
{                                                                  }
{ Revision History                                                 }
{ ----------------                                                 }
{   July    04 2001 - JF : Initial translation.                    }
{   Sept    29 2001 - JF : Added Róbert Surface Adding and         }
{                          Subtraction functions                   }
{                                                                  }
{
  $Log: sfont.pas,v $
  Revision 1.2  2004/03/31 09:04:31  savage
  Added jedi-sdl.inc files for better FreePascal/multi compiler support.

  Revision 1.1  2004/03/28 10:45:16  savage
  Standardised SFont Functions so that they are prefixed with SFont_ and more in line with Karl's v2.02 release. Demos have been updated appropriately.


}
{******************************************************************}

{$I jedi-sdl.inc}

interface

uses
  SysUtils,
  sdl,
  sdlutils;

// Delcare one variable of this type for each font you are using.
// To load the fonts, load the font image into YourFont->Surface
// and call InitFont( YourFont );
type
  TSfont_FontInfo = record
    Surface : PSDL_Surface; //SDL_Surface *Surface;
    CharPos : array[ 0..511 ] of integer; //int CharPos[512];
    MaxPos : integer; //int h;
  end;
  PSFont_FontInfo = ^TSfont_FontInfo;

  // Initializes the font
  // Font: this contains the suface with the font.
  //       The font must be loaded before using this function.
procedure SFont_InitFont( Font : PSDL_Surface );
procedure SFont_InitFont2( Font : PSFont_FontInfo );

// Blits a string to a surface
// Destination: the suface you want to blit to
// text: a string containing the text you want to blit.
procedure SFont_Write( Surface_ : PSDL_Surface; x : Integer; y : Integer; text :
  pchar );
procedure SFont_WriteAdd( Surface_ : PSDL_Surface; x : Integer; y : Integer; text :
  pchar );
procedure SFont_WriteSub( Surface_ : PSDL_Surface; x : Integer; y : Integer; text :
  pchar );

procedure SFont_Write2( Surface_ : PSDL_Surface; Font : PSFont_FontInfo; x : integer;
  y : integer; text : pchar );
procedure SFont_WriteAdd2( Surface_ : PSDL_Surface; Font : PSFont_FontInfo; x : integer;
  y : integer; text : pchar );
procedure SFont_WriteSub2( Surface_ : PSDL_Surface; Font : PSFont_FontInfo; x : integer;
  y : integer; text : pchar );

// Returns the width of "text" in pixels
function SFont_TextWidth( Text : pchar ) : integer;
function SFont_TextWidth2( Font : PSFont_FontInfo; Text : pchar ) : integer;

// Blits a string to with centered x position
procedure SFont_WriteCentered( Surface_ : PSDL_Surface; y : Integer; text : pchar );
procedure SFont_WriteCentered2( Surface_ : PSDL_Surface; Font : PSFont_FontInfo; y :
  integer; text : pchar );

// Allows the user to enter text
// Width: What is the maximum width of the text (in pixels)
// text: This string contains the text which was entered by the user
procedure SFont_Input( Destination : PSDL_Surface; x : Integer; y : integer; Width :
  integer; text : pchar );
procedure SFont_Input2( Destination : PSDL_Surface; Font : PSFont_FontInfo; x :
  integer; y : integer; Width : integer; text : pchar );
// Not part of the original implementation, but We really shouldn't be falling for the C Scanf problem...
// This version requires a maximum length for the amount of text to input.
procedure SFont_Input3( Destination : PSDL_Surface; Font : PSFont_FontInfo; x :
  integer; y : integer; Width : integer; text : pchar; MaxChars : Cardinal );

{ We'll use SDL for reporting errors }
procedure SFont_SetError( fmt : PChar );

function SFont_GetError : PChar;

var
  InternalFont : TSFont_FontInfo;

implementation

procedure SFont_SetError( fmt : PChar );
begin
  SDL_SetError( fmt );
end;

function SFont_GetError : PChar;
begin
  result := SDL_GetError;
end;


procedure SFont_InitFont2( Font : PSFont_FontInfo );
var
  X : Integer;
  I : Integer;
begin

  x := 0;
  i := 0;

  if Font.Surface = nil then
  begin
    //Font_SetError ("The font has not been loaded!");
 //printf("The font has not been loaded!\n");

 //exit(1);
    exit;
  end;

  while x < Font.Surface.w do
  begin

    if SDL_GetPixel( Font.Surface, x, 0 ) = SDL_MapRGB( Font.Surface.format, 255, 0,
      255 ) then
    begin
      Font.CharPos[ i ] := x;
      inc( i );
      while ( ( x < Font.Surface.w - 1 ) and ( SDL_GetPixel( Font.Surface, x, 0 ) =
        SDL_MapRGB( Font.Surface.format, 255, 0, 255 ) ) ) do
      begin
        inc( x );
      end;
      Font.CharPos[ i ] := x;
      inc( i );
    end;
    inc( x );
  end;

  Font.MaxPos := Font.Surface.h;
  SDL_SetColorKey( Font.Surface, SDL_SRCCOLORKEY, SDL_GetPixel( Font.Surface, 0,
    Font.Surface.h - 1 ) );
end;

procedure SFont_InitFont( Font : PSDL_Surface );
begin
  InternalFont.Surface := Font;
  SFont_InitFont2( @InternalFont );
end;

procedure SFont_WriteAdd2( Surface_ : PSDL_Surface; Font : PSFont_FontInfo; x : integer;
  y : integer; text : pchar );
var
  ofs : Integer;
  i : Integer;
  srcrect, dstrect : SDL_Rect;

begin
  i := 0;

  while text[ i ] <> chr( 0 ) do
  begin
    if text[ i ] = ' ' then
    begin
      x := x + Font.CharPos[ 2 ] - Font.CharPos[ 1 ];
      inc( i );

    end
    else
    begin
      ofs := ( ( integer( text[ i ] ) - 33 ) * 2 ) + 1;

      srcrect.w := ( Font.CharPos[ ofs + 2 ] + Font.CharPos[ ofs + 1 ] ) div 2 -
        ( Font.CharPos[ ofs ] + Font.CharPos[ ofs - 1 ] ) div 2;
      dstrect.w := srcrect.w;
      srcrect.h := Font.Surface.h - 1;
      dstrect.h := srcrect.h;
      srcrect.x := ( Font.CharPos[ ofs ] + Font.CharPos[ ofs - 1 ] ) div 2;
      srcrect.y := 1;
      dstrect.x := x - ( Font.CharPos[ ofs ] - Font.CharPos[ ofs - 1 ] ) div 2;
      dstrect.y := y;

      SDL_AddSurface( Font.Surface, @srcrect, Surface_, @dstrect );

      x := x + Font.CharPos[ ofs + 1 ] - Font.CharPos[ ofs ];
      inc( i );
    end;
  end;
end;

procedure SFont_WriteSub2( Surface_ : PSDL_Surface; Font : PSFont_FontInfo; x : integer;
  y : integer; text : pchar );
var
  ofs : Integer;
  i : Integer;
  srcrect, dstrect : SDL_Rect;

begin
  i := 0;

  while text[ i ] <> chr( 0 ) do
  begin
    if text[ i ] = ' ' then
    begin
      x := x + Font.CharPos[ 2 ] - Font.CharPos[ 1 ];
      inc( i );

    end
    else
    begin
      ofs := ( ( integer( text[ i ] ) - 33 ) * 2 ) + 1;

      srcrect.w := ( Font.CharPos[ ofs + 2 ] + Font.CharPos[ ofs + 1 ] ) div 2 -
        ( Font.CharPos[ ofs ] + Font.CharPos[ ofs - 1 ] ) div 2;
      dstrect.w := srcrect.w;
      srcrect.h := Font.Surface.h - 1;
      dstrect.h := srcrect.h;
      srcrect.x := ( Font.CharPos[ ofs ] + Font.CharPos[ ofs - 1 ] ) div 2;
      srcrect.y := 1;
      dstrect.x := x - ( Font.CharPos[ ofs ] - Font.CharPos[ ofs - 1 ] ) div 2;
      dstrect.y := y;

      SDL_SubSurface( Font.Surface, @srcrect, Surface_, @dstrect );

      x := x + Font.CharPos[ ofs + 1 ] - Font.CharPos[ ofs ];
      inc( i );
    end;
  end;
end;

procedure SFont_Write2( Surface_ : PSDL_Surface; Font : PSFont_FontInfo; x : integer;
  y : integer; text : pchar );
var
  ofs : Integer;
  i : Integer;
  srcrect, dstrect : SDL_Rect;

begin
  i := 0;

  while text[ i ] <> chr( 0 ) do
  begin
    if text[ i ] = ' ' then
    begin
      x := x + Font.CharPos[ 2 ] - Font.CharPos[ 1 ];
      inc( i );

    end
    else
    begin
      ofs := ( ( integer( text[ i ] ) - 33 ) * 2 ) + 1;

      srcrect.w := ( Font.CharPos[ ofs + 2 ] + Font.CharPos[ ofs + 1 ] ) div 2 -
        ( Font.CharPos[ ofs ] + Font.CharPos[ ofs - 1 ] ) div 2;
      dstrect.w := srcrect.w;
      srcrect.h := Font.Surface.h - 1;
      dstrect.h := srcrect.h;
      srcrect.x := ( Font.CharPos[ ofs ] + Font.CharPos[ ofs - 1 ] ) div 2;
      srcrect.y := 1;
      dstrect.x := x - ( Font.CharPos[ ofs ] - Font.CharPos[ ofs - 1 ] ) div 2;
      dstrect.y := y;

      SDL_BlitSurface( Font.Surface, @srcrect, Surface_, @dstrect );

      x := x + Font.CharPos[ ofs + 1 ] - Font.CharPos[ ofs ];
      inc( i );
    end;
  end;
end;

procedure SFont_Write( Surface_ : PSDL_Surface; x : Integer; y : Integer; text :
  pchar );
begin
  SFont_Write2( Surface_, @InternalFont, x, y, text );
end;

procedure SFont_WriteAdd( Surface_ : PSDL_Surface; x : Integer; y : Integer; text :
  pchar );
begin

  SFont_WriteAdd2( Surface_, @InternalFont, x, y, text );
end;

procedure SFont_WriteSub( Surface_ : PSDL_Surface; x : Integer; y : Integer; text :
  pchar );
begin

  SFont_WriteSub2( Surface_, @InternalFont, x, y, text );
end;



function SFont_TextWidth2( Font : PSFont_FontInfo; Text : pchar ) : integer;
var
  x, i, ofs : integer;

begin
  x := 0;
  i := 0;
  ofs := 0;
  while text[ i ] <> chr( 0 ) do
  begin
    if text[ i ] = ' ' then
    begin
      x := x + Font.CharPos[ 2 ] - Font.CharPos[ 1 ];
      inc( i );
    end
    else
    begin
      ofs := ( integer( text[ i ] ) - 33 ) * 2 + 1;
      x := x + Font.CharPos[ ofs + 1 ] - Font.CharPos[ ofs ];
      inc( i );
    end;
  end;
  result := ( x + Font.CharPos[ ofs + 2 ] - Font.CharPos[ ofs + 1 ] );
end;

function SFont_TextWidth( Text : pchar ) : integer;
begin
  result := SFont_TextWidth2( @InternalFont, Text );
end;

procedure SFont_WriteCentered2( Surface_ : PSDL_Surface; Font : PSFont_FontInfo; y :
  integer; text : pchar );
begin
  SFont_Write2( Surface_, @InternalFont, Surface_.w div 2 - SFont_TextWidth( text ) div 2,
    y, text );
end;

procedure SFont_WriteCentered( Surface_ : PSDL_Surface; y : Integer; text : pchar );
begin
  SFont_WriteCentered2( Surface_, @InternalFont, y, text );
end;

procedure SFont_Input3( Destination : PSDL_Surface; Font : PSFont_FontInfo; x :
  integer; y : integer; Width : integer; text : pchar; MaxChars : Cardinal );
var
  event : TSDL_Event;
  ch, ofs, leftshift : integer;

  Back : PSDL_Surface;
  rect : SDL_Rect;
begin
  ch := 0; //Just to shut the compiler up
  ofs := ( integer( text[ 0 ] ) - 33 ) * 2 + 1;
  leftshift := ( Font.CharPos[ ofs ] - Font.CharPos[ ofs - 1 ] ) div 2;

  Back := SDL_AllocSurface( Destination.flags,
    Width,
    Font.MaxPos,
    Destination.format.BitsPerPixel,
    Destination.format.Rmask,
    Destination.format.Gmask,
    Destination.format.Bmask, 0 );

  rect.x := x - leftshift;
  rect.y := y;
  rect.w := Width;
  rect.h := Font.Surface.h;
  SDL_BlitSurface( Destination, @rect, Back, nil );
  SFont_Write2( Destination, Font, x, y, text );
  SDL_UpdateRect( Destination, x - leftshift, y, Width, Font.MaxPos );

  // start input
  SDL_EnableUNICODE( 1 );
  while ( ( ch <> SDLK_RETURN ) and ( SDL_WaitEvent( @event ) > 0 ) ) do
  begin
    if event.type_ = SDL_KEYDOWN then
    begin

      ch := event.key.keysym.unicode;
      if ( ch = SDLK_BACKSPACE ) and ( strlen( text ) > 0 ) then
      begin
        text[ strlen( text ) - 1 ] := chr( 0 );
      end
      else
      begin
        if strlen( text ) < MaxChars then
        begin
          if ch <> SDLK_BACKSPACE then
          begin
            text[ strlen( text ) ] := chr( ch );
            text[ strlen( text ) ] := chr( 0 );
          end;
          if ( SFont_TextWidth2( Font, text ) > Width ) then
            text[ strlen( text ) ] := chr( 0 );

        end;
      end;
      SDL_BlitSurface( Back, nil, Destination, @rect );
      SFont_Write2( Destination, Font, x, y, text );
      SDL_UpdateRect( Destination, x - ( Font.CharPos[ ofs ] - Font.CharPos[ ofs - 1 ] )
        div 2, y, Width, Font.Surface.h );

    end;
  end;
  text[ strlen( text ) ] := chr( 0 );
  SDL_FreeSurface( Back );
end;

procedure SFont_Input2( Destination : PSDL_Surface; Font : PSFont_FontInfo; x :
  integer; y : integer; Width : integer; text : pchar );
var
  MaxChars : Cardinal;
begin
  MaxChars := length( text ); // Just to make sure that we don't spill into
  // memory that doesn't belong to us.
  // We can't test the array as we use it
  // Because we're putting the 0 at the current
  // position.

  SFont_Input3( Destination, Font, x, y, Width, Text, MaxChars );
end;

procedure SFont_Input( Destination : PSDL_Surface; x : Integer; y : integer; Width :
  integer; text : pchar );
begin
  SFont_Input2( Destination, @InternalFont, x, y, Width, text );
end;

end.

