unit sdlfilter;
{******************************************************************************}
{
  $Id: sdlfilter.pas,v 1.2 2004/03/31 09:04:30 savage Exp $
}
{                                                                              }
{       Borland Delphi SDL_Image - An image processing and effects library for }
{                                  use with SDL Surfaces                       }
{                                                                              }
{                                                                              }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ Jason Farmer <jason@cerebral-bicycle.co.uk>                                  }
{                                                                              }

{ Contributor(s)                                                               }
{ --------------                                                               }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                            }
{                                                                              }
{ Obtained through:                                                            }
{ Joint Endeavour of Delphi Innovators ( Project JEDI )                        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project              }
{ JEDI home page, located at http://delphi-jedi.org                            }
{                                                                              }
{ The contents of this file are used with permission, subject to               }
{ the Mozilla Public License Version 1.1 (the "License"); you may              }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an                  }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License.                                    }
{                                                                              }
{ Description                                                                  }
{ -----------                                                                  }
{   A simple library to manipulate SDL surfaces.                               }
{   Applies Image Kernel Filters and procedural effects to images              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   SDL.pas in your search path.                                               }
{   SDL_Image in your search path                                              }
{   SDL_Utils in your search path                                              }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{   The Kernels must be built prior to application. Use the BuildXxX Kernel    }
{   functions provided to use predefined effects or supply your own.           }
{                                                                              }
{   Effect Functions always output to another surface. Do not use the source   }
{   Surface for the results, strange things will happen if you do.             }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{   Sept      30 2001 - JF : First Written                                     }
{   Oct       01 2001 - DL : Made Kylix Friendly                               }
{   Oct       03 2001 - RK : Fixed a bug in OutLine effect + minor speed up    }
{
  $Log: sdlfilter.pas,v $
  Revision 1.2  2004/03/31 09:04:30  savage
  Added jedi-sdl.inc files for better FreePascal/multi compiler support.

  Revision 1.1  2004/03/28 13:52:14  savage
  Filtering unit and demo

}
{******************************************************************************}

{$I jedi-sdl.inc}

interface

uses
  SysUtils,
  sdl,
  sdlutils;

Type

TKernelTypes = (  HighPassVeryWeak,
                  HighPassVeryStrong,
                  HighPassStrong,
                  HighPassWeak,
                  LowPassUniform,
                  LowPassPeaked,
                  LowPassStronglyPeaked,
                  PrewittEdge_NW_SE,
                  PrewittEdge_N_S,
                  PrewittEdge_NE_SW,
                  PrewittEdge_E_W,
                  PrewittEdge_SE_NW,
                  PrewittEdge_S_N,
                  PrewittEdge_SW_NE,
                  PrewittEdge_W_E,
                  LapiacianEdgeWeak,
                  LapiacianEdgeStrong,
                  LapiacianEdgeVeryStrong);

T3x3Kernel = array[1..3,1..3] of double; // Just work with 3x3 Kernels
//T5x5Kernel = array[1..5,1..5] of double; // Not implemented yet
//T7x7Kernel = array[1..7,1..7] of double;
P3x3Kernel = ^T3x3Kernel;


procedure ApplyFilter( SourceSurface : PSDL_Surface; SourceRect : PSDL_Rect; DestinationSurface : PSDL_Surface; DestinationRect : PSDL_Rect; KernelToApply: P3x3Kernel);Overload;
//procedure ApplyFilter( SourceSurface : PSDL_Surface; SourceRect : PSDL_Rect; DestinationSurface : PSDL_Surface; DestinationRect : PSDL_Rect; KernelToApply: T5x5Kernel);overload;
//procedure ApplyFilter( SourceSurface : PSDL_Surface; SourceRect : PSDL_Rect; DestinationSurface : PSDL_Surface; DestinationRect : PSDL_Rect; KernelToApply: T7x7Kernel);overload;

// 3X3 kernel construction functions

procedure Build3x3Kernel( KernelType : TKernelTypes; FilterKernel: P3x3Kernel); Overload;

procedure ApplyImageOutline( SourceSurface : PSDL_Surface; SourceRect : PSDL_Rect; DestinationSurface : PSDL_Surface; DestinationRect : PSDL_Rect; OutlineColour : Cardinal);Overload;

function PixelMatch( TestSurface : PSDL_Surface;X : Integer;Y:Integer;TransparentColour:cardinal) : Boolean;

implementation

procedure ApplyImageOutline( SourceSurface : PSDL_Surface; SourceRect : PSDL_Rect; DestinationSurface : PSDL_Surface; DestinationRect : PSDL_Rect; OutlineColour : Cardinal);Overload;
// This procedure traces the outline of a sprite based on its transparent value.
// It draws the outline in the supplied colour.
var
  Red,Green,Blue : UInt8;
  TempRed,TempGreen,TempBlue : integer;
  X,Y,MaxX,MaxY,SOX,SOY,DOX,DOY,KX,KY,LeftX,RightX,TopY,BottomY:Integer;
  Srect,DRect : SDL_Rect;
  SourcePixel, DestinationPixel: cardinal;
  WorkRed,WorkGreen,WorkBlue : double;
  SourceTransparentPixel,DestinationTransparentPixel : cardinal;
  FoundAPixel : Boolean;
begin

// Make sure we have rects and make sure they are within the bounds of the surface
  if SourceRect = nil then
  begin
    Srect.x := 0;
    Srect.y := 0;
    Srect.w := Sourcesurface.w ;
    Srect.h := sourcesurface.h ;
  end
  else
  begin
    Srect.x := SourceRect.x;
    Srect.y := SourceRect.y;
    if (SourceRect.x + sourcerect.w)> SourceSurface.w then
    begin
      Srect.w := SourceSurface.w - SourceRect.x;
    end
    else
    begin
      Srect.w := SourceRect.w;
    end;

    if (SourceRect.y + sourcerect.h)> SourceSurface.h then
    begin
      Srect.h := SourceSurface.h - SourceRect.y;
    end
    else
    begin
      Srect.h := SourceRect.h;
    end;
  end;

  if DestinationRect = nil then
  begin
    DRect.x := 0;
    DRect.y := 0;
    DRect.w := DestinationSurface.w;
    DRect.h := DestinationSurface.h;
  end
  else
  begin
    DRect.x :=DestinationRect.x;
    DRect.y :=DestinationRect.y;
    if (DestinationRect.x + DestinationRect.w)> SourceSurface.w then
    begin
      DRect.w := DestinationSurface.w - DestinationRect.x;
    end
    else
    begin
      DRect.w := DestinationRect.w;
    end;

    if (DestinationRect.y + DestinationRect.h)> DestinationSurface.h then
    begin
      DRect.h := DestinationSurface.h - DestinationRect.y;
    end
    else
    begin
      DRect.h := DestinationRect.h;
    end;
  end;

  // Now we're happy that the rects are within valid areas,
  // We need to find the lowest extents for the rects

  // Get pixel RGB

  if srect.w>DRect.w then
  begin
    MaxX := DRect.w - 1;
  end
  else
  begin
    MaxX := SRect.w - 1;
  end;

  if srect.h>DRect.h then
  begin
    MaxY := DRect.h - 1;
  end
  else
  begin
    MaxY := SRect.h - 1;
  end;

  // Now we know the lowest width and height, we can get on with the work
  // Set the Source Offsets and Dest Offsets

  SOX := SRect.x;
  SOY := Srect.y;
  DOX := DRect.X;
  DOY := DRect.y;

  // Do the test

  // Lock both surfaces

  SourceTransparentPixel := sourcesurface.format.colorkey;
  DestinationTransparentPixel := DestinationSurface.format.colorkey;

  SDL_FillRect(DestinationSurface, @DRect, DestinationTransparentPixel);

  SDL_LockSurface(SourceSurface);
  SDL_LockSurface(DestinationSurface);
  for Y := 0 to maxy do
  begin
    for X := 0 to maxx do
    begin

      sourcepixel := SDL_GetPixel(SourceSurface, X, Y);

      if sourcepixel = SourceTransparentPixel then
      begin
        KX := x + sox;
        KY := y + soy;

        LeftX := kx - 1;
        if LeftX < sox then LeftX := sox;

        RightX := kx + 1;
        if RightX > maxx + sox then RightX := Maxx+sox;

        TopY := ky - 1;
        if TopY < soy then TopY := soy;

        BottomY := ky + 1;
        if BottomY > maxy + soy then BottomY := Maxy + soy;

//        sourcepixel := SDL_GetPixel(SourceSurface, KX, KY);

        // Check pixels around current pixel for non transparent values

        FoundAPixel := not PixelMatch(SourceSurface,LeftX,TopY,SourceTransparentPixel);
        if (FoundAPixel=False) then FoundAPixel := not PixelMatch(SourceSurface, LeftX, KY, SourceTransparentPixel);
        if (FoundAPixel=False) then FoundAPixel := not PixelMatch(SourceSurface, LeftX, BottomY, SourceTransparentPixel);
        if (FoundAPixel=False) then FoundAPixel := not PixelMatch(SourceSurface, KX, TopY, SourceTransparentPixel);
        if (FoundAPixel=False) then FoundAPixel := not PixelMatch(SourceSurface, KX, BottomY, SourceTransparentPixel);
        if (FoundAPixel=False) then FoundAPixel := not PixelMatch(SourceSurface, RightX, TopY, SourceTransparentPixel);
        if (FoundAPixel=False) then FoundAPixel := not PixelMatch(SourceSurface, RightX, KY, SourceTransparentPixel);
        if (FoundAPixel=False) then FoundAPixel := not PixelMatch(SourceSurface, RightX, BottomY, SourceTransparentPixel);;

        if FoundAPixel = true then
        begin
          // A non transparent pixel is next to our transpa
          SDL_PutPixel(DestinationSurface,dox+x,doy+y,OutlineColour);
        end;
      end;
    end;
  end;
  SDL_UnlockSurface(SourceSurface);
  SDL_UnlockSurface(DestinationSurface);

end;

function PixelMatch( TestSurface : PSDL_Surface;X : Integer;Y:Integer;TransparentColour:cardinal) : Boolean;
begin
result := (SDL_GetPixel(TestSurface,x,y)=TransparentColour);

end;

procedure ApplyFilter( SourceSurface : PSDL_Surface; SourceRect : PSDL_Rect; DestinationSurface : PSDL_Surface; DestinationRect : PSDL_Rect; KernelToApply: P3x3Kernel);Overload;
var
  Red,Green,Blue : UInt8;
  TempRed,TempGreen,TempBlue : integer;
  X,Y,MaxX,MaxY,SOX,SOY,DOX,DOY,KX,KY:Integer;
  Srect,DRect : SDL_Rect;
  SourcePixel, DestinationPixel: cardinal;
  WorkRed,WorkGreen,WorkBlue : double;
begin

  // Make sure we have rects and make sure they are within the bounds of the surface
  if SourceRect = nil then
  begin
    Srect.x := 0;
    Srect.y := 0;
    Srect.w := Sourcesurface.w ;
    Srect.h := sourcesurface.h ;
  end
  else
  begin
    Srect.x := SourceRect.x;
    Srect.y := SourceRect.y;
    if (SourceRect.x + sourcerect.w)> SourceSurface.w then
    begin
      Srect.w := SourceSurface.w - SourceRect.x;
    end
    else
    begin
      Srect.w := SourceRect.w;
    end;

    if (SourceRect.y + sourcerect.h)> SourceSurface.h then
    begin
      Srect.h := SourceSurface.h - SourceRect.y;
    end
    else
    begin
      Srect.h := SourceRect.h;
    end;
  end;

  if DestinationRect = nil then
  begin
    DRect.x := 0;
    DRect.y := 0;
    DRect.w := DestinationSurface.w;
    DRect.h := DestinationSurface.h;
  end
  else
  begin
    DRect.x :=DestinationRect.x;
    DRect.y :=DestinationRect.y;
    if (DestinationRect.x + DestinationRect.w)> SourceSurface.w then
    begin
      DRect.w := DestinationSurface.w - DestinationRect.x;
    end
    else
    begin
      DRect.w := DestinationRect.w;
    end;

    if (DestinationRect.y + DestinationRect.h)> DestinationSurface.h then
    begin
      DRect.h := DestinationSurface.h - DestinationRect.y;
    end
    else
    begin
      DRect.h := DestinationRect.h;
    end;
  end;

  // Now we're happy that the rects are within valid areas,
  // We need to find the lowest extents for the rects

  // Get pixel RGB

  if srect.w>DRect.w then
  begin
    MaxX := DRect.w;
  end
  else
  begin
    MaxX := SRect.w;
  end;

  if srect.h>DRect.h then
  begin
    MaxY := DRect.h;
  end
  else
  begin
    MaxY := SRect.h;
  end;

  // Now we know the lowest width and height, we can get on with the work
  // Set the Source Offsets and Dest Offsets

  SOX := SRect.x;
  SOY := Srect.y;
  DOX := DRect.X;
  DOY := DRect.y;

  // Alter the values to allow for a 1 pixel border
  if SOX = 0 then SOX := 1;
  if SOY = 0 then SOY := 1;
  if DOX = 0 then DOX := 1;
  if DOY = 0 then DOY := 1;

  If Maxx+Sox >= SourceSurface.w then
  begin
    dec(maxx);
  end;

  If Maxy+Soy >= SourceSurface.h then
  begin
    dec(maxy);
  end;

  If Maxx+dox >= DestinationSurface.w then
  begin
    dec(maxx);
  end;

  If Maxy+doy >= DestinationSurface.h then
  begin
    dec(maxy);
  end;

  // Do the filter

  // Lock both surfaces

  SDL_LockSurface(SourceSurface);
  SDL_LockSurface(DestinationSurface);
  for Y:=0 to maxy-1 do
  begin
    for X := 0 to maxx-1 do
    begin
      TempRed := 0;
      TempGreen := 0;
      TempBlue := 0;
      for KX := 1 to 3 do
        begin
          for KY := 1 to 3 do
            begin

            sourcepixel := SDL_GetPixel(SourceSurface,x+sox+(KY-2),y+soy+(KX-2));

            SDL_GetRGB(sourcepixel,SourceSurface.format,@Red,@Green,@Blue);

            workred := red;
            workgreen := green;
            workblue := blue;

            TempRed := round( TempRed + workred  *  KernelToApply[KY, KX]);
            TempGreen := round(TempGreen +workgreen *  KernelToApply[KY, KX]);
            TempBlue := round( TempBlue + workblue *  KernelToApply[KY, KX]);
          end;

        end;

        // Make sure we can put the values back into bytes

        If TempRed < 0 Then TempRed := 0;
        If TempRed > 255 Then TempRed := 255 ;
        If TempGreen < 0 Then TempGreen := 0;
        If TempGreen > 255 Then TempGreen := 255;
        If TempBlue < 0 Then TempBlue := 0;
        If TempBlue > 255 Then TempBlue := 255;

        // Put the pixel back into the destination

        DestinationPixel := SDL_MapRGB(destinationsurface.format,byte(tempred),byte(tempgreen),byte(tempblue));
        try
        SDL_PutPixel(DestinationSurface,dox+x,doy+y,destinationpixel);
        except
          on E: Exception do e.CreateFmt('Error occurred X=%d,Y=%d,dox=%d,doy=%d',[x,y,dox,doy]);

        end;

    end;
  end;
  SDL_UnlockSurface(SourceSurface);
  SDL_UnlockSurface(DestinationSurface);

end;

procedure Build3x3Kernel( KernelType : TKernelTypes; FilterKernel: P3x3Kernel); Overload;
var
  X,Y : integer;
begin
  // Depending on the type of known kernel that we want to build,
  // Populate the kernel array
  case KernelType of
    HighPassVeryWeak :
      begin

        FilterKernel[1,1] := -1 / 12; FilterKernel[1,2] := -1 / 12; FilterKernel[1,3] :=  -1 / 12;
        FilterKernel[2,1] := -1 / 12; FilterKernel[2,2] := 20 / 12; FilterKernel[2,3] := -1 / 12;
        FilterKernel[3,1] := -1 / 12; FilterKernel[3,2] := -1 / 12; FilterKernel[3,3] :=  -1 / 12;

      end;
    HighPassVeryStrong :
      begin

        FilterKernel[1,1] := -1 ; FilterKernel[1,2] := -1 ; FilterKernel[1,3] :=  -1 ;
        FilterKernel[2,1] := -1 ; FilterKernel[2,2] := 9; FilterKernel[2,3] := -1 ;
        FilterKernel[3,1] := -1 ; FilterKernel[3,2] := -1 ; FilterKernel[3,3] :=  -1 ;

      end;
    HighPassStrong :
      begin

        FilterKernel[1,1] :=  0; FilterKernel[1,2] := -1; FilterKernel[1,3] :=  0;
        FilterKernel[2,1] := -1; FilterKernel[2,2] :=  5; FilterKernel[2,3] := -1;
        FilterKernel[3,1] :=  0; FilterKernel[3,2] := -1; FilterKernel[3,3] :=  0;

      end;
    HighPassWeak :
      begin

        FilterKernel[1,1] := -1 / 4; FilterKernel[1,2] := -1 / 4; FilterKernel[1,3] :=  -1 / 4;
        FilterKernel[2,1] := -1 / 4; FilterKernel[2,2] := 12 / 4; FilterKernel[2,3] := -1 / 4;
        FilterKernel[3,1] := -1 / 4; FilterKernel[3,2] := -1 / 4; FilterKernel[3,3] :=  -1 / 4;

      end;
    LowPassUniform :
      begin
        For X := 1 To 3 do
          begin
            For Y := 1 To 3 do
              begin
                FilterKernel[X, Y] := 0.1 ;
              end;
          end;

      end;
    LowPassPeaked :
      begin
      
        FilterKernel[1,1] := 0.0666; FilterKernel[1,2] := 0.1333; FilterKernel[1,3] :=  0.0666;
        FilterKernel[2,1] := 0.1333; FilterKernel[2,2] := 0.2; FilterKernel[2,3] := 0.1333;
        FilterKernel[3,1] := 0.0666; FilterKernel[3,2] := 0.1333; FilterKernel[3,3] :=  0.0666;

      end;
    LowPassStronglyPeaked :
      begin

        FilterKernel[1,1] := 0.05; FilterKernel[1,2] := 0.05; FilterKernel[1,3] :=  0.05;
        FilterKernel[2,1] := 0.05; FilterKernel[2,2] := 0.6; FilterKernel[2,3] := 0.05;
        FilterKernel[3,1] := 0.05; FilterKernel[3,2] := 0.05; FilterKernel[3,3] :=  0.05;

      end;
    PrewittEdge_NW_SE :
      begin

        FilterKernel[1,1] := 1; FilterKernel[1,2] :=  1; FilterKernel[1,3] :=  1;
        FilterKernel[2,1] := 1; FilterKernel[2,2] := -2; FilterKernel[2,3] := -1;
        FilterKernel[3,1] := 1; FilterKernel[3,2] := -1; FilterKernel[3,3] := -1;

      end;
    PrewittEdge_N_S :
      begin

        FilterKernel[1,1] := 1; FilterKernel[1,2] :=  1; FilterKernel[1,3] :=  1;
        FilterKernel[2,1] := 1; FilterKernel[2,2] := -2; FilterKernel[2,3] :=  1;
        FilterKernel[3,1] :=-1; FilterKernel[3,2] := -1; FilterKernel[3,3] := -1;

      end;
    PrewittEdge_NE_SW :
      begin

        FilterKernel[1,1] := 1; FilterKernel[1,2] :=  1; FilterKernel[1,3] :=  1;
        FilterKernel[2,1] :=-1; FilterKernel[2,2] := -2; FilterKernel[2,3] :=  1;
        FilterKernel[3,1] :=-1; FilterKernel[3,2] := -1; FilterKernel[3,3] :=  1;

      end;
    PrewittEdge_E_W :
      begin

        FilterKernel[1,1] :=-1; FilterKernel[1,2] :=  1; FilterKernel[1,3] :=  1;
        FilterKernel[2,1] :=-1; FilterKernel[2,2] := -2; FilterKernel[2,3] :=  1;
        FilterKernel[3,1] :=-1; FilterKernel[3,2] :=  1; FilterKernel[3,3] :=  1;

      end;
    PrewittEdge_SE_NW :
      begin

        FilterKernel[1,1] :=-1; FilterKernel[1,2] := -1; FilterKernel[1,3] :=  1;
        FilterKernel[2,1] :=-1; FilterKernel[2,2] := -2; FilterKernel[2,3] :=  1;
        FilterKernel[3,1] := 1; FilterKernel[3,2] :=  1; FilterKernel[3,3] :=  1;

      end;
    PrewittEdge_S_N :
      begin

        FilterKernel[1,1] :=-1; FilterKernel[1,2] := -1; FilterKernel[1,3] := -1;
        FilterKernel[2,1] := 1; FilterKernel[2,2] := -2; FilterKernel[2,3] :=  1;
        FilterKernel[3,1] := 1; FilterKernel[3,2] :=  1; FilterKernel[3,3] :=  1;

      end;
    PrewittEdge_SW_NE :
      begin

        FilterKernel[1,1] := 1; FilterKernel[1,2] := -1; FilterKernel[1,3] := -1;
        FilterKernel[2,1] := 1; FilterKernel[2,2] := -2; FilterKernel[2,3] := -1;
        FilterKernel[3,1] := 1; FilterKernel[3,2] :=  1; FilterKernel[3,3] :=  1;

      end;
    PrewittEdge_W_E :
      begin

        FilterKernel[1,1] := 1; FilterKernel[1,2] :=  1; FilterKernel[1,3] := -1;
        FilterKernel[2,1] := 1; FilterKernel[2,2] := -2; FilterKernel[2,3] := -1;
        FilterKernel[3,1] := 1; FilterKernel[3,2] :=  1; FilterKernel[3,3] := -1;

      end;
    LapiacianEdgeWeak :
      begin

        FilterKernel[1,1] := 0; FilterKernel[1,2] := -1; FilterKernel[1,3] :=  0;
        FilterKernel[2,1] :=-1; FilterKernel[2,2] :=  4; FilterKernel[2,3] := -1;
        FilterKernel[3,1] := 0; FilterKernel[3,2] := -1; FilterKernel[3,3] := 0;

      end;
    LapiacianEdgeStrong :
      begin

        FilterKernel[1,1] :=-1; FilterKernel[1,2] := -1; FilterKernel[1,3] := -1;
        FilterKernel[2,1] :=-1; FilterKernel[2,2] :=  8; FilterKernel[2,3] := -1;
        FilterKernel[3,1] :=-1; FilterKernel[3,2] := -1; FilterKernel[3,3] := -1;

      end;
    LapiacianEdgeVeryStrong :
      begin

        FilterKernel[1,1] :=-1; FilterKernel[1,2] := -2; FilterKernel[1,3] := -1;
        FilterKernel[2,1] :=-2; FilterKernel[2,2] := 12; FilterKernel[2,3] := -2;
        FilterKernel[3,1] :=-1; FilterKernel[3,2] := -2; FilterKernel[3,3] := -1;

      end;
  end;
end;


end.
