{
   Dummy unit to compile imagemagick in one command

   This unit is part of imagemagick.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the
   Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02111-1301, USA.
}
{$IFNDEF FPC_DOTTEDUNITS}
unit buildim;
{$ENDIF FPC_DOTTEDUNITS}

Interface

{$IFDEF FPC_DOTTEDUNITS}
uses Api.Imagemagick, Api.Magick_wand;
{$ELSE FPC_DOTTEDUNITS}
uses imagemagick, magick_wand;
{$ENDIF FPC_DOTTEDUNITS}

Implementation

end.
