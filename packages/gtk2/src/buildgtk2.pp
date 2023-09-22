{
   Dummy unit to compile everything in one go

   This unit is part of gtk2forpascal.

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
   Boston, MA 02110-1301, USA.
}
{$IFNDEF FPC_DOTTEDUNITS}
unit buildgtk2; // keep unit name lowercase for kylix
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Api.Gtk2.Gtk2, Api.Glade2,Api.Gtk2.Gdkglext,Api.Gtk2.Gtkglext, Api.Gtk2.Gtk2ext
{$ifdef Unix}  
  ,Api.Gtk2.Gdk2x
{$endif Unix}
  , Api.Pangocairo;
{$ELSE FPC_DOTTEDUNITS}
uses
  gtk2, libglade2,gdkglext,gtkglext, gtk2ext
{$ifdef unix}  
  ,gdk2x
{$endif unix}
  , pangocairo;
{$ENDIF FPC_DOTTEDUNITS}

implementation

end.
