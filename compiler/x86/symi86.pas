{
    Copyright (c) 2014 by Florian Klaempfl

    Symbol table overrides for i386 and i8086

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit symi86;

{$i fpcdefs.inc}

interface

uses
  symtype,symdef,symsym;

type

  { ti86absolutevarsym }

  ti86absolutevarsym = class(tabsolutevarsym)
   protected
    procedure ppuload_platform(ppufile: tcompilerppufile); override;
    procedure ppuwrite_platform(ppufile: tcompilerppufile); override;
   public
    absseg  : boolean;
  end;

implementation

uses
  symconst;

{ ti86absolutevarsym }

procedure ti86absolutevarsym.ppuload_platform(ppufile: tcompilerppufile);
  begin
    inherited;
    if abstyp=toaddr then
      absseg:=boolean(ppufile.getbyte)
    else
      absseg:=false;
  end;


procedure ti86absolutevarsym.ppuwrite_platform(ppufile: tcompilerppufile);
  begin
    inherited;
    if abstyp=toaddr then
      ppufile.putbyte(byte(absseg));
  end;

end.

