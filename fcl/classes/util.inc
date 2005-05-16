{
    $Id: util.inc,v 1.2 2005/02/14 17:13:11 peter Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Michael Van Canneyt and Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Function IntToStr (I : Longint) : String;

begin
  Str(I,Result);
end;

function IsValidIdent(const Ident: string): Boolean;

begin
  Result:=True;
end;

{
  $Log: util.inc,v $
  Revision 1.2  2005/02/14 17:13:11  peter
    * truncate log

}
