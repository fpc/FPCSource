{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ *********************************************************************
  *                         TFiler                                    *
  *********************************************************************}

procedure TFiler.SetRoot(ARoot: TComponent);
begin
  FRoot := ARoot;
end;


{
  $Log$
  Revision 1.6  2000-06-29 16:29:23  sg
  * Implemented streaming. Note: The writer driver interface is stable, but
    the reader interface is not final yet!

  Revision 1.5  2000/01/07 01:24:33  peter
    * updated copyright to 2000
}
