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
  Revision 1.1  2003-10-06 20:33:58  peter
    * classes moved to rtl for 1.1
    * classes .inc and classes.pp files moved to fcl/classes for
      backwards 1.0.x compatiblity to have it in the fcl

  Revision 1.3  2002/09/07 15:15:24  peter
    * old logs removed and tabs fixed

}
