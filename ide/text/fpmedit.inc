{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Edit menu entries

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure TIDEApp.ShowClipboard;
begin
  ClipboardWindow^.Show;
  ClipboardWindow^.Focus;
end;

{
  $Log$
  Revision 1.1  2000-07-13 09:48:35  michael
  + Initial import

  Revision 1.1  1998/12/22 14:27:54  peter
    * moved

  Revision 1.2  1998/12/22 10:39:45  peter
    + options are now written/read
    + find and replace routines

}
