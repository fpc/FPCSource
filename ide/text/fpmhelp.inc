{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Help menu entries

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure TIDEApp.HelpContents;
begin
  Help(0,hcContents,false);
end;

procedure TIDEApp.HelpHelpIndex;
begin
  HelpIndex('');
end;

procedure TIDEApp.HelpTopicSearch;
begin
end;

procedure TIDEApp.HelpPrevTopic;
begin
  if HelpWindow=nil then HelpContents else
  with HelpWindow^ do
  if GetState(sfVisible) then Message(HelpWindow^.HelpView,evCommand,cmPrevTopic,nil)
     else begin HelpWindow^.Show; HelpWindow^.MakeFirst; end;
end;

procedure TIDEApp.HelpUsingHelp;
begin
  Help(0,hcUsingHelp,false);
end;

procedure TIDEApp.HelpFiles;
begin
end;

procedure TIDEApp.About;
var R,R2: TRect;
    D: PCenterDialog;
begin
  R.Assign(0,0,38,10);
  New(D, Init(R, 'About'));
  with D^ do
  begin
    GetExtent(R); R.Grow(-3,-2);
    R2.Copy(R); R2.B.Y:=R2.A.Y+1;
    Insert(New(PStaticText, Init(R2, ^C'FreePascal IDE / DOS')));
    R2.Move(0,2);
    Insert(New(PStaticText, Init(R2, ^C'Version '+VersionStr)));
    R2.Move(0,2);
    Insert(New(PStaticText, Init(R2, ^C'Copyright (C) 1998 by')));
    R2.Move(0,2);
    Insert(New(PStaticText, Init(R2, ^C'B‚rczi G bor')));
  end;
  InsertOK(D);
  Desktop^.ExecView(D);
  Dispose(D, Done);
end;

{
  $Log$
  Revision 1.1  1998-12-22 14:27:54  peter
    * moved

  Revision 1.2  1998/12/22 10:39:47  peter
    + options are now written/read
    + find and replace routines

}
