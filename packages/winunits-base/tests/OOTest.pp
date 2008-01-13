{***********************************************************************
 *
 *  $RCSfile: SampleUI.pas,v $
 *
 *  $Revision: 1.2 $
 *
 *  last change: $Author: hr $ $Date: 2003/06/30 15:51:41 $
 *
 *  The Contents of this file are made available subject to the terms of
 *  the BSD license.
 *  
 *  Copyright (c) 2003 by Sun Microsystems, Inc.
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *  3. Neither the name of Sun Microsystems, Inc. nor the names of its
 *     contributors may be used to endorse or promote products derived
 *     from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 *  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 *  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 *  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 *  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 *  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 *  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
 *  TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
 *  USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *     
 *************************************************************************}
{$mode objfpc}
uses
  OOHelper;
  
var
  Sample : TSampleCode;

procedure error(const s : string = 'Error');
  begin
    writeln(s);
    halt(1);
  end;
  
  
begin
  write('Connection to StarOffice ... ');
  Sample := TSampleCode.Create();
  if Sample.Connect() then
    writeln('done.')
  else
    error;
  write('Creating new text document ... ');
  if Sample.CreateDocument(false) then
    writeln('done.')
  else
    error;
{  
  try
    writeln('Inserting Table ...');
    Sample.InsertTable(Edit2.Text, Edit1.Text);
    writeln('Ready');
  except
    Error;
  end;
}   
  write('Disconnection from StarOffice ... ');
  Sample.Disconnect();
  writeln('done.');
  writeln('Finished');
end.

{
end;

procedure TOKBottomDlg.OnCreateDocument(Sender: TObject);
begin
end;

procedure TOKBottomDlg.OnInsertTable(Sender: TObject);
begin
end;

procedure TOKBottomDlg.OnGetDatabasePointer(Sender: TObject);
var
    res : String;
begin
    try
        StatusBar1.SimpleText := 'Getting database pointer ...';
        res := Sample.getDatabasePointer(Edit4.Text, Edit3.Text);
        Application.MessageBox(PChar('the pointer: ' + res), PChar('Result'), ID_OK);
        StatusBar1.SimpleText := 'Ready';
    except
        StatusBar1.SimpleText := 'Error';
    end;
end;

procedure TOKBottomDlg.OnGetCellContent(Sender: TObject);
var
    res : String;
begin
    try
        StatusBar1.SimpleText := 'Getting cell content ...';
        res := Sample.getCellContent(Edit6.Text);
        Application.MessageBox(PChar('the content: ' + res), PChar('Result'), ID_OK);
        StatusBar1.SimpleText := 'Ready';
    except
        StatusBar1.SimpleText := 'Error';
    end;
end;

end.
}