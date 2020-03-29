{
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 2000 by Pierre Muller

    Ansi dump capability

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure TIDEApp.CreateAnsiFile;
var
   f : text;
   Buf : PVideoBuf;
   re : word;
   p : longint;
   UL,LR : TPoint;
   SaveAsText : boolean;
   Name, DefExt : string;

   function GetPoint(var P :TPoint) : boolean;
     var
       E : TEvent;
     begin
       repeat
         GetEvent(E);
       until (E.What=evMouseDown) or
         ((E.What=evKeyDown) and ((E.KeyCode=kbEsc) or (E.KeyCode=kbEnter)));
       if (E.What=evMouseDown) then
         begin
           GetPoint:=true;
           P:=E.Where;
         end
       else if (E.KeyCode=kbEnter) then
         GetPoint:=true
       else
         GetPoint:=false;
     end;
begin
  { First copy the whole screen untouched }
  GetMem(Buf,VideoBufSize);
  Move(VideoBuf^,Buf^,VideoBufSize);
  { partial screen save ? }
  PushStatus(msg_click_upper_left);
  UL.X:=0;UL.Y:=0;
  if not GetPoint(UL) then
    begin
      PopStatus;
      exit;
    end;
  PopStatus;
  PushStatus(msg_click_lower_right);
  LR.X:=Size.X-1;LR.Y:=Size.Y-1;
  if not GetPoint(LR) then
    begin
      PopStatus;
      exit;
    end;
  PopStatus;
  { How should we do this ?? }
  { after ask for a file name to save }
  DefExt:='*.ans';
  Name:='screen.ans';
  PushStatus(msg_saveansifile);
  Re:=Application^.ExecuteDialog(New(PFileDialog, Init(DefExt,
     dialog_savefileas, label_name, fdOkButton, FileId)), @Name);
  if Re<>cmCancel then
    begin
      Assign(f,Name);
      Rewrite(f);
      p:=system.pos('.',Name);
      SaveAsText:=Copy(Name,p+1,High(Name))='txt';
      ExportBufferToAnsiFile(Buf^,UL.X,LR.X,UL.Y,LR.Y,
        Size.X,SaveAsText,f);
      Close(f);
    end;
  PopStatus;
end;

