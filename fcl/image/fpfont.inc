{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Implementation of TFPCustomFont

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ FPCustomFont }

procedure TFPCustomFont.SetName (AValue:string);
begin
  FName := AValue;
end;

procedure TFPCustomFont.SetSize (AValue:integer);
begin
  FSize := AValue;
end;

procedure TFPCustomFont.DoCopyProps (From:TFPCanvasHelper);
begin
  with from as TFPCustomFont do
    begin
    self.FName := FName;
    self.FSize := FSize;
    self.FFPColor := FFPColor;
    self.FFlags := FFlags;
    end;
end;

function TFPCustomFont.CopyFont : TFPCustomFont;
begin
  result := TFPCustomFont(self.ClassType.Create);
  result.DoCopyProps (self);
end;

procedure TFPCustomFont.GetTextSize (text:string; var w,h:integer);
begin
  if inheritsFrom (TFPCustomDrawFont) then
    TFPCustomDrawFont(self).DoGetTextSize (text,w,h)
  else
    FCanvas.DoGetTextSize (text, w,h);
end;

function TFPCustomFont.GetTextHeight (text:string) : integer;
begin
  if inheritsFrom (TFPCustomDrawFont) then
    result := TFPCustomDrawFont(self).DoGetTextHeight (text)
  else
    result := FCanvas.GetTextHeight (text);
end;

function TFPCustomFont.GetTextWidth (text:string) : integer;
begin
  if inheritsFrom (TFPCustomDrawFont) then
    result := TFPCustomDrawFont(self).DoGetTextWidth (text)
  else
    result := FCanvas.GetTextWidth (text);
end;

