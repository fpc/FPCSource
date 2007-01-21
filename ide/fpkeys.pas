{
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998-2000 by Pierre Muller

    Learn keys routines for the IDE

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit fpkeys;

interface

  uses
    keyboard, Objects, Drivers, Dialogs, App,
    FPViews, WViews;

procedure  LearnKeysDialog;

Const
  NumWantedKeys = 24;
  WantedKeys : Array[1..NumWantedKeys] of word =
  (kbF1,kbF2,kbF3,kbF4,
   kbF5,kbF6,kbF7,kbF8,
   kbF9,kbF10,kbF11,kbF12,
   kbLeft,kbRight,kbUp,kbDown,
   kbPgUp,kbPgDn,kbIns,kbDel,
   kbEnd,kbHome,kbBack,kbShiftTab);

type
   PKeyDialog = ^TKeyDialog;
   TKeyDialog = object(TCenterDialog)
     Constructor Init(Const ATitle : String);
     {Procedure HandleEvent(var E : TEvent);virtual;}
     function Execute : Word;Virtual;
      PSTL : Array [1..NumWantedKeys] of PLabel;
      PL : Array [1..NumWantedKeys] of PInputLine;
      KeyOK : Array [1..NumWantedKeys] of boolean;
      PST,PST2 : PAdvancedStaticText;
   end;

Procedure LoadKeys(var S : TStream);
Procedure StoreKeys(var S : TStream);
Procedure SetKnownKeys;

implementation

uses
  FVConsts,
  WUtils;

{$ifndef NotUseTree}
function GetKey(Const St : String) : word;
var
  AChar,AScan : byte;
begin
  If FindSequence(St,AChar,Ascan) then
    GetKey:=Ascan*$100+AChar
  else
    GetKey:=0;
end;

Procedure SetKey(Const St : String;key :word);
var
  AChar,AScan : byte;
begin
  AChar:=key and $ff;
  AScan:=key shr 8;
  AddSequence(St,AChar,Ascan);
end;

{$endif not NotUseTree}

Const
  WantedKeysLabels : Array[1..NumWantedKeys] of String[5] =
  ('F1   ','F2   ','F3   ','F4   ',
   'F5   ','F6   ','F7   ','F8   ',
   'F9   ','F10  ','F11  ','F12  ',
   'Left ','Right','Up   ','Down ',
   'PgUp ','PgDn ','Ins  ','Del  ',
   'End  ','Home ','Back ','ShTab');

var
  KeyEscape : Array[1..NumWantedKeys] of String[10];


Procedure StoreKeys(var S : TStream);
var
  i,index : longint;
  l : byte;
begin
  for i:=1 to NumWantedKeys do
    if KeyEscape[i]<>'' then
      begin
        { need temporary local var, because write has var argument }
        index:=i;
        S.Write(index,Sizeof(index));
        l:=Length(KeyEscape[i]);
        S.Write(l,sizeof(l));
        S.Write(KeyEscape[i][1],l);
      end;
end;

Procedure LoadKeys(var S : TStream);
var
  i : longint;
  l : byte;
begin
  While S.GetPos<S.GetSize do
      begin
        S.Read(i,Sizeof(i));
        S.Read(l,Sizeof(l));
        S.Read(KeyEscape[i][1],l);
        KeyEscape[i][0]:=chr(l);
      end;
  SetKnownKeys;
end;

Procedure SetKnownKeys;
var
  i : longint;
begin
{$ifndef NotUseTree}
  for i:=1 to NumWantedKeys do
    if KeyEscape[i]<>'' then
      SetKey(KeyEscape[i],WantedKeys[i]);
{$endif not NotUseTree}
end;

function NiceEscape(Const St : String) : String;
var
  s : string;
  i : longint;
begin
  s:='';
  for i:=1 to length(St) do
    case ord(St[i]) of
     1..26 : s:=s+'^'+chr(ord('A')-1+Ord(St[i]));
     27 : s:=s+'Esc';
     0,28..31,127..255 : s:=s+'"#'+IntToStr(ord(St[i]))+'"';
    else
      s:=s+St[i];
    end;
  NiceEscape:=s;
end;

constructor TKeyDialog.Init(Const ATitle : String);
  var
      St : String;
      D : PCenterDialog;
      R : TRect;
      E : TEvent;
      i,hight,key : longint;
begin
  Hight:=(NumWantedKeys + 2) div 3;
  R.Assign(0,0,63 + 4,Hight + 4);
  Inherited Init(R,ATitle);
  for i:=1 to NumWantedKeys do
    begin
      GetExtent(R);
      R.Grow(-1,-1);
      R.A.Y:=R.A.Y + ((i-1) mod Hight);
      R.A.X:=R.A.X + 21 * ((i-1) div Hight);
      R.B.Y:=R.A.Y+1;
      R.B.X:=R.A.X + 10;
      St:=WantedKeysLabels[i]+' key';
      KeyOK[i]:=false;
      New(PSTL[i],Init(R,St,nil));
      Insert(PSTL[i]);
      R.A.X:=R.B.X+1;
      R.B.X:=R.B.X+11;
      New(PL[i],Init(R,20));
      St:=NiceEscape(KeyEscape[i]);
      PL[i]^.SetData(St);
      Insert(PL[i]);
      PSTL[i]^.Link:=PL[i];
    end;
  GetExtent(R);
  R.Grow(-1,-1);
  Dec(R.B.Y);
  R.A.Y:=R.B.Y-1;
  New(PST,init(R,'Press all listed keys'));
  Insert(PST);
  GetExtent(R);
  R.Grow(-1,-1);
  R.A.Y:=R.B.Y-1;
  New(PST2,init(R,'Alt prefix "'+NiceEscape(chr(AltPrefix))+'" Shift prefix = "'+
    NiceEscape(chr(ShiftPrefix))+'" Ctrl prefix = "'+NiceEscape(chr(CtrlPrefix))+'"'));
  Insert(PST2);
  InsertButtons(@Self);
end;

function TKeyDialog.Execute : Word;

var
  APL : PInputLine;
  i,j : longint;
  St : String;
  E : TEvent;
  OldKey : word;
  keyfound : boolean;
begin
{$ifndef NotUseTree}
  repeat
    EndState := 0;
    repeat
    if TypeOf(Current^)=Typeof(TInputLine) then
      APL:=PInputLine(Current)
    else if TypeOf(Current^)=Typeof(TLabel) then
      APL:=PInputLine(Plabel(Current)^.Link)
    else
      APL:=nil;
    FillChar(E,SizeOf(E),#0);
    if Keyboard.KeyPressed then
      St:=RawReadString
    else
      begin
        St:='';
        Application^.GetEvent(E);
      end;
    if E.What= evNothing then
      begin
        if St<>'' then
          begin
            if GetKey(St)<>0 then
              begin
                E.What:=evKeyDown;
                E.KeyCode:=GetKey(St);
              end
            else if St=#9 then
              begin
                E.What:=evKeyDown;
                E.KeyCode:=kbTab;
              end
            else if St=#27 then
              begin
                E.What:=evKeyDown;
                E.KeyCode:=kbEsc;
              end
            else if St=#13 then
              begin
                E.What:=evKeyDown;
                E.KeyCode:=kbEnter;
              end;
          end;
      end;
    keyFound:=false;
    if (E.What=evKeyDown) and not assigned(APL) then
      begin
        for i:=1 to NumWantedKeys do
          if E.Keycode=WantedKeys[i] then
            begin
              DisposeStr(PSTL[i]^.Text);
              PSTL[i]^.Text:=NewStr(WantedKeysLabels[i]+' OK ');
              keyFound:=true;
              keyOK[i]:=true;
              KeyEscape[i]:=St;
              St:=NiceEscape(St);
              PL[i]^.SetData(St);
              ClearEvent(E);
              ReDraw;
            end;
      end;
    if (St<>'') and not keyfound and
       ((E.What<>evKeyDown) or
       ((E.KeyCode<>kbTab) and (E.Keycode<>kbEnter) and (E.Keycode<>kbEsc))) then
      begin
        PST^.SetText('"'+NiceEscape(St)+'"');
        if Assigned(APL) then
          begin
            j:=-1;
            for i:=1 to NumWantedKeys do
              if APL=PL[i] then
                j:=i;
            if (j>0) and (j<=NumWantedKeys) then
              begin
                OldKey:=GetKey(St);
                if OldKey<>0 then
                  begin
                    for i:=1 to NumWantedKeys do
                      if (OldKey=WantedKeys[i]) and (i<>j) then
                        begin
                          If ConfirmBox('"'+St+'" is used for'+#13+
                            'key $'+hexstr(oldKey,4)+' '+WantedKeysLabels[i]+#13+
                            'Change it to '+WantedKeysLabels[j],nil,true)=cmYes then
                            begin
                              KeyEscape[i]:='';
                              PL[i]^.SetData(KeyEscape[i]);
                            end
                          else
                            begin
                              St:='';
                            end;
                        end;
                  end;
                if St<>'' then
                  Begin
                    SetKey(St,WantedKeys[j]);
                    KeyEscape[j]:=St;
                    St:=NiceEscape(St);
                    APL^.SetData(St);
                  end;
              end;
            ClearEvent(E);
          end;
      end;
  if (E.What<>evNothing) then
     HandleEvent(E);
  if E.What <> evNothing then EventError(E);
  until EndState <> 0;
  until Valid(EndState);
  Execute := EndState;
{$else NotUseTree}
  Execute:=cmCancel;
{$endif NotUseTree}
end;


procedure  LearnKeysDialog;

var
  D : PKeyDialog;
begin
{$ifdef NotUseTree}
  NotImplemented;
{$else not NotUseTree}
  New(D,Init('Learn keys'));
  Application^.ExecuteDialog(D,nil);
{$endif not NotUseTree}
end;

end.
