Program Edit_Demo;
{---------------------------------------------------------------------------
                                 CncWare
                         (c) Copyright 1999-2000
 ---------------------------------------------------------------------------
  Filename..: edit_demo.pp
  Programmer: Ken J. Wright, ken@cncware.com
  Date......: 12/12/99

  Purpose - Demonstrate the use of nCrt unit.

-------------------------------<< REVISIONS >>--------------------------------
  Ver  |   Date   | Prog| Description
-------+----------+-----+-----------------------------------------------------
  1.00 | 12/12/99 | kjw | Initial Release.
  1.01 | 12/13/99 | kjw | Changed to use oCrt.
  1.02 | 06/16/00 | kjw | Added help & goto line pop-up screens.
                        | Changes for control keys.
------------------------------------------------------------------------------
}
uses oCrt;
var
   ss : array[1..25] of string[80];
   xp,yp : string;
   c : char;
   win1,status : tnWindow;
   idx : integer;
   Finished : boolean;

Procedure Help;
Var
   hwin : pnWindow;
Begin
   New(hwin,Init(1,1,40,20,62,true,49));
   With hwin^ Do Begin
      Align(center,center);
      PutHeader('Edit_Demo Help',15,center);
      FWrite(2, 2,63,0,'Ctrl/Q    - Move to column 1');
      FWrite(2, 3,63,0,'Ctrl/W    - Move to end of line');
      FWrite(2, 4,63,0,'Ctrl/A    - Move to previous word');
      FWrite(2, 5,63,0,'Ctrl/F    - Move to next word');
      FWrite(2, 6,63,0,'Ctrl/G    - Delete character');
      FWrite(2, 7,63,0,'Ctrl/H    - Destructive Backspace');
      FWrite(2, 8,63,0,'Ctrl/D    - Move forward one column');
      FWrite(2, 9,63,0,'Ctrl/S    - Move back one column');
      FWrite(2,10,63,0,'Ctrl/I    - Toggle Insert/Overwrite');
      FWrite(2,11,63,0,'Ctrl/P    - Embed control character');
      FWrite(2,12,63,0,'Ctrl/L    - Goto line number');
      FWrite(2,13,63,0,'Ctrl/N    - Insert new line');
      FWrite(2,14,63,0,'Ctrl/Y    - Delete current line');
      FWrite(2,15,63,0,'Ctrl/X    - Move down one line');
      FWrite(2,16,63,0,'Ctrl/E    - Move up one line');
      FWrite(2,17,63,0,'Esc/1..0  - F1..F10');
      Show;
      Repeat Until Keypressed;
      While KeyPressed Do ReadKey;
      Hide;
   End;
   Dispose(hwin,Done);
End;

Procedure GotoLine(var i : integer);
Var
   gwin : pnWindow;
   ii : integer;
   esc : boolean;
Begin
   New(gwin,Init(1,1,40,3,62,true,49));
   With gwin^ Do Begin
      Align(center,center);
      PutHeader('Goto Line Number',15,center);
      FWrite(2,1,63,0,'Line: ');
      Show;
      ii := i;
      ec.ClearMode := true;
      i := EditNumber(8,1,63,2,0,'',i,1,win1.rows,esc);
      If esc or not (i in [1..win1.rows]) Then i := ii;
      Hide;
   End;
   Dispose(gwin,Done);
End;

Begin
   Status.Init(1,nStdScr.Rows,nStdScr.Cols,nStdScr.Rows,63,false,0);
   nFWrite(1,1,63,80,' [F1-InsLn]  [F2-DelLn]  [F3-Help]  [F10-Exit]');
   Status.Show;
   fillchar(ss,sizeof(ss),#0);
   With win1 Do Begin
      Init(1,1,nStdScr.Cols,nStdScr.Rows-1,31,true,31);
      PutHeader(' nCrt Editor Demonstration ',15,center);
      Show;
      GotoXY(1,1);
      {--------------------------------------------------------------------
        The next line causes sedit to exit after every keystroke so we can
        capture the insert mode and cursor positions for display update.
        Alternatively, we could setup an ec.Special string to exit only on
        certain keystrokes of interest.
       --------------------------------------------------------------------}
      ec.ExitMode := true;
      { too re-assign a built-in key, put it in ec.special,
        then use it in the case statement below

      win1.ec.Special := win1.ec.Special + #5;
      }
      { now let's bind some keystrokes to the editor screen }
      ec.AddChMap(^a#0#0+char(nKeyCtrlLeft));
      ec.AddChMap(^s#0#0+char(nKeyLeft));
      ec.AddChMap(^f#0#0+char(nKeyCtrlRight));
      ec.AddChMap(^d#0#0+char(nKeyRight));
      ec.AddChMap(^e#0#0+char(nKeyUp));
      ec.AddChMap(^x#0#0+char(nKeyDown));
      ec.AddChMap(^q#0#0+char(nKeyHome));
      ec.AddChMap(^w#0#0+char(nKeyEnd));
   End;
   idx := 1;
   Finished := false;
   Repeat
      With win1 Do Begin
         Case ec.InsMode of
            true : Status.FWrite(50,1,48,0,'Ins');
            false: Status.FWrite(50,1,48,0,'Ovr');
         End;
         Str(WhereX:0,xp);
         Str(WhereY:0,yp);
         Status.FWrite(60,1,48,80,'X='+xp+', Y='+yp);
         ss[idx] := Edit(1,idx,30,Cols,WhereX,ss[idx],c);
         Case ord(c) of
                  12 : GotoLine(idx);
            {5,}
            nKeyUp   : dec(idx);
            nKeyDown : inc(idx);
            nKeyPgUp : idx := 1;
            nKeyPgDn : idx := Rows;
            nKeyEnter: Begin
                          inc(idx);
                          GotoXY(1,WhereY);
                       End;
            14, { ctrl/n }
            nKeyF1   : Begin
                          InsLine;
                          system.move(ss[idx],ss[idx+1],(rows-idx)*81);
                          ss[idx] := '';
                       End;
            25, { ctrl/y }
            nKeyF2   : Begin
                          DelLine;
                          system.move(ss[idx+1],ss[idx],(rows-idx)*81);
                          ss[rows] := '';
                       End;
            nKeyF3   : Help;
            nKeyEsc,
            nKeyF10  : Finished := true;
         End;
         If idx > rows Then idx := rows;
         If idx < 1 Then idx := 1;
         GotoXY(WhereX,idx);
      End;
   Until Finished;
   win1.Done;
   Status.Done;
   ClrScr;
End.
{
  $Log$
  Revision 1.3  2000-08-20 10:11:41  jonas
    * added missing open comment at start of log section

  Revision 1.2  2000/07/13 11:33:27  michael
  + removed logs

}
