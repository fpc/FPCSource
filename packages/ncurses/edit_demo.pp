Program Edit_Demo;
{---------------------------------------------------------------------------
                                 CncWare
                           (c) Copyright 1999
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
------------------------------------------------------------------------------
}
uses oCrt;
var
   ss : array[1..25] of string[80];
   xp,yp,
   s : string;
   c : char;
   win1,status : tnWindow;
   idx : integer;
   Finished : boolean;

Begin
   Status.Init(1,25,80,25,63,false,0);
   Status.FWrite(1,1,63,80,' [F1-InsLn]  [F2-DelLn]  [F10-Exit]');
   Status.Show;
   fillchar(ss,sizeof(ss),#0);
   win1.Init(1,1,80,24,31,true,31);
   win1.PutHeader(' nCrt Editor Demonstration ',15,center);
   win1.Show;
   win1.GotoXY(1,1);
   {--------------------------------------------------------------------
     The next line causes sedit to exit after every keystroke so we can
     capture the insert mode and cursor positions.
    --------------------------------------------------------------------}
   win1.ec.ExitMode := true;
   idx := 1;
   Finished := false;
   Repeat
      With win1 Do Begin
         Case ec.InsMode of
            true : Status.FWrite(40,1,48,0,'Ins');
            false: Status.FWrite(40,1,48,0,'Ovr');
         End;
         Str(WhereX:0,xp);
         Str(WhereY:0,yp);
         Status.FWrite(50,1,48,80,'X='+xp+', Y='+yp);
         ss[idx] := SEdit(1,idx,30,Cols,WhereX,ss[idx],c);
         Case ord(c) of
            nKeyUp : dec(idx);
            nKeyDown : inc(idx);
            nKeyPgUp : idx := 1;
            nKeyPgDn : idx := Rows;
            nKeyEnter: Begin
                          inc(idx);
                          GotoXY(1,WhereY);
                       End;
            nKeyF1   : Begin
                          InsLine;
                          system.move(ss[idx],ss[idx+1],(rows-idx)*81);
                          ss[idx] := '';
                       End;
            nKeyF2   : Begin
                          DelLine;
                          system.move(ss[idx+1],ss[idx],(rows-idx)*81);
                          ss[rows] := '';
                       End;
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
   halt(1);
End.
