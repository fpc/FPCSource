{------------------------------------------------------------------------------
                                 CncWare
                           (c) Copyright 2000
 ------------------------------------------------------------------------------
  Filename..: db_demo.pp
  Programmer: Ken J. Wright, ken@cncware.com
  Date......: 06/29/2000

  Purpose - Demonstrate the use of oCrt in a simulated database record editor.

-------------------------------<< REVISIONS >>---------------------------------
  Ver  |    Date    | Prog| Description
-------+------------+-----+----------------------------------------------------
  1.00 | 06/29/2000 | kjw | Initial Release.
-------------------------------------------------------------------------------
}
Program db_demo;
Uses oCrt;
Const
   MAXCOLS = 6;
   MAXROWS = 10;
Type
   tAddress = Record
      FirstName,
      LastName,
      Street  : string[40];
      Country : string[2];
      Zip     : string[5];
      City    : string[30];
   End;

   tFields = Record
      x,y,wid : integer;
      pic : string;
   End;

Var
   win : tnWindow;
   address : Array [1..MAXROWS] of tAddress;
   fields : Array [1..MAXCOLS] of tFields;
   s : string;
   i,
   m1,m2,
   att1,att2,att3,
   row,
   col : integer;
   ch : char;
   IsDone : boolean;

Procedure Display(row : integer);
Begin
   With address[row] Do Begin
      For i := 1 to MAXCOLS Do Begin
         With fields[i] Do Begin
            Case i of
               1 : s := FirstName;
               2 : s := LastName;
               3 : s := Street;
               4 : s := Country;
               5 : s := Zip;
               6 : s := City;
            End;
            win.FWrite(x,y,att1,x+wid-1,s);
         End;
      End;
   End;
   col := 1;
End;

{ bind the arrow keys so they trigger an exit }
Procedure BindArrows;
Begin
   win.ec.Special := ^I^R^L^P^N;
   m1 := win.ec.AddChMap(#0+Char(nKeyRight)+^R#0);
   m2 := win.ec.AddChMap(#0+Char(nKeyLeft)+^L#0);
   win.FWrite(1,win.Rows,48,0,'[F2]-Arrows');
End;

Procedure UnBindArrows;
Begin
   win.ec.Special := ^R^L^P^N;
   win.ec.ClrChMap(m1);
   win.ec.ClrChMap(m2);
   win.FWrite(1,win.Rows,62,0,'[F2]-Arrows');
End;

Begin
   FillChar(address,SizeOf(address),#0);
   With address[1] Do Begin
      FirstName := 'Rainer';
      LastName := 'Hantsch';
      Street := '12345 Some Street';
      Country := 'A';
      Zip := '1030';
      City := 'Vienna';
   End;

   For i := 1 to MAXCOLS Do Begin
      With fields[i] Do Begin
         Case i of
            1 : Begin x := 14; y := 2; wid := 40; pic := ''; End;
            2 : Begin x := 14; y := 3; wid := 40; pic := ''; End;
            3 : Begin x := 14; y := 4; wid := 40; pic := ''; End;
            4 : Begin x := 14; y := 5; wid :=  2; pic := ''; End;
            5 : Begin x := 19; y := 5; wid :=  5; pic := '*#'; End;
            6 : Begin x := 27; y := 5; wid := 30; pic := ''; End;
         End;
      End;
   End;

   att1 := 19; { field display color }
   att2 := 31; { field edit color }
   att3 := 23; { labels color }

   nMakeWindow(win,1,1,60,10,att3,30,63,true,center,' Rainer''s Address Book ');
   With win Do Begin
      Align(center,center);
      FWrite(1,Rows,48,Cols,'[F2]-Arrows [F10]-Exit [Tab]-NextField [^P]-Prev [^N]-Next');
      Writeln;
      Writeln(' First Name [                                        ]');
      Writeln('  Last Name [                                        ]');
      Writeln('     Street [                                        ]');
      Write  ('   Zip/City [  ]-[     ] [                              ]');
      Show;
      ec.AddChMap(^P#0#0+Char(nKeyPgUp));
      ec.AddChMap(^N#0#0+Char(nKeyPgDn));
      BindArrows;
      row := 1;
      col := 1;
      display(row);
      IsDone := false;
      Repeat
         Str(row:2,s);
         FWrite((cols-10) div 2,rows-1,26,0,'Record #'+s);
         With address[row] Do Begin
            With fields[col] Do Begin
               ec.Picture := pic;
               Case col of
                  1 : s := FirstName;
                  2 : s := LastName;
                  3 : s := Street;
                  4 : s := Country;
                  5 : s := Zip;
                  6 : s := City;
               End;
               s := Edit(x,y,att2,x+wid-1,x+Length(s),s,ch);
               If ch <> #27 Then
                  Case col of
                     1 : FirstName := s;
                     2 : LastName := s;
                     3 : Street := s;
                     4 : Country := s;
                     5 : Zip := s;
                     6 : City := s;
                  End;
               FWrite(x,y,att1,x+wid-1,s);
               Case Ord(ch) of
                   9,
                  13,
                  Ord(^r) : Inc(col);
                  Ord(^l) : Dec(col);
                  nKeyUp : Case col of
                     1 : col := 4;
                     2,3,4 : Dec(col);
                     5,6 : col := 3;
                  End;
                  nKeyDown : Case col of
                     1..3 : Inc(col);
                     4..6 : col := 1;
                  End;
                  nKeyPgDn : Inc(row);
                  nKeyPgUp : Dec(row);
                  nKeyF2 : UnBindArrows; { use arrows for editing }
                  nKeyF10 : IsDone := true;
               End;
            End;
         End;
         If row > MAXROWS Then row := MAXROWS;
         If row < 1 Then row := 1;
         If col > MAXCOLS Then col := 1;
         If col < 1 Then col := MAXCOLS;
         If Ord(ch) in [nKeyPgUp,nKeyPgDn] Then Display(row);
         If Ord(ch) <> nKeyF2 Then BindArrows; { arrows for navigation }
      Until IsDone;
      Hide;
      Done;
   End;
End.
