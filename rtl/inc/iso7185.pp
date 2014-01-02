{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 2010 by Florian Klaempfl

    This unit contain procedures specific for iso pascal mode.
    It should be platform independant.

    See the file COPYING.FPC, included in this distribution,
    For details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit iso7185;

  interface

    const
       MaxInt  = MaxLongint;

    type
      Integer = Longint;

    Procedure Rewrite(var t : Text);
    Procedure Reset(var t : Text);
    Procedure Reset(var f : TypedFile);   [INTERNPROC: fpc_in_Reset_TypedFile];
    Procedure Rewrite(var f : TypedFile); [INTERNPROC: fpc_in_Rewrite_TypedFile];

    Function Eof(Var t: Text): Boolean;
    Function Eof:Boolean;
    Function Eoln(Var t: Text): Boolean;
    Function Eoln:Boolean;

    Procedure Page;
    Procedure Page(Var t: Text);

    Procedure Get(Var t: Text);
    Procedure Put(Var t: Text);

    Function Eof(var f:TypedFile): Boolean;

  implementation

{$i-}
    procedure DoAssign(var t : Text);
      begin
        Assign(t,'fpc_'+HexStr(random(1000000000),8)+'.tmp');
      end;


    Procedure Rewrite(var t : Text);[IOCheck];
      Begin
        { create file name? }
        if Textrec(t).mode=0 then
          DoAssign(t);

        System.Rewrite(t);
      End;


    Procedure Reset(var t : Text);[IOCheck];
      Begin
        case Textrec(t).mode of
          { create file name? }
          0:
            DoAssign(t);
          fmOutput:
            Write(t,#26);
        end;

        System.Reset(t);
      End;


    Function Eof(Var t: Text): Boolean;[IOCheck];
      var
        OldCtrlZMarksEof : Boolean;
      Begin
        OldCtrlZMarksEof:=CtrlZMarksEOF;
        CtrlZMarksEof:=false;
        Eof:=System.Eof(t);
        CtrlZMarksEof:=OldCtrlZMarksEOF;
      end;


    Function Eof:Boolean;
      Begin
        Eof:=Eof(Input);
      End;


    Function Eoln(Var t: Text): Boolean;[IOCheck];
      var
        OldCtrlZMarksEof : Boolean;
      Begin
        OldCtrlZMarksEof:=CtrlZMarksEOF;
        CtrlZMarksEof:=true;
        Eoln:=System.Eoln(t);
        CtrlZMarksEof:=OldCtrlZMarksEOF;
      end;


    Function Eoln:Boolean;
      Begin
        Eoln:=Eoln(Input);
      End;


    Procedure Page;
      begin
        Page(Output);
      end;


    Procedure Page(var t : Text);
      Begin
        write(#12);
      End;


    procedure Get(var t : Text);
      var
        c : char;
      Begin
        Read(t,c);
      End;


    Procedure Put(var t : Text);
      type
        FileFunc = Procedure(var t : TextRec);
      begin
        inc(TextRec(t).BufPos);
        If TextRec(t).BufPos>=TextRec(t).BufSize Then
          FileFunc(TextRec(t).InOutFunc)(TextRec(t));
      end;


    Function Eof(var f:TypedFile): Boolean;
      Type
        UnTypedFile = File;
      Begin
        Eof:=System.Eof(UnTypedFile(f));
      End;

begin
  { we shouldn't do this because it might confuse user programs, but for now it
    is good enough to get pretty unique tmp file names }
  Randomize;
end.




