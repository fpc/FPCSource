Program ModeList;


{ ***********************************************************************
  * This is example shows how to use p96AllocModeListTagList()
  *
  * tabt (Sat Dec 28 03:44:35 1996)
  *********************************************************************** }

{
    Translated to fpc pascal.
    2 Mars 2001.

    Updated for fpc 1.0.7
    08 Jan 2003.

    nils.sjoholm@mailbox.swipnet.se
}

uses exec, amigados, picasso96api, utility;
Const
    template    :   pchar  =   'Width=W/N,Height=H/N,Depth=D/N';
    vecarray    :   Array[0..2] of long = (0,0,0);

Var
    ml          :   pList;
    width,
    height,
    depth       :   longint;
    rda         :   pRDArgs;
    mn          :   pP96Mode;

Begin
  width:=640;
  height:=480;
  depth:=8;

  rda:=ReadArgs (template,@vecarray,Nil);
  If rda<>Nil Then Begin
      If vecarray[0] <> 0 then width := long(@vecarray[0]);
      If vecarray[1] <> 0 then height := long(@vecarray[1]);
      If vecarray[2] <> 0 then depth := long(@vecarray[2]);
      FreeArgs(rda);
  End;

  ml:=p96AllocModeListTags([P96MA_MinWidth, width,
                            P96MA_MinHeight, height,
                            P96MA_MinDepth, depth,
                            TAG_DONE]);

  If ml<>Nil Then Begin
     mn := pointer(ml^.lh_Head);
     If mn <> Nil Then Begin
        While mn^.Node.ln_Succ <> Nil Do Begin
            Writeln (mn^.Description);
            mn := pointer(mn^.Node.ln_Succ);
        End;
     End;
     p96FreeModeList(ml);
  End Else
     Writeln ('Unable to allocate list.');
End.
