{ Test program for PalmOS support of FPC
  Ported from pilrctst.c by Florian Klaempfl (florian@freepascal.org)

  pilrctst.c was made by
  Wes Cherry (wesc@ricochet.net)
}
{$APPID FPCA}
{$APPNAME FPC Demo}

{$R pilrctst.rcp}

program pilrctst;

  uses
     palm;

  const
     kidForm1 = 1000;
     kidForm2 = 1001;
     kidForm3 = 1002;
     kidForm4 = 1003;
     kidForm5 = 1004;
     kidForm6 = 1005;
     kidForm7 = 1006;
     kidForm8 = 1007;
     kidForm9 = 1008;
     kidForm10 = 1009;
     kidForm11 = 1010;
     kidForm12 = 1011;
     kidFormLast = 1011;
     kidOk = 9999;
     kidTable = 1000;
     kidAlert1 = 1000;
     kidMenu1 = 1000;
     kidHelp1 = 1000;
     kidHelp2 = 1001;
     kidBitmap = 1000;

  var
     kidForm : longint;

  var
     b : boolean;
     e : EventType;
     err : word;
     pfrm : PFormType;
     ptbl : PTableType;

begin
  kidForm:=kidForm3;
  FrmGotoForm(kidForm);
  while true do
    begin
      EvtGetEvent(e,100);
      if SysHandleEvent(e)<>0 then
        continue;
      if MenuHandleEvent(nil,e,err)<>0 then
        continue;
      case e.eType of
         ctlSelectEvent:
           begin
             if e.data.controlID=kidOk then
               begin
                 inc(kidForm);
                 if kidForm>kidFormLast then
                   kidForm:=kidForm1;
                  FrmGotoForm(kidForm);
                end;
              if assigned(FrmGetActiveForm) then
                FrmHandleEvent(FrmGetActiveForm,e);
           end;
         frmLoadEvent:
           begin
             FrmSetActiveForm(FrmInitForm(e.data.formID));
           end;
         frmOpenEvent:
           begin
             pfrm:=FrmGetActiveForm;
             FrmDrawForm(pfrm);
             if e.data.formID=kidForm12 then
               begin
                 ptbl:=PTableType(FrmGetObjectPtr(pfrm, FrmGetObjectIndex(pfrm, kidTable)));
                 TblSetColumnUsable(ptbl, 0, true);
                 TblSetColumnUsable(ptbl, 1, true);
                 TblSetColumnUsable(ptbl, 2, true);
                 TblSetColumnUsable(ptbl, 3, true);
                 TblSetColumnUsable(ptbl, 4, true);
                 TblSetRowUsable(ptbl, 0, true);
                 TblSetRowUsable(ptbl, 1, true);

                 TblSetItemStyle(ptbl, 0, 0, textTableItem);
                 TblSetItemStyle(ptbl, 1, 0, textTableItem);
                 TblSetItemStyle(ptbl, 2, 0, textTableItem);
                 TblDrawTable(ptbl);
               end;
            end;
         menuEvent:
           FrmAlert(kidAlert1);
         appStopEvent:
           break;
         else
           if assigned(FrmGetActiveForm) then
             FrmHandleEvent(FrmGetActiveForm,e);
      end;
    end;
end.
