/* pilrctst.c:  Test program for PilRC
 *
 * Wes Cherry
 * wesc@ricochet.net
 */

#include "pilrctst.h"
#include <Common.h>
#include <System/SysAll.h>
#include <UI/UIAll.h>

int kidForm;


DWord PilotMain(Word cmd, Ptr cmdPBP, Word launchFlags)

	short err;
	EventType e;
	FormType *pfrm;
					TableType *ptbl;
	

     begin
	if cmd=0 then
          begin
	     kidForm:=kidForm1;
	     FrmGotoForm(kidForm);
             while true do
  	       begin
	          EvtGetEvent(@e,100);
		  if SysHandleEvent(@e) then
 		    continue;
		  if MenuHandleEvent(nil,@e,@err) then
		    continue;
	
		  case e.eType of
		     ctlSelectEvent:
				if (e.data.ctlSelect.controlID == kidOk)
					begin
					kidForm++;
					if (kidForm > kidFormLast)
						kidForm = kidForm1;
					FrmGotoForm(kidForm); 
					end;
				goto Dft;
		     frmLoadEvent:
	               FrmSetActiveForm(FrmInitForm(e.data.frmLoad.formID));
   	             frmOpenEvent:
                       begin
			  pfrm:=FrmGetActiveForm;
			  FrmDrawForm(pfrm);
			  if e.data.frmLoad.formID=kidForm12 then
			    begin
			       ptbl:=FrmGetObjectPtr(pfrm, FrmGetObjectIndex(pfrm, kidTable));
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
			case menuEvent:
				FrmAlert(kidAlert1);
				break;

			case appStopEvent:
				return 0;

			default:
Dft:
				if (FrmGetActiveForm())
					FrmHandleEvent(FrmGetActiveForm(), &e);
				end
			end
		end
	return 0;
	end

