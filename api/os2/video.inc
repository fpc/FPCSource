{

  System independent low-level video interface for os/2



  $Id$

}



uses
{$IFDEF PPC_FPC}
 VioCalls;
{$ELSE}
 {$IFDEF PPC_VIRTUAL}
 Os2Base;
 {$ENDIF}
{$ENDIF}



var videobuf:Pvideobuf;

    videobufsize:cardinal;

    lastcursortype:word=crunderline;

    cell_width,cell_height:word;



{$ASMMODE ATT}



procedure update_cell_size;



begin

    {This function cannot fail when the default handle is used.}

    viogetdevicecellsize(cell_height,cell_width,0);

end;



procedure initvideo;



begin

    initvideocalled:=true;

    {Get the address of the videobuffer.}

    if viogetbuf(videobuf,videobufsize,0)=0 then

        begin

            update_cell_size;

            sethighbitblink;

            setcursortype(lastcursortype);

        end

    else

        errcode:=errvioinit;

end;



procedure setcursorpos(newcursorx,newcursory:word);



begin

    if viosetcurpos(newcursory,newcursorx,0)<>0 then

        {Do not set an error code; people should fix invalid newcursorx or

         newcursory values when designing, there is no need for detecting

         these errors at runtime.}

        runerror(225);

end;



function getcursortype:word;



var cd:Tviocursorinfo;



begin

    viogetcurtype(cd,0);    {Never fails, because handle is default handle.}

    if cd.attr=-1 then

        getcursortype:=crhidden

    else

        {Because the cursor's start and end lines are returned, we'll have

         to guess heuristically what cursor type we have.}

        case cd.cend-cd.ystart of

            0:

                {Propably this does not occur, but you'll never know.}

                getcursortype:=crhidden;

            1..cell_height div 4:

                getcursortype=crunderline;

            cell_height div 4..cell_height div 2:

                getcursortype:=crhalfblock;

            else

                getcursortype:=crblock;

        end;

end;



procedure setcursortype;



begin

end;



procedure donevideo;



begin

    If initvideocalled then

        begin

            lastcursortype:=getcursortype;

            clearscreen;

            setcursortype(crunderline);

            setcursorpos(0,0);

            initvideocalled:=false;

            videobufsize:=0;

        end;

end;





function GetCapabilities: Word;



begin

  GetCapabilities := $3F;

end;





procedure SetCursorPos(NewCursorX, NewCursorY: Word);



begin

end;





function GetCursorType: Word;



begin

end;





procedure SetCursorType(NewType: Word);



begin

end;





function DefaultVideoModeSelector(const VideoMode: TVideoMode; Params: Longint): Boolean;



begin

end;





procedure ClearScreen;



begin

end;





procedure UpdateScreen(Force: Boolean);

begin

  if LockUpdateScreen<>0 then

   exit;

  if not force then

   begin

	 asm

		movl    VideoBuf,%esi

		movl    OldVideoBuf,%edi

		movl    VideoBufSize,%ecx

		shrl    $2,%ecx

		repe

		cmpsl

		orl     %ecx,%ecx

		jz      .Lno_update

		movb    $1,force

.Lno_update:

	 end;

   end;

  if Force then

   begin

	 dosmemput(videoseg,0,videobuf^,VideoBufSize);

	 move(videobuf^,oldvideobuf^,VideoBufSize);

   end;

end;





procedure RegisterVideoModes;

begin

  RegisterVideoMode(40, 25, False, DefaultVideoModeSelector, $00000000);

  RegisterVideoMode(40, 25, True, DefaultVideoModeSelector, $00000001);

  RegisterVideoMode(80, 25, False, DefaultVideoModeSelector, $00000002);

  RegisterVideoMode(80, 25, True, DefaultVideoModeSelector, $00000003);

end;



{

  $Log$
  Revision 1.1  2000-07-13 06:29:40  michael
  + Initial import

  Revision 1.2  2000/07/09 07:56:09  hajny
    * proper units added

  Revision 1.1  2000/02/23 22:44:22  daniel
  * Video.inc for os/2


  Revision 1.4  1998/12/15 17:17:17  peter

    + cursor at 1,1 at the end



  Revision 1.3  1998/12/12 19:13:01  peter

    * keyboard updates

    * make test target, make all only makes units



  Revision 1.2  1998/12/10 11:41:50  florian

    * cursor is properly restored in DoneVideo



  Revision 1.1  1998/12/04 12:48:27  peter

    * moved some dirs



  Revision 1.4  1998/11/01 20:29:11  peter

    + lockupdatescreen counter to not let updatescreen() update



  Revision 1.3  1998/10/28 21:18:26  peter

    * more fixes



  Revision 1.2  1998/10/28 00:02:08  peter

    + mouse

    + video.clearscreen, video.videobufsize



  Revision 1.1  1998/10/26 11:31:47  peter

    + inital include files



}

