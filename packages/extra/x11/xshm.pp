unit xshm;
interface
uses
  x,xlib;

{$ifndef os2}
  {$LinkLib c}
  {$LinkLib X11}
const
  libX11='X11';
{$else}
const
  libX11='X11';
{$endif}

{
  Automatically converted by H2Pas 0.99.15 from xshm.h
  The following command line parameters were used:
    -p
    -T
    -S
    -d
    -c
    xshm.h
}

{$PACKRECORDS C}


const
   X_ShmQueryVersion = 0;
   X_ShmAttach = 1;
   X_ShmDetach = 2;
   X_ShmPutImage = 3;
   X_ShmGetImage = 4;
   X_ShmCreatePixmap = 5;
   ShmCompletion = 0;
   ShmNumberEvents = ShmCompletion + 1;
   BadShmSeg = 0;
   ShmNumberErrors = BadShmSeg + 1;
type

   PShmSeg = ^TShmSeg;
   TShmSeg = dword;

   PXShmCompletionEvent = ^TXShmCompletionEvent;
   TXShmCompletionEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        drawable : TDrawable;
        major_code : longint;
        minor_code : longint;
        shmseg : TShmSeg;
        offset : dword;
     end;

   PXShmSegmentInfo = ^TXShmSegmentInfo;
   TXShmSegmentInfo = record
        shmseg : TShmSeg;
        shmid : longint;
        shmaddr : Pchar;
        readOnly : TBool;
     end;

function XShmQueryExtension(para1:PDisplay):TBool;cdecl;external libX11;
function XShmQueryVersion(para1:PDisplay; para2:Plongint; para3:Plongint; para4:PBool):TBool;cdecl;external libX11;
function XShmPixmapFormat(para1:PDisplay):longint;cdecl;external libX11;
function XShmAttach(para1:PDisplay; para2:PXShmSegmentInfo):TStatus;cdecl;external libX11;
function XShmDetach(para1:PDisplay; para2:PXShmSegmentInfo):TStatus;cdecl;external libX11;
function XShmPutImage(para1:PDisplay; para2:TDrawable; para3:TGC; para4:PXImage; para5:longint;
           para6:longint; para7:longint; para8:longint; para9:dword; para10:dword;
           para11:TBool):TStatus;cdecl;external libX11;
function XShmGetImage(para1:PDisplay; para2:TDrawable; para3:PXImage; para4:longint; para5:longint;
           para6:dword):TStatus;cdecl;external libX11;
function XShmCreateImage(para1:PDisplay; para2:PVisual; para3:dword; para4:longint; para5:Pchar;
           para6:PXShmSegmentInfo; para7:dword; para8:dword):PXImage;cdecl;external libX11;
function XShmCreatePixmap(para1:PDisplay; para2:TDrawable; para3:Pchar; para4:PXShmSegmentInfo; para5:dword;
           para6:dword; para7:dword):TPixmap;cdecl;external libX11;

implementation


end.
