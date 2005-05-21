{$MODE objFPC}
unit tb0427;
// Purpose: Demonstrate Internal Error #10
//
// Version: Free Pascal Compiler version 1.0.6 [2002/04/23] for i386
//          Copyright (c) 1993-2002 by Florian Klaempfl
//
// Compiler Output:
//  Free pascal Compiler version 1.0.6 [2002/04/23] for i386
//  Copyright (c) 1993-2002 by Florian Klaempfl
//  Target OS: Win32 for i386
//  Compiling c:\windows\desktop\files\projects\sageapi\t.pas
//  t.pas(68,51) Fatal: Internal error 10
//
// Bug Contributor:
//   Jason Sage
//   jazesage@aol.com
//
// Date Contributed:  2002-12-01
// System OS: MS Windows ME v4.90.3000
// System: Compaq, Genuine Intel, Intel(r) Celeron(tm) processor
//         63.0MB Ram
//
interface

implementation

type TClass = class
  protected
  VBuf: ^word;
  public
  constructor Init;
  destructor Done;
  procedure Test(p_dwNewWidth, p_dwNewHeight: Cardinal);
end;

var
    MyClass: TClass;

constructor TClass.Init; begin GetMem(VBuf,2); end;

destructor TClass.Done; begin freemem(VBuf); end;

procedure TClass.Test(p_dwNewWidth, p_dwNewHeight: Cardinal);
var
  OldVBuf: ^word;
  t,s: Cardinal;
  w,h: Cardinal; // preserve Width and Height of VC
  wData: word;
begin
  getmem(OldVBuf,1); freemem(OldVBuf); // shutoff hint
  w:=w; h:=h; // shut off hint
  OldVBuf:=VBuf;
  GetMem(VBuf, p_dwNewWidth * p_dwNewHeight * 2);
  for t:=1 to W do // won't cause error if you do this the more efficient
  begin            // way: for t:=0 to W-1 do
    for s:=1 to H do//     for s:=0 to H-1 do
    begin           // and replace the [(t-1)+((s-1)*W)] logic to [t+s*w]
      if (t<=p_dwNewWidth) and (s<=p_dwNewHeight) then
      begin
        {
        // This is the work around that I used in my UNIT and the code Works
        wData:=OldVBuf[(t-1)+(s-1)*H];
        VBuf[(t-1)+((s-1)*p_dwNewWidth)]:=wData;
        }

        // This way causes an Internal Error 10 from the compiler.
        VBuf[(t-1)+((s-1)*p_dwNewWidth)]:=OldVBuf[(t-1)+(s-1)*H];
      end;
    end;
  end;
  Freemem(OldVBuf);
end;

begin
  MyClass:=TClass.Init;
  MyClass.Test(1,1);
  MyClass.Done;
end.
