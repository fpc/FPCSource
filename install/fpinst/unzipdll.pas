{
  $Id$
}
unit UnzipDLL;

{$IFDEF VIRTUALPASCAL}
 {$Cdecl+,AlignRec-,OrgName+}
{$ELSE}
 {$IFDEF FPC}
  {$PACKRECORDS 1}
 {$ENDIF}
{$ENDIF}

interface

const
{$IFDEF OS2}
 AllFiles: string [1] = '*';
{$ELSE}
 AllFiles: string [3] = '*.*';
{$ENDIF}
 UnzipErr: longint = 0;

type
 TArgV = array [0..1024] of PChar;
 PArgV = ^TArgV;
 TCharArray = array [1..1024*1024] of char;
 PCharArray = ^TCharArray;

function FileUnzipEx (SourceZipFile, TargetDirectory,
                                                    FileSpecs: PChar): integer;
(* Returns non-zero result on success. *)

implementation

uses
{$IFDEF OS2}
 {$IFDEF FPC}
     DosCalls,
 {$ELSE FPC}
  {$IFDEF VirtualPascal}
     OS2Base,
  {$ELSE VirtualPascal}
     BseDos,
  {$ENDIF VirtualPascal}
 {$ENDIF FPC}
{$ELSE}
 {$IFDEF WIN32}
     Windows,
 {$ENDIF WIN32}
{$ENDIF OS2}
 Dos;

type
 UzpMainFunc = function (ArgC: longint; var ArgV: TArgV): longint; cdecl;

const
{$IFDEF OS2}
 LibPath = 'LIBPATH';
{$ELSE}
 LibPath = 'PATH';
{$ENDIF}
 UzpMainOrd = 4;
 DLLName: string [8] = 'UNZIP32'#0;
 UzpMain: UzpMainFunc = nil;
 QuiteOpt: array [1..4] of char = '-qq'#0;
 OverOpt: array [1..3] of char = '-o'#0;
 CaseInsOpt: array [1..3] of char = '-C'#0;
 ExDirOpt: array [1..3] of char = '-d'#0;
 OptCount = 4;

var
 DLLHandle: longint;
 OldExit: pointer;

function DLLInit: boolean;
var
{$IFDEF OS2}
 ErrPath: array [0..259] of char;
{$ENDIF}
 DLLPath: PathStr;
 Dir: DirStr;
 Name: NameStr;
 Ext: ExtStr;
begin
 DLLInit := false;
 FSplit (FExpand (ParamStr (0)), Dir, Name, Ext);
 DLLPath := Dir + DLLName;
 Insert ('.DLL', DLLPath, byte (DLLPath [0]));
{$IFDEF OS2}
 if (DosLoadModule (@ErrPath, SizeOf (ErrPath), @DLLPath [1], DLLHandle) <> 0)
 and (DosLoadModule (@ErrPath, SizeOf (ErrPath), @DLLName [1], DLLHandle) <> 0)
                                                                           then
 begin
  if ErrPath [0] <> #0 then
  begin
   Write (#13#10'Error while loading module ');
   WriteLn (PChar (@ErrPath));
  end;
 end else DLLInit := DosQueryProcAddr (DLLHandle, UzpMainOrd, nil, @UzpMain) = 0;
{$ELSE}
 {$IFDEF WIN32}
 DLLHandle := LoadLibrary (@DLLPath [1]);
 if DLLHandle = 0 then DLLHandle := LoadLibrary (@DLLName [1]);
 if DLLHandle = 0 then WriteLn (#13#10'Error while loading DLL.') else
 begin
(*  UzpMain := UzpMainFunc (GetProcAddress (DLLHandle, 'UzpMain'));
*)
  UzpMain := UzpMainFunc (GetProcAddress (DLLHandle, 'Unz_Unzip'));
  DLLInit := Assigned (UzpMain);
 end;
 {$ENDIF}
{$ENDIF}
end;

procedure NewExit;
begin
 ExitProc := OldExit;
{$IFDEF OS2}
 DosFreeModule (DLLHandle);
{$ELSE}
 {$IFDEF WIN32}
 FreeLibrary (DLLHandle);
 {$ENDIF}
{$ENDIF}
end;

function FileUnzipEx (SourceZipFile, TargetDirectory,
                                                    FileSpecs: PChar): integer;
var
 I, FCount, ArgC: longint;
 ArgV: TArgV;
 P: PChar;
 StrLen: array [Succ (OptCount)..1024] of longint;
begin
 ArgV [0] := @DLLName;
 ArgV [1] := @QuiteOpt;
 ArgV [2] := @OverOpt;
 ArgV [3] := @CaseInsOpt;
 ArgV [4] := SourceZipFile;
 FCount := 0;
 if FileSpecs^ <> #0 then
 begin
  P := FileSpecs;
  I := 0;
  repeat
   case FileSpecs^ of
    '"': begin
          Inc (FileSpecs);
          repeat Inc (I) until (FileSpecs^ = '"') or (FileSpecs^ = #0);
          Inc (FileSpecs);
          Inc (I);
         end;
    '''':  begin
            Inc (FileSpecs);
            repeat Inc (I) until (FileSpecs^ = '''') or (FileSpecs^ = #0);
            Inc (FileSpecs);
            Inc (I);
           end;
    #0, ' ', #9: begin
                  Inc (I);
                  Inc (FCount);
                  GetMem (ArgV [OptCount + FCount], I);
                  Move (P^, ArgV [OptCount + FCount]^, Pred (I));
                  PCharArray (ArgV [OptCount + FCount])^ [I] := #0;
                  StrLen [OptCount + FCount] := I;
                  while (FileSpecs^ = #9) or (FileSpecs^ = ' ') do Inc (FileSpecs);
                  P := FileSpecs;
                  I := 0;
                 end;
    else
    begin
     Inc (I);
     Inc (FileSpecs);
    end;
   end;
  until (FileSpecs^ = #0) and (I = 0);
 end else
 begin
  FCount := 1;
  StrLen [OptCount + FCount] := Succ (byte (AllFiles [0]));
  GetMem (ArgV [OptCount + FCount], StrLen [OptCount + FCount]);
  Move (AllFiles [1], ArgV [OptCount + FCount]^, StrLen [OptCount + FCount]);
 end;
 ArgC := Succ (FCount + OptCount);
 ArgV [ArgC] := @ExDirOpt;
 Inc (ArgC);
 ArgV [ArgC] := TargetDirectory;
 Inc (ArgC);
 ArgV [ArgC] := @ExDirOpt [3]; (* contains #0 *)
 UnzipErr := UzpMain (ArgC, ArgV);
 if UnzipErr <> 0 then FileUnzipEx := 0 else FileUnzipEx := FCount;
 for I := 1 to FCount do FreeMem (ArgV [I + OptCount], StrLen [I + OptCount]);
end;

begin
 if DLLInit then
 begin
  OldExit := ExitProc;
  ExitProc := @NewExit;
 end else
 begin
  WriteLn (#13#10'Dynamic library UNZIP32.DLL from InfoZip is needed to install.');
  WriteLn ('This library could not be found on your system, however.');
  WriteLn ('Please, download the library, either from the location where you found');
  WriteLn ('this installer, or from any FTP archive carrying InfoZip programs.');
  WriteLn ('If you have this DLL on your disk, please, check your configuration (' + LIBPATH + ').');
  Halt (255);
 end;
end.
{
  $Log$
  Revision 1.5  2000-06-18 18:27:32  hajny
    + archive validity checking, progress indicator, better error checking

  Revision 1.4  2000/06/13 16:21:36  hajny
    * Win32 support corrected/completed

  Revision 1.3  2000/03/05 17:57:08  hajny
    + added support for Win32 (untested)

  Revision 1.2  1999/06/10 07:28:29  hajny
    * compilable with TP again

  Revision 1.1  1999/02/19 16:45:26  peter
    * moved to fpinst/ directory
    + makefile

}
