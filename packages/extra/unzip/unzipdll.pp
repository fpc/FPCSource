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
 UnzipErr: longint = 0;

type
 TArgV = array [0..1024] of PChar;
 PArgV = ^TArgV;
 TCharArray = array [1..1024*1024] of char;
 PCharArray = ^TCharArray;
 TFileUnzipEx = function (SourceZipFile, TargetDirectory,
                                                    FileSpecs: PChar): integer;

function DllFileUnzipEx (SourceZipFile, TargetDirectory,
                                                    FileSpecs: PChar): integer;

const
 FileUnzipEx: TFileUnzipEx = @DllFileUnzipEx;

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
 Unzip, Dos;

type
 UzpMainFunc = function (ArgC: longint; var ArgV: TArgV): longint; cdecl;

const
{$IFDEF OS2}
 AllFiles: string [1] = '*';
{$ELSE}
 {$IFDEF WIN32}
 AllFiles: string [3] = '*.*';
 {$ENDIF}
{$ENDIF}
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
 C: char;

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
 {$IFDEF FPC}
 end else DLLInit := DosQueryProcAddr (DLLHandle, UzpMainOrd, nil, pointer (UzpMain)) = 0;
 {$ELSE}
 end else DLLInit := DosQueryProcAddr (DLLHandle, UzpMainOrd, nil, @UzpMain) = 0;
 {$ENDIF}
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

function DllFileUnzipEx (SourceZipFile, TargetDirectory,
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
 if UnzipErr <> 0 then DllFileUnzipEx := 0 else DllFileUnzipEx := FCount;
 for I := 1 to FCount do FreeMem (ArgV [I + OptCount], StrLen [I + OptCount]);
end;

begin
 if DLLInit then
 begin
  OldExit := ExitProc;
  ExitProc := @NewExit;
 end else
 begin
  WriteLn (#13#10'Dynamic library UNZIP32.DLL from InfoZip is needed to unpack archives.');
  WriteLn ('This library could not be found on your system, however.');
  WriteLn ('Please, download the library, either from the location where you found');
  WriteLn ('this package, or from any FTP archive carrying InfoZip programs.');
{$IFDEF OS2}
  WriteLn ('If you already have this DLL, please, check your configuration (' + LIBPATH + ').');
{$ELSE}
  WriteLn ('If you already have this DLL, please, check your configuration (' + PATH + ').');
{$ENDIF}
  WriteLn (#13#10'If you want to try unpacking the files with internal unpacking routine,');
  WriteLn ('answer the following question with Y. However, this might not work correctly');
  WriteLn ('under some conditions (e.g. for long names and drives not supporting them).');
  Write (#13#10'Do you want to continue now (y/N)? ');
  ReadLn (C);
  if UpCase (C) = 'Y' then FileUnzipEx := TFileUnzipEx (@Unzip.FileUnzipEx) else Halt (255);
 end;
end.
{
  $Log$
  Revision 1.2  2002-07-07 08:22:17  hajny
    * warning message modified to be more general

  Revision 1.1  2002/01/29 17:55:23  peter
    * splitted to base and extra

  Revision 1.1  2001/01/30 19:26:18  peter
    * renamed zip to unzip

  Revision 1.2  2000/12/19 00:51:10  hajny
    * modifications from /install/fpinst merged in

  Revision 1.1  2000/07/13 06:34:24  michael
  + Initial import

  Revision 1.1  2000/03/02 12:16:57  michael
  + Initial implementation

  Revision 1.2  1999/06/10 07:28:29  hajny
    * compilable with TP again

  Revision 1.1  1999/02/19 16:45:26  peter
    * moved to fpinst/ directory
    + makefile

}
