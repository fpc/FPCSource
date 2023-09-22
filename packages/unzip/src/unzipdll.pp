{
}
{$IFNDEF FPC_DOTTEDUNITS}
unit UnzipDLL;
{$ENDIF FPC_DOTTEDUNITS}

{$IFDEF VIRTUALPASCAL}
{$Cdecl+,AlignRec-,OrgName+}
{$ELSE}
 {$IFDEF FPC}
  {$PACKRECORDS 1}
 {$ENDIF}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
 System.Ziptypes;
{$ELSE FPC_DOTTEDUNITS}
uses
 ZipTypes;
{$ENDIF FPC_DOTTEDUNITS}

const
 UnzipErr: longint = 0;

type
 TArgV = array [0..1023] of PAnsiChar;
 PArgV = ^TArgV;
 TCharArray = array [1..1024*1024] of AnsiChar;
 PCharArray = ^TCharArray;
 TFileUnzipEx = function (SourceZipFile, TargetDirectory,
                                                    FileSpecs: PAnsiChar): integer;

function DllFileUnzipEx (SourceZipFile, TargetDirectory,
                                                    FileSpecs: PAnsiChar): integer;

const
 FileUnzipEx: TFileUnzipEx = @DllFileUnzipEx;

(* Returns non-zero result on success. *)

implementation

{$IFDEF FPC_DOTTEDUNITS}
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
 System.Unzip51g, TP.DOS;
{$ELSE FPC_DOTTEDUNITS}
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
 Unzip51g, Dos;
{$ENDIF FPC_DOTTEDUNITS}

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
 QuietOpt: array [1..4] of AnsiChar = '-qq'#0;
 OverOpt: array [1..3] of AnsiChar = '-o'#0;
 CaseInsOpt: array [1..3] of AnsiChar = '-C'#0;
 ExDirOpt: array [1..3] of AnsiChar = '-d'#0;
 OptCount = 4;

var
 DLLHandle: longint;
 OldExit: pointer;
 C: AnsiChar;

function DLLInit: boolean;
var
{$IFDEF OS2}
 ErrPath: array [0..259] of AnsiChar;
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
   WriteLn (PAnsiChar (@ErrPath));
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
                                                    FileSpecs: PAnsiChar): integer;
var
 I, FCount, ArgC: longint;
 ArgV: TArgV;
 P: PAnsiChar;
 StrLen: array [Succ (OptCount)..1023] of longint;
begin
 ArgV [0] := @DLLName;
 ArgV [1] := @QuietOpt;
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
{$IFDEF EMX}
 if os_Mode <> osOS2 then
  FileUnzipEx := TFileUnzipEx (@Unzip51g.FileUnzipEx)
 else
{$ENDIF EMX}
 if DLLInit then
 begin
  OldExit := ExitProc;
  ExitProc := @NewExit;
  if GetEnv ('TZ') = '' then
  begin
   WriteLn (#13#10'TZ variable was not found in your environment.');
   WriteLn ('This variable is necessary for setting correct date/time of unpacked files.');
   WriteLn ('Please, add it to your environment and restart this program afterwards.');
   Halt (1);
  end;
 end else
 begin
  WriteLn (#13#10'Dynamic library UNZIP32.DLL from InfoZip is needed to unpack archives.');
  WriteLn ('This library could not be found on your system, however.');
  WriteLn ('Please, download the library, either from the location where you found');
  WriteLn ('this package, or from any FTP archive carrying InfoZip programs.');
  WriteLn ('If you already have this DLL, please, check your configuration (' + LibPath + ').');
  WriteLn (#13#10'If you want to try unpacking the files with internal unpacking routine,');
  WriteLn ('answer the following question with Y. However, this might not work correctly');
  WriteLn ('under some conditions (e.g. for long names and drives not supporting them).');
  Write (#13#10'Do you want to continue now (y/N)? ');
  ReadLn (C);
  if UpCase (C) = 'Y' then FileUnzipEx := TFileUnzipEx (@Unzip51g.FileUnzipEx) else Halt (255);
 end;
end.
