{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 2000-2003 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
    History:

    First version.
    15 Oct 2000.

    Updated for fpc 1.0.7
    16 Jan 2003.

    Changed integer > smallint.
    Changed startcode for unit.
    12 Feb 2003.

    nils.sjoholm@mailbox.swipnet.se

}

UNIT ptreplay;

INTERFACE

USES Exec;

 const
     PTREPLAYNAME : PChar = 'ptreplay.library';
  { The rest is private for now, but more details may be released later.  }

  type
     PModule = ^TModule;
     TModule = record
          mod_Name : PChar;
     { The rest is private for now, but more details may be released later.  }
     end;

     { This structure is returned by GetSample function  }
     PPTSample = ^TPTSample;
     TPTSample = record
          Name     : array[0..21] of Char; { Null terminated string with samplename  }
          Length   :  WORD;                { Sample length in words  }
          FineTune : BYTE;                 { FineTune of sample in lower 4 bits  }
          Volume   : BYTE;                 { Volume of sample  }
          Repeat_  : WORD;                 { Repeat start in number of words  }
          Replen   : WORD;                 { Repeat length in number of words  }
       end;

VAR PTReplayBase : pLibrary;

FUNCTION PTLoadModule(name : pCHAR location 'a0') : pModule; syscall PTReplayBase 030;
PROCEDURE PTUnloadModule(module : pModule location 'a0'); syscall PTReplayBase 036;
FUNCTION PTPlay(module : pModule location 'a0') : ULONG; syscall PTReplayBase 042;
FUNCTION PTStop(module : pModule location 'a0') : ULONG; syscall PTReplayBase 048;
FUNCTION PTPause(module : pModule location 'a0') : ULONG; syscall PTReplayBase 054;
FUNCTION PTResume(module : pModule location 'a0') : ULONG; syscall PTReplayBase 060;
PROCEDURE PTFade(module : pModule location 'a0'; speed : BYTE location 'd0'); syscall PTReplayBase 066;
PROCEDURE PTSetVolume(module : pModule location 'a0'; vol : BYTE location 'd0'); syscall PTReplayBase 072;
FUNCTION PTSongPos(module : pModule location 'a0') : BYTE; syscall PTReplayBase 078;
FUNCTION PTSongLen(module : pModule location 'a0') : BYTE; syscall PTReplayBase 084;
FUNCTION PTSongPattern(module : pModule location 'a0'; Pos : WORD location 'd0') : BYTE; syscall PTReplayBase 090;
FUNCTION PTPatternPos(Module : pModule location 'a0') : BYTE; syscall PTReplayBase 096;
FUNCTION PTPatternData(Module : pModule location 'a0'; Pattern : BYTE location 'd0'; Row : BYTE location 'd1') : POINTER; syscall PTReplayBase 102;
PROCEDURE PTInstallBits(Module : pModule location 'a0'; Restart : SHORTINT location 'd0'; NextPattern : SHORTINT location 'd1'; NextRow : SHORTINT location 'd2'; Fade : SHORTINT location 'd3'); syscall PTReplayBase 108;
FUNCTION PTSetupMod(ModuleFile : POINTER location 'a0') : pModule; syscall PTReplayBase 114;
PROCEDURE PTFreeMod(Module : pModule location 'a0'); syscall PTReplayBase 120;
PROCEDURE PTStartFade(Module : pModule location 'a0'; speed : BYTE location 'd0'); syscall PTReplayBase 126;
PROCEDURE PTOnChannel(Module : pModule location 'a0'; Channels : SHORTINT location 'd0'); syscall PTReplayBase 132;
PROCEDURE PTOffChannel(Module : pModule location 'a0'; Channels : SHORTINT location 'd0'); syscall PTReplayBase 138;
PROCEDURE PTSetPos(Module : pModule location 'a0'; Pos : BYTE location 'd0'); syscall PTReplayBase 144;
PROCEDURE PTSetPri(Pri : SHORTINT location 'd0'); syscall PTReplayBase 150;
FUNCTION PTGetPri : SHORTINT; syscall PTReplayBase 156;
FUNCTION PTGetChan : SHORTINT; syscall PTReplayBase 162;
FUNCTION PTGetSample(Module : pModule location 'a0'; Nr : smallint location 'd0') : pPTSample; syscall PTReplayBase 168;

FUNCTION PTLoadModule(const name : String) : pModule;

{You can remove this include and use a define instead}
{$I useautoopenlib.inc}
{$ifdef use_init_openlib}
procedure InitPTREPLAYLibrary;
{$endif use_init_openlib}

{This is a variable that knows how the unit is compiled}
var
    PTREPLAYIsCompiledHow : longint;

IMPLEMENTATION

{$ifndef dont_use_openlib}
uses
  amsgbox;
{$endif dont_use_openlib}


FUNCTION PTLoadModule(const name : string) : pModule;
var
  s: RawByteString;
begin
  s:=name;
  PTLoadModule := PTLoadModule(PChar(s));
end;

const
    { Change VERSION and LIBVERSION to proper values }

    VERSION : string[2] = '0';
    LIBVERSION : longword = 0;

{$ifdef use_init_openlib}
  {$Info Compiling initopening of ptreplay.library}
  {$Info don't forget to use InitPTREPLAYLibrary in the beginning of your program}

var
    ptreplay_exit : Pointer;

procedure CloseptreplayLibrary;
begin
    ExitProc := ptreplay_exit;
    if PTReplayBase <> nil then begin
        CloseLibrary(PTReplayBase);
        PTReplayBase := nil;
    end;
end;

procedure InitPTREPLAYLibrary;
begin
    PTReplayBase := nil;
    PTReplayBase := OpenLibrary(PTREPLAYNAME,LIBVERSION);
    if PTReplayBase <> nil then begin
        ptreplay_exit := ExitProc;
        ExitProc := @CloseptreplayLibrary;
    end else begin
        MessageBox('FPC Pascal Error',
        'Can''t open ptreplay.library version ' + VERSION + #10 +
        'Deallocating resources and closing down',
        'Oops');
        halt(20);
    end;
end;

begin
    PTREPLAYIsCompiledHow := 2;
{$endif use_init_openlib}

{$ifdef use_auto_openlib}
  {$Info Compiling autoopening of ptreplay.library}

var
    ptreplay_exit : Pointer;

procedure CloseptreplayLibrary;
begin
    ExitProc := ptreplay_exit;
    if PTReplayBase <> nil then begin
        CloseLibrary(PTReplayBase);
        PTReplayBase := nil;
    end;
end;

begin
    PTReplayBase := nil;
    PTReplayBase := OpenLibrary(PTREPLAYNAME,LIBVERSION);
    if PTReplayBase <> nil then begin
        ptreplay_exit := ExitProc;
        ExitProc := @CloseptreplayLibrary;
        PTREPLAYIsCompiledHow := 1;
    end else begin
        MessageBox('FPC Pascal Error',
        'Can''t open ptreplay.library version ' + VERSION + #10 +
        'Deallocating resources and closing down',
        'Oops');
        halt(20);
    end;

{$endif use_auto_openlib}

{$ifdef dont_use_openlib}
begin
    PTREPLAYIsCompiledHow := 3;
   {$Warning No autoopening of ptreplay.library compiled}
   {$Warning Make sure you open ptreplay.library yourself}
{$endif dont_use_openlib}


END. (* UNIT PTREPLAY *)
