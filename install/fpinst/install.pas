{
    $Id$
    This file is part of Free Pascal
    Copyright (c) 1993-2000 by Florian Klaempfl
    member of the Free Pascal development team

    This is the install program for Free Pascal

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program install;

{$DEFINE FV}         (* TH - added to make use of the original Turbo Vision possible. *)
{ $DEFINE DLL}       (* TH - if defined, UNZIP32.DLL library is used to unpack. *)
{ $DEFINE DOSSTUB}   (* TH - should _not_ be defined unless creating a bound DOS and OS/2 installer!!! *)
(* Defining DOSSTUB causes adding a small piece of code    *)
(* for starting the OS/2 part from the DOS part of a bound *)
(* application if running in OS/2 VDM (DOS) window. Used   *)
(* only if compiling with TP/BP (see conditionals below).  *)

{$IFDEF VER60}
 {$DEFINE TP}
{$ENDIF}

{$IFDEF VER70}
 {$DEFINE TP}
{$ENDIF}

{$IFNDEF TP}
 {$UNDEF DOSSTUB}
{$ELSE}
 {$IFDEF OS2}
  {$UNDEF DOSSTUB}
 {$ENDIF}
{$ENDIF}

{$IFDEF DPMI}
 {$UNDEF DOSSTUB}
{$ENDIF}

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
{$ENDIF OS2}
{$IFDEF GO32V2}
     emu387,
{$ENDIF}
{$ifdef HEAPTRC}
     heaptrc,
{$endif HEAPTRC}
     strings,dos,objects,drivers,
{$IFDEF FV}
     commands,
{$ENDIF}
{$IFDEF DLL}
     unzipdll,
{$ENDIF}
     unzip,ziptypes,
     app,dialogs,views,menus,msgbox,colortxt,tabs,inststr,scroll,
     HelpCtx,WHTMLScn;


  const
     installerversion='1.0.2';


     {$ifdef TP}lfnsupport=false;{$endif}

     maxpacks=10;
     maxpackages=20;
     maxdefcfgs=1024;

     HTMLIndexExt = '.htx';
     CfgExt = '.dat';

     MaxStatusPos = 4;
     StatusChars: string [MaxStatusPos] = '/-\|';
     StatusPos: byte = 1;
     { this variable is set to true if an ide is installed }
     haside : boolean = false;
     hashtmlhelp : boolean = false;

{$IFDEF LINUX}
     DirSep='/';
{$ELSE}
 {$IFDEF UNIX}
     DirSep='/';
 {$ELSE}
     DirSep='\';
 {$ENDIF}
{$ENDIF}

{$IFNDEF GO32V2}
 {$IFDEF GO32V1}
     LFNSupport = false;
 {$ELSE}
  {$IFDEF TP}
     LFNSupport = false;
  {$ELSE}
     LFNSupport = true;
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

  type
     tpackage=record
       name  : string[60];
       zip   : string[12];
     end;

     tpack=record
       name     : string[12];
       binsub   : string[40];
       ppc386   : string[20];
       defidecfgfile,
       defideinifile,
       defcfgfile : string[12];
       include  : boolean;
       filechk  : string[40];
       packages : longint;
       package  : array[1..maxpackages] of tpackage;
     end;

     tcfgarray = array[1..maxdefcfgs] of pstring;

     cfgrec=record
       title    : string[80];
       version  : string[20];
       language : string[30];
       helpidx,
       docsub,
       basepath : DirStr;
       packs    : word;
       pack     : array[1..maxpacks] of tpack;
       defideinis,
       defidecfgs,
       defcfgs  : longint;
       defideini,
       defidecfg,
       defcfg   : tcfgarray;
     end;

     datarec=packed record
       basepath : DirStr;
       cfgval   : word;
       packmask : array[1..maxpacks] of word;
     end;

     punzipdialog=^tunzipdialog;
     tunzipdialog=object(tdialog)
        filetext : pstatictext;
        extractfiletext : pstatictext;
        constructor Init(var Bounds: TRect; ATitle: TTitleStr);
        procedure do_unzip(s,topath:string);
     end;

     penddialog = ^tenddialog;
     tenddialog = object(tdialog)
        constructor init;
     end;

     pinstalldialog = ^tinstalldialog;
     tinstalldialog = object(tdialog)
        constructor init;
     end;

     planguagedialog = ^tlanguagedialog;
     tlanguagedialog = object(tdialog)
        constructor init;
     end;

     PFPHTMLFileLinkScanner = ^TFPHTMLFileLinkScanner;
     TFPHTMLFileLinkScanner = object(THTMLFileLinkScanner)
        function    CheckURL(const URL: string): boolean; virtual;
        function    CheckText(const Text: string): boolean; virtual;
        procedure   ProcessDoc(Doc: PHTMLLinkScanFile); virtual;
     end;

     phtmlindexdialog = ^thtmlindexdialog;
     thtmlindexdialog = object(tdialog)
       text : pstatictext;
       constructor init(var Bounds: TRect; ATitle: TTitleStr);
     end;

     tapp = object(tapplication)
         procedure initmenubar;virtual;
         procedure handleevent(var event : tevent);virtual;
         procedure do_installdialog;
         procedure do_languagedialog;
         procedure readcfg(const fn:string);
         procedure checkavailpack;
     end;

{$IFDEF DOSSTUB}
  PByte = ^byte;
  PRunBlock = ^TRunBlock;
  TRunBlock = record
    Length: word;
    Dependent: word;
    Background: word;
    TraceLevel: word;
    PrgTitle: PChar;
    PrgName: PChar;
    Args: PChar;
    TermQ: longint;
    Environment: pointer;
    Inheritance: word;
    SesType: word;
    Icon: pointer;
    PgmHandle: longint;
    PgmControl: word;
    Column: word;
    Row: word;
    Width: word;
    Height: word;
  end;
{$ENDIF}

  var
     installapp  : tapp;
     startpath   : string;
     successfull : boolean;
     cfg         : cfgrec;
     data        : datarec;
     CfgName: NameStr;
     DStr: DirStr;
     EStr: ExtStr;
     UnzDlg      : punzipdialog;
     log         : text;
     createlog   : boolean;
     msgfile     : string;
{$IFNDEF DLL}

  const
     UnzipErr: longint = 0;
{$ENDIF}


{*****************************************************************************
                                  Helpers
*****************************************************************************}

  procedure errorhalt;
    begin
      installapp.done;
      halt(1);
    end;


  function packagemask(i:longint):longint;
    begin
      packagemask:=1 shl (i-1);
    end;


  function upper(const s : string):string;
    var
       i : integer;
    begin
       for i:=1 to length(s) do
         if s[i] in ['a'..'z'] then
          upper[i]:=chr(ord(s[i])-32)
         else
          upper[i]:=s[i];
       upper[0]:=s[0];
    end;


  function Replace(var s:string;const s1,s2:string) : boolean;
    var
       i  : longint;
    begin
      Replace:=false;
      repeat
        i:=pos(s1,s);
        if i>0 then
         begin
           Delete(s,i,length(s1));
           Insert(s2,s,i);
           Replace:=true;
         end;
      until i=0;
    end;


  function file_exists(const f : string;const path : string) : boolean;
    begin
       file_exists:=fsearch(f,path)<>'';
    end;


  function createdir(s:string):boolean;
    var
      s1,start : string;
      err : boolean;
      i : longint;
    begin
       err:=false;
       {$I-}
       getdir(0,start);
{$ifndef linux}
       if (s[2]=':') and (s[3]=DirSep) then
        begin
          chdir(Copy(s,1,3));
          Delete(S,1,3);
        end;
{$endif}
       repeat
         i:=Pos(DirSep,s);
         if i=0 then
          i:=255;
         s1:=Copy(s,1,i-1);
         Delete(s,1,i);
         ChDir(s1);
         if ioresult<>0 then
          begin
            mkdir(s1);
            chdir(s1);
            if ioresult<>0 then
             begin
               err:=true;
               break;
             end;
          end;
       until s='';
       chdir(start);
       {$I+}
       createdir:=err;
    end;


  function DiskSpaceN(const zipfile : string) : longint;
    var
      compressed,uncompressed : longint;
      s : string;
    begin
      s:=zipfile+#0;
      if not (IsZip (@S [1])) then DiskSpaceN := -1 else
      begin
       Uncompressed:=UnzipSize(@s[1],compressed);
       DiskSpaceN:=uncompressed shr 10;
      end;
    end;


  function diskspace(const zipfile : string) : string;
    var
      uncompressed : longint;
      s : string;
    begin
      uncompressed:=DiskSpaceN (zipfile);
      if Uncompressed = -1 then DiskSpace := str_invalid else
      begin
       str(uncompressed,s);
       diskspace:=' ('+s+' KB)';
      end;
    end;


  function createinstalldir(s : string) : boolean;
    var
      err : boolean;
      dir : searchrec;
      params : array[0..0] of pointer;
    begin
       if s[length(s)]=DirSep then
        dec(s[0]);
       FindFirst(s,AnyFile,dir);
       if doserror=0 then
         begin
            if Dir.Attr and Directory = 0 then
              begin
                messagebox(msg_problems_create_dir,nil,
                mferror+mfokbutton);
                createinstalldir:=false;
              end else
                createinstalldir:=messagebox(msg_install_dir_exists,nil,
                mferror+mfyesbutton+mfnobutton)=cmYes;
            exit;
         end;
       err:=Createdir(s);
       if err then
         begin
            params[0]:=@s;
            messagebox(msg_install_cant_be_created,
              @params,mferror+mfokbutton);
            createinstalldir:=false;
            exit;
         end;
{$ifndef TP}
 {$IFNDEF OS2}
       FindClose (dir);
 {$ENDIF}
{$endif}
       createinstalldir:=true;
    end;


  function GetProgDir: DirStr;
    var
      D: DirStr;
      N: NameStr;
      E: ExtStr;
    begin
       FSplit (FExpand (ParamStr (0)), D, N, E);
       if (D [0] <> #0) and (D [byte (D [0])] = '\') then Dec (D [0]);
       GetProgDir := D;
    end;

  function RTrim(const S: string): string;
  var
    i : longint;
  begin
    i:=length(s);
    while (i>0) and (s[i]=' ') do
     dec(i);
    RTrim:=Copy(s,1,i);
  end;

  function LTrim(const S: string): string;
  var
    i : longint;
  begin
    i:=1;
    while (i<length(s)) and (s[i]=' ') do
     inc(i);
    LTrim:=Copy(s,i,255);
  end;

  function Trim(const S: string): string;
  begin
    Trim:=RTrim(LTrim(S));
  end;

  function CompareText(S1, S2: string): integer;
  var R: integer;
  begin
    S1:=Upcase(S1);
    S2:=Upcase(S2);
    if S1<S2 then R:=-1 else
    if S1>S2 then R:= 1 else
    R:=0;
    CompareText:=R;
  end;

  function ExtOf(const S: string): string;
  var D: DirStr; E: ExtStr; N: NameStr;
  begin
    FSplit(S,D,N,E);
    ExtOf:=E;
  end;

  function DirAndNameOf(const S: string): string;
  var D: DirStr; E: ExtStr; N: NameStr;
  begin
    FSplit(S,D,N,E);
    DirAndNameOf:=D+N;
  end;

{*****************************************************************************
                          HTML-Index Generation
*****************************************************************************}
  var
     indexdlg : phtmlindexdialog;

  constructor thtmlindexdialog.Init(var Bounds: TRect; ATitle: TTitleStr);
    var
      r : trect;
    begin
      inherited init(bounds,atitle);
      R.Assign (4, 2,bounds.B.X-Bounds.A.X-2, 4);
      text:=new(pstatictext,init(r,'Please wait ...'));
      insert(text);
    end;

  procedure TFPHTMLFileLinkScanner.ProcessDoc(Doc: PHTMLLinkScanFile);

    var
       oldtext : pstring;
    begin
       oldtext:=indexdlg^.text^.text;
       indexdlg^.text^.text:=newstr('Processing '+Doc^.GetDocumentURL);
       indexdlg^.text^.drawview;
       inherited ProcessDoc(Doc);
       disposestr(indexdlg^.text^.text);
       indexdlg^.text^.text:=oldtext;
       indexdlg^.text^.drawview;
    end;

  function TFPHTMLFileLinkScanner.CheckURL(const URL: string): boolean;
  var OK: boolean;
  const HTTPPrefix = 'http:';
        FTPPrefix  = 'ftp:';
  begin
    OK:=inherited CheckURL(URL);
    if OK then OK:=DirAndNameOf(URL)<>'';
    if OK then OK:=CompareText(copy(ExtOf(URL),1,4),'.HTM')=0;
    if OK then OK:=CompareText(copy(URL,1,length(HTTPPrefix)),HTTPPrefix)<>0;
    if OK then OK:=CompareText(copy(URL,1,length(FTPPrefix)),FTPPrefix)<>0;
    CheckURL:=OK;
  end;

  function TFPHTMLFileLinkScanner.CheckText(const Text: string): boolean;
  var OK: boolean;
      S: string;
  begin
    S:=Trim(Text);
    OK:=(S<>'') and (copy(S,1,1)<>'[');
    CheckText:=OK;
  end;

  procedure writehlpindex(filename : string);

    var
       LS : PFPHTMLFileLinkScanner;
       BS : PBufStream;
       S : String;
       Re : Word;
       params : array[0..0] of pointer;
       dir    : searchrec;
       r : trect;

    begin
       S:='HTML Index';
       r.assign(10,10,70,15);
       indexdlg:=new(phtmlindexdialog,init(r,'Creating HTML index file, please wait ...'));
       desktop^.insert(indexdlg);
       New(LS, Init);
       LS^.ProcessDocument(FileName,[soSubDocsOnly]);
       if LS^.GetDocumentCount=0 then
         begin
           params[0]:=@filename;
           MessageBox('Problem creating help index %1, aborting',@params,
                  mferror+mfokbutton);
         end
       else
         begin
           FileName:=DirAndNameOf(FileName)+HTMLIndexExt;
           findfirst(filename,AnyFile,dir);
           if doserror=0 then
             begin
                params[0]:=@filename;
                Re:=MessageBox('Help index %s already exists, overwrite it?',@params,
                  mfinformation+mfyesbutton+mfnobutton);
             end;
           if Re<>cmNo then
           begin
             New(BS, Init(FileName, stCreate, 4096));
             if Assigned(BS)=false then
               begin
                  MessageBox('Error while writing help index! '+
                    'No help index is created',@params,
                    mferror+mfokbutton);
                  Re:=cmCancel;
               end
             else
               begin
                 LS^.StoreDocuments(BS^);
                 if BS^.Status<>stOK then
                   begin
                      MessageBox('Error while writing help index!'#13+
                        'No help index is created',@params,
                        mferror+mfokbutton);
                      Re:=cmCancel;
                   end;
                 Dispose(BS, Done);
               end;
           end;
         end;
       Dispose(LS, Done);
       desktop^.delete(indexdlg);
       dispose(indexdlg,done);
    end;

{*****************************************************************************
                          Writing of fpc.cfg
*****************************************************************************}

  procedure writedefcfg(const fn:string;const cfgdata : tcfgarray;count : longint);
    var
      t      : text;
      i      : longint;
      s      : string;
      dir    : searchrec;
      params : array[0..0] of pointer;
      d : dirstr;
      n : namestr;
      e : extstr;
    begin
    { already exists }
      findfirst(fn,AnyFile,dir);
      if doserror=0 then
       begin
         params[0]:=@fn;
         if MessageBox(msg_overwrite_cfg,@params,
                       mfinformation+mfyesbutton+mfnobutton)=cmNo then
           exit;
       end;
    { create directory }
      fsplit(fn,d,n,e);
      createdir(d);
    { create the fpc.cfg }
      assign(t,fn);
      {$I-}
       rewrite(t);
      {$I+}
      if ioresult<>0 then
       begin
         params[0]:=@fn;
         MessageBox(msg_problems_writing_cfg,@params,mfinformation+mfokbutton);
         exit;
       end;
      for i:=1 to count do
       if assigned(cfgdata[i]) then
         begin
           s:=cfgdata[i]^;
           Replace(s,'$1',data.basepath);

           { error msg file entry? }
           if Replace(s,'$L',msgfile) then
             begin
                { if we've to set an error msg file, we }
                { write it else we discard the line     }
                if msgfile<>'' then
                  writeln(t,s);
             end
           else
             writeln(t,s);
         end
       else
         writeln(t,'');

      close(t);
    end;


{*****************************************************************************
                               TUnZipDialog
*****************************************************************************}

  constructor tunzipdialog.Init(var Bounds: TRect; ATitle: TTitleStr);
    var
      r : trect;
    begin
      inherited init(bounds,atitle);
(*      R.Assign (11, 4, 38, 6);*)
      R.Assign (1, 4,bounds.B.X-Bounds.A.X-2, 6);
      filetext:=new(pstatictext,init(r,#3'File: '));
      insert(filetext);
      R.Assign (1, 7,bounds.B.X-Bounds.A.X-2, 9);
      extractfiletext:=new(pstatictext,init(r,#3' '));
      insert(extractfiletext);
    end;

{$IFNDEF DLL}
  procedure UnzipCheckFn (Retcode: longint; Rec: pReportRec );{$ifdef Delphi32}STDCALL;{$endif}
  {$IFNDEF BIT32} FAR;{$ENDIF BIT32}
  var
    name : string;
  begin
    case Rec^.Status of
     unzip_starting:
         UnzipErr := 0;
     file_starting:
        begin
         with UnzDlg^.extractfiletext^ do
         begin
          Disposestr(text);
          name:=Strpas(Rec^.FileName);
          Text:=NewStr(#3+name);
          DrawView;
         end;
        end;
     file_failure: UnzipErr := RetCode;
     file_unzipping:
        begin
         with UnzDlg^.FileText^ do
         begin
          Inc (StatusPos);
          if StatusPos > MaxStatusPos then StatusPos := 1;
          Text^ [Length (Text^)] := StatusChars [StatusPos];
          DrawView;
         end;
        end;
    end;
  end;
{$ENDIF}

  procedure tunzipdialog.do_unzip(s,topath : string);
    var
      again : boolean;
      fn,dir,wild : string;
      Cnt: integer;
      params : array[0..0] of pointer;

    begin
       Disposestr(filetext^.text);
       filetext^.Text:=NewStr(#3+str_file+s+ #13#3' ');
       filetext^.drawview;
       if not(file_exists(s,startpath)) then
         begin
            params[0]:=@s;
            messagebox(msg_file_missing,@params,mferror+mfokbutton);
            errorhalt;
         end;
{$IFNDEF DLL}
 {$IFDEF FPC}
       SetUnzipReportProc (@UnzipCheckFn);
 {$ELSE FPC}
       SetUnzipReportProc (UnzipCheckFn);
 {$ENDIF FPC}
{$ENDIF DLL}
       repeat
         fn:=startpath+DirSep+s+#0;
         dir:=topath+#0;
         wild:=AllFiles + #0;
         again:=false;
         FileUnzipEx(@fn[1],@dir[1],@wild[1]);
         if (UnzipErr <> 0) then
           begin
              Str(UnzipErr,s);
              params[0]:=@s;
              if messagebox(msg_extraction_error,@params,mferror+mfyesbutton+mfnobutton)=cmNo then
               errorhalt
              else
               again:=true;
           end;
       until not again;
    end;


{*****************************************************************************
                               TEndDialog
*****************************************************************************}

  constructor tenddialog.init;
    var
      R       : TRect;
      P       : PStaticText;
      Control : PButton;
      YB: word;
{$IFNDEF LINUX}
      i : longint;
      S: string;
      WPath: boolean;
{$ENDIF}
{$IFDEF OS2}
      ErrPath: array [0..259] of char;
      Handle: longint;
      WLibPath: boolean;
    const
      EMXName: array [1..4] of char = 'EMX'#0;
{$ENDIF}
    begin
      if haside then
        YB := 15
      else
        YB := 14;

{$IFNDEF LINUX}
      s:='';
      for i:=1 to cfg.packs do
       if cfg.pack[i].binsub<>'' then
        begin
          if s<>'' then
           s:=s+';';
          S := s+Data.BasePath + Cfg.pack[i].BinSub;
        end;
      if Pos (Upper (S), Upper (GetEnv ('PATH'))) = 0 then
       begin
         WPath := true;
         Inc (YB, 2);
       end
      else
       WPath := false;
  {$IFDEF OS2}
      if DosLoadModule (@ErrPath, SizeOf (ErrPath), @EMXName, Handle) = 0 then
       begin
         WLibPath := false;
         DosFreeModule (Handle);
       end
      else
       begin
         WLibPath := true;
         Inc (YB, 2);
       end;
  {$ENDIF}
{$ENDIF}

      R.Assign(6, 6, 74, YB);
      inherited init(r,dialog_enddialog_title);

{$IFNDEF LINUX}
      if WPath then
       begin
         R.Assign(2, 3, 64, 5);
         P:=new(pstatictext,init(r,str_extend_path+''''+S+''''));
         insert(P);
       end;

  {$IFDEF OS2}
      if WLibPath then
       begin
         if WPath then
          S := str_libpath+'''' + S + '\'+str_dll+''''
         else
          S := str_extend_libpath+'''' + S + '\'+str_dll+'''';
         R.Assign (2, YB - 14, 64, YB - 12);
         P := New (PStaticText, Init (R, S));
         Insert (P);
       end;
  {$ENDIF}
{$ENDIF}

      R.Assign(2, YB - 13, 64, YB - 12);
      P:=new(pstatictext,init(r,str_to_compile+''''+cfg.pack[1].ppc386+str_file2+''''));
      insert(P);

      if haside then
        begin
           R.Assign(2, YB - 12, 64, YB - 10);
           P:=new(pstatictext,init(r,str_start_ide));
           insert(P);
        end;

      R.Assign (29, YB - 9, 39, YB - 7);
      Control := New (PButton, Init (R,str_ok, cmOK, bfDefault));
      Insert (Control);
    end;


{*****************************************************************************
                               TInstallDialog
*****************************************************************************}

  var
     islfn : boolean;

  procedure lfnreport( Retcode : longint;Rec : pReportRec );
{$IFDEF TP}
                                                             far;
{$ENDIF}

    var
       p : pathstr;
       n : namestr;
       e : extstr;

    begin
       fsplit(strpas(rec^.Filename),p,n,e);
       if length(n)>8 then
         islfn:=true;
    end;

  function haslfn(const zipfile,path : string) : boolean;

    var
       buf : array[0..255] of char;

    begin
       strpcopy(buf,path+DirSep+zipfile);
       islfn:=false;
{$ifdef FPC}
       ViewZip(buf,AllFiles,@lfnreport);
{$else FPC}
       ViewZip(buf,AllFiles,lfnreport);
{$endif FPC}
       haslfn:=islfn;
    end;

  constructor tlanguagedialog.init;
    const
       languages = 8;
       width = 40;
       height = languages+6;
       x1 = (79-width) div 2;
       y1 = (23-height) div 2;
       x2 = x1+width;
       y2 = y1+height;
    var
       r : trect;
       okbut : pbutton;
       line : longint;
       rb : PRadioButtons;

    begin
       r.assign(x1,y1,x2,y2);
       inherited init(r,dialog_language_title);
       GetExtent(R);
       R.Grow(-2,-1);
       line:=r.a.y+1;
       r.assign((width div 2)-15,line,(width div 2)+15,line+languages);
       New(rb, Init(r,
          NewSItem(dialog_language_english,
          NewSItem(dialog_language_dutch,
          NewSItem(dialog_language_french,
          NewSItem(dialog_language_russian,
          NewSItem(dialog_language_hungarian,
          NewSItem(dialog_language_spanish,
          NewSItem(dialog_language_german,
          NewSItem(dialog_language_russian_win,
          nil))))))))));
       insert(rb);
       inc(line,languages);
       inc(line,1);
       r.assign((width div 2)-5,line,(width div 2)+5,line+2);
       new(okbut,init(r,str_ok,cmok,bfdefault));

      Insert(OkBut);
    end;

  constructor tinstalldialog.init;
    const
       width = 76;
       height = 20;
       x1 = (79-width) div 2;
       y1 = (23-height) div 2;
       x2 = x1+width;
       y2 = y1+height;
    var
       tabr,tabir,r : trect;
       packmask : array[1..maxpacks] of longint;
       i,line,j : integer;
       items : array[1..maxpacks] of psitem;
       f : pview;
       found : boolean;
       okbut,cancelbut : pbutton;
       firstitem : array[1..maxpacks] of integer;
       packcbs : array[1..maxpacks] of pcheckboxes;
       packtd : ptabdef;
       labpath : plabel;
       ilpath : pinputline;
       tab : ptab;
       titletext : pcoloredtext;
       labcfg : plabel;
       cfgcb : pcheckboxes;
       scrollbox: pscrollbox;
       sbr,sbsbr: trect;
       sbsb: pscrollbar;
    begin
       f:=nil;
     { walk packages reverse and insert a newsitem for each, and set the mask }
       for j:=1 to cfg.packs do
        with cfg.pack[j] do
         begin
           firstitem[j]:=0;
           items[j]:=nil;
           packmask[j]:=0;
           for i:=packages downto 1 do
            begin
              if file_exists(package[i].zip,startpath) then
               begin
{$ifdef go32v2}
                 if not(lfnsupport) then
                   begin
                      if not(haslfn(package[i].zip,startpath)) then
                        begin
                           items[j]:=newsitem(package[i].name+diskspace(startpath+DirSep+package[i].zip),items[j]);
                           packmask[j]:=packmask[j] or packagemask(i);
                           firstitem[j]:=i;
                           if createlog then
                             writeln(log,str_checking_lfn,startpath+DirSep+package[i].zip,' ... no lfn');
                        end
                      else
                        begin
                           items[j]:=newsitem(package[i].name+str_requires_lfn,items[j]);
                           if createlog then
                             writeln(log,str_checking_lfn,startpath+DirSep+package[i].zip,' ... uses lfn');
                        end;
                   end
                 else
{$endif go32v2}
                   begin
                      items[j]:=newsitem(package[i].name+diskspace(startpath+DirSep+package[i].zip),items[j]);
                      packmask[j]:=packmask[j] or packagemask(i);
                      firstitem[j]:=i;
                   end;
               end
              else
               items[j]:=newsitem(package[i].name,items[j]);
            end;
         end;

     { If no component found abort }
       found:=false;
       for j:=1 to cfg.packs do
        if packmask[j]<>0 then
         found:=true;
       if not found then
        begin
          messagebox(msg_no_components_found,nil,mferror+mfokbutton);
          errorhalt;
        end;

       r.assign(x1,y1,x2,y2);
       inherited init(r,'');
       GetExtent(R);
       R.Grow(-2,-1);
       Dec(R.B.Y,2);
       TabR.Copy(R);
       TabIR.Copy(R);
       TabIR.Grow(-2,-2);
       TabIR.Move(-2,0);

       {-------- General Sheets ----------}
       R.Copy(TabIR);
       r.move(0,1);
       r.b.x:=r.a.x+40;
       r.b.y:=r.a.y+1;
       new(titletext,init(r,cfg.title,$71));

       r.move(0,2);
       r.b.x:=r.a.x+40;
       new(labpath,init(r,dialog_install_basepath,f));
       r.move(0,1);
       r.b.x:=r.a.x+40;
       r.b.y:=r.a.y+1;
       new(ilpath,init(r,high(DirStr)));

       r.move(0,2);
       r.b.x:=r.a.x+40;
       new(labcfg,init(r,dialog_install_config,f));
       r.move(0,1);
       r.b.x:=r.a.x+40;
       r.b.y:=r.a.y+1;
       new(cfgcb,init(r,newsitem(dialog_install_createppc386cfg,nil)));
       data.cfgval:=1;

       {-------- Pack Sheets ----------}
       for j:=1 to cfg.packs do
        begin
          R.Copy(TabIR);
          if R.A.Y+cfg.pack[j].packages>R.B.Y then
            R.B.Y:=R.A.Y+cfg.pack[j].packages;
          new(packcbs[j],init(r,items[j]));
          if data.packmask[j]=$ffff then
           data.packmask[j]:=packmask[j];
          packcbs[j]^.enablemask:={$ifdef DEV}$7fffffff{$else}packmask[j]{$endif};
          packcbs[j]^.movedto(firstitem[j]);
        end;

       {--------- Main ---------}
       packtd:=nil;
       sbr.assign(1,3,tabr.b.x-tabr.a.x-3,tabr.b.y-tabr.a.y-1);
       for j:=cfg.packs downto 1 do
       begin
         if (sbr.b.y-sbr.a.y)<cfg.pack[j].packages then
          begin
            sbsbr.assign(sbr.b.x,sbr.a.y,sbr.b.x+1,sbr.b.y);
            New(sbsb, init(sbsbr));
          end
         else
           sbsb:=nil;
         New(ScrollBox, Init(sbr, nil, sbsb));
         PackCbs[j]^.MoveTo(0,0);
         ScrollBox^.Insert(PackCbs[j]);

         packtd:=NewTabDef(
           cfg.pack[j].name,ScrollBox,
             NewTabItem(sbsb,
             NewTabItem(ScrollBox,
             nil)),
           packtd);
       end;

       New(Tab, Init(TabR,
         NewTabDef(dialog_install_general,IlPath,
           NewTabItem(TitleText,
           NewTabItem(LabPath,
           NewTabItem(ILPath,
           NewTabItem(LabCfg,
           NewTabItem(CfgCB,
           nil))))),
         packtd)
       ));
       Tab^.GrowMode:=0;

       Insert(Tab);

       line:=tabr.b.y;
       r.assign((width div 2)-18,line,(width div 2)-4,line+2);
       new(okbut,init(r,str_continue,cmok,bfdefault));
       Insert(OkBut);

       r.assign((width div 2)+4,line,(width div 2)+14,line+2);
       new(cancelbut,init(r,str_quit,cmcancel,bfnormal));
       Insert(CancelBut);

       Tab^.Select;
    end;


{*****************************************************************************
                               TUnZipDialog
*****************************************************************************}

  procedure tapp.do_languagedialog;

    var
       p : planguagedialog;
       langdata : longint;
       c : word;

    begin
       { select components }
       new(p,init);
       langdata:=0;
       c:=executedialog(p,@langdata);
       writeln(langdata);
       if c=cmok then
         begin
            case langdata of
               0:
                 cfg.language:='English';
               1:
                 begin
                    cfg.language:='Dutch';
                    msgfile:='errorn.msg';
                 end;
               2:
                 begin
                    cfg.language:='French';
                    msgfile:='errorf.msg';
                 end;
               3:
                 begin
                    cfg.language:='Russian';
                    msgfile:='errorr.msg';
                 end;
               4:
                 cfg.language:='Hungarian';
               5:
                 begin
                    cfg.language:='Spanish';
                    msgfile:='errors.msg';
                 end;
               6:
                 begin
                    cfg.language:='German';
                    msgfile:='errord.msg';
                 end;
               7:
                 begin
                    cfg.language:='RussianWin';
                    msgfile:='errorrw.msg';
                 end;
            end;
         end;
    end;

{*****************************************************************************
                                TApp
*****************************************************************************}

  const
     cmstart = 1000;

  procedure tapp.do_installdialog;
    var
       p    : pinstalldialog;
       p3   : penddialog;
       r    : trect;
       result,
       c    : word;
       i,j  : longint;
       found : boolean;
       params : array[0..0] of pointer;
{$ifndef linux}
       DSize,Space,ASpace : longint;
       S: DirStr;
{$endif}

    procedure doconfigwrite;

      var
         i : longint;

      begin
         for i:=1 to cfg.packs do
           if cfg.pack[i].defcfgfile<>'' then
             writedefcfg(data.basepath+cfg.pack[i].binsub+DirSep+cfg.pack[i].defcfgfile,cfg.defcfg,cfg.defcfgs);
         if haside then
           begin
              for i:=1 to cfg.packs do
                if cfg.pack[i].defidecfgfile<>'' then
                 writedefcfg(data.basepath+cfg.pack[i].binsub+DirSep+cfg.pack[i].defidecfgfile,cfg.defidecfg,cfg.defidecfgs);
              for i:=1 to cfg.packs do
                if cfg.pack[i].defideinifile<>'' then
                 writedefcfg(data.basepath+cfg.pack[i].binsub+DirSep+cfg.pack[i].defideinifile,cfg.defideini,cfg.defideinis);
              if hashtmlhelp then
                writehlpindex(data.basepath+DirSep+cfg.DocSub+DirSep+cfg.helpidx);
           end;
      end;

    begin
      data.basepath:=cfg.basepath;
      data.cfgval:=0;
      for j:=1 to cfg.packs do
       data.packmask[j]:=$ffff;

      repeat
      { select components }
        p:=new(pinstalldialog,init);
        c:=executedialog(p,@data);
        if (c=cmok) then
          begin
            if Data.BasePath = '' then
              messagebox(msg_select_dir,nil,mferror+mfokbutton)
            else
             begin
               found:=false;
               for j:=1 to cfg.packs do
                if data.packmask[j]>0 then
                 found:=true;
               if found then
                begin
{$IFNDEF LINUX}
                { TH - check the available disk space here }
                  DSize := 0;
                  for j:=1 to cfg.packs do
                   with cfg.pack[j] do
                    begin
                      for i:=1 to packages do
                       begin
                         if data.packmask[j] and packagemask(i)<>0 then
                         begin
                          ASpace := DiskSpaceN (package[i].zip);
                          if ASpace = -1 then
                            begin
                                params[0]:=@package[i].zip;
                                MessageBox (msg_corrupt_zip,
                                            @params,mferror + mfokbutton);
                            end
                          else Inc (DSize, ASpace);
                         end;
                       end;
                    end;
                  S := FExpand (Data.BasePath);
                  if S [Length (S)] = DirSep then
                   Dec (S [0]);
                  Space := DiskFree (byte (Upcase(S [1])) - 64) shr 10;

                  if Space < DSize then
                   S := str_is_not
                  else
                   S := '';
                  if (Space < DSize + 500) then
                   begin
                     if S = '' then
                      S := str_might_not_be;
                     params[0]:=@s;
                     if messagebox(msg_space_warning,@params,
                                   mferror+mfyesbutton+mfnobutton) = cmYes then
                      Continue;
                   end;
{$ENDIF}
                  if createinstalldir(data.basepath) then
                   break;
                end
               else
                begin
                  { maybe only config }
                  if (data.cfgval and 1)<>0 then
                   begin
                     result:=messagebox(msg_no_components_selected,nil,
                                                mfinformation+mfyesbutton+mfnobutton);
                     if (result=cmYes) and createinstalldir(data.basepath) then
                       doconfigwrite;
                     exit;
                   end
                  else
                   begin
                     result:=messagebox(msg_nocomponents,nil,
                                               mferror+mfyesbutton+mfnobutton);
                     if result=cmYes then
                      exit;
                   end;
                end;
             end;
          end
        else
          exit;
      until false;

    { extract packages }
      for j:=1 to cfg.packs do
       with cfg.pack[j] do
        begin
          r.assign(10,7,70,18);
          UnzDlg:=new(punzipdialog,init(r,dialog_unzipdialog_title));
          desktop^.insert(UnzDlg);
          for i:=1 to packages do
           begin
             if data.packmask[j] and packagemask(i)<>0 then
               begin
                  UnzDlg^.do_unzip(package[i].zip,data.basepath);
                  { gather some information about the installed files }
                  if copy(package[i].zip,1,3)='ide' then
                    haside:=true;
                  if copy(package[i].zip,1,7)='doc-htm' then
                    hashtmlhelp:=true;
               end;
           end;
          desktop^.delete(UnzDlg);
          dispose(UnzDlg,done);
        end;

    { write config }
      if (data.cfgval and 1)<>0 then
        doconfigwrite;

    { show end message }
      p3:=new(penddialog,init);
      executedialog(p3,nil);
    end;


  procedure tapp.readcfg(const fn:string);
    var
      t    : text;
      i,j,
      line : longint;
      item,
      s    : string;
      params : array[0..0] of pointer;

{$ifndef FPC}
      procedure readln(var t:text;var s:string);
      var
        c : char;
        i : longint;
      begin
        c:=#0;
        i:=0;
        while (not eof(t)) and (c<>#10) do
         begin
           read(t,c);
           if c<>#10 then
            begin
              inc(i);
              s[i]:=c;
            end;
         end;
        if (i>0) and (s[i]=#13) then
         dec(i);
        s[0]:=chr(i);
      end;
{$endif}

    begin
      assign(t,StartPath + DirSep + fn);
      {$I-}
       reset(t);
      {$I+}
      if ioresult<>0 then
       begin
         StartPath := GetProgDir;
         assign(t,StartPath + DirSep + fn);
         {$I-}
          reset(t);
         {$I+}
         if ioresult<>0 then
          begin
            params[0]:=@fn;
            messagebox(msg_file_not_found,@params,mferror+mfokbutton);
            errorhalt;
          end;
       end;
      line:=0;
      while not eof(t) do
       begin
         readln(t,s);
         inc(line);
         if (s<>'') and not(s[1] in ['#',';']) then
          begin
            i:=pos('=',s);
            if i>0 then
             begin
               item:=upper(Copy(s,1,i-1));
               system.delete(s,1,i);
               if item='VERSION' then
                cfg.version:=s
               else
                if item='TITLE' then
                 cfg.title:=s
               else
                if item='LANGUAGE' then
                 cfg.language:=s
               else
                if item='BASEPATH' then
                 cfg.basepath:=s
               else
                if item='HELPIDX' then
                   cfg.helpidx:=s
               else
                if item='DOCSUB' then
                   cfg.docsub:=s
               else
                if item='DEFAULTCFG' then
                 begin
                   repeat
                     readln(t,s);
                     if upper(s)='ENDCFG' then
                      break;
                     if cfg.defcfgs<maxdefcfgs then
                      begin
                        inc(cfg.defcfgs);
                        cfg.defcfg[cfg.defcfgs]:=newstr(s);
                      end;
                   until false;
                 end
                else if item='DEFAULTIDECFG' then
                 begin
                   repeat
                     readln(t,s);
                     if upper(s)='ENDCFG' then
                      break;
                     if cfg.defidecfgs<maxdefcfgs then
                      begin
                        inc(cfg.defidecfgs);
                        cfg.defidecfg[cfg.defidecfgs]:=newstr(s);
                      end;
                   until false;
                 end
                else if item='DEFAULTIDEINI' then
                 begin
                   repeat
                     readln(t,s);
                     if upper(s)='ENDCFG' then
                      break;
                     if cfg.defideinis<maxdefcfgs then
                      begin
                        inc(cfg.defideinis);
                        cfg.defideini[cfg.defideinis]:=newstr(s);
                      end;
                   until false;
                 end
               else
                if item='PACK' then
                 begin
                   inc(cfg.packs);
                   if cfg.packs>maxpacks then
                    begin
                      writeln('Too many packs');
                      halt(1);
                    end;
                   cfg.pack[cfg.packs].name:=s;
                 end
               else
                if item='CFGFILE' then
                 begin
                   if cfg.packs=0 then
                    begin
                      writeln('No pack set');
                      halt(1);
                    end;
                   cfg.pack[cfg.packs].defcfgfile:=s
                 end
               else
                if item='IDECFGFILE' then
                 begin
                   if cfg.packs=0 then
                    begin
                      writeln('No pack set');
                      halt(1);
                    end;
                   cfg.pack[cfg.packs].defidecfgfile:=s
                 end
               else
                if item='IDEINIFILE' then
                 begin
                   if cfg.packs=0 then
                    begin
                      writeln('No pack set');
                      halt(1);
                    end;
                   cfg.pack[cfg.packs].defideinifile:=s
                 end
               else
                if item='PPC386' then
                 begin
                   if cfg.packs=0 then
                    begin
                      writeln('No pack set');
                      halt(1);
                    end;
                   cfg.pack[cfg.packs].ppc386:=s;
                 end
               else
                if item='BINSUB' then
                 begin
                   if cfg.packs=0 then
                    begin
                      writeln('No pack set');
                      halt(1);
                    end;
                   cfg.pack[cfg.packs].binsub:=s;
                 end
               else
                if item='FILECHECK' then
                 begin
                   if cfg.packs=0 then
                    begin
                      writeln('No pack set');
                      halt(1);
                    end;
                   cfg.pack[cfg.packs].filechk:=s;
                 end
               else
                if item='PACKAGE' then
                 begin
                   if cfg.packs=0 then
                    begin
                      writeln('No pack set');
                      halt(1);
                    end;
                   with cfg.pack[cfg.packs] do
                    begin
                      j:=pos(',',s);
                      if (j>0) and (packages<maxpackages) then
                       begin
                         inc(packages);
                         package[packages].zip:=copy(s,1,j-1);
                         package[packages].name:=copy(s,j+1,255);
                       end;
                    end;
                 end
             end;
          end;
       end;
      close(t);
    end;


  procedure tapp.checkavailpack;
    var
      j : longint;
      dir : searchrec;
    begin
    { check the packages }
      j:=0;
      while (j<cfg.packs) do
       begin
         inc(j);
         if cfg.pack[j].filechk<>'' then
          begin
            findfirst(cfg.pack[j].filechk,$20,dir);
            if doserror<>0 then
             begin
               { remove the package }
               move(cfg.pack[j+1],cfg.pack[j],sizeof(tpack)*(cfg.packs-j));
               dec(cfg.packs);
               dec(j);
             end;
{$IFNDEF TP}
            findclose(dir);
{$ENDIF}
          end;
       end;
     end;


  procedure tapp.initmenubar;
    var
       r : trect;
    begin
       getextent(r);
       r.b.y:=r.a.y+1;
       menubar:=new(pmenubar,init(r,newmenu(
          newsubmenu(menu_install,hcnocontext,newmenu(nil
          ),
       nil))));
    end;


  procedure tapp.handleevent(var event : tevent);
    begin
       inherited handleevent(event);
       if event.what=evcommand then
         if event.command=cmstart then
           begin
              clearevent(event);
              do_installdialog;
              if successfull then
               begin
                 event.what:=evcommand;
                 event.command:=cmquit;
                 handleevent(event);
               end;
           end;
    end;

{$IFDEF DOSSTUB}
function CheckOS2: boolean;
var
 OwnName: PathStr;
 OwnDir: DirStr;
 Name: NameStr;
 Ext: ExtStr;
 DosV, W: word;
 P: PChar;
const
 Title: string [15] = 'FPC Installer'#0;
 RunBlock: TRunBlock = (Length: $32;
                        Dependent: 0;
                        Background: 0;
                        TraceLevel: 0;
                        PrgTitle: @Title [1];
                        PrgName: nil;
                        Args: nil;
                        TermQ: 0;
                        Environment: nil;
                        Inheritance: 0;
                        SesType: 2;
                        Icon: nil;
                        PgmHandle: 0;
                        PgmControl: 2;
                        Column: 0;
                        Row: 0;
                        Width: 80;
                        Height: 25);
begin
 CheckOS2 := false;
 asm
  mov ah, 30h
  int 21h
  xchg ah, al
  mov DosV, ax
  mov ax, 4010h
  int 2Fh
  cmp ax, 4010h
  jnz @0
  xor bx, bx
@0:
  mov W, bx
 end;
 if DosV > 3 shl 8 then
 begin
  OwnName := FExpand (ParamStr (0));
  FSplit (OwnName, OwnDir, Name, Ext);
  if (DosV >= 20 shl 8 + 10) and (W >= 20 shl 8 + 10) then
                       (* OS/2 version 2.1 or later running (double-checked) *)
  begin
   OwnName [Succ (byte (OwnName [0]))] := #0;
   RunBlock.PrgName := @OwnName [1];
   P := Ptr (PrefixSeg, $80);
   if PByte (P)^ <> 0 then
   begin
    Inc (P);
    RunBlock.Args := Ptr (PrefixSeg, $81);
   end;
   asm
    mov ax, 6400h
    mov bx, 0025h
    mov cx, 636Ch
    mov si, offset RunBlock
    int 21h
    jc @0
    mov DosV, 0
@0:
   end;
   CheckOS2 := DosV = 0;
  end;
 end;
end;
{$ENDIF}

var
   i : longint;

begin
   { register objects for help streaming }
   RegisterWHTMLScan;
{$ifdef FPC}
{$ifdef win32}
  Dos.Exec(GetEnv('COMSPEC'),'/C echo This dummy call gets the mouse to become visible');
{$endif win32}
{$endif FPC}
(* TH - no error boxes if checking an inaccessible disk etc. *)
{$IFDEF OS2}
 {$IFDEF FPC}
   DosCalls.DosError (0);
 {$ELSE FPC}
  {$IFDEF VirtualPascal}
   OS2Base.DosError (ferr_DisableHardErr);
  {$ELSE VirtualPascal}
   BseDos.DosError (0);
  {$ENDIF VirtualPascal}
 {$ENDIF FPC}
{$ENDIF}
{$IFDEF DOSSTUB}
   if CheckOS2 then Halt;
{$ENDIF}
   createlog:=false;
   for i:=1 to paramcount do
     begin
        if paramstr(i)='-l' then
          createlog:=true
        else if paramstr(i)='-h' then
          begin
             writeln('FPC Installer Copyright (c) 1993-2000 Florian Klaempfl');
             writeln('Command line options:');
             writeln('  -l   create log file');
             writeln;
             writeln('  -h   displays this help');
             halt(0);
          end
        else
          begin
             writeln('Illegal command line parameter: ',paramstr(i));
             halt(1);
          end;
     end;
   if createlog then
     begin
        assign(log,'install.log');
        rewrite(log);
        if not(lfnsupport) then
          writeln(log,'OS doesn''t have LFN support');
     end;
   getdir(0,startpath);
   successfull:=false;

   fillchar(cfg, SizeOf(cfg), 0);
   fillchar(data, SizeOf(data), 0);

   { set a default language }
   cfg.language:='English';

   { don't use a message file by default }
   msgfile:='';

   installapp.init;

   FSplit (FExpand (ParamStr (0)), DStr, CfgName, EStr);

   installapp.readcfg(CfgName + CfgExt);
   installapp.checkavailpack;
   installapp.do_languagedialog;
{   installapp.readcfg(startpath+dirsep+cfgfile);}
   if not(lfnsupport) then
     MessageBox(msg_no_lfn,nil,mfinformation or mfokbutton);
   installapp.do_installdialog;
   installapp.done;
   if createlog then
     close(log);
end.
{
  $Log$
  Revision 1.13  2001-11-24 14:34:10  carl
  * ppc386.cfg -> fpc.cfg

  Revision 1.12  2000/11/26 19:00:44  hajny
    * English correction

  Revision 1.11  2000/10/11 17:16:01  peter
    * fixed a typo and the setting of haside and hashtmlhelp (merged)

  Revision 1.10  2000/10/11 15:57:47  peter
    * merged ide additions

  Revision 1.9  2000/10/08 18:43:17  hajny
    * the language dialog repaired

  Revision 1.8  2000/09/24 10:52:36  peter
    * smaller window

  Revision 1.7  2000/09/22 23:13:37  pierre
     * add emulation for go32v2 and display currently extraced file
     and changes by Gabor for scrolling support (merged)

  Revision 1.6  2000/09/22 12:15:49  florian
    + support of Russian (Windows)

  Revision 1.5  2000/09/22 11:07:51  florian
    + all language dependend strings are now resource strings
    + the -Fr switch is now set in the ppc386.cfg

  Revision 1.4  2000/09/21 22:09:23  florian
    + start of multilanguage support

  Revision 1.3  2000/09/17 14:44:12  hajny
    * compilable with TP again

  Revision 1.2  2000/07/21 10:43:01  florian
    + added for lfn support

  Revision 1.1  2000/07/13 06:30:21  michael
  + Initial import

}
