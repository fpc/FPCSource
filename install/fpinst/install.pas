{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993-98 by Florian Klaempfl
    member of the Free Pascal development team

    This is the install program for the DOS and OS/2 versions of Free Pascal

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

{$IFDEF OS2}
 {$UNDEF FV}
 {$IFDEF VIRTUALPASCAL}
  {$DEFINE DLL}
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
{$ifdef HEAPTRC}
     heaptrc,
{$endif HEAPTRC}
     strings,dos,objects,drivers,
{$IFDEF FV}
     commands,
{$ENDIF}
     unzip,ziptypes,
{$IFDEF DLL}
     unzipdll,
{$ENDIF}
     app,dialogs,views,menus,msgbox,colortxt,tabs;


  const
     installerversion='0.99.14';

     maxpackages=20;
     maxsources=20;
     maxdefcfgs=1024;

     CfgExt = '.dat';

{$ifdef linux}
     DirSep='/';
{$else}
     DirSep='\';
{$endif}

  type
     tpackage=record
       name : string[60];
       zip  : string[12];
     end;

     cfgrec=record
       title    : string[80];
       version  : string[20];
       basepath : DirStr;
       binsub   : string[12];
       ppc386   : string[12];
       packages : longint;
       package  : array[1..maxpackages] of tpackage;
       sources  : longint;
       source   : array[1..maxsources] of tpackage;
       defcfgfile : string[12];
       defcfgs  : longint;
       defcfg   : array[1..maxdefcfgs] of pstring;
     end;

     datarec=packed record
       basepath : DirStr;
       cfgval   : word;
       packmask : word;
       srcmask  : word;
     end;

     punzipdialog=^tunzipdialog;
     tunzipdialog=object(tdialog)
         filetext : pstatictext;
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

     tapp = object(tapplication)
         procedure initmenubar;virtual;
         procedure handleevent(var event : tevent);virtual;
         procedure do_installdialog;
         procedure readcfg(const fn:string);
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


  procedure Replace(var s:string;const s1,s2:string);
    var
       i  : longint;
    begin
      repeat
        i:=pos(s1,s);
        if i>0 then
         begin
           Delete(s,i,length(s1));
           Insert(s2,s,i);
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
      uncompressed:=UnzipSize(@s[1],compressed);
      DiskSpaceN:=uncompressed shr 10;
    end;


  function diskspace(const zipfile : string) : string;
    var
      uncompressed : longint;
      s : string;
    begin
      uncompressed:=DiskSpaceN (zipfile);
      str(uncompressed,s);
      diskspace:=' ('+s+' KB)';
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
                messagebox('A file with the name chosen as the installation '+
                'directory exists already. Cannot create this directory!',nil,
                mferror+mfokbutton);
                createinstalldir:=false;
              end else
                createinstalldir:=messagebox('The installation directory exists already. '+
                'Do you want to continue ?',nil,
                mferror+mfyesbutton+mfnobutton)=cmYes;
            exit;
         end;
       err:=Createdir(s);
       if err then
         begin
            params[0]:=@s;
            messagebox('The installation directory %s couldn''t be created',
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


{*****************************************************************************
                          Writing of ppc386.cfg
*****************************************************************************}

  procedure writedefcfg(const fn:string);
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
         if MessageBox('Config %s already exists, continue writing default config?',@params,
                       mfinformation+mfyesbutton+mfnobutton)=cmNo then
           exit;
       end;
    { create directory }
      fsplit(fn,d,n,e);
      createdir(d);
    { create the ppc386.cfg }
      assign(t,fn);
      {$I-}
       rewrite(t);
      {$I+}
      if ioresult<>0 then
       begin
         params[0]:=@fn;
         MessageBox(#3'Default config not written.'#13#3'%s'#13#3'couldn''t be created',@params,mfinformation+mfokbutton);
         exit;
       end;
      for i:=1 to cfg.defcfgs do
       if assigned(cfg.defcfg[i]) then
         begin
           s:=cfg.defcfg[i]^;
           Replace(s,'$1',data.basepath);
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
      R.Assign(11, 4, 38, 5);
      filetext:=new(pstatictext,init(r,'File: '));
      insert(filetext);
    end;


  procedure tunzipdialog.do_unzip(s,topath : string);
    var
      fn,dir,wild : string;
    begin
       Disposestr(filetext^.text);
       filetext^.Text:=NewStr('File: '+s);
       filetext^.drawview;
       if not(file_exists(s,startpath)) then
         begin
            messagebox('File: '+s+' missed for the selected installation. '+
                       'Installation doesn''t becomes complete',nil,mferror+mfokbutton);
            errorhalt;
         end;
       fn:=startpath+DirSep+s+#0;
       dir:=topath+#0;
       wild:=AllFiles + #0;
(* TH - added to clear the previous state of DosError *)
       DosError := 0;
{$IFDEF DLL}
       if
{$ENDIF}
          FileUnzipEx(@fn[1],@dir[1],@wild[1])
{$IFDEF DLL}
                                               = 0 then
{$ELSE}
                                              ;
       if doserror<>0 then
{$ENDIF}
         begin
            messagebox('Error when extracting. Disk full?',nil,mferror+mfokbutton);
            errorhalt;
         end;
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
      YB := 14;

{$IFNDEF LINUX}
      S := Data.BasePath + Cfg.BinSub;
      if Pos (Upper (S), Upper (GetEnv ('PATH'))) = 0 then
      begin
       WPath := true;
       Inc (YB, 2);
      end else WPath := false;
 {$IFDEF OS2}
      if DosLoadModule (@ErrPath, SizeOf (ErrPath), @EMXName, Handle) = 0 then
      begin
       WLibPath := false;
       DosFreeModule (Handle);
      end else
      begin
       WLibPath := true;
       Inc (YB, 2);
      end;
 {$ENDIF}
{$ENDIF}

      R.Assign(6, 6, 74, YB);
      inherited init(r,'Installation Successfull');

{$IFNDEF LINUX}
      if WPath then
      begin
       R.Assign(2, 3, 64, 5);
       P:=new(pstatictext,init(r,'Extend your PATH variable with '''+S+''''));
       insert(P);
      end;

 {$IFDEF OS2}
      if WLibPath then
      begin
       if WPath then S := 'and your LIBPATH with ''' + S + '\dll''' else
                             S := 'Extend your LIBPATH with ''' + S + '\dll''';
       R.Assign (2, YB - 13, 64, YB - 11);
       P := New (PStaticText, Init (R, S));
       Insert (P);
      end;
 {$ENDIF}
{$ENDIF}

      R.Assign(2, YB - 11, 64, YB - 10);
      P:=new(pstatictext,init(r,'To compile files enter '''+cfg.ppc386+' [file]'''));
      insert(P);

      R.Assign (29, YB - 9, 39, YB - 7);
      Control := New (PButton, Init (R,'~O~k', cmOK, bfDefault));
      Insert (Control);
    end;


{*****************************************************************************
                               TInstallDialog
*****************************************************************************}

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
       srcmask,
       mask_components : longint;
       i,line : integer;
       srcitems,items : psitem;
       f : pview;
       okbut,cancelbut : pbutton;
       packcbs,sourcecbs : pcheckboxes;
       labpath : plabel;
       ilpath : pinputline;
       tab : ptab;
       titletext : pcoloredtext;
       labcfg : plabel;
       cfgcb : pcheckboxes;
    begin
       f:=nil;
     { walk packages reverse and insert a newsitem for each, and set the mask }
       items:=nil;
       mask_components:=0;
       for i:=cfg.packages downto 1 do
        begin
          if file_exists(cfg.package[i].zip,startpath) then
           begin
             items:=newsitem(cfg.package[i].name+diskspace(startpath+DirSep+cfg.package[i].zip),items);
             mask_components:=mask_components or packagemask(i);
           end
          else
           items:=newsitem(cfg.package[i].name,items);
        end;

     { walk source packages reverse and insert a newsitem for each, and set the mask }
       srcitems:=nil;
       srcmask:=0;
       for i:=cfg.sources downto 1 do
        begin
          if file_exists(cfg.source[i].zip,startpath) then
           begin
             srcitems:=newsitem(cfg.source[i].name+diskspace(startpath+DirSep+cfg.source[i].zip),srcitems);
             srcmask:=srcmask or packagemask(i);
           end
          else
           srcitems:=newsitem(cfg.source[i].name,srcitems);
        end;

     { If no component found abort }
       if (mask_components=0) and (srcmask=0) then
        begin
          messagebox('No components found to install, aborting.',nil,mferror+mfokbutton);
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

       {-------- Sheet 1 ----------}
       R.Copy(TabIR);
       r.move(0,1);
       r.b.x:=r.a.x+40;
       r.b.y:=r.a.y+1;
       new(titletext,init(r,cfg.title,$71));

       r.move(0,2);
       r.b.x:=r.a.x+40;
       new(labpath,init(r,'~B~ase path',f));
       r.move(0,1);
       r.b.x:=r.a.x+40;
       r.b.y:=r.a.y+1;
       new(ilpath,init(r,high(DirStr)));

       r.move(0,2);
       r.b.x:=r.a.x+40;
       new(labcfg,init(r,'Con~f~ig',f));
       r.move(0,1);
       r.b.x:=r.a.x+40;
       r.b.y:=r.a.y+1;
       new(cfgcb,init(r,newsitem('create ppc386.cfg',nil)));
       data.cfgval:=1;

       {-------- Sheet 2 ----------}
       R.Copy(TabIR);
       new(packcbs,init(r,items));
       data.packmask:=mask_components;
       pcluster(packcbs)^.enablemask:=mask_components;

       {-------- Sheet 3 ----------}
       R.Copy(TabIR);
       new(sourcecbs,init(r,srcitems));
       data.srcmask:=srcmask;
       pcluster(sourcecbs)^.enablemask:=srcmask;

       {--------- Main ---------}
       New(Tab, Init(TabR,
         NewTabDef('~G~eneral',IlPath,
           NewTabItem(TitleText,
           NewTabItem(LabPath,
           NewTabItem(ILPath,
           NewTabItem(LabCfg,
           NewTabItem(CfgCB,
           nil))))),
         NewTabDef('~P~ackages',PackCbs,
           NewTabItem(PackCbs,
           nil),
         NewTabDef('~S~ources',SourceCbs,
           NewTabItem(SourceCbs,
           nil),
         nil)))));
       Tab^.GrowMode:=0;
       Insert(Tab);

       line:=tabr.b.y;
       r.assign((width div 2)-14,line,(width div 2)-4,line+2);
       new(okbut,init(r,'~O~k',cmok,bfdefault));
       Insert(OkBut);

       r.assign((width div 2)+4,line,(width div 2)+14,line+2);
       new(cancelbut,init(r,'~C~ancel',cmcancel,bfnormal));
       Insert(CancelBut);

       Tab^.Select;
    end;


{*****************************************************************************
                                TApp
*****************************************************************************}

  const
     cmstart = 1000;

  procedure tapp.do_installdialog;
    var
       p    : pinstalldialog;
       p2   : punzipdialog;
       p3   : penddialog;
       r    : trect;
       result,
       c    : word;
       i    : longint;
{$ifndef linux}
       DSize,Space : longint;
       S: DirStr;
{$endif}
    begin
      data.basepath:=cfg.basepath;
      data.cfgval:=0;
      data.srcmask:=0;
      data.packmask:=0;

      repeat
      { select components }
        p:=new(pinstalldialog,init);
        c:=executedialog(p,@data);
        if (c=cmok) then
          begin
            if Data.BasePath = '' then
              messagebox('Please, choose the directory for installation first.',nil,mferror+mfokbutton)
            else
             begin
               if (data.srcmask>0) or (data.packmask>0) then
                begin
{$IFNDEF LINUX}
                { TH - check the available disk space here }
                  DSize := 0;
                  for i:=1 to cfg.packages do
                   begin
                     if data.packmask and packagemask(i)<>0 then
                      Inc (DSize, DiskSpaceN(cfg.package[i].zip));
                   end;
                  for i:=1 to cfg.sources do
                   begin
                     if data.srcmask and packagemask(i)<>0 then
                      Inc (DSize, DiskSpaceN(cfg.source[i].zip));
                   end;
                  if data.packmask and packagemask(i)<>0 then
                   Inc (DSize, DiskSpaceN(cfg.package[i].zip));
                  S := FExpand (Data.BasePath);
                  if S [Length (S)] = DirSep then
                   Dec (S [0]);
                  Space := DiskFree (byte (S [1]) - 64) shr 10;
                  if Space < DSize then
                   S := 'is not'
                  else
                   S := '';
                  if Space < DSize + 500 then
                   begin
                     if S = '' then
                      S := 'might not be';
                     if messagebox('There ' + S + ' enough space on the target ' +
                                   'drive for all the selected components. Do you ' +
                                   'want to change the installation path?',nil,
                                   mferror+mfyesbutton+mfnobutton) = cmYes then
                      Continue;
                   end;
{$ENDIF}
                  if createinstalldir(data.basepath) then
                   break;
                end
               else
                begin
                  result:=messagebox('No components selected.'#13#13'Abort installation?',nil,mferror+mfyesbutton+mfnobutton);
                  if result=cmYes then
                   exit;
                end;
             end;
          end
        else
          exit;
      until false;

    { extract packages }
      r.assign(20,7,60,16);
      p2:=new(punzipdialog,init(r,'Extracting Packages'));
      desktop^.insert(p2);
      for i:=1 to cfg.packages do
       begin
         if data.packmask and packagemask(i)<>0 then
          p2^.do_unzip(cfg.package[i].zip,data.basepath);
       end;
      desktop^.delete(p2);
      dispose(p2,done);

    { extract sources }
      r.assign(20,7,60,16);
      p2:=new(punzipdialog,init(r,'Extracting Sources'));
      desktop^.insert(p2);
      for i:=1 to cfg.sources do
       begin
         if data.srcmask and packagemask(i)<>0 then
          p2^.do_unzip(cfg.source[i].zip,data.basepath);
       end;
      desktop^.delete(p2);
      dispose(p2,done);

    { write config }
      if (data.cfgval and 1)<>0 then
       writedefcfg(data.basepath+cfg.binsub+DirSep+cfg.defcfgfile);

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
            messagebox('File %s not found!',@params,mferror+mfokbutton);
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
                if item='BASEPATH' then
                 cfg.basepath:=s
               else
                if item='PPC386' then
                 cfg.ppc386:=s
               else
                if item='BINSUB' then
                 cfg.binsub:=s
               else
                if item='CFGFILE' then
                 cfg.defcfgfile:=s
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
               else
                if item='PACKAGE' then
                 begin
                   j:=pos(',',s);
                   if (j>0) and (cfg.packages<maxpackages) then
                    begin
                      inc(cfg.packages);
                      cfg.package[cfg.packages].zip:=copy(s,1,j-1);
                      cfg.package[cfg.packages].name:=copy(s,j+1,255);
                    end;
                 end
               else
                if item='SOURCE' then
                 begin
                   j:=pos(',',s);
                   if (j>0) and (cfg.sources<maxsources) then
                    begin
                      inc(cfg.sources);
                      cfg.source[cfg.sources].zip:=copy(s,1,j-1);
                      cfg.source[cfg.sources].name:=copy(s,j+1,255);
                    end;
                 end;
             end;
          end;
       end;
      close(t);
    end;


  procedure tapp.initmenubar;
    var
       r : trect;
    begin
       getextent(r);
       r.b.y:=r.a.y+1;
       menubar:=new(pmenubar,init(r,newmenu(
          newsubmenu('~F~ree Pascal Installer '+installerversion,hcnocontext,newmenu(nil
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

begin
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
   getdir(0,startpath);
   successfull:=false;

   fillchar(cfg, SizeOf(cfg), 0);
   fillchar(data, SizeOf(data), 0);

   installapp.init;

   FSplit (FExpand (ParamStr (0)), DStr, CfgName, EStr);

   installapp.readcfg(CfgName + CfgExt);
{   installapp.readcfg(startpath+dirsep+cfgfile);}
   installapp.do_installdialog;
   installapp.done;
end.
{
  $Log$
  Revision 1.10  2000-01-18 00:22:48  peter
    * fixed uninited local var

  Revision 1.9  1999/08/03 20:21:53  peter
    * fixed sources mask which was not set correctly

  Revision 1.7  1999/07/01 07:56:58  hajny
    * installation to root fixed

  Revision 1.6  1999/06/29 22:20:19  peter
    * updated to use tab pages

  Revision 1.5  1999/06/25 07:06:30  hajny
    + searching for installation script updated

  Revision 1.4  1999/06/10 20:01:23  peter
    + fcl,fv,gtk support

  Revision 1.3  1999/06/10 15:00:14  peter
    * fixed to compile for not os2
    * update install.dat

  Revision 1.2  1999/06/10 07:28:27  hajny
    * compilable with TP again

  Revision 1.1  1999/02/19 16:45:26  peter
    * moved to fpinst/ directory
    + makefile

  Revision 1.15  1999/02/17 22:34:08  peter
    * updates from TH for OS2

  Revision 1.14  1998/12/22 22:47:34  peter
    * updates for OS2
    * small fixes

  Revision 1.13  1998/12/21 13:11:39  peter
    * updates for 0.99.10

  Revision 1.12  1998/12/16 00:25:34  peter
    * updated for 0.99.10
    * new end dialogbox

  Revision 1.11  1998/11/01 20:32:25  peter
    * packed record

  Revision 1.10  1998/10/25 23:38:35  peter
    * removed warnings

  Revision 1.9  1998/10/23 16:57:40  pierre
   * compiles without -So option
   * the main dialog init was buggy !!

  Revision 1.8  1998/09/22 21:10:31  jonas
    * initialize cfg and data with 0 at startup

  Revision 1.7  1998/09/16 16:46:37  peter
    + updates

  Revision 1.6  1998/09/15 13:11:14  pierre
  small fix to cleanup if no package

  Revision 1.5  1998/09/15 12:06:06  peter
    * install updated to support w32 and dos and config file

  Revision 1.4  1998/09/10 10:50:49  florian
    * DOS install program updated

  Revision 1.3  1998/09/09 13:39:58  peter
    + internal unzip
    * dialog is showed automaticly

  Revision 1.2  1998/04/07 22:47:57  florian
    + version/release/patch numbers as string added

}
