{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993-98 by Florian Klaempfl
    member of the Free Pascal development team

    This is the install program for the DOS version of Free Pascal

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program install;

  uses
{$ifdef HEAPTRC}
     heaptrc,
{$endif HEAPTRC}
     app,dialogs,views,objects,menus,drivers,strings,msgbox,dos,unzip,ziptypes;

  const
     maxpackages=20;
     maxdefcfgs=200;

     cfgfile='install.dat';

  type
     tpackage=record
       name : string[60];
       zip  : string[12];
     end;

     cfgrec=record
       title    : string[80];
       version  : string[20];
       basepath : string[80];
       binsub   : string[12];
       ppc386   : string[12];
       packages : longint;
       package  : array[1..maxpackages] of tpackage;
       defcfgfile : string[12];
       defcfgs  : longint;
       defcfg   : array[1..maxdefcfgs] of pstring;
     end;

     datarec=packed record
       basepath : string[80];
       mask     : word;
     end;

  var
     startpath   : string;
     successfull : boolean;
     cfg         : cfgrec;
     data        : datarec;

{*****************************************************************************
                                  Helpers
*****************************************************************************}

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


  function lower(const s : string):string;
    var
       i : integer;
    begin
       for i:=1 to length(s) do
         if s[i] in ['A'..'Z'] then
          lower[i]:=chr(ord(s[i])+32)
         else
          lower[i]:=s[i];
       lower[0]:=s[0];
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


  function diskspace(const zipfile : string) : string;
    var
      compressed,uncompressed : longint;
      s : string;
    begin
      s:=zipfile+#0;
      uncompressed:=UnzipSize(@s[1],compressed);
      uncompressed:=uncompressed shr 10;
      str(uncompressed,s);
      diskspace:=' ('+s+' Kb)';
    end;


  function createdir(var s : string) : boolean;
    var
      result : longint;
      dir : searchrec;
      params : array[0..0] of pointer;
    begin
       if s[length(s)]='\' then
        dec(s[0]);
       s:=lower(s);
       FindFirst(s,$ff,dir);
       if doserror=0 then
         begin
            result:=messagebox('The installation directory exists already. '+
              'Do want to enter a new installation directory ?',nil,
              mferror+mfyesbutton+mfnobutton);
            createdir:=(result=cmNo);
            exit;
         end;
       {$I-}
        mkdir(s);
       {$I+}
       if ioresult<>0 then
         begin
            params[0]:=@s;
            messagebox('The installation directory %s couldn''t be created',
              @params,mferror+mfokbutton);
            createdir:=false;
            exit;
         end;
       createdir:=true;
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
    begin
      findfirst(fn,$ff,dir);
      if doserror=0 then
       begin
         params[0]:=@fn;
         MessageBox('Config file %s already exists, default config not written',@params,mfinformation+mfokbutton);
         exit;
       end;
      assign(t,fn);
      {$I-}
       rewrite(t);
      {$I+}
      if ioresult<>0 then
       begin
         params[0]:=@fn;
         MessageBox('Can''t create %s, default config not written',@params,mfinformation+mfokbutton);
         exit;
       end;
      for i:=1to cfg.defcfgs do
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
                                   Cfg Read
*****************************************************************************}

  procedure readcfg(const fn:string);
    var
      t    : text;
      i,j,
      line : longint;
      item,
      s    : string;

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
      assign(t,fn);
      {$I-}
       reset(t);
      {$I+}
      if ioresult<>0 then
       begin
         writeln('error: ',fn,' not found!');
         halt(1);
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
               delete(s,1,i);
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
                writeln('error in confg, unknown item "',item,'" skipping line ',line);
             end
            else
             writeln('error in confg, skipping line ',line);
          end;
       end;
      close(t);
    end;


{*****************************************************************************
                               TUnZipDialog
*****************************************************************************}

  type
     punzipdialog=^tunzipdialog;
     tunzipdialog=object(tdialog)
         filetext : pstatictext;
         constructor Init(var Bounds: TRect; ATitle: TTitleStr);
         procedure do_unzip(s,topath:string);
     end;


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
            halt(1);
         end;
       fn:=startpath+'\'+s+#0;
       dir:=topath+#0;
       wild:='*.*'#0;
       FileUnzipEx(@fn[1],@dir[1],@wild[1]);
       if doserror<>0 then
         begin
            messagebox('Error when extracting. Disk full?',nil,mferror+mfokbutton);
            halt(1);
         end;
    end;


{*****************************************************************************
                               TInstallDialog
*****************************************************************************}

  type
     pinstalldialog = ^tinstalldialog;
     tinstalldialog = object(tdialog)
        constructor init;
     end;

  type
     tapp = object(tapplication)
         procedure initmenubar;virtual;
         procedure handleevent(var event : tevent);virtual;
         procedure do_installdialog;
     end;

  var
     installapp : tapp;


  constructor tinstalldialog.init;
    var
       r : trect;
       mask_components : longint;
       i,line : integer;
       items : psitem;
       p,f : pview;

    const
       width = 76;
       height = 20;
       x1 = (79-width) div 2;
       y1 = (23-height) div 2;
       x2 = x1+width;
       y2 = y1+height;

    begin
       r.assign(x1,y1,x2,y2);
       inherited init(r,cfg.title+' Installation');

       line:=2;
       r.assign(3,line+1,28,line+2);

       f:=new(pinputline,init(r,80));
       insert(f);

       r.assign(3,line,8,line+1);
       insert(new(plabel,init(r,'~P~ath',f)));

     { walk packages reverse and insert a newsitem for each, and set the mask }
       items:=nil;
       mask_components:=0;
       for i:=cfg.packages downto 1 do
        begin
          if file_exists(cfg.package[i].zip,startpath) then
           begin
             items:=newsitem(cfg.package[i].name+diskspace(startpath+'\'+cfg.package[i].zip),items);
             mask_components:=mask_components or packagemask(i);
           end
          else
           begin
             items:=newsitem(cfg.package[i].name,items);
           end;
        end;

     { If no component found abort }
       if mask_components=0 then
        begin
          messagebox('No components found to install, aborting.',nil,mferror+mfokbutton);
          { this clears the screen at least PM }
          installapp.done;
          halt(1);
        end;

       inc(line,3);
       r.assign(3,line+1,width-3,line+cfg.packages+1);
       p:=new(pcheckboxes,init(r,items));
       r.assign(3,line,14,line+1);
       insert(new(plabel,init(r,'~C~omponents',p)));
       pcluster(p)^.enablemask:=mask_components;
       insert(p);

       inc(line,cfg.packages+2);
       r.assign((width div 2)-14,line,(width div 2)-4,line+2);
       insert(new(pbutton,init(r,'~O~k',cmok,bfdefault)));
       r.assign((width div 2)+4,line,(width div 2)+14,line+2);
       insert(new(pbutton,init(r,'~C~ancel',cmcancel,bfnormal)));

       f^.select;
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
       r    : trect;
       result,
       c    : word;
       i    : longint;
    begin
      data.basepath:=cfg.basepath;
      data.mask:=0;

      repeat
        p:=new(pinstalldialog,init);
        { default settings }
        c:=executedialog(p,@data);
        if (c=cmok) then
          begin
            if (data.mask>0) then
             begin
               if createdir(data.basepath) then
                break;
             end
            else
             begin
               result:=messagebox('No components selected.'#13#13'Abort installation?',nil,
                 mferror+mfyesbutton+mfnobutton);
               if result=cmYes then
                exit;
             end;
          end
        else
          exit;
      until false;

      r.assign(20,7,60,16);
      p2:=new(punzipdialog,init(r,'Extracting files'));
      desktop^.insert(p2);
      for i:=1to cfg.packages do
       begin
         if data.mask and packagemask(i)<>0 then
          p2^.do_unzip(cfg.package[i].zip,data.basepath);
       end;
      desktop^.delete(p2);
      dispose(p2,done);

      writedefcfg(data.basepath+cfg.binsub+'\'+cfg.defcfgfile);

      messagebox('Installation successfull',nil,mfinformation+mfokbutton);
      successfull:=true;
    end;


  procedure tapp.initmenubar;
    var
       r : trect;
    begin
       getextent(r);
       r.b.y:=r.a.y+1;
       menubar:=new(pmenubar,init(r,newmenu(
          newsubmenu('~F~ree Pascal '+cfg.version,hcnocontext,newmenu(nil
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


begin
   getdir(0,startpath);
   successfull:=false;

   fillchar(cfg, SizeOf(cfg), 0);
   fillchar(data, SizeOf(data), 0);

   readcfg(cfgfile);

   installapp.init;
   installapp.do_installdialog;
   installapp.done;

   if successfull then
     begin
        writeln('Extend your PATH variable with ''',data.basepath+cfg.binsub+'''');
        writeln;
        writeln('To compile files enter ''',cfg.ppc386,' [file]''');
        writeln;
     end;
end.
{
  $Log$
  Revision 1.11  1998-11-01 20:32:25  peter
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
