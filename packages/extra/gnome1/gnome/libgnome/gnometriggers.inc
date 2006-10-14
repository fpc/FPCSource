{$IfDef read_interface}

type
  TGnomeTriggerType = (GTRIG_NONE,GTRIG_FUNCTION,GTRIG_COMMAND,
     GTRIG_MEDIAPLAY);

  TGnomeTriggerActionFunction = procedure (msg:Pchar; level:Pchar; supinfo:PPchar);cdecl;

  PGnomeTrigger = ^TGnomeTrigger;
  TGnomeTrigger = Record
    thetype : TGnomeTriggerType;
    U : record
      Case longint of
        1: (thefunction : TGnomeTriggerActionFunction);
        2: (command : pgchar);
        3 :(media : record
              thefile : Pgchar;
              cache_id : integer;
            end);
      end;
    level : PGchar;
  end;

procedure gnome_triggers_init;cdecl;external libgnomedll name 'gnome_triggers_init';
function gnome_triggers_readfile(infilename:Pchar):gint;cdecl;external libgnomedll name 'gnome_triggers_readfile';
procedure gnome_triggers_add_trigger(nt:PGnomeTrigger; args:array of const);cdecl;external libgnomedll name 'gnome_triggers_add_trigger';
procedure gnome_triggers_add_trigger(nt:PGnomeTrigger);cdecl;varargs;external libgnomedll name 'gnome_triggers_add_trigger';
procedure gnome_triggers_vadd_trigger(nt:PGnomeTrigger; supinfo:PPchar);cdecl;external libgnomedll name 'gnome_triggers_vadd_trigger';
procedure gnome_triggers_do(msg:Pchar; level:Pchar; args:array of const);cdecl;external libgnomedll name 'gnome_triggers_do';
procedure gnome_triggers_do(msg:Pchar; level:Pchar);cdecl;varargs;external libgnomedll name 'gnome_triggers_do';
procedure gnome_triggers_vdo(msg:Pchar; level:Pchar; supinfo:PPchar);cdecl;external libgnomedll name 'gnome_triggers_vdo';

{$EndIf read_interface}

{$Ifdef read_implementation}

{$Endif read_implementation}
