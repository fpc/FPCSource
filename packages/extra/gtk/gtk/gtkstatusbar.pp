{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkStatusbar = ^TGtkStatusbar;
     TGtkStatusbar = record
          parent_widget : TGtkHBox;
          frame : PGtkWidget;
          thelabel : PGtkWidget;
          messages : PGSList;
          keys : PGSList;
          seq_context_id : guint;
          seq_message_id : guint;
       end;

     PGtkStatusbarClass = ^TGtkStatusbarClass;
     TGtkStatusbarClass = record
          parent_class : TGtkHBoxClass;
          messages_mem_chunk : PGMemChunk;
          text_pushed : procedure (statusbar:PGtkStatusbar; context_id:guint; thetext:Pgchar); cdecl;
          text_popped : procedure (statusbar:PGtkStatusbar; context_id:guint; thetext:Pgchar); cdecl;
       end;

     PGtkStatusbarMsg = ^TGtkStatusbarMsg;
     TGtkStatusbarMsg = record
          thetext : Pgchar;
          context_id : guint;
          message_id : guint;
       end;

Type
  GTK_STATUSBAR=PGtkStatusbar;
  GTK_STATUSBAR_CLASS=PGtkStatusbarClass;

function  GTK_STATUSBAR_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_statusbar_get_type';
function  GTK_IS_STATUSBAR(obj:pointer):boolean;
function  GTK_IS_STATUSBAR_CLASS(klass:pointer):boolean;

function  gtk_statusbar_get_type:TGtkType;cdecl;external gtkdll name 'gtk_statusbar_get_type';
function  gtk_statusbar_new:PGtkWidget;cdecl;external gtkdll name 'gtk_statusbar_new';
function  gtk_statusbar_get_context_id(statusbar:PGtkStatusbar; context_description:Pgchar):guint;cdecl;external gtkdll name 'gtk_statusbar_get_context_id';
function  gtk_statusbar_push(statusbar:PGtkStatusbar; context_id:guint; thetext:Pgchar):guint;cdecl;external gtkdll name 'gtk_statusbar_push';
procedure gtk_statusbar_pop(statusbar:PGtkStatusbar; context_id:guint);cdecl;external gtkdll name 'gtk_statusbar_pop';
procedure gtk_statusbar_remove(statusbar:PGtkStatusbar; context_id:guint; message_id:guint);cdecl;external gtkdll name 'gtk_statusbar_remove';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_STATUSBAR(obj:pointer):boolean;
begin
  GTK_IS_STATUSBAR:=(obj<>nil) and GTK_IS_STATUSBAR_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_STATUSBAR_CLASS(klass:pointer):boolean;
begin
  GTK_IS_STATUSBAR_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_STATUSBAR_TYPE);
end;

{$endif read_implementation}


