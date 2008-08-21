// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}

    PGtkCellRendererProgress = ^TGtkCellRendererProgress;
    TGtkCellRendererProgress = record
        parent : TGtkCellRenderer;
        priv : pointer;
    end;

    PGtkCellRendererProgressClass = ^TGtkCellRendererProgressClass;
    TGtkCellRendererProgressClass = record
       parent_class : TGtkCellRendererClass;
       // Padding for future expansion
       _gtk_reserved1: procedure; cdecl;
       _gtk_reserved2: procedure; cdecl;
       _gtk_reserved3: procedure; cdecl;
       _gtk_reserved4: procedure; cdecl;
    end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

    function GTK_TYPE_CELL_RENDERER_PROGRESS: GType;
    function GTK_CELL_RENDERER_PROGRESS(obj: Pointer): PGtkCellRendererProgress;
    function GTK_CELL_RENDERER_PROGRESS_CLASS(klass: pointer): PGtkCellRendererProgressClass;
    function GTK_IS_CELL_RENDERER_PROGRESS(obj: pointer): boolean;
    function GTK_IS_CELL_RENDERER_PROGRESS_CLASS(klass: pointer):boolean;
    function GTK_CELL_RENDERER_PROGRESS_GET_CLASS(obj: pointer): PGtkCellRendererProgressClass;

    function gtk_cell_renderer_progress_get_type:TGtkType; cdecl; external gtklib;
    function gtk_cell_renderer_progress_new: PGtkCellRendererProgress; cdecl; external gtklib;

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}

    function GTK_TYPE_CELL_RENDERER_PROGRESS: GType;
    begin
      Result:= gtk_cell_renderer_progress_get_type ();
    end;
    
    function GTK_CELL_RENDERER_PROGRESS(obj: Pointer): PGtkCellRendererProgress;
    begin
      Result:= PGtkCellRendererProgress(GTK_CHECK_CAST(obj, GTK_TYPE_CELL_RENDERER_PROGRESS));
    end;
    
    function GTK_CELL_RENDERER_PROGRESS_CLASS(klass: pointer): PGtkCellRendererProgressClass;
    begin
      Result:= PGtkCellRendererProgressClass(GTK_CHECK_CLASS_CAST(klass, GTK_TYPE_CELL_RENDERER_PROGRESS));
    end;
     
    function GTK_IS_CELL_RENDERER_PROGRESS(obj: pointer): boolean;
    begin
      Result:= GTK_CHECK_TYPE(obj, GTK_TYPE_CELL_RENDERER_PROGRESS);
    end;
    
    function GTK_IS_CELL_RENDERER_PROGRESS_CLASS(klass: pointer):boolean;
    begin
      Result:= GTK_CHECK_CLASS_TYPE(klass, GTK_TYPE_CELL_RENDERER_PROGRESS);
    end;
    
    function GTK_CELL_RENDERER_PROGRESS_GET_CLASS(obj: pointer): PGtkCellRendererProgressClass;
    begin
      Result:= PGtkCellRendererProgressClass(GTK_CHECK_GET_CLASS(obj, GTK_TYPE_CELL_RENDERER_PROGRESS));
    end;


{$ENDIF read_implementation}
// included by gtk2.pas
