{ Parsed from Quartzcore.framework CARenderer.h }

{$ifdef TYPES}
{$ifndef CARENDERER_PAS_T}
{$define CARENDERER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef CARENDERER_PAS_R}
{$define CARENDERER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef CARENDERER_PAS_F}
{$define CARENDERER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef CARENDERER_PAS_S}
{$define CARENDERER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  CARenderer = objcclass;
  CARendererPointer = ^CARenderer;
  CARendererPtr = CARendererPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef CARENDERER_PAS_C}
{$define CARENDERER_PAS_C}

{ CARenderer }
  CARenderer = objcclass external (NSObject)
  private
    __priv: Pointer;
    
  public
    class function rendererWithCGLContext_options(ctx: Pointer; dict: NSDictionary): CARenderer; message 'rendererWithCGLContext:options:';
    procedure setLayer (newValue: CALayer); message 'setLayer:';
    function layer: CALayer; message 'layer';
    procedure setBounds (newValue: CGRect); message 'setBounds:';
    function bounds: CGRect; message 'bounds';
    procedure beginFrameAtTime_timeStamp(t: CFTimeInterval; ts: CVTimeStampPtr); message 'beginFrameAtTime:timeStamp:';
    function updateBounds: CGRect; message 'updateBounds';
    procedure addUpdateRect(r: CGRect); message 'addUpdateRect:';
    procedure render; message 'render';
    function nextFrameTime: CFTimeInterval; message 'nextFrameTime';
    procedure endFrame; message 'endFrame';
  end;

{$endif}
{$endif}
