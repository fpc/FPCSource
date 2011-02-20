{ Parsed from Quartzcore.framework CATiledLayer.h }

{$ifdef TYPES}
{$ifndef CATILEDLAYER_PAS_T}
{$define CATILEDLAYER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef CATILEDLAYER_PAS_R}
{$define CATILEDLAYER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef CATILEDLAYER_PAS_F}
{$define CATILEDLAYER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef CATILEDLAYER_PAS_S}
{$define CATILEDLAYER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  CATiledLayer = objcclass;
  CATiledLayerPointer = ^CATiledLayer;
  CATiledLayerPtr = CATiledLayerPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef CATILEDLAYER_PAS_C}
{$define CATILEDLAYER_PAS_C}

{ CATiledLayer }
  CATiledLayer = objcclass external (CALayer)
    
  public
    class function fadeDuration: CFTimeInterval; message 'fadeDuration';
    procedure setLevelsOfDetail (newValue: size_t); message 'setLevelsOfDetail:';
    function levelsOfDetail: size_t; message 'levelsOfDetail';
    procedure setLevelsOfDetailBias (newValue: size_t); message 'setLevelsOfDetailBias:';
    function levelsOfDetailBias: size_t; message 'levelsOfDetailBias';
    procedure setTileSize (newValue: CGSize); message 'setTileSize:';
    function tileSize: CGSize; message 'tileSize';
  end;

{$endif}
{$endif}
