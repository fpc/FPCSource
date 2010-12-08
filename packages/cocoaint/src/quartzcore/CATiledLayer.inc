{ Parsed from Quartzcore.framework CATiledLayer.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:07 CET 2010 }


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
  CATiledLayer = objcclass(CALayer)
    
  public
    class function alloc: CATiledLayer; message 'alloc';

    class function fadeDuration: CFTimeInterval; message 'fadeDuration';
    procedure setLevelsOfDetail (newValue: size_t); message 'setLevelsOfDetail:';
    function levelsOfDetail: size_t; message 'levelsOfDetail';
    procedure setLevelsOfDetailBias (newValue: size_t); message 'setLevelsOfDetailBias:';
    function levelsOfDetailBias: size_t; message 'levelsOfDetailBias';
    procedure setTileSize (newValue: CGSize); message 'setTileSize:';
    function tileSize: CGSize; message 'tileSize';
  end; external;

{$endif}
{$endif}
