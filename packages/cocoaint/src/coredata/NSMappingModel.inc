{ Parsed from Coredata.framework NSMappingModel.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Fri Oct 2 11:46:33 EDT 2009 }


{$ifdef TYPES}
{$ifndef NSMAPPINGMODEL_PAS_T}
{$define NSMAPPINGMODEL_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSMAPPINGMODEL_PAS_R}
{$define NSMAPPINGMODEL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSMAPPINGMODEL_PAS_F}
{$define NSMAPPINGMODEL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSMAPPINGMODEL_PAS_S}
{$define NSMAPPINGMODEL_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSMappingModel = objcclass;
  NSMappingModelPointer = ^NSMappingModel;

{$endif}

{$ifdef CLASSES}
{$ifndef NSMAPPINGMODEL_PAS_C}
{$define NSMAPPINGMODEL_PAS_C}

{ NSMappingModel }
  NSMappingModel = objcclass(NSObject)
  private
    __reserved: Pointer;
    __reserved1: Pointer;
    __reserved2: Pointer;
    __entityMappings: NSMutableArray;
    __entityMappingsByName: NSMutableDictionary;
    __modelMappingFlags: bitpacked record
        _isInUse: 0..1;
        _reservedModelMapping: 0..((1 shl 31)-1);
      end;
    
  public
    class function alloc: NSMappingModel; message 'alloc';

    class function mappingModelFromBundles_forSourceModel_destinationModel(bundles: NSArray; sourceModel: NSManagedObjectModel; destinationModel: NSManagedObjectModel): NSMappingModel; message 'mappingModelFromBundles:forSourceModel:destinationModel:';
    function initWithContentsOfURL(url: NSURL): id; message 'initWithContentsOfURL:';
    function entityMappings: NSArray; message 'entityMappings';
    procedure setEntityMappings(mappings: NSArray); message 'setEntityMappings:';
    function entityMappingsByName: NSDictionary; message 'entityMappingsByName';
  end; external;

{$endif}
{$endif}
