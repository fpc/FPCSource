{ Parsed from AppKit.framework NSPredicateEditorRowTemplate.h }


{$ifdef TYPES}
type
  NSPredicateEditorRowTemplatePtr = ^NSPredicateEditorRowTemplate;
{$endif}

{$ifdef CLASSES}

type
  NSPredicateEditorRowTemplate = objcclass external (NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    _templateType: NSInteger;
    _predicateOptions: NSUInteger;
    _predicateModifier: NSUInteger;
    _leftAttributeType: NSUInteger;
    _rightAttributeType: NSUInteger;
    _views: NSArray;
    _ptFlags: bitpacked record
      case byte of
        0: (_anonBitField__ptFlags0: cuint);
        1: (
          leftIsWildcard: 0..1;
          rightIsWildcard: 0..1;
          reserved: 0..((1 shl 30)-1);
        );
      end;
    _patternReserved: id;
  public
    function matchForPredicate (predicate: NSPredicate): double; message 'matchForPredicate:';
    function templateViews: NSArray; message 'templateViews';
    procedure setPredicate (predicate: NSPredicate); message 'setPredicate:';
    function predicateWithSubpredicates (subpredicates: NSArray): NSPredicate; message 'predicateWithSubpredicates:';
    function displayableSubpredicatesOfPredicate (predicate: NSPredicate): NSArray; message 'displayableSubpredicatesOfPredicate:';
    function initWithLeftExpressions_rightExpressions_modifier_operators_options (leftExpressions: NSArray; rightExpressions: NSArray; modifier: NSComparisonPredicateModifier; operators: NSArray; options: NSUInteger): instancetype; message 'initWithLeftExpressions:rightExpressions:modifier:operators:options:';
    function initWithLeftExpressions_rightExpressionAttributeType_modifier_operators_options (leftExpressions: NSArray; attributeType: NSAttributeType; modifier: NSComparisonPredicateModifier; operators: NSArray; options: NSUInteger): instancetype; message 'initWithLeftExpressions:rightExpressionAttributeType:modifier:operators:options:';
    function initWithCompoundTypes (compoundTypes: NSArray): instancetype; message 'initWithCompoundTypes:';
    function leftExpressions: NSArray; message 'leftExpressions';
    function rightExpressions: NSArray; message 'rightExpressions';
    function rightExpressionAttributeType: NSAttributeType; message 'rightExpressionAttributeType';
    function modifier: NSComparisonPredicateModifier; message 'modifier';
    function operators: NSArray; message 'operators';
    function options: NSUInteger; message 'options';
    function compoundTypes: NSArray; message 'compoundTypes';
    class function templatesWithAttributeKeyPaths_inEntityDescription (keyPaths: NSArray; entityDescription: NSEntityDescription): NSArray; message 'templatesWithAttributeKeyPaths:inEntityDescription:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

