{
    $Id: dbconst.pp,v 1.11 2005/04/10 22:18:43 joost Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt, member of the
    Free Pascal development team

    Constants used for displaying messages in DB units

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit dbconst;

Interface

Const
  SActiveDataset           = 'Operation cannot be performed on an active dataset';
  SBadParamFieldType       = 'Bad fieldtype for parameter "%s".';
  SCantSetAutoIncFields    = 'AutoInc Fields are read-only';
  SConnected               = 'Operation cannot be performed on an connected database';
  SDatasetReadOnly         = 'Dataset is read-only.';
  SDatasetRegistered       = 'Dataset already registered : "%s"';
  SDuplicateFieldName      = 'Duplicate fieldname : "%s"';
  SErrAssTransaction       = 'Cannot assign transaction while old transaction active!';
  SErrColumnNotFound       = 'Column "%s" not found.';
  SErrDatabasenAssigned    = 'Database not assigned!';
  SErrNoDatabaseAvailable  = 'Invalid operation: Not attached to database';
  SErrNoSelectStatement    = 'Cannot open a non-select statement';
  SErrNoStatement          = 'SQL statement not set';
  SErrTransAlreadyActive   = 'Transaction already active';
  SErrTransactionnSet      = 'Transaction not set';
  SErrConnTransactionnSet  = 'Transaction of connection not set';
  STransNotActive          = 'Operation cannot be performed on an inactive transaction';
  STransActive             = 'Operation cannot be performed on an active transaction';
  SFieldNotFound           = 'Field not found : "%s"';
  SInactiveDataset         = 'Operation cannot be performed on an inactive dataset';
  SInvalidDisplayValues    = '"%s" are not valid boolean displayvalues';
  SInvalidFieldKind        = '%s : invalid field kind : ';
  SInvalidFieldSize        = 'Invalid field size : %d';
  SInvalidTypeConversion   = 'Invalid type conversion to %s in field %s';
  SNeedField               = 'Field %s is required, but not supplied.';
  SNeedFieldName           = 'Field needs a name';
  SNoDataset               = 'No dataset asssigned for field : "%s"';
  SNoDatasetRegistered     = 'No such dataset registered : "%s"';
  SNoDatasets              = 'No datasets are attached to the database';
  SNoSuchRecord            = 'Could not find the requested record.';
  SNoTransactionRegistered = 'No such transaction registered : "%s"';
  SNoTransactions          = 'No transactions are attached to the database';
  SNotABoolean             = '"%s" is not a valid boolean';
  SNotAFloat               = '"%s" is not a valid float';
  SNotAninteger            = '"%s" is not a valid integer';
  SNotConnected            = 'Operation cannot be performed on an disconnected database';
  SNotInEditState          = 'Operation not allowed, dataset "%s" is not in an edit state.';
  SParameterNotFound       = 'Parameter "%s" not found';
  SRangeError              = '%f is not between %f and %f for %s';
  SReadOnlyField           = 'Field %s cannot be modified, it is read-only.';
  STransactionRegistered   = 'Transaction already registered : "%s"';
  SUniDirectional          = 'Operation cannot be performed on an unidirectional dataset';
  SUnknownField            = 'No field named "%s" was found in dataset "%s"';
  SUnknownFieldType        = 'Unknown field type : %s';
  SUnknownParamFieldType   = 'Unknown fieldtype for parameter "%s".';
  SMetadataUnavailable     = 'The metadata is not available for this type of database.';
  SDeletedRecord           = 'The record is deleted.';
  SIndexNotFound           = 'Index ''%s'' not found';
  SParameterCountIncorrect = 'The number of parameters is incorrect.';
  SFieldValueError         = 'Invalid value for field ''%s''';
  SInvalidCalcType         = 'Field ''%s'' cannot be a calculated or lookup field';


Implementation

end.

{
  $Log: dbconst.pp,v $
  Revision 1.11  2005/04/10 22:18:43  joost
  Patch from Alexandrov Alexandru
  - implemented TDataset.BindFields
  - master-detail relation implemented
  - improved variant-support for fields
  - implemented TField.Assign and TField.AssignValue

  Revision 1.10  2005/04/10 18:28:30  joost
  - implemented parameter support for sqldb

  Revision 1.9  2005/03/18 10:17:34  michael
  + Patch to IndexDefs from Alexandrov Alexandru

  Revision 1.8  2005/02/14 17:13:12  peter
    * truncate log

  Revision 1.7  2005/02/07 11:23:04  joost
    - added SMetadataUnavailable
    - added SDeletedRecord

  Revision 1.6  2005/01/12 10:29:20  michael
    * Patch from Joost Van der Sluis:
    - added error message for if transaction of DB is not set

}
