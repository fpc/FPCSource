{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt, member of the
    Free Pascal development team

    Constants used for displaying messages in DB unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Const
  SUnknownFieldType = 'Unknown field type : %s';
  SUnknownField = 'No field named "%s" was found in dataset "%s"';
  SNeedFieldName = 'Field needs a name';
  SInvalidTypeConversion = 'Invalid type conversion to %s in field %s';
  SReadOnlyField = 'Field %s cannot be modified, it is read-only.';
  SInvalidFieldSize = 'Invalid field size : %d';
  SNoDataset = 'No dataset asssigned for field : "%s"';
  SDuplicateFieldName = 'Duplicate fieldname : "%s"';
  SFieldNotFound = 'Field not found : "%s"';
  SInvalidFieldKind = '%s : invalid field kind : ';
  SRangeError = '%f is not between %f and %f for %s';
  SNotAninteger = '"%s" is not a valid integer';
  SCantSetAutoIncFields = 'AutoInc Fields are read-only';
  SNotAFloat = '"%s" is not a valid float';
  SInvalidDisplayValues = '"%s" are not valid boolean displayvalues';
  SNotABoolean = '"%s" is not a valid boolean';
  SInactiveDataset = 'Operation cannot be performed on an inactive dataset';
  SActiveDataset = 'Operation cannot be performed on an active dataset';
  SNoDatasets = 'No datasets are attached to the database';
  SDatasetRegistered = 'Dataset already registered : "%s"';
  SNoDatasetRegistered = 'No such dataset registered : "%s"';
  SNotConnected = 'Operation cannot be performed on an disconnected database';
  SConnected = 'Operation cannot be performed on an connected database';
  SNoSuchRecord = 'Could not find the requested record.';
  SDatasetReadOnly = 'Dataset is read-only.';
  SNeedField = 'Field %s is required, but not supplied.';
  SNotInEditState = 'Operation not allowed, dataset "%s" is not in an edit state.';
{
  $Log$
  Revision 1.4  2002-09-07 15:15:23  peter
    * old logs removed and tabs fixed

}
