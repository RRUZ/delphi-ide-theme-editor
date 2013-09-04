{**************************************************************************************************}
{                                                                                                  }
{ Unit uSupportedIDEs                                                                              }
{ unit uSupportedIDEs  for the Delphi IDE Theme Editor                                             }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is uSupportedIDEs.pas.                                                         }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2013 Rodrigo Ruz V.                    }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}

unit uSupportedIDEs;

interface

type
  TSupportedIDEs = (DelphiIDE,LazarusIDE, SMSIDE);
const
  ListSupportedIDEs : array[TSupportedIDEs] of string = ('Delphi IDE','Lazarus IDE', 'Smart Mobile Studio IDE');

implementation

end.
