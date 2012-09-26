{**************************************************************************************************}
{                                                                                                  }
{ Unit uHookLib                                                                                    }
{ unit uHookLib  for the Delphi IDE Colorizer                                                      }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is uHookLib.pas.                                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2012 Rodrigo Ruz V.                    }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}

unit uHookLib;

interface

type
  TJumpOfs = Integer;
  PPointer = ^Pointer;

  PXRedirCode = ^TXRedirCode;
  TXRedirCode = packed record
    Jump: Byte;
    Offset: TJumpOfs;
  end;

  PAbsoluteIndirectJmp = ^TAbsoluteIndirectJmp;
  TAbsoluteIndirectJmp = packed record
    OpCode: Word;
    Addr: PPointer;
  end;

function  GetActualAddr(Proc: Pointer): Pointer;
function  GetVirtualMethod(AClass: TClass; const VmtOffset: Integer): Pointer;
procedure SetVirtualMethod(AClass: TClass; const VmtOffset: Integer; const Method: Pointer);
procedure HookProc(Proc, Dest: Pointer; var BackupCode: TXRedirCode);
procedure UnhookProc(Proc: Pointer; var BackupCode: TXRedirCode);

implementation

uses
 SysUtils,
 Windows;

function GetActualAddr(Proc: Pointer): Pointer;
begin
  if Proc <> nil then
  begin
    if (Win32Platform = VER_PLATFORM_WIN32_NT) and (PAbsoluteIndirectJmp(Proc).OpCode = $25FF) then
      Result := PAbsoluteIndirectJmp(Proc).Addr^
    else
      Result := Proc;
  end
  else
    Result := nil;
end;

function GetVirtualMethod(AClass: TClass; const VmtOffset: Integer): Pointer;
begin
  Result := PPointer(Integer(AClass) + VmtOffset)^;
end;

procedure SetVirtualMethod(AClass: TClass; const VmtOffset: Integer; const Method: Pointer);
var
  WrittenBytes: {$IF CompilerVersion >= 23} NativeUInt {$ELSE} Cardinal {$IFEND};

  PatchAddress: PPointer;
begin
  PatchAddress := Pointer(Integer(AClass) + VmtOffset);
  WriteProcessMemory(GetCurrentProcess, PatchAddress, @Method, SizeOf(Method), WrittenBytes);
end;

procedure HookProc(Proc, Dest: Pointer; var BackupCode: TXRedirCode);
var
  n: {$IF CompilerVersion >= 23} NativeUInt {$ELSE} Cardinal {$IFEND};
  Code: TXRedirCode;
begin
  Proc := GetActualAddr(Proc);
  Assert(Proc <> nil);
  if ReadProcessMemory(GetCurrentProcess, Proc, @BackupCode, SizeOf(BackupCode), n) then
  begin
    Code.Jump := $E9;
    Code.Offset := PAnsiChar(Dest) - PAnsiChar(Proc) - SizeOf(Code);
    WriteProcessMemory(GetCurrentProcess, Proc, @Code, SizeOf(Code), n);
  end;
end;

procedure UnhookProc(Proc: Pointer; var BackupCode: TXRedirCode);
var
  n: {$IF CompilerVersion >= 23} NativeUInt {$ELSE} Cardinal {$IFEND};
begin
  if (BackupCode.Jump <> 0) and (Proc <> nil) then
  begin
    Proc := GetActualAddr(Proc);
    Assert(Proc <> nil);
    WriteProcessMemory(GetCurrentProcess, Proc, @BackupCode, SizeOf(BackupCode), n);
    BackupCode.Jump := 0;
  end;
end;


end.
