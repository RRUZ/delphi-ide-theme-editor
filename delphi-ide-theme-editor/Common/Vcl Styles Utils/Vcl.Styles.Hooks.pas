// **************************************************************************************************
//
// Unit Vcl.Styles.Hooks
// unit for the VCL Styles Utils
// http://code.google.com/p/vcl-styles-utils/
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is Vcl.Styles.Hooks.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2013-2015 Rodrigo Ruz V.
// Contributor(s): Mahdi Safsafi.
// All Rights Reserved.
//
// **************************************************************************************************
unit Vcl.Styles.Hooks;

interface

uses
  WinApi.Windows;

var
  TrampolineGetSysColor: function(nIndex: Integer): DWORD; stdcall;

implementation

{$DEFINE HOOK_UXTHEME}
{$DEFINE HOOK_TDateTimePicker}
{$DEFINE HOOK_TProgressBar}

uses
  DDetours,
  System.SyncObjs,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Generics.Collections,
  System.StrUtils,
  WinApi.Messages,
  Vcl.Graphics,
{$IFDEF HOOK_UXTHEME}
  Vcl.Styles.UxTheme,
{$ENDIF HOOK_UXTHEME}
  Vcl.Styles.Utils.SysControls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Themes;

type
  TListStyleBrush = TObjectDictionary<Integer, HBRUSH>;
  TSetStyle = procedure(Style: TCustomStyleServices) of object;

var
  VCLStylesBrush: TObjectDictionary<string, TListStyleBrush>;
  VCLStylesLock: TCriticalSection = nil;
  LSetStylePtr: TSetStyle;

  TrampolineGetSysColorBrush: function(nIndex: Integer): HBRUSH; stdcall;
  TrampolineFillRect: function(hDC: hDC; const lprc: TRect; hbr: HBRUSH): Integer; stdcall;
  TrampolineDrawEdge: function(hDC: hDC; var qrc: TRect; edge: UINT; grfFlags: UINT): BOOL;
stdcall = nil;
TrampolineSetStyle:

procedure(Self: TObject; Style: TCustomStyleServices);

  function InterceptDrawEdge(hDC: hDC; var qrc: TRect; edge: UINT; grfFlags: UINT): BOOL; stdcall;
  var
    CanDraw: Boolean;
  begin
    CanDraw := (not StyleServices.IsSystemStyle) or (TSysStyleManager.Enabled);
    if (CanDraw) and (edge <> BDR_OUTER) and (edge <> BDR_INNER) then
    begin
      DrawStyleEdge(hDC, qrc, TStyleElementEdges(edge), TStyleElementEdgeFlags(grfFlags));
      Exit(True);
    end;
    Exit(TrampolineDrawEdge(hDC, qrc, edge, grfFlags));
  end;

  function InterceptFillRect(hDC: hDC; const lprc: TRect; hbr: HBRUSH): Integer; stdcall;
  begin
    if StyleServices.IsSystemStyle or not TSysStyleManager.Enabled then
      Exit(TrampolineFillRect(hDC, lprc, hbr))
    else if (hbr > 0) and (hbr < COLOR_ENDCOLORS + 1) then
      Exit(TrampolineFillRect(hDC, lprc, GetSysColorBrush(hbr - 1)))
    else
      Exit(TrampolineFillRect(hDC, lprc, hbr));
  end;

  function InterceptGetSysColor(nIndex: Integer): DWORD; stdcall;
  begin
    if StyleServices.IsSystemStyle or not TSysStyleManager.Enabled then
      Result := TrampolineGetSysColor(nIndex)
    else if nIndex = COLOR_HOTLIGHT then
      Result := DWORD(StyleServices.GetSystemColor(clHighlight))
    else
      Result := DWORD(StyleServices.GetSystemColor(TColor(nIndex or Integer($FF000000))));
  end;

  function InterceptGetSysColorBrush(nIndex: Integer): HBRUSH; stdcall;
  var
    LCurrentStyleBrush: TListStyleBrush;
    LBrush: HBRUSH;
    LColor: TColor;
  begin
    {
      The reason to change the previous code implementation
      is that the win32 graphics may differ with the VCL graphics:
      Eg: TColor is signed in VCL and Unsigned in Win32Api.
      When hooking : keep always using the native way !
      Need Color ?
      Use GetObject with LOGBRUSH ! or use TBrushColorPair !
    }
    VCLStylesLock.Enter;
    try
      if StyleServices.IsSystemStyle or not TSysStyleManager.Enabled then
        Exit(TrampolineGetSysColorBrush(nIndex))
      else
      begin
        if VCLStylesBrush.ContainsKey(StyleServices.Name) then
          LCurrentStyleBrush := VCLStylesBrush.Items[StyleServices.Name]
        else
        begin
          VCLStylesBrush.Add(StyleServices.Name, TListStyleBrush.Create());
          LCurrentStyleBrush := VCLStylesBrush.Items[StyleServices.Name];
        end;
        if Assigned(LCurrentStyleBrush) then
        begin
          if LCurrentStyleBrush.ContainsKey(nIndex) then
            Exit(LCurrentStyleBrush[nIndex])
          else
          begin
            LColor := StyleServices.GetSystemColor(TColor(nIndex or Integer($FF000000)));
            LBrush := CreateSolidBrush(LColor);
            LCurrentStyleBrush.Add(nIndex, LBrush);
            Exit(LBrush);
          end;
        end;
        Exit(TrampolineGetSysColorBrush(nIndex));
      end;
    finally
      VCLStylesLock.Leave;
    end;
  end;

  procedure InterceptSetStyle(Self: TObject; Style: TCustomStyleServices);
  var
    I: Integer;
    LActiveStyle: TCustomStyleServices;
  begin
    LActiveStyle := TStyleManager.ActiveStyle;
    TrampolineSetStyle(Self, Style);
    if (Style <> LActiveStyle) then
    begin
      for I := 0 to Screen.FormCount - 1 do
        if Screen.Forms[I].HandleAllocated then
          SendMessage(Screen.Forms[I].Handle, WM_SYSCOLORCHANGE, 0, 0);
    end;
  end;

initialization

VCLStylesLock  := TCriticalSection.Create;
VCLStylesBrush := TObjectDictionary<string, TListStyleBrush>.Create([doOwnsValues]);

if StyleServices.Available then
begin
{$IFDEF HOOK_TDateTimePicker}
  TCustomStyleEngine.RegisterStyleHook(TDateTimePicker, TStyleHook);
{$ENDIF HOOK_TDateTimePicker}
{$IFDEF HOOK_TProgressBar}
  TCustomStyleEngine.RegisterStyleHook(TProgressBar, TStyleHook);
{$ENDIF HOOK_TProgressBar}
  LSetStylePtr := TStyleManager.SetStyle;

  @TrampolineGetSysColor := InterceptCreate(user32, 'GetSysColor', @InterceptGetSysColor);
  @TrampolineGetSysColorBrush := InterceptCreate(user32, 'GetSysColorBrush', @InterceptGetSysColorBrush);
  @TrampolineFillRect := InterceptCreate(user32, 'FillRect', @InterceptFillRect);
  @TrampolineDrawEdge := InterceptCreate(user32, 'DrawEdge', @InterceptDrawEdge);
  @TrampolineSetStyle := InterceptCreate(@LSetStylePtr, @InterceptSetStyle);
end;

finalization

InterceptRemove(@TrampolineGetSysColor);
InterceptRemove(@TrampolineGetSysColorBrush);
InterceptRemove(@TrampolineFillRect);
InterceptRemove(@TrampolineDrawEdge);
InterceptRemove(@TrampolineSetStyle);

VCLStylesBrush.Free;
VCLStylesLock.Free;
VCLStylesLock := nil;

end.
