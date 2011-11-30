{**************************************************************************************************}
{                                                                                                  }
{ Unit uVclControlsHooks                                                                           }
{ unit uVclControlsHooks  for the Delphi IDE Colorizer                                             }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is uVclControlsHooks.pas.                                                      }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2011 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}

unit uVclControlsHooks;

interface

uses
  Windows,
  Classes,
  SysUtils,
  Graphics,
  ImgList,
  CommCtrl,
  Themes,
  uHookLib;

implementation

type
  TCustomImageListHack = class(TCustomImageList);
  //TCustomActionBarHack  = class(TCustomActionBar);

var
  //BackupThemeControl                  : TXRedirCode;
  BackupCustomImage_DoDraw            : TXRedirCode;
  //BackupCustomActionBar_GetColorMap   : TXRedirCode;

{$IFDEF Hack_TPageControl}
  OrgTTabSheet_NewInstance            : Pointer;
  OrgTPageControl_NewInstance         : Pointer;
{$ENDIF}


procedure Bitmap2GrayScale(const BitMap: TBitmap);
type
  TRGBArray = array[0..32767] of TRGBTriple;
  PRGBArray = ^TRGBArray;
var
  x, y, Gray: Integer;
  Row       : PRGBArray;
begin
  BitMap.PixelFormat := pf24Bit;
  for y := 0 to BitMap.Height - 1 do
  begin
    Row := BitMap.ScanLine[y];
    for x := 0 to BitMap.Width - 1 do
    begin
      Gray             := (Row[x].rgbtRed + Row[x].rgbtGreen + Row[x].rgbtBlue) div 3;
      Row[x].rgbtRed   := Gray;
      Row[x].rgbtGreen := Gray;
      Row[x].rgbtBlue  := Gray;
    end;
  end;
end;

//from ImgList.GetRGBColor
function GetRGBColor(Value: TColor): DWORD;
begin
  Result := ColorToRGB(Value);
  case Result of
    clNone:
      Result := CLR_NONE;
    clDefault:
      Result := CLR_DEFAULT;
  end;
end;

procedure CustomImageListHack_DoDraw(Self: TObject; Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean);
var
  MaskBitMap : TBitmap;
  GrayBitMap : TBitmap;
begin
  with TCustomImageListHack(Self) do
  begin
    if not HandleAllocated then Exit;
    if Enabled then
      ImageList_DrawEx(Handle, Index, Canvas.Handle, X, Y, 0, 0, GetRGBColor(BkColor), GetRGBColor(BlendColor), Style)
    else
    begin
      GrayBitMap := TBitmap.Create;
      MaskBitMap := TBitmap.Create;
      try
        GrayBitMap.SetSize(Width, Height);
        MaskBitMap.SetSize(Width, Height);
        GetImages(Index, GrayBitMap, MaskBitMap);
        Bitmap2GrayScale(GrayBitMap);
        BitBlt(Canvas.Handle, X, Y, Width, Height, MaskBitMap.Canvas.Handle, 0, 0, SRCERASE);
        BitBlt(Canvas.Handle, X, Y, Width, Height, GrayBitMap.Canvas.Handle, 0, 0, SRCINVERT);
      finally
        GrayBitMap.Free;
        MaskBitMap.Free;
      end;
    end;
  end;
end;


//Let draws the TGroupBox,TRadioGroup,TPropRadioGroup Flat, unthemed
{
function Hack_ThemeControl(AControl: TControl): Boolean;
begin
  Result := False;
  if not (csDesigning in AControl.ComponentState)  then   exit;
  if AControl = nil then exit;
  Result := (not (csDesigning in AControl.ComponentState) and ThemeServices.ThemesEnabled) or
            ((csDesigning in AControl.ComponentState) and (AControl.Parent <> nil) and
             (ThemeServices.ThemesEnabled and not UnthemedDesigner(AControl.Parent)));
end;
}

procedure InstallHooks;
{$IFOPT W+}{$DEFINE WARN}{$ENDIF}{$WARNINGS OFF} // no compiler warning
const
  vmtNewInstance = System.vmtNewInstance;
{$IFDEF WARN}{$WARNINGS ON}{$ENDIF}
begin
  HookProc(@TCustomImageListHack.DoDraw, @CustomImageListHack_DoDraw, BackupCustomImage_DoDraw);
  //HookProc(@Themes.ThemeControl, @Hack_ThemeControl, BackupThemeControl);
  //HookProc(@TCustomActionBarHack.GetColorMap, @Hack_ThemeControl, BackupThemeControl);

{$IFDEF Hack_TPageControl}
  OrgTTabSheet_NewInstance := GetVirtualMethod(TTabSheet, vmtNewInstance);
  SetVirtualMethod(ComCtrls.TTabSheet, vmtNewInstance, @TTabSheet_NewInstance);

  OrgTPageControl_NewInstance := GetVirtualMethod(TPageControl, vmtNewInstance);
  SetVirtualMethod(ComCtrls.TPageControl, vmtNewInstance, @TPageControl_NewInstance);
{$ENDIF}
end;

procedure RemoveHooks;
{$IFOPT W+}{$DEFINE WARN}{$ENDIF}{$WARNINGS OFF} // no compiler warning
const
  vmtNewInstance = System.vmtNewInstance;
{$IFDEF WARN}{$WARNINGS ON}{$ENDIF}
begin
  UnhookProc(@TCustomImageListHack.DoDraw, BackupCustomImage_DoDraw);
  //UnhookProc(@Themes.ThemeControl, BackupThemeControl);

{$IFDEF Hack_TPageControl}
  SetVirtualMethod(ComCtrls.TTabSheet, vmtNewInstance, OrgTTabSheet_NewInstance);
  SetVirtualMethod(ComCtrls.TPageControl, vmtNewInstance, OrgTPageControl_NewInstance);
{$ENDIF}
end;


initialization
 InstallHooks;
finalization
 RemoveHooks;
end.

