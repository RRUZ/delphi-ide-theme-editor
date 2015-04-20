//**************************************************************************************************
//
// Unit Colorizer.Hooks.GDIPOBJ
// unit Colorizer.Hooks.GDIPOBJ for the Delphi IDE Colorizer
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is Colorizer.Hooks.GDIPOBJ.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2015 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

unit Colorizer.Hooks.GDIPOBJ;

interface
{$I ..\Common\Jedi.inc}

  procedure InstallHooksGDI;
  procedure RemoveHooksGDI;

implementation

uses
    Colorizer.Utils,
    JclDebug,
    DDetours,
    Graphics,
    SysUtils,
    uMisc,
    GDIPAPI,
    GDIPOBJ;

{
    __fastcall Winapi::Gdipobj::initialization()
    __fastcall Winapi::Gdipobj::Finalization()
    __fastcall Winapi::Gdipobj::TGPGraphicsPath::AddCurve(TGPPoint *, int)
    __fastcall Winapi::Gdipobj::TGPGraphicsPath::AddLine(int, int, int, int)
    __fastcall Winapi::Gdipobj::TGPGraphicsPath::Reset()
    __fastcall Winapi::Gdipobj::TGPGraphicsPath::TGPGraphicsPath(FillMode)
    __fastcall Winapi::Gdipobj::TGPGraphics::FillPath(Winapi::Gdipobj::TGPBrush *, Winapi::Gdipobj::TGPGraphicsPath *)
    __fastcall Winapi::Gdipobj::TGPGraphics::DrawPath(Winapi::Gdipobj::TGPPen *, Winapi::Gdipobj::TGPGraphicsPath *)
    __fastcall Winapi::Gdipobj::TGPGraphics::TGPGraphics(HDC__ *)
    __fastcall Winapi::Gdipobj::TGPLinearGradientBrush::TGPLinearGradientBrush(TGPPoint&, TGPPoint&, unsigned int, unsigned int)
    __fastcall Winapi::Gdipobj::TGPPen::TGPPen(unsigned int, float)
    __tpdsc__ Winapi::Gdipobj::TGPGraphics
    Winapi::Gdipobj::TGPGraphics::
    __tpdsc__ Winapi::Gdipobj::TGPGraphicsPath
    Winapi::Gdipobj::TGPGraphicsPath::
    __tpdsc__ Winapi::Gdipobj::TGPPen
    Winapi::Gdipobj::TGPPen::
    __tpdsc__ Winapi::Gdipobj::TGPLinearGradientBrush
    Winapi::Gdipobj::TGPLinearGradientBrush::

    00104A1C 5198 07A6 __fastcall Gdiplus::Gradientdrawer::TGradientTabDrawer::TGradientTabDrawer(const Vcl::Graphics::TCanvas * const)
    00104A6C 5197 07A7 __fastcall Gdiplus::Gradientdrawer::TGradientTabDrawer::~TGradientTabDrawer()
    001B0C20 5206 07A8 Gdiplus::Gradientdrawer::TGradientTabDrawer::CLeftTextOffset
    00104AC0 5196 07A9 __fastcall Gdiplus::Gradientdrawer::TGradientTabDrawer::CreateGradientBrush(System::Uitypes::TColor, System::Uitypes::TColor)
    00104B64 5195 07AA __fastcall Gdiplus::Gradientdrawer::TGradientTabDrawer::CreateGraphics()
    001051BC 5179 07AB __fastcall Gdiplus::Gradientdrawer::TGradientTabDrawer::DrawActiveTab(const const int, const const int, const System::WideString, const const int, const const int)
    00104E4C 5184 07AC __fastcall Gdiplus::Gradientdrawer::TGradientTabDrawer::DrawCurve(const unsigned char *, const const int, const unsigned char *, const const int, int, int)
    00104F80 5182 07AD __fastcall Gdiplus::Gradientdrawer::TGradientTabDrawer::DrawImage(int&, const const int, const const int)
    001052FC 5178 07AE __fastcall Gdiplus::Gradientdrawer::TGradientTabDrawer::DrawInactiveTab(const const int, const const int, const System::WideString, const const int, const const int)
    00104F00 5183 07AF __fastcall Gdiplus::Gradientdrawer::TGradientTabDrawer::DrawTabContents(const Winapi::Gdipobj::TGPLinearGradientBrush * const, const System::WideString, const const int, const const int, const const int, const const int)
    001050B0 5180 07B0 __fastcall Gdiplus::Gradientdrawer::TGradientTabDrawer::DrawTabOutline(const const int, const const int)
    00104FE8 5181 07B1 __fastcall Gdiplus::Gradientdrawer::TGradientTabDrawer::DrawText(const const int, const const int, const const int, const System::WideString)
    00104BFC 5193 07B2 __fastcall Gdiplus::Gradientdrawer::TGradientTabDrawer::GetActiveGradientBrush()
    00104C68 5191 07B3 __fastcall Gdiplus::Gradientdrawer::TGradientTabDrawer::GetGraphicsPath()
    00104C30 5192 07B4 __fastcall Gdiplus::Gradientdrawer::TGradientTabDrawer::GetInActiveGradientBrush()
    00104BA8 5194 07B5 __fastcall Gdiplus::Gradientdrawer::TGradientTabDrawer::GetOutlinePen()
    00104C88 5190 07B6 __fastcall Gdiplus::Gradientdrawer::TGradientTabDrawer::GetTextWidth(const System::WideString)
    00104CDC 5189 07B7 __fastcall Gdiplus::Gradientdrawer::TGradientTabDrawer::GetTextYPos()
    00104D2C 5188 07B8 __fastcall Gdiplus::Gradientdrawer::TGradientTabDrawer::ResetColors()
    00104DA4 5187 07B9 __fastcall Gdiplus::Gradientdrawer::TGradientTabDrawer::SetDrawingOffsets(const const int, const const int, const const int)
    00104DD0 5186 07BA __fastcall Gdiplus::Gradientdrawer::TGradientTabDrawer::SetTabColors(const Gdiplus::Gradientdrawer::TTabColors * const)
    00104DE8 5185 07BB __fastcall Gdiplus::Gradientdrawer::TGradientTabDrawer::TabColorsChanged(System::TObject *)
}


var
   Trampoline_TGPGraphics_DrawPath : function (Self : TGPGraphics; pen: TGPPen; path: TGPGraphicsPath): TStatus = nil;
   Trampoline_TGPGraphics_FillPath : function (Self : TGPGraphics; brush: TGPBrush; path: TGPGraphicsPath): TStatus = nil;

function Detour_TGPGraphics_DrawPath(Self : TGPGraphics; pen: TGPPen; path: TGPGraphicsPath): TStatus;
const
 sTGradientTabSet= 'GDIPlus.GradientTabs.TGradientTabSet.DrawTabsToMemoryBitmap';
var
 sCaller : string;
 PenColor, LGPColor: TGPColor;
begin
  //AddLog2('Detour_TGPGraphics_DrawPath', '1');
  if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and (pen<>nil) then
  begin
    pen.GetColor(PenColor);
    LGPColor := ColorRefToARGB(ColorToRGB(TColorizerLocalSettings.ColorMap.FrameTopLeftOuter));
    if TColorizerLocalSettings.Settings.TabIDECustom then
    LGPColor  :=  ColorRefToARGB(ColorToRGB(TryStrToColor(TColorizerLocalSettings.Settings.TabIDEOutLineColor, TColorizerLocalSettings.ColorMap.FrameTopLeftOuter)));
    if PenColor<>LGPColor then
    begin
     sCaller  := ProcByLevel(3);
     //AddLog2('Detour_TGPGraphics_DrawPath sCaller', sCaller);
     if SameText(sTGradientTabSet, sCaller) then
      pen.SetColor(LGPColor);
    end;
  end;

  Result:=Trampoline_TGPGraphics_DrawPath(Self, pen, path);
  //AddLog('Detour_TGPGraphics_DrawPath', '2');
end;

function Detour_TGPGraphics_FillPath(Self : TGPGraphics; brush: TGPBrush; path: TGPGraphicsPath): TStatus;
const
 sTGradientTabSet= 'GDIPlus.GradientTabs.TGradientTabSet.DrawTabsToMemoryBitmap';
var
 sCaller : string;
 color1, color2: TGPColor;
 LActive  : Boolean;
begin                                                                                                                                               //improve performance
  //AddLog2('Detour_TGPGraphics_FillPath', '1');
  if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and TColorizerLocalSettings.Settings.TabIDEOutLine and (brush is TGPLinearGradientBrush)  then
  begin
    sCaller  := ProcByLevel(3);
    //AddLog2('Detour_TGPGraphics_FillPath sCaller', sCaller);
    if SameText(sTGradientTabSet, sCaller)  then
    begin
      TGPLinearGradientBrush(brush).GetLinearColors(color1, color2);
      //use colors from tabs to determine when tab is active
      {
        TRttiUtils.SetRttiPropertyValue(AComponent,'TabColors.ActiveStart', AColorMap.Color);
        TRttiUtils.SetRttiPropertyValue(AComponent,'TabColors.ActiveEnd', AColorMap.Color);
        TRttiUtils.SetRttiPropertyValue(AComponent,'TabColors.InActiveStart', AColorMap.MenuColor);
        TRttiUtils.SetRttiPropertyValue(AComponent,'TabColors.InActiveEnd', AColorMap.MenuColor);
      }
      //AddLog('Detour_TGPGraphics_FillPath', Format('Original Color1 %s Color2 %s', [ColorToString(TColor(ARGBToColorRef(color1))), ColorToString(TColor(ARGBToColorRef(color2)))]));
      if not TColorizerLocalSettings.Settings.TabIDECustom then
       LActive :=  (TColor(ARGBToColorRef(color1)) =  TColorizerLocalSettings.ColorMap.Color)
      else
       LActive :=  (TColor(ARGBToColorRef(color1)) =  TryStrToColor(TColorizerLocalSettings.Settings.TabIDEStartGradActive, clNone));

      if LActive then
      begin
        color1  :=  ColorRefToARGB(ColorToRGB(TColorizerLocalSettings.ColorMap.Color));
        color2  :=  ColorRefToARGB(ColorToRGB(TColorizerLocalSettings.ColorMap.SelectedColor));
      end
      else
      begin
        color1  :=  ColorRefToARGB(ColorToRGB(TColorizerLocalSettings.ColorMap.MenuColor));
        color2  :=  ColorRefToARGB(ColorToRGB(TColorizerLocalSettings.ColorMap.SelectedColor));
      end;

      if TColorizerLocalSettings.Settings.TabIDECustom then
      begin
        if LActive then
        begin
          try
            color1  :=  ColorRefToARGB(ColorToRGB(TryStrToColor(TColorizerLocalSettings.Settings.TabIDEStartGradActive, TColorizerLocalSettings.ColorMap.Color)));
          except
            color1   :=  ColorRefToARGB(ColorToRGB(TColorizerLocalSettings.ColorMap.Color));
          end;
          try
           color2  :=  ColorRefToARGB(ColorToRGB(TryStrToColor(TColorizerLocalSettings.Settings.TabIDEEndGradActive, TColorizerLocalSettings.ColorMap.SelectedColor)));
          except
           color2  :=  ColorRefToARGB(ColorToRGB(TColorizerLocalSettings.ColorMap.SelectedColor));
          end;
        end
        else
        begin
          color1  :=  ColorRefToARGB(ColorToRGB(TryStrToColor(TColorizerLocalSettings.Settings.TabIDEStartGradInActive, TColorizerLocalSettings.ColorMap.MenuColor)));
          color2  :=  ColorRefToARGB(ColorToRGB(TryStrToColor(TColorizerLocalSettings.Settings.TabIDEEndGradInActive, TColorizerLocalSettings.ColorMap.SelectedColor)));
        end;
      end;

      TGPLinearGradientBrush(brush).SetLinearColors(color1, color2);
    end;
  end;
  Result:=Trampoline_TGPGraphics_FillPath(Self, brush, path);
  //AddLog('Detour_TGPGraphics_FillPath', '2');
end;


procedure InstallHooksGDI;
begin
  Trampoline_TGPGraphics_DrawPath            := InterceptCreate(@TGPGraphics.DrawPath, @Detour_TGPGraphics_DrawPath);
  Trampoline_TGPGraphics_FillPath            := InterceptCreate(@TGPGraphics.FillPath, @Detour_TGPGraphics_FillPath);
end;

procedure RemoveHooksGDI;
begin
  InterceptRemove(@Trampoline_TGPGraphics_DrawPath);
  InterceptRemove(@Trampoline_TGPGraphics_FillPath);
end;
end.
