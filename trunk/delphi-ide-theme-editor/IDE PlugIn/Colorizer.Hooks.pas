//**************************************************************************************************
//
// Unit Colorizer.Hooks
// unit Colorizer.Hooks for the Delphi IDE Colorizer
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is Colorizer.Hooks.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

unit Colorizer.Hooks;

interface

uses
{$IF CompilerVersion >= 23}
  Vcl.Styles,
  Vcl.Themes,
  Messages,
  Controls,
{$IFEND}
  System.IOUtils,
  ExtCtrls,
  Dialogs,
  Windows,
  Classes,
  uDelphiVersions,
  uDelphiIDEHighlight,
  SysUtils,
  ComCtrls,
  Graphics,
  ImgList,
  CommCtrl,
  JclDebug,
  Colorizer.Utils,
  DDetours;



implementation

type
 TWinControlClass      = class(TWinControl);
 TCustomPanelClass     = class(TCustomPanel);
 TCustomStatusBarClass = class(TCustomStatusBar);
var
  TrampolineCustomImageList_DoDraw     : procedure(Self: TObject; Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean) = nil;
  Trampoline_TCanvas_FillRect          : procedure(Self: TCanvas;const Rect: TRect) = nil;
  Trampoline_TStyleEngine_HandleMessage: function(Self: TStyleEngine; Control: TWinControl; var Message: TMessage; DefWndProc: TWndMethod): Boolean = nil;
  Trampoline_TCustomStatusBar_WMPAINT  : procedure(Self: TCustomStatusBarClass; var Message: TWMPaint) = nil;

  FGutterBkColor : TColor = clNone;

type
  TCustomStatusBarHelper = class helper for TCustomStatusBar
  private
    function GetCanvasRW: TCanvas;
    procedure SetCanvasRW(const Value: TCanvas);
  public
    function  WMPaintAddress: Pointer;
    procedure DoUpdatePanels(UpdateRects, UpdateText: Boolean);
    property  CanvasRW : TCanvas read GetCanvasRW Write SetCanvasRW;
   end;



{ TCustomStatusBarHelper }

procedure TCustomStatusBarHelper.DoUpdatePanels(UpdateRects,
  UpdateText: Boolean);
begin
  Self.UpdatePanels(UpdateRects, UpdateText);
end;

function TCustomStatusBarHelper.GetCanvasRW: TCanvas;
begin
 Result:= Self.FCanvas;
end;

procedure TCustomStatusBarHelper.SetCanvasRW(const Value: TCanvas);
begin
 Self.FCanvas:= Value;
end;

function TCustomStatusBarHelper.WMPaintAddress: Pointer;
var
  MethodAddr: procedure(var Message: TWMPaint) of object;
begin
  MethodAddr := Self.WMPaint;
  Result     := TMethod(MethodAddr).Code;
end;

 //@Editorcontrol@TCustomEditControl@EVFillGutter$qqrrx18
 //002F0A00 11656 219E __fastcall Editorcontrol::TCustomEditControl::EVFillGutter(System::Types::TRect&, unsigned short, int, bool, int)
//  Trampoline_EVFillGutter              : procedure(Self: TObject;ARect:TRect; p1 : USHORT; p2 : Integer; p3 :Boolean; p4 : Integer);
//  Addr_EVFillGutter                    : Pointer;
//Const
  //FooMethod='@Editcolorpage@TEditorColor@SetColorSpeedSetting$qqr26Vedopts@TColorSpeedSetting';
  //FooMethod='@Editcolorpage@TEditorColor@ColorSpeedSettingClick$qqrp14System@TObject';


//function GetBplMethodAddress(Method: Pointer): Pointer;
//type
//  PJmpCode = ^TJmpCode;
//  TJmpCode = packed record
//    Code: Word;
//    Addr: ^Pointer;
//  end;
//const
//  csJmp32Code = $25FF;
//begin
//  if PJmpCode(Method)^.Code = csJmp32Code then
//    Result := PJmpCode(Method)^.Addr^
//  else
//    Result := Method;
//end;

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

function GetRGBColor(Value: TColor): DWORD;
begin
  Result := ColorToRGB(Value);
  case Result of
    clNone    :  Result := CLR_NONE;
    clDefault :  Result := CLR_DEFAULT;
  end;
end;

type
  TCustomImageListClass = class(TCustomImageList);

procedure CustomImageListHack_DoDraw(Self: TObject; Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean);
var
  MaskBitMap : TBitmap;
  GrayBitMap : TBitmap;
begin
  with TCustomImageListClass(Self) do
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

function GetGutterBkColor : TColor;
var
  ATheme : TIDETheme;
  sColor : string;
begin
  if FGutterBkColor<>clNone then
   Result:=FGutterBkColor
  else
  begin
    if Assigned(TColorizerLocalSettings.IDEData) then
    begin
      ImportDelphiIDEThemeFromReg(ATheme, TColorizerLocalSettings.IDEData.Version, False);
      sColor:=ATheme[LineNumber].BackgroundColorNew;
      try
        Result:=StringToColor(sColor);
      except
        Result:=clBtnFace;
      end;
      FGutterBkColor:=Result;
    end
    else
    if Assigned(TColorizerLocalSettings.ColorMap) then
      Result:=TColorizerLocalSettings.ColorMap.Color
    else
      Result:=clBtnFace
  end;
end;


procedure  CustomFillRect(Self: TCanvas;const Rect: TRect);
var
  sCaller : string;
begin
   if Assigned(TColorizerLocalSettings.ColorMap) and  (Self.Brush.Color=clBtnFace) then
   begin
     sCaller := ProcByLevel(1);
     if SameText(sCaller, 'EditorControl.TCustomEditControl.EVFillGutter') then
        Self.Brush.Color:=GetGutterBkColor
     else
      if SameText(sCaller, 'GDIPlus.GradientTabs.TGradientTabSet.DrawTabsToMemoryBitmap') then
        Self.Brush.Color:=TColorizerLocalSettings.ColorMap.Color;
   end;
   Trampoline_TCanvas_FillRect(Self, Rect);
end;



function CustomHandleMessage(Self: TStyleEngine; Control: TWinControl; var Message: TMessage; DefWndProc: TWndMethod): Boolean;
begin
  if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.UseVCLStyles then
  begin
    Result:=False;
    if not Assigned(Control) then exit;
    if csDesigning in Control.ComponentState then  exit;
  end;
  Result:=Trampoline_TStyleEngine_HandleMessage(Self, Control, Message, DefWndProc);
end;

procedure CustomStatusBarWMPaint(Self: TCustomStatusBarClass; var Message: TWMPaint);
var
  DC: HDC;
  Buffer: TBitmap;
  LCanvas: TCanvas;
  PS: TPaintStruct;

      procedure DrawControlText(Canvas: TCanvas; Details: TThemedElementDetails;
        const S: string; var R: TRect; Flags: Cardinal);
      var
        ThemeTextColor: TColor;
        TextFormat: TTextFormatFlags;
      begin
        Canvas.Font := TWinControlClass(Self).Font;
        TextFormat := TTextFormatFlags(Flags);
        if StyleServices.GetElementColor(Details, ecTextColor, ThemeTextColor) then
        begin
          if not Self.Enabled or (seFont in Self.StyleElements) then
            Canvas.Font.Color := ThemeTextColor;
          StyleServices.DrawText(Canvas.Handle, Details, S, R, TextFormat, Canvas.Font.Color);
        end
        else
        begin
          Canvas.Refresh;
          StyleServices.DrawText(Canvas.Handle, Details, S, R, TextFormat);
        end;
      end;

      procedure Paint(Canvas : TCanvas);
      const
        AlignStyles: array [TAlignment] of Integer = (DT_LEFT, DT_RIGHT, DT_CENTER);
      var
        R : TRect;
        Res, Count, I: Integer;
        Idx, Flags: Cardinal;
        Details: TThemedElementDetails;
        LText: string;
        Borders: array [0..2] of Integer;
        SaveCanvas: TCanvas;
      begin
        if not StyleServices.Available then
          Exit;

        Details := StyleServices.GetElementDetails(tsStatusRoot);
        //StyleServices.DrawElement(Canvas.Handle, Details, Rect(0, 0, Self.Width, Self.Height));
        Canvas.Brush.Color := TColorizerLocalSettings.ColorMap.Color;
        Canvas.FillRect(Rect(0, 0, Self.Width, Self.Height));


        if SendMessage(Self.Handle, SB_ISSIMPLE, 0, 0) > 0 then
        begin
          R := Self.ClientRect;
          FillChar(Borders, SizeOf(Borders), 0);
          SendMessage(Self.Handle, SB_GETBORDERS, 0, IntPtr(@Borders));
          R.Left := Borders[0] + Borders[2];
          R.Top := Borders[1];
          R.Bottom := R.Bottom - Borders[1];
          R.Right := R.Right - Borders[2];

          Canvas.Brush.Color := TColorizerLocalSettings.ColorMap.Color;
          Canvas.FillRect(R);

          //R1 := Self.ClientRect;
          //R1.Left := R1.Right - R.Height;
          //Details := StyleServices.GetElementDetails(tsGripper);
          //StyleServices.DrawElement(Canvas.Handle, Details, R1);

          Details := StyleServices.GetElementDetails(tsPane);
          SetLength(LText, Word(SendMessage(Self.Handle, SB_GETTEXTLENGTH, 0, 0)));
          if Length(LText) > 0 then
          begin
           SendMessage(Self.Handle, SB_GETTEXT, 0, IntPtr(@LText[1]));
           Flags := Self.DrawTextBiDiModeFlags(DT_LEFT);
           DrawControlText(Canvas, Details, LText, R, Flags);
          end;
        end
        else
        begin
          Count := Self.Panels.Count;
          for I := 0 to Count - 1 do
          begin
            R := Rect(0, 0, 0, 0);
            SendMessage(Self.Handle, SB_GETRECT, I, IntPtr(@R));
            if IsRectEmpty(R) then
              Exit;

            Canvas.Brush.Color := TColorizerLocalSettings.ColorMap.HighlightColor;
            Canvas.FillRect(R);

//            if I = Count - 1 then
//            begin
//              R1 := Self.ClientRect;
//              R1.Left := R1.Right - R.Height;
//              Details := StyleServices.GetElementDetails(tsGripper);
//              StyleServices.DrawElement(Canvas.Handle, Details, R1);
//            end;
            Details := StyleServices.GetElementDetails(tsPane);
            InflateRect(R, -1, -1);
            if Self is TCustomStatusBar then
              Flags := Self.DrawTextBiDiModeFlags(AlignStyles[TCustomStatusBar(Self).Panels[I].Alignment])
            else
              Flags := Self.DrawTextBiDiModeFlags(DT_LEFT);
            Idx := I;
            SetLength(LText, Word(SendMessage(Self.Handle, SB_GETTEXTLENGTH, Idx, 0)));
            if Length(LText) > 0 then
            begin
              Res := SendMessage(Self.Handle, SB_GETTEXT, Idx, IntPtr(@LText[1]));
              if (Res and SBT_OWNERDRAW = 0) then
                DrawControlText(Canvas, Details, LText, R, Flags)
              else
              if (Self is TCustomStatusBar) and Assigned(TCustomStatusBar(Self).OnDrawPanel) then
              begin
                SaveCanvas  := Self.Canvas;
                Self.CanvasRW := Canvas;
                try
                  Self.OnDrawPanel(TCustomStatusBar(Self), Self.Panels[I], R);
                finally
                  Self.CanvasRW := SaveCanvas;
                end;
              end;
            end
            else if (Self is TCustomStatusBar) then
             if (TCustomStatusBar(Self).Panels[I].Style <> psOwnerDraw) then
               DrawControlText(Canvas, Details, TCustomStatusBar(Self).Panels[I].Text, R, Flags)
             else
               if Assigned(TCustomStatusBar(Self).OnDrawPanel) then
               begin
                 SaveCanvas := TCustomStatusBar(Self).Canvas;
                 TCustomStatusBar(Self).CanvasRW := Canvas;
                 try
                   TCustomStatusBar(Self).OnDrawPanel(TCustomStatusBar(Self), TCustomStatusBar(Self).Panels[I], R);
                 finally
                   TCustomStatusBar(Self).CanvasRW := SaveCanvas;
                 end;
               end;
          end;
        end;

      end;

begin
    if (not Assigned(TColorizerLocalSettings.ColorMap)) or (Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.UseVCLStyles) then
    begin
     Trampoline_TCustomStatusBar_WMPAINT(Self, Message);
     exit;
    end;

    Self.DoUpdatePanels(False, True);

    DC := HDC(Message.DC);
    LCanvas := TCanvas.Create;
    try
        if DC <> 0 then
          LCanvas.Handle := DC
        else
        LCanvas.Handle := BeginPaint(Self.Handle, PS);
        if (DC = 0) then
        begin
          Buffer := TBitmap.Create;
          try
            Buffer.SetSize(Self.Width, Self.Height);
            LCanvas.Brush.Color := TColorizerLocalSettings.ColorMap.Color;
            LCanvas.FillRect(Self.ClientRect);
            Paint(Buffer.Canvas);
            // paint other controls
            if Self is TWinControl then
              TWinControlClass(Self).PaintControls(Buffer.Canvas.Handle, nil);
            LCanvas.Draw(0, 0, Buffer);
          finally
            Buffer.Free;
          end;
        end;

      if DC = 0 then
        EndPaint(Self.Handle, PS);
    finally
      LCanvas.Handle := 0;
      LCanvas.Free;
    end;

end;



//const
// sEVFillGutter ='@Editorcontrol@TCustomEditControl@EVFillGutter$qqrr';
procedure InstallColorizerHooks;
begin
  TrampolineCustomImageList_DoDraw:=InterceptCreate(@TCustomImageListClass.DoDraw, @CustomImageListHack_DoDraw);
  Trampoline_TCanvas_FillRect     :=InterceptCreate(@TCanvas.FillRect, @CustomFillRect);
  Trampoline_TStyleEngine_HandleMessage := InterceptCreate(@TStyleEngine.HandleMessage,   @CustomHandleMessage);
  Trampoline_TCustomStatusBar_WMPAINT   := InterceptCreate(TCustomStatusBarClass(nil).WMPaintAddress,   @CustomStatusBarWMPaint);
end;

procedure RemoveColorizerHooks;
begin
  if Assigned(TrampolineCustomImageList_DoDraw) then
    InterceptRemove(@TrampolineCustomImageList_DoDraw);
  if Assigned(Trampoline_TCanvas_FillRect) then
    InterceptRemove(@Trampoline_TCanvas_FillRect);
  if Assigned(Trampoline_TStyleEngine_HandleMessage) then
    InterceptRemove(@Trampoline_TStyleEngine_HandleMessage);
  if Assigned(Trampoline_TCustomStatusBar_WMPAINT) then
    InterceptRemove(@Trampoline_TCustomStatusBar_WMPAINT);

end;

{
    004228A4 17391 1F38 __fastcall Editcolorpage::Finalization()
    00420B94 17411 1F39 Editcolorpage::TEditorColor::
    00422188 17400 1F3A __fastcall Editcolorpage::TEditorColor::ColorClick(System::TObject *)
    0042174C 17407 1F3B __fastcall Editcolorpage::TEditorColor::ColorSpeedSettingClick(System::TObject *)
    004224BC 17396 1F3C __fastcall Editcolorpage::TEditorColor::DefaultClick(System::TObject *)
    00422414 17397 1F3D __fastcall Editcolorpage::TEditorColor::EditorColorBroadcast(System::TObject *, Winapi::Messages::TMessage&)
    00421584 17409 1F3E __fastcall Editcolorpage::TEditorColor::EditorColorCreate(System::TObject *)
    00421730 17408 1F3F __fastcall Editcolorpage::TEditorColor::EditorColorDestroy(System::TObject *)
    004217B0 17406 1F40 __fastcall Editcolorpage::TEditorColor::ElementListClick(System::TObject *)
    004222E8 17399 1F41 __fastcall Editcolorpage::TEditorColor::FontClick(System::TObject *)
    004225DC 17395 1F42 __fastcall Editcolorpage::TEditorColor::HelpClick(System::TObject *)
    00421AE8 17404 1F43 __fastcall Editcolorpage::TEditorColor::InitLineFlags(const System::DelphiInterface<Toolsapi::IOTAHighlighterPreview>)
    004219B8 17405 1F44 __fastcall Editcolorpage::TEditorColor::InitSamplePane()
    00421BC8 17403 1F45 __fastcall Editcolorpage::TEditorColor::InitSyntaxEditView(const System::DelphiInterface<Toolsapi::IOTAHighlighterPreview>)
    0042262C 17393 1F46 __fastcall Editcolorpage::TEditorColor::LoadHighlightPreviews()
    004225F4 17394 1F47 __fastcall Editcolorpage::TEditorColor::MarkDirty()
    004220E4 17401 1F48 __fastcall Editcolorpage::TEditorColor::SampleClick(System::TObject *)
    00422080 17402 1F49 __fastcall Editcolorpage::TEditorColor::SetColorSpeedSetting(Vedopts::TColorSpeedSetting)
    0042238C 17398 1F4A __fastcall Editcolorpage::TEditorColor::UpdateSamplePane()
    00422814 17392 1F4B __fastcall Editcolorpage::TEditorColor::tbsetPreviewsChange(System::TObject *, int, bool&)
    004AA8D4 17390 1F4C __fastcall Editcolorpage::initialization()

    002F0A00 11656 219E __fastcall Editorcontrol::TCustomEditControl::EVFillGutter(System::Types::TRect&, unsigned short, int, bool, int)
}

initialization
  InstallColorizerHooks;

finalization
  RemoveColorizerHooks;



end.

