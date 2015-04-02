unit Colorizer.Vcl.Styles;

interface

uses
  Windows,
  Messages,
  Controls,
  Classes,
  Forms,
  Vcl.Themes,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  System.UITypes,
  System.Types,
  Graphics;


type
  TColorizerStyleHook = class
  strict private
    FBrush: TBrush;
    FControl: TWinControl;
    FCallDefaultProc: Boolean;
    FDoubleBuffered: Boolean;
    FFocused: Boolean;
    FFocusUpdate: Boolean;
    FFontColor: TColor;
    FHandled: Boolean;
    FNeedsDefaultPaint: Boolean;
    FOverrideEraseBkgnd: Boolean;
    FOverridePaint: Boolean;
    FOverridePaintNC: Boolean;
    FPaintOnEraseBkgnd: Boolean;
    FText: string;
    function GetHandle: HWND;
    function GetText: string;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMPaint(var Message: TMessage); message WM_PAINT;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
    procedure WMSetText(var Message: TMessage); message WM_SETTEXT;
    procedure WMEnable(var Message: TMessage); message WM_ENABLE;
  strict protected
    procedure CallDefaultProc(var Msg: TMessage);
    function AcceptMessage(var Message: TMessage): Boolean; virtual;
    function GetStringPropValue(const PropName: string): string;
    function HasBorder: Boolean; virtual;
    function HasClientEdge: Boolean; virtual;
    function InternalPaint(DC: HDC): Boolean; virtual;
    procedure Paint(Canvas: TCanvas); virtual;
    procedure PaintBackground(Canvas: TCanvas); virtual;
    procedure PaintNC(Canvas: TCanvas); virtual;
    procedure SetRedraw(Value: Boolean);
    procedure WndProc(var Message: TMessage); virtual;
    property Brush: TBrush read FBrush;
    property DoubleBuffered: Boolean read FDoubleBuffered write FDoubleBuffered;
    property Focused: Boolean read FFocused write FFocused;
    property FocusUpdate: Boolean read FFocusUpdate write FFocusUpdate;
    property FontColor: TColor read FFontColor write FFontColor;
    property Handle: HWND read GetHandle;
    property Handled: Boolean read FHandled write FHandled;
    property NeedsDefaultPaint: Boolean read FNeedsDefaultPaint write FNeedsDefaultPaint;
    property OverridePaint: Boolean read FOverridePaint write FOverridePaint;
    property OverridePaintNC: Boolean read FOverridePaintNC write FOverridePaintNC;
    property OverrideEraseBkgnd: Boolean read FOverrideEraseBkgnd write FOverrideEraseBkgnd;
    property PaintOnEraseBkgnd: Boolean read FPaintOnEraseBkgnd write FPaintOnEraseBkgnd;
    property Text: string read GetText;
  public
    constructor Create(AControl: TWinControl); virtual;
    destructor Destroy; override;

    procedure DrawControlText(Canvas: TCanvas; Details: TThemedElementDetails;
      const S: string; var R: TRect; Flags: Cardinal);
    function HandleMessage(var Message: TMessage): Boolean; virtual;
    procedure Invalidate; virtual;
    procedure InvalidateNC; virtual;
    property Control: TWinControl read FControl;
  end;

  TColorizerMouseTrackControlStyleHook = class(TColorizerStyleHook)
  public type
    TMousePosition = (mpNone, mpLeft, mpRight, mpTop, mpBottom);
  strict private
    FHotTrackTimer: TComponent; { TTimer }
    FMouseInControl: Boolean;
    FMouseInNCArea: Boolean;
    procedure WMMouseMove(var Message: TWMMouse); message WM_MOUSEMOVE;
    procedure WMNCMouseMove(var Message: TWMMouse); message WM_NCMOUSEMOVE;
  strict protected
    procedure DoHotTrackTimer(Sender: TObject); virtual;
    function IsMouseInControl: Boolean;
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
    function IsChildHandle(AHandle: HWnd): Boolean; virtual;
    procedure StartHotTrackTimer;
    procedure StopHotTrackTimer;
    procedure WndProc(var Message: TMessage); override;
    property MouseInControl: Boolean read FMouseInControl write FMouseInControl;
    property MouseInNCArea: Boolean read FMouseInNCArea write FMouseInNCArea;
  public
    constructor Create(AControl: TWinControl); override;
    destructor Destroy; override;
  end;

  TColorizerFormStyleHook = class(TColorizerMouseTrackControlStyleHook)
  strict private const
    WM_NCUAHDRAWCAPTION = $00AE;
  strict private
    FCaptionRect: TRect;
    FChangeSizeCalled: Boolean;
    FChangeVisibleChildHandle: HWND;
    FCloseButtonRect: TRect;
    FFormActive: Boolean;
    FHotButton: Integer;
    FHeight: Integer;
    FHelpButtonRect: TRect;
    FIcon: TIcon;
    FIconHandle: HICON;
    FMaxButtonRect: TRect;
    FMDIClientInstance: Pointer;
    FMDIHorzScrollBar: TWinControl; { TScrollBar }
    FMDIPrevClientProc: Pointer;
    FMDIScrollSizeBox: TWinControl;
    FMDIStopHorzScrollBar: Boolean;
    FMDIStopVertScrollBar: Boolean;
    FMDIVertScrollBar: TWinControl; { TScrollBar }
    FMinButtonRect: TRect;
    FLeft: Integer;
    FNeedsUpdate: Boolean;
    FOldHorzSrollBarPosition: Integer;
    FOldVertSrollBarPosition: Integer;
    FPressedButton: Integer;
    FRegion: HRGN;
    FStopCheckChildMove: Boolean;
    FSysMenuButtonRect: TRect;
    FTop: Integer;
    FWidth: Integer;
    FCaptionEmulation: Boolean;
    procedure AdjustMDIScrollBars;
    procedure ChangeSize;
    function IsStyleBorder: Boolean;
    function GetBorderSize: TRect;
    function GetForm: TCustomForm; inline;
    function GetIconFast: TIcon;
    function GetIcon: TIcon;
    function GetHitTest(P: TPoint): Integer;
    procedure GetMDIScrollInfo(SetRange: Boolean);
    function GetMDIWorkArea: TRect;
    function GetRegion: HRgn;
    procedure InitMDIScrollBars;
    function MDIChildMaximized: Boolean;
    procedure MDIHorzScroll(Offset: Integer);
    procedure MDIVertScroll(Offset: Integer);
    function NormalizePoint(P: TPoint): TPoint;
    procedure UpdateForm;
    procedure OnMDIHScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure OnMDIVScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure CMDialogChar(var Message: TWMKey); message CM_DIALOGCHAR;
    procedure CMMenuChanged(var Message: TMessage); message CM_MENUCHANGED;
    procedure WMInitMenu(var Message: TMessage); message WM_INITMENU;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCActivate(var Message: TMessage); message WM_NCACTIVATE;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure WMSize(var Message: TWMSIZE); message WM_SIZE;
    procedure WMMove(var Message: TWMMOVE); message WM_MOVE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCMouseMove(var Message: TWMNCHitMessage); message WM_NCMOUSEMOVE;
    procedure WMNCLButtonDown(var Message: TWMNCHitMessage); message WM_NCLBUTTONDOWN;
    procedure WMNCRButtonDown(var Message: TWMNCHitMessage); message WM_NCRBUTTONDOWN;
    procedure WMNCLButtonUp(var Message: TWMNCHitMessage); message WM_NCLBUTTONUP;
    procedure WMNCRButtonUp(var Message: TWMNCHitMessage); message WM_NCRBUTTONUP;
    procedure WMNCLButtonDblClk(var Message: TWMNCHitMessage); message WM_NCLBUTTONDBLCLK;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMNCUAHDrawCaption(var Message: TMessage); message WM_NCUAHDRAWCAPTION;
    procedure WMShowWindow(var Message: TWMShowWindow); message WM_SHOWWINDOW;
    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure WMSetText(var Message: TMessage); message WM_SETTEXT;
    procedure WMMDIChildMove(var Message: TMessage); message WM_MDICHILDMOVE;
    procedure WMMDIChildClose(var Message: TMessage); message WM_MDICHILDCLOSE;
    procedure WMSysCommand(var Message: TMessage); message WM_SYSCOMMAND;
    procedure WMDestroy(var Message: TMessage); message WM_DESTROY;
  strict protected
    procedure Close; virtual;
    procedure Help; virtual;
    procedure Maximize; virtual;
    procedure MDIClientWndProc(var Message: TMessage); virtual;
    procedure Minimize; virtual;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure PaintBackground(Canvas: TCanvas); override;
    procedure PaintNC(Canvas: TCanvas); override;
    procedure Restore; virtual;
    procedure WndProc(var Message: TMessage); override;
    property Form: TCustomForm read GetForm;
  public
    constructor Create(AControl: TWinControl); override;
    destructor Destroy; override;
    procedure Invalidate; override;
    property Handle;
  end;

  TColorizerCheckBoxStyleHook = class(TColorizerMouseTrackControlStyleHook)
  strict private
    FPressed: Boolean;
    procedure WMLButtonDown(var Message: TWMMouse); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMMouse); message WM_LBUTTONUP;
    procedure WMLButtonDblClk(var Message: TWMMouse); message WM_LBUTTONDBLCLK;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKeyUp(var Message: TWMKeyUp); message WM_KEYUP;
    procedure BMSetCheck(var Message: TMessage); message BM_SETCHECK;
    function RightAlignment: Boolean;
  strict protected
    function GetDrawState(State: TCheckBoxState): TThemedButton; virtual;
    procedure Paint(Canvas: TCanvas); override;
    procedure PaintBackground(Canvas: TCanvas); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure WndProc(var Message: TMessage); override;
    property Pressed: Boolean read FPressed;
  public
    constructor Create(AControl: TWinControl); override;
  end;

  TColorizerRadioButtonStyleHook = class(TColorizerCheckBoxStyleHook)
  strict protected
    function GetDrawState(State: TCheckBoxState): TThemedButton; override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AControl: TWinControl); override;
  end;


  TColorizerStatusBarStyleHook = class(TColorizerStyleHook)
  strict protected
    procedure Paint(Canvas: TCanvas); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AControl: TWinControl); override;
  end;

  TColorizerEditStyleHook = class(TColorizerMouseTrackControlStyleHook)
  strict private
    procedure UpdateColors;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
  strict protected
    procedure PaintNC(Canvas: TCanvas); override;
    procedure WndProc(var Message: TMessage); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
  public
    constructor Create(AControl: TWinControl); override;
  end;

   procedure SetColorizerVCLStyle(const StyleName : string);
   function  ColorizerStyleServices: TCustomStyleServices;

implementation

uses
  System.SysUtils,
  System.TypInfo,
  Vcl.GraphUtil,
  Vcl.ExtCtrls,
  Colorizer.Utils,
  Winapi.CommCtrl;

var
   CurrentStyleName : string ='';

type
  TWinControlClass = class(TWinControl);
  TCustomFormClass = class(TCustomForm);
  TCustomCheckBoxClass = class(TCustomCheckBox);
  TRadioButtonClass = class(TRadioButton);
  TCustomEditClass = class(TCustomEdit);

  TCustomStatusBarHelper = class helper for TCustomStatusBar
  private
    function GetCanvasRW: TCanvas;
    procedure SetCanvasRW(const Value: TCanvas);
  public
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

procedure _DrawControlText(Canvas: TCanvas; const S: string; var R: TRect; Flags: Cardinal; ThemeTextColor: TColor);
var
  TextFormat: TTextFormatFlags;
begin
  //Canvas.Font := TWinControlClass(Control).Font;
  Canvas.Font.Color := ThemeTextColor;
  TextFormat := TTextFormatFlags(Flags);
   DrawText(Canvas.Handle, S,  Length(S), R, TTextFormatFlags(TextFormat));
end;

procedure SetColorizerVCLStyle(const StyleName : string);
var
 s : string;
 found : Boolean;
begin
 found:=false;
 if (StyleName<>'') and (not SameText('Windows', StyleName))  and (StyleName<>CurrentStyleName) then
  for s in TStyleManager.StyleNames do
    if s=StyleName then
    begin
      found:=True;
      Break;
    end;

 if found or (StyleName='') then
   CurrentStyleName:=StyleName;

 //AddLog('SetColorizerVCLStyle', StyleName);
end;

function ColorizerStyleServices: TCustomStyleServices;
begin
 if (CurrentStyleName<>'') then
  Result := TStyleManager.Style[CurrentStyleName]
 else
  Result:= StyleServices;
end;

constructor TColorizerStyleHook.Create(AControl: TWinControl);
begin
  FControl := AControl;
  FCallDefaultProc := False;
  FOverridePaint := False;
  FOverrideEraseBkgnd := False;
  FOverridePaintNC := False;
  FDoubleBuffered := False;
  FNeedsDefaultPaint := False;
  FPaintOnEraseBkgnd := False;
  FHandled := False;
  FFocusUpdate := False;
  FBrush := TBrush.Create;
  FBrush.Color := ColorizerStyleServices.GetStyleColor(scWindow);
  FFontColor := ColorizerStyleServices.GetSystemColor(clWindowText);
end;

destructor TColorizerStyleHook.Destroy;
begin
  FBrush.Free;
  inherited Destroy;
end;

procedure TColorizerStyleHook.CallDefaultProc(var Msg: TMessage);
begin
  if FCallDefaultProc then
    FControl.WindowProc(Msg)
  else
  begin
    FCallDefaultProc := True;
    FControl.WindowProc(Msg);
    FCallDefaultProc := False;
  end;
end;

procedure TColorizerStyleHook.DrawControlText(Canvas: TCanvas; Details: TThemedElementDetails;
  const S: string; var R: TRect; Flags: Cardinal);
var
  ThemeTextColor: TColor;
  TextFormat: TTextFormatFlags;
begin
  Canvas.Font := TWinControlClass(Control).Font;
  TextFormat := TTextFormatFlags(Flags);
  if ColorizerStyleServices.GetElementColor(Details, ecTextColor, ThemeTextColor) then
  begin
    if not Control.Enabled {$IFDEF DELPHIXE3_UP} or (seFont in Control.StyleElements) {$ENDIF} then
      Canvas.Font.Color := ThemeTextColor;
    ColorizerStyleServices.DrawText(Canvas.Handle, Details, S, R, TextFormat, Canvas.Font.Color);
  end
  else
  begin
    Canvas.Refresh;
    ColorizerStyleServices.DrawText(Canvas.Handle, Details, S, R, TextFormat);
  end;
end;

function TColorizerStyleHook.AcceptMessage(var Message: TMessage): Boolean;
begin
  Result := True;
  {$IFDEF DELPHIXE3_UP}
  if not (seBorder in Control.StyleElements) then
  begin
    case Message.Msg of
      WM_NCCREATE..WM_NCMBUTTONDBLCLK:
        Result := False;
    end;
  end;

  if not (seClient in Control.StyleElements) then
  begin
    case Message.Msg of
      WM_ERASEBKGND, WM_PAINT,
      WM_CTLCOLORMSGBOX..WM_CTLCOLORSTATIC,
      CN_CTLCOLORMSGBOX..CN_CTLCOLORSTATIC,
      TVM_SETBKCOLOR, LVM_SETBKCOLOR:
        Result := False;
    end;
  end;
  {$ENDIF}
end;

function TColorizerStyleHook.HandleMessage(var Message: TMessage): Boolean;
begin
  Result := False;

  if not AcceptMessage(Message) then Exit;

  if FCallDefaultProc then
  begin
    if not FNeedsDefaultPaint then
      case Message.Msg of
        WM_PAINT, WM_ERASEBKGND, WM_NCPAINT,
        WM_CTLCOLORMSGBOX..WM_CTLCOLORSTATIC,
        CN_CTLCOLORMSGBOX..CN_CTLCOLORSTATIC: ; // Allow message to be passed to WndProc
      else
        Exit;
      end
    else
      Exit;
  end;
  Handled := False;
  if ColorizerStyleServices.Available then
    WndProc(Message);
  Result := Handled;
end;

function TColorizerStyleHook.GetStringPropValue(const PropName: string): string;
var
  PropInfo: PPropInfo;
begin
  Result := '';
  if Control = nil then Exit;
  if (PropName = 'Caption') and (Control <> nil) then // do not localize
    Result := TWinControlClass(Control).Caption;
  PropInfo := GetPropInfo(Control.ClassInfo, PropName);
  if PropInfo <> nil then
    Result := GetStrProp(Control, PropInfo);
end;

function TColorizerStyleHook.GetText: string;
var
  Buffer: array [0..255] of Char;
begin
  if (Handle <> 0) then
    SetString(Result, Buffer, GetWindowText(Handle, Buffer, Length(Buffer)))
  else
    Result := GetStringPropValue('Caption'); // do not localize
  FText := Result;
end;

procedure TColorizerStyleHook.SetRedraw(Value: Boolean);
begin
  if Control.Visible and Control.HandleAllocated then
    SendMessage(Control.Handle, WM_SETREDRAW, LPARAM(Value), 0);
end;

function TColorizerStyleHook.GetHandle: HWND;
begin
  if FControl.HandleAllocated then
    Result := FControl.Handle
  else
    Result := 0;
end;

function TColorizerStyleHook.HasBorder: Boolean;
begin
  Result := (GetWindowLong(Handle, GWL_STYLE) and WS_BORDER = WS_BORDER) or
    (GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_CLIENTEDGE = WS_EX_CLIENTEDGE);
end;

function TColorizerStyleHook.HasClientEdge: Boolean;
begin
  Result := GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_CLIENTEDGE = WS_EX_CLIENTEDGE;
end;

procedure TColorizerStyleHook.InvalidateNC;
begin
  if Control.HandleAllocated then
    SendMessage(Handle, WM_NCPAINT, 0, 0);
end;
 
procedure TColorizerStyleHook.Invalidate;
begin
  if Control.HandleAllocated then
  begin
    if FOverridePaintNC then
      InvalidateNC;
    InvalidateRect(Handle, nil, False);
  end;
end;

function TColorizerStyleHook.InternalPaint(DC: HDC): Boolean;
begin
  Result := False;
end;

procedure TColorizerStyleHook.Paint(Canvas: TCanvas);
begin
end;

procedure TColorizerStyleHook.PaintBackground(Canvas: TCanvas);
begin
  Canvas.Brush.Color := FBrush.Color;
  Canvas.FillRect(Control.ClientRect);
end;

procedure TColorizerStyleHook.PaintNC(Canvas: TCanvas);
begin
end;

procedure TColorizerStyleHook.WMPaint(var Message: TMessage);
var
  DC: HDC;
  Buffer: TBitmap;
  Canvas: TCanvas;
  PS: TPaintStruct;
begin
  if FOverridePaint then
  begin
    DC := HDC(Message.WParam);
    Canvas := TCanvas.Create;
    try
      if DC <> 0 then
        Canvas.Handle := DC
      else
        Canvas.Handle := BeginPaint(Control.Handle, PS);
      if not InternalPaint(Canvas.Handle) then
        if FDoubleBuffered and (DC = 0) then
        begin
          Buffer := TBitmap.Create;
          try
            Buffer.SetSize(Control.Width, Control.Height);
            PaintBackground(Buffer.Canvas);
            Paint(Buffer.Canvas);
            // paint other controls
            if Control is TWinControl then
              TWinControlClass(Control).PaintControls(Buffer.Canvas.Handle, nil);
            Canvas.Draw(0, 0, Buffer);
          finally
            Buffer.Free;
          end;
        end
       else
         begin
           Paint(Canvas);
           // paint other controls
           if Control is TWinControl then
              TWinControlClass(Control).PaintControls(Canvas.Handle, nil);
         end;
      if DC = 0 then
        EndPaint(Control.Handle, PS);
    finally
      Canvas.Handle := 0;
      Canvas.Free;
    end;
    Handled := True;
  end;
end;

procedure TColorizerStyleHook.WMNCPaint(var Message: TMessage);
var
  Canvas: TCanvas;
begin
  if FOverridePaintNC then
  begin
    Canvas := TCanvas.Create;
    try
      Canvas.Handle := GetWindowDC(Control.Handle);
      PaintNC(Canvas);
    finally
      ReleaseDC(Handle, Canvas.Handle);
      Canvas.Handle := 0;
      Canvas.Free;
    end;
    Handled := True;
  end;
end;

procedure TColorizerStyleHook.WMEraseBkgnd(var Message: TMessage);
var
  DC: HDC;
  Canvas: TCanvas;
  SaveIndex: Integer;
begin
  if FOverrideEraseBkgnd then
  begin
    if not FDoubleBuffered then
    begin
      DC := HDC(Message.WParam);
      SaveIndex := SaveDC(DC);
      Canvas := TCanvas.Create;
      try
        Canvas.Handle := DC;
        PaintBackground(Canvas);
        if FPaintOnEraseBkgnd then
          Paint(Canvas);
      finally
        Canvas.Handle := 0;
        Canvas.Free;
        RestoreDC(DC, SaveIndex);
      end;
    end;
    Handled := True;
    Message.Result := 1;
  end;
end;

procedure TColorizerStyleHook.WMSetFocus(var Message: TMessage);
begin
  FFocused := True;
  if FFocusUpdate then
    Invalidate;
end;

procedure TColorizerStyleHook.WMKillFocus(var Message: TMessage);
begin
  FFocused := False;
  if FFocusUpdate then
    Invalidate;
end;

procedure TColorizerStyleHook.CMEnabledChanged(var Message: TMessage);
begin
  CallDefaultProc(Message);
  Invalidate;
  Handled := True;
end;

procedure TColorizerStyleHook.CMTextChanged(var Message: TMessage);
begin
  CallDefaultProc(Message);
  Invalidate;
  Handled := True;
end;

procedure TColorizerStyleHook.WMSetText(var Message: TMessage);
begin
  CallDefaultProc(Message);
  Invalidate;
  Handled := True;
end;

procedure TColorizerStyleHook.WMEnable(var Message: TMessage);
begin
  CallDefaultProc(Message);
  Invalidate;
  Handled := True;
end;

procedure TColorizerStyleHook.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_CTLCOLORMSGBOX..WM_CTLCOLORSTATIC:
      with Message do
        Result := SendMessage(LParam, CN_BASE + Msg, WParam, LParam);
    CN_CTLCOLORMSGBOX..CN_CTLCOLORSTATIC:
      begin
        SetTextColor(Message.WParam, ColorToRGB(FFontColor));
        SetBkColor(Message.WParam, ColorToRGB(FBrush.Color));
        Message.Result := LRESULT(FBrush.Handle);
        Handled := True;
      end;
  end;
  Dispatch(Message);
end;

constructor TColorizerMouseTrackControlStyleHook.Create(AControl: TWinControl);
begin
  inherited;
  FMouseInControl := False;
  FMouseInNCArea := False;
  Focused := False;
  FHotTrackTimer := nil;
end;

destructor TColorizerMouseTrackControlStyleHook.Destroy;
begin
  if FHotTrackTimer <> nil then
    StopHotTrackTimer;
  inherited;
end;

function TColorizerMouseTrackControlStyleHook.IsChildHandle(AHandle: HWnd): Boolean;
begin
  Result := False;
end;

function TColorizerMouseTrackControlStyleHook.IsMouseInControl: Boolean;
var
  P: TPoint;
begin
  GetCursorPos(P);
  Result := WindowFromPoint(P) = Handle;
  FMouseInControl := Result;
end;

procedure TColorizerMouseTrackControlStyleHook.StartHotTrackTimer;
begin
  if FHotTrackTimer <> nil then
    StopHotTrackTimer;
  FHotTrackTimer := TTimer.Create(nil);
  TTimer(FHotTrackTimer).Interval := 100;
  TTimer(FHotTrackTimer).OnTimer := DoHotTrackTimer;
  TTimer(FHotTrackTimer).Enabled := True;
end;

procedure TColorizerMouseTrackControlStyleHook.StopHotTrackTimer;
begin
  if FHotTrackTimer <> nil then
  begin
    TTimer(FHotTrackTimer).Enabled := False;
    FreeAndNil(FHotTrackTimer);
  end;
end;

procedure TColorizerMouseTrackControlStyleHook.DoHotTrackTimer;
var
  P: TPoint;
  FWindowHandle: HWnd;
begin
  GetCursorPos(P);
  FWindowHandle := WindowFromPoint(P);
  if (FWindowHandle <> Handle) and not IsChildHandle(FWindowHandle) then
  begin
    StopHotTrackTimer;
    FMouseInControl := False;
    MouseLeave;
  end;
end;

procedure TColorizerMouseTrackControlStyleHook.WMMouseMove(var Message: TWMMouse);
begin
  inherited;
  if not FMouseInControl and not FMouseInNCArea then
  begin
    FMouseInControl := True;
    StartHotTrackTimer;
    MouseEnter;
  end
  else if FMouseInNCArea and FMouseInControl then
  begin
    StopHotTrackTimer;
    FMouseInControl := False;
    MouseLeave;
  end;
end;

procedure TColorizerMouseTrackControlStyleHook.WMNCMouseMove(var Message: TWMMouse);
begin
  inherited;
  if not FMouseInControl then
  begin
    FMouseInControl := True;
    StartHotTrackTimer;
    MouseEnter;
  end;
end;

procedure TColorizerMouseTrackControlStyleHook.MouseEnter;
begin
end;

procedure TColorizerMouseTrackControlStyleHook.MouseLeave;
begin
end;

procedure TColorizerMouseTrackControlStyleHook.WndProc(var Message: TMessage);
begin
  // Reserved for potential updates
  inherited;
end;



constructor TColorizerFormStyleHook.Create(AControl: TWinControl);
begin
  inherited;
  FocusUpdate := False;
 {$IFDEF DELPHIXE3_UP}
  if seClient in Form.StyleElements then
    OverrideEraseBkgnd := True;
 {$ENDIF}
  if IsStyleBorder then
    OverridePaintNC := True;

  FMDIHorzScrollBar := nil;
  FMDIVertScrollBar := nil;
  FMDIScrollSizeBox := nil;
  FMDIClientInstance := nil;
  FMDIPrevClientProc := nil;
  FChangeVisibleChildHandle := 0;
  FStopCheckChildMove := False;
  FOldHorzSrollBarPosition := 0;
  FOldVertSrollBarPosition := 0;

  FMDIStopHorzScrollBar := False;
  FMDIStopVertScrollBar := False;

  MouseInNCArea := True;
  FFormActive := False;
  FChangeSizeCalled := False;
  FRegion := 0;
  FLeft := Control.Left;
  FTop := Control.Top;
  FWidth := Control.Width;
  FHeight := Control.Height;
  FNeedsUpdate := True;
  FIcon := nil;
  FIconHandle := 0;
  FHotButton := 0;
  FPressedButton := 0;
  FCaptionEmulation := False;
end;

destructor TColorizerFormStyleHook.Destroy;
begin
  if FIcon <> nil then
    FreeAndNil(FIcon);
  if FRegion <> 0 then
  begin
    DeleteObject(FRegion);
    FRegion := 0;
  end;
  if FMDIClientInstance <> nil then
  begin
    SetWindowLong(TForm(Control).ClientHandle, GWL_WNDPROC, IntPtr(FMDIPrevClientProc));
    FreeObjectInstance(FMDIClientInstance);
  end;

  if FMDIHorzScrollBar <> nil then
    FreeAndNil(FMDIHorzScrollBar);
  if FMDIVertScrollBar <> nil then
    FreeAndNil(FMDIVertScrollBar);
  if FMDIScrollSizeBox <> nil then
    FreeAndNil(FMDIScrollSizeBox);
  inherited;
end;

function TColorizerFormStyleHook.IsStyleBorder: Boolean;
begin
  Result := {$IFDEF DELPHIXE3_UP}(TStyleManager.FormBorderStyle = fbsCurrentStyle) and (seBorder in Form.StyleElements){$ELSE}True{$ENDIF};
end;

procedure TColorizerFormStyleHook.Invalidate;
begin
  // Prevent ancestor's Invalidate from executing
end;

procedure TColorizerFormStyleHook.MDIHorzScroll(Offset: Integer);
var
  I: Integer;
begin
  FStopCheckChildMove := True;
  try
    for I := 0 to TCustomFormClass(Form).MDIChildCount -1 do
      if TCustomFormClass(Form).MDIChildren[I].Visible then
        TCustomFormClass(Form).MDIChildren[I].Left := TCustomFormClass(Form).MDIChildren[I].Left + Offset;
  finally
    FStopCheckChildMove := False;
  end;
  GetMDIScrollInfo(False);
end;

procedure TColorizerFormStyleHook.MDIVertScroll(Offset: Integer);
var
  I: Integer;
begin
  FStopCheckChildMove := True;
  try
    for I := 0 to TCustomFormClass(Form).MDIChildCount -1 do
      if TCustomFormClass(Form).MDIChildren[I].Visible then
        TCustomFormClass(Form).MDIChildren[I].Top := TCustomFormClass(Form).MDIChildren[I].Top + Offset;
  finally
    FStopCheckChildMove := False;
  end;
  GetMDIScrollInfo(False);
end;

procedure TColorizerFormStyleHook.OnMDIHScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
var
  Offset: Integer;
begin
  if (FMDIStopHorzScrollBar) or (ScrollCode <> scEndScroll) then
    Exit;

  Offset := TScrollBar(FMDIHorzScrollBar).Position - FOldHorzSrollBarPosition;
  if Offset <> 0 then
    MDIHorzScroll(-Offset);
  FOldHorzSrollBarPosition := TScrollBar(FMDIHorzScrollBar).Position;
end;

procedure TColorizerFormStyleHook.OnMDIVScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
var
  Offset: Integer;
begin
  if (FMDIStopVertScrollBar) or (ScrollCode <> scEndScroll) then
    Exit;

  Offset := TScrollBar(FMDIVertScrollBar).Position - FOldVertSrollBarPosition;
  if Offset <> 0 then
    MDIVertScroll(-Offset);
  FOldVertSrollBarPosition := TScrollBar(FMDIVertScrollBar).Position;
end;

function TColorizerFormStyleHook.MDIChildMaximized: Boolean;
begin
  Result := (TCustomFormClass(Form).ActiveMDIChild <> nil) and
    (TCustomFormClass(Form).ActiveMDIChild.WindowState = wsMaximized);
end;

procedure TColorizerFormStyleHook.GetMDIScrollInfo(SetRange: Boolean);
var
  I, MinX, MinY, MaxX, MaxY, HPage, VPage: Integer;
  R, MDIR, MDICLR: TRect;
  ReCalcInfo: Boolean;
  LHorzScrollVisible, LVertScrollVisible: Boolean;
  LMDIHorzScrollBar: TScrollBar;
  LMDIVertScrollBar: TScrollBar;
begin
  LMDIHorzScrollBar := TScrollBar(FMDIHorzScrollBar);
  LMDIVertScrollBar := TScrollBar(FMDIVertScrollBar);
  if (LMDIHorzScrollBar = nil) or (LMDIVertScrollBar = nil) then
    Exit;

  if (not (LMDIVertScrollBar.HandleAllocated)) or
     (not LMDIHorzScrollBar.HandleAllocated) then
    Exit;

  if MDIChildMaximized then
  begin
    if IsWindowVisible(LMDIHorzScrollBar.Handle) then
      ShowWindow(LMDIHorzScrollBar.Handle, SW_HIDE);
    if IsWindowVisible(LMDIVertScrollBar.Handle) then
      ShowWindow(LMDIVertScrollBar.Handle, SW_HIDE);
    Exit;
  end;

  ReCalcInfo := False;
  R := GetMDIWorkArea;

  MinX := MaxInt;
  MinY := MaxInt;
  MaxX := -MaxInt;
  MaxY := -MaxInt;

  for I := 0 to TCustomFormClass(Form).MDIChildCount -1 do
   if (TCustomFormClass(Form).MDIChildren[I].Visible) and
      (TCustomFormClass(Form).MDIChildren[I].Handle <> FChangeVisibleChildHandle) then
     with TCustomFormClass(Form) do
     begin
       GetWindowRect(MDIChildren[I].Handle, MDIR);
       GetWindowRect(TForm(Control).ClientHandle, MDICLR);
       OffsetRect(MDIR, -MDICLR.Left, -MDICLR.Top);
       if MinX > MDIR.Left then
         MinX := MDIR.Left;
       if MinY > MDIR.Top then
         MinY := MDIR.Top;
       if MaxX < MDIR.Left + MDIR.Width then
         MaxX := MDIR.Left + MDIR.Width;
       if MaxY < MDIR.Top + MDIR.Height then
         MaxY := MDIR.Top + MDIR.Height;
     end;

  LHorzScrollVisible := (MinX < 0) or (MaxX > R.Width);
  LVertScrollVisible := (MinY < 0) or (MaxY > R.Height);

  if LVertScrollVisible and not LHorzScrollVisible then
    LHorzScrollVisible := (MinX < 0) or (MaxX > R.Width - LMDIVertScrollBar.Width);

  if LHorzScrollVisible and not LVertScrollVisible then
    LVertScrollVisible := (MinY < 0) or (MaxY > R.Height - LMDIHorzScrollBar.Height);

  if LHorzScrollVisible and not IsWindowVisible(LMDIHorzScrollBar.Handle) then
  begin
    SetWindowPos(LMDIHorzScrollBar.Handle, HWND_TOP,
      R.Left, R.Bottom - LMDIHorzScrollBar.Height,
      R.Width, LMDIHorzScrollBar.Height, SWP_SHOWWINDOW);
    ShowWindow(LMDIHorzScrollBar.Handle, SW_SHOW);
    ReCalcInfo := True;
  end
  else if not LHorzScrollVisible and IsWindowVisible(LMDIHorzScrollBar.Handle) then
  begin
    ShowWindow(LMDIHorzScrollBar.Handle, SW_HIDE);
    ReCalcInfo := True;
  end;

  if LVertScrollVisible and not IsWindowVisible(LMDIVertScrollBar.Handle) then
  begin
    if LHorzScrollVisible
    then
      SetWindowPos(LMDIVertScrollBar.Handle, HWND_TOP,
        R.Right - LMDIVertScrollBar.Width,
        R.Top, LMDIVertScrollBar.Width, R.Height - LMDIHorzScrollBar.Height, SWP_SHOWWINDOW)
    else
      SetWindowPos(LMDIVertScrollBar.Handle, HWND_TOP,
        R.Right - LMDIVertScrollBar.Width,
        R.Top, LMDIVertScrollBar.Width, R.Height, SWP_SHOWWINDOW);
    ShowWindow(LMDIVertScrollBar.Handle, SW_SHOW);
    ReCalcInfo := True;
  end
  else if not LVertScrollVisible and IsWindowVisible(LMDIVertScrollBar.Handle) then
  begin
    ShowWindow(LMDIVertScrollBar.Handle, SW_HIDE);
    ReCalcInfo := True;
  end;

  HPage := R.Width;
  VPage := R.Height;

  AdjustMDIScrollBars;

  if IsWindowVisible(LMDIHorzScrollBar.Handle) then
  begin
    if MinX > 0 then
      MinX := 0;
    if MaxX < R.Width then
      MaxX := R.Width;
    if SetRange then
    begin
      FMDIStopHorzScrollBar := True;
      if IsWindowVisible(LMDIVertScrollBar.Handle) then
        LMDIHorzScrollBar.PageSize := HPage - LMDIVertScrollBar.Width
      else
        LMDIHorzScrollBar.PageSize := HPage;
      LMDIHorzScrollBar.SetParams(-MinX, 0, MaxX - MinX - 1);
      FOldHorzSrollBarPosition := LMDIHorzScrollBar.Position;
      FMDIStopHorzScrollBar := False;
    end;
    LMDIHorzScrollBar.LargeChange := LMDIHorzScrollBar.PageSize;
  end;

  if IsWindowVisible(LMDIVertScrollBar.Handle) then
  begin
    if MinY > 0 then
      MinY := 0;
    if MaxY < R.Height then
      MaxY := R.Height;
    if SetRange then
    begin
      FMDIStopVertScrollBar := True;
      if IsWindowVisible(LMDIHorzScrollBar.Handle) then
        LMDIVertScrollBar.PageSize := VPage - LMDIHorzScrollBar.Height
      else
        LMDIVertScrollBar.PageSize := VPage;
      LMDIVertScrollBar.SetParams(-MinY, 0, MaxY - MinY - 1);
      FOldVertSrollBarPosition := LMDIVertScrollBar.Position;
      FMDIStopVertScrollBar := False;
    end;
    LMDIVertScrollBar.LargeChange := LMDIVertScrollBar.PageSize;
  end;

  if (not IsWindowVisible(LMDIHorzScrollBar.Handle)) and
     (not IsWindowVisible(LMDIVertScrollBar.Handle)) then ReCalcInfo := False;

  if IsWindowVisible(LMDIHorzScrollBar.Handle) and IsWindowVisible(LMDIVertScrollBar.Handle) and
     not IsWindowVisible(FMDIScrollSizeBox.Handle) then
  begin
    SetWindowPos(FMDIScrollSizeBox.Handle, HWND_TOP,
      R.Right - LMDIVertScrollBar.Width, R.Bottom - LMDIHorzScrollBar.Height,
      LMDIVertScrollBar.Width, LMDIHorzScrollBar.Height, SWP_SHOWWINDOW);
    ShowWindow(FMDIScrollSizeBox.Handle, SW_SHOW);
  end
  else if not IsWindowVisible(LMDIHorzScrollBar.Handle) or not IsWindowVisible(LMDIVertScrollBar.Handle) and
     IsWindowVisible(FMDIScrollSizeBox.Handle) then
    ShowWindow(FMDIScrollSizeBox.Handle, SW_HIDE);

  if ReCalcInfo then
    GetMDIScrollInfo(SetRange);
end;

procedure TColorizerFormStyleHook.InitMDIScrollBars;
begin
  if FMDIHorzScrollBar = nil then
  begin
    FMDIHorzScrollBar := TScrollBar.CreateParented(Control.Handle);
    with TScrollBar(FMDIHorzScrollBar) do
    begin
      Kind := sbHorizontal;
      OnScroll := OnMDIHScroll;
      SetWindowPos(FMDIHorzScrollBar.Handle, HWND_TOP,
        0, 0, 0, GetSystemMetrics(SM_CYHSCROLL), SWP_NOREDRAW);
      ShowWindow(FMDIHorzScrollBar.Handle, SW_HIDE);
    end;
  end;

  if FMDIVertScrollBar = nil then
  begin
    FMDIVertScrollBar := TScrollBar.CreateParented(Control.Handle);
    with TScrollBar(FMDIVertScrollBar) do
    begin
      Kind := sbVertical;
      OnScroll := OnMDIVScroll;
      SetWindowPos(FMDIVertScrollBar.Handle, HWND_TOP,
        0, 0, GetSystemMetrics(SM_CXVSCROLL), 0, SWP_NOREDRAW);
      ShowWindow(FMDIVertScrollBar.Handle, SW_HIDE);
    end;
  end;

  if FMDIScrollSizeBox = nil
  then
    begin
      FMDIScrollSizeBox := TScrollBarStyleHook.TScrollWindow.CreateParented(Control.Handle);
      with TScrollBarStyleHook.TScrollWindow(FMDIScrollSizeBox) do
      begin
        SizeBox := True;
        SetWindowPos(FMDIScrollSizeBox.Handle, HWND_TOP,
          0, 0, GetSystemMetrics(SM_CXVSCROLL), GetSystemMetrics(SM_CYHSCROLL), SWP_NOREDRAW);
        ShowWindow(FMDIScrollSizeBox.Handle, SW_HIDE);
      end;
    end;
end;

procedure TColorizerFormStyleHook.AdjustMDIScrollBars;
var
  R: TRect;
begin
  R := GetMDIWorkArea;

  if (FMDIHorzScrollBar <> nil) and IsWindowVisible(FMDIHorzScrollBar.Handle)
  then
    begin
      if (FMDIVertScrollBar <> nil) and IsWindowVisible(FMDIVertScrollBar.Handle) then
        SetWindowPos(FMDIHorzScrollBar.Handle, HWND_TOP, R.Left,
          R.Bottom - FMDIHorzScrollBar.Height, R.Width - FMDIVertScrollBar.Width, FMDIHorzScrollBar.Height, SWP_SHOWWINDOW)
      else
        SetWindowPos(FMDIHorzScrollBar.Handle, HWND_TOP, R.Left,
          R.Bottom - FMDIHorzScrollBar.Height, R.Width, FMDIHorzScrollBar.Height, SWP_SHOWWINDOW);
    end;

  if (FMDIVertScrollBar <> nil) and IsWindowVisible(FMDIVertScrollBar.Handle) then
  begin
    if (FMDIHorzScrollBar <> nil) and IsWindowVisible(FMDIHorzScrollBar.Handle)
    then
      SetWindowPos(FMDIVertScrollBar.Handle, HWND_TOP,
        R.Right - FMDIVertScrollBar.Width,
        R.Top, FMDIVertScrollBar.Width, R.Height - FMDIHorzScrollBar.Height, SWP_SHOWWINDOW)
    else
      SetWindowPos(FMDIVertScrollBar.Handle, HWND_TOP,
        R.Right - FMDIVertScrollBar.Width,
        R.Top, FMDIVertScrollBar.Width, R.Height, SWP_SHOWWINDOW)
  end;

  if (FMDIScrollSizeBox <> nil) and IsWindowVisible(FMDIScrollSizeBox.Handle) and
     (FMDIVertScrollBar <> nil) and IsWindowVisible(FMDIVertScrollBar.Handle) and
     (FMDIHorzScrollBar <> nil) and IsWindowVisible(FMDIHorzScrollBar.Handle) then
    SetWindowPos(FMDIScrollSizeBox.Handle, HWND_TOP,
      R.Right - FMDIVertScrollBar.Width, R.Bottom - FMDIHorzScrollBar.Height,
      FMDIVertScrollBar.Width, FMDIHorzScrollBar.Height, SWP_SHOWWINDOW);
end;

function TColorizerFormStyleHook.GetMDIWorkArea: TRect;
var
  P: TPoint;
begin
  Result := Control.ClientRect;
  if TForm(Control).ClientHandle <> 0 then
  begin
    GetWindowRect(TForm(Control).ClientHandle, Result);
    P := Control.ClientToScreen(Point(0, 0));
    OffsetRect(Result, -P.X, -P.Y);
  end;
end;

procedure TColorizerFormStyleHook.MDIClientWndProc(var Message: TMessage);
var
  FCallOldProc: Boolean;
  R: TRect;
  Details: TThemedElementDetails;
begin
  FCallOldProc := True;
  case Message.Msg of
    WM_NCACTIVATE:
      begin
        if TForm(Control).ActiveMDIChild <> nil then
          SendMessage(TForm(Control).ActiveMDIChild.Handle,
            Message.Msg, Message.wParam, Message.LParam);
        FCallOldProc := False;
        Message.Result := 1;
      end;
    WM_NCCALCSIZE:
      FCallOldProc := False;
   WM_NCPAINT:
      FCallOldProc := False;
    WM_ERASEBKGND:
      if ColorizerStyleServices.Available then
      begin
        Details.Element := teWindow;
        Details.Part := 0;
        R := Rect(0, 0, TForm(Control).ClientWidth, TForm(Control).ClientHeight);
        if ColorizerStyleServices.Available then
          ColorizerStyleServices.DrawElement(Message.wParam, Details, R);
        FCallOldProc := False;
      end;
  end;

  if FCallOldProc then
    with Message do
      Result := CallWindowProc(FMDIPrevClientProc, TCustomFormClass(Form).ClientHandle,
        Msg, wParam, lParam);
end;

procedure TColorizerFormStyleHook.PaintBackground(Canvas: TCanvas);
var
  R: TRect;
begin
   R := Rect(0, 0, Control.ClientWidth, Control.ClientHeight);
   Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.Color;
   Canvas.FillRect(R);
end;

function TColorizerFormStyleHook.GetBorderSize: TRect;
var
  Size: TSize;
  Details: TThemedElementDetails;
  Detail: TThemedWindow;
begin
  Result := Rect(0, 0, 0, 0);
  if Form.BorderStyle = bsNone then Exit;

  if not ColorizerStyleServices.Available then Exit;
  {caption height}
  if (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
    Detail := twCaptionActive
  else
    Detail := twSmallCaptionActive;
  Details := ColorizerStyleServices.GetElementDetails(Detail);
  ColorizerStyleServices.GetElementSize(0, Details, esActual, Size);
  Result.Top := Size.cy;
  {left border width}
  if (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
    Detail := twFrameLeftActive
  else
    Detail := twSmallFrameLeftActive;
  Details := ColorizerStyleServices.GetElementDetails(Detail);
  ColorizerStyleServices.GetElementSize(0, Details, esActual, Size);
  Result.Left := Size.cx;
  {right border width}
  if (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
    Detail := twFrameRightActive
  else
    Detail := twSmallFrameRightActive;
  Details := ColorizerStyleServices.GetElementDetails(Detail);
  ColorizerStyleServices.GetElementSize(0, Details, esActual, Size);
  Result.Right := Size.cx;
  {bottom border height}
  if (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
    Detail := twFrameBottomActive
  else
    Detail := twSmallFrameBottomActive;
  Details := ColorizerStyleServices.GetElementDetails(Detail);
  ColorizerStyleServices.GetElementSize(0, Details, esActual, Size);
  Result.Bottom := Size.cy;
end;

function TColorizerFormStyleHook.GetForm: TCustomForm;
begin
  Result := TCustomForm(Control);
end;

function TColorizerFormStyleHook.NormalizePoint(P: TPoint): TPoint;
var
  WindowPos, ClientPos: TPoint;
  HandleParent: HWnd;
begin
  if (TCustomFormClass(Form).FormStyle = fsMDIChild) or (Form.Parent <> nil) then
  begin
    HandleParent := GetParent(Control.Handle);
    WindowPos := Point(FLeft, FTop);
    ClientToScreen(HandleParent, WindowPos);
    ClientPos := Point(0, 0);
    ClientToScreen(Handle, ClientPos);
    Result := P;
    ScreenToClient(Handle, Result);
    Inc(Result.X, ClientPos.X - WindowPos.X);
    Inc(Result.Y, ClientPos.Y - WindowPos.Y);
  end
  else
  begin
    WindowPos := Point(FLeft, FTop);
    ClientPos := Point(0, 0);
    ClientToScreen(Handle, ClientPos);
    Result := P;
    ScreenToClient(Handle, Result);
    Inc(Result.X, ClientPos.X - WindowPos.X);
    Inc(Result.Y, ClientPos.Y - WindowPos.Y);
  end;
end;


function TColorizerFormStyleHook.GetHitTest(P: TPoint): Integer;
var
  FBorderSize: TRect;
  FTopLeftRect,  FTopRightRect,
  FBottomLeftRect, FBottomRightRect,
  FTopRect, FLeftRect, FRightRect, FBottomRect, FHitCaptionRect: TRect;
begin
  Result := HTCLIENT;
  if Form.BorderStyle = bsNone then
      Exit;

  FBorderSize := GetBorderSize;
  FHitCaptionRect := FCaptionRect;
  FHitCaptionRect.Top := FBorderSize.Left;
  FBorderSize.Top := FHitCaptionRect.Top;

  if FHitCaptionRect.Contains(P) then
    Exit(HTCAPTION)
  else if FCloseButtonRect.Contains(P) then
    Exit(HTCLOSE)
  else if FMaxButtonRect.Contains(P) then
    Exit(HTMAXBUTTON)
  else if FMinButtonRect.Contains(P) then
    Exit(HTMINBUTTON)
  else if FHelpButtonRect.Contains(P) then
    Exit(HTHELP)
  else if FSysMenuButtonRect.Contains(P) then
    Exit(HTSYSMENU);

  {check window state}
  if (Form.WindowState = wsMaximized) or
     (Form.WindowState = wsMinimized) then
    Exit;

  {check border}
  if (Form.BorderStyle = bsDialog) or
     (Form.BorderStyle = bsSingle) or
     (Form.BorderStyle = bsToolWindow) then
  begin
    if Rect(FBorderSize.Left, FBorderSize.Top,
       FWidth - FBorderSize.Right, FHeight - FBorderSize.Bottom).Contains(P) then
      Exit(HTCLIENT)
    else
      Exit(HTBORDER);
  end;

  FTopLeftRect := Rect(0, 0, FBorderSize.Left, FBorderSize.Top);
  FTopRightRect := Rect(FWidth - FBorderSize.Right, 0, FWidth, FBorderSize.Top);
  FBottomLeftRect := Rect(0, FHeight - FBorderSize.Bottom, FBorderSize.Left, FHeight);
  FBottomRightRect := Rect(FWidth - FBorderSize.Right, FHeight - FBorderSize.Bottom,
    FWidth, FHeight);
  FTopRect := Rect(FTopLeftRect.Right, 0, FTopRightRect.Left, FBorderSize.Top);
  FLeftRect := Rect(0, FTopLeftRect.Bottom, FBorderSize.Left, FBottomLeftRect.Top);
  FRightRect := Rect(FWidth - FBorderSize.Right, FTopRightRect.Bottom, FWidth, FBottomRightRect.Top);
  FBottomRect := Rect(FBottomLeftRect.Right, FHeight - FBorderSize.Bottom, FBottomRightRect.Left, FHeight);

  if FTopLeftRect.Contains(P) then
    Result := HTTOPLEFT
  else if FTopRightRect.Contains(P) then
    Result := HTTOPRIGHT
  else if FBottomLeftRect.Contains(P) then
    Result := HTBOTTOMLEFT
   else if FBottomRightRect.Contains(P) then
    Result := HTBOTTOMRIGHT
  else if FLeftRect.Contains(P) then
    Result := HTLEFT
  else if FRightRect.Contains(P) then
    Result := HTRIGHT
  else if FBottomRect.Contains(P) then
    Result := HTBOTTOM
  else if FTopRect.Contains(P) then
    Result := HTTOP;
end;

procedure TColorizerFormStyleHook.CMDialogChar(var Message: TWMKey);
begin
end;

procedure TColorizerFormStyleHook.WMSetText(var Message: TMessage);
var
  FRedraw: Boolean;
begin
  if not IsStyleBorder then
  begin
    Handled := False;
    Exit;
  end;
  FRedraw := True;
  if not (fsShowing in Form.FormState) and IsWindowVisible(Form.Handle) then
  begin
    if not SameText(Form.ClassName, 'TEditWindow') then
     Application.ProcessMessages;
    FRedraw := False;
    SetRedraw(False);
  end;
  CallDefaultProc(Message);
  if not (fsShowing in Form.FormState) and not FRedraw then
  begin
    SetRedraw(True);
    InvalidateNC;
  end;
  Handled := True;
end;

procedure TColorizerFormStyleHook.WMMDIChildClose(var Message: TMessage);

function IsAnyMDIChildMaximized: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to TCustomFormClass(Form).MDIChildCount - 1 do
    if (FChangeVisibleChildHandle <> TCustomFormClass(Form).MDIChildren[I].Handle) and
       (TCustomFormClass(Form).MDIChildren[I].Visible) and
       (TCustomFormClass(Form).MDIChildren[I].WindowState = wsMaximized) then
    begin
      Result := True;
      Break;
    end;
end;

begin
  FChangeVisibleChildHandle := Message.WParam;
  GetMDIScrollInfo(True);
end;

procedure TColorizerFormStyleHook.WMDestroy(var Message: TMessage);
begin
 if not (csRecreating in Form.ControlState) and (TCustomFormClass(Form).FormStyle = fsMDIChild) then
   PostMessage(Application.MainForm.Handle, WM_MDICHILDCLOSE, 0, 0);
end;

procedure TColorizerFormStyleHook.WMSysCommand(var Message: TMessage);
begin
  if IsStyleBorder then
    case Message.WParam  of
      SC_CLOSE:
        if TCustomFormClass(Form).FormStyle = fsMDIChild then
         PostMessage(Application.MainForm.Handle, WM_MDICHILDCLOSE,
           Winapi.Windows.WPARAM(Form.Handle), 0);
      SC_MINIMIZE:
        if TCustomFormClass(Form).FormStyle = fsMDIChild then
         FFormActive := False;
      SC_KEYMENU:;
    end;
end;

procedure TColorizerFormStyleHook.WMInitMenu(var Message: TMessage);
begin
  if (WPARAM(GetMenu(Control.Handle)) = Message.wParam) and IsStyleBorder then
    SetMenu(Control.Handle, 0);
end;

procedure TColorizerFormStyleHook.CMMenuChanged(var Message: TMessage);
begin
  if IsStyleBorder then
  begin
    if GetMenu(Control.Handle) <> 0 then
      SetMenu(Control.Handle, 0);
     Handled := True;
  end;
end;

procedure TColorizerFormStyleHook.WMNCHitTest(var Message: TWMNCHitTest);
var
  P: TPoint;
begin
  if IsStyleBorder then
  begin
    P := NormalizePoint(Point(Message.XPos, Message.YPos));
    Message.Result := GetHitTest(P);
    Handled := True;
  end;
end;

procedure TColorizerFormStyleHook.WMNCCalcSize(var Message: TWMNCCalcSize);
var
  Params: PNCCalcSizeParams;
  R : TRect;
  MenuHeight: Integer;
begin
  if not IsStyleBorder then
  begin
    Handled := False;
    Exit;
  end;

  {calc NC info}
  if (Message.CalcValidRects and (Form.BorderStyle <> bsNone)) then
  begin
    R := GetBorderSize;
    MenuHeight := 0;
    Params := Message.CalcSize_Params;
    with Params^.rgrc[0] do
    begin
      Inc(Left, R.Left);
      Inc(Top, R.Top + MenuHeight);
      Dec(Right, R.Right);
      Dec(Bottom, R.Bottom);
    end;
    Handled := True;
  end;
end;

function TColorizerFormStyleHook.GetIconFast: TIcon;
begin
  if (FIcon = nil) or (FIconHandle = 0) then
    Result := GetIcon
  else
    Result := FIcon;
end;

function TColorizerFormStyleHook.GetIcon: TIcon;
var
  IconX, IconY: Integer;
  TmpHandle: THandle;
  Info: TWndClassEx;
  Buffer: array [0..255] of Char;
begin
  TmpHandle := THandle(SendMessage(Handle, WM_GETICON, ICON_SMALL, 0));
  if TmpHandle = 0 then
    TmpHandle := THandle(SendMessage(Handle, WM_GETICON, ICON_BIG, 0));
  if TmpHandle = 0 then
  begin
    { Get instance }
    GetClassName(Handle, @Buffer, SizeOf(Buffer));
    FillChar(Info, SizeOf(Info), 0);
    Info.cbSize := SizeOf(Info);

    if GetClassInfoEx(GetWindowLong(Handle, GWL_HINSTANCE), @Buffer, Info) then
    begin
      TmpHandle := Info.hIconSm;
      if TmpHandle = 0 then
        TmpHandle := Info.hIcon;
    end
  end;

  if FIcon = nil then
    FIcon := TIcon.Create;
  if TmpHandle <> 0 then
  begin
    IconX := GetSystemMetrics(SM_CXSMICON);
    if IconX = 0 then
      IconX := GetSystemMetrics(SM_CXSIZE);
    IconY := GetSystemMetrics(SM_CYSMICON);
    if IconY = 0 then
      IconY := GetSystemMetrics(SM_CYSIZE);
    FIcon.Handle := CopyImage(TmpHandle, IMAGE_ICON, IconX, IconY, 0);
    FIconHandle := TmpHandle;
  end;

  Result := FIcon;
end;

function RectVCenter(var R: TRect; Bounds: TRect): TRect;
begin
  OffsetRect(R, -R.Left, -R.Top);
  OffsetRect(R, 0, (Bounds.Height - R.Height) div 2);
  OffsetRect(R, Bounds.Left, Bounds.Top);

  Result := R;
end;

procedure TColorizerFormStyleHook.PaintNC(Canvas: TCanvas);
var
  Details, CaptionDetails, IconDetails: TThemedElementDetails;
  Detail: TThemedWindow;
  R, R1, DrawRect, ButtonRect, TextRect: TRect;
  CaptionBuffer: TBitmap;
  FButtonState: TThemedWindow;
  TextFormat: TTextFormat;
  LText: string;
  TextTopOffset: Integer;

  function GetTopOffset: Integer;
  var
    P: TPoint;
  begin
    P.X := Form.Left + Form.Width div 2;
    P.Y := Form.Top + Form.Height div 2;
    Result := Screen.MonitorFromPoint(P).WorkareaRect.Top;
    if Form.Top < Result then Result := Result - Form.Top else Result := 0;
  end;

  procedure CorrectRightButtonRect(var AButtonRect: TRect);
  var
    TopOffset, RightOffset: Integer;
    BS: TRect;
  begin
    if (Form.WindowState = wsMaximized) and (TCustomFormClass(Form).FormStyle <> fsMDIChild) and (ButtonRect.Width > 0) then
    begin
      BS := GetBorderSize;
      TopOffset := GetTopOffset;
      RightOffset := -BS.Right;
      if ButtonRect.Top < TopOffset then
      begin
        TopOffset := TopOffset - ButtonRect.Top;
        OffsetRect(ButtonRect, RightOffset, TopOffset);
        TopOffset := ButtonRect.Bottom - BS.Top;
        if TopOffset > 0 then
          OffsetRect(ButtonRect, 0, -TopOffset);
      end;
    end;
  end;

  procedure CorrectLeftButtonRect(var AButtonRect: TRect);
  var
    TopOffset, LeftOffset: Integer;
    BS: TRect;
  begin
    if (Form.WindowState = wsMaximized) and (TCustomFormClass(Form).FormStyle <> fsMDIChild) and (ButtonRect.Width > 0) then
    begin
      BS := GetBorderSize;
      TopOffset := GetTopOffset;
      LeftOffset := BS.Left;
      if ButtonRect.Top < TopOffset then
      begin
        TopOffset := TopOffset - ButtonRect.Top;
        OffsetRect(ButtonRect, LeftOffset, TopOffset);
        TopOffset := ButtonRect.Bottom - BS.Top;
        if TopOffset > 0 then
          OffsetRect(ButtonRect, 0, -TopOffset);
      end;
    end;
  end;

begin
  if Form.BorderStyle = bsNone then
    Exit;

  {init some parameters}
  FCloseButtonRect := Rect(0, 0, 0, 0);
  FMaxButtonRect := Rect(0, 0, 0, 0);
  FMinButtonRect := Rect(0, 0, 0, 0);
  FHelpButtonRect := Rect(0, 0, 0, 0);
  FSysMenuButtonRect := Rect(0, 0, 0, 0);
  FCaptionRect := Rect(0, 0, 0, 0);

  if not ColorizerStyleServices.Available then
    Exit;
  R := GetBorderSize;

  {draw caption}

  if (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
  begin
    if FFormActive then
      Detail := twCaptionActive
    else
      Detail := twCaptionInActive
  end
  else
  begin
   if FFormActive then
      Detail := twSmallCaptionActive
    else
      Detail := twSmallCaptionInActive
  end;
  CaptionBuffer := TBitmap.Create;
  CaptionBuffer.SetSize(FWidth, R.Top);

  {draw caption border}
  DrawRect := Rect(0, 0, CaptionBuffer.Width, CaptionBuffer.Height);
  Details := ColorizerStyleServices.GetElementDetails(Detail);
  ColorizerStyleServices.DrawElement(CaptionBuffer.Canvas.Handle, Details, DrawRect);
  TextRect := DrawRect;
  CaptionDetails := Details;
  TextTopOffset := 3;

  {draw icon}
  if (biSystemMenu in TCustomFormClass(Form).BorderIcons) and
     (Form.BorderStyle <> bsDialog) and
     (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
  begin
    IconDetails := ColorizerStyleServices.GetElementDetails(twSysButtonNormal);
    if not ColorizerStyleServices.GetElementContentRect(0, IconDetails, DrawRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);

    R1 := ButtonRect;

    {$IFDEF DELPHIXE3_UP}
    if not ColorizerStyleServices.HasElementFixedPosition(Details) then
    begin
      CorrectLeftButtonRect(ButtonRect);
      TextTopOffset := Abs(R1.Top - ButtonRect.Top);
      if TextTopOffset > R.Top then TextTopOffset := 3;
    end
    else
      TextTopOffset := 0;
    {$ENDIF}

    R1 := Rect(0, 0, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON));
    RectVCenter(R1, ButtonRect);
    if ButtonRect.Width > 0 then
      DrawIconEx(CaptionBuffer.Canvas.Handle, R1.Left, R1.Top, GetIconFast.Handle, 0, 0, 0, 0, DI_NORMAL);
    Inc(TextRect.Left, ButtonRect.Width + 5);
    FSysMenuButtonRect := ButtonRect;
  end
  else
    Inc(TextRect.Left, R.Left);

  {draw buttons}
  if (biSystemMenu in TCustomFormClass(Form).BorderIcons) then
  begin
    if (Form.BorderStyle <> bsToolWindow) and
       (Form.BorderStyle <> bsSizeToolWin) then
    begin
      if (FPressedButton = HTCLOSE) and (FHotButton = HTCLOSE) then
        FButtonState := twCloseButtonPushed
      else if FHotButton = HTCLOSE then
        FButtonState := twCloseButtonHot
      else
        if FFormActive then
          FButtonState := twCloseButtonNormal
        else
          FButtonState := twCloseButtonDisabled;
     end
    else
    begin
      if (FPressedButton = HTCLOSE) and (FHotButton = HTCLOSE) then
        FButtonState := twSmallCloseButtonPushed
      else if FHotButton = HTCLOSE then
        FButtonState := twSmallCloseButtonHot
      else
        if FFormActive then
          FButtonState := twSmallCloseButtonNormal
        else
          FButtonState := twSmallCloseButtonDisabled;
    end;

    Details := ColorizerStyleServices.GetElementDetails(FButtonState);
    if not ColorizerStyleServices.GetElementContentRect(0, Details, DrawRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);

    {$IFDEF DELPHIXE3_UP}
    if not ColorizerStyleServices.HasElementFixedPosition(Details) then
      CorrectRightButtonRect(ButtonRect);
    {$ENDIF}

    if ButtonRect.Width > 0 then
      ColorizerStyleServices.DrawElement(CaptionBuffer.Canvas.Handle, Details, ButtonRect);

    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
    FCloseButtonRect := ButtonRect;
  end;

  if (biMaximize in TCustomFormClass(Form).BorderIcons) and
     (biSystemMenu in TCustomFormClass(Form).BorderIcons) and
     (Form.BorderStyle <> bsDialog) and
     (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
  begin
    if Form.WindowState = wsMaximized then
    begin
      if (FPressedButton = HTMAXBUTTON) and (FHotButton = HTMAXBUTTON) then
        FButtonState := twRestoreButtonPushed
      else if FHotButton = HTMAXBUTTON then
        FButtonState := twRestoreButtonHot
      else
      if FFormActive then
        FButtonState := twRestoreButtonNormal
      else
        FButtonState := twRestoreButtonDisabled;
    end
    else
    begin
      if (FPressedButton = HTMAXBUTTON) and (FHotButton = HTMAXBUTTON) then
        FButtonState := twMaxButtonPushed
      else if FHotButton = HTMAXBUTTON then
        FButtonState := twMaxButtonHot
      else
      if FFormActive then
        FButtonState := twMaxButtonNormal
      else
        FButtonState := twMaxButtonDisabled;
    end;
    Details := ColorizerStyleServices.GetElementDetails(FButtonState);

    if not ColorizerStyleServices.GetElementContentRect(0, Details, DrawRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);

    {$IFDEF DELPHIXE3_UP}
    if not ColorizerStyleServices.HasElementFixedPosition(Details) then
     CorrectRightButtonRect(ButtonRect);
    {$ENDIF}
    if ButtonRect.Width > 0 then
      ColorizerStyleServices.DrawElement(CaptionBuffer.Canvas.Handle, Details, ButtonRect);
    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
    FMaxButtonRect := ButtonRect;
  end;

  if (biMinimize in TCustomFormClass(Form).BorderIcons) and
     (biSystemMenu in TCustomFormClass(Form).BorderIcons) and
     (Form.BorderStyle <> bsDialog) and
     (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
  begin
    if (FPressedButton = HTMINBUTTON) and (FHotButton = HTMINBUTTON) then
      FButtonState := twMinButtonPushed
    else if FHotButton = HTMINBUTTON then
      FButtonState := twMinButtonHot
    else
      if FFormActive then
        FButtonState := twMinButtonNormal
      else
        FButtonState := twMinButtonDisabled;

    Details := ColorizerStyleServices.GetElementDetails(FButtonState);

    if not ColorizerStyleServices.GetElementContentRect(0, Details, DrawRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);

    {$IFDEF DELPHIXE3_UP}
    if not ColorizerStyleServices.HasElementFixedPosition(Details) then
      CorrectRightButtonRect(ButtonRect);
    {$ENDIF}

    if ButtonRect.Width > 0 then
      ColorizerStyleServices.DrawElement(CaptionBuffer.Canvas.Handle, Details, ButtonRect);
    if ButtonRect.Left > 0 then TextRect.Right := ButtonRect.Left;
    FMinButtonRect := ButtonRect;
  end;

  if (biHelp in TCustomFormClass(Form).BorderIcons) and (biSystemMenu in TCustomFormClass(Form).BorderIcons) and
     ((not (biMaximize in TCustomFormClass(Form).BorderIcons) and
     not (biMinimize in TCustomFormClass(Form).BorderIcons)) or (Form.BorderStyle = bsDialog))
  then
  begin
    if (FPressedButton = HTHELP) and (FHotButton = HTHELP) then
      FButtonState := twHelpButtonPushed
    else if FHotButton = HTHELP then
      FButtonState := twHelpButtonHot
    else
    if FFormActive then
      FButtonState := twHelpButtonNormal
    else
      FButtonState := twHelpButtonDisabled;
    Details := ColorizerStyleServices.GetElementDetails(FButtonState);

    if not ColorizerStyleServices.GetElementContentRect(0, Details, DrawRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);

    {$IFDEF DELPHIXE3_UP}
    if not ColorizerStyleServices.HasElementFixedPosition(Details) then
      CorrectRightButtonRect(ButtonRect);
    {$ENDIF}

    if ButtonRect.Width > 0 then
      ColorizerStyleServices.DrawElement(CaptionBuffer.Canvas.Handle, Details, ButtonRect);

    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
    FHelpButtonRect := ButtonRect;
  end;

  {draw text}
  TextFormat := [tfLeft, tfSingleLine, tfVerticalCenter];
  if Control.UseRightToLeftReading then
    Include(TextFormat, tfRtlReading);
  // Important: Must retrieve Text prior to calling DrawText as it causes
  // CaptionBuffer.Canvas to free its handle, making the outcome of the call
  // to DrawText dependent on parameter evaluation order.
  LText := Text;

  if (Form.WindowState = wsMaximized) and (TCustomFormClass(Form).FormStyle <> fsMDIChild) and
     (TextTopOffset <> 0) and (biSystemMenu in TCustomFormClass(Form).BorderIcons) then
  begin
    Inc(TextRect.Left, R.Left);
    MoveWindowOrg(CaptionBuffer.Canvas.Handle, 0, TextTopOffset);
    ColorizerStyleServices.DrawText(CaptionBuffer.Canvas.Handle, CaptionDetails, LText, TextRect, TextFormat);
    MoveWindowOrg(CaptionBuffer.Canvas.Handle, 0, -TextTopOffset);
  end
  else
    ColorizerStyleServices.DrawText(CaptionBuffer.Canvas.Handle, CaptionDetails, LText, TextRect, TextFormat);

  FCaptionRect := TextRect;

  {draw caption buffer}

  Canvas.Draw(0, 0, CaptionBuffer);
  CaptionBuffer.Free;

  {draw left border}

  if (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
  begin
    if FFormActive then
      Detail := twFrameLeftActive
    else
      Detail := twFrameLeftInActive
  end
  else
  begin
    if FFormActive then
      Detail := twSmallFrameLeftActive
    else
      Detail := twSmallFrameLeftInActive
  end;
  DrawRect := Rect(0, R.Top, R.Left, FHeight - R.Bottom);
  Details := ColorizerStyleServices.GetElementDetails(Detail);

  if DrawRect.Bottom - DrawRect.Top > 0 then
    ColorizerStyleServices.DrawElement(Canvas.Handle, Details, DrawRect);

  {draw right border}
  if (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
  begin
    if FFormActive then
      Detail := twFrameRightActive
    else
      Detail := twFrameRightInActive
  end
  else
  begin
   if FFormActive then
      Detail := twSmallFrameRightActive
    else
      Detail := twSmallFrameRightInActive
  end;
  DrawRect := Rect(FWidth - R.Right, R.Top, FWidth, FHeight - R.Bottom);
  Details := ColorizerStyleServices.GetElementDetails(Detail);

  if DrawRect.Bottom - DrawRect.Top > 0 then
    ColorizerStyleServices.DrawElement(Canvas.Handle, Details, DrawRect);

  {draw Bottom border}
  if (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
  begin
    if FFormActive then
      Detail := twFrameBottomActive
    else
      Detail := twFrameBottomInActive
  end
  else
  begin
   if FFormActive then
      Detail := twSmallFrameBottomActive
    else
      Detail := twSmallFrameBottomInActive
  end;
  DrawRect := Rect(0, FHeight - R.Bottom, FWidth, FHeight);
  Details := ColorizerStyleServices.GetElementDetails(Detail);

  if DrawRect.Bottom - DrawRect.Top > 0 then
    ColorizerStyleServices.DrawElement(Canvas.Handle, Details, DrawRect);
end;

procedure TColorizerFormStyleHook.WMNCACTIVATE(var Message: TMessage);
begin
  if not IsStyleBorder then
  begin
    Handled := False;
    Exit;
  end;

  FFormActive := Message.WParam > 0;

  if (TCustomFormClass(Form).FormStyle = fsMDIChild) then
  begin
    if (TCustomFormClass(Form).FormStyle = fsMDIChild) and (Win32MajorVersion >=6) then
      SetRedraw(False);

    CallDefaultProc(Message);

    if (TCustomFormClass(Form).FormStyle = fsMDIChild) and (Win32MajorVersion >=6) then
    begin
      SetRedraw(True);
      if not (csDestroying in Control.ComponentState) and
         not (csLoading in Control.ComponentState) then
        RedrawWindow(Handle, nil, 0, RDW_INVALIDATE + RDW_ALLCHILDREN + RDW_UPDATENOW);
    end;
  end
  else
    Message.Result := 1;

  if TCustomFormClass(Form).ClientHandle <> 0 then
    PostMessage(TForm(Control).ClientHandle, WM_NCACTIVATE, Message.WParam, Message.LParam);

  if (Form.BorderStyle <> bsNone) and
     not ((TCustomFormClass(Form).FormStyle = fsMDIChild) and
     (Form.WindowState = wsMaximized)) then
    InvalidateNC;

  Handled := True;
end;

function TColorizerFormStyleHook.GetRegion: HRgn;
var
  R: TRect;
  Details: TThemedElementDetails;
  Detail: TThemedWindow;
begin
  Result := 0;
  if not ColorizerStyleServices.Available then
    Exit;

  R := Rect(0, 0, FWidth, FHeight);
  if (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
    Detail := twCaptionActive
  else
    Detail := twSmallCaptionActive;
  Details := ColorizerStyleServices.GetElementDetails(Detail);
  ColorizerStyleServices.GetElementRegion(Details, R, Result);
end;

procedure TColorizerFormStyleHook.ChangeSize;
var
  TempRegion: HRGN;
  R: TRect;
begin
  FChangeSizeCalled := True;
  try
    if IsIconic(Handle) then
     begin
       R := GetBorderSize;
       FHeight := R.Top + R.Bottom;
     end;

    if Form.BorderStyle <> bsNone then
    begin
      TempRegion := FRegion;
      try
        FRegion := GetRegion;
        SetWindowRgn(Handle, FRegion, True);
      finally
        if TempRegion <> 0 then
          DeleteObject(TempRegion);
     end;
    end
    else
    if (Form.BorderStyle = bsNone) and (FRegion <> 0) then
    begin
      SetWindowRgn(Handle, 0, True);
      DeleteObject(FRegion);
      FRegion := 0;
    end;
  finally
    FChangeSizeCalled := False;
  end;
end;

procedure TColorizerFormStyleHook.WMMove(var Message: TWMMOVE);
begin
  if TCustomFormClass(Form).FormStyle = fsMDIChild then
  begin
    CallDefaultProc(TMessage(Message));
    SendMessage(Application.MainForm.Handle, WM_MDICHILDMOVE, 0, 0);
    Handled := True;
  end;
end;

procedure TColorizerFormStyleHook.WMMDIChildMove(var Message: TMessage);
begin
  if (TCustomFormClass(Form).FormStyle = fsMDIForm) and not FStopCheckChildMove then
  begin
    FChangeVisibleChildHandle := Message.WParam;
    GetMDIScrollInfo(True);
    FChangeVisibleChildHandle := 0;
    Handled := True;
  end;
end;

procedure TColorizerFormStyleHook.WMSize(var Message: TWMSize);
begin
  if IsIconic(Handle) and (Application.MainForm.Handle <> Handle) and IsStyleBorder then
    InvalidateNC;

  if (FMDIClientInstance <> nil) then
  begin
    CallDefaultProc(TMessage(Message));
    GetMDIScrollInfo(True);
    Handled := True;
    Exit;
  end;

  if TCustomFormClass(Form).FormStyle = fsMDIChild then
  begin
    CallDefaultProc(TMessage(Message));
    SendMessage(Application.MainForm.Handle, WM_MDICHILDMOVE, 0, 0);
    if IsIconic(Handle) and IsStyleBorder then
      InvalidateNC;
    Handled := True;
  end;

end;

procedure TColorizerFormStyleHook.WMWindowPosChanging(var Message: TWMWindowPosChanging);
var
  Changed: Boolean;
begin
  if not IsStyleBorder then
  begin
    Handled := False;
    Exit;
  end;

  CallDefaultProc(TMessage(Message));

  Handled := True;
  Changed := False;

  if FChangeSizeCalled then
    Exit;

  if (Message.WindowPos^.flags and SWP_NOSIZE = 0) or
     (Message.WindowPos^.flags and SWP_NOMOVE = 0) then
  begin
    if (Message.WindowPos^.flags and SWP_NOMOVE = 0) then
    begin
      FLeft := Message.WindowPos^.x;
      FTop := Message.WindowPos^.y;
    end;
    if (Message.WindowPos^.flags and SWP_NOSIZE = 0) then
    begin
      Changed := ((Message.WindowPos^.cx <> FWidth) or (Message.WindowPos^.cy <> FHeight)) and
                 (Message.WindowPos^.flags and SWP_NOSIZE = 0);
      FWidth := Message.WindowPos^.cx;
      FHeight := Message.WindowPos^.cy;
    end;
  end;

  if (Message.WindowPos^.flags and SWP_FRAMECHANGED  <> 0) then
    Changed := True;

  if Changed then
  begin
    ChangeSize;
    if Form.BorderStyle <> bsNone then
      InvalidateNC;
  end;
end;

procedure TColorizerFormStyleHook.WndProc(var Message: TMessage);
begin
  // Reserved for potential updates
  inherited;
  case Message.Msg of
    WM_WINDOWPOSCHANGED:
      if IsStyleBorder and (Form.WindowState = wsMaximized) then
        with TWMWindowPosChanged(Message) do
          if (WindowPos^.flags and SWP_NOSIZE = 0) or
             (WindowPos^.flags and SWP_NOMOVE = 0) then
          begin
            if (WindowPos^.flags and SWP_NOMOVE = 0) then
            begin
              FLeft := WindowPos^.x;
              FTop := WindowPos^.y;
            end;
            if (WindowPos^.flags and SWP_NOSIZE = 0) then
            begin
              FWidth := WindowPos^.cx;
              FHeight := WindowPos^.cy;
            end;
          end;
  end;
end;

procedure TColorizerFormStyleHook.UpdateForm;
begin
  if Form.BorderStyle = bsNone then Exit;

  Control.Width := Control.Width - 1;
  Control.Width := Control.Width + 1;
end;

procedure TColorizerFormStyleHook.WMNCMouseMove(var Message: TWMNCHitMessage);
begin
  if not IsStyleBorder then
  begin
    Handled := False;
    Exit;
  end;
  inherited;

  if (Message.HitTest = HTCLOSE) or (Message.HitTest = HTMAXBUTTON) or
     (Message.HitTest = HTMINBUTTON) or (Message.HitTest = HTHELP) then
  begin
    if FHotButton <> Message.HitTest then
    begin
      FHotButton := Message.HitTest;
      InvalidateNC;
    end;
    Message.Result := 0;
    Message.Msg := WM_NULL;
    Handled := True;
  end
  else if FHotButton <> 0 then
   begin
     FHotButton := 0;
     InvalidateNC;
   end;
end;

procedure TColorizerFormStyleHook.WMNCRButtonDown(var Message: TWMNCHitMessage);
begin
  if not IsStyleBorder then
  begin
    Handled := False;
    Exit;
  end;
  inherited;
end;

procedure TColorizerFormStyleHook.WMNCLButtonDown(var Message: TWMNCHitMessage);
begin
  if not IsStyleBorder then
  begin
    Handled := False;
    Exit;
  end;

  inherited;

  if (Message.HitTest = HTCLOSE) or (Message.HitTest = HTMAXBUTTON) or
     (Message.HitTest = HTMINBUTTON) or (Message.HitTest = HTHELP) then
  begin
    FPressedButton := Message.HitTest;
    InvalidateNC;
    Message.Result := 0;
    Message.Msg := WM_NULL;
    Handled := True;
  end;

end;

procedure TColorizerFormStyleHook.WMNCRButtonUp(var Message: TWMNCHitMessage);
begin
  if not IsStyleBorder then
  begin
    Handled := False;
    Exit;
  end;

  // call system menu
  if (Message.HitTest = HTCAPTION) and FCaptionEmulation then
  begin
    SendMessage(Handle, $313, 0,
      MakeLong(Message.XCursor, Message.YCursor));
  end;
end;

procedure TColorizerFormStyleHook.WMNCLButtonUp(var Message: TWMNCHitMessage);
var
  FWasPressedButton: Integer;
begin
  if not IsStyleBorder then
  begin
    Handled := False;
    Exit;
  end;

  FWasPressedButton := FPressedButton;

  if FPressedButton <> 0 then
  begin
    FPressedButton := 0;
    InvalidateNC;
  end;

  if (Message.HitTest = HTTOP) or (Message.HitTest = HTBOTTOM) or (Message.HitTest = HTLEFT) or
     (Message.HitTest = HTRIGHT) or (Message.HitTest = HTCAPTION) or (Message.HitTest = HTTOPLEFT) or
     (Message.HitTest = HTTOPRIGHT) or (Message.HitTest = HTBOTTOMRIGHT) or
     (Message.HitTest = HTBOTTOMLEFT) or (Message.HitTest = HTSYSMENU) then
  begin
    Exit;
  end;

  if FWasPressedButton = FHotButton then
    if Message.HitTest = HTCLOSE then
      Close
    else if (Message.HitTest = HTMAXBUTTON) and (biMaximize in TCustomFormClass(Form).BorderIcons) then
    begin
      if Form.WindowState <> wsMaximized then
        Maximize
      else
        Restore;
    end
    else if (Message.HitTest = HTMINBUTTON) and (biMinimize in TCustomFormClass(Form).BorderIcons) then
    begin
      if Form.WindowState <> wsMinimized then
        Minimize
      else
        Restore;
    end
    else if (Message.HitTest = HTHELP) and (biHelp in TCustomFormClass(Form).BorderIcons) then
      Help;

  Message.Result := 0;
  Message.Msg := WM_NULL;
  Handled := True;
end;

procedure TColorizerFormStyleHook.WMNCLButtonDblClk(var Message: TWMNCHitMessage);
begin
  inherited;

  if (Message.HitTest = HTTOP) or (Message.HitTest = HTBOTTOM) or (Message.HitTest = HTLEFT) or
     (Message.HitTest = HTRIGHT) or (Message.HitTest = HTCAPTION) or (Message.HitTest = HTTOPLEFT) or
     (Message.HitTest = HTTOPRIGHT) or (Message.HitTest = HTBOTTOMRIGHT) or (Message.HitTest = HTBOTTOMLEFT) then
  begin
    Exit;
  end;

  Message.Result := 0;
  Message.Msg := WM_NULL;
  Handled := True;
end;

procedure TColorizerFormStyleHook.MouseEnter;
begin
  inherited;
  FPressedButton := 0;
end;

procedure TColorizerFormStyleHook.MouseLeave;
begin
  inherited;
  if FHotButton <> 0 then
  begin
    FHotButton := 0;
    FPressedButton := 0;
    if Form.BorderStyle <> bsNone then
      InvalidateNC;
  end;
end;

procedure TColorizerFormStyleHook.WMActivate(var Message: TWMActivate);
begin
  if IsStyleBorder then
  begin
    CallDefaultProc(TMessage(Message));
    FFormActive := Message.Active > 0;
    Handled := True;
  end;
end;

 procedure TColorizerFormStyleHook.WMNCUAHDrawCaption(var Message: TMessage);
 begin
   if IsStyleBorder then
   begin
     InvalidateNC;
     Handled := True;
   end;
 end;

procedure TColorizerFormStyleHook.Close;
begin
  if Handle <> 0 then
    SendMessage(Handle, WM_SYSCOMMAND, SC_CLOSE, 0);
end;

procedure TColorizerFormStyleHook.Restore;
begin
  FPressedButton := 0;
  FHotButton := 0;

  if Handle <> 0 then
    SendMessage(Handle, WM_SYSCOMMAND, SC_RESTORE, 0);
end;

procedure TColorizerFormStyleHook.Maximize;
begin
  if Handle <> 0 then
  begin
    FPressedButton := 0;
    FHotButton := 0;

    if IsZoomed(Handle) then
      SendMessage(Handle, WM_SYSCOMMAND, SC_RESTORE, 0)
    else
      SendMessage(Handle, WM_SYSCOMMAND, SC_MAXIMIZE, 0);
  end;
end;

procedure TColorizerFormStyleHook.Minimize;
begin
  if Handle <> 0 then
  begin
    FPressedButton := 0;
    FHotButton := 0;
    if IsIconic(Handle) then
      SendMessage(Handle, WM_SYSCOMMAND, SC_RESTORE, 0)
    else
      SendMessage(Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
   end;
end;

procedure TColorizerFormStyleHook.Help;
begin
  SendMessage(Handle, WM_SYSCOMMAND, SC_CONTEXTHELP, 0)
end;

procedure TColorizerFormStyleHook.WMShowWindow(var Message: TWMShowWindow);
begin
  if Message.Show and FNeedsUpdate then
  begin
    FNeedsUpdate := False;
    if (Control is TForm) and (TForm(Control).FormStyle = fsMDIForm) and (FMDIClientInstance = nil) then
    begin
      FMDIPrevClientProc := Pointer(GetWindowLong(TForm(Control).ClientHandle, GWL_WNDPROC));
      FMDIClientInstance := MakeObjectInstance(MDIClientWndProc);
      SetWindowLong(TForm(Control).ClientHandle, GWL_WNDPROC, IntPtr(FMDIClientInstance));
      InitMDIScrollBars;
      AdjustMDIScrollBars;
    end;
    if IsStyleBorder and not TStyleManager.SystemStyle.Enabled and (GetWindowLong(Handle, GWL_STYLE) and WS_CAPTION <> 0) and
       not (TCustomFormClass(Form).FormStyle = fsMDIChild) then
    begin
      FCaptionEmulation := True;
      SetWindowLong(Handle, GWL_STYLE,
           GetWindowLong(Handle, GWL_STYLE) and not WS_CAPTION);
    end;
    UpdateForm;
  end;
end;

procedure TColorizerFormStyleHook.WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
var
  R: TRect;
  MM: PMinMaxInfo;
begin
  if IsStyleBorder then
  begin
    CallDefaultProc(TMessage(Message));
    R := GetBorderSize;
    MM := Message.MinMaxInfo;
    MM^.ptMinTrackSize.y := R.Top + R.Bottom;
    Handled := True;
  end;
end;

{ TColorizerCheckBoxStyleHook }

procedure TColorizerCheckBoxStyleHook.BMSetCheck(var Message: TMessage);
begin
  SetRedraw(False);
  CallDefaultProc(TMessage(Message));
  SetRedraw(True);
  Invalidate;
  Handled := True;
end;

constructor TColorizerCheckBoxStyleHook.Create(AControl: TWinControl);
begin
  inherited;
  OverridePaint := True;
  OverrideEraseBkgnd := True;
  DoubleBuffered := True;
end;


function TColorizerCheckBoxStyleHook.GetDrawState(
  State: TCheckBoxState): TThemedButton;
begin
  Result := tbButtonDontCare;

  if not Control.Enabled then
    case State of
      cbUnChecked: Result := tbCheckBoxUncheckedDisabled;
      cbChecked: Result := tbCheckBoxCheckedDisabled;
      cbGrayed: Result := tbCheckBoxMixedDisabled;
    end
  else if Pressed and MouseInControl then
    case State of
      cbUnChecked: Result := tbCheckBoxUncheckedPressed;
      cbChecked: Result := tbCheckBoxCheckedPressed;
      cbGrayed: Result := tbCheckBoxMixedPressed;
    end
  else if MouseInControl then
    case State of
      cbUnChecked: Result := tbCheckBoxUncheckedHot;
      cbChecked: Result := tbCheckBoxCheckedHot;
      cbGrayed: Result := tbCheckBoxMixedHot;
    end
  else
    case State of
      cbUnChecked: Result := tbCheckBoxUncheckedNormal;
      cbChecked: Result := tbCheckBoxCheckedNormal;
      cbGrayed: Result := tbCheckBoxMixedNormal;
    end;
end;

procedure TColorizerCheckBoxStyleHook.MouseEnter;
begin
  inherited;
  Invalidate;
  Handled := True;
end;


procedure TColorizerCheckBoxStyleHook.MouseLeave;
begin
  inherited;
  Invalidate;
  Handled := True;
end;


procedure TColorizerCheckBoxStyleHook.Paint(Canvas: TCanvas);
var
   State: TCheckBoxState;
   Details: TThemedElementDetails;
   R: TRect;
   Spacing: Integer;
   BoxSize: TSize;
   LCaption: string;
   FWordWrap: Boolean;
   LRect: TRect;
   ElementSize: TElementSize;

   LBuffer : TBitmap;
begin

  if not (TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls) then
  begin
     State := TCheckBoxState(SendMessage(Handle, BM_GETCHECK, 0, 0));
     BoxSize.cx := 13;
     BoxSize.cy := 13;

    if Control is TCustomCheckBox then
      FWordWrap :=  TCustomCheckBoxClass(Control).WordWrap
    else
    if Control is TRadioButton then
      FWordWrap :=  TRadioButton(Control).WordWrap
    else
      FWordWrap := False;

     R := Control.ClientRect;
     R.Width  :=  BoxSize.cx;
     R.Height :=  BoxSize.cy;

    if Control is TCustomCheckBox then
    begin
     Canvas.Brush.Color:= TColorizerLocalSettings.ColorMap.WindowColor;
     Canvas.Pen.Color  := TColorizerLocalSettings.ColorMap.FontColor;
     Canvas.Rectangle(R);

     if State=cbChecked then
       DrawCheck(Canvas, Point(R.Left+3, R.Top+6), 2, False);
    end
    else
    if Control is TRadioButton then
    begin
       Canvas.Brush.Color:= TColorizerLocalSettings.ColorMap.WindowColor;
       Canvas.Pen.Color  := TColorizerLocalSettings.ColorMap.FontColor;

       LBuffer:=TBitmap.Create;
       try
         LBuffer.SetSize(BoxSize.cx, BoxSize.cy);
         LRect := Rect(0, 0, BoxSize.cx, BoxSize.cy);
         LBuffer.Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.WindowColor;
         LBuffer.Canvas.FillRect(LRect);
         LBuffer.Canvas.Pen.Color  :=TColorizerLocalSettings.ColorMap.FontColor;
         LBuffer.Canvas.Ellipse(0, 0, BoxSize.cx, BoxSize.cy);

         if State=cbChecked then
         begin
           LBuffer.Canvas.Brush.Color:= LBuffer.Canvas.Pen.Color;
           LBuffer.Canvas.Ellipse(3, 3, 3 + BoxSize.cx div 2 ,3 + BoxSize.cy div 2);
         end;

          if not RightAlignment then
          begin
            R := Rect(0, 0, BoxSize.cx, BoxSize.cy);
            RectVCenter(R, Rect(0, 0, Control.Width, Control.Height));
          end
          else
          begin
            R := Rect(Control.Width - BoxSize.cx - 1, 0, Control.Width, Control.Height);
            RectVCenter(R, Rect(Control.Width - BoxSize.cy - 1, 0, Control.Width, Control.Height));
          end;

         RectCenter(LRect, R);
         BitBlt(Canvas.Handle, LRect.Left, LRect.Top, BoxSize.cx, BoxSize.cy, LBuffer.Canvas.Handle, 0, 0, SRCCOPY);
       finally
         LBuffer.Free;
       end;
     end;

    Spacing := 3;
    R := Rect(0, 0, Control.Width - BoxSize.cx - 10, Control.Height);
    Canvas.Font := TWinControlClass(Control).Font;
    LCaption := Text;
    if FWordWrap then
      DrawText(Canvas.Handle, PWideChar(LCaption), Length(LCaption), R,
        Control.DrawTextBiDiModeFlags(DT_CALCRECT or DT_EXPANDTABS or DT_WORDBREAK))
    else
      DrawText(Canvas.Handle, PWideChar(LCaption), Length(LCaption), R,
        Control.DrawTextBiDiModeFlags(DT_CALCRECT or DT_EXPANDTABS));

     //AddLog2(Format('%s R.Left %d R.Top %d R.Width %d R.Height %d', ['1', R.Left, R.Top, R.Width, R.Height]));

    if not RightAlignment then
      RectVCenter(R, Rect(BoxSize.cx + Spacing, 0, Control.Width, Control.Height))
    else
     begin
       if Control.BiDiMode <> bdRightToLeft then
         RectVCenter(R, Rect(3, 0,
           Control.Width - BoxSize.cx - Spacing, Control.Height))
       else
         RectVCenter(R, Rect(Control.Width - BoxSize.cx - Spacing - R.Right, 0,
           Control.Width - BoxSize.cx - Spacing, Control.Height));
     end;


     //AddLog2(Format('%s R.Left %d R.Top %d R.Width %d R.Height %d', [LCaption, R.Left, R.Top, R.Width, R.Height]));
    if FWordWrap then
      _DrawControlText(Canvas, LCaption, R, Control.DrawTextBiDiModeFlags(DT_LEFT or DT_VCENTER or DT_EXPANDTABS or DT_WORDBREAK), TColorizerLocalSettings.ColorMap.FontColor)
    else
      _DrawControlText(Canvas, LCaption, R, Control.DrawTextBiDiModeFlags(DT_LEFT or DT_VCENTER or DT_EXPANDTABS), TColorizerLocalSettings.ColorMap.FontColor);


    if Control.Focused then
    begin
      InflateRect(R, 2, 1);
      if R.Top < 0 then
        R.Top := 0;
      if R.Bottom > Control.Height then
        R.Bottom := Control.Height;
      Canvas.Brush.Color := TColorizerLocalSettings.ColorMap.FontColor;
      Canvas.DrawFocusRect(R);
    end;
  end
  else
  if ColorizerStyleServices.Available then
  begin
    State := TCheckBoxState(SendMessage(Handle, BM_GETCHECK, 0, 0));
    Details := ColorizerStyleServices.GetElementDetails(GetDrawState(State));

    if Control is TCustomCheckBox then
      FWordWrap :=  TCustomCheckBoxClass(Control).WordWrap
    else
    if Control is TRadioButton then
      FWordWrap :=  TRadioButton(Control).WordWrap
    else
      FWordWrap := False;

    Spacing := 3;
    LRect := System.Classes.Rect(0, 0, 20, 20);
    ElementSize := esActual;
    R := Control.ClientRect;
    with ColorizerStyleServices do
      if not GetElementSize(Canvas.Handle, GetElementDetails(tbCheckBoxCheckedNormal),
         LRect, ElementSize, BoxSize) then
      begin
        BoxSize.cx := 13;
        BoxSize.cy := 13;
      end;
    if not RightAlignment then
    begin
      R := Rect(0, 0, BoxSize.cx, BoxSize.cy);
      RectVCenter(R, Rect(0, 0, Control.Width, Control.Height));
    end
    else
    begin
      R := Rect(Control.Width - BoxSize.cx - 1, 0, Control.Width, Control.Height);
      RectVCenter(R, Rect(Control.Width - BoxSize.cy - 1, 0, Control.Width, Control.Height));
    end;

    ColorizerStyleServices.DrawElement(Canvas.Handle, Details, R);
    Canvas.Font := TWinControlClass(Control).Font;

    R := Rect(0, 0, Control.Width - BoxSize.cx - 10, Control.Height);
    LCaption := Text;
    if FWordWrap then
      DrawText(Canvas.Handle, PWideChar(LCaption), Length(LCaption), R,
        Control.DrawTextBiDiModeFlags(DT_CALCRECT or DT_EXPANDTABS or DT_WORDBREAK))
    else
      DrawText(Canvas.Handle, PWideChar(LCaption), Length(LCaption), R,
        Control.DrawTextBiDiModeFlags(DT_CALCRECT or DT_EXPANDTABS));

    if not RightAlignment then
      RectVCenter(R, Rect(BoxSize.cx + Spacing, 0, Control.Width, Control.Height))
    else
     begin
       if Control.BiDiMode <> bdRightToLeft then
         RectVCenter(R, Rect(3, 0,
           Control.Width - BoxSize.cx - Spacing, Control.Height))
       else
         RectVCenter(R, Rect(Control.Width - BoxSize.cx - Spacing - R.Right, 0,
           Control.Width - BoxSize.cx - Spacing, Control.Height));
     end;

    if FWordWrap then
      DrawControlText(Canvas, Details, LCaption, R, Control.DrawTextBiDiModeFlags(DT_LEFT or DT_VCENTER or DT_EXPANDTABS or DT_WORDBREAK))
    else
      DrawControlText(Canvas, Details, LCaption, R, Control.DrawTextBiDiModeFlags(DT_LEFT or DT_VCENTER or DT_EXPANDTABS));

    if Control.Focused then
    begin
      InflateRect(R, 2, 1);
      if R.Top < 0 then
        R.Top := 0;
      if R.Bottom > Control.Height then
        R.Bottom := Control.Height;
      Canvas.Brush.Color := ColorizerStyleServices.GetSystemColor(clBtnFace);
      Canvas.DrawFocusRect(R);
    end;
  end;
end;

procedure TColorizerCheckBoxStyleHook.PaintBackground(Canvas: TCanvas);
var
  Details:  TThemedElementDetails;
var
  R: TRect;
begin
  if not (TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls) then
  begin
   R := Rect(0, 0, Control.ClientWidth, Control.ClientHeight);
   Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.Color;
   Canvas.FillRect(R);
  end
  else
  if ColorizerStyleServices.Available then
  begin
    Details.Element := teButton;
    if ColorizerStyleServices.HasTransparentParts(Details) then
        ColorizerStyleServices.DrawParentBackground(Handle, Canvas.Handle, Details, False);
  end;
end;

function TColorizerCheckBoxStyleHook.RightAlignment: Boolean;
begin
  Result := (Control.BiDiMode = bdRightToLeft) or
            (GetWindowLong(Handle, GWL_STYLE) and BS_RIGHTBUTTON = BS_RIGHTBUTTON);
end;

procedure TColorizerCheckBoxStyleHook.WMKeyDown(var Message: TWMKeyDown);
begin
  if Message.CharCode = VK_SPACE then
    SetRedraw(False);
  CallDefaultProc(TMessage(Message));
  if Message.CharCode = VK_SPACE then
  begin
    SetRedraw(True);
    Invalidate;
  end;
  Handled := True;
end;

procedure TColorizerCheckBoxStyleHook.WMKeyUp(var Message: TWMKeyUp);
begin
  if Message.CharCode = VK_SPACE then
    SetRedraw(False);
  CallDefaultProc(TMessage(Message));
  if Message.CharCode = VK_SPACE then
  begin
    SetRedraw(True);
    Invalidate;
  end;
  Handled := True;
end;

procedure TColorizerCheckBoxStyleHook.WMLButtonDblClk(var Message: TWMMouse);
begin
  SetRedraw(False);
  CallDefaultProc(TMessage(Message));
  SetRedraw(True);
  Invalidate;
  Handled := True;
end;


procedure TColorizerCheckBoxStyleHook.WMLButtonDown(var Message: TWMMouse);
begin
  SetRedraw(False);
  CallDefaultProc(TMessage(Message));
  SetRedraw(True);
  FPressed := True;
  Invalidate;
  Handled := True;
end;

procedure TColorizerCheckBoxStyleHook.WMLButtonUp(var Message: TWMMouse);
begin
  SetRedraw(False);
  CallDefaultProc(TMessage(Message));
  SetRedraw(True);
  FPressed := False;
  Invalidate;
  Handled := True;
end;


procedure TColorizerCheckBoxStyleHook.WndProc(var Message: TMessage);
begin
  inherited;

end;

{ TColorizerRadioButtonStyleHook }

constructor TColorizerRadioButtonStyleHook.Create(AControl: TWinControl);
begin
  inherited;
  OverridePaint := True;
  OverrideEraseBkgnd := True;
  DoubleBuffered := True;
end;


function TColorizerRadioButtonStyleHook.GetDrawState(
  State: TCheckBoxState): TThemedButton;
begin
  Result := tbButtonDontCare;

  if not Control.Enabled then
    case State of
      cbUnChecked: Result := tbRadioButtonUncheckedDisabled;
      cbChecked: Result := tbRadioButtonCheckedDisabled;
    end
  else if Pressed and MouseInControl then
    case State of
      cbUnChecked: Result := tbRadioButtonUncheckedPressed;
      cbChecked: Result := tbRadioButtonCheckedPressed;
    end
  else if MouseInControl then
    case State of
      cbUnChecked: Result := tbRadioButtonUncheckedHot;
      cbChecked: Result := tbRadioButtonCheckedHot;
    end
  else
    case State of
      cbUnChecked: Result := tbRadioButtonUncheckedNormal;
      cbChecked: Result := tbRadioButtonCheckedNormal;
    end;
end;

procedure TColorizerRadioButtonStyleHook.WndProc(var Message: TMessage);
begin
  inherited;

end;

{ TColorizerStatusBarStyleHook }

constructor TColorizerStatusBarStyleHook.Create(AControl: TWinControl);
begin
  inherited;
  OverridePaint := True;
  DoubleBuffered := True;
end;

procedure TColorizerStatusBarStyleHook.Paint(Canvas: TCanvas);
const
  AlignStyles: array [TAlignment] of Integer = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  R, R1: TRect;
  Res, Count, I: Integer;
  Idx, Flags: Cardinal;
  Details: TThemedElementDetails;
  LText: string;
  Borders: array [0..2] of Integer;
  SaveCanvas: TCanvas;
  LStyleServices : TCustomStyleServices;

      procedure _DrawControlText(Canvas: TCanvas; const S: string; var R: TRect; Flags: Cardinal);
      var
        TextFormat: TTextFormatFlags;
      begin
        Canvas.Font := TWinControlClass(Control).Font;
        TextFormat := TTextFormatFlags(Flags);
        Canvas.Font.Color := TColorizerLocalSettings.ColorMap.FontColor;
        LStyleServices.DrawText(Canvas.Handle, Details, S, R, TextFormat, Canvas.Font.Color);
      end;

begin
  if not (TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls) then
  begin
    LStyleServices:=StyleServices;

    Canvas.Brush.Color := TColorizerLocalSettings.ColorMap.Color;
    Canvas.FillRect(Rect(0, 0, Control.Width, Control.Height));

    if SendMessage(Handle, SB_ISSIMPLE, 0, 0) > 0 then
    begin
      R := Control.ClientRect;
      FillChar(Borders, SizeOf(Borders), 0);
      SendMessage(Handle, SB_GETBORDERS, 0, IntPtr(@Borders));
      R.Left := Borders[0] + Borders[2];
      R.Top := Borders[1];
      R.Bottom := R.Bottom - Borders[1];
      R.Right := R.Right - Borders[2];

      R1 := Control.ClientRect;
      R1.Left := R1.Right - R.Height;
      Canvas.Brush.Color := TColorizerLocalSettings.ColorMap.Color;
      Canvas.FillRect(R1);
      Details := LStyleServices.GetElementDetails(tsPane);//necesary for canvas colors.

      SetLength(LText, Word(SendMessage(Handle, SB_GETTEXTLENGTH, 0, 0)));
      if Length(LText) > 0 then
      begin
       SendMessage(Handle, SB_GETTEXT, 0, IntPtr(@LText[1]));
       Flags := Control.DrawTextBiDiModeFlags(DT_LEFT);
       //AddLog2(Format('TColorizerStatusBarStyleHook.Paint R.Left %d R.Top %d R.Width %d R.Height %d', [R.Left, R.Top, R.Width, R.Height]));
       //AddLog2('LText '+LText);
       //AddLog2('Flags '+IntToStr(Flags));
       _DrawControlText(Canvas, LText, R, Flags);
      end;
    end
    else
    begin

      if Control is TStatusBar then
        Count := TStatusBar(Control).Panels.Count
      else
        Count := SendMessage(Handle, SB_GETPARTS, 0, 0);

      for I := 0 to Count - 1 do
      begin
        R := Rect(0, 0, 0, 0);
        SendMessage(Control.Handle, SB_GETRECT, I, LParam(@R));
        if IsRectEmpty(R) then
          Exit;

        //AddLog2(Format('TColorizerStatusBarStyleHook.Paint R.Left %d R.Top %d R.Width %d R.Height %d', [R.Left, R.Top, R.Width, R.Height]));
        Canvas.Brush.Color := TColorizerLocalSettings.ColorMap.Color;
        Canvas.FillRect(R);

        Details := LStyleServices.GetElementDetails(tsPane);
        InflateRect(R, -1, -1);
        if Control is TCustomStatusBar then
          Flags := Control.DrawTextBiDiModeFlags(AlignStyles[TCustomStatusBar(Control).Panels[I].Alignment])
        else
          Flags := Control.DrawTextBiDiModeFlags(DT_LEFT);
        Idx := I;
        SetLength(LText, Word(SendMessage(Control.Handle, SB_GETTEXTLENGTH, Idx, 0)));
        if Length(LText) > 0 then
        begin
          Res := SendMessage(Control.Handle, SB_GETTEXT, Idx, LParam(@LText[1]));
          if (Res and SBT_OWNERDRAW = 0) then
            _DrawControlText(Canvas, LText, R, Flags)
          else
          if (Control is TCustomStatusBar) and Assigned(TCustomStatusBar(Control).OnDrawPanel) then
          begin
            SaveCanvas  := TCustomStatusBar(Control).Canvas;
            TCustomStatusBar(Control).CanvasRW := Canvas;
            try
              TCustomStatusBar(Control).OnDrawPanel(TCustomStatusBar(Control), TCustomStatusBar(Control).Panels[I], R);
            finally
              TCustomStatusBar(Control).CanvasRW := SaveCanvas;
            end;
          end;
        end
        else if (Control is TCustomStatusBar) then
         if (TCustomStatusBar(Control).Panels[I].Style <> psOwnerDraw) then
           _DrawControlText(Canvas, TCustomStatusBar(Control).Panels[I].Text, R, Flags)
         else
           if Assigned(TCustomStatusBar(Control).OnDrawPanel) then
           begin
             SaveCanvas := TCustomStatusBar(Control).Canvas;
             TCustomStatusBar(Control).CanvasRW := Canvas;
             try
               TCustomStatusBar(Control).OnDrawPanel(TCustomStatusBar(Control), TCustomStatusBar(Control).Panels[I], R);
             finally
               TCustomStatusBar(Control).CanvasRW := SaveCanvas;
             end;
           end;
      end;
    end;

  end
  else
  begin
    if not ColorizerStyleServices.Available then
      Exit;

    Details := ColorizerStyleServices.GetElementDetails(tsStatusRoot);
    ColorizerStyleServices.DrawElement(Canvas.Handle, Details, Rect(0, 0, Control.Width, Control.Height));

    if SendMessage(Handle, SB_ISSIMPLE, 0, 0) > 0 then
    begin
      R := Control.ClientRect;
      FillChar(Borders, SizeOf(Borders), 0);
      SendMessage(Handle, SB_GETBORDERS, 0, IntPtr(@Borders));
      R.Left := Borders[0] + Borders[2];
      R.Top := Borders[1];
      R.Bottom := R.Bottom - Borders[1];
      R.Right := R.Right - Borders[2];

      Details := ColorizerStyleServices.GetElementDetails(tsPane);
      ColorizerStyleServices.DrawElement(Canvas.Handle, Details, R);

      R1 := Control.ClientRect;
      R1.Left := R1.Right - R.Height;
      Details := ColorizerStyleServices.GetElementDetails(tsGripper);
      ColorizerStyleServices.DrawElement(Canvas.Handle, Details, R1);
      Details := ColorizerStyleServices.GetElementDetails(tsPane);
      //AddLog2(Format('* TColorizerStatusBarStyleHook.Paint R.Left %d R.Top %d R.Width %d R.Height %d', [R.Left, R.Top, R.Width, R.Height]));
      SetLength(LText, Word(SendMessage(Handle, SB_GETTEXTLENGTH, 0, 0)));
      if Length(LText) > 0 then
      begin
       SendMessage(Handle, SB_GETTEXT, 0, IntPtr(@LText[1]));
       Flags := Control.DrawTextBiDiModeFlags(DT_LEFT);
       //AddLog2('* Flags '+IntToStr(Flags));
       DrawControlText(Canvas, Details, LText, R, Flags);
      end;
    end
    else
    begin
      if Control is TStatusBar then
        Count := TStatusBar(Control).Panels.Count
      else
        Count := SendMessage(Handle, SB_GETPARTS, 0, 0);
      for I := 0 to Count - 1 do
      begin
        R := Rect(0, 0, 0, 0);
        SendMessage(Handle, SB_GETRECT, I, IntPtr(@R));
        if IsRectEmpty(R) then
          Exit;
        Details := ColorizerStyleServices.GetElementDetails(tsPane);
        ColorizerStyleServices.DrawElement(Canvas.Handle, Details, R);
        if I = Count - 1 then
        begin
          R1 := Control.ClientRect;
          R1.Left := R1.Right - R.Height;
          Details := ColorizerStyleServices.GetElementDetails(tsGripper);
          ColorizerStyleServices.DrawElement(Canvas.Handle, Details, R1);
        end;
        Details := ColorizerStyleServices.GetElementDetails(tsPane);
        InflateRect(R, -1, -1);
        if Control is TCustomStatusBar then
          Flags := Control.DrawTextBiDiModeFlags(AlignStyles[TCustomStatusBar(Control).Panels[I].Alignment])
        else
          Flags := Control.DrawTextBiDiModeFlags(DT_LEFT);
        Idx := I;
        SetLength(LText, Word(SendMessage(Handle, SB_GETTEXTLENGTH, Idx, 0)));
        if Length(LText) > 0 then
        begin
          Res := SendMessage(Handle, SB_GETTEXT, Idx, IntPtr(@LText[1]));
          if (Res and SBT_OWNERDRAW = 0) then
            DrawControlText(Canvas, Details, LText, R, Flags)
          else
          if (Control is TCustomStatusBar) and Assigned(TCustomStatusBar(Control).OnDrawPanel) then
          begin
            SaveCanvas := TCustomStatusBar(Control).Canvas;
            TCustomStatusBar(Control).CanvasRW := Canvas;
            try
              TCustomStatusBar(Control).OnDrawPanel(TCustomStatusBar(Control),
                TCustomStatusBar(Control).Panels[I], R);
            finally
              TCustomStatusBar(Control).CanvasRW := SaveCanvas;
            end;
          end;
        end
        else if (Control is TCustomStatusBar) then
         if (TCustomStatusBar(Control).Panels[I].Style <> psOwnerDraw) then
           DrawControlText(Canvas, Details, TCustomStatusBar(Control).Panels[I].Text, R, Flags)
         else
           if Assigned(TCustomStatusBar(Control).OnDrawPanel) then
           begin
             SaveCanvas := TCustomStatusBar(Control).Canvas;
             TCustomStatusBar(Control).CanvasRW := Canvas;
             try
               TCustomStatusBar(Control).OnDrawPanel(TCustomStatusBar(Control),
                 TCustomStatusBar(Control).Panels[I], R);
             finally
               TCustomStatusBar(Control).CanvasRW := SaveCanvas;
             end;
           end;
      end;
    end;
  end;


end;

procedure TColorizerStatusBarStyleHook.WndProc(var Message: TMessage);
begin
  inherited;

end;



{ TColorizerEditStyleHook }

constructor TColorizerEditStyleHook.Create(AControl: TWinControl);
begin
  inherited;
  OverridePaintNC := True;
  OverrideEraseBkgnd := True;
  UpdateColors;
end;

procedure TColorizerEditStyleHook.MouseEnter;
begin
  InvalidateNC;
end;

procedure TColorizerEditStyleHook.MouseLeave;
begin
  InvalidateNC;
end;

procedure TColorizerEditStyleHook.PaintNC(Canvas: TCanvas);
var
  Details: TThemedElementDetails;
  R: TRect;
begin
  if not (TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls) then
  begin
    R := Rect(0, 0, Control.Width, Control.Height);
    InflateRect(R, -2, -2);
    ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
    Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.WindowColor;
    Canvas.Pen.Color:=TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
    Canvas.Rectangle(Rect(0, 0, Control.Width, Control.Height));
  end
  else
  if ColorizerStyleServices.Available and HasBorder then
  begin
    if Control.Focused then
      Details := ColorizerStyleServices.GetElementDetails(teEditBorderNoScrollFocused)
    else
    if MouseInControl then
      Details := ColorizerStyleServices.GetElementDetails(teEditBorderNoScrollHot)
    else
    if Control.Enabled then
      Details := ColorizerStyleServices.GetElementDetails(teEditBorderNoScrollNormal)
    else
      Details := ColorizerStyleServices.GetElementDetails(teEditBorderNoScrollDisabled);
    R := Rect(0, 0, Control.Width, Control.Height);
    InflateRect(R, -2, -2);
    ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
    ColorizerStyleServices.DrawElement(Canvas.Handle, Details, Rect(0, 0, Control.Width, Control.Height));
  end;
end;

procedure TColorizerEditStyleHook.UpdateColors;
const
  ColorStates: array[Boolean] of TStyleColor = (scEditDisabled, scEdit);
  FontColorStates: array[Boolean] of TStyleFont = (sfEditBoxTextDisabled, sfEditBoxTextNormal);
var
  LStyle: TCustomStyleServices;
begin
  if not (TColorizerLocalSettings.Settings.UseVCLStyles and TColorizerLocalSettings.Settings.VCLStylesControls) then
  begin
    Brush.Color := TColorizerLocalSettings.ColorMap.WindowColor;
    if Control.Enabled then
     FontColor := TColorizerLocalSettings.ColorMap.FontColor
    else
     FontColor := TColorizerLocalSettings.ColorMap.DisabledFontColor;
  end
  else
  begin
    LStyle := ColorizerStyleServices;
    Brush.Color := LStyle.GetStyleColor(ColorStates[Control.Enabled]);
    {$IFDEF DELPHIXE3_UP}
    if seFont in Control.StyleElements then
    {$ENDIF}
      FontColor := LStyle.GetStyleFontColor(FontColorStates[Control.Enabled])
    {$IFDEF DELPHIXE3_UP}
    else
      FontColor := TWinControlClass(Control).Font.Color;
    {$ENDIF}
  end;
end;

procedure TColorizerEditStyleHook.WMNCCalcSize(var Message: TWMNCCalcSize);
var
  Params: PNCCalcSizeParams;
begin
  if (Control is TCustomEdit) then
  begin
    if TCustomEditClass(Control).BevelKind <> bkNone then
    begin
      Params := Message.CalcSize_Params;
      if HasBorder then
        with Params^.rgrc[0] do
        begin
          Inc(Left, 2);
          Inc(Top, 2);
          Dec(Right, 2);
          Dec(Bottom, 2);
        end;
      Handled := True;
    end;
  end;
end;

procedure TColorizerEditStyleHook.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    CN_CTLCOLORMSGBOX..CN_CTLCOLORSTATIC:
      begin
        SetTextColor(Message.WParam, ColorToRGB(FontColor));
        SetBkColor(Message.WParam, ColorToRGB(Brush.Color));
        Message.Result := LRESULT(Brush.Handle);
        Handled := True;
      end;
    CM_ENABLEDCHANGED:
      begin
        UpdateColors;
        Handled := False; // Allow control to handle message
      end
  else
    inherited WndProc(Message);
  end;
  case Message.Msg of
    WM_SIZE:
    begin
    {$IFDEF DELPHIXE3_UP}
      if seBorder in Control.StyleElements then
    {$ENDIF}
      InvalidateNC;
      Handled := False;
    end;
    CM_FOCUSCHANGED:
      if not TStyleManager.SystemStyle.Enabled then
        InvalidateNC;
  end;
end;

end.
