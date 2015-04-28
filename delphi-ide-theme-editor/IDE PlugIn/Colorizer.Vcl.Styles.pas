unit Colorizer.Vcl.Styles;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.CommCtrl,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Themes,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  System.UITypes,
  System.Types,
  Vcl.Graphics;


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

  TColorizerScrollingStyleHook = class(TColorizerMouseTrackControlStyleHook)
  strict private type
    {$REGION 'TScrollWindow'}
    TScrollWindow = class(TWinControl)
    strict private
      FStyleHook: TColorizerScrollingStyleHook;
      FVertical: Boolean;
      procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
      procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
      procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    strict protected
      procedure CreateParams(var Params: TCreateParams); override;
      procedure WndProc(var Message: TMessage); override;
    public
      constructor Create(AOwner: TComponent); override;
      property StyleHook: TColorizerScrollingStyleHook read FStyleHook write FStyleHook;
      property Vertical: Boolean read FVertical write FVertical;
    end;
    {$ENDREGION}
  strict private
    FHorzDownState: TThemedScrollBar;
    FHorzScrollWnd: TScrollWindow;
    FHorzSliderState: TThemedScrollBar;
    FHorzUpState: TThemedScrollBar;
    FLeftButtonDown: Boolean;
    FListPos: Single;
    FPrevScrollPos: Integer;
    FScrollPos: Single;
    FVertDownState: TThemedScrollBar;
    FVertScrollWnd: TScrollWindow;
    FVertSliderState: TThemedScrollBar;
    FVertUpState: TThemedScrollBar;
    FInitingScrollBars: Boolean;
    function GetHorzDownButtonRect: TRect;
    function GetHorzScrollRect: TRect;
    function GetHorzSliderRect: TRect;
    function GetHorzTrackRect: TRect;
    function GetHorzUpButtonRect: TRect;
    function GetParentBounds: TRect;
    function GetVertDownButtonRect: TRect;
    function GetVertScrollRect: TRect;
    function GetVertSliderRect: TRect;
    function GetVertTrackRect: TRect;
    function GetVertUpButtonRect: TRect;
    function IsPopupWindow: Boolean;
    procedure InitScrollBars;
    procedure InitScrollState;
    procedure UpdateScroll;
    procedure CMVisibleChanged(var Msg: TMessage); message CM_VISIBLECHANGED;
    procedure WMKeyDown(var Msg: TMessage); message WM_KEYDOWN;
    procedure WMKeyUp(var Msg: TMessage); message WM_KEYUP;
    procedure WMLButtonDown(var Msg: TWMMouse); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Msg: TWMMouse); message WM_LBUTTONUP;
    procedure WMNCLButtonDown(var Msg: TWMMouse); message WM_NCLBUTTONDOWN;
    procedure WMNCMouseMove(var Msg: TWMMouse); message WM_NCMOUSEMOVE;
    procedure WMNCLButtonUp(var Msg: TWMMouse); message WM_NCLBUTTONUP;
    procedure WMMouseMove(var Msg: TWMMouse); message WM_MOUSEMOVE;
    procedure WMMouseWheel(var Msg: TMessage); message WM_MOUSEWHEEL;
    procedure WMVScroll(var Msg: TMessage); message WM_VSCROLL;
    procedure WMHScroll(var Msg: TMessage); message WM_HSCROLL;
    procedure WMSize(var Msg: TMessage); message WM_SIZE;
    procedure WMMove(var Msg: TMessage); message WM_MOVE;
    procedure WMCaptureChanged(var Msg: TMessage); message WM_CAPTURECHANGED;
    procedure WMNCLButtonDblClk(var Msg: TWMMouse); message WM_NCLBUTTONDBLCLK;
    procedure WMWindowPosChanged(var Msg: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMShowWindow(var Msg: TWMShowWindow); message WM_SHOWWINDOW;
    procedure WMClose(var Msg: TWMCLOSE); message WM_CLOSE;
  strict protected
    procedure DrawBorder; virtual;
    procedure DrawHorzScroll(DC: HDC); virtual;
    procedure DrawVertScroll(DC: HDC); virtual;
    procedure PaintBackground(Canvas: TCanvas); override;
    procedure PaintScroll; virtual;
    procedure Paint(Canvas: TCanvas); override;
    procedure PaintNC(Canvas: TCanvas); override;
    procedure MouseLeave; override;
    procedure WndProc(var Message: TMessage); override;
    property HorzDownButtonRect: TRect read GetHorzDownButtonRect;
    property HorzDownState: TThemedScrollBar read FHorzDownState write FHorzDownState;
    property HorzScrollRect: TRect read GetHorzScrollRect;
    property HorzSliderRect: TRect read GetHorzSliderRect;
    property HorzSliderState: TThemedScrollBar read FHorzSliderState write FHorzSliderState;
    property HorzTrackRect: TRect read GetHorzTrackRect;
    property HorzUpButtonRect: TRect read GetHorzUpButtonRect;
    property HorzUpState: TThemedScrollBar read FHorzUpState write FHorzUpState;
    property LeftButtonDown: Boolean read FLeftButtonDown;
    property ListPos: Single read FListPos write FListPos;
    property ParentBounds: TRect read GetParentBounds;
    property PrevScrollPos: Integer read FPrevScrollPos write FPrevScrollPos;
    property ScrollPos: Single read FScrollPos write FScrollPos;
    property VertDownButtonRect: TRect read GetVertDownButtonRect;
    property VertDownState: TThemedScrollBar read FVertDownState write FVertDownState;
    property VertScrollRect: TRect read GetVertScrollRect;
    property VertSliderRect: TRect read GetVertSliderRect;
    property VertSliderState: TThemedScrollBar read FVertSliderState write FVertSliderState;
    property VertTrackRect: TRect read GetVertTrackRect;
    property VertUpButtonRect: TRect read GetVertUpButtonRect;
    property VertUpState: TThemedScrollBar read FVertUpState write FVertUpState;
  public
    constructor Create(AControl: TWinControl); override;
    destructor Destroy; override;
  end;

  TColorizerListBoxStyleHook = class(TColorizerScrollingStyleHook)
  strict private
    procedure UpdateColors;
  strict protected
    procedure WndProc(var Message: TMessage); override;
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
  public
    constructor Create(AControl: TWinControl); override;
  end;

  TColorizerComboBoxStyleHook = class(TColorizerMouseTrackControlStyleHook)
  strict private
    FDownPos, FMovePos: TPoint;
    FDownSliderPos: Integer;
    FOldIdx, FInvsibleCount, FSliderSize: Integer;
    FVSliderState, FVUpState, FVDownState: TThemedScrollBar;
    FIgnoreStyleChanged: Boolean;
    FMouseOnButton: Boolean;
    FListHandle, FEditHandle: HWnd;
    FListBoxInstance: Pointer;
    FDefListBoxProc: Pointer;
    FListBoxTimerCode: Integer;
    FListBoxUpBtnDown, FListBoxDownBtnDown,
    FListBoxTrackUpDown, FListBoxTrackDownDown: Boolean;
    FTempItemIndex: Integer;
    procedure DrawListBoxVertScroll(DC: HDC);
    procedure DrawListBoxBorder;
    function DroppedDown: Boolean;
    function GetButtonRect: TRect;
    function Style: TComboBoxStyle;
    function ListBoxBoundsRect: TRect;
    function ListBoxClientRect: TRect;
    procedure ListBoxSetTimer(ATimerCode: Integer);
    procedure ListBoxStopTimer;
    function ListBoxVertScrollRect: TRect;
    function ListBoxVertDownButtonRect: TRect;
    function ListBoxVertUpButtonRect: TRect;
    function ListBoxVertScrollArea: TRect;
    function ListBoxVertSliderRect: TRect;
    function ListBoxVertTrackRect: TRect;
    function ListBoxVertTrackRectUp: TRect;
    function ListBoxVertTrackRectDown: TRect;
    procedure PaintListBoxBorder(Canvas: TCanvas; const R: TRect);
    procedure UpdateColors;
    procedure WMCommand(var Message: TWMCommand); message WM_COMMAND;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure WMPaint(var Message: TMessage); message WM_PAINT;
    procedure WMMouseMove(var Message: TWMMouse); message WM_MOUSEMOVE;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure WMDrawItem(var Message: TWMDrawItem); message WM_DRAWITEM;
    procedure WMParentNotify(var Message: TMessage); message WM_PARENTNOTIFY;
  strict protected
    function IsChildHandle(AHandle: HWnd): Boolean; override;
    function AcceptMessage(var Message: TMessage): Boolean; override;
    procedure DrawItem(Canvas: TCanvas; Index: Integer;
      const R: TRect; Selected: Boolean); virtual;
    procedure HookListBox(AListHandle: HWND);
    property ListBoxInstance: Pointer read FListBoxInstance;
    procedure ListBoxWndProc(var Msg: TMessage); virtual;
    property ListHandle: HWND read FListHandle;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure PaintBorder(Canvas: TCanvas); virtual;
    procedure WndProc(var Message: TMessage); override;
    property ButtonRect: TRect read GetButtonRect;
    property MouseOnButton: Boolean read FMouseOnButton write FMouseOnButton;
  public
    constructor Create(AControl: TWinControl); override;
    destructor Destroy; override;
  end;

  TColorizerTrackBarStyleHook = class(TColorizerStyleHook)
  strict private
    FMouseOnThumb: Boolean;
    FThumbPressed: Boolean;
    procedure CNHScroll(var Message: TWMHScroll); message CN_HSCROLL;
    procedure CNVScroll(var Message: TWMVScroll); message CN_VSCROLL;
    procedure WMMouseMove(var Message: TWMMouse); message WM_MOUSEMOVE;
    procedure WMLButtonUp(var Message: TWMMouse); message WM_LBUTTONUP;
    procedure WMLButtonDown(var Message: TWMMouse); message WM_LBUTTONDOWN;
  strict protected
    function AcceptMessage(var Message: TMessage): Boolean; override;
    procedure Paint(Canvas: TCanvas); override;
    procedure PaintBackground(Canvas: TCanvas); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AControl: TWinControl); override;
  end;

  TColorizerTreeViewStyleHook = class(TColorizerScrollingStyleHook)
  strict private
    procedure TVMSetBkColor(var Message: TMessage); message TVM_SETBKCOLOR;
    procedure TVMSetTextColor(var Message: TMessage); message TVM_SETTEXTCOLOR;
    procedure WMMouseMove(var Msg: TWMMouse); message WM_MOUSEMOVE;
  strict protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AControl: TWinControl); override;
  end;



  procedure SetColorizerVCLStyle(const StyleName : string);
  function  ColorizerStyleServices: TCustomStyleServices;

implementation

uses
  System.SysUtils,
  System.TypInfo,
  Buttons,
  Vcl.GraphUtil,
  Vcl.ExtCtrls,
  Colorizer.Utils;

var
   CurrentStyleName : string ='';

type
  TWinControlClass = class(TWinControl);
  TCustomFormClass = class(TCustomForm);
  TCustomCheckBoxClass = class(TCustomCheckBox);
  TCustomComboBoxClass = class(TCustomComboBox);
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
  if FOverridePaint  then
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

{ TColorizerScrollingStyleHook }

procedure TColorizerScrollingStyleHook.CMVisibleChanged(var Msg: TMessage);
begin
  if Control.HandleAllocated then
  begin
    if FVertScrollWnd <> nil then
      if (Control.Visible) then
        ShowWindow(FVertScrollWnd.Handle, SW_SHOW)
      else
        ShowWindow(FVertScrollWnd.Handle, SW_HIDE);

    if FHorzScrollWnd <> nil then
      if (Control.Visible) then
        ShowWindow(FHorzScrollWnd.Handle, SW_SHOW)
      else
        ShowWindow(FHorzScrollWnd.Handle, SW_HIDE);
  end;
  Handled := False;
end;


constructor TColorizerScrollingStyleHook.Create(AControl: TWinControl);
begin
  inherited;
  OverridePaintNC := True;
  FVertScrollWnd := nil;
  FHorzScrollWnd := nil;
  FInitingScrollBars := False;
end;

destructor TColorizerScrollingStyleHook.Destroy;
begin
  FInitingScrollBars := True;
  if FVertScrollWnd <> nil then
  begin
    FVertScrollWnd.StyleHook := nil;
    FreeAndNil(FVertScrollWnd);
  end;
  if FHorzScrollWnd <> nil then
  begin
    FHorzScrollWnd.StyleHook := nil;
    FreeAndNil(FHorzScrollWnd);
  end;
  FInitingScrollBars := False;
  inherited;
end;

procedure TColorizerScrollingStyleHook.DrawBorder;
begin
  if ColorizerStyleServices.Available then
    ColorizerStyleServices.PaintBorder(Control, True);
end;

procedure TColorizerScrollingStyleHook.DrawHorzScroll(DC: HDC);
var
  B: TBitmap;
  Details: TThemedElementDetails;
  R: TRect;
begin
  if Handle = 0 then Exit;
  if DC = 0 then Exit;

  if (HorzScrollRect.Height > 0) and (HorzScrollRect.Width > 0) then
  begin
    B := TBitmap.Create;
    try
      B.Width := HorzScrollRect.Width;
      B.Height := HorzScrollRect.Height;
      MoveWindowOrg(B.Canvas.Handle, -HorzScrollRect.Left, -HorzScrollRect.Top);
      if ColorizerStyleServices.Available then
      begin
        R := HorzScrollRect;
        R.Left := HorzUpButtonRect.Right;
        R.Right := HorzDownButtonRect.Left;
        if (R.Height > 0) and (R.Width > 0) then
        begin
          Details := ColorizerStyleServices.GetElementDetails(tsUpperTrackHorzNormal);
          ColorizerStyleServices.DrawElement(B.Canvas.Handle, Details, R);
        end;

        if (HorzSliderRect.Height > 0) and (HorzSliderRect.Width > 0) then
        begin
          Details := ColorizerStyleServices.GetElementDetails(FHorzSliderState);
          ColorizerStyleServices.DrawElement(B.Canvas.Handle, Details, HorzSliderRect);
        end;

        if HorzSliderRect.Height > 0 then
          Details := ColorizerStyleServices.GetElementDetails(FHorzUpState)
        else
          Details := ColorizerStyleServices.GetElementDetails(tsArrowBtnLeftDisabled);
        ColorizerStyleServices.DrawElement(B.Canvas.Handle, Details, HorzUpButtonRect);

        if HorzSliderRect.Height > 0 then
          Details := ColorizerStyleServices.GetElementDetails(FHorzDownState)
        else
          Details := ColorizerStyleServices.GetElementDetails(tsArrowBtnRightDisabled);
        ColorizerStyleServices.DrawElement(B.Canvas.Handle, Details, HorzDownButtonRect);
      end;
      MoveWindowOrg(B.Canvas.Handle, HorzScrollRect.Left, HorzScrollRect.Top);
      with HorzScrollRect do
        BitBlt(DC, Left, Top, B.Width, B.Height,
          B.Canvas.Handle, 0, 0, SRCCOPY);
    finally
      B.Free;
    end;
  end;
end;
procedure TColorizerScrollingStyleHook.DrawVertScroll(DC: HDC);
var
  B: TBitmap;
  Details: TThemedElementDetails;
  R: TRect;
begin
  if Handle = 0 then Exit;
  if DC = 0 then Exit;

  if (VertScrollRect.Width > 0) and (VertScrollRect.Height > 0) then
  begin
    B := TBitmap.Create;
    try
      B.Width := VertScrollRect.Width;
      B.Height := VertScrollRect.Height;
      MoveWindowOrg(B.Canvas.Handle, -VertScrollRect.Left, -VertScrollRect.Top);
      if ColorizerStyleServices.Available then
      begin
        R := VertScrollRect;
        R.Top := VertUpButtonRect.Bottom;
        R.Bottom := VertDownButtonRect.Top;
        if (R.Height > 0) and (R.Width > 0) then
        begin
          Details := ColorizerStyleServices.GetElementDetails(tsUpperTrackVertNormal);
          ColorizerStyleServices.DrawElement(B.Canvas.Handle, Details, R);
        end;

        if (VertSliderRect.Height > 0) and (VertSliderRect.Width > 0) then
        begin
          Details := ColorizerStyleServices.GetElementDetails(VertSliderState);
          ColorizerStyleServices.DrawElement(B.Canvas.Handle, Details, VertSliderRect);
        end;

        if VertSliderRect.Height <> 0 then
          Details := ColorizerStyleServices.GetElementDetails(FVertUpState)
        else
          Details := ColorizerStyleServices.GetElementDetails(tsArrowBtnUpDisabled);
        ColorizerStyleServices.DrawElement(B.Canvas.Handle, Details, VertUpButtonRect);

        if VertSliderRect.Height <> 0 then
          Details := ColorizerStyleServices.GetElementDetails(FVertDownState)
        else
          Details := ColorizerStyleServices.GetElementDetails(tsArrowBtnDownDisabled);
        ColorizerStyleServices.DrawElement(B.Canvas.Handle, Details, VertDownButtonRect);
      end;
      MoveWindowOrg(B.Canvas.Handle, VertScrollRect.Left, VertScrollRect.Top);
      with VertScrollRect do
        BitBlt(DC, Left, Top, B.Width, B.Height, B.Canvas.Handle, 0, 0, SRCCOPY);
    finally
      B.Free;
    end;
  end;
end;

function TColorizerScrollingStyleHook.GetHorzDownButtonRect: TRect;
begin
  Result := HorzScrollRect;
  if Result.Height > 0 then
    Result.Left := Result.Right - GetSystemMetrics(SM_CXHTHUMB)
  else
    Result := Rect(0, 0, 0, 0);
end;

function TColorizerScrollingStyleHook.GetHorzScrollRect: TRect;
var
  P: TPoint;
  BarInfo: TScrollBarInfo;
begin
  BarInfo.cbSize := SizeOf(BarInfo);
  GetScrollBarInfo(Handle, Integer(OBJID_HSCROLL), BarInfo);
  if STATE_SYSTEM_INVISIBLE and BarInfo.rgstate[0] <> 0 then
    Result := Rect(0, 0, 0, 0)
  else
  begin
    P := BarInfo.rcScrollBar.TopLeft;
    ScreenToClient(Handle, P);
    Result.TopLeft := P;
    P := BarInfo.rcScrollBar.BottomRight;
    ScreenToClient(Handle, P);
    Result.BottomRight := P;
    if HasBorder then
      if HasClientEdge then
        OffsetRect(Result, 2, 2)
      else
        OffsetRect(Result, 1, 1);
  end;
end;

function TColorizerScrollingStyleHook.GetHorzSliderRect: TRect;
var
  P: TPoint;
  BarInfo: TScrollBarInfo;
begin
  BarInfo.cbSize := SizeOf(BarInfo);
  GetScrollBarInfo(Handle, Integer(OBJID_HSCROLL), BarInfo);
  if (STATE_SYSTEM_INVISIBLE and BarInfo.rgstate[0] <> 0) or
     (STATE_SYSTEM_UNAVAILABLE and BarInfo.rgstate[0] <> 0) then
    Result := TRect.Create(0, 0, 0, 0)
  else
  begin
    P := BarInfo.rcScrollBar.TopLeft;
    ScreenToClient(Handle, P);
    Result.TopLeft := P;
    P := BarInfo.rcScrollBar.BottomRight;
    ScreenToClient(Handle, P);
    Result.BottomRight := P;
    Result.Left := BarInfo.xyThumbTop + 0;
    Result.Right := BarInfo.xyThumbBottom + 0;
    if HasBorder then
      if HasClientEdge then
        OffsetRect(Result, 2, 2)
      else
        OffsetRect(Result, 1, 1);
  end;
end;

function TColorizerScrollingStyleHook.GetHorzTrackRect: TRect;
begin
  Result := HorzScrollRect;
  if Result.Width > 0 then
  begin
    Result.Left := Result.Left + GetSystemMetrics(SM_CXHTHUMB);
    Result.Right := Result.Right - GetSystemMetrics(SM_CXHTHUMB);
  end
  else
    Result := Rect(0, 0, 0, 0);
end;


function TColorizerScrollingStyleHook.GetHorzUpButtonRect: TRect;
begin
  Result := HorzScrollRect;
  if Result.Height > 0 then
  begin
    Result.Right := Result.Left + GetSystemMetrics(SM_CXHTHUMB);
  end
  else
    Result := Rect(0, 0, 0, 0);
end;


function TColorizerScrollingStyleHook.GetParentBounds: TRect;
begin
  if (Control <> nil) and (Control.Parent <> nil) then
    Result := Control.Parent.BoundsRect
  else if Handle <> 0 then
    GetWindowRect(Control.ParentWindow, Result)
  else
    Result := TRect.Empty;
end;

function TColorizerScrollingStyleHook.GetVertDownButtonRect: TRect;
begin
  Result := VertScrollRect;
  if Result.Width > 0 then
    Result.Top := Result.Bottom - GetSystemMetrics(SM_CYVTHUMB)
  else
    Result := Rect(0, 0, 0, 0);
end;


function TColorizerScrollingStyleHook.GetVertScrollRect: TRect;
var
  P: TPoint;
  BarInfo: TScrollBarInfo;
begin
  BarInfo.cbSize := SizeOf(BarInfo);
  GetScrollBarInfo(Handle, Integer(OBJID_VSCROLL), BarInfo);
  if STATE_SYSTEM_INVISIBLE and BarInfo.rgstate[0] <> 0 then
    Result := Rect(0, 0, 0, 0)
  else
  begin
    P := BarInfo.rcScrollBar.TopLeft;
    ScreenToClient(Handle, P);
    Result.TopLeft := P;
    P := BarInfo.rcScrollBar.BottomRight;
    ScreenToClient(Handle, P);
    Result.BottomRight := P;
    if HasBorder then
      if HasClientEdge then
         OffsetRect(Result, 2, 2)
       else
         OffsetRect(Result, 1, 1);
  end;
end;

function TColorizerScrollingStyleHook.GetVertSliderRect: TRect;
var
  P: TPoint;
  BarInfo: TScrollBarInfo;
begin
  BarInfo.cbSize := SizeOf(BarInfo);
  GetScrollBarInfo(Handle, Integer(OBJID_VSCROLL), BarInfo);
  if (STATE_SYSTEM_INVISIBLE and BarInfo.rgstate[0] <> 0) or
     (STATE_SYSTEM_UNAVAILABLE and BarInfo.rgstate[0] <> 0) then
    Result := Rect(0, 0, 0, 0)
  else
  begin
    P := BarInfo.rcScrollBar.TopLeft;
    ScreenToClient(Handle, P);
    Result.TopLeft := P;
    P := BarInfo.rcScrollBar.BottomRight;
    ScreenToClient(Handle, P);
    Result.BottomRight := P;
    Result.Top := BarInfo.xyThumbTop + 0;
    Result.Bottom := BarInfo.xyThumbBottom + 0;
    if HasBorder then
      if HasClientEdge then
        OffsetRect(Result, 2, 2)
      else
        OffsetRect(Result, 1, 1);
  end;
end;

function TColorizerScrollingStyleHook.GetVertTrackRect: TRect;
begin
  Result := VertScrollRect;
  if Result.Width > 0 then
  begin
    Result.Top := Result.Top + GetSystemMetrics(SM_CYVTHUMB);
    Result.Bottom := Result.Bottom - GetSystemMetrics(SM_CYVTHUMB);
  end
  else
    Result := Rect(0, 0, 0, 0);
end;

function TColorizerScrollingStyleHook.GetVertUpButtonRect: TRect;
begin
  Result := VertScrollRect;
  if Result.Width > 0 then
    Result.Bottom := Result.Top + GetSystemMetrics(SM_CYVTHUMB)
  else
    Result := Rect(0, 0, 0, 0);
end;


procedure TColorizerScrollingStyleHook.InitScrollBars;
var
  R: TRect;
begin
  if FInitingScrollBars then Exit;

  FInitingScrollBars := True;

  InitScrollState;

  FVertScrollWnd := TScrollWindow.CreateParented(GetParent(Control.Handle));
  FVertScrollWnd.StyleHook := Self;
  FVertScrollWnd.Vertical := True;
  R := VertScrollRect;
  if (Control.BiDiMode = bdRightToLeft) and not IsRectEmpty(R) then
  begin
    OffsetRect(R, -R.Left, 0);
    if HasBorder then
      if HasClientEdge then
        OffsetRect(R, 2, 0)
      else
        OffsetRect(R, 1, 0);
  end;
  with R do
   if IsPopupWindow then
     SetWindowPos(FVertScrollWnd.Handle, HWND_TOPMOST,
       Control.Left + Left, Control.Top + Top,
         Right - Left, Bottom - Top, SWP_NOREDRAW)
   else
     SetWindowPos(FVertScrollWnd.Handle, HWND_TOP, Control.Left + Left, Control.Top + Top,
       Right - Left, Bottom - Top, SWP_NOREDRAW);

  if IsRectEmpty(VertScrollRect) then
    ShowWindow(FVertScrollWnd.Handle, SW_HIDE)
  else
    ShowWindow(FVertScrollWnd.Handle, SW_SHOW);

  FHorzScrollWnd := TScrollWindow.CreateParented(GetParent(Control.Handle));
  FHorzScrollWnd.StyleHook := Self;
  FHorzScrollWnd.Vertical := False;

  R := HorzScrollRect;
  if (Control.BiDiMode = bdRightToLeft) and not IsRectEmpty(VertScrollRect) then
    OffsetRect(R, VertScrollRect.Width, 0);
  with R do
    if IsPopupWindow then
      SetWindowPos(FHorzScrollWnd.Handle, HWND_TOPMOST,
        Control.Left + Left, Control.Top + Top,
          Right - Left, Bottom - Top, SWP_NOREDRAW)
    else
      SetWindowPos(FHorzScrollWnd.Handle, HWND_TOP, Control.Left + Left, Control.Top + Top,
        Right - Left, Bottom - Top, SWP_NOREDRAW);

  if IsRectEmpty(HorzScrollRect) then
    ShowWindow(FHorzScrollWnd.Handle, SW_HIDE)
  else
    ShowWindow(FHorzScrollWnd.Handle, SW_SHOW);

  FInitingScrollBars := False;
end;

procedure TColorizerScrollingStyleHook.InitScrollState;
begin
  FVertSliderState := tsThumbBtnVertNormal;
  FVertUpState := tsArrowBtnUpNormal;
  FVertDownState := tsArrowBtnDownNormal;
  FHorzSliderState := tsThumbBtnHorzNormal;
  FHorzUpState := tsArrowBtnLeftNormal;
  FHorzDownState := tsArrowBtnRightNormal;
end;


function TColorizerScrollingStyleHook.IsPopupWindow: Boolean;
begin
  Result := (GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_TOOLWINDOW = WS_EX_TOOLWINDOW) or
            (GetWindowLong(Handle, GWL_STYLE) and WS_POPUP = WS_POPUP);
end;

procedure TColorizerScrollingStyleHook.MouseLeave;
begin
  inherited;
  if VertSliderState = tsThumbBtnVertHot then
    FVertSliderState := tsThumbBtnVertNormal;

  if FHorzSliderState = tsThumbBtnHorzHot then
    FHorzSliderState := tsThumbBtnHorzNormal;

  if FVertUpState = tsArrowBtnUpHot then
    FVertUpState := tsArrowBtnUpNormal;

  if FVertDownState = tsArrowBtnDownHot then
    FVertDownState := tsArrowBtnDownNormal;

  if FHorzUpState = tsArrowBtnLeftHot then
    FHorzUpState := tsArrowBtnLeftNormal;

  if FHorzDownState = tsArrowBtnRightHot then
    FHorzDownState := tsArrowBtnRightNormal;

  PaintScroll;
end;


procedure TColorizerScrollingStyleHook.Paint(Canvas: TCanvas);
begin
  PaintScroll;
end;


procedure TColorizerScrollingStyleHook.PaintBackground(Canvas: TCanvas);
begin
  inherited PaintBackground(Canvas);
  PaintScroll;
end;

procedure TColorizerScrollingStyleHook.PaintNC(Canvas: TCanvas);
begin
  if FInitingScrollBars then Exit;
  inherited;
  DrawBorder;
  if FVertScrollWnd = nil then
    InitScrollBars;
  UpdateScroll;
  PaintScroll;
end;

procedure TColorizerScrollingStyleHook.PaintScroll;
begin
  if FInitingScrollBars then Exit;
  if FVertScrollWnd <> nil then
    FVertScrollWnd.Repaint;
  if FHorzScrollWnd <> nil then
    FHorzScrollWnd.Repaint;
end;

procedure TColorizerScrollingStyleHook.UpdateScroll;
var
  R: TRect;
begin
  if (FVertScrollWnd <> nil) and (FVertScrollWnd.HandleAllocated) then
  begin
    R := VertScrollRect;
    if (Control.BiDiMode = bdRightToLeft) and not IsRectEmpty(R) then
    begin
      OffsetRect(R, -R.Left, 0);
      if HasBorder then
        if HasClientEdge then
          OffsetRect(R, 2, 0)
        else
          OffsetRect(R, 1, 0);
    end;

    if IsRectEmpty(R) then
      ShowWindow(FVertScrollWnd.Handle, SW_HIDE)
    else
    begin
      ShowWindow(FVertScrollWnd.Handle, SW_SHOW);
      with R do
        if IsPopupWindow then
          SetWindowPos(FVertScrollWnd.Handle, HWND_TOPMOST,
            Control.Left + Left, Control.Top + Top,
              Right - Left, Bottom - Top, SWP_SHOWWINDOW)
        else
          SetWindowPos(FVertScrollWnd.Handle, HWND_TOP, Control.Left + Left,
           Control.Top + Top, Right - Left, Bottom - Top, SWP_SHOWWINDOW);
    end
  end;
  if (FHorzScrollWnd <> nil) and (FHorzScrollWnd.HandleAllocated) then
  begin
    R := HorzScrollRect;
    if (Control.BiDiMode = bdRightToLeft) and not IsRectEmpty(VertScrollRect) then
      OffsetRect(R, VertScrollRect.Width, 0);
    if IsRectEmpty(R) then
      ShowWindow(FHorzScrollWnd.Handle, SW_HIDE)
    else
    begin
      ShowWindow(FHorzScrollWnd.Handle, SW_SHOW);
      with R do
        if IsPopupWindow then
        begin
          SetWindowPos(FHorzScrollWnd.Handle, HWND_TOPMOST, Control.Left + Left,
            Control.Top + Top, Right - Left, Bottom - Top, SWP_SHOWWINDOW)
        end
        else
          SetWindowPos(FHorzScrollWnd.Handle, HWND_TOP, Control.Left + Left,
            Control.Top + Top, Right - Left, Bottom - Top, SWP_SHOWWINDOW);
    end;
  end;
end;

procedure TColorizerScrollingStyleHook.WMCaptureChanged(var Msg: TMessage);
begin
  if FVertUpState = tsArrowBtnUpPressed then
  begin
    FVertUpState := tsArrowBtnUpNormal;
    PaintScroll;
  end;

  if FVertDownState = tsArrowBtnDownPressed then
  begin
    FVertDownState := tsArrowBtnDownNormal;
    PaintScroll;
  end;

  if FHorzUpState = tsArrowBtnLeftPressed then
  begin
    FHorzUpState := tsArrowBtnLeftNormal;
    PaintScroll;
  end;

  if FHorzDownState = tsArrowBtnRightPressed then
  begin
    FHorzDownState := tsArrowBtnRightNormal;
    PaintScroll;
  end;

  CallDefaultProc(TMessage(Msg));
  Handled := True;
end;

procedure TColorizerScrollingStyleHook.WMClose(var Msg: TWMCLOSE);
begin
  Handled := True;
end;

procedure TColorizerScrollingStyleHook.WMHScroll(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScroll;
  Handled := True;
end;


procedure TColorizerScrollingStyleHook.WMKeyDown(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScroll;
  Handled := True;
end;

procedure TColorizerScrollingStyleHook.WMKeyUp(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScroll;
  Handled := True;
end;

procedure TColorizerScrollingStyleHook.WMLButtonDown(var Msg: TWMMouse);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScroll;
  Handled := True;
end;

procedure TColorizerScrollingStyleHook.WMLButtonUp(var Msg: TWMMouse);
begin
  if VertSliderState = tsThumbBtnVertPressed then
  begin
    PostMessage(Handle, WM_VSCROLL, Integer(SmallPoint(SB_THUMBPOSITION, Round(FScrollPos))), 0);
    FLeftButtonDown := False;
    FVertSliderState := tsThumbBtnVertNormal;
    PaintScroll;
    Handled := True;
    ReleaseCapture;
    PostMessage(Handle, WM_VSCROLL, Integer(SmallPoint(SB_ENDSCROLL, 0)), 0);
    Exit;
  end;

  if FHorzSliderState = tsThumbBtnHorzPressed then
  begin
    PostMessage(Handle, WM_HSCROLL, Integer(SmallPoint(SB_THUMBPOSITION, Round(FScrollPos))), 0);
    FLeftButtonDown := False;
    FHorzSliderState := tsThumbBtnHorzNormal;
    PaintScroll;
    Handled := True;
    ReleaseCapture;
    PostMessage(Handle, WM_HSCROLL, Integer(SmallPoint(SB_ENDSCROLL, 0)), 0);
    Exit;
  end;

  if FVertUpState = tsArrowBtnUpPressed then
    FVertUpState := tsArrowBtnUpNormal;

  if FVertDownState = tsArrowBtnDownPressed then
    FVertDownState := tsArrowBtnDownNormal;

  if FHorzUpState = tsArrowBtnLeftPressed then
    FHorzUpState := tsArrowBtnLeftNormal;

  if FHorzDownState = tsArrowBtnRightPressed then
    FHorzDownState := tsArrowBtnRightNormal;

  FLeftButtonDown := False;
  PaintScroll;
end;

procedure TColorizerScrollingStyleHook.WMMouseMove(var Msg: TWMMouse);
var
  SF: TScrollInfo;
begin
  inherited;

  if VertSliderState = tsThumbBtnVertPressed then
  begin
    SF.fMask := SIF_ALL;
    SF.cbSize := SizeOf(SF);
    GetScrollInfo(Handle, SB_VERT, SF);
    if SF.nPos <> Round(FScrollPos) then FScrollPos := SF.nPos;

    FScrollPos := FScrollPos + (SF.nMax - SF.nMin) * ((Mouse.CursorPos.Y - FPrevScrollPos) / VertTrackRect.Height);
    if FScrollPos < SF.nMin then FScrollPos := SF.nMin;
    if FScrollPos > SF.nMax then FScrollPos := SF.nMax;
    if SF.nPage <> 0 then
      if Round(FScrollPos) > SF.nMax - Integer(SF.nPage) + 1 then
        FScrollPos := SF.nMax - Integer(SF.nPage) + 1;
    FPrevScrollPos := Mouse.CursorPos.Y;
    SF.nPos := Round(FScrollPos);

    SetScrollInfo(Handle, SB_VERT, SF, False);
    PostMessage(Handle, WM_VSCROLL, Integer(SmallPoint(SB_THUMBTRACK, Round(FScrollPos))), 0);

    PaintScroll;
    Handled := True;
    Exit;
  end;

  if FHorzSliderState = tsThumbBtnHorzPressed then
  begin
    SF.fMask := SIF_ALL;
    SF.cbSize := SizeOf(SF);
    GetScrollInfo(Handle, SB_HORZ, SF);
    if SF.nPos <> Round(FScrollPos) then FScrollPos := SF.nPos;

    FScrollPos := FScrollPos + (SF.nMax - SF.nMin) * ((Mouse.CursorPos.X - FPrevScrollPos) / HorzTrackRect.Width);
    if FScrollPos < SF.nMin then FScrollPos := SF.nMin;
    if FScrollPos > SF.nMax then FScrollPos := SF.nMax;
    if SF.nPage <> 0 then
      if Round(FScrollPos) > SF.nMax - Integer(SF.nPage) + 1 then
        FScrollPos := SF.nMax - Integer(SF.nPage) + 1;
    FPrevScrollPos := Mouse.CursorPos.X;
    SF.nPos := Round(FScrollPos);

    SetScrollInfo(Handle, SB_HORZ, SF, False);
    PostMessage(Handle, WM_HSCROLL, Integer(SmallPoint(SB_THUMBTRACK, Round(FScrollPos))), 0);

    PaintScroll;
    Handled := True;
    Exit;
  end;

  if (FHorzSliderState <> tsThumbBtnHorzPressed) and (FHorzSliderState = tsThumbBtnHorzHot) then
  begin
    FHorzSliderState := tsThumbBtnHorzNormal;
    PaintScroll;
  end;

  if (VertSliderState <> tsThumbBtnVertPressed) and (VertSliderState = tsThumbBtnVertHot) then
  begin
    FVertSliderState := tsThumbBtnVertNormal;
    PaintScroll;
  end;

  if (FHorzUpState <> tsArrowBtnLeftPressed) and (FHorzUpState = tsArrowBtnLeftHot) then
  begin
    FHorzUpState := tsArrowBtnLeftNormal;
    PaintScroll;
  end;

  if (FHorzDownState <> tsArrowBtnRightPressed) and (FHorzDownState =tsArrowBtnRightHot) then
  begin
    FHorzDownState := tsArrowBtnRightNormal;
    PaintScroll;
  end;

  if (FVertUpState <> tsArrowBtnUpPressed) and (FVertUpState = tsArrowBtnUpHot) then
  begin
    FVertUpState := tsArrowBtnUpNormal;
    PaintScroll;
  end;

  if (FVertDownState <> tsArrowBtnDownPressed) and (FVertDownState = tsArrowBtnDownHot) then
  begin
    FVertDownState := tsArrowBtnDownNormal;
    PaintScroll;
  end;

  CallDefaultProc(TMessage(Msg));
  if FLeftButtonDown then
    PaintScroll;
  Handled := True;
end;

procedure TColorizerScrollingStyleHook.WMMouseWheel(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScroll;
  Handled := True;
end;

procedure TColorizerScrollingStyleHook.WMMove(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  if (FVertScrollWnd <> nil) or (FHorzScrollWnd <> nil) then
    SetWindowPos(Handle, 0,0,0,0,0, SWP_FRAMECHANGED or SWP_NOACTIVATE or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
  UpdateScroll;
  Handled := True;
end;

procedure TColorizerScrollingStyleHook.WMNCLButtonDblClk(var Msg: TWMMouse);
begin
  WMNCLButtonDown(Msg);
end;


procedure TColorizerScrollingStyleHook.WMNCLButtonDown(var Msg: TWMMouse);
var
  P: TPoint;
  SF: TScrollInfo;
begin

  P := Point(Msg.XPos, Msg.YPos);
  ScreenToClient(Handle, P);

  if HasBorder then
    if HasClientEdge then
    begin
      P.X := P.X + 2;
      P.Y := P.Y + 2;
    end
    else
    begin
      P.X := P.X + 1;
      P.Y := P.Y + 1;
    end;

  if VertSliderRect.Contains(P) then
  begin
    FLeftButtonDown := True;
    SF.fMask := SIF_ALL;
    SF.cbSize := SizeOf(SF);
    GetScrollInfo(Handle, SB_VERT, SF);
    FListPos := SF.nPos;
    FScrollPos := SF.nPos;
    FPrevScrollPos := Mouse.CursorPos.Y;
    FVertSliderState := tsThumbBtnVertPressed;
    PaintScroll;
    SetCapture(Handle);
    Handled := True;
    Exit;
  end;

  if HorzSliderRect.Contains(P) then
  begin
    FLeftButtonDown := True;
    SF.fMask := SIF_ALL;
    SF.cbSize := SizeOf(SF);
    GetScrollInfo(Handle, SB_HORZ, SF);
    FListPos := SF.nPos;
    FScrollPos := SF.nPos;
    FPrevScrollPos := Mouse.CursorPos.X;
    FHorzSliderState :=  tsThumbBtnHorzPressed;
    PaintScroll;
    SetCapture(Handle);
    Handled := True;
    Exit;
  end;

  if VertDownButtonRect.Contains(P) and (VertSliderRect.Height > 0) then
    FVertDownState := tsArrowBtnDownPressed;

  if VertUpButtonRect.Contains(P) and (VertSliderRect.Height > 0) then
    FVertUpState := tsArrowBtnUpPressed;

  if HorzDownButtonRect.Contains(P) and (HorzSliderRect.Width > 0)  then
    FHorzDownState := tsArrowBtnRightPressed;

  if HorzUpButtonRect.Contains(P) and (HorzSliderRect.Width > 0) then
    FHorzUpState := tsArrowBtnLeftPressed;

  PaintScroll;
end;

procedure TColorizerScrollingStyleHook.WMNCLButtonUp(var Msg: TWMMouse);
var
  P: TPoint;
begin
  P := Point(Msg.XPos, Msg.YPos);
  ScreenToClient(Handle, P);

  if HasBorder then
    if HasClientEdge then
    begin
      P.X := P.X + 2;
      P.Y := P.Y + 2;
    end
    else
    begin
      P.X := P.X + 1;
      P.Y := P.Y + 1;
    end;

  if VertSliderState =  tsThumbBtnVertPressed then
  begin
    FLeftButtonDown := False;
    FVertSliderState := tsThumbBtnVertNormal;
    PaintScroll;
    Handled := True;
    Exit;
  end;

  if FHorzSliderState = tsThumbBtnHorzPressed then
  begin
    FLeftButtonDown := False;
    FHorzSliderState := tsThumbBtnHorzNormal;
    PaintScroll;
    Handled := True;
    Exit;
  end;

  if VertSliderRect.Height > 0 then
    if VertDownButtonRect.Contains(P) then
      FVertDownState := tsArrowBtnDownHot
    else
      FVertDownState := tsArrowBtnDownNormal;

  if VertSliderRect.Height > 0 then
    if VertUpButtonRect.Contains(P) then
      FVertUpState := tsArrowBtnUpHot
    else
      FVertUpState := tsArrowBtnUpNormal;

  if HorzSliderRect.Width > 0 then
    if HorzDownButtonRect.Contains(P) then
      FHorzDownState := tsArrowBtnRightHot
    else
      FHorzDownState := tsArrowBtnRightNormal;

  if HorzSliderRect.Width > 0 then
    if HorzUpButtonRect.Contains(P) then
      FHorzUpState := tsArrowBtnLeftHot
    else
      FHorzUpState := tsArrowBtnLeftNormal;

  CallDefaultProc(TMessage(Msg));
  if (HorzSliderRect.Width > 0) or (VertSliderRect.Height > 0) then
    PaintScroll;
  Handled := True;
end;

procedure TColorizerScrollingStyleHook.WMNCMouseMove(var Msg: TWMMouse);
var
  P: TPoint;
  MustUpdateScroll: Boolean;
begin
  inherited;
  P := Point(Msg.XPos, Msg.YPos);
  ScreenToClient(Handle, P);

  if HasBorder then
    if HasClientEdge then
    begin
      P.X := P.X + 2;
      P.Y := P.Y + 2;
    end
    else
    begin
      P.X := P.X + 1;
      P.Y := P.Y + 1;
    end;

  MustUpdateScroll := False;

  if VertSliderRect.Height > 0 then
  if VertSliderRect.Contains(P) and (VertSliderState = tsThumbBtnVertNormal) then
  begin
    FVertSliderState := tsThumbBtnVertHot;
    MustUpdateScroll := True;
  end
  else
    if not VertSliderRect.Contains(P) and (VertSliderState = tsThumbBtnVertHot) then
    begin
      FVertSliderState := tsThumbBtnVertNormal;
      MustUpdateScroll := True;
    end;

  if HorzSliderRect.Width > 0 then
    if HorzSliderRect.Contains(P) and (FHorzSliderState = tsThumbBtnHorzNormal) then
    begin
      FHorzSliderState := tsThumbBtnHorzHot;
      MustUpdateScroll := True;
    end
    else
      if not HorzSliderRect.Contains(P) and (FHorzSliderState = tsThumbBtnHorzHot) then
      begin
        FHorzSliderState := tsThumbBtnHorzNormal;
        MustUpdateScroll := True;
      end;

  if VertSliderRect.Height > 0 then
    if VertDownButtonRect.Contains(P) and (FVertDownState = tsArrowBtnDownNormal) then
    begin
      FVertDownState := tsArrowBtnDownHot;
      MustUpdateScroll := True;
    end
    else
      if not VertDownButtonRect.Contains(P) and (FVertDownState = tsArrowBtnDownHot) then
      begin
        FVertDownState := tsArrowBtnDownNormal;
        MustUpdateScroll := True;
      end;

  if VertSliderRect.Height > 0 then
    if VertUpButtonRect.Contains(P) and (FVertUpState = tsArrowBtnUpNormal) then
    begin
      FVertUpState := tsArrowBtnUpHot;
      MustUpdateScroll := True;
    end
    else if not VertUpButtonRect.Contains(P) and (FVertUpState = tsArrowBtnUpHot) then
    begin
      FVertUpState := tsArrowBtnUpNormal;
      MustUpdateScroll := True;
    end;

  if HorzSliderRect.Width > 0 then
    if HorzDownButtonRect.Contains(P) and (FHorzDownState = tsArrowBtnRightNormal) then
    begin
      FHorzDownState := tsArrowBtnRightHot;
      MustUpdateScroll := True;
    end
    else if not HorzDownButtonRect.Contains(P) and (FHorzDownState = tsArrowBtnRightHot) then
    begin
      FHorzDownState := tsArrowBtnRightNormal;
      MustUpdateScroll := True;
    end;

  if HorzSliderRect.Width > 0 then
    if HorzUpButtonRect.Contains(P) and (FHorzUpState = tsArrowBtnLeftNormal) then
    begin
      FHorzUpState := tsArrowBtnLeftHot;
      MustUpdateScroll := True;
    end
    else if not HorzUpButtonRect.Contains(P) and (FHorzUpState = tsArrowBtnLeftHot) then
    begin
      FHorzUpState := tsArrowBtnLeftNormal;
      MustUpdateScroll := True;
    end;

  if MustUpdateScroll then
    PaintScroll;
end;


procedure TColorizerScrollingStyleHook.WMShowWindow(var Msg: TWMShowWindow);
begin
  CallDefaultProc(TMessage(Msg));

  if (FVertScrollWnd <> nil) and FVertScrollWnd.HandleAllocated then
    if Msg.Show then
      ShowWindow(FVertScrollWnd.Handle, SW_SHOW)
    else
      ShowWindow(FVertScrollWnd.Handle, SW_HIDE);

  if (FHorzScrollWnd <> nil) and FHorzScrollWnd.HandleAllocated then
    if Msg.Show then
      ShowWindow(FHorzScrollWnd.Handle, SW_SHOW)
    else
      ShowWindow(FHorzScrollWnd.Handle, SW_HIDE);

  Handled := True;
end;


procedure TColorizerScrollingStyleHook.WMSize(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  UpdateScroll;
  Handled := True;
end;

procedure TColorizerScrollingStyleHook.WMVScroll(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScroll;
  Handled := True;
end;

procedure TColorizerScrollingStyleHook.WMWindowPosChanged(
  var Msg: TWMWindowPosChanged);
begin
  CallDefaultProc(TMessage(Msg));
  if Msg.WindowPos.Flags and SWP_HIDEWINDOW = SWP_HIDEWINDOW then
  begin
    if FVertScrollWnd <> nil then
      ShowWindow(FVertScrollWnd.Handle, SW_HIDE);
    if FHorzScrollWnd <> nil then
      ShowWindow(FHorzScrollWnd.Handle, SW_HIDE);
  end
  else
    if IsWindowVisible(Control.Handle) then
      UpdateScroll;
  Handled := True;
end;

procedure TColorizerScrollingStyleHook.WndProc(var Message: TMessage);
begin
  inherited;

end;

{ TColorizerScrollingStyleHook.TScrollWindow }

constructor TColorizerScrollingStyleHook.TScrollWindow.Create(
  AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOverrideStylePaint];
  FStyleHook := nil;
end;


procedure TColorizerScrollingStyleHook.TScrollWindow.CreateParams(
  var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_CHILDWINDOW or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
  Params.ExStyle := Params.ExStyle or WS_EX_NOPARENTNOTIFY;
  Params.WindowClass.style := Params.WindowClass.style;
  if (FStyleHook <> nil) and FStyleHook.IsPopupWindow then
    Params.ExStyle := Params.ExStyle or WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
end;

procedure TColorizerScrollingStyleHook.TScrollWindow.WMEraseBkgnd(
  var Msg: TMessage);
begin
  Msg.Result := 1;
end;

procedure TColorizerScrollingStyleHook.TScrollWindow.WMNCHitTest(
  var Msg: TWMNCHitTest);
begin
  Msg.Result := HTTRANSPARENT;
end;

procedure TColorizerScrollingStyleHook.TScrollWindow.WMPaint(var Msg: TWMPaint);
var
  PS: TPaintStruct;
  DC: HDC;
begin
  BeginPaint(Handle, PS);
  try
    if (FStyleHook <> nil) and
       (FStyleHook.Control.Width > 0) and
       (FStyleHook.Control.Height > 0) then
    begin
      DC := GetWindowDC(Handle);
      try
        if FVertical then
        begin
          with FStyleHook.VertScrollRect do
            MoveWindowOrg(DC, -Left, -Top);
          FStyleHook.DrawVertScroll(DC);
        end
        else
        begin
          with FStyleHook.HorzScrollRect do
            MoveWindowOrg(DC, -Left, -Top);
          FStyleHook.DrawHorzScroll(DC);
        end;
      finally
        ReleaseDC(Handle, DC);
      end;
    end;
  finally
    EndPaint(Handle, PS);
  end;
end;

procedure TColorizerScrollingStyleHook.TScrollWindow.WndProc(
  var Message: TMessage);
begin
  inherited;

end;

{ TColorizerListBoxStyleHook }

constructor TColorizerListBoxStyleHook.Create(AControl: TWinControl);
begin
  inherited;
  OverrideEraseBkgnd := True;
  UpdateColors;
end;

procedure TColorizerListBoxStyleHook.UpdateColors;
const
  ColorStates: array[Boolean] of TStyleColor = (scListBoxDisabled, scListBox);
  FontColorStates: array[Boolean] of TStyleFont = (sfListItemTextDisabled, sfListItemTextNormal);
var
  LStyle: TCustomStyleServices;
begin
  LStyle := ColorizerStyleServices;
  Brush.Color := LStyle.GetStyleColor(ColorStates[Control.Enabled]);
  FontColor := LStyle.GetStyleFontColor(FontColorStates[Control.Enabled])
end;

procedure TColorizerListBoxStyleHook.WMKillFocus(var Message: TMessage);
begin
  inherited;
  CallDefaultProc(Message);
  RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
  Handled := True;
end;

procedure TColorizerListBoxStyleHook.WMSetFocus(var Message: TMessage);
begin
  inherited;
  CallDefaultProc(Message);
  RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
  Handled := True;
end;


procedure TColorizerListBoxStyleHook.WndProc(var Message: TMessage);
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
end;


{ TColorizerComboBoxStyleHook }

constructor TColorizerComboBoxStyleHook.Create;
begin
  inherited;
  if Style = csSimple then
    OverrideEraseBkgnd := True;
  FMouseOnButton := False;
  FEditHandle := 0;
  FListHandle := 0;
  FListBoxInstance := nil;
  FIgnoreStyleChanged := False;
  FVSliderState := tsThumbBtnVertNormal;
  FVUpState := tsArrowBtnUpNormal;
  FVDownState := tsArrowBtnDownNormal;
  FSliderSize := 0;
  FListBoxTimerCode := 0;
  FListBoxUpBtnDown := False;
  FListBoxDownBtnDown := False;
  FListBoxTrackUpDown := False;
  FListBoxTrackDownDown := False;
  FTempItemIndex := -1;
  UpdateColors;
end;

destructor TColorizerComboBoxStyleHook.Destroy;
begin
  if (FListHandle <> 0) and (FListBoxInstance <> nil) then
  begin
    SetWindowLong(FListHandle, GWL_WNDPROC, IntPtr(FDefListBoxProc));
    FreeObjectInstance(FListBoxInstance);
    FListBoxInstance := nil;
  end;
  if FListBoxTimerCode <> 0 then
    ListBoxStopTimer;
  inherited;
end;

function TColorizerComboBoxStyleHook.IsChildHandle(AHandle: HWnd): Boolean;
begin
  Result := (FEditHandle <> 0) and (FEditHandle = AHandle);
end;

function TColorizerComboBoxStyleHook.AcceptMessage(var Message: TMessage): Boolean;
begin
  Result := {$IFDEF DELPHIXE3_UP}seBorder in Control.StyleElements {$ELSE} True {$ENDIF};
end;

procedure TColorizerComboBoxStyleHook.ListBoxSetTimer(ATimerCode: Integer);
begin
  if FListBoxTimerCode <> 0 then ListBoxStopTimer;
  FListBoxTimerCode := ATimerCode;
  if ATimerCode < 4 then
    SetTimer(FListHandle, 1, 300, nil)
  else
    SetTimer(FListHandle, 1, 50, nil);
end;

procedure TColorizerComboBoxStyleHook.ListBoxStopTimer;
begin
  FListBoxTimerCode := -1;
  KillTimer(FListHandle, 1);
end;

function TColorizerComboBoxStyleHook.DroppedDown: Boolean;
begin
  if (Control <> nil) and (Control is TCustomComboBox) then
    Result := TCustomComboBox(Control).DroppedDown
  else if Handle <> 0 then
    Result := LongBool(SendMessage(Handle, CB_GETDROPPEDSTATE, 0, 0))
  else
    Result := False;
end;

function TColorizerComboBoxStyleHook.GetButtonRect: TRect;
begin
  Result := Control.ClientRect;
  InflateRect(Result, -2, -2);
  if Control.BiDiMode <> bdRightToLeft then
    Result.Left := Result.Right - GetSystemMetrics(SM_CXVSCROLL) + 1
  else
    Result.Right := Result.Left + GetSystemMetrics(SM_CXVSCROLL) - 1;
end;

function TColorizerComboBoxStyleHook.Style: TComboBoxStyle;
const
  ComboBoxStyles: array[TComboBoxStyle] of DWORD = (
    CBS_DROPDOWN, CBS_SIMPLE, CBS_DROPDOWNLIST,
    CBS_DROPDOWNLIST or CBS_OWNERDRAWFIXED,
    CBS_DROPDOWNLIST or CBS_OWNERDRAWVARIABLE);
var
  LStyle: Cardinal;
begin
  if (Control <> nil) and (Control is TCustomComboBox) then
    Result := TCustomComboBoxClass(Control).Style
  else if Handle <> 0 then
  begin
    LStyle := GetWindowLong(Handle, GWL_STYLE);
    Result := csDropDown;
    if LStyle and ComboBoxStyles[csDropDown] = ComboBoxStyles[csDropDown] then
      Result := csDropDown;
    if LStyle and ComboBoxStyles[csSimple] = ComboBoxStyles[csSimple] then
      Result := csSimple;
    if LStyle and ComboBoxStyles[csDropDownList] = ComboBoxStyles[csDropDownList] then
      Result := csDropDownList;
    if LStyle and ComboBoxStyles[csOwnerDrawFixed] = ComboBoxStyles[csOwnerDrawFixed] then
      Result := csOwnerDrawFixed;
    if LStyle and ComboBoxStyles[csOwnerDrawVariable] = ComboBoxStyles[csOwnerDrawVariable] then
      Result := csOwnerDrawVariable;
  end
  else
    Result := csDropDown;
end;

procedure TColorizerComboBoxStyleHook.UpdateColors;
const
  ColorStates: array[Boolean] of TStyleColor = (scComboBoxDisabled, scComboBox);
  FontColorStates: array[Boolean] of TStyleFont = (sfComboBoxItemDisabled, sfComboBoxItemNormal);
var
  LStyle: TCustomStyleServices;
begin
  LStyle := ColorizerStyleServices;
  Brush.Color := LStyle.GetStyleColor(ColorStates[Control.Enabled]);
  FontColor := LStyle.GetStyleFontColor(FontColorStates[Control.Enabled]);
end;

procedure TColorizerComboBoxStyleHook.PaintBorder(Canvas: TCanvas);
var
  R, ControlRect, EditRect, ListRect: TRect;
  DrawState: TThemedComboBox;
  BtnDrawState: TThemedComboBox;
  Details: TThemedElementDetails;
  Buffer: TBitmap;
begin
  if not ColorizerStyleServices.Available then Exit;

  if not Control.Enabled then
    BtnDrawState := tcDropDownButtonDisabled
  else if DroppedDown then
    BtnDrawState := tcDropDownButtonPressed
  else if FMouseOnButton then
    BtnDrawState := tcDropDownButtonHot
  else
    BtnDrawState := tcDropDownButtonNormal;

  if not Control.Enabled then
    DrawState := tcBorderDisabled
  else
  if Control.Focused then
    DrawState := tcBorderFocused
  else if MouseInControl then
    DrawState := tcBorderHot
  else
    DrawState := tcBorderNormal;

  Buffer := TBitMap.Create;
  Buffer.SetSize(Control.Width, Control.Height);
  try
    R := Rect(0, 0, Buffer.Width, Buffer.Height);
    // draw border + client in buffer
    Details := ColorizerStyleServices.GetElementDetails(DrawState);
    if (Style = csSimple) and (FListHandle <> 0) then
    begin
      GetWindowRect(FListHandle, ListRect);
      GetWindowRect(Handle, ControlRect);
      R.Bottom := ListRect.Top - ControlRect.Top;
      ColorizerStyleServices.DrawElement(Buffer.Canvas.Handle, Details, R);
      R := Rect(0, Control.Height - (ControlRect.Bottom - ListRect.Bottom),
        Control.Width, Control.Height);
      with Buffer.Canvas do
      begin
        Brush.Style := bsSolid;
        Brush.Color := ColorizerStyleServices.GetSystemColor(clBtnFace);
        FillRect(R);
      end;
      R := Rect(0, 0, Buffer.Width, Buffer.Height);
      R.Bottom := ListRect.Top - ControlRect.Top;
    end
    else
      ColorizerStyleServices.DrawElement(Buffer.Canvas.Handle, Details, R);

    if {$IFDEF DELPHIXE3_UP}not (seClient in Control.StyleElements) and {$ENDIF} (FEditHandle = 0) then
    begin
      R := Control.ClientRect;
      InflateRect(R, -3, -3);
      R.Right := ButtonRect.Left - 2;
      with Buffer.Canvas do
      begin
        Brush.Color := TWinControlClass(Control).Color;
        FillRect(R);
      end;
    end;
    // draw button in buffer
    if Style <> csSimple then
    begin
      Details := ColorizerStyleServices.GetElementDetails(BtnDrawState);
      ColorizerStyleServices.DrawElement(Buffer.Canvas.Handle, Details, ButtonRect);
    end;
    // calculation of exclude area for drawing buffer
    if (SendMessage(Handle, CB_GETCURSEL, 0, 0) >= 0) and (FEditHandle = 0) then
    begin
      R := Control.ClientRect;
      InflateRect(R, -3, -3);
      R.Right := ButtonRect.Left - 2;
      ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
    end
    else
    if FEditHandle <> 0 then
    begin
      GetWindowRect(Handle, R);
      GetWindowRect(FEditHandle, EditRect);
      OffsetRect(EditRect, -R.Left, -R.Top);
      with EditRect do
        ExcludeClipRect(Canvas.Handle, Left, Top, Right, Bottom);
    end;
    // draw buffer
    Canvas.Draw(0, 0, Buffer);
  finally
    Buffer.Free;
  end;
end;

procedure TColorizerComboBoxStyleHook.DrawItem(Canvas: TCanvas;
  Index: Integer; const R: TRect; Selected: Boolean);
var
  DIS: TDrawItemStruct;
begin
  FillChar(DIS, SizeOf(DIS), 0);
  DIS.CtlType := ODT_COMBOBOX;
  DIS.CtlID := GetDlgCtrlID(Handle);
  DIS.itemAction := ODA_DRAWENTIRE;
  DIS.hDC := Canvas.Handle;
  DIS.hwndItem := Handle;
  DIS.rcItem := R;
  DIS.itemID := Index;
  DIS.itemData := SendMessage(FListHandle, LB_GETITEMDATA, 0, 0);
  if Selected then
    DIS.itemState := DIS.itemState or ODS_FOCUS or ODS_SELECTED;

  SendMessage(Handle, WM_DRAWITEM, Handle, LPARAM(@DIS));
end;

procedure TColorizerComboBoxStyleHook.WMParentNotify(var Message: TMessage);
begin
  if (FListHandle = 0) and (LoWord(Message.WParam) = WM_CREATE) then
  begin
    if (Message.LParam <> 0) and (FListBoxInstance = nil) then
      HookListBox(Message.LParam);
  end
  else if (FEditHandle = 0) and (LoWord(Message.WParam) = WM_CREATE) then
    FEditHandle := Message.LParam;
end;

procedure TColorizerComboBoxStyleHook.WndProc(var Message: TMessage);
const
  States: array[Boolean] of TStyleColor = (scEditDisabled, scComboBox);
begin
  case Message.Msg of
    WM_CTLCOLORMSGBOX..WM_CTLCOLORSTATIC,
    CN_CTLCOLORMSGBOX..CN_CTLCOLORSTATIC:
      begin
//        SetTextColor(Message.WParam, ColorToRGB(FontColor));
//        Brush.Color := ColorizerStyleServices.GetStyleColor(States[Control.Enabled]);
          SetTextColor(Message.WParam, ColorToRGB(TColorizerLocalSettings.ColorMap.FontColor));
          Brush.Color:=TColorizerLocalSettings.ColorMap.WindowColor;
        SetBkColor(Message.WParam, ColorToRGB(Brush.Color));

        Message.Result := LRESULT(Brush.Handle);
        Handled := True;
      end;
    CM_ENABLEDCHANGED:
      begin
        UpdateColors;
        Handled := False; // Allow control to handle message
      end;
    CM_FOCUSCHANGED:
      begin
        Invalidate;
        Handled := False; // Allow control to handle message
      end;
  else
    inherited WndProc(Message);
  end;
end;

procedure TColorizerComboBoxStyleHook.MouseEnter;
begin
  inherited;
  Invalidate;
end;

procedure TColorizerComboBoxStyleHook.MouseLeave;
begin
  inherited;
  if not DroppedDown and FMouseOnButton then
    FMouseOnButton := False;
  Invalidate;
end;

procedure TColorizerComboBoxStyleHook.WMMouseMove(var Message: TWMMouse);
var
  P: TPoint;
  R: TRect;
  FOldMouseOnButton: Boolean;
begin
  CallDefaultProc(TMessage(Message));
  inherited;

  P := Point(Message.XPos, Message.YPos);
  FOldMouseOnButton := FMouseOnButton;
  R := ButtonRect;
  if R.Contains(P) then
    FMouseOnButton := True
  else
    FMouseOnButton := False;

  if FOldMouseOnButton <> FMouseOnButton then
    InvalidateRect(Handle, @R, False);

  Handled := True;
end;

procedure TColorizerComboBoxStyleHook.CNDrawItem(var Message: TWMDrawItem);
begin
  WMDrawItem(Message);
  Handled := True;
end;

procedure TColorizerComboBoxStyleHook.WMDrawItem(var Message: TWMDrawItem);
begin
  CallDefaultProc(TMessage(Message));
  Handled := True;
end;

procedure TColorizerComboBoxStyleHook.WMPaint(var Message: TMessage);
var
  R: TRect;
  Canvas: TCanvas;
  PS: TPaintStruct;
  SaveIndex: Integer;
  DC: HDC;
begin
  DC := Message.WParam;
  Canvas := TCanvas.Create;
  try
    if DC = 0 then
      Canvas.Handle := BeginPaint(Handle, PS)
    else
      Canvas.Handle := DC;

    SaveIndex := SaveDC(Canvas.Handle);
    try
      PaintBorder(Canvas);
    finally
      RestoreDC(Canvas.Handle, SaveIndex);
    end;

    if (Style <> csSimple) and (FEditHandle = 0)  then
    begin
      R := Control.ClientRect;
      InflateRect(R, -3, -3);
      if Control.BiDiMode <> bdRightToLeft then
        R.Right := ButtonRect.Left - 1
      else
        R.Left := ButtonRect.Right + 1;
      SaveIndex := SaveDC(Canvas.Handle);
      try
        IntersectClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
        if not DroppedDown then
          DrawItem(Canvas, TComboBox(Control).ItemIndex, R, Focused)
        else
          DrawItem(Canvas, FTempItemIndex, R, Focused)
      finally
        RestoreDC(Canvas.Handle, SaveIndex);
      end;
    end;

  finally
    Canvas.Handle := 0;
    Canvas.Free;
    if DC = 0 then
      EndPaint(Handle, PS);
  end;

  Handled := True;
end;

procedure TColorizerComboBoxStyleHook.WMCommand(var Message: TWMCommand);
begin
  if (Message.NotifyCode = CBN_SELENDCANCEL) or (Message.NotifyCode = CBN_SELENDOK) or
     (Message.NotifyCode = CBN_CLOSEUP) or (Message.NotifyCode = CBN_DROPDOWN) or
     (Message.NotifyCode = CBN_SELCHANGE) then
  begin
    if (Message.NotifyCode = CBN_DROPDOWN) or (Message.NotifyCode = CBN_SELCHANGE) then
      FTempItemIndex := TComboBox(Control).ItemIndex;

    if FListBoxTimerCode <> 0 then
      ListBoxStopTimer;
    FMouseOnButton := False;
    Invalidate;
  end;
end;

procedure TColorizerComboBoxStyleHook.CNCommand(var Message: TWMCommand);
begin
  if (Message.NotifyCode = CBN_SELENDCANCEL) or (Message.NotifyCode = CBN_SELENDOK) or
     (Message.NotifyCode = CBN_CLOSEUP) or (Message.NotifyCode = CBN_DROPDOWN) or
     (Message.NotifyCode = CBN_SELCHANGE)  then
  begin
    if (Message.NotifyCode = CBN_DROPDOWN) or (Message.NotifyCode = CBN_SELCHANGE) then
      FTempItemIndex := TComboBox(Control).ItemIndex;

    if FListBoxTimerCode <> 0 then ListBoxStopTimer;
    FMouseOnButton := False;
    Invalidate;
  end;
end;

procedure TColorizerComboBoxStyleHook.HookListBox(AListHandle: HWND);
begin
  if (AListHandle <> 0) and (FListBoxInstance = nil) then
  begin
    FListHandle := AListHandle;
    FListBoxInstance := MakeObjectInstance(ListBoxWndProc);
    FDefListBoxProc := Pointer(GetWindowLong(FListHandle, GWL_WNDPROC));
    SetWindowLong(FListHandle, GWL_WNDPROC, IntPtr(FListBoxInstance));
  end;
end;


function TColorizerComboBoxStyleHook.ListBoxBoundsRect: TRect;
begin
  GetWindowRect(FListHandle, Result);
end;

function TColorizerComboBoxStyleHook.ListBoxClientRect: TRect;
begin
   GetClientRect(FListHandle, Result);
end;

function TColorizerComboBoxStyleHook.ListBoxVertScrollRect: TRect;
begin
  Result := ListBoxBoundsRect;
  OffsetRect(Result, -Result.Left, -Result.Top);
  InflateRect(Result, -1, -1);
  OffsetRect(Result, 1, 1);
  if Control.BiDiMode <> bdRightToLeft then
    Result.Left := Result.Right - GetSystemMetrics(SM_CXVSCROLL)
  else
    Result.Right := Result.Left + GetSystemMetrics(SM_CXVSCROLL);
  if ListBoxBoundsRect.Height > 30 then OffsetRect(Result, -1, -1);
end;

function TColorizerComboBoxStyleHook.ListBoxVertDownButtonRect: TRect;
begin
  Result := ListBoxVertScrollRect;
  if Result.Width > 0 then
    Result.Top := Result.Bottom - GetSystemMetrics(SM_CYVTHUMB)
  else
    Result := TRect.Empty;
 end;

function TColorizerComboBoxStyleHook.ListBoxVertUpButtonRect: TRect;
begin
  Result := ListBoxVertScrollRect;
  if Result.Width > 0 then
    Result.Bottom := Result.Top + GetSystemMetrics(SM_CYVTHUMB)
  else
    Result := TRect.Empty;
end;

function TColorizerComboBoxStyleHook.ListBoxVertScrollArea: TRect;
begin
  if GetWindowLong(FListHandle, GWL_STYLE) and WS_VSCROLL = 0 then
  begin
    Result := TRect.Empty;
    Exit;
  end;
  Result := ListBoxBoundsRect;
  OffsetRect(Result, -Result.Left, -Result.Top);
  if Control.BiDiMode <> bdRightToLeft then
     Result.Left := Result.Right - GetSystemMetrics(SM_CYVSCROLL) - 1
   else
     Result.Right := Result.Left + GetSystemMetrics(SM_CYVSCROLL);
end;

function TColorizerComboBoxStyleHook.ListBoxVertSliderRect: TRect;
var
  I, LVisibleHeight, LTotalHeight, LSize, LTotalSize,
  LFinalHeight, LItemHeight, LBoundsHeight, LBorderHeight,
  LTopIndex, LItemCount: Integer;
begin
  Result := ListBoxVertScrollRect;
  Result.Top := ListBoxVertUpButtonRect.Bottom;
  Result.Bottom := ListBoxVertDownButtonRect.Top;
  LSize := Result.Bottom - Result.Top;
  LTopIndex := SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0);
  LItemCount := SendMessage(FListHandle, LB_GETCOUNT, 0, 0);
  LTotalSize := LItemCount * LSize;

  if LTotalSize = 0 then
    Exit;

  Result.Top := Result.Top + Round(LTopIndex / LItemCount * LSize);

  LTotalHeight := 1;
  FInvsibleCount := 0;
  LBoundsHeight := ListBoxBoundsRect.Height;
  LItemHeight := SendMessage(FListHandle, LB_GETITEMHEIGHT, 0, 0);
  for I := 0 to LItemCount - 1 do
  begin
    LTotalHeight := LTotalHeight + LItemHeight;
    if (LTotalHeight > LBoundsHeight) and (FInvsibleCount = 0) then
      FInvsibleCount := LItemCount - I;
  end;

  LVisibleHeight := 0;
  for I := LTopIndex to LItemCount - 1 do
  begin
    LVisibleHeight := LVisibleHeight + LItemHeight;
    if Style <> csSimple then LBorderHeight := 2 else
      LBorderHeight := 4;
    if LVisibleHeight >= ListBoxBoundsRect.Height - LBorderHeight then
      Break;
  end;

  Result.Bottom := Result.Top + Round(LVisibleHeight / LTotalHeight * LSize);
  if Result.Height < 8 then
  begin
    Dec(LSize, 8 - Result.Height + 1);
    Result.Top := ListBoxVertUpButtonRect.Bottom +
      Round(LTopIndex / LItemCount * LSize);
    Result.Bottom := Result.Top + 8;
  end;

  if (I = LItemCount - 1) and
     (Result.Bottom <> ListBoxVertDownButtonRect.Top) then
  begin
    LFinalHeight := Result.Height;
    Result.Bottom := ListBoxVertDownButtonRect.Top;
    Result.Top := Result.Bottom - LFinalHeight;
  end;
  FSliderSize := Result.Height;
end;

function TColorizerComboBoxStyleHook.ListBoxVertTrackRect: TRect;
begin
  Result := ListBoxVertScrollRect;
  if Result.Width > 0 then
  begin
    Result.Top := Result.Top + GetSystemMetrics(SM_CYVTHUMB);
    Result.Bottom := Result.Bottom - GetSystemMetrics(SM_CYVTHUMB);
  end
  else
    Result := TRect.Empty;
end;

function TColorizerComboBoxStyleHook.ListBoxVertTrackRectUp: TRect;
begin
  Result := ListBoxVertTrackRect;
  if (Result.Width > 0) and (ListBoxVertSliderRect.Height > 0) then
    Result.Bottom := ListBoxVertSliderRect.Top;
end;

function TColorizerComboBoxStyleHook.ListBoxVertTrackRectDown: TRect;
begin
  Result := ListBoxVertTrackRect;
  if (Result.Width > 0) and (ListBoxVertSliderRect.Height > 0) then
    Result.Top := ListBoxVertSliderRect.Bottom;
end;

procedure TColorizerComboBoxStyleHook.PaintListBoxBorder(Canvas: TCanvas; const R: TRect);
begin
  with Canvas do
  begin
    Brush.Color := ColorizerStyleServices.GetSystemColor(clWindowFrame);
    FillRect(R);
  end;
end;

procedure TColorizerComboBoxStyleHook.DrawListBoxVertScroll(DC: HDC);
var
  B: TBitmap;
  Details: TThemedElementDetails;
  Canvas: TCanvas;
  R: TRect;
begin
  if GetWindowLong(FListHandle, GWL_STYLE) and WS_VSCROLL = 0 then
    Exit;
  Canvas := TCanvas.Create;
  try
    if DC <> 0 then
      Canvas.Handle := DC
    else
      Canvas.Handle := GetWindowDC(FListHandle);
    if ListBoxVertScrollRect.Width > 0 then
    begin
      B := TBitmap.Create;
      try
        B.Width := ListBoxVertScrollRect.Width;
        B.Height := ListBoxVertScrollRect.Height;
        MoveWindowOrg(B.Canvas.Handle, -ListBoxVertScrollRect.Left, -ListBoxVertScrollRect.Top);

        if ColorizerStyleServices.Available then
        begin
          R := ListBoxVertScrollRect;
          R.Top := ListBoxVertUpButtonRect.Bottom;
          R.Bottom := ListBoxVertDownButtonRect.Top;
          if R.Height > 0 then
          begin
            Details := ColorizerStyleServices.GetElementDetails(tsUpperTrackVertNormal);
            ColorizerStyleServices.DrawElement(B.Canvas.Handle, Details, R);
          end;
          Details := ColorizerStyleServices.GetElementDetails(FVSliderState);
          ColorizerStyleServices.DrawElement(B.Canvas.Handle, Details, ListBoxVertSliderRect);
          Details := ColorizerStyleServices.GetElementDetails(FVUpState);
          ColorizerStyleServices.DrawElement(B.Canvas.Handle, Details, ListBoxVertUpButtonRect);
          Details := ColorizerStyleServices.GetElementDetails(FVDownState);
          ColorizerStyleServices.DrawElement(B.Canvas.Handle, Details, ListBoxVertDownButtonRect);
        end;

        MoveWindowOrg(B.Canvas.Handle, ListBoxVertScrollRect.Left, ListBoxVertScrollRect.Top);
        Canvas.Draw(ListBoxVertScrollRect.Left, ListBoxVertScrollRect.Top,  B);
      finally
        B.Free;
      end;
    end;
  finally
    if DC <> 0 then
      Canvas.Handle := 0
    else
    begin
      ReleaseDC(FListHandle, Canvas.Handle);
      Canvas.Handle := 0;
    end;
    Canvas.Free;
  end;
end;

procedure TColorizerComboBoxStyleHook.DrawListBoxBorder;
var
  R: TRect;
  Canvas: TCanvas;
  SaveIdx: Integer;
  P: TPoint;
begin
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := GetWindowDC(FListHandle);
    P := Point(0, 0);
    ClientToScreen(FListHandle, P);
    GetWindowRect(FListHandle, R);
    P.X := P.X - R.Left;
    P.Y := P.Y - R.Top;
    if (R.Width < 5000) and (R.Height < 5000) then
    begin
      GetClientRect(FListHandle, R);
      ExcludeClipRect(Canvas.Handle, P.X, P.Y, R.Right - R.Left + P.X, R.Bottom - R.Top + P.Y);
      GetWindowRect(FListHandle, R);
      OffsetRect(R, -R.Left, -R.Top);
      SaveIdx := SaveDC(Canvas.Handle);
      try
        PaintListBoxBorder(Canvas, R);
      finally
        RestoreDC(Canvas.Handle, SaveIdx);
      end;
      DrawListBoxVertScroll(Canvas.Handle);
    end;
  finally
    ReleaseDC(FListHandle, Canvas.Handle);
    Canvas.Handle := 0;
    Canvas.Free;
  end;
end;

procedure TColorizerComboBoxStyleHook.ListBoxWndProc(var Msg: TMessage);
var
  MsgHandled: Boolean;

  procedure WMNCCalcSize(var Msg: TWMNCCalcSize);
  var
    LCalcSizeParams: PNCCalcSizeParams;
    LWindowPos: PWindowPos;
    LLeft, LRight, LTop, LBottom: Integer;
    LStyle, LNewStyle: Integer;
  begin
    LStyle := GetWindowLong(FListHandle, GWL_STYLE);
    if ((LStyle and WS_VSCROLL = WS_VSCROLL) or (LStyle and WS_HSCROLL = WS_HSCROLL)) then
    begin
      LNewStyle := LStyle and not WS_VSCROLL and not WS_HSCROLL;
      FIgnoreStyleChanged := True;
      SetWindowLong(FListHandle, GWL_STYLE, LNewStyle);
      Msg.Result := CallWindowProc(FDefListBoxProc, FListHandle,
        TMessage(Msg).Msg, TMessage(Msg).WParam, TMessage(Msg).LParam);
      SetWindowLong(FListHandle, GWL_STYLE, LStyle);
      FIgnoreStyleChanged := False;
    end
    else
      Msg.Result := CallWindowProc(FDefListBoxProc, FListHandle,
       TMessage(Msg).Msg, TMessage(Msg).WParam, TMessage(Msg).LParam);

    if (Msg.CalcValidRects) then
    begin
      LCalcSizeParams := Msg.CalcSize_Params;
      if Control.BiDiMode <> bdRightToLeft then
      begin
        LLeft := 1;
         if LStyle and WS_VSCROLL = WS_VSCROLL then
           LRight := ListBoxVertScrollRect.Width + 1
         else
          LRight := 1;
        end
      else
      begin
        LRight := 1;
        if LStyle and WS_VSCROLL = WS_VSCROLL then
         LLeft := ListBoxVertScrollRect.Width + 1
       else
         LLeft := 1;
      end;

      LTop := 1;
      LBottom := 1;
      LWindowPos := LCalcSizeParams.lppos;
      with LCalcSizeParams^.rgrc[0] do
      begin
        left := LWindowPos^.x;
        top := LWindowPos^.y;
        right := LWindowPos^.x + LWindowPos^.cx;
        bottom := LWindowPos^.y + LWindowPos^.cy;
        left := left + LLeft;
        top := top + LTop;
        right := right - LRight;
        bottom := bottom - LBottom;
      end;
      LCalcSizeParams^.rgrc[1] := LCalcSizeParams^.rgrc[0];
      Msg.CalcSize_Params := LCalcSizeParams;
      Msg.Result := WVR_VALIDRECTS;
    end;
    Msg.Result := 0;
    MsgHandled := True;
  end;

  procedure WMMouseWheel(var Msg: TWMMouseWheel);
  var
    Index: Integer;
    R: TRect;
  begin
    SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
    Index := SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0);
    if Msg.WheelDelta < 0 then
      Inc(Index)
    else
      Dec(Index);
    SendMessage(FListHandle, LB_SETTOPINDEX, Index, 0);
    SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
    R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
    RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_ERASE);
    DrawListBoxVertScroll(0);
    MsgHandled := True;
  end;

  procedure WMNCLButtonDblClk(var Msg: TWMMouse);
  var
    R: TRect;
    P: TPoint;
  begin
    P := Point(Msg.XPos, Msg.YPos);
    if ListBoxVertScrollArea.Contains(P) then
    begin
      if ListBoxVertUpButtonRect.Contains(Point(Msg.XPos, Msg.YPos)) then
      begin
        SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
        SendMessage(FListHandle, LB_SETTOPINDEX, SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0) - 1, 0);
        SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
        R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
        RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_ERASE);
        DrawListBoxVertScroll(0);
        Exit;
      end;

      if ListBoxVertDownButtonRect.Contains(Point(Msg.XPos, Msg.YPos)) then
      begin
        SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
        SendMessage(FListHandle, LB_SETTOPINDEX, SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0) + 1, 0);
        SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
        R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
        RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_ERASE);
        DrawListBoxVertScroll(0);
        Exit;
      end;
    end;
    MsgHandled := True;
  end;

  procedure WMLButtonDown(var Msg: TWMMouse);
  var
    P: TPoint;
    R: TRect;
    ItemHeight, VisibleCount, TopIndex: Integer;
  begin
    MsgHandled := False;
    P := Point(Msg.XPos, Msg.YPos);
    if Control.BiDiMode = bdRightToLeft then P.X := - P.X;
    FDownPos := P;
    if ListBoxVertScrollArea.Contains(P) then
    begin
      if Style = csSimple then
        SetCapture(FListHandle);
      FDownPos := P;
      if ListBoxVertTrackRectUp.Contains(P) then
      begin
        ItemHeight := SendMessage(FListHandle, LB_GETITEMHEIGHT, 0, 0);
        if ItemHeight > 0 then
          VisibleCount := ListBoxClientRect.Height div ItemHeight
        else
          VisibleCount := 0;
        TopIndex := SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0) - VisibleCount + 1;
        if TopIndex < 0 then TopIndex := 0;
        SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
        SendMessage(FListHandle, LB_SETTOPINDEX, TopIndex, 0);
        SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
        R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
        RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_ERASE);
        DrawListBoxVertScroll(0);
        ListBoxSetTimer(3);
      end
      else if ListBoxVertTrackRectDown.Contains(P) then
      begin
        ItemHeight := SendMessage(FListHandle, LB_GETITEMHEIGHT, 0, 0);
        if ItemHeight > 0 then
          VisibleCount := ListBoxClientRect.Height div ItemHeight
        else
          VisibleCount := 0;
        TopIndex := SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0) + VisibleCount - 1;
        SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
        SendMessage(FListHandle, LB_SETTOPINDEX, TopIndex, 0);
        SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
        R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
        RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_ERASE);
        DrawListBoxVertScroll(0);
        ListBoxSetTimer(4);
      end
      else if ListBoxVertSliderRect.Contains(P) then
      begin
        FVSliderState := tsThumbBtnVertPressed;
        FDownSliderPos := FDownPos.Y - ListBoxVertSliderRect.Top;
        DrawListBoxVertScroll(0);
      end
      else if ListBoxVertDownButtonRect.Contains(P) then
      begin
        FListBoxDownBtnDown := True;
        FVDownState := tsArrowBtnDownPressed;
        DrawListBoxVertScroll(0);

        SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
        SendMessage(FListHandle, LB_SETTOPINDEX, SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0) + 1, 0);
        SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
        R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
        RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_ERASE);

        ListBoxSetTimer(2);
      end
      else if ListBoxVertUpButtonRect.Contains(P) then
      begin
        FListBoxUpBtnDown := True;
        FVUpState := tsArrowBtnUpPressed;
        DrawListBoxVertScroll(0);

        SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
        SendMessage(FListHandle, LB_SETTOPINDEX, SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0) - 1, 0);
        SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
        R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
        RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_ERASE);

        ListBoxSetTimer(1);
      end;
      MsgHandled := True;
    end
    else
    begin
      if (FVSliderState <> tsThumbBtnVertNormal) or
         (FVUpState <> tsArrowBtnUpNormal) or (FVDownState <> tsArrowBtnDownNormal) then
      begin
        FVSliderState := tsArrowBtnUpNormal;
        FVUpState := tsArrowBtnUpNormal;
        FVDownState := tsArrowBtnDownNormal;
        DrawListBoxVertScroll(0);
      end;
    end;
    FOldIdx := SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0);
  end;

  procedure WMMouseMove(var Msg: TWMMouse);
  var
    P: TPoint;
    NewIndex, Index: Integer;
    Dist: Integer;
    R: TRect;
  begin
    P := Point(Msg.XPos, Msg.YPos);
    if Control.BiDiMode = bdRightToLeft then
      P.X := - P.X;

    FMovePos := P;
    if (FVSliderState = tsThumbBtnVertPressed) then
    begin
      Index := SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0);
      Dist := (ListBoxVertScrollRect.Height - ListBoxVertUpButtonRect.Height - ListBoxVertDownButtonRect.Height - ListBoxVertSliderRect.Height);
      if Dist > 0 then
      begin
        NewIndex := round((((FMovePos.y - FDownSliderPos - ListBoxVertUpButtonRect.Bottom) / Dist) * FInvsibleCount));
        if NewIndex <> Index then
        begin
          if NewIndex < 0 then NewIndex := 0;
          if NewIndex >= SendMessage(FListHandle, LB_GETCOUNT, 0, 0) then NewIndex := SendMessage(FListHandle, LB_GETCOUNT, 0, 0) - 1;
          SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
          SendMessage(FListHandle, LB_SETTOPINDEX, NewIndex, 0);
          SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
          R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
          RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_ERASE);
          DrawListBoxVertScroll(0);
        end;
      end;
      MsgHandled := True;
      Exit;
    end;

    if FListBoxUpBtnDown and not ListBoxVertUpButtonRect.Contains(P) and (FVUpState = tsArrowBtnUpPressed)
    then
    begin
      FVUpState := tsArrowBtnUpNormal;
      DrawListBoxVertScroll(0);
      ListBoxStopTimer;
      Exit;
    end;

    if FListBoxUpBtnDown and ListBoxVertUpButtonRect.Contains(P) and (FVUpState = tsArrowBtnUpNormal)
    then
    begin
      FVUpState := tsArrowBtnUpPressed;
      DrawListBoxVertScroll(0);
      ListBoxSetTimer(5);
      Exit;
    end;

    if FListBoxDownBtnDown and not ListBoxVertDownButtonRect.Contains(P) and (FVDownState = tsArrowBtnDownPressed)
    then
    begin
      FVDownState := tsArrowBtnDownNormal;
      DrawListBoxVertScroll(0);
      ListBoxStopTimer;
      Exit;
    end;

    if FListBoxDownBtnDown and ListBoxVertDownButtonRect.Contains(P) and (FVDownState = tsArrowBtnDownNormal)
    then
    begin
      FVDownState := tsArrowBtnDownPressed;
      DrawListBoxVertScroll(0);
      ListBoxSetTimer(6);
      Exit;
    end;

    if ListBoxVertScrollArea.Contains(P) then
    begin
      if ListBoxVertSliderRect.Contains(P) and (FVSliderState = tsThumbBtnVertNormal) then
      begin
        FVSliderState := tsThumbBtnVertHot;
        DrawListBoxVertScroll(0);
      end
      else if not ListBoxVertSliderRect.Contains(P) and (FVSliderState = tsThumbBtnVertHot) then
      begin
        FVSliderState := tsThumbBtnVertNormal;
        DrawListBoxVertScroll(0);
      end
      else if ListBoxVertUpButtonRect.Contains(P) and (FVUpState = tsArrowBtnUpNormal) then
      begin
        FVUpState := tsArrowBtnUpHot;
        DrawListBoxVertScroll(0);
      end
      else if not ListBoxVertUpButtonRect.Contains(P) and (FVUpState = tsArrowBtnUpHot) then
      begin
        FVUpState := tsArrowBtnUpNormal;
        DrawListBoxVertScroll(0);
      end
      else if ListBoxVertDownButtonRect.Contains(P) and (FVDownState = tsArrowBtnDownNormal) then
      begin
        FVDownState :=  tsArrowBtnDownHot;
        DrawListBoxVertScroll(0);
      end
      else if not ListBoxVertDownButtonRect.Contains(P) and (FVDownState =  tsArrowBtnDownHot) then
      begin
        FVDownState :=  tsArrowBtnDownNormal;
        DrawListBoxVertScroll(0);
      end;
      MsgHandled := True;
    end
    else
    begin
      if (FVSliderState <> tsThumbBtnVertNormal) or (FVUpState <> tsArrowBtnUpNormal) or
         (FVUpState <> tsArrowBtnDownNormal) then
      begin
        if FListBoxTimerCode <> 0 then ListBoxStopTimer;
        FVSliderState := tsThumbBtnVertNormal;
        FVUpState := tsArrowBtnUpNormal;
        FVDownState := tsArrowBtnDownNormal;
        DrawListBoxVertScroll(0);
      end;
    end;
  end;

  procedure WMLButtonUp(var Msg: TWMMouse);
  var
    P: TPoint;
  begin
    FListBoxUpBtnDown := False;
    FListBoxDownBtnDown := False;
    FListBoxTrackUpDown := False;
    FListBoxTrackDownDown := False;

    P := Point(Msg.XPos, Msg.YPos);
    if Control.BiDiMode = bdRightToLeft then P.X := - P.X;

    if (Style = csSimple) and ListBoxVertScrollArea.Contains(FDownPos)
    then
      ReleaseCapture;


    if ListBoxVertSliderRect.Contains(P) then
      FVSliderState := tsThumbBtnVertHot
    else
      FVSliderState := tsThumbBtnVertNormal;

    if ListBoxVertUpButtonRect.Contains(P) then
      FVUpState := tsArrowBtnUpHot
    else
      FVUpState := tsArrowBtnUpNormal;

    if ListBoxVertDownButtonRect.Contains(P) then
      FVDownState := tsArrowBtnDownHot
    else
      FVDownState := tsArrowBtnDownNormal;

    DrawListBoxVertScroll(0);

    if FListBoxTimerCode <> 0 then
      ListBoxStopTimer;

    MsgHandled := ListBoxVertScrollArea.Contains(P);
  end;

  procedure WMNCLButtonDown(var Msg: TWMMouse);
  var
    P: TPoint;
  begin
    if Style <> csSimple then
      SetCapture(FListHandle);
    P := Point(Msg.XPos, Msg.YPos);
    ScreenToClient(FListHandle, P);
    with P do
    begin
      Msg.XPos := X;
      Msg.YPos := Y;
    end;
    WMLButtonDown(Msg);
    MsgHandled := True;
  end;

  procedure WMPrint(var Msg: TMessage);
  var
    SaveIndex: Integer;
    Canvas: TCanvas;
    R: TRect;
  begin
    Msg.Result := CallWindowProc(FDefListBoxProc, FListHandle, Msg.Msg, Msg.WParam, Msg.LParam);

    if (Msg.LParam and PRF_NONCLIENT = PRF_NONCLIENT) and
       (Msg.wParam > 0) then
    begin
      SaveIndex := 0;
      Canvas := TCanvas.Create;
      try
        SaveIndex := SaveDC(Msg.WParam);
        Canvas.Handle := Msg.WParam;
        GetWindowRect(FListHandle, R);
        OffsetRect(R, -R.Left, -R.Top);
        ExcludeClipRect(Canvas.Handle, R.Left + 2, R.Top + 2, R.Right - 2, R.Bottom - 2);
        PaintListBoxBorder(Canvas, R);
      finally
        if SaveIndex <> 0 then
          RestoreDC(Canvas.Handle, SaveIndex);
        Canvas.Handle := 0;
        Canvas.Free;
      end;
      DrawListBoxVertScroll(Msg.wParam);
    end;
    MsgHandled := True;
  end;

  procedure WMTimer(var Msg: TMessage);
  var
    R: TRect;
    ItemHeight, VisibleCount, TopIndex: Integer;
  begin
    case FListBoxTimerCode of
      1: ListBoxSetTimer(5);
      2: ListBoxSetTimer(6);
      3: ListBoxSetTimer(7);
      4: ListBoxSetTimer(8);
      5:
        begin
          SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
          SendMessage(FListHandle, LB_SETTOPINDEX,
            SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0) - 1, 0);
          SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
          R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
          RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_ERASE);
          DrawListBoxVertScroll(0);
        end;
      6:
        begin
          SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
          SendMessage(FListHandle, LB_SETTOPINDEX,
            SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0) + 1, 0);
          SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
          R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
          RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_ERASE);
          DrawListBoxVertScroll(0);
        end;
      7:
        begin
          if ListBoxVertSliderRect.Contains(FMovePos) or (FMovePos.Y > ListBoxVertSliderRect.Bottom) then
          begin
            ListBoxStopTimer;
            Exit;
          end;
          ItemHeight := SendMessage(FListHandle, LB_GETITEMHEIGHT, 0, 0);
          if ItemHeight > 0 then
            VisibleCount := ListBoxClientRect.Height div ItemHeight
          else
            VisibleCount := 0;
          TopIndex := SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0) - VisibleCount + 1;
          if TopIndex < 0 then TopIndex := 0;
          SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
          SendMessage(FListHandle, LB_SETTOPINDEX, TopIndex, 0);
          SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
          R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
          RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_ERASE);
          DrawListBoxVertScroll(0);
        end;
      8:
        begin
          if ListBoxVertSliderRect.Contains(FMovePos) or (FMovePos.Y < ListBoxVertSliderRect.Top) then
          begin
            ListBoxStopTimer;
            Exit;
          end;
          ItemHeight := SendMessage(FListHandle, LB_GETITEMHEIGHT, 0, 0);
          if ItemHeight > 0 then
            VisibleCount := ListBoxClientRect.Height div ItemHeight
          else
            VisibleCount := 0;
          TopIndex := SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0) + VisibleCount - 1;
          SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
          SendMessage(FListHandle, LB_SETTOPINDEX, TopIndex, 0);
          SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
          R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
          RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_ERASE);
          DrawListBoxVertScroll(0);
        end;
    end;
  end;

begin
  MsgHandled := False;
  if ListBoxVertScrollArea.Height = 0 then
  begin
    case Msg.Msg of
      WM_NCCALCSIZE:
        WMNCCalcSize(TWMNCCalcSize(Msg));
      WM_NCPAINT:
         begin
           DrawListBoxBorder;
           MsgHandled := True;
         end;
    end;
  end
  else
    case Msg.Msg of
      WM_NCHITTEST:
        if Style = csSimple then
        begin
          Msg.Result := HTCLIENT;
          MsgHandled := True;
        end;
      WM_MOUSELEAVE, WM_NCMOUSELEAVE:
        if Style = csSimple then
        begin
          FVSliderState := tsThumbBtnVertNormal;
          FVUpState := tsArrowBtnUpNormal;
          FVDownState := tsArrowBtnDownNormal;
          DrawListBoxVertScroll(0);
        end;
      WM_TIMER: WMTimer(Msg);
      WM_UpdateUIState: MsgHandled := True;
      WM_NCCALCSIZE: WMNCCalcSize(TWMNCCalcSize(Msg));
      WM_MOUSEWHEEL: WMMouseWheel(TWMMouseWheel(Msg));
      WM_NCLButtonDblClk: WMNCLButtonDblClk(TWMMouse(Msg));
      WM_LButtonDown: WMLButtonDown(TWMMouse(Msg));
      WM_MOUSEMOVE: WMMouseMove(TWMMouse(Msg));
      WM_LBUTTONUP: WMLButtonUp(TWMMouse(Msg));
      WM_NCLButtonDown: WMNCLButtonDown(TWMMouse(Msg));
      WM_NCLButtonUp, WM_NCMouseMove: MsgHandled := True;
      WM_PRINT: WMPrint(Msg);
      WM_KEYDOWN, WM_KEYUP:
        begin
          Msg.Result := CallWindowProc(FDefListBoxProc, FListHandle,
            Msg.Msg, Msg.WParam, Msg.LParam);
          DrawListBoxVertScroll(0);
          MsgHandled := True;
        end;
      WM_NCPAINT:
       begin
         DrawListBoxBorder;
         DrawListBoxVertScroll(0);
         MsgHandled := True;
       end;
      LB_SETTOPINDEX:
        begin
          Msg.Result := CallWindowProc(FDefListBoxProc, FListHandle,
            Msg.Msg, Msg.WParam, Msg.LParam);
          DrawListBoxVertScroll(0);
          MsgHandled := True;
        end;
      WM_STYLECHANGED, WM_STYLECHANGING:
       if FIgnoreStyleChanged then
       begin
         Msg.Result := 0;
         MsgHandled := True;
       end;
    end;
  if not MsgHandled then
    Msg.Result := CallWindowProc(FDefListBoxProc, FListHandle,
      Msg.Msg, Msg.WParam, Msg.LParam);
end;

{ TColorizerTrackBarStyleHook }

function TColorizerTrackBarStyleHook.AcceptMessage(
  var Message: TMessage): Boolean;
begin
  Result := {$IFDEF DELPHIXE3_UP}seClient in Control.StyleElements{$ELSE}True{$ENDIF};
end;

procedure TColorizerTrackBarStyleHook.CNHScroll(var Message: TWMHScroll);
begin
  Invalidate;
end;

procedure TColorizerTrackBarStyleHook.CNVScroll(var Message: TWMVScroll);
begin
  Invalidate;
end;

constructor TColorizerTrackBarStyleHook.Create(AControl: TWinControl);
begin
  inherited;
  OverridePaint := True;
  DoubleBuffered := True;
  FThumbPressed := False;
end;

procedure TColorizerTrackBarStyleHook.Paint(Canvas: TCanvas);
var
  R, R1, ThumbRect: TRect;
  WinStyle: Cardinal;
  Thumb: TThemedTrackBar;
  I, TickCount, TickStart, TickEnd, TickPos: Integer;
  Details: TThemedElementDetails;
begin
  if not ColorizerStyleServices.Available then Exit;

  Thumb := ttbTrackBarDontCare;
  { Track }
  WinStyle := GetWindowLong(Handle, GWL_STYLE);
  SendMessage(Handle, TBM_GETCHANNELRECT, 0, IntPtr(@R));
  if WinStyle and TBS_VERT = 0 then
  begin
    Details := ColorizerStyleServices.GetElementDetails(ttbTrack);
    ColorizerStyleServices.DrawElement(Canvas.Handle, Details, R);
  end
  else
  begin
    R1 := R;
    R.Left := R1.Top;
    R.Top := R1.Left;
    R.Right := R1.Bottom;
    R.Bottom := R1.Right;
    Details := ColorizerStyleServices.GetElementDetails(ttbTrackVert);
    ColorizerStyleServices.DrawElement(Canvas.Handle, Details, R);
  end;

  SendMessage(Handle, TBM_GETCHANNELRECT, 0, IntPtr(@R));
  SendMessage(Handle, TBM_GETTHUMBRECT, 0, IntPtr(@ThumbRect));

  // Ticks
  if WinStyle and TBS_NOTICKS = 0 then
  begin
    TickCount := SendMessage(Handle, TBM_GETNUMTICS, 0, 0);

    Canvas.Pen.Color := ColorizerStyleServices.GetSystemColor(clBtnText);

    // First
    if WinStyle and TBS_VERT = 0 then
    begin
      TickPos := R.Left + ThumbRect.Width div 2;
      if (WinStyle and TBS_TOP = TBS_TOP) or (WinStyle and TBS_BOTH = TBS_BOTH) then
      begin
        Canvas.MoveTo(TickPos, R.Top - 7);
        Canvas.LineTo(TickPos, R.Top - 3);
      end;
      if (WinStyle and TBS_TOP = 0) or (WinStyle and TBS_BOTH = TBS_BOTH) then
      begin
        Canvas.MoveTo(TickPos, R.Bottom + 3);
        Canvas.LineTo(TickPos, R.Bottom + 7);
      end;
      TickStart := TickPos;
    end
    else
    begin
      TickPos := R.Left + ThumbRect.Height div 2;
      if (WinStyle and TBS_TOP = TBS_TOP) or (WinStyle and TBS_BOTH = TBS_BOTH) then
      begin
        Canvas.MoveTo(R.Top - 7, TickPos);
        Canvas.LineTo(R.Top - 3, TickPos);
      end;
      if (WinStyle and TBS_TOP = 0) or (WinStyle and TBS_BOTH = TBS_BOTH) then
      begin
        Canvas.MoveTo(R.Bottom + 3, TickPos);
        Canvas.LineTo(R.Bottom + 7, TickPos);
      end;
      TickStart := TickPos;
    end;
    // last
    if WinStyle and TBS_VERT = 0 then
    begin
      TickPos := R.Right - ThumbRect.Width div 2;
      if (WinStyle and TBS_TOP = TBS_TOP) or (WinStyle and TBS_BOTH = TBS_BOTH) then
      begin
        Canvas.MoveTo(TickPos, R.Top - 7);
        Canvas.LineTo(TickPos, R.Top - 3);
      end;
      if (WinStyle and TBS_TOP = 0) or (WinStyle and TBS_BOTH = TBS_BOTH) then
      begin
        Canvas.MoveTo(TickPos, R.Bottom + 3);
        Canvas.LineTo(TickPos, R.Bottom + 7);
      end;
      TickEnd := TickPos;
    end
    else
    begin
      TickPos := R.Right - ThumbRect.Height div 2;
      if (WinStyle and TBS_TOP = TBS_TOP) or (WinStyle and TBS_BOTH = TBS_BOTH) then
      begin
        Canvas.MoveTo(R.Top - 7, TickPos);
        Canvas.LineTo(R.Top - 3, TickPos);
      end;
      if (WinStyle and TBS_TOP = 0) or (WinStyle and TBS_BOTH = TBS_BOTH) then
      begin
        Canvas.MoveTo(R.Bottom + 3, TickPos);
        Canvas.LineTo(R.Bottom + 7, TickPos);
      end;
      TickEnd := TickPos;
    end;
    //ticks
    for I := 1 to TickCount - 1 do
    begin
      TickPos := TickStart + Round((TickEnd - TickStart) * (I/(TickCount-1)));
      if WinStyle and TBS_VERT = 0 then
      begin
        if (WinStyle and TBS_TOP = TBS_TOP) or (WinStyle and TBS_BOTH = TBS_BOTH) then
        begin
          Canvas.MoveTo(TickPos, R.Top - 6);
          Canvas.LineTo(TickPos, R.Top - 3);
        end;
        if (WinStyle and TBS_TOP = 0) or (WinStyle and TBS_BOTH = TBS_BOTH) then
        begin
          Canvas.MoveTo(TickPos, R.Bottom + 3);
          Canvas.LineTo(TickPos, R.Bottom + 6);
        end;
      end
      else
      begin
        if (WinStyle and TBS_TOP = TBS_TOP) or (WinStyle and TBS_BOTH = TBS_BOTH) then
        begin
          Canvas.MoveTo(R.Top - 6, TickPos);
          Canvas.LineTo(R.Top - 3, TickPos);
        end;
        if (WinStyle and TBS_TOP = 0) or (WinStyle and TBS_BOTH = TBS_BOTH) then
        begin
          Canvas.MoveTo(R.Bottom + 3, TickPos);
          Canvas.LineTo(R.Bottom + 6, TickPos);
        end;
      end;
    end;
  end;

  // Thumb
  if WinStyle and TBS_NOTHUMB = 0 then
  begin
    SendMessage(Handle, TBM_GETTHUMBRECT, 0, IntPtr(@R));
    if not Control.Enabled then
    begin
      if WinStyle and TBS_VERT = 0 then
      begin
       if WinStyle and TBS_BOTH = TBS_BOTH then
          Thumb := ttbThumbDisabled
        else
        if WinStyle and TBS_TOP = TBS_TOP then
          Thumb := ttbThumbTopDisabled
        else
        if WinStyle and TBS_BOTTOM = TBS_BOTTOM then
          Thumb := ttbThumbBottomDisabled;
      end
      else
      begin
        Thumb := ttbThumbRightDisabled;
        if WinStyle and TBS_TOP = TBS_TOP then
        Thumb := ttbThumbLeftDisabled else
        if WinStyle and TBS_BOTH = TBS_BOTH then
         Thumb := ttbThumbVertDisabled;
      end;
    end
    else if FThumbPressed then
    begin
      if WinStyle and TBS_VERT = 0 then
      begin
        if WinStyle and TBS_BOTH = TBS_BOTH then
          Thumb := ttbThumbPressed
        else
        if WinStyle and TBS_TOP = TBS_TOP then
          Thumb := ttbThumbTopPressed
        else
        if WinStyle and TBS_BOTTOM = TBS_BOTTOM then
          Thumb := ttbThumbBottomPressed;
      end
      else
      begin
        Thumb := ttbThumbRightPressed;
        if WinStyle and TBS_TOP = TBS_TOP then
        Thumb := ttbThumbLeftPressed else
        if WinStyle and TBS_BOTH = TBS_BOTH then
         Thumb := ttbThumbVertPressed;
      end;
    end
    else if FMouseOnThumb then
    begin
      if WinStyle and TBS_VERT = 0 then
      begin
        if WinStyle and TBS_BOTH = TBS_BOTH then
          Thumb := ttbThumbHot
        else
        if WinStyle and TBS_TOP = TBS_TOP then
          Thumb := ttbThumbTopHot
        else
        if WinStyle and TBS_BOTTOM = TBS_BOTTOM then
          Thumb := ttbThumbBottomHot;
      end
      else
      begin
        Thumb := ttbThumbRightHot;
        if WinStyle and TBS_TOP = TBS_TOP then
        Thumb := ttbThumbLeftHot else
        if WinStyle and TBS_BOTH = TBS_BOTH then
         Thumb := ttbThumbVertHot;
      end;
    end
    else
    begin
      if WinStyle and TBS_VERT = 0 then
      begin
         if WinStyle and TBS_BOTH = TBS_BOTH then
          Thumb := ttbThumbNormal
        else
        if WinStyle and TBS_TOP = TBS_TOP then
          Thumb := ttbThumbTopNormal
        else
        if WinStyle and TBS_BOTTOM = TBS_BOTTOM then
          Thumb := ttbThumbBottomNormal;
      end
      else
      begin
        Thumb := ttbThumbRightNormal;
        if WinStyle and TBS_TOP = TBS_TOP then
        Thumb := ttbThumbLeftNormal else
        if WinStyle and TBS_BOTH = TBS_BOTH then
         Thumb := ttbThumbVertNormal;
      end;
    end;

    Details := ColorizerStyleServices.GetElementDetails(Thumb);
    ColorizerStyleServices.DrawElement(Canvas.Handle, Details, R);
  end;

  if Focused then
    Canvas.DrawFocusRect(Rect(0, 0, Control.Width, Control.Height));
end;


procedure TColorizerTrackBarStyleHook.PaintBackground(Canvas: TCanvas);
var
  Details:  TThemedElementDetails;
begin
  if ColorizerStyleServices.Available then
  begin
    Details.Element := teTrackBar;
    ColorizerStyleServices.DrawParentBackground(Handle, Canvas.Handle, Details, False);
  end;
end;


procedure TColorizerTrackBarStyleHook.WMLButtonDown(var Message: TWMMouse);
var
  R: TRect;
begin
  if GetWindowLong(Handle, GWL_STYLE) and TBS_NOTHUMB = 0 then
  begin
    SendMessage(Handle, TBM_GETTHUMBRECT, 0, IntPtr(@R));
    if R.Contains(Point(Message.XPos, Message.YPos)) then
      FThumbPressed := True;
    Invalidate;
  end;
end;

procedure TColorizerTrackBarStyleHook.WMLButtonUp(var Message: TWMMouse);
begin
  if GetWindowLong(Handle, GWL_STYLE) and TBS_NOTHUMB = 0 then
  begin
    FThumbPressed := False;
    Invalidate;
  end;
end;

procedure TColorizerTrackBarStyleHook.WMMouseMove(var Message: TWMMouse);
var
  R: TRect;
  NewValue: Boolean;
begin
  if GetWindowLong(Handle, GWL_STYLE) and TBS_NOTHUMB = 0 then
  begin
    SendMessage(Handle, TBM_GETTHUMBRECT, 0, IntPtr(@R));
    NewValue := R.Contains(Point(Message.XPos, Message.YPos));
    if NewValue <> FMouseOnThumb then
    begin
      FMouseOnThumb := NewValue;
      Invalidate;
    end;
  end;
end;

procedure TColorizerTrackBarStyleHook.WndProc(var Message: TMessage);
begin
  // Reserved for potential updates
  inherited;
  case Message.Msg  of
    TBM_SETPOS:
      Invalidate;
  end;
end;



{ TColorizerTreeViewStyleHook }

constructor TColorizerTreeViewStyleHook.Create(AControl: TWinControl);
var
  LColor: TColor;
begin
  inherited;
  OverrideEraseBkgnd := True;
  with ColorizerStyleServices do
  begin
    if not GetElementColor(GetElementDetails(ttItemNormal), ecTextColor, LColor) or
       (LColor = clNone) then
      LColor :=  GetSystemColor(clWindowText);
  end;
  FontColor := LColor;
  Brush.Color := ColorizerStyleServices.GetStyleColor(scTreeView);
end;

procedure TColorizerTreeViewStyleHook.TVMSetBkColor(var Message: TMessage);
begin
  Message.LParam := LPARAM(ColorToRGB(Brush.Color));
  Handled := False;
end;


procedure TColorizerTreeViewStyleHook.TVMSetTextColor(var Message: TMessage);
begin
  Message.LParam := LPARAM(ColorToRGB(FontColor));
  Handled := False;
end;

procedure TColorizerTreeViewStyleHook.WMMouseMove(var Msg: TWMMouse);
var
  SF: TScrollInfo;
begin
  if VertSliderState = tsThumbBtnVertPressed then
  begin
    SF.fMask := SIF_ALL;
    SF.cbSize := SizeOf(SF);
    GetScrollInfo(Handle, SB_VERT, SF);
    ScrollPos := ScrollPos + (SF.nMax - SF.nMin) * ((Mouse.CursorPos.Y - PrevScrollPos) / VertTrackRect.Height);
    if ScrollPos < SF.nMin then
      ScrollPos := SF.nMin;
    if ScrollPos > SF.nMax then
      ScrollPos := SF.nMax;

    PrevScrollPos := Mouse.CursorPos.Y;
    if Control is TCustomTreeView then
    begin
      PostMessage(Handle, WM_VSCROLL, Integer(SmallPoint(SB_THUMBTRACK, Round(ScrollPos))), 0);
      SF.nPos := Round(ScrollPos);
      SF.nTrackPos := Round(ScrollPos);
      SetScrollInfo(Handle, SB_VERT, SF, True);
    end
    else
      PostMessage(Handle, WM_VSCROLL, Integer(SmallPoint(SB_THUMBPOSITION, Round(ScrollPos))), 0);
    PaintScroll;
    Handled := True;
    Exit;
  end;

  if HorzSliderState = tsThumbBtnHorzPressed then
  begin
    SF.fMask := SIF_ALL;
    SF.cbSize := SizeOf(SF);
    GetScrollInfo(Handle, SB_HORZ, SF);
    ScrollPos := ScrollPos + (SF.nMax - SF.nMin) * ((Mouse.CursorPos.X - PrevScrollPos) / HorzTrackRect.Width);
    if ScrollPos < SF.nMin then
      ScrollPos := SF.nMin;
    if ScrollPos > SF.nMax then
      ScrollPos := SF.nMax;

    PrevScrollPos := Mouse.CursorPos.X;

    if Control is TCustomTreeView then
    begin
      PostMessage(Handle, WM_HSCROLL, Integer(SmallPoint(SB_THUMBTRACK, Round(ScrollPos))), 0);
      SF.nPos := Round(ScrollPos);
      SF.nTrackPos := Round(ScrollPos);
      SetScrollInfo(Handle, SB_HORZ, SF, True);
    end
    else
      PostMessage(Handle, WM_HSCROLL, Integer(SmallPoint(SB_THUMBPOSITION, Round(ScrollPos))), 0);
    PaintScroll;
    Handled := True;
    Exit;
  end;

  if (HorzSliderState <> tsThumbBtnHorzPressed) and (HorzSliderState = tsThumbBtnHorzHot) then
  begin
    HorzSliderState := tsThumbBtnHorzNormal;
    PaintScroll;
  end;

  if (VertSliderState <> tsThumbBtnVertPressed) and (VertSliderState = tsThumbBtnVertHot) then
  begin
    VertSliderState := tsThumbBtnVertNormal;
    PaintScroll;
  end;

  if (HorzUpState <> tsArrowBtnLeftPressed) and (HorzUpState = tsArrowBtnLeftHot) then
  begin
    HorzUpState := tsArrowBtnLeftNormal;
    PaintScroll;
  end;

  if (HorzDownState <> tsArrowBtnRightPressed) and (HorzDownState =tsArrowBtnRightHot) then
  begin
    HorzDownState := tsArrowBtnRightNormal;
    PaintScroll;
  end;

  if (VertUpState <> tsArrowBtnUpPressed) and (VertUpState = tsArrowBtnUpHot) then
  begin
    VertUpState := tsArrowBtnUpNormal;
    PaintScroll;
  end;

  if (VertDownState <> tsArrowBtnDownPressed) and (VertDownState = tsArrowBtnDownHot) then
  begin
    VertDownState := tsArrowBtnDownNormal;
    PaintScroll;
  end;

  CallDefaultProc(TMessage(Msg));
  if LeftButtonDown then
    PaintScroll;
  Handled := True;
end;

procedure TColorizerTreeViewStyleHook.WndProc(var Message: TMessage);
begin
  inherited;

end;

end.
