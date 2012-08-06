unit mbDeskPickerButton;

interface

uses
  SysUtils, Windows, Classes, Controls, StdCtrls, Graphics, Forms, ScreenWin;

type
  TmbDeskPickerButton = class(TButton)
  private
   FSelColor: TColor;
   ScreenFrm: TScreenForm;
   FOnColorPicked: TNotifyEvent;
   FOnKeyDown: TKeyEvent;
   FHintFmt: string;
   FShowScreenHint: boolean;
   OnWUp, OnWDown: TMouseWheelUpDownEvent;
  protected
   procedure StartPicking;
   procedure ColorPicked(Sender: TObject);
   procedure ScreenKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
   procedure WheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
   procedure WheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
  public
   constructor Create(AOwner: TComponent); override;
   procedure Click; override;

   property SelectedColor: TColor read FSelColor;
  published
   property OnSelColorChange: TNotifyEvent read FOnColorPicked write FOnColorPicked;
   property OnScreenKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
   property OnSelMouseWheelUp: TMouseWheelUpDownEvent read OnWUp write OnWUp;
   property OnSelMouseWheelDown: TMouseWheelUpDownEvent read OnWDown write OnWDown;
   property ScreenHintFormat: string read FHintFmt write FHintFmt;
   property ShowScreenHint: boolean read FShowScreenHint write FShowScreenHint default false;
  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('mbColor Lib', [TmbDeskPickerButton]);
end;

constructor TmbDeskPickerButton.Create(AOwner: TComponent);
begin
 inherited;
 DoubleBuffered := true;
 ControlStyle := ControlStyle - [csAcceptsControls] + [csOpaque{$IFDEF DELPHI_7_UP}, csParentBackground{$ENDIF}];
 FHintFmt := 'RGB(%r, %g, %b)'#13'Hex: %h';
 FShowScreenHint := false;
end;

procedure TmbDeskPickerButton.Click;
begin
 inherited;
 StartPicking;
end;

procedure TmbDeskPickerButton.StartPicking;
begin
 ScreenFrm := TScreenForm.Create(Application);
 try
  ScreenFrm.OnSelColorChange := ColorPicked;
  ScreenFrm.OnScreenKeyDown := ScreenKeyDown;
  ScreenFrm.OnMouseWheelDown := WheelDown;
  ScreenFrm.OnMouseWheelUp := WheelUp;
  ScreenFrm.ShowHint := FShowScreenHint;
  ScreenFrm.FHintFormat := FHintFmt;
  ScreenFrm.ShowModal;
 finally
  ScreenFrm.Free;
 end;
end;

procedure TmbDeskPickerButton.ColorPicked(Sender: TObject);
begin
 FSelColor := ScreenFrm.SelectedColor;
 if Assigned(FOnColorPicked) then FOnColorPicked(Self);
end;

procedure TmbDeskPickerButton.ScreenKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 if Assigned(FOnKeyDown) then FOnKeyDown(Self, Key, Shift);
end;

procedure TmbDeskPickerButton.WheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
 if Assigned(OnWUp) then OnWUp(Self, Shift, MousePos, Handled);
end;

procedure TmbDeskPickerButton.WheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
 if Assigned(OnWDown) then OnWDown(Self, Shift, MousePos, Handled);
end;

end.
