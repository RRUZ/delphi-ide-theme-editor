unit SelPropUtils;

interface

uses
 Windows, Classes, Graphics;

procedure DrawSelCross(x, y: integer; Canvas: TCanvas; Color: TColor);
procedure DrawSelCrossCirc(x, y: integer; Canvas: TCanvas; Color: TColor);
procedure DrawSelCirc(x, y: integer; Canvas: TCanvas);
procedure DrawSelSquare(x, y: integer; Canvas: TCanvas);

implementation

procedure DrawSelCross(x, y: integer; Canvas: TCanvas; Color: TColor);
const
 w = 5;
 h = 3;
 o = 8;
var
 R: TRect;
begin
 R := Rect(x-10, y-10, x+9, y+9);
 Canvas.Brush.Color := Color;
 Canvas.FillRect(Rect(R.Left, R.Top + o, R.Left + w, R.Top + o + h));
 Canvas.FillRect(Rect(R.Left + o, R.Top, R.Left + o + h, R.Top + w));
 Canvas.FillRect(Rect(R.Right - w, R.Top + o, R.Right, R.Top + o + h));
 Canvas.FillRect(Rect(R.Left + o, R.Bottom - w, R.Left + o + h, R.Bottom));
end;

procedure DrawSelCrossCirc(x, y: integer; Canvas: TCanvas; Color: TColor);
var
 R: TRect;
begin
 R := Rect(x - 6, y - 6, x + 6, y + 6);
 ExcludeClipRect(Canvas.Handle, x - 6, y - 1, x + 6, y + 1);
 ExcludeClipRect(Canvas.Handle, x - 1, y - 6, x + 1, y + 6);
 Canvas.Pen.Color := Color;
 Canvas.Brush.Style := bsClear;
 InflateRect(R, -1, -1);
 Canvas.Ellipse(R);
 InflateRect(R, -1, -1);
 Canvas.Ellipse(R);
 Canvas.Brush.Style := bsSolid;
end;

procedure DrawSelCirc(x, y: integer; Canvas: TCanvas);
var
 R: TRect;
begin
 R := Rect(x - 5, y - 5, x + 5, y + 5);
 Canvas.Brush.Style := bsClear;
 Canvas.Pen.Mode := pmNot;
 Canvas.Ellipse(R);
 Canvas.Pen.Mode := pmCopy;
 Canvas.Brush.Style := bsSolid;
end;

procedure DrawSelSquare(x, y: integer; Canvas: TCanvas);
var
 R: TRect;
begin
 R := Rect(x - 5, y - 5, x + 5, y + 5);
 Canvas.Brush.Style := bsClear;
 Canvas.Pen.Mode := pmNot;
 Canvas.Rectangle(R);
 Canvas.Pen.Mode := pmCopy;
 Canvas.Brush.Style := bsSolid;
end;

end.
