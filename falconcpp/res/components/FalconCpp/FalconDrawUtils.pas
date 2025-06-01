unit FalconDrawUtils;

interface

uses
  Windows, Messages, Classes, Controls;

procedure DrawParentBackground(Control: TControl; DC: HDC; Rc: TRect);

implementation

procedure DrawParentBackground(Control: TControl; DC: HDC; Rc: TRect);
var
  SaveDCIndex: Integer;
  P: TPoint;
begin
  if Control.Parent = nil then
    Exit;
  SaveDCIndex := SaveDC(DC);
  GetViewportOrgEx(DC, P);
  SetViewportOrgEx(DC, P.X - Control.Left, P.Y - Control.Top, nil);
  IntersectClipRect(DC, Rc.Left + Control.Left - P.X, Rc.Top + Control.Top - P.Y,
    Rc.Right + Control.Left - P.X, Rc.Bottom + Control.Top - P.Y);
  if not (csDesigning in Control.ComponentState) then
  begin
    Control.Parent.Perform(WM_ERASEBKGND, DC, 0);
    Control.Parent.Perform(WM_PAINT, DC, 0);
  end
  else
  begin
    try
      Control.Parent.Perform(WM_ERASEBKGND, DC, 0);
      Control.Parent.Perform(WM_PAINT, DC, 0);
    except
    end;
  end;
  RestoreDC(DC, SaveDCIndex);
end;

end.
