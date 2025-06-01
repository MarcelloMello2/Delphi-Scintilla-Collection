unit XPPanels;

interface

uses
  Windows, Messages, Forms, Themes, SysUtils, Classes, Controls, StdCtrls,
  ExtCtrls, Graphics;

type
  TCustomXPPanel = class(TCustomControl)
  private
    { Private declarations }
    FBorderWidth: TBorderWidth;
    FParentBackgroundSet: Boolean;
    FBorderStyle: TBorderStyle;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure WMSize(var Message: TMessage); message WM_SIZE;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure SetBorderWidth(Value: TBorderWidth);
    procedure SetBorderStyle(Value: TBorderStyle);
  protected
    { Protected declarations }
    procedure AdjustDrawRect(var Rect: TRect); virtual;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    property BorderWidth: TBorderWidth read FBorderWidth write SetBorderWidth default 0;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    procedure SetParentBackground(Value: Boolean); override;
  public
    { Public declarations }
    property ParentBackground stored FParentBackgroundSet;
    constructor Create(AOwner: TComponent); override;
  end;

  TXPPanel = class(TCustomXPPanel)
  published
    { Published declarations }
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBiDiMode;
    property ParentBackground;
    property ParentColor;
    property ParentCtl3D;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;


  TCustomSplitterPanel = class(TCustomXPPanel)
  private
    { Private declarations }
    FActiveControl: TWinControl;
    FSizing: Boolean;
    FDownPos: TPoint;
    FLineDC: HDC;
    FLineVisible: Boolean;
    FOldKeyDown: TKeyEvent;
    FOldSize: Integer;
    FMaxSize: Integer;
    FMinSize: NaturalNumber;
    FNewSize: Integer;
    FBrush: TBrush;
    FPrevBrush: HBrush;
    FResizeStyle: TResizeStyle;
    FSplit: Integer;
    FSize: Integer;
    FOnCanResize: TCanResizeEvent;
    FOnMoved: TNotifyEvent;
    procedure AllocateLineDC;
    procedure DrawLine;
    procedure FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ReleaseLineDC;
    procedure SetSize(Value: Integer);
    procedure GetSplitRect(var Rect: TRect);
    function InSplitRect(X, Y: Integer): Boolean;
    procedure UpdateSize(X, Y: Integer);
    procedure CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
    procedure UpdateControlSize;
  protected
    { Protected declarations }
    function CanResize(var NewSize, Split: Integer): Boolean; reintroduce; virtual;
    procedure AdjustDrawRect(var Rect: TRect); override;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure StopSizing; dynamic;
    property ResizeStyle: TResizeStyle read FResizeStyle write FResizeStyle
      default rsPattern;
    property Size: Integer read FSize write SetSize default 3;
    property MinSize: NaturalNumber read FMinSize write FMinSize default 30;
    property OnCanResize: TCanResizeEvent read FOnCanResize write FOnCanResize;
    property OnMoved: TNotifyEvent read FOnMoved write FOnMoved;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TSplitterPanel = class(TCustomSplitterPanel)
  published
    { Published declarations }
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property MinSize;
    property ParentBiDiMode;
    property ParentBackground;
    property ParentColor;
    property ParentCtl3D;
    property PopupMenu;
    property ResizeStyle;
    property Size;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMoved;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

uses Types;

{ TCustomXPPanel }

constructor TCustomXPPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csOpaque, csDoubleClicks, csReplicatable];
  { When themes are on in an application default to making
    TCustomXPPanel's paint with their ParentBackground }
  if ThemeServices.ThemesEnabled then
    ControlStyle := ControlStyle + [csParentBackground] - [csOpaque];
  Width := 185;
  Height := 41;
  BorderStyle := bsSingle;
  DoubleBuffered := True;
  UseDockManager := True;
end;

procedure TCustomXPPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
    style := style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TCustomXPPanel.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  InflateRect(Rect, -BorderWidth, -BorderWidth);
end;

procedure TCustomXPPanel.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
  Realign;
end;

procedure TCustomXPPanel.WMSize(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TCustomXPPanel.Paint;
var
  R: TRect;
  Box: TThemedButton;
  Details: TThemedElementDetails;
begin
  R := GetClientRect;
  with Canvas do
  begin
    if ThemeServices.ThemesEnabled and (BorderStyle <> bsNone) then
    begin
      if Enabled then
        Box := tbGroupBoxNormal
      else
        Box := tbGroupBoxDisabled;
      Details := ThemeServices.GetElementDetails(Box);
      ThemeServices.DrawElement(Handle, Details, R);
    end
    else
    begin
      if not ParentBackground then
      begin
        Brush.Color := Color;
        FillRect(R);
      end;
      if (BorderStyle <> bsNone) then
      begin
        if Ctl3D then
        begin
          Inc(R.Left);
          Inc(R.Top);
          Brush.Color := clBtnHighlight;
          FrameRect(R);
          OffsetRect(R, -1, -1);
          Brush.Color := clBtnShadow;
        end else
          Brush.Color := clWindowFrame;
        FrameRect(R);
      end;
    end;
  end;
end;

procedure TCustomXPPanel.SetBorderWidth(Value: TBorderWidth);
begin
  FBorderWidth := Value;
  Realign;
  Invalidate;
end;

procedure TCustomXPPanel.SetParentBackground(Value: Boolean);
begin
  { TCustomXPPanel needs to not have csOpaque when painting
    with the ParentBackground in Themed applications }
  if Value then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];
  FParentBackgroundSet := Value;
  inherited;
end;

procedure TCustomXPPanel.AdjustDrawRect(var Rect: TRect);
begin
end;

procedure TCustomXPPanel.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    Invalidate;
  end;
end;

procedure TCustomXPPanel.WMEraseBkgnd(var Message: TWmEraseBkgnd);
var
  R: TRect;
  Box: TThemedButton;
  Details: TThemedElementDetails;
begin
  inherited;
  R := GetClientRect;
  FillRect(Message.DC, R, Brush.Handle);
  AdjustDrawRect(R);
  if ThemeServices.ThemesEnabled and (BorderStyle <> bsNone) then
  begin
    if Enabled then
      Box := tbGroupBoxNormal
    else
      Box := tbGroupBoxDisabled;
    Details := ThemeServices.GetElementDetails(Box);
    ThemeServices.DrawElement(Message.DC, Details, R);
  end
  else
  begin
    if (BorderStyle <> bsNone) then
    begin
      if Ctl3D then
      begin
        Inc(R.Left);
        Inc(R.Top);
        Brush.Color := clBtnHighlight;
        FrameRect(Message.DC, R, Brush.Handle);
        OffsetRect(R, -1, -1);
        Brush.Color := clBtnShadow;
      end else
        Brush.Color := clWindowFrame;
      FrameRect(Message.DC, R, Brush.Handle);
    end;
  end;
  Message.Result := 1;
end;

{ TCustomSplitterPanel }

type
  TWinControlAccess = class(TWinControl);

constructor TCustomSplitterPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSize := 3;
  FMinSize := 30;
  FResizeStyle := rsPattern;
  FOldSize := -1;
end;

procedure TCustomSplitterPanel.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  case Align of
    alLeft: Dec(Rect.Right, FSize);
    alRight: Inc(Rect.Left, FSize);
    alTop: Dec(Rect.Bottom, FSize);
    alBottom: Inc(Rect.Top, FSize);
  end;
end;

procedure TCustomSplitterPanel.SetSize(Value: Integer);
begin
  if Value <> FSize then
  begin
    FSize := Value;
    Realign;
    if csDesigning in ComponentState then
      Invalidate;
  end;
end;

procedure TCustomSplitterPanel.GetSplitRect(var Rect: TRect);
begin
  Rect := GetClientRect;
  case Align of
    alLeft: Rect.Left := Rect.Right - FSize;
    alRight: Rect.Right := Rect.Left + FSize;
    alTop: Rect.Top := Rect.Bottom - FSize;
    alBottom: Rect.Bottom := Rect.Top + FSize;
  end;
end;

procedure TCustomSplitterPanel.AdjustDrawRect(var Rect: TRect);
begin
  inherited;
  case Align of
    alLeft: Dec(Rect.Right, FSize);
    alRight: Inc(Rect.Left, FSize);
    alTop: Dec(Rect.Bottom, FSize);
    alBottom: Inc(Rect.Top, FSize);
  end;
end;

procedure TCustomSplitterPanel.Paint;
const
  XorColor = $00FFD8CE;
var
  R: TRect;
begin
  inherited;
  if csDesigning in ComponentState then
    { Draw outline }
    with Canvas do
    begin
      Pen.Style := psDot;
      Pen.Mode := pmXor;
      Pen.Color := XorColor;
      Brush.Style := bsClear;
      GetSplitRect(R);
      Rectangle(R.Left, R.Top, R.Right, R.Bottom);
    end;
end;

procedure TCustomSplitterPanel.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  inherited;
  if (Button = mbLeft) and InSplitRect(X, Y) then
  begin
    FSizing := True;
    FDownPos := Point(X, Y);
    if Align in [alLeft, alRight] then
    begin
      FMaxSize := Parent.ClientWidth - FMinSize;
      for I := 0 to Parent.ControlCount - 1 do
      begin
        if Parent.Controls[I] = Self then
          Continue;
        with Parent.Controls[I] do
          if Visible and (Align in [alLeft, alRight]) then
            Dec(FMaxSize, Width);
      end;
    end
    else
    begin
      FMaxSize := Parent.ClientHeight - FMinSize;
      for I := 0 to Parent.ControlCount - 1 do
      begin
        if Parent.Controls[I] = Self then
          Continue;
        with Parent.Controls[I] do
          if Align in [alTop, alBottom] then
            Dec(FMaxSize, Height);
      end;
    end;
    UpdateSize(X, Y);
    AllocateLineDC;
    with ValidParentForm(Self) do
      if ActiveControl <> nil then
      begin
        FActiveControl := ActiveControl;
        FOldKeyDown := TWinControlAccess(FActiveControl).OnKeyDown;
        TWinControlAccess(FActiveControl).OnKeyDown := FocusKeyDown;
      end;
    if ResizeStyle in [rsLine, rsPattern] then DrawLine;
  end;
end;

procedure TCustomSplitterPanel.MouseMove(Shift: TShiftState; X,
  Y: Integer);
var
  NewSize, Split: Integer;
  Rect: TRect;
  P: TPoint;
begin
  inherited;
  GetSplitRect(Rect);
  P := Point(X, Y);
  if PtInRect(Rect, P) then
  begin
    if Align in[alLeft, alRight] then
      Cursor := crHSplit
    else
      Cursor := crVSplit;
  end
  else
    Cursor := crDefault;
  if FSizing then
  begin
    CalcSplitSize(X, Y, NewSize, Split);
    if CanResize(NewSize, Split) then
    begin
      if ResizeStyle in [rsLine, rsPattern] then DrawLine;
      FNewSize := NewSize;
      FSplit := Split;
      if ResizeStyle = rsUpdate then UpdateControlSize;
      if ResizeStyle in [rsLine, rsPattern] then DrawLine;
    end;
  end;
end;

procedure TCustomSplitterPanel.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FSizing then
  begin
    if ResizeStyle in [rsLine, rsPattern] then DrawLine;
    UpdateControlSize;
    StopSizing;
  end;
end;

function TCustomSplitterPanel.CanResize(var NewSize, Split: Integer): Boolean;
begin
  Result := (NewSize > Size);
  if not Result then
  begin
    Split := Split + (Size - NewSize);
    NewSize := Size;
    Result := True;
  end;
  if Assigned(FOnCanResize) then
    FOnCanResize(Self, NewSize, Result);
end;

procedure TCustomSplitterPanel.UpdateControlSize;
begin
  if FNewSize <> FOldSize then
  begin
    case Align of
      alLeft:
      begin
        Width := FNewSize;
      end;
      alTop:
      begin
        Height := FNewSize;
      end;
      alRight:
        begin
          Parent.DisableAlign;
          try
            Left := Left + (Width - FNewSize);
            Width := FNewSize;
          finally
            Parent.EnableAlign;
          end;
        end;
      alBottom:
        begin
          Parent.DisableAlign;
          try
            Top := Top + (Height - FNewSize);
            Height := FNewSize;
          finally
            Parent.EnableAlign;
          end;
        end;
    end;
    Update;
    if Assigned(FOnMoved) then
      FOnMoved(Self);
    FOldSize := FNewSize;
  end;
end;

procedure TCustomSplitterPanel.UpdateSize(X, Y: Integer);
begin
  CalcSplitSize(X, Y, FNewSize, FSplit);
end;

procedure TCustomSplitterPanel.CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
var
  S: Integer;
begin
  if Align in [alLeft, alRight] then
    Split := X - FDownPos.X
  else
    Split := Y - FDownPos.Y;
  S := 0;
  case Align of
    alLeft: S := Width + Split;
    alRight: S := Width - Split;
    alTop: S := Height + Split;
    alBottom: S := Height - Split;
  end;
  NewSize := S;
  if S > FMaxSize then
    NewSize := FMaxSize;
  if S <> NewSize then
  begin
    if Align in [alRight, alBottom] then
      S := S - NewSize
    else
      S := NewSize - S;
    Inc(Split, S);
  end;
end;

procedure TCustomSplitterPanel.AllocateLineDC;
begin
  FLineDC := GetDCEx(Parent.Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS
    or DCX_LOCKWINDOWUPDATE);
  if ResizeStyle = rsPattern then
  begin
    if FBrush = nil then
    begin
      FBrush := TBrush.Create;
{$IFDEF MSWINDOWS}
      FBrush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
{$ENDIF}
{$IFDEF LINUX}
      FBrush.Color := clGray;
{$ENDIF}
    end;
    FPrevBrush := SelectObject(FLineDC, FBrush.Handle);
  end;
end;

procedure TCustomSplitterPanel.DrawLine;
var
  P: TPoint;
  Rect: TRect;
  splitWidth, splitHeight: Integer;
begin
  FLineVisible := not FLineVisible;
  GetSplitRect(Rect);
  splitWidth := Rect.Right - Rect.Left;
  splitHeight := Rect.Bottom - Rect.Top;
  P := Point(Left, Top);
  case Align of
    alLeft: P.X := Left + Width - FSize + FSplit;
    alRight: P.X := Left + FSplit;
    alTop: P.Y := Top + Height - FSize + FSplit;
    alBottom: P.Y := Top + FSplit;
  end;
  with P do
    PatBlt(FLineDC, X, Y, splitWidth, splitHeight, PATINVERT);
end;

procedure TCustomSplitterPanel.FocusKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    StopSizing
  else if Assigned(FOldKeyDown) then
    FOldKeyDown(Sender, Key, Shift);
end;

procedure TCustomSplitterPanel.ReleaseLineDC;
begin
  if FPrevBrush <> 0 then
    SelectObject(FLineDC, FPrevBrush);
  ReleaseDC(Parent.Handle, FLineDC);
  if FBrush <> nil then
  begin
    FBrush.Free;
    FBrush := nil;
  end;
end;

procedure TCustomSplitterPanel.StopSizing;
begin
  if FSizing then
  begin
    FSizing := False;
    if FLineVisible then
      DrawLine;
    ReleaseLineDC;
    if Assigned(FActiveControl) then
    begin
      TWinControlAccess(FActiveControl).OnKeyDown := FOldKeyDown;
    end;
  end;
  if Assigned(FOnMoved) then
    FOnMoved(Self);
end;

function TCustomSplitterPanel.InSplitRect(X, Y: Integer): Boolean;
var
  Rect: TRect;
  P: TPoint;
begin
  GetSplitRect(Rect);
  P := Point(X, Y);
  Result := PtInRect(Rect, P);
end;

destructor TCustomSplitterPanel.Destroy;
begin
  if FBrush <> nil then
    FBrush.Free;
  inherited;
end;

end.
