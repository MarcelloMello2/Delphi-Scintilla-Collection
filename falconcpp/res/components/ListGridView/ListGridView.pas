unit ListGridView;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Grids, Themes, Graphics;

type

  TPaintInfo = record
    MouseInCol: Integer; // the column that the mouse is in
    ColPressed: Boolean; // a column has been pressed
    ColPressedIdx: Integer; // idx of the pressed column
    ColSizing: Boolean; // currently sizing a column
    ColMoving: Boolean; // currently moving a column
  end;

  TEditType = (etString, etInteger, etCurrency);

  TEditColumnEvent = procedure(Sender: TObject; ACol, ARow: Integer;
    var CanEdit: Boolean; var EditType: TEditType) of Object;

  TListGridView = class(TStringGrid)
  private
    FPaintInfo: TPaintInfo;
    FCell: TGridCoord;
    FEditType: TEditType;
    FEditingCell: TGridCoord;
    FAllowTitleClick: Boolean;
    FOnEditColumn: TEditColumnEvent;
    function CentreV(ARect: TRect; ATextHeight: Integer): Integer;
    function CentreH(ARect: TRect; ATextWidth: Integer): Integer;
    function ValidCell(ACell: TGridCoord): Boolean;
  protected
    function BeginColumnDrag(var Origin: Integer; var Destination: Integer;
      const MousePt: TPoint): Boolean; override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure ColumnMoved(FromIndex: Integer; ToIndex: Integer); override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    function CanEditAcceptKey(Key: Char): Boolean; override;
    function CanEditShow: Boolean; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    property AllowTitleClick: Boolean read FAllowTitleClick write
      FAllowTitleClick default True;
    property OnEditColumn: TEditColumnEvent read FOnEditColumn write
      FOnEditColumn;
  end;

procedure Register;

implementation

uses Types;

procedure Register;
begin
  RegisterComponents('Win32', [TListGridView]);
end;

constructor TListGridView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPaintInfo.ColPressed := False;
  FPaintInfo.MouseInCol := -1;
  FPaintInfo.ColPressedIdx := -1;
  FPaintInfo.ColMoving := False;
  FPaintInfo.ColSizing := False;
  DefaultDrawing := False;
  FEditType := etString;
  Options := Options + [goColSizing, goEditing];
  FCell.X := -1;
  FCell.Y := -1;
  FEditingCell.X := -1;
  FEditingCell.Y := -1;
  FAllowTitleClick := True;
  RowCount := 5;
  FixedCols := 0;
  DefaultRowHeight := 15;
  RowHeights[0] := 19;
end;

function TListGridView.CanEditShow: Boolean;
var
  CanEdit: Boolean;
  EditType: TEditType;
begin
  CanEdit := inherited CanEditShow;
  if not CanEdit then
  begin
    Result := CanEdit;
    Exit;
  end;
  if Assigned(FOnEditColumn) then
  begin
    CanEdit := True;
    EditType := etString;
    FOnEditColumn(Self, FEditingCell.X, FEditingCell.Y, CanEdit, EditType);
    FEditType := EditType;
    Result := CanEdit;
  end
  else
    Result := ((FEditingCell.Y > 0) or (FEditingCell.Y = -1)) and CanEdit;
end;

function TListGridView.CanEditAcceptKey(Key: Char): Boolean;
var
  Str: String;
begin
  case FEditType of
    etCurrency:
    begin
      Str := Cells[FEditingCell.X, FEditingCell.Y];
      if (Pos(',', Str) > 0) and CharinSet(Key, [',', '.']) then
        Result := False
      else
        Result := CharinSet(Key, ['0'..'9', ',', '.']);
    end;
    etInteger: Result := CharinSet(Key, ['0'..'9']);
  else
    Result := True;
  end;
end;

function TListGridView.BeginColumnDrag(var Origin, Destination: Integer;
const MousePt: TPoint): Boolean;
begin
  result := inherited BeginColumnDrag(Origin, Destination, MousePt);
  FPaintInfo.ColMoving := result;
end;


procedure TListGridView.CMMouseEnter(var Message: TMessage);
var
  Cell: TGridCoord;
  lPt: TPoint;
begin
  lPt := Point(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  Cell := MouseCoord(lPt.X, lPt.Y);
  if (Cell.Y = 0) then
    InvalidateCell(Cell.X, Cell.Y);
end;

procedure TListGridView.CMMouseLeave(var Message: TMessage);
begin
  if ValidCell(FCell) and (not FPaintInfo.ColMoving) then
    InvalidateCell(FCell.X, FCell.Y);
  FCell.X := -1;
  FCell.Y := -1;
  FPaintInfo.MouseInCol := -1;
  FPaintInfo.ColPressedIdx := -1;
end;

procedure TListGridView.ColumnMoved(FromIndex, ToIndex: Integer);
begin
  inherited;
  FPaintInfo.ColMoving := False;
  Invalidate;
end;

procedure TListGridView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  lCell: TGridCoord;
begin
  if not (csDesigning in ComponentState) and (Button = mbLeft) then
  begin
    FPaintInfo.ColSizing := Sizing(X, Y);
    if not FPaintInfo.ColSizing then
    begin
      FPaintInfo.ColPressedIdx := -1;
      FPaintInfo.ColPressed := False;
      if FAllowTitleClick then
        FPaintInfo.MouseInCol := -1;
      lCell := MouseCoord(X,Y);
      if (Button = mbLeft) and (lCell.Y = 0) and (FAllowTitleClick) then
      begin
        FPaintInfo.ColPressed := True;
        if FPaintInfo.ColPressed then
        begin
          FPaintInfo.ColPressedIdx := lCell.X;
        end;
        if ValidCell(FCell) then
          InvalidateCell(FCell.X, FCell.Y);
        FCell := lCell;
      end;
    end;
  end;
  inherited;
end;

procedure TListGridView.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  lCell: TGridCoord;
  lMouseInCol: Integer;
begin
  if not (csDesigning in ComponentState) then
  begin
    if (not FPaintInfo.ColSizing) and (not FPaintInfo.ColMoving) then
    begin
      lCell := MouseCoord(X,Y);
      if (lCell.Y = 0) then
      begin
        lMouseInCol := lCell.X;
        if lMouseInCol <> FPaintInfo.MouseInCol then
        begin
          InvalidateCell(lCell.X, lCell.Y);
          if (FPaintInfo.MouseInCol > -1) then
            InvalidateCell(FPaintInfo.MouseInCol, lCell.Y);
          FPaintInfo.MouseInCol := lMouseInCol;
        end;
      end
      else
      begin
        FPaintInfo.MouseInCol := -1;
        if ValidCell(FCell) then
          InvalidateCell(FCell.X, FCell.Y);
      end;
      FCell := lCell;
    end;
  end;
  inherited;
end;

procedure TListGridView.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  FPaintInfo.ColSizing := False;
  FPaintInfo.ColMoving := False;
  FPaintInfo.ColPressedIdx := -1;
  Invalidate;
end;

function TListGridView.CentreV(ARect: TRect; ATextHeight: Integer): Integer;
var
  ATop: Integer;
begin
  ATop := ((ARect.Bottom - ARect.Top) div 2) - (ATextHeight div 2);
  if (ATop >= 2) then
    Result := ARect.Top + ATop
  else
    Result := ARect.Top + 2;
end;

function TListGridView.CentreH(ARect: TRect; ATextWidth: Integer): Integer;
var
  ALeft: Integer;
begin
  ALeft := ((ARect.Right - ARect.Left) div 2) - (ATextWidth div 2);
  if (ALeft >= 5) then
    Result := ARect.Left + ALeft
  else
    Result := ARect.Left + 5;
end;

function TListGridView.SelectCell(ACol, ARow: Longint): Boolean;
begin
  inherited SelectCell(ACol, ARow);
  Result := (ARow <> 0);
  FEditingCell.X := ACol;
  FEditingCell.Y := ARow;
end;

procedure TListGridView.DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState);
var
  Details: TThemedElementDetails;
  lCaptionRect: TRect;
  lCellRect: TRect;
  lStr: string;
  AColor, lColor: TColor;
begin
  lCellRect := ARect;
  if (ThemeServices.ThemesEnabled) and (ARow = 0) then
  begin
    AColor := Canvas.Brush.Color;
    Canvas.Brush.Color := clBtnFace;
    lCaptionRect := ARect;
    lCellRect.Right := lCellRect.Right + 1;
    lCellRect.Bottom := lCellRect.Bottom + 1;
    if (not FPaintInfo.ColPressed) or (FPaintInfo.ColPressedIdx <> ACol) then
    begin
      if (FPaintInfo.MouseInCol = -1) or (FPaintInfo.MouseInCol <> ACol) or 
         (csDesigning in ComponentState) then
          Details := ThemeServices.GetElementDetails(thHeaderItemNormal)
      else
      begin
        Canvas.Brush.Color := clWhite;
        Details := ThemeServices.GetElementDetails(thHeaderItemHot);
      end;
    end
    else if FAllowTitleClick then
    begin
      Canvas.Brush.Color := $00D5DEDD;
      Details := ThemeServices.GetElementDetails(thHeaderItemPressed);
      InflateRect(lCaptionRect, -1, 1);
    end
    else
    begin
      if FPaintInfo.MouseInCol = ACol then
      begin
        Canvas.Brush.Color := clWhite;
        Details := ThemeServices.GetElementDetails(thHeaderItemHot);
      end
      else
        Details := ThemeServices.GetElementDetails(thHeaderItemNormal);
    end;
    lColor := Canvas.Brush.Color;
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(lCellRect);
    Canvas.Brush.Color := lColor;
    ThemeServices.DrawElement(Canvas.Handle, Details, lCellRect);
    lStr :=  Cells[ACol, ARow];
    Canvas.TextOut(CentreH(lCaptionRect, Canvas.TextWidth(lStr)),
      CentreV(lCaptionRect, Canvas.TextHeight(lStr)), lStr);
    Canvas.Brush.Color := AColor;
  end
  else
  begin
    if (gdFocused in AState) then
    begin
      if (goDrawFocusSelected in Options) then
      begin
        Canvas.Brush.Color := clHighlight;
        Canvas.Font.Color := clWhite;
      end
      else
      begin
        Canvas.Brush.Color := clWhite;
        Canvas.Font.Color := clBlack;
      end;
      Canvas.FillRect(ARect);
      Canvas.TextOut(ARect.Left + 2,
        ARect.Bottom - Canvas.TextHeight(Cells[ACol, ARow]), Cells[ACol, ARow]);
      Canvas.DrawFocusRect(ARect);
    end
    else if (gdSelected in AState) then
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.Font.Color := clWhite;
      Canvas.FillRect(ARect);
      Canvas.TextOut(ARect.Left + 2,
        ARect.Bottom - Canvas.TextHeight(Cells[ACol, ARow]), Cells[ACol, ARow]);
    end
    else
    begin
      Canvas.Brush.Color := Color;
      Canvas.Font.Color := clBlack;
      Canvas.FillRect(ARect);
      Canvas.TextOut(ARect.Left + 2,
        ARect.Bottom - Canvas.TextHeight(Cells[ACol, ARow]), Cells[ACol, ARow]);
    end;
    
    Canvas.Pen.Color := clSilver;
    Canvas.MoveTo(ARect.Right, ARect.Top);
    Canvas.LineTo(ARect.Right, ARect.Bottom);
    Canvas.MoveTo(ARect.Left, ARect.Bottom);
    Canvas.LineTo(ARect.Right, ARect.Bottom);
    Canvas.Pen.Color := clBlack;
    Canvas.Font.Color := clBlack;
  end;
end;

function TListGridView.ValidCell(ACell: TGridCoord): Boolean;
begin
  result := (ACell.X <> -1) and (ACell.Y <> -1);
end;

end.
