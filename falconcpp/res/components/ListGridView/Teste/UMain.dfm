object Form1: TForm1
  Left = 310
  Top = 311
  Width = 643
  Height = 407
  Caption = 'Caption'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ListView1: TListView
    Left = 376
    Top = 24
    Width = 250
    Height = 150
    Columns = <
      item
        Caption = 'Caption'
        Width = 60
      end
      item
      end
      item
      end>
    GridLines = True
    Items.Data = {
      5B0000000300000000000000FFFFFFFFFFFFFFFF000000000000000006666768
      68666800000000FFFFFFFFFFFFFFFF0000000000000000066867666867660000
      0000FFFFFFFFFFFFFFFF0000000000000000086766686867666867}
    TabOrder = 1
    ViewStyle = vsReport
  end
  object StringGrid1: TStringGrid
    Left = 72
    Top = 208
    Width = 481
    Height = 120
    DefaultRowHeight = 15
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    TabOrder = 2
  end
  object ListGridView1: TListGridView
    Left = 16
    Top = 32
    Width = 345
    Height = 153
    DefaultRowHeight = 15
    DefaultDrawing = False
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing]
    TabOrder = 0
    OnEditColumn = ListGridView1EditColumn
    RowHeights = (
      19
      15
      15
      15
      15)
  end
  object XPManifest1: TXPManifest
    Left = 464
    Top = 24
  end
end
