object FrmMain: TFrmMain
  Left = 378
  Top = 209
  Width = 689
  Height = 508
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Output Console'
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
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 44
    Height = 13
    Caption = 'FileName'
  end
  object Label2: TLabel
    Left = 8
    Top = 64
    Width = 35
    Height = 13
    Caption = 'Params'
  end
  object Label3: TLabel
    Left = 8
    Top = 112
    Width = 22
    Height = 13
    Caption = 'Path'
  end
  object MemoOut: TMemo
    Left = 8
    Top = 200
    Width = 665
    Height = 265
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clSilver
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    PopupMenu = PopupMenu1
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object EditFileName: TEdit
    Left = 8
    Top = 32
    Width = 665
    Height = 21
    TabOrder = 1
    Text = 'cmd /?'
  end
  object EditParams: TEdit
    Left = 7
    Top = 80
    Width = 666
    Height = 21
    TabOrder = 2
  end
  object BtnStart: TButton
    Left = 8
    Top = 160
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 3
    OnClick = BtnStartClick
  end
  object BtnStop: TButton
    Left = 96
    Top = 160
    Width = 75
    Height = 25
    Caption = 'Stop'
    Enabled = False
    TabOrder = 4
    OnClick = BtnStopClick
  end
  object EditPath: TEdit
    Left = 7
    Top = 128
    Width = 666
    Height = 21
    TabOrder = 5
  end
  object XPManifest1: TXPManifest
    Left = 296
    Top = 104
  end
  object PopupMenu1: TPopupMenu
    Left = 112
    Top = 216
    object Clear1: TMenuItem
      Caption = 'Clear'
      OnClick = Clear1Click
    end
  end
  object OutputConsole: TOutputConsole
    OnStart = OutputConsoleStart
    OnProgress = OutputConsoleProgress
    OnFinish = OutputConsoleFinish
    Left = 248
    Top = 160
  end
end
