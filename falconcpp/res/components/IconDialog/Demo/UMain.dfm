object Form1: TForm1
  Left = 573
  Top = 406
  Width = 209
  Height = 286
  Caption = 'Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 144
    Width = 44
    Height = 13
    Caption = 'FileName'
  end
  object Label2: TLabel
    Left = 16
    Top = 200
    Width = 49
    Height = 13
    Caption = 'Icon index'
  end
  object Panel1: TPanel
    Left = 76
    Top = 35
    Width = 50
    Height = 50
    AutoSize = True
    BevelOuter = bvLowered
    TabOrder = 0
    object Image1: TImage
      Left = 1
      Top = 1
      Width = 48
      Height = 48
    end
  end
  object Edit1: TEdit
    Left = 16
    Top = 160
    Width = 169
    Height = 21
    TabOrder = 1
  end
  object Edit2: TEdit
    Left = 16
    Top = 216
    Width = 57
    Height = 21
    TabOrder = 2
  end
  object Button1: TButton
    Left = 64
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Change...'
    TabOrder = 3
    OnClick = Button1Click
  end
  object IconDialog1: TIconDialog
    FileName = 
      'D:\Minhas imagens\Icones e temas\icones\Photoshop.exe_I03ea_0417' +
      '.ico'
    IconIndex = 0
    Left = 40
    Top = 32
  end
  object XPManifest1: TXPManifest
    Left = 144
    Top = 24
  end
end
