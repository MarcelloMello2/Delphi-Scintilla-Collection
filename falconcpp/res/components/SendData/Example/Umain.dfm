object FrmMain1: TFrmMain1
  Left = 482
  Top = 353
  BorderStyle = bsToolWindow
  Caption = 'Aplica'#231#227'o 1'
  ClientHeight = 342
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lname: TLabel
    Left = 216
    Top = 16
    Width = 28
    Height = 13
    Caption = 'Nome'
  end
  object rgs: TRadioGroup
    Left = 8
    Top = 16
    Width = 193
    Height = 73
    Caption = 'Sexo'
    Items.Strings = (
      'Masculino'
      'Feminino')
    TabOrder = 0
  end
  object ednom: TEdit
    Left = 216
    Top = 33
    Width = 193
    Height = 21
    MaxLength = 256
    TabOrder = 1
  end
  object btsend: TButton
    Left = 272
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Enviar'
    TabOrder = 2
    OnClick = btsendClick
  end
  object Panel1: TPanel
    Left = 8
    Top = 104
    Width = 401
    Height = 233
    BevelInner = bvLowered
    BevelOuter = bvLowered
    Caption = 'Duplo click aqui'
    TabOrder = 3
    object Image1: TImage
      Left = 2
      Top = 2
      Width = 397
      Height = 229
      Align = alClient
      OnDblClick = Image1DblClick
    end
  end
  object SendData1: TSendData
    ClassNamed = 'TFrmMain2'
    ClassText = 'Aplica'#231#227'o 2'
    OnReceivedStream = SendData1ReceivedStream
    OnReceivedRecord = SendData1ReceivedRecord
    OnCopyData = SendData1CopyData
    Left = 232
    Top = 56
  end
  object XPManifest1: TXPManifest
    Left = 200
    Top = 56
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Left = 288
    Top = 184
  end
end
