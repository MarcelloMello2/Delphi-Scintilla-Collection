object Form4: TForm4
  Left = 549
  Top = 50
  Caption = 'Scintilla Delphi Code Gen'
  ClientHeight = 635
  ClientWidth = 863
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'OpenSymbol'
  Font.Style = []
  Position = poDesigned
  ShowHint = True
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  DesignSize = (
    863
    635)
  TextHeight = 16
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 863
    Height = 635
    ActivePage = TabSheet3
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 680
    ExplicitHeight = 563
    object TabSheet1: TTabSheet
      Caption = 'Scintilla.iface'
      object memIFace: TMemo
        Left = 0
        Top = 0
        Width = 855
        Height = 604
        Align = alClient
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'DScintillaTypes.pas'
      ImageIndex = 1
      object memTypes: TMemo
        Left = 0
        Top = 0
        Width = 855
        Height = 604
        Align = alClient
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'DScintilla.pas'
      ImageIndex = 2
      object memProps: TMemo
        Left = 0
        Top = 0
        Width = 855
        Height = 604
        Align = alClient
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Custom code'
      ImageIndex = 3
      DesignSize = (
        855
        604)
      object lblType: TLabel
        Left = 79
        Top = 8
        Width = 80
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'lblType'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label1: TLabel
        Left = 3
        Top = 60
        Width = 9
        Height = 16
        Caption = 'D'
      end
      object Label2: TLabel
        Left = 3
        Top = 87
        Width = 7
        Height = 16
        Caption = '1'
      end
      object Label3: TLabel
        Left = 3
        Top = 114
        Width = 7
        Height = 16
        Caption = '2'
      end
      object Label4: TLabel
        Left = 3
        Top = 141
        Width = 9
        Height = 16
        Caption = 'R'
      end
      object cb: TCheckListBox
        Left = 3
        Top = 238
        Width = 156
        Height = 365
        Anchors = [akLeft, akTop, akBottom]
        PopupMenu = PopupMenu1
        Style = lbOwnerDrawFixed
        TabOrder = 0
        OnClick = cbClick
        OnClickCheck = cbClickCheck
        OnDrawItem = cbDrawItem
        ExplicitHeight = 293
      end
      object memCode: TMemo
        Left = 165
        Top = 328
        Width = 687
        Height = 275
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = EASTEUROPE_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = [fsBold]
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 1
        WantTabs = True
        WordWrap = False
        OnChange = edtDefChange
        ExplicitWidth = 504
        ExplicitHeight = 203
      end
      object edtDef: TEdit
        Left = 165
        Top = 5
        Width = 687
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        OnChange = edtDefChange
        ExplicitWidth = 504
      end
      object chkShowHidden: TCheckBox
        Left = 3
        Top = 188
        Width = 156
        Height = 17
        Caption = 'Show hidden'
        Color = clBtnFace
        ParentColor = False
        TabOrder = 3
        OnClick = chkShowHiddenClick
      end
      object edtFilter: TEdit
        Left = 3
        Top = 211
        Width = 156
        Height = 24
        TabOrder = 4
        TextHint = 'Search'
        OnChange = edtFilterChange
      end
      object chkConfigUnicode: TCheckBox
        Left = 70
        Top = 34
        Width = 26
        Height = 17
        Hint = 'Unicode'
        Caption = '&U'
        TabOrder = 5
        OnClick = chkConfigClick
      end
      object chkConfigDisabled: TCheckBox
        Left = 36
        Top = 34
        Width = 28
        Height = 17
        Hint = 'Disabled'
        Caption = '&D'
        TabOrder = 6
        OnClick = chkConfigClick
      end
      object chkConfigHidden: TCheckBox
        Left = 3
        Top = 34
        Width = 27
        Height = 17
        Hint = 'Hidden'
        Caption = '&H'
        TabOrder = 7
        OnClick = chkConfigClick
      end
      object btnDel: TButton
        Left = 3
        Top = 3
        Width = 38
        Height = 25
        Caption = '&Delete'
        Enabled = False
        TabOrder = 8
        OnClick = btnDelClick
      end
      object btnSave: TButton
        Left = 47
        Top = 3
        Width = 34
        Height = 25
        Caption = '&Save'
        Enabled = False
        TabOrder = 9
        OnClick = btnSaveClick
      end
      object chkOnlyEnums: TCheckBox
        Left = 3
        Top = 165
        Width = 78
        Height = 17
        Caption = 'Only enums'
        TabOrder = 10
        OnClick = chkShowHiddenClick
      end
      object chkConfigEnumSet: TCheckBox
        Left = 102
        Top = 34
        Width = 27
        Height = 17
        Hint = 'EnumSet'
        Caption = '&E'
        TabOrder = 11
        OnClick = chkConfigClick
      end
      object cfgDefaultValue: TEdit
        Left = 16
        Top = 57
        Width = 143
        Height = 24
        Hint = 'DefaultValue'
        TabOrder = 12
        TextHint = 'DefaultValue'
        OnExit = cfgDefaultValueChange
      end
      object cfgPara1Enum: TComboBox
        Left = 16
        Top = 84
        Width = 143
        Height = 24
        Hint = 'Param1Enum'
        TabOrder = 13
        TextHint = 'Param1 Enum'
        OnExit = cfgPara1EnumExit
      end
      object cfgPara2Enum: TComboBox
        Left = 16
        Top = 111
        Width = 143
        Height = 24
        Hint = 'Param2Enum'
        TabOrder = 14
        TextHint = 'Param2 Enum'
        OnExit = cfgPara1EnumExit
      end
      object cfgReturnEnum: TComboBox
        Left = 16
        Top = 138
        Width = 143
        Height = 24
        Hint = 'ReturnEnum'
        TabOrder = 15
        TextHint = 'ReturnEnum'
        OnExit = cfgPara1EnumExit
      end
      object memDoc: TMemo
        Left = 165
        Top = 33
        Width = 687
        Height = 72
        Anchors = [akLeft, akTop, akRight]
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 16
        ExplicitWidth = 504
      end
      object memDef: TMemo
        Left = 165
        Top = 110
        Width = 687
        Height = 212
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = EASTEUROPE_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = [fsBold]
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 17
        ExplicitWidth = 504
      end
      object chkConfigForcePublicProp: TCheckBox
        Left = 132
        Top = 34
        Width = 27
        Height = 17
        Hint = 'ForcePublicProp'
        Caption = '&P'
        Font.Charset = EASTEUROPE_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Segoe Print'
        Font.Style = [fsBold, fsStrikeOut]
        ParentFont = False
        TabOrder = 18
        OnClick = chkConfigClick
      end
    end
  end
  object btnRegen: TButton
    Left = 780
    Top = 1
    Width = 75
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Regenerate'
    TabOrder = 1
    OnClick = btnRegenClick
    ExplicitLeft = 597
  end
  object tmrSearch: TTimer
    Enabled = False
    Interval = 320
    OnTimer = tmrSearchTimer
    Left = 344
    Top = 80
  end
  object PopupMenu1: TPopupMenu
    Left = 56
    Top = 368
    object ClearHideflagforallitems1: TMenuItem
      Caption = 'Clear Hide flag for all items'
      OnClick = ClearHideflagforallitems1Click
    end
  end
end
