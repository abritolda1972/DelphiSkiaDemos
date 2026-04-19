object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'QR Code Generator Skia4Delphi'
  ClientHeight = 780
  ClientWidth = 1320
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    1320
    780)
  TextHeight = 17
  object PaintBox: TSkPaintBox
    Left = 280
    Top = 8
    Width = 684
    Height = 684
    Anchors = [akLeft, akTop, akRight, akBottom]
  end
  object LblTexto: TLabel
    Left = 16
    Top = 16
    Width = 70
    Height = 17
    Caption = 'Texto / URL:'
  end
  object LblDados: TLabel
    Left = 16
    Top = 98
    Width = 106
    Height = 17
    Caption = 'Shape dos dados:'
  end
  object LblFinderExt: TLabel
    Left = 16
    Top = 154
    Width = 88
    Height = 17
    Caption = 'Finder exterior:'
  end
  object LblFinderInt: TLabel
    Left = 16
    Top = 210
    Width = 85
    Height = 17
    Caption = 'Finder interior:'
  end
  object LblModoDados: TLabel
    Left = 16
    Top = 266
    Width = 103
    Height = 17
    Caption = 'Modo cor dados:'
  end
  object LblModoFinder: TLabel
    Left = 16
    Top = 322
    Width = 100
    Height = 17
    Caption = 'Modo cor finder:'
  end
  object LblBg: TLabel
    Left = 16
    Top = 378
    Width = 81
    Height = 17
    Caption = 'Cor de fundo:'
  end
  object LblDadosCor1: TLabel
    Left = 16
    Top = 434
    Width = 75
    Height = 17
    Caption = 'Dados cor 1:'
  end
  object LblDadosCor2: TLabel
    Left = 144
    Top = 434
    Width = 75
    Height = 17
    Caption = 'Dados cor 2:'
  end
  object LblFinderCor1: TLabel
    Left = 16
    Top = 492
    Width = 73
    Height = 17
    Caption = 'Finder cor 1:'
  end
  object LblFinderCor2: TLabel
    Left = 144
    Top = 492
    Width = 73
    Height = 17
    Caption = 'Finder cor 2:'
  end
  object LblLogoInfo: TLabel
    Left = 16
    Top = 552
    Width = 43
    Height = 13
    Caption = 'Sem logo'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGrayText
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = [fsItalic]
    ParentFont = False
  end
  object LblLogoSize: TLabel
    Left = 16
    Top = 623
    Width = 116
    Height = 17
    Caption = 'Tamanho logo: 20%'
  end
  object Label1: TLabel
    Left = 16
    Top = 604
    Width = 230
    Height = 13
    Caption = '(Imagens quadradas, mesma altura e largura)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = 13
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 186
    Top = 626
    Width = 70
    Height = 13
    Caption = 'ECC m'#225'x. 30%'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = 13
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object EdtTexto: TMemo
    Left = 16
    Top = 34
    Width = 240
    Height = 52
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object CboDados: TComboBox
    Left = 16
    Top = 116
    Width = 240
    Height = 25
    Style = csDropDownList
    TabOrder = 1
  end
  object CboFinderExt: TComboBox
    Left = 16
    Top = 172
    Width = 240
    Height = 25
    Style = csDropDownList
    TabOrder = 2
  end
  object CboFinderInt: TComboBox
    Left = 16
    Top = 228
    Width = 240
    Height = 25
    Style = csDropDownList
    TabOrder = 3
  end
  object CboModoDados: TComboBox
    Left = 16
    Top = 284
    Width = 240
    Height = 25
    Style = csDropDownList
    TabOrder = 4
  end
  object CboModoFinder: TComboBox
    Left = 16
    Top = 340
    Width = 240
    Height = 25
    Style = csDropDownList
    TabOrder = 5
  end
  object PanelBgColor: TPanel
    Left = 16
    Top = 396
    Width = 240
    Height = 26
    Cursor = crHandPoint
    Caption = '(clique para escolher)'
    Color = clWhite
    ParentBackground = False
    TabOrder = 6
    OnClick = PanelColorClick
  end
  object PanelDataColor1: TPanel
    Left = 16
    Top = 452
    Width = 112
    Height = 26
    Cursor = crHandPoint
    Caption = 'Cor 1'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 7
    OnClick = PanelColorClick
  end
  object PanelDataColor2: TPanel
    Left = 144
    Top = 452
    Width = 112
    Height = 26
    Cursor = crHandPoint
    Caption = 'Cor 2'
    Color = clRed
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 8
    OnClick = PanelColorClick
  end
  object PanelFinderColor1: TPanel
    Left = 16
    Top = 510
    Width = 112
    Height = 26
    Cursor = crHandPoint
    Caption = 'Cor 1'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 9
    OnClick = PanelColorClick
  end
  object PanelFinderColor2: TPanel
    Left = 144
    Top = 510
    Width = 112
    Height = 26
    Cursor = crHandPoint
    Caption = 'Cor 2'
    Color = clBlue
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 10
    OnClick = PanelColorClick
  end
  object BtnLogo: TButton
    Left = 16
    Top = 570
    Width = 155
    Height = 28
    Caption = 'Escolher logo...'
    TabOrder = 11
    OnClick = BtnLogoClick
  end
  object BtnRemoverLogo: TButton
    Left = 178
    Top = 570
    Width = 78
    Height = 28
    Caption = 'Remover'
    TabOrder = 12
    OnClick = BtnRemoverLogoClick
  end
  object TrkLogoSize: TTrackBar
    Left = 16
    Top = 641
    Width = 240
    Height = 28
    Max = 35
    Min = 10
    Position = 20
    TabOrder = 13
    TickStyle = tsNone
    OnChange = TrkLogoSizeChange
  end
  object BtnGerar: TButton
    Left = 16
    Top = 681
    Width = 240
    Height = 34
    Caption = 'Gerar QR Code'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -14
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 14
    OnClick = BtnGerarClick
  end
  object BtnSalvar: TButton
    Left = 16
    Top = 723
    Width = 240
    Height = 34
    Caption = 'Guardar PNG (1200 px)'
    TabOrder = 15
    OnClick = BtnSalvarClick
  end
  object PnlBatch: TPanel
    Left = 984
    Top = 8
    Width = 328
    Height = 764
    Anchors = [akTop, akRight, akBottom]
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 16
    object LblBatchTitle: TLabel
      Left = 8
      Top = 8
      Width = 118
      Height = 20
      Caption = 'Gera'#231#227'o em Lote'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object LblBatchList: TLabel
      Left = 8
      Top = 38
      Width = 178
      Height = 17
      Caption = 'Lista de entradas (1 por linha):'
    end
    object LblOutputDir: TLabel
      Left = 8
      Top = 270
      Width = 115
      Height = 17
      Caption = 'Direct'#243'rio de sa'#237'da:'
    end
    object LblFilePrefix: TLabel
      Left = 8
      Top = 326
      Width = 110
      Height = 17
      Caption = 'Prefixo do ficheiro:'
    end
    object LblExportSize: TLabel
      Left = 160
      Top = 326
      Width = 112
      Height = 17
      Caption = 'Tamanho PNG (px):'
    end
    object LblProgressInfo: TLabel
      Left = 8
      Top = 498
      Width = 3
      Height = 15
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object LblBatchStatus: TLabel
      Left = 8
      Top = 520
      Width = 300
      Height = 17
      AutoSize = False
      Caption = 'Pronto'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsItalic]
      ParentFont = False
      WordWrap = True
    end
    object MemoLote: TMemo
      Left = 8
      Top = 58
      Width = 308
      Height = 200
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object EdtOutputDir: TEdit
      Left = 8
      Top = 290
      Width = 240
      Height = 25
      TabOrder = 1
    end
    object BtnEscolherDir: TButton
      Left = 254
      Top = 290
      Width = 62
      Height = 25
      Caption = '...'
      TabOrder = 2
      OnClick = BtnEscolherDirClick
    end
    object EdtFilePrefix: TEdit
      Left = 8
      Top = 346
      Width = 140
      Height = 25
      TabOrder = 3
      Text = 'qr_'
    end
    object EdtExportSize: TEdit
      Left = 160
      Top = 346
      Width = 80
      Height = 25
      TabOrder = 4
      Text = '1200'
    end
    object BtnGerarLote: TButton
      Left = 8
      Top = 386
      Width = 308
      Height = 36
      Caption = 'Gerar Lote'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -14
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 5
      OnClick = BtnGerarLoteClick
    end
    object BtnCancelarLote: TButton
      Left = 8
      Top = 430
      Width = 308
      Height = 28
      Caption = 'Cancelar'
      Enabled = False
      TabOrder = 6
      OnClick = BtnCancelarLoteClick
    end
    object ProgressLote: TProgressBar
      Left = 8
      Top = 470
      Width = 308
      Height = 20
      TabOrder = 7
    end
    object Button1: TButton
      Left = 240
      Top = 264
      Width = 75
      Height = 25
      Caption = 'Button1'
      TabOrder = 8
      OnClick = Button1Click
    end
  end
end
