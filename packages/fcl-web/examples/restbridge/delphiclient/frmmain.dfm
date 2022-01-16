object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'SQLDB Rest client demo'
  ClientHeight = 319
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    527
    319)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 158
    Height = 13
    Caption = 'SQLDBRest bridge resource URL:'
  end
  object Label2: TLabel
    Left = 16
    Top = 54
    Width = 48
    Height = 13
    Caption = 'Username'
  end
  object LEPassword: TLabel
    Left = 172
    Top = 54
    Width = 46
    Height = 13
    Caption = 'Password'
    FocusControl = EPassword
  end
  object DBNavigator1: TDBNavigator
    Left = 16
    Top = 81
    Width = 240
    Height = 25
    DataSource = DSRest
    TabOrder = 0
  end
  object DBGrid1: TDBGrid
    Left = 16
    Top = 112
    Width = 498
    Height = 199
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DSRest
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object EURL: TEdit
    Left = 16
    Top = 24
    Width = 417
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    Text = 'http://192.168.0.98:3000/projects/'
  end
  object BFetch: TButton
    Left = 439
    Top = 22
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Fetch data'
    TabOrder = 3
    OnClick = BFetchClick
  end
  object EUserName: TEdit
    Left = 70
    Top = 51
    Width = 96
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
    Text = 'Michael'
  end
  object EPassword: TEdit
    Left = 224
    Top = 51
    Width = 134
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    PasswordChar = '*'
    TabOrder = 5
    Text = 'secret'
  end
  object DSRest: TDataSource
    DataSet = CDSRest
    Left = 72
    Top = 128
  end
  object CDSRest: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 128
    Top = 128
  end
  object RestClient: TIdHTTP
    AllowCookies = True
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    Request.ContentLength = -1
    Request.ContentRangeEnd = -1
    Request.ContentRangeStart = -1
    Request.ContentRangeInstanceLength = -1
    Request.Accept = 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8'
    Request.BasicAuthentication = False
    Request.UserAgent = 'Mozilla/3.0 (compatible; Indy Library)'
    Request.Ranges.Units = 'bytes'
    Request.Ranges = <>
    HTTPOptions = [hoInProcessAuth, hoForceEncodeParams]
    Left = 200
    Top = 128
  end
end
