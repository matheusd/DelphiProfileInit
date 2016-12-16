object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 336
  ClientWidth = 633
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 64
    Top = 43
    Width = 75
    Height = 25
    Caption = 'Listar'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ListBox1: TListBox
    Left = 8
    Top = 112
    Width = 585
    Height = 216
    ItemHeight = 13
    TabOrder = 1
  end
  object Button2: TButton
    Left = 145
    Top = 43
    Width = 75
    Height = 25
    Caption = 'Alterar'
    TabOrder = 2
    OnClick = Button2Click
  end
  object edtDir: TEdit
    Left = 32
    Top = 16
    Width = 337
    Height = 21
    TabOrder = 3
    Text = 'c:\projetos\sistema'
  end
end
