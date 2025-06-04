program Seven.Scintilla.BuildDScintilla;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  Classes,
  Windows,
  Seven.OSUtils in 'Seven.OSUtils.pas';

const
  SCINTILLA_REPO = 'https://github.com/ScintillaOrg/Scintilla';
  DSCINTILLA_VERSION = '5.0.0';

procedure DownloadScintillaIface;
begin
  WriteLn('Baixando Scintilla.iface mais recente...');
  // Aqui você pode usar Indy, Synapse ou chamar wget/curl
  // Por simplicidade, assumimos que o arquivo já existe
  if not FileExists('Scintilla.iface') then
  begin
    WriteLn('ERRO: Scintilla.iface não encontrado!');
    WriteLn('Baixe de: https://raw.githubusercontent.com/ScintillaOrg/Scintilla/master/include/Scintilla.iface');
    Halt(1);
  end;
end;

procedure BuildGenerator;
var
  ExitCode: Integer;
begin
  WriteLn('Compilando gerador...');

  // Compila com FreePascal
  ExitCode := ExecuteProcess('fpc', 'Seven.Scintilla.IfaceGeneratorApp.dpr -O3');

  if ExitCode <> 0 then
  begin
    WriteLn('ERRO: Falha ao compilar gerador!');
    Halt(1);
  end;

  WriteLn('Gerador compilado com sucesso!');
end;

procedure GenerateWrapper;
var
  ExitCode: Integer;
begin
  WriteLn('Gerando wrapper DScintilla...');

  // Cria diretório de saída
  if not DirectoryExists('Output') then
    CreateDir('Output');

  // Executa gerador
  ExitCode := ExecuteProcess('Seven.Scintilla.IfaceGeneratorApp.exe',
    'Scintilla.iface Output\DScintilla.pas');

  if ExitCode <> 0 then
  begin
    WriteLn('ERRO: Falha ao gerar wrapper!');
    Halt(1);
  end;

  WriteLn('Wrapper gerado com sucesso!');
end;

procedure GeneratePackage;
var
  SL: TStringList;
begin
  WriteLn('Gerando package Delphi...');

  SL := TStringList.Create;
  try
    SL.Add('package DScintillaPkg;');
    SL.Add('');
    SL.Add('{$R *.res}');
    SL.Add('{$ALIGN 8}');
    SL.Add('{$ASSERTIONS ON}');
    SL.Add('{$DESCRIPTION ''DScintilla - Delphi Wrapper for Scintilla''}');
    SL.Add('{$IMPLICITBUILD ON}');
    SL.Add('');
    SL.Add('requires');
    SL.Add('  rtl,');
    SL.Add('  vcl;');
    SL.Add('');
    SL.Add('contains');
    SL.Add('  DScintilla in ''DScintilla.pas'';');
    SL.Add('');
    SL.Add('end.');

    SL.SaveToFile('Output\DScintillaPkg.dpk');
  finally
    SL.Free;
  end;

  WriteLn('Package gerado!');
end;

procedure GenerateDemo;
var
  SL: TStringList;
begin
  WriteLn('Gerando aplicação demo...');

  SL := TStringList.Create;
  try
    // Unit principal do demo
    SL.Clear;
    SL.Add('unit DemoMain;');
    SL.Add('');
    SL.Add('interface');
    SL.Add('');
    SL.Add('uses');
    SL.Add('  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,');
    SL.Add('  Dialogs, StdCtrls, ComCtrls, Menus, DScintilla;');
    SL.Add('');
    SL.Add('type');
    SL.Add('  TFormDemo = class(TForm)');
    SL.Add('    MainMenu: TMainMenu;');
    SL.Add('    StatusBar: TStatusBar;');
    SL.Add('    MenuFile: TMenuItem;');
    SL.Add('    MenuOpen: TMenuItem;');
    SL.Add('    MenuSave: TMenuItem;');
    SL.Add('    procedure FormCreate(Sender: TObject);');
    SL.Add('    procedure MenuOpenClick(Sender: TObject);');
    SL.Add('    procedure ScintillaChange(Sender: TObject);');
    SL.Add('  private');
    SL.Add('    FScintilla: TScintilla;');
    SL.Add('    procedure SetupEditor;');
    SL.Add('  public');
    SL.Add('  end;');
    SL.Add('');
    SL.Add('var');
    SL.Add('  FormDemo: TFormDemo;');
    SL.Add('');
    SL.Add('implementation');
    SL.Add('');
    SL.Add('{$R *.dfm}');
    SL.Add('');
    SL.Add('procedure TFormDemo.FormCreate(Sender: TObject);');
    SL.Add('begin');
    SL.Add('  FScintilla := TScintilla.Create(Self);');
    SL.Add('  FScintilla.Parent := Self;');
    SL.Add('  FScintilla.Align := alClient;');
    SL.Add('  ');
    SL.Add('  SetupEditor;');
    SL.Add('end;');
    SL.Add('');
    SL.Add('procedure TFormDemo.SetupEditor;');
    SL.Add('begin');
    SL.Add('  // Configuração básica do editor');
    SL.Add('  with FScintilla do');
    SL.Add('  begin');
    SL.Add('    // Define lexer para Pascal');
    SL.Add('    SetLexer(SCLEX_PASCAL);');
    SL.Add('    ');
    SL.Add('    // Configuração de cores');
    SL.Add('    StyleSetFore(STYLE_DEFAULT, clBlack);');
    SL.Add('    StyleSetBack(STYLE_DEFAULT, clWhite);');
    SL.Add('    StyleSetSize(STYLE_DEFAULT, 10);');
    SL.Add('    StyleSetFont(STYLE_DEFAULT, ''Consolas'');');
    SL.Add('    StyleClearAll;');
    SL.Add('    ');
    SL.Add('    // Cores para Pascal');
    SL.Add('    StyleSetFore(SCE_PAS_COMMENT, clGreen);');
    SL.Add('    StyleSetFore(SCE_PAS_COMMENT2, clGreen);');
    SL.Add('    StyleSetFore(SCE_PAS_COMMENTLINE, clGreen);');
    SL.Add('    StyleSetFore(SCE_PAS_STRING, clMaroon);');
    SL.Add('    StyleSetFore(SCE_PAS_WORD, clNavy);');
    SL.Add('    StyleSetBold(SCE_PAS_WORD, True);');
    SL.Add('    ');
    SL.Add('    // Palavras-chave do Pascal');
    SL.Add('    SetKeyWords(0, ''begin end if then else while do for to '' +');
    SL.Add('      ''repeat until case of const var type array record '' +');
    SL.Add('      ''procedure function unit interface implementation'');');
    SL.Add('    ');
    SL.Add('    // Margens');
    SL.Add('    SetMarginTypeN(0, SC_MARGIN_NUMBER);');
    SL.Add('    SetMarginWidthN(0, 40);');
    SL.Add('    ');
    SL.Add('    // Outros');
    SL.Add('    SetTabWidth(2);');
    SL.Add('    SetUseTabs(False);');
    SL.Add('    SetCaretLineVisible(True);');
    SL.Add('    SetCaretLineBack($E8E8FF);');
    SL.Add('  end;');
    SL.Add('end;');
    SL.Add('');
    SL.Add('procedure TFormDemo.MenuOpenClick(Sender: TObject);');
    SL.Add('var');
    SL.Add('  OpenDialog: TOpenDialog;');
    SL.Add('begin');
    SL.Add('  OpenDialog := TOpenDialog.Create(nil);');
    SL.Add('  try');
    SL.Add('    OpenDialog.Filter := ''Pascal files|*.pas|All files|*.*'';');
    SL.Add('    if OpenDialog.Execute then');
    SL.Add('    begin');
    SL.Add('      FScintilla.LoadFromFile(OpenDialog.FileName);');
    SL.Add('      Caption := ''DScintilla Demo - '' + ExtractFileName(OpenDialog.FileName);');
    SL.Add('    end;');
    SL.Add('  finally');
    SL.Add('    OpenDialog.Free;');
    SL.Add('  end;');
    SL.Add('end;');
    SL.Add('');
    SL.Add('procedure TFormDemo.ScintillaChange(Sender: TObject);');
    SL.Add('begin');
    SL.Add('  StatusBar.Panels[0].Text := Format(''Linha: %d, Coluna: %d'',');
    SL.Add('    [FScintilla.GetCurrentLine + 1, FScintilla.GetColumn + 1]);');
    SL.Add('end;');
    SL.Add('');
    SL.Add('end.');

    SL.SaveToFile('Output\DemoMain.pas');

    // Projeto demo
    SL.Clear;
    SL.Add('program DScintillaDemo;');
    SL.Add('');
    SL.Add('uses');
    SL.Add('  Forms,');
    SL.Add('  DemoMain in ''DemoMain.pas'' {FormDemo},');
    SL.Add('  DScintilla in ''DScintilla.pas'';');
    SL.Add('');
    SL.Add('{$R *.res}');
    SL.Add('');
    SL.Add('begin');
    SL.Add('  Application.Initialize;');
    SL.Add('  Application.CreateForm(TFormDemo, FormDemo);');
    SL.Add('  Application.Run;');
    SL.Add('end.');

    SL.SaveToFile('Output\DScintillaDemo.dpr');
  finally
    SL.Free;
  end;

  WriteLn('Demo gerado!');
end;

procedure GenerateReadme;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add('# DScintilla ' + DSCINTILLA_VERSION);
    SL.Add('');
    SL.Add('Delphi/FreePascal wrapper for Scintilla editor component');
    SL.Add('');
    SL.Add('## Features');
    SL.Add('- Auto-generated from official Scintilla.iface');
    SL.Add('- Full API coverage');
    SL.Add('- Native Delphi properties');
    SL.Add('- Event support');
    SL.Add('- Compatible with Delphi 7+, FreePascal');
    SL.Add('');
    SL.Add('## Installation');
    SL.Add('1. Copy SciLexer.dll to your application directory');
    SL.Add('2. Install DScintillaPkg.dpk package');
    SL.Add('3. Add DScintilla to your uses clause');
    SL.Add('');
    SL.Add('## Usage');
    SL.Add('```pascal');
    SL.Add('var');
    SL.Add('  Editor: TScintilla;');
    SL.Add('begin');
    SL.Add('  Editor := TScintilla.Create(Self);');
    SL.Add('  Editor.Parent := Self;');
    SL.Add('  Editor.SetText(''Hello, Scintilla!'');');
    SL.Add('end;');
    SL.Add('```');
    SL.Add('');
    SL.Add('## Building from source');
    SL.Add('1. Download latest Scintilla.iface');
    SL.Add('2. Run: ScintillaIfaceGenerator Scintilla.iface');
    SL.Add('3. Compile generated files');

    SL.SaveToFile('Output\README.md');
  finally
    SL.Free;
  end;
end;

// Programa principal
begin
  WriteLn('DScintilla Build System');
  WriteLn('======================');
  WriteLn('');

  try
    // 1. Verifica/baixa Scintilla.iface
    DownloadScintillaIface;

    // 2. Compila o gerador
    BuildGenerator;

    // 3. Gera o wrapper
    GenerateWrapper;

    // 4. Gera package Delphi
    GeneratePackage;

    // 5. Gera aplicação demo
    GenerateDemo;

    // 6. Gera documentação
    GenerateReadme;

    WriteLn('');
    WriteLn('Build concluído com sucesso!');
    WriteLn('Arquivos gerados em: Output\');
    WriteLn('');
    WriteLn('Próximos passos:');
    WriteLn('1. Copie SciLexer.dll para Output\');
    WriteLn('2. Abra DScintillaPkg.dpk no Delphi');
    WriteLn('3. Compile e instale o package');
    WriteLn('4. Teste com DScintillaDemo.dpr');
  except
    on E: Exception do
    begin
      WriteLn('ERRO: ', E.Message);
      ExitCode := 1;
    end;
  end;

  WriteLn('');
  WriteLn('Pressione ENTER para sair...');
  ReadLn;
end.
