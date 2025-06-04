program Project5;

uses
  Forms,
  Unit5 in 'Unit5.pas' {Form5},
  Accessor in '..\source\Accessor.pas',
  EdOptionsWin in '..\source\EdOptionsWin.pas' {EdOptionsWindow},
  sciAbbrevationManager in '..\source\sciAbbrevationManager.pas',
  sciAddLanguageFormUnit in '..\source\sciAddLanguageFormUnit.pas' {sciAddLanguageForm},
  SciAutoComplete in '..\source\SciAutoComplete.pas',
  SciCallTips in '..\source\SciCallTips.pas',
  SciConfirmReplaceDlg in '..\source\SciConfirmReplaceDlg.pas' {ConfirmReplaceDialog},
  SciControllerHandler in '..\source\SciControllerHandler.pas',
  SciDetectUtils in '..\source\SciDetectUtils.pas',
  SciDocuments in '..\source\SciDocuments.pas',
  SciKeyBindings in '..\source\SciKeyBindings.pas',
  SciKeyEditForm in '..\source\SciKeyEditForm.pas' {KeyEditForm},
  scilangfiller in '..\source\scilangfiller.pas',
  SciLexer in '..\source\SciLexer.pas',
  SciLexerAuto in '..\source\SciLexerAuto.pas',
  SciLexerMemo in '..\source\SciLexerMemo.pas',
  scilexermod in '..\source\scilexermod.pas',
  SciLexerOptionsDlg in '..\source\SciLexerOptionsDlg.pas',
  SciMacroRecording in '..\source\SciMacroRecording.pas',
  ScintillaLanguageManager in '..\source\ScintillaLanguageManager.pas',
  ScintillaSynLexers in '..\source\ScintillaSynLexers.pas',
  SciPropertyMgr in '..\source\SciPropertyMgr.pas',
  SciReplaceTextDlg in '..\source\SciReplaceTextDlg.pas' {TextReplaceDialog},
  SciResLang in '..\source\SciResLang.pas',
  SciResLangDcl in '..\source\SciResLangDcl.pas',
  SciSearchReplace in '..\source\SciSearchReplace.pas',
  SciSearchTextDlg in '..\source\SciSearchTextDlg.pas' {TextSearchDialog},
  SciSupport in '..\source\SciSupport.pas',
  sciUtils in '..\source\sciUtils.pas',
  SciWhatToFillUnit in '..\source\SciWhatToFillUnit.pas' {TSciWhatToFillForm},
  tcFontCombobox in '..\source\tcFontCombobox.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm5, Form5);
  Application.CreateForm(TEdOptionsWindow, EdOptionsWindow);
  Application.CreateForm(TsciAddLanguageForm, sciAddLanguageForm);
  Application.CreateForm(TConfirmReplaceDialog, ConfirmReplaceDialog);
  Application.CreateForm(TTSciWhatToFillForm, TSciWhatToFillForm);
  Application.Run;
end.
