program ScintillaIfaceGeneratorV1;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  SysUtils, Classes, StrUtils, System.RegularExpressions;

type
  TParamInfo = record
    ParamType: string;
    ParamName: string;
  end;

  TFunctionInfo = record
    FuncType: string;      // fun, get, set
    ReturnType: string;
    Name: string;
    MessageID: string;
    Params: TArray<TParamInfo>;
    Category: string;
  end;

  TConstantInfo = record
    Name: string;
    Value: string;
    EnumName: string;
  end;

  TEventInfo = record
    ReturnType: string;
    Name: string;
    EventID: string;
    Params: string;
  end;

  TScintillaGenerator = class
  private
    FFunctions: array of TFunctionInfo;
    FConstants: array of TConstantInfo;
    FEvents: array of TEventInfo;
    FCurrentCategory: string;
    FCurrentEnum: string;

    function ConvertType(const CppType: string): string;
    function ParseFunctionLine(const Line: string): Boolean;
    function ParseConstantLine(const Line: string): Boolean;
    function ParseEnumLine(const Line: string): Boolean;
    function ParseEventLine(const Line: string): Boolean;
    procedure ParseParameters(const ParamStr: string; var Params: TArray<TParamInfo>);
    function ParseFunctionLineSimple(const Line: string): Boolean;
    function ParseConstantLineSimple(const Line: string): Boolean;
    function ParseEnumLineSimple(const Line: string): Boolean;

    function GenerateConstants: string;
    function GenerateFunctionDeclarations: string;
    function GenerateFunctionImplementations: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ParseIfaceFile(const FileName: string);
    procedure GenerateDelphiUnit(const OutputFile: string);
  end;

constructor TScintillaGenerator.Create;
begin
  inherited;
  SetLength(FFunctions, 0);
  SetLength(FConstants, 0);
  SetLength(FEvents, 0);
  FCurrentCategory := '';
  FCurrentEnum := '';
end;

destructor TScintillaGenerator.Destroy;
begin
  SetLength(FFunctions, 0);
  SetLength(FConstants, 0);
  SetLength(FEvents, 0);
  inherited;
end;

function ExtractBetween(const S, StartStr, EndStr: string; var StartPos: Integer): string;
var
  P1, P2: Integer;
begin
  Result := '';
  P1 := PosEx(StartStr, S, StartPos);
  if P1 > 0 then
  begin
    P1 := P1 + Length(StartStr);
    P2 := PosEx(EndStr, S, P1);
    if P2 > 0 then
    begin
      Result := Copy(S, P1, P2 - P1);
      StartPos := P2 + Length(EndStr);
    end;
  end;
end;

function TScintillaGenerator.ParseFunctionLineSimple(const Line: string): Boolean;
var
  FuncInfo: TFunctionInfo;
  TempStr, TypeAndName: string;
  P, EqualPos, ParenPos: Integer;
  Parts: TStringList;
begin
  Result := False;

  // Verifica se é uma linha de função
  if not (StartsStr('fun ', Line) or StartsStr('get ', Line) or StartsStr('set ', Line)) then
    Exit;

  Parts := TStringList.Create;
  try
    Parts.Delimiter := ' ';
    Parts.DelimitedText := Line;

    if Parts.Count < 3 then Exit;

    FuncInfo.FuncType := Parts[0];  // fun, get ou set

    // Encontra o '=' para separar nome e ID
    EqualPos := Pos('=', Line);
    if EqualPos = 0 then Exit;

    // Extrai tipo de retorno e nome
    TempStr := Copy(Line, Length(Parts[0]) + 2, EqualPos - Length(Parts[0]) - 2);
    Parts.Clear;
    Parts.Delimiter := ' ';
    Parts.DelimitedText := Trim(TempStr);

    if Parts.Count >= 2 then
    begin
      FuncInfo.ReturnType := ConvertType(Parts[0]);
      FuncInfo.Name := Parts[1];
    end
    else
      Exit;

    // Extrai ID da mensagem
    ParenPos := Pos('(', Line);
    if ParenPos = 0 then Exit;

    FuncInfo.MessageID := Copy(Line, EqualPos + 1, ParenPos - EqualPos - 1);

    // Extrai parâmetros
    P := ParenPos + 1;
    TempStr := ExtractBetween(Line, '(', ')', P);
    ParseParameters(TempStr, FuncInfo.Params);

    FuncInfo.Category := FCurrentCategory;

    // Adiciona à lista
    SetLength(FFunctions, Length(FFunctions) + 1);
    FFunctions[High(FFunctions)] := FuncInfo;

    Result := True;
  finally
    Parts.Free;
  end;
end;

function TScintillaGenerator.ParseConstantLineSimple(const Line: string): Boolean;
var
  ConstInfo: TConstantInfo;
  EqualPos: Integer;
begin
  Result := False;

  if not StartsStr('val ', Line) then
    Exit;

  EqualPos := Pos('=', Line);
  if EqualPos = 0 then Exit;

  ConstInfo.Name := Trim(Copy(Line, 5, EqualPos - 5));
  ConstInfo.Value := Trim(Copy(Line, EqualPos + 1, Length(Line)));
  ConstInfo.EnumName := FCurrentEnum;

  SetLength(FConstants, Length(FConstants) + 1);
  FConstants[High(FConstants)] := ConstInfo;

  Result := True;
end;

function TScintillaGenerator.ParseEnumLineSimple(const Line: string): Boolean;
var
  EqualPos: Integer;
begin
  Result := False;

  if not StartsStr('enu ', Line) then
    Exit;

  EqualPos := Pos('=', Line);
  if EqualPos > 0 then
  begin
    FCurrentEnum := Trim(Copy(Line, 5, EqualPos - 5));
    Result := True;
  end;
end;

function TScintillaGenerator.ConvertType(const CppType: string): string;
begin
  if CppType = 'void' then Result := ''
  else if CppType = 'int' then Result := 'Integer'
  else if CppType = 'bool' then Result := 'Boolean'
  else if CppType = 'position' then Result := 'Integer'
  else if CppType = 'colour' then Result := 'TColor'
  else if CppType = 'string' then Result := 'PAnsiChar'
  else if CppType = 'stringresult' then Result := 'PAnsiChar'
  else if CppType = 'cells' then Result := 'PAnsiChar'
  else if CppType = 'textrange' then Result := 'PTextRange'
  else if CppType = 'findtext' then Result := 'PFindText'
  else if CppType = 'keymod' then Result := 'Integer'
  else if CppType = 'formatrange' then Result := 'PFormatRange'
  else Result := 'Integer'; // Padrão
end;

procedure TScintillaGenerator.ParseParameters(const ParamStr: string; var Params: TArray<TParamInfo>);
var
  ParamList: TStringList;
  I, SpacePos: Integer;
  Param, PType, PName: string;
begin
  ParamList := TStringList.Create;
  try
    ParamList.Delimiter := ',';
    ParamList.StrictDelimiter := True;
    ParamList.DelimitedText := ParamStr;

    SetLength(Params, ParamList.Count);

    for I := 0 to ParamList.Count - 1 do
    begin
      Param := Trim(ParamList[I]);
      if Param <> '' then
      begin
        SpacePos := LastDelimiter(' ', Param);
        if SpacePos > 0 then
        begin
          PType := Copy(Param, 1, SpacePos - 1);
          PName := Copy(Param, SpacePos + 1, Length(Param));
          Params[I].ParamType := ConvertType(Trim(PType));
          Params[I].ParamName := Trim(PName);
        end
        else
        begin
          Params[I].ParamType := ConvertType(Param);
          Params[I].ParamName := '';
        end;
      end;
    end;
  finally
    ParamList.Free;
  end;
end;

//function TScintillaGenerator.ParseFunctionLine(const Line: string): Boolean;
//var
//  RegEx: TRegEx;
//  FuncInfo: TFunctionInfo;
//  Index: Integer;
//begin
//  Result := False;
//  RegEx := TRegEx.Create('(fun|get|set)\s+(\w+)\s+(\w+)=(\d+)\((.*?)\)');
//  try
//    // Regex: (fun|get|set)\s+(\w+)\s+(\w+)=(\d+)\((.*?)\)
////    RegEx.Expression := '(fun|get|set)\s+(\w+)\s+(\w+)=(\d+)\((.*?)\)';
//
//    if RegEx.mExec(Line) then
//    begin
//      FuncInfo.FuncType := RegEx.Match[1];
//      FuncInfo.ReturnType := ConvertType(RegEx.Match[2]);
//      FuncInfo.Name := RegEx.Match[3];
//      FuncInfo.MessageID := RegEx.Match[4];
//      FuncInfo.Category := FCurrentCategory;
//
//      // Parse parameters
//      ParseParameters(RegEx.Match[5], FuncInfo.Params);
//
//      // Adiciona à lista
//      Index := Length(FFunctions);
//      SetLength(FFunctions, Index + 1);
//      FFunctions[Index] := FuncInfo;
//
//      Result := True;
//    end;
//  finally
//    RegEx.Free;
//  end;
//end;

//function TScintillaGenerator.ParseConstantLine(const Line: string): Boolean;
//var
//  RegEx: TRegExpr;
//  ConstInfo: TConstantInfo;
//  Index: Integer;
//begin
//  Result := False;
//  RegEx := TRegExpr.Create;
//  try
//    // Regex: val\s+(\w+)=(.+)
//    RegEx.Expression := 'val\s+(\w+)=(.+)';
//
//    if RegEx.Exec(Line) then
//    begin
//      ConstInfo.Name := RegEx.Match[1];
//      ConstInfo.Value := RegEx.Match[2];
//      ConstInfo.EnumName := FCurrentEnum;
//
//      Index := Length(FConstants);
//      SetLength(FConstants, Index + 1);
//      FConstants[Index] := ConstInfo;
//
//      Result := True;
//    end;
//  finally
//    RegEx.Free;
//  end;
//end;

//function TScintillaGenerator.ParseEnumLine(const Line: string): Boolean;
//var
//  RegEx: TRegExpr;
//begin
//  Result := False;
//  RegEx := TRegExpr.Create;
//  try
//    // Regex: enu\s+(\w+)=(\w+)
//    RegEx.Expression := 'enu\s+(\w+)=(\w+)';
//
//    if RegEx.Exec(Line) then
//    begin
//      FCurrentEnum := RegEx.Match[1];
//      Result := True;
//    end;
//  finally
//    RegEx.Free;
//  end;
//end;

function TScintillaGenerator.ParseEventLine(const Line: string): Boolean;
var
  RegEx: TRegExpr;
  EventInfo: TEventInfo;
  Index: Integer;
begin
  Result := False;
  RegEx := TRegExpr.Create;
  try
    // Regex: evt\s+(\w+)\s+(\w+)=(\d+)\((.*?)\)
    RegEx.Expression := 'evt\s+(\w+)\s+(\w+)=(\d+)\((.*?)\)';

    if RegEx.Exec(Line) then
    begin
      EventInfo.ReturnType := RegEx.Match[1];
      EventInfo.Name := RegEx.Match[2];
      EventInfo.EventID := RegEx.Match[3];
      EventInfo.Params := RegEx.Match[4];

      Index := Length(FEvents);
      SetLength(FEvents, Index + 1);
      FEvents[Index] := EventInfo;

      Result := True;
    end;
  finally
    RegEx.Free;
  end;
end;

procedure TScintillaGenerator.ParseIfaceFile(const FileName: string);
var
  FileLines: TStringList;
  I: Integer;
  Line: string;
begin
  FileLines := TStringList.Create;
  try
    FileLines.LoadFromFile(FileName);

    for I := 0 to FileLines.Count - 1 do
    begin
      Line := Trim(FileLines[I]);

      // Ignora linhas vazias e comentários
      if (Line = '') or (Line[1] = '#') then
        Continue;

      // Categoria
      if StartsStr('cat ', Line) then
      begin
        FCurrentCategory := Copy(Line, 5, Length(Line));
        Continue;
      end;

      // Tenta fazer parse de cada tipo
      if StartsStr('fun ', Line) or StartsStr('get ', Line) or StartsStr('set ', Line) then
        ParseFunctionLine(Line)
      else if StartsStr('val ', Line) then
        ParseConstantLine(Line)
      else if StartsStr('enu ', Line) then
        ParseEnumLine(Line)
      else if StartsStr('evt ', Line) then
        ParseEventLine(Line);
    end;

    WriteLn(Format('Parseado: %d funções, %d constantes, %d eventos',
      [Length(FFunctions), Length(FConstants), Length(FEvents)]));
  finally
    FileLines.Free;
  end;
end;

function TScintillaGenerator.GenerateConstants: string;
var
  SL: TStringList;
  I: Integer;
  LastEnum: string;
begin
  SL := TStringList.Create;
  try
    SL.Add('const');

    LastEnum := '';
    for I := 0 to Length(FConstants) - 1 do
    begin
      // Adiciona comentário para nova enumeração
      if (FConstants[I].EnumName <> '') and (FConstants[I].EnumName <> LastEnum) then
      begin
        LastEnum := FConstants[I].EnumName;
        SL.Add('');
        SL.Add('  // ' + LastEnum);
      end;

      SL.Add(Format('  %s = %s;', [FConstants[I].Name, FConstants[I].Value]));
    end;

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TScintillaGenerator.GenerateFunctionDeclarations: string;
var
  SL: TStringList;
  I, J: Integer;
  ParamStr: string;
  LastCategory: string;
begin
  SL := TStringList.Create;
  try
    LastCategory := '';

    for I := 0 to Length(FFunctions) - 1 do
    begin
      // Adiciona comentário de categoria
      if FFunctions[I].Category <> LastCategory then
      begin
        LastCategory := FFunctions[I].Category;
        SL.Add('');
        SL.Add('    // ' + LastCategory);
      end;

      // Monta string de parâmetros
      ParamStr := '';
      for J := 0 to Length(FFunctions[I].Params) - 1 do
      begin
        if FFunctions[I].Params[J].ParamName <> '' then
        begin
          if ParamStr <> '' then
            ParamStr := ParamStr + '; ';
          ParamStr := ParamStr + FFunctions[I].Params[J].ParamName + ': ' +
                      FFunctions[I].Params[J].ParamType;
        end;
      end;

      // Gera declaração
      if FFunctions[I].ReturnType <> '' then
        SL.Add(Format('    function %s(%s): %s;',
          [FFunctions[I].Name, ParamStr, FFunctions[I].ReturnType]))
      else
        SL.Add(Format('    procedure %s(%s);',
          [FFunctions[I].Name, ParamStr]));
    end;

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TScintillaGenerator.GenerateFunctionImplementations: string;
var
  SL: TStringList;
  I, J: Integer;
  ParamStr, WParam, LParam: string;
begin
  SL := TStringList.Create;
  try
    for I := 0 to Length(FFunctions) - 1 do
    begin
      // Monta string de parâmetros
      ParamStr := '';
      for J := 0 to Length(FFunctions[I].Params) - 1 do
      begin
        if FFunctions[I].Params[J].ParamName <> '' then
        begin
          if ParamStr <> '' then
            ParamStr := ParamStr + '; ';
          ParamStr := ParamStr + FFunctions[I].Params[J].ParamName + ': ' +
                      FFunctions[I].Params[J].ParamType;
        end;
      end;

      // Determina WParam e LParam
      WParam := '0';
      LParam := '0';

      if Length(FFunctions[I].Params) >= 1 then
        if FFunctions[I].Params[0].ParamName <> '' then
          WParam := FFunctions[I].Params[0].ParamName;

      if Length(FFunctions[I].Params) >= 2 then
        if FFunctions[I].Params[1].ParamName <> '' then
          LParam := 'LPARAM(' + FFunctions[I].Params[1].ParamName + ')';

      // Gera implementação
      SL.Add('');
      if FFunctions[I].ReturnType <> '' then
      begin
        SL.Add(Format('function TScintilla.%s(%s): %s;',
          [FFunctions[I].Name, ParamStr, FFunctions[I].ReturnType]));
        SL.Add('begin');
        SL.Add(Format('  Result := SendMessage(FHandle, %s, %s, %s);',
          [FFunctions[I].MessageID, WParam, LParam]));
        SL.Add('end;');
      end
      else
      begin
        SL.Add(Format('procedure TScintilla.%s(%s);',
          [FFunctions[I].Name, ParamStr]));
        SL.Add('begin');
        SL.Add(Format('  SendMessage(FHandle, %s, %s, %s);',
          [FFunctions[I].MessageID, WParam, LParam]));
        SL.Add('end;');
      end;
    end;

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

procedure TScintillaGenerator.GenerateDelphiUnit(const OutputFile: string);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add('unit ScintillaWrapper;');
    SL.Add('');
    SL.Add('interface');
    SL.Add('');
    SL.Add('uses');
    SL.Add('  Windows, Messages, SysUtils, Classes, Graphics, Controls;');
    SL.Add('');

    // Constantes
    SL.Add(GenerateConstants);
    SL.Add('');

    // Tipos
    SL.Add('type');
    SL.Add('  // Tipos auxiliares');
    SL.Add('  PTextRange = ^TTextRange;');
    SL.Add('  TTextRange = record');
    SL.Add('    chrg: TCharRange;');
    SL.Add('    lpstrText: PAnsiChar;');
    SL.Add('  end;');
    SL.Add('');
    SL.Add('  PFindText = ^TFindText;');
    SL.Add('  TFindText = record');
    SL.Add('    chrg: TCharRange;');
    SL.Add('    lpstrText: PAnsiChar;');
    SL.Add('  end;');
    SL.Add('');

    // Classe principal
    SL.Add('  TScintilla = class(TCustomControl)');
    SL.Add('  private');
    SL.Add('    FHandle: HWND;');
    SL.Add('  protected');
    SL.Add('    procedure CreateWnd; override;');
    SL.Add('    procedure DestroyWnd; override;');
    SL.Add('  public');
    SL.Add('    constructor Create(AOwner: TComponent); override;');
    SL.Add('    destructor Destroy; override;');

    // Declarações de funções
    SL.Add(GenerateFunctionDeclarations);

    SL.Add('  end;');
    SL.Add('');
    SL.Add('implementation');
    SL.Add('');

    // Implementações básicas
    SL.Add('const');
    SL.Add('  ScintillaClassName = ''Scintilla'';');
    SL.Add('  ScintillaDLL = ''SciLexer.dll'';');
    SL.Add('');

    SL.Add('constructor TScintilla.Create(AOwner: TComponent);');
    SL.Add('begin');
    SL.Add('  inherited Create(AOwner);');
    SL.Add('  Width := 300;');
    SL.Add('  Height := 200;');
    SL.Add('end;');
    SL.Add('');

    SL.Add('destructor TScintilla.Destroy;');
    SL.Add('begin');
    SL.Add('  inherited Destroy;');
    SL.Add('end;');
    SL.Add('');

    SL.Add('procedure TScintilla.CreateWnd;');
    SL.Add('begin');
    SL.Add('  LoadLibrary(ScintillaDLL);');
    SL.Add('  FHandle := CreateWindow(ScintillaClassName, nil,');
    SL.Add('    WS_CHILD or WS_VISIBLE or WS_TABSTOP or WS_CLIPCHILDREN,');
    SL.Add('    0, 0, Width, Height, Handle, 0, HInstance, nil);');
    SL.Add('end;');
    SL.Add('');

    SL.Add('procedure TScintilla.DestroyWnd;');
    SL.Add('begin');
    SL.Add('  if FHandle <> 0 then');
    SL.Add('  begin');
    SL.Add('    DestroyWindow(FHandle);');
    SL.Add('    FHandle := 0;');
    SL.Add('  end;');
    SL.Add('end;');

    // Implementações das funções
    SL.Add(GenerateFunctionImplementations);

    SL.Add('');
    SL.Add('end.');

    SL.SaveToFile(OutputFile);
    WriteLn('Arquivo ' + OutputFile + ' gerado com sucesso!');
  finally
    SL.Free;
  end;
end;

// Programa principal
var
  Generator: TScintillaGenerator;
  InputFile, OutputFile: string;
begin
  if ParamCount < 1 then
  begin
    WriteLn('Uso: ScintillaIfaceGenerator <arquivo.iface> [arquivo_saida.pas]');
    Exit;
  end;

  InputFile := ParamStr(1);
  if not FileExists(InputFile) then
  begin
    WriteLn('Erro: Arquivo não encontrado: ' + InputFile);
    Exit;
  end;

  if ParamCount >= 2 then
    OutputFile := ParamStr(2)
  else
    OutputFile := 'ScintillaWrapper.pas';

  Generator := TScintillaGenerator.Create;
  try
    WriteLn('Parseando arquivo: ' + InputFile);
    Generator.ParseIfaceFile(InputFile);

    WriteLn('Gerando unit Delphi...');
    Generator.GenerateDelphiUnit(OutputFile);

    WriteLn('Concluído!');
  finally
    Generator.Free;
  end;

  ReadLn;
end.
