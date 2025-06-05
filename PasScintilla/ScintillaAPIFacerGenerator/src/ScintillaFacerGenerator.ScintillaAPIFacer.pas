unit ScintillaFacerGenerator.ScintillaAPIFacer;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  System.SysUtils,
  System.Classes,
  System.StrUtils,
  System.Generics.Collections,
  ScintillaFacerGenerator.Face,
  ScintillaFacerGenerator.FileGenerator,
  ScintillaFacerGenerator.HFacer;

procedure RegenerateAll(const RootPath: string);

implementation

const
  NamespacePrefix = 'Scintilla::';
  
type
  TTypeAliases = TDictionary<string, string>;
  
var
  TypeAliases: TTypeAliases;
  BasicTypes: TStringList;
  DeadValues: TStringList;

procedure InitializeGlobals;
begin
  TypeAliases := TTypeAliases.Create;
  TypeAliases.Add('cells', 'const char *');
  TypeAliases.Add('colour', 'Colour');
  TypeAliases.Add('colouralpha', 'ColourAlpha');
  TypeAliases.Add('findtext', 'void *');
  TypeAliases.Add('findtextfull', 'TextToFindFull *');
  TypeAliases.Add('formatrange', 'void *');
  TypeAliases.Add('formatrangefull', 'RangeToFormatFull *');
  TypeAliases.Add('int', 'int');
  TypeAliases.Add('keymod', 'int');
  TypeAliases.Add('line', 'Line');
  TypeAliases.Add('pointer', 'void *');
  TypeAliases.Add('position', 'Position');
  TypeAliases.Add('string', 'const char *');
  TypeAliases.Add('stringresult', 'char *');
  TypeAliases.Add('textrange', 'void *');
  TypeAliases.Add('textrangefull', 'TextRangeFull *');
  
  BasicTypes := TStringList.Create;
  BasicTypes.Add('bool');
  BasicTypes.Add('char *');
  BasicTypes.Add('Colour');
  BasicTypes.Add('ColourAlpha');
  BasicTypes.Add('const char *');
  BasicTypes.Add('int');
  BasicTypes.Add('intptr_t');
  BasicTypes.Add('Line');
  BasicTypes.Add('Position');
  BasicTypes.Add('void');
  BasicTypes.Add('void *');
  
  DeadValues := TStringList.Create;
  DeadValues.Add('INDIC_CONTAINER');
  DeadValues.Add('INDIC_IME');
  DeadValues.Add('INDIC_IME_MAX');
  DeadValues.Add('INDIC_MAX');
end;

procedure FinalizeGlobals;
begin
  TypeAliases.Free;
  BasicTypes.Free;
  DeadValues.Free;
end;

function ActualTypeName(const TypeName: string; const Identifier: string = ''): string;
begin
  if (TypeName = 'pointer') and 
     ((Identifier = 'doc') or (Identifier = 'DocPointer') or (Identifier = 'CreateDocument')) then
    Result := 'IDocumentEditable *'
  else if TypeAliases.ContainsKey(TypeName) then
    Result := TypeAliases[TypeName]
  else
    Result := TypeName;
end;

function IsEnumerationType(const S: string): Boolean;
begin
  if (S = 'Position') or (S = 'Line') or (S = 'Colour') or (S = 'ColourAlpha') then
    Exit(False);
  if EndsStr('*', S) then
    Exit(False);
  if S = '' then
    Exit(False);
  Result := CharInSet(S[1], ['A'..'Z']);
end;

function JoinTypeAndIdentifier(const TypeName, Identifier: string): string;
begin
  // Adiciona espaço para separar tipo de identificador, exceto se for ponteiro
  if EndsStr('*', TypeName) then
    Result := TypeName + Identifier
  else
    Result := TypeName + ' ' + Identifier;
end;

function HMessages(F: TFace): TStringList;
var
  I: Integer;
  Name: string;
  V: TFeature;
begin
  Result := TStringList.Create;
  Result.Add('enum class Message {');
  
  for I := 0 to F.Order.Count - 1 do
  begin
    Name := F.Order[I];
    if F.Features.TryGetValue(Name, V) then
    begin
      if V.Category <> 'Deprecated' then
      begin
        if V.FeatureType in [ftFun, ftGet, ftSet] then
          Result.Add(#9 + Name + ' = ' + V.Value + ',');
      end;
    end;
  end;
  
  Result.Add('};');
end;

function HEnumerations(F: TFace): TStringList;
var
  I, J: Integer;
  Name, ValueName: string;
  V, VEnum: TFeature;
  Prefixes: TStringList;
  PrefixMatched: string;
  ValueNameNoPrefix: string;
  PascalName: string;
begin
  Result := TStringList.Create;
  Prefixes := TStringList.Create;
  try
    for I := 0 to F.Order.Count - 1 do
    begin
      Name := F.Order[I];
      if F.Features.TryGetValue(Name, V) then
      begin
        if (V.Category <> 'Deprecated') and (V.FeatureType = ftEnu) and (Name <> 'Lexer') then
        begin
          Result.Add('');
          Result.Add('enum class ' + Name + ' {');
          
          Prefixes.Clear;
          Prefixes.Delimiter := ' ';
          Prefixes.StrictDelimiter := True;
          Prefixes.DelimitedText := V.Value;
          
          for J := 0 to F.Order.Count - 1 do
          begin
            ValueName := F.Order[J];
            PrefixMatched := '';
            
            for PrefixMatched in Prefixes do
            begin
              if StartsStr(PrefixMatched, ValueName) and 
                 (DeadValues.IndexOf(ValueName) = -1) then
              begin
                if F.Features.TryGetValue(ValueName, VEnum) then
                begin
                  ValueNameNoPrefix := '';
                  
                  if F.Aliases.ContainsKey(ValueName) then
                    ValueNameNoPrefix := F.Aliases[ValueName]
                  else
                  begin
                    ValueNameNoPrefix := Copy(ValueName, Length(PrefixMatched) + 1, MaxInt);
                    if ValueNameNoPrefix = '' then  // Removeu o nome todo
                      ValueNameNoPrefix := ValueName;
                    if StartsStr('SC_', ValueNameNoPrefix) then
                      ValueNameNoPrefix := Copy(ValueNameNoPrefix, 4, MaxInt);
                  end;
                  
                  PascalName := ScintillaFacerGenerator.Face.PascalCase(ValueNameNoPrefix);
                  Result.Add(#9 + PascalName + ' = ' + VEnum.Value + ',');
                end;
                Break;
              end;
            end;
          end;
          
          Result.Add('};');
        end;
      end;
    end;
    
    // Adiciona enumeração de notificações
    Result.Add('');
    Result.Add('enum class Notification {');
    
    for I := 0 to F.Order.Count - 1 do
    begin
      Name := F.Order[I];
      if F.Features.TryGetValue(Name, V) then
      begin
        if (V.Category <> 'Deprecated') and (V.FeatureType = ftEvt) then
          Result.Add(#9 + Name + ' = ' + V.Value + ',');
      end;
    end;
    
    Result.Add('};');
  finally
    Prefixes.Free;
  end;
end;

function HConstants(F: TFace): TStringList;
var
  AllEnumPrefixes: TStringList;
  I: Integer;
  Name: string;
  V: TFeature;
  HasPrefix: Boolean;
  Prefix: string;
  TypeName: string;
begin
  Result := TStringList.Create;
  AllEnumPrefixes := TStringList.Create;
  try
    // Constantes não em uma enumeração
    AllEnumPrefixes.Add('SCE_');  // Estilos lexicais
    AllEnumPrefixes.Add('SCI_');  // Alocação de número de mensagem
    AllEnumPrefixes.Add('SCEN_'); // Notificações enviadas com WM_COMMAND
    
    // Adiciona prefixos de todas as enumerações
    for Name in F.Features.Keys do
    begin
      if F.Features.TryGetValue(Name, V) then
      begin
        if (V.Category <> 'Deprecated') and (V.FeatureType = ftEnu) then
        begin
          AllEnumPrefixes.Delimiter := ' ';
          AllEnumPrefixes.StrictDelimiter := True;
          AllEnumPrefixes.DelimitedText := AllEnumPrefixes.DelimitedText + ' ' + V.Value;
        end;
      end;
    end;
    
    for I := 0 to F.Order.Count - 1 do
    begin
      Name := F.Order[I];
      if F.Features.TryGetValue(Name, V) then
      begin
        if (V.Category <> 'Deprecated') and (V.FeatureType = ftVal) then
        begin
          HasPrefix := False;
          
          for Prefix in AllEnumPrefixes do
          begin
            if StartsStr(Prefix, Name) then
            begin
              HasPrefix := True;
              Break;
            end;
          end;
          
          if not HasPrefix then
          begin
            if StartsStr('SC_', Name) then
              Name := Copy(Name, 4, MaxInt);
              
            TypeName := 'int';
            if Name = 'INVALID_POSITION' then
              TypeName := 'Position';

            Result.Add('constexpr ' + TypeName + ' ' + ScintillaFacerGenerator.Face.PascalCase(Name) + ' = ' + V.Value + ';');
          end;
        end;
      end;
    end;
  finally
    AllEnumPrefixes.Free;
  end;
end;

// Para Pascal, vamos gerar as declarações e implementações
function GeneratePascalDeclarations(F: TFace): TStringList;
var
  I: Integer;
  Name: string;
  V: TFeature;
  RetType: string;
  ParamStr: string;
begin
  Result := TStringList.Create;
  
  for I := 0 to F.Order.Count - 1 do
  begin
    Name := F.Order[I];
    if F.Features.TryGetValue(Name, V) then
    begin
      if (V.Category <> 'Deprecated') and (V.FeatureType in [ftFun, ftGet, ftSet]) then
      begin
        if (V.FeatureType = ftGet) and StartsStr('Get', Name) then
          Name := Copy(Name, 4, MaxInt);
          
        RetType := ActualTypeName(V.ReturnType, Name);

        // Converte tipos C++ para Pascal
        RetType := StringReplace(RetType, 'const char *', 'PAnsiChar', [rfReplaceAll]);
        RetType := StringReplace(RetType, 'char *', 'PAnsiChar', [rfReplaceAll]);
        RetType := StringReplace(RetType, 'void *', 'Pointer', [rfReplaceAll]);
        RetType := StringReplace(RetType, 'void', '', [rfReplaceAll]);
        RetType := StringReplace(RetType, 'bool', 'Boolean', [rfReplaceAll]);
        RetType := StringReplace(RetType, 'Colour', 'TColor', [rfReplaceAll]);
        RetType := StringReplace(RetType, 'ColourAlpha', 'TColorAlpha', [rfReplaceAll]);
        RetType := StringReplace(RetType, 'Position', 'Integer', [rfReplaceAll]);
        RetType := StringReplace(RetType, 'Line', 'Integer', [rfReplaceAll]);
        
        // Monta parâmetros
        ParamStr := '';
        if V.Param1.ParamType <> '' then
        begin
          if V.Param1.Name <> '' then
            ParamStr := V.Param1.Name + ': ' + ActualTypeName(V.Param1.ParamType, V.Param1.Name);
        end;
        
        if V.Param2.ParamType <> '' then
        begin
          if ParamStr <> '' then
            ParamStr := ParamStr + '; ';
          if V.Param2.Name <> '' then
            ParamStr := ParamStr + V.Param2.Name + ': ' + ActualTypeName(V.Param2.ParamType, V.Param2.Name);
        end;
        
        // Converte tipos nos parâmetros
        ParamStr := StringReplace(ParamStr, 'const char *', 'PAnsiChar', [rfReplaceAll]);
        ParamStr := StringReplace(ParamStr, 'char *', 'PAnsiChar', [rfReplaceAll]);
        ParamStr := StringReplace(ParamStr, 'void *', 'Pointer', [rfReplaceAll]);
        ParamStr := StringReplace(ParamStr, 'Position', 'Integer', [rfReplaceAll]);
        ParamStr := StringReplace(ParamStr, 'Line', 'Integer', [rfReplaceAll]);
        
        if RetType <> '' then
          Result.Add('    function ' + Name + '(' + ParamStr + '): ' + RetType + ';')
        else
          Result.Add('    procedure ' + Name + '(' + ParamStr + ');');
      end;
    end;
  end;
end;

procedure RegenerateAll(const RootPath: string);
var
  F: TFace;
  IncludePath: string;
  MessagesContent, EnumerationsContent, ConstantsContent: TStringList;
  PascalDeclarations: TStringList;
begin
  InitializeGlobals;
  try
    // Primeiro gera os headers C/C++
    ScintillaFacerGenerator.HFacer.RegenerateAll(RootPath, False);
    
    // Agora gera os arquivos da API
    F := TFace.Create;
    MessagesContent := nil;
    EnumerationsContent := nil;
    ConstantsContent := nil;
    PascalDeclarations := nil;
    try
      IncludePath := IncludeTrailingPathDelimiter(RootPath) + 'include' + PathDelim;
      F.ReadFromFile(IncludePath + 'Scintilla.iface');
      
      // Gera ScintillaMessages.h
      MessagesContent := HMessages(F);
      ScintillaFacerGenerator.FileGenerator.Regenerate(IncludePath + 'ScintillaMessages.h', '//', [MessagesContent]);
      
      // Gera ScintillaTypes.h
      EnumerationsContent := HEnumerations(F);
      ConstantsContent := HConstants(F);
      ScintillaFacerGenerator.FileGenerator.Regenerate(IncludePath + 'ScintillaTypes.h', '//',
        [EnumerationsContent, ConstantsContent]);

      // Para Pascal, vamos gerar um arquivo único com declarações
      PascalDeclarations := GeneratePascalDeclarations(F);
      
      WriteLn('Generated:');
      WriteLn('  - ScintillaMessages.h');
      WriteLn('  - ScintillaTypes.h');
      WriteLn('  - Ready to generate Pascal wrapper');
      
    finally
      F.Free;
      MessagesContent.Free;
      EnumerationsContent.Free;
      ConstantsContent.Free;
      PascalDeclarations.Free;
    end;
  finally
    FinalizeGlobals;
  end;
end;

end.