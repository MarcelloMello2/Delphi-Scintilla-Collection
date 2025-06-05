unit ScintillaFacerGenerator.HFacer;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  ScintillaFacerGenerator.Face,
  ScintillaFacerGenerator.FileGenerator;

procedure RegenerateAll(const RootPath: string; ShowMaxID: Boolean);

implementation

function PrintHFile(F: TFace): TStringList;
var
  PreviousCategory: string;
  AnyProvisional: Boolean;
  I: Integer;
  Name: string;
  V: TFeature;
  FeatureDefineName: string;
begin
  Result := TStringList.Create;
  PreviousCategory := '';
  AnyProvisional := False;
  
  for I := 0 to F.Order.Count - 1 do
  begin
    Name := F.Order[I];
    if F.Features.TryGetValue(Name, V) then
    begin
      if V.Category <> 'Deprecated' then
      begin
        if (V.Category = 'Provisional') and (PreviousCategory <> 'Provisional') then
        begin
          Result.Add('#ifndef SCI_DISABLE_PROVISIONAL');
          AnyProvisional := True;
        end;
        
        PreviousCategory := V.Category;
        
        case V.FeatureType of
          ftFun, ftGet, ftSet:
          begin
            FeatureDefineName := 'SCI_' + UpperCase(Name);
            Result.Add('#define ' + FeatureDefineName + ' ' + V.Value);
          end;
          
          ftEvt:
          begin
            FeatureDefineName := 'SCN_' + UpperCase(Name);
            Result.Add('#define ' + FeatureDefineName + ' ' + V.Value);
          end;
          
          ftVal:
          begin
            Result.Add('#define ' + Name + ' ' + V.Value);
          end;
        end;
      end;
    end;
  end;
  
  if AnyProvisional then
    Result.Add('#endif');
end;

procedure RegenerateAll(const RootPath: string; ShowMaxID: Boolean);
var
  F: TFace;
  IncludePath: string;
  HFileContent: TStringList;
  ValueSet: TStringList;
  I, MaximumID, IntVal: Integer;
  ValuesUnused: TStringList;
  ValueToName: TDictionary<Integer, string>;
  V: TFeature;
  Name: string;
  ShowUnused: Boolean;
begin
  ShowUnused := False;
  IncludePath := IncludeTrailingPathDelimiter(RootPath) + 'include' + PathDelim;
  
  F := TFace.Create;
  HFileContent := nil;
  ValueSet := TStringList.Create;
  ValuesUnused := TStringList.Create;
  ValueToName := TDictionary<Integer, string>.Create;
  try
    F.ReadFromFile(IncludePath + 'Scintilla.iface');
    
    HFileContent := PrintHFile(F);
    ScintillaFacerGenerator.FileGenerator.Regenerate(IncludePath + 'Scintilla.h', '/* ', [HFileContent]);
    
    if ShowMaxID then
    begin
      // Coleta todos os valores numéricos < 3000
      for Name in F.Values.Keys do
      begin
        if F.Features.TryGetValue(Name, V) then
        begin
          if TryStrToInt(V.Value, IntVal) and (IntVal < 3000) then
            ValueSet.Add(IntToStr(IntVal));
        end;
      end;
      
      // Encontra o máximo
      MaximumID := 0;
      for I := 0 to ValueSet.Count - 1 do
      begin
        IntVal := StrToInt(ValueSet[I]);
        if IntVal > MaximumID then
          MaximumID := IntVal;
      end;
      
      WriteLn('Maximum ID is ' + IntToStr(MaximumID));
      
      if ShowUnused then
      begin
        // Constrói mapa de valor para nome
        for Name in F.Features.Keys do
        begin
          if F.Features.TryGetValue(Name, V) then
          begin
            if TryStrToInt(V.Value, IntVal) then
              ValueToName.AddOrSetValue(IntVal, Name);
          end;
        end;
        
        // Encontra valores não usados
        WriteLn('');
        WriteLn('Unused values');
        for I := 2001 to MaximumID do
        begin
          if ValueSet.IndexOf(IntToStr(I)) = -1 then
          begin
            Name := '';
            if ValueToName.TryGetValue(I - 1, Name) then
              WriteLn(IntToStr(I) + ' ' + Name)
            else
              WriteLn(IntToStr(I));
          end;
        end;
      end;
    end;
  finally
    F.Free;
    HFileContent.Free;
    ValueSet.Free;
    ValuesUnused.Free;
    ValueToName.Free;
  end;
end;

end.