unit Seven.Scintilla.Types;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

{$SCOPEDENUMS ON}

const
  /// <summary>
  ///   Recuperar um ponteiro para uma função que processa mensagens para este Scintilla.
  /// </summary>
  SCI_GETDIRECTFUNCTION = 2184;

  /// <summary>
  ///   Recupera um ponteiro para uma função que processa mensagens para esta Scintilla. Semelhante a SCI_GETDIRECTFUNCTION,
  ///   mas o status também é retornado ao chamador.
  /// </summary>
  SCI_GETDIRECTSTATUSFUNCTION = 2772;

  /// <summary>
  /// Recupera um valor de ponteiro para usar como primeiro argumento ao chamar
  /// a função retornada por GetDirectFunction.
  /// </summary>
  SCI_GETDIRECTPOINTER = 2185;


implementation

end.
