// =============================================================================
//  uNodeEditor.pas  —  v3.0  (Plugin Edition)
//  Editor de Nós estilo n8n / Node-RED — com sistema de plugins
//
//  © 2024-2026 Alberto Brito. Todos os direitos reservados.
//
//  Contacto: [abritolda@gmail.com]
// =============================================================================
//  uExamplePlugins.pas  —  Exemplos de plugins personalizados
//
//  COMO USAR ESTE FICHEIRO:
//  ────────────────────────
//  1. Copie este ficheiro para o seu projecto
//  2. Renomeie a unit e as classes
//  3. Adicione a unit ao .dpr (uses) — o registo é automático (initialization)
//  4. Compile e os seus nós aparecem na paleta sob a categoria "Custom"
//
//  Não é necessário modificar nenhum ficheiro do editor!
// =============================================================================

unit uExamplePlugins;

interface

uses
  uNodePlugin;

// ── Exemplo 1: Formatar Data ──────────────────────────────────────────────────
type
  TFormatDateNode = class(TNodePlugin)
    function GetTitle      : string;      override;
    function GetColor      : Cardinal; override;
    function GetIconText   : string;      override;
    function GetDescription: string;      override;
    function GetPropertyDefs: TArray<TNodePropertyDef>; override;
    function Execute(const AInput: TNodeData): TNodeData; override;
  end;

// ── Exemplo 2: Gerar UUID ─────────────────────────────────────────────────────
  TGenerateUUIDNode = class(TNodePlugin)
    function GetTitle      : string;      override;
    function GetColor      : Cardinal; override;
    function GetIconText   : string;      override;
    function GetMinInputs  : Integer;     override;   // Não precisa de entrada
    function GetPropertyDefs: TArray<TNodePropertyDef>; override;
    function Execute(const AInput: TNodeData): TNodeData; override;
  end;

// ── Exemplo 3: Calcular Expressão ─────────────────────────────────────────────
  TCalcNode = class(TNodePlugin)
    function GetTitle      : string;      override;
    function GetColor      : Cardinal; override;
    function GetIconText   : string;      override;
    function GetPropertyDefs: TArray<TNodePropertyDef>; override;
    function Execute(const AInput: TNodeData): TNodeData; override;
  end;

// ── Exemplo 4: Filtrar Lista ──────────────────────────────────────────────────
  TFilterListNode = class(TNodePlugin)
    function GetTitle      : string;      override;
    function GetColor      : Cardinal; override;
    function GetIconText   : string;      override;
    function GetPropertyDefs: TArray<TNodePropertyDef>; override;
    function Execute(const AInput: TNodeData): TNodeData; override;
  end;

// ── Exemplo 5: Juntar Texto ───────────────────────────────────────────────────
  TConcatNode = class(TNodePlugin)
    function GetTitle      : string;      override;
    function GetColor      : Cardinal; override;
    function GetIconText   : string;      override;
    function GetPropertyDefs: TArray<TNodePropertyDef>; override;
    function Execute(const AInput: TNodeData): TNodeData; override;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.DateUtils;

// =============================================================================
//  TFormatDateNode — Formata uma data/hora
// =============================================================================
function TFormatDateNode.GetTitle      : string;      begin Result := 'Formatar Data'; end;
function TFormatDateNode.GetColor      : Cardinal; begin Result := $FF1565C0;       end;
function TFormatDateNode.GetIconText   : string;      begin Result := '📅';           end;
function TFormatDateNode.GetDescription: string;      begin Result := 'Converte timestamps e formata datas'; end;

function TFormatDateNode.GetPropertyDefs: TArray<TNodePropertyDef>;
begin
  Result := [
    NodeProp('InputField', 'Campo de entrada', npkString, 'timestamp', 'Nome do campo com a data'),
    NodeProp('Format',     'Formato saída',    npkSelect, 'dd/mm/yyyy hh:nn'),
    NodeProp('OutputField','Campo de saída',   npkString, 'data_formatada'),
    NodeProp('Locale',     'Locale',           npkSelect, 'pt-PT')
  ];
  Result[1].Options :=
    'dd/mm/yyyy hh:nn|yyyy-mm-dd|dd-mm-yyyy|dd mmm yyyy|' +
    'hh:nn:ss|dddd\, dd mmm yyyy';
  Result[3].Options := 'pt-PT|pt-BR|en-US|en-GB|es-ES|fr-FR';
end;

function TFormatDateNode.Execute(const AInput: TNodeData): TNodeData;
var
  Raw, Fmt, OutField: string;
  DT: TDateTime;
begin
  Result := NewNodeData;
  if Assigned(AInput.Values) then
    Result.Values.AddStrings(AInput.Values);

  Raw      := AInput.Values.Values[GetProp('InputField')];
  Fmt      := GetProp('Format');
  OutField := GetProp('OutputField');

  if TryStrToFloat(Raw, Double(DT)) then
    Result.Values.Values[OutField] := FormatDateTime(Fmt, DT)
  else if TryStrToDateTime(Raw, DT) then
    Result.Values.Values[OutField] := FormatDateTime(Fmt, DT)
  else
  begin
    Result.Values.Values[OutField] := FormatDateTime(Fmt, Now);
    Result.Values.Values['_date_warning'] := 'Campo não encontrado, usou data actual';
  end;
end;

// =============================================================================
//  TGenerateUUIDNode — Gera um identificador único
// =============================================================================
function TGenerateUUIDNode.GetTitle    : string;      begin Result := 'Gerar UUID';  end;
function TGenerateUUIDNode.GetColor    : Cardinal; begin Result := $FF4527A0;     end;
function TGenerateUUIDNode.GetIconText : string;      begin Result := '🔑';         end;
function TGenerateUUIDNode.GetMinInputs: Integer;     begin Result := 0;             end;

function TGenerateUUIDNode.GetPropertyDefs: TArray<TNodePropertyDef>;
begin
  Result := [
    NodeProp('OutputField', 'Campo de saída', npkString, 'uuid'),
    NodeProp('Format',      'Formato',        npkSelect, 'lowercase')
  ];
  Result[1].Options := 'lowercase|uppercase|no-hyphens';
end;

function TGenerateUUIDNode.Execute(const AInput: TNodeData): TNodeData;
var
  GUID: TGUID;
  S, Fmt, OutField: string;
begin
  Result := NewNodeData;
  if Assigned(AInput.Values) then
    Result.Values.AddStrings(AInput.Values);

  CreateGUID(GUID);
  S        := GUIDToString(GUID);
  Fmt      := GetProp('Format');
  OutField := GetProp('OutputField');

  if Fmt = 'uppercase' then
    S := UpperCase(S)
  else if Fmt = 'no-hyphens' then
    S := StringReplace(LowerCase(S), '-', '', [rfReplaceAll])
  else
    S := LowerCase(S);

  // Remove chaves { }
  S := StringReplace(S, '{', '', [rfReplaceAll]);
  S := StringReplace(S, '}', '', [rfReplaceAll]);

  Result.Values.Values[OutField] := S;
end;

// =============================================================================
//  TCalcNode — Avalia expressão matemática simples
// =============================================================================
function TCalcNode.GetTitle    : string;      begin Result := 'Calcular';   end;
function TCalcNode.GetColor    : Cardinal; begin Result := $FF00838F;    end;
function TCalcNode.GetIconText : string;      begin Result := '🔢';        end;

function TCalcNode.GetPropertyDefs: TArray<TNodePropertyDef>;
begin
  Result := [
    NodeProp('FieldA',      'Campo A',       npkString,  'valor_a', 'Nome do campo numérico'),
    NodeProp('Operation',   'Operação',      npkSelect,  '+'),
    NodeProp('FieldOrValue','Campo B ou valor', npkString,'0', 'Nome de campo ou número literal'),
    NodeProp('OutputField', 'Campo resultado',  npkString,'resultado'),
    NodeProp('Decimals',    'Casas decimais',   npkInteger,'2')
  ];
  Result[1].Options := '+|-|*|/|%|max|min|abs|round';
end;

function TCalcNode.Execute(const AInput: TNodeData): TNodeData;
var
  VA, VB: Double;
  Op, OutF: string;
  Decimals: Integer;
  Res: Double;
begin
  Result := NewNodeData;
  if Assigned(AInput.Values) then
    Result.Values.AddStrings(AInput.Values);

  VA       := StrToFloatDef(AInput.Values.Values[GetProp('FieldA')], 0);
  VB       := StrToFloatDef(AInput.Values.Values[GetProp('FieldOrValue')],
              StrToFloatDef(GetProp('FieldOrValue'), 0));
  Op       := GetProp('Operation');
  OutF     := GetProp('OutputField');
  Decimals := StrToIntDef(GetProp('Decimals'), 2);

  if Op = '+'   then Res := VA + VB
  else if Op = '-'   then Res := VA - VB
  else if Op = '*'   then Res := VA * VB
  else if Op = '/'   then begin
    if VB = 0 then begin Result.Error := 'Divisão por zero'; Result.Success := False; Exit; end;
    Res := VA / VB;
  end
  else if Op = '%'   then Res := VA - Trunc(VA / VB) * VB
  else if Op = 'max' then Res := Max(VA, VB)
  else if Op = 'min' then Res := Min(VA, VB)
  else if Op = 'abs' then Res := Abs(VA)
  else { round }          Res := SimpleRoundTo(VA, -Decimals);

  Result.Values.Values[OutF] :=
    FormatFloat('0.' + StringOfChar('0', Decimals), Res);
end;

// =============================================================================
//  TFilterListNode — Filtra itens de uma lista CSV
// =============================================================================
function TFilterListNode.GetTitle    : string;      begin Result := 'Filtrar Lista'; end;
function TFilterListNode.GetColor    : Cardinal; begin Result := $FF2E7D32;       end;
function TFilterListNode.GetIconText : string;      begin Result := '🔍';           end;

function TFilterListNode.GetPropertyDefs: TArray<TNodePropertyDef>;
begin
  Result := [
    NodeProp('InputField',  'Campo de entrada', npkString, 'items', 'Lista separada por vírgula'),
    NodeProp('Contains',    'Contém texto',     npkString, ''),
    NodeProp('CaseSens',    'Case sensitive',   npkBoolean,'False'),
    NodeProp('OutputField', 'Campo de saída',   npkString, 'filtered_items'),
    NodeProp('CountField',  'Campo de contagem',npkString, 'count')
  ];
end;

function TFilterListNode.Execute(const AInput: TNodeData): TNodeData;
var
  Raw, Filter, OutF, CntF: string;
  Items: TStringList;
  Filtered: TStringList;
  I: Integer;
  CS: Boolean;
  A, B: string;
begin
  Result := NewNodeData;
  if Assigned(AInput.Values) then
    Result.Values.AddStrings(AInput.Values);

  Raw    := AInput.Values.Values[GetProp('InputField')];
  Filter := GetProp('Contains');
  OutF   := GetProp('OutputField');
  CntF   := GetProp('CountField');
  CS     := LowerCase(GetProp('CaseSens')) = 'true';

  Items    := TStringList.Create;
  Filtered := TStringList.Create;
  try
    Items.CommaText := Raw;
    for I := 0 to Items.Count - 1 do
    begin
      A := Items[I];
      B := Filter;
      if not CS then begin A := LowerCase(A); B := LowerCase(B); end;
      if (Filter = '') or (Pos(B, A) > 0) then
        Filtered.Add(Items[I]);
    end;
    Result.Values.Values[OutF] := Filtered.CommaText;
    Result.Values.Values[CntF] := IntToStr(Filtered.Count);
  finally
    Items.Free;
    Filtered.Free;
  end;
end;

// =============================================================================
//  TConcatNode — Concatena campos de texto
// =============================================================================
function TConcatNode.GetTitle    : string;      begin Result := 'Concatenar'; end;
function TConcatNode.GetColor    : Cardinal; begin Result := $FFE65100;    end;
function TConcatNode.GetIconText : string;      begin Result := '🔗';        end;

function TConcatNode.GetPropertyDefs: TArray<TNodePropertyDef>;
begin
  Result := [
    NodeProp('Fields',      'Campos (separados por |)', npkString, 'nome|apelido',
             'Ex: campo1|campo2|campo3'),
    NodeProp('Separator',   'Separador',               npkString, ' '),
    NodeProp('OutputField', 'Campo de saída',           npkString, 'texto_completo'),
    NodeProp('Trim',        'Remover espaços',          npkBoolean,'True')
  ];
end;

function TConcatNode.Execute(const AInput: TNodeData): TNodeData;
var
  FieldList, Sep, OutF: string;
  Parts: TArray<string>;
  SB: TStringBuilder;
  I: Integer;
  Val: string;
  DoTrim: Boolean;
begin
  Result := NewNodeData;
  if Assigned(AInput.Values) then
    Result.Values.AddStrings(AInput.Values);

  FieldList := GetProp('Fields');
  Sep       := GetProp('Separator');
  OutF      := GetProp('OutputField');
  DoTrim    := LowerCase(GetProp('Trim')) = 'true';

  Parts  := FieldList.Split(['|']);
  SB     := TStringBuilder.Create;
  try
    for I := 0 to High(Parts) do
    begin
      Val := AInput.Values.Values[Trim(Parts[I])];
      if DoTrim then Val := Trim(Val);
      if I > 0 then SB.Append(Sep);
      SB.Append(Val);
    end;
    Result.Values.Values[OutF] := SB.ToString;
  finally
    SB.Free;
  end;
end;

// =============================================================================
//  Auto-registo
// =============================================================================
initialization
  with NodePluginRegistry do
  begin
    Register(TFormatDateNode);
    Register(TGenerateUUIDNode);
    Register(TCalcNode);
    Register(TFilterListNode);
    Register(TConcatNode);
  end;

end.