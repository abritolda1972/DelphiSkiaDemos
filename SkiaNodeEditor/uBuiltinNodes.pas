// =============================================================================
//  uNodeEditor.pas  —  v3.0  (Plugin Edition)
//  Editor de Nós estilo n8n / Node-RED — com sistema de plugins
//
//  © 2024-2026 Alberto Brito. Todos os direitos reservados.
//
//  Contacto: [abritolda@gmail.com]
// =============================================================================
//  uBuiltinNodes.pas  —  Nodes integrados implementados como plugins
//
//  Pode usar estes como exemplos para criar os seus próprios nós.
// =============================================================================

unit uBuiltinNodes;

interface

uses
  uNodePlugin;

// ── Triggers ──────────────────────────────────────────────────────────────────
type
  TWebhookNode = class(TNodePlugin)
    function GetTitle      : string;       override;
    function GetColor      : Cardinal;  override;
    function GetCategory   : TNodeCategory; override;
    function GetDescription: string;       override;
    function GetIconText   : string;       override;
    function GetMinInputs  : Integer;      override;
    function GetPropertyDefs: TArray<TNodePropertyDef>; override;
    function Execute(const AInput: TNodeData): TNodeData; override;
  end;

// ── HTTP ──────────────────────────────────────────────────────────────────────
  THTTPRequestNode = class(TNodePlugin)
    function GetTitle      : string;       override;
    function GetColor      : Cardinal;  override;
    function GetCategory   : TNodeCategory; override;
    function GetIconText   : string;       override;
    function GetPropertyDefs: TArray<TNodePropertyDef>; override;
    function Execute(const AInput: TNodeData): TNodeData; override;
  end;

// ── Transform ─────────────────────────────────────────────────────────────────
  TTransformNode = class(TNodePlugin)
    function GetTitle      : string;       override;
    function GetColor      : Cardinal;  override;
    function GetCategory   : TNodeCategory; override;
    function GetIconText   : string;       override;
    function GetPropertyDefs: TArray<TNodePropertyDef>; override;
    function Execute(const AInput: TNodeData): TNodeData; override;
  end;

// ── Logic ─────────────────────────────────────────────────────────────────────
  TConditionNode = class(TNodePlugin)
    function GetTitle      : string;       override;
    function GetColor      : Cardinal;  override;
    function GetCategory   : TNodeCategory; override;
    function GetIconText   : string;       override;
    function GetPropertyDefs: TArray<TNodePropertyDef>; override;
    function Execute(const AInput: TNodeData): TNodeData; override;
  end;

// ── I/O ───────────────────────────────────────────────────────────────────────
  TSendEmailNode = class(TNodePlugin)
    function GetTitle      : string;       override;
    function GetColor      : Cardinal;  override;
    function GetCategory   : TNodeCategory; override;
    function GetIconText   : string;       override;
    function GetPropertyDefs: TArray<TNodePropertyDef>; override;
    function Execute(const AInput: TNodeData): TNodeData; override;
  end;

  TSaveToDatabaseNode = class(TNodePlugin)
    function GetTitle      : string;       override;
    function GetColor      : Cardinal;  override;
    function GetCategory   : TNodeCategory; override;
    function GetIconText   : string;       override;
    function GetPropertyDefs: TArray<TNodePropertyDef>; override;
    function Execute(const AInput: TNodeData): TNodeData; override;
  end;

// ── Utility ───────────────────────────────────────────────────────────────────
  TDelayNode = class(TNodePlugin)
    function GetTitle      : string;       override;
    function GetColor      : Cardinal;  override;
    function GetCategory   : TNodeCategory; override;
    function GetIconText   : string;       override;
    function GetPropertyDefs: TArray<TNodePropertyDef>; override;
    function Execute(const AInput: TNodeData): TNodeData; override;
  end;

  TDebugNode = class(TNodePlugin)
    function GetTitle      : string;       override;
    function GetColor      : Cardinal;  override;
    function GetCategory   : TNodeCategory; override;
    function GetIconText   : string;       override;
    function GetPropertyDefs: TArray<TNodePropertyDef>; override;
    function Execute(const AInput: TNodeData): TNodeData; override;
  end;

// Regista todos os nós integrados
procedure RegisterBuiltinNodes;

implementation

uses
  System.SysUtils,
  System.Classes;

// =============================================================================
//  TWebhookNode
// =============================================================================
function TWebhookNode.GetTitle      : string;        begin Result := 'Webhook';    end;
function TWebhookNode.GetColor      : Cardinal;   begin Result := $FFDC143C;   end;
function TWebhookNode.GetCategory   : TNodeCategory; begin Result := ncTrigger;   end;
function TWebhookNode.GetDescription: string;        begin Result := 'Recebe chamadas HTTP externas'; end;
function TWebhookNode.GetIconText   : string;        begin Result := '⚡';        end;
function TWebhookNode.GetMinInputs  : Integer;       begin Result := 0;           end;

function TWebhookNode.GetPropertyDefs: TArray<TNodePropertyDef>;
begin
  Result := [
    NodeProp('Method',  'Método HTTP', npkSelect,  'POST', '', True),
    NodeProp('Path',    'Caminho',     npkString,  '/evento', 'Ex: /meu-webhook'),
    NodeProp('Auth',    'Autenticação',npkSelect,  'None', ''),
    NodeProp('Token',   'Token/Senha', npkPassword,'')
  ];
  Result[0].Options := 'GET|POST|PUT|PATCH|DELETE';
  Result[2].Options := 'None|Bearer Token|Basic Auth';
end;

function TWebhookNode.Execute(const AInput: TNodeData): TNodeData;
begin
  Result         := NewNodeData;
  Result.Values.Values['source']  := 'webhook';
  Result.Values.Values['method']  := GetProp('Method');
  Result.Values.Values['path']    := GetProp('Path');
end;

// =============================================================================
//  THTTPRequestNode
// =============================================================================
function THTTPRequestNode.GetTitle    : string;        begin Result := 'HTTP Request'; end;
function THTTPRequestNode.GetColor    : Cardinal;   begin Result := $FF00897B;      end;
function THTTPRequestNode.GetCategory : TNodeCategory; begin Result := ncHTTP;         end;
function THTTPRequestNode.GetIconText : string;        begin Result := '🌐';          end;

function THTTPRequestNode.GetPropertyDefs: TArray<TNodePropertyDef>;
begin
  Result := [
    NodeProp('URL',     'URL',          npkString,  'https://', 'URL completa', True),
    NodeProp('Method',  'Método',       npkSelect,  'GET'),
    NodeProp('Headers', 'Cabeçalhos',   npkCode,    '', 'JSON ex: {"Authorization":"Bearer ..."}'),
    NodeProp('Body',    'Corpo (JSON)', npkCode,    '', 'Apenas para POST/PUT/PATCH'),
    NodeProp('Timeout', 'Timeout (ms)', npkInteger, '5000')
  ];
  Result[1].Options := 'GET|POST|PUT|PATCH|DELETE';
end;

function THTTPRequestNode.Execute(const AInput: TNodeData): TNodeData;
begin
  Result := NewNodeData;
  // Implementação real usaria Indy/WinInet/etc.
  Result.Values.Values['status']   := '200';
  Result.Values.Values['url']      := GetProp('URL');
  Result.Values.Values['method']   := GetProp('Method');
  Result.Values.Values['response'] := '{"ok":true}';
end;

// =============================================================================
//  TTransformNode
// =============================================================================
function TTransformNode.GetTitle    : string;        begin Result := 'Transformar'; end;
function TTransformNode.GetColor    : Cardinal;   begin Result := $FF2979FF;     end;
function TTransformNode.GetCategory : TNodeCategory; begin Result := ncTransform;   end;
function TTransformNode.GetIconText : string;        begin Result := '🔄';         end;

function TTransformNode.GetPropertyDefs: TArray<TNodePropertyDef>;
begin
  Result := [
    NodeProp('Field',   'Campo de origem', npkString, 'body.data', 'Caminho dot-notation'),
    NodeProp('Format',  'Formato saída',   npkSelect, 'objeto'),
    NodeProp('Script',  'Script custom',   npkCode,   '', 'JS/Pascal-like expr opcional')
  ];
  Result[1].Options := 'objeto|array|string|número|booleano';
end;

function TTransformNode.Execute(const AInput: TNodeData): TNodeData;
var
  Field, Fmt: string;
begin
  Result          := NewNodeData;
  Field           := GetProp('Field');
  Fmt             := GetProp('Format');
  Result.Values.Values['field']  := Field;
  Result.Values.Values['format'] := Fmt;
  // Copia os valores de entrada, depois transforma
  if Assigned(AInput.Values) then
    Result.Values.AddStrings(AInput.Values);
end;

// =============================================================================
//  TConditionNode
// =============================================================================
function TConditionNode.GetTitle    : string;        begin Result := 'Condição IF'; end;
function TConditionNode.GetColor    : Cardinal;   begin Result := $FF00BFA5;     end;
function TConditionNode.GetCategory : TNodeCategory; begin Result := ncLogic;       end;
function TConditionNode.GetIconText : string;        begin Result := '🔀';         end;

function TConditionNode.GetPropertyDefs: TArray<TNodePropertyDef>;
begin
  Result := [
    NodeProp('LeftField',  'Campo esquerdo', npkString, 'valor', '', True),
    NodeProp('Operator',   'Operador',       npkSelect, '=='),
    NodeProp('RightValue', 'Valor direito',  npkString, '0')
  ];
  Result[1].Options := '==|!=|>|<|>=|<=|contains|starts_with';
end;

function TConditionNode.Execute(const AInput: TNodeData): TNodeData;
var
  LV, RV, Op: string;
  Cond: Boolean;
begin
  Result := NewNodeData;
  if Assigned(AInput.Values) then
    Result.Values.AddStrings(AInput.Values);

  LV   := AInput.Values.Values[GetProp('LeftField')];
  RV   := GetProp('RightValue');
  Op   := GetProp('Operator');

  if Op = '==' then      Cond := LV = RV
  else if Op = '!=' then Cond := LV <> RV
  else if Op = '>' then  Cond := StrToFloatDef(LV,0) > StrToFloatDef(RV,0)
  else if Op = '<' then  Cond := StrToFloatDef(LV,0) < StrToFloatDef(RV,0)
  else Cond := Pos(LowerCase(RV), LowerCase(LV)) > 0;

  Result.Values.Values['_condition'] := BoolToStr(Cond, True);
end;

// =============================================================================
//  TSendEmailNode
// =============================================================================
function TSendEmailNode.GetTitle    : string;        begin Result := 'Enviar E-mail'; end;
function TSendEmailNode.GetColor    : Cardinal;   begin Result := $FFFF6F00;       end;
function TSendEmailNode.GetCategory : TNodeCategory; begin Result := ncIO;            end;
function TSendEmailNode.GetIconText : string;        begin Result := '📧';           end;

function TSendEmailNode.GetPropertyDefs: TArray<TNodePropertyDef>;
begin
  Result := [
    NodeProp('To',       'Para',      npkString, '', 'user@exemplo.com', True),
    NodeProp('Subject',  'Assunto',   npkString, ''),
    NodeProp('Body',     'Corpo',     npkCode,   ''),
    NodeProp('SMTP',     'Servidor',  npkString, 'smtp.gmail.com'),
    NodeProp('Port',     'Porta',     npkInteger,'587'),
    NodeProp('User',     'Utilizador',npkString, ''),
    NodeProp('Password', 'Senha',     npkPassword,'')
  ];
end;

function TSendEmailNode.Execute(const AInput: TNodeData): TNodeData;
begin
  Result := NewNodeData;
  if Assigned(AInput.Values) then
    Result.Values.AddStrings(AInput.Values);
  // Implementação real usaria Indy TIdSMTP
  Result.Values.Values['email_sent'] := 'true';
  Result.Values.Values['to']         := GetProp('To');
end;

// =============================================================================
//  TSaveToDatabaseNode
// =============================================================================
function TSaveToDatabaseNode.GetTitle    : string;        begin Result := 'Salvar no BD';  end;
function TSaveToDatabaseNode.GetColor    : Cardinal;   begin Result := $FF7B1FA2;       end;
function TSaveToDatabaseNode.GetCategory : TNodeCategory; begin Result := ncIO;            end;
function TSaveToDatabaseNode.GetIconText : string;        begin Result := '🗄';           end;

function TSaveToDatabaseNode.GetPropertyDefs: TArray<TNodePropertyDef>;
begin
  Result := [
    NodeProp('Table',      'Tabela',     npkString, 'eventos', '', True),
    NodeProp('Connection', 'Ligação',    npkString, '', 'ConnectionString ou alias FireDAC'),
    NodeProp('OnDuplicate','Duplicados', npkSelect, 'Insert'),
    NodeProp('PrimaryKey', 'Chave prim.',npkString, 'id')
  ];
  Result[2].Options := 'Insert|Update|Upsert|Ignore';
end;

function TSaveToDatabaseNode.Execute(const AInput: TNodeData): TNodeData;
begin
  Result := NewNodeData;
  if Assigned(AInput.Values) then
    Result.Values.AddStrings(AInput.Values);
  Result.Values.Values['rows_affected'] := '1';
  Result.Values.Values['table']         := GetProp('Table');
end;

// =============================================================================
//  TDelayNode
// =============================================================================
function TDelayNode.GetTitle    : string;        begin Result := 'Delay';      end;
function TDelayNode.GetColor    : Cardinal;   begin Result := $FF5D4037;    end;
function TDelayNode.GetCategory : TNodeCategory; begin Result := ncUtility;    end;
function TDelayNode.GetIconText : string;        begin Result := '⏱';        end;

function TDelayNode.GetPropertyDefs: TArray<TNodePropertyDef>;
begin
  Result := [
    NodeProp('Amount', 'Duração',  npkInteger, '2000', 'Milissegundos'),
    NodeProp('Unit',   'Unidade',  npkSelect,  'ms')
  ];
  Result[1].Options := 'ms|s|min';
end;

function TDelayNode.Execute(const AInput: TNodeData): TNodeData;
var Ms: Integer;
begin
  Result := NewNodeData;
  if Assigned(AInput.Values) then
    Result.Values.AddStrings(AInput.Values);
  Ms := StrToIntDef(GetProp('Amount'), 2000);
  // Sleep(Ms);  // Descomente para execução real
  Result.Values.Values['_delayed_ms'] := IntToStr(Ms);
end;

// =============================================================================
//  TDebugNode
// =============================================================================
function TDebugNode.GetTitle    : string;        begin Result := 'Debug Log';   end;
function TDebugNode.GetColor    : Cardinal;   begin Result := $FF37474F;     end;
function TDebugNode.GetCategory : TNodeCategory; begin Result := ncUtility;     end;
function TDebugNode.GetIconText : string;        begin Result := '🐛';         end;

function TDebugNode.GetPropertyDefs: TArray<TNodePropertyDef>;
begin
  Result := [
    NodeProp('Label',    'Etiqueta',  npkString, 'DEBUG'),
    NodeProp('LogFile',  'Ficheiro',  npkString, '', 'Deixar vazio = só consola'),
    NodeProp('PrintAll', 'Mostrar tudo', npkBoolean, 'True')
  ];
end;

function TDebugNode.Execute(const AInput: TNodeData): TNodeData;
var
  I: Integer;
  Lbl: string;
begin
  Result := NewNodeData;
  if Assigned(AInput.Values) then
    Result.Values.AddStrings(AInput.Values);
  Lbl := GetProp('Label');
  // Log para consola/debug
  if IsConsole then
  begin
    WriteLn('[', Lbl, ']');
    if Assigned(AInput.Values) then
      for I := 0 to AInput.Values.Count - 1 do
        WriteLn('  ', AInput.Values[I]);
  end;
end;

// =============================================================================
//  Registo
// =============================================================================
procedure RegisterBuiltinNodes;
begin
  with NodePluginRegistry do
  begin
    Register(TWebhookNode);
    Register(THTTPRequestNode);
    Register(TTransformNode);
    Register(TConditionNode);
    Register(TSendEmailNode);
    Register(TSaveToDatabaseNode);
    Register(TDelayNode);
    Register(TDebugNode);
  end;
end;

initialization
  RegisterBuiltinNodes;

end.
