// =============================================================================
//  uNodeEditor.pas  —  v3.0  (Plugin Edition)
//  Editor de Nós estilo n8n / Node-RED — com sistema de plugins
//
//  © 2024-2026 Alberto Brito. Todos os direitos reservados.
//
//  Contacto: [abritolda@gmail.com]
// =============================================================================
//  uNodePlugin.pas  —  Interface de Plugin para o Node Editor
//  Versão: 1.0
//
//  Como criar um node personalizado:
//
//  1. Crie uma nova unit (ex: MyPlugin.pas)
//  2. Implemente TNodePluginBase (ou use TNodePlugin como atalho)
//  3. Registe no NodePluginRegistry durante a inicialização:
//       NodePluginRegistry.Register(TMyNode);
//
//  Exemplo mínimo:
//  ──────────────────────────────────────────────────────────────────
//  unit MyPlugin;
//  interface
//  uses uNodePlugin;
//  type
//    TMyNode = class(TNodePlugin)
//      function  GetTitle: string; override;
//      function  GetColor: Cardinal; override;
//      function  Execute(const AInput: TNodeData): TNodeData; override;
//    end;
//  implementation
//  uses FMX.Dialogs;
//  function TMyNode.GetTitle: string; begin Result := 'Meu Node'; end;
//  function TMyNode.GetColor: Cardinal; begin Result := $FF00BCD4; end;
//  function TMyNode.Execute(const AInput: TNodeData): TNodeData;
//  begin
//    Result := AInput;
//    Result.Values.Values['resultado'] := 'OK';
//  end;
//  initialization
//    NodePluginRegistry.Register(TMyNode);
//  end.
//  ──────────────────────────────────────────────────────────────────
// =============================================================================

unit uNodePlugin;

interface

uses
  System.SysUtils,
  System.Classes,
  System.UITypes,
  System.Generics.Collections;

// =============================================================================
//  TNodeData — estrutura de dados que circula entre nós
// =============================================================================
type
  TNodeData = record
    Values : TStringList;   // Pares chave=valor (não gere memória - ver helper)
    Error  : string;        // Descrição de erro (vazio = sem erro)
    Success: Boolean;       // False se o nó falhou
  end;

// Helpers para criar/libertar TNodeData de forma segura
function  NewNodeData: TNodeData;
procedure FreeNodeData(var D: TNodeData);

// =============================================================================
//  TNodePropertyDef — Definição de uma propriedade configurável do node
// =============================================================================
type
  TNodePropKind = (
    npkString,    // Campo de texto livre
    npkInteger,   // Número inteiro
    npkFloat,     // Número decimal
    npkBoolean,   // True/False
    npkPassword,  // Texto mascarado (não exibe o valor real no editor)
    npkSelect,    // Lista de opções (preencher Options)
    npkCode       // Bloco de código/script
  );

  TNodePropertyDef = record
    Name       : string;          // Identificador interno (sem espaços)
    Label_     : string;          // Texto visível no painel
    Kind       : TNodePropKind;   // Tipo de input
    Default    : string;          // Valor inicial
    Options    : string;          // Para npkSelect: opções separadas por |
    Hint       : string;          // Tooltip / descrição curta
    Required   : Boolean;         // Validação básica
  end;

// =============================================================================
//  TNodeCategoryDef — Categoria para agrupar nós na paleta
// =============================================================================
  TNodeCategory = (
    ncTrigger,     // Webhook, Timer, etc.
    ncHTTP,        // Requisições web
    ncTransform,   // Transformação de dados
    ncLogic,       // Condicionais e loops
    ncIO,          // Email, ficheiro, BD
    ncUtility,     // Delay, log, debug
    ncCustom       // Plugins do utilizador
  );

const
  NodeCategoryNames: array[TNodeCategory] of string = (
    'Trigger', 'HTTP', 'Transform', 'Logic', 'I/O', 'Utility', 'Custom'
  );

// =============================================================================
//  TNodePluginClass — metaclasse para registo no registry
// =============================================================================
type
  TNodePluginBase = class;
  TNodePluginClass = class of TNodePluginBase;

// =============================================================================
//  TNodePluginBase — classe base que todos os plugins herdam
// =============================================================================
  TNodePluginBase = class
  private
    FProps: TStringList;
  public
    // ── Metadados obrigatórios ─────────────────────────────────────────────
    function GetTitle   : string;       virtual; abstract;
    function GetColor   : Cardinal;  virtual; abstract;

    // ── Metadados opcionais ────────────────────────────────────────────────
    function GetCategory   : TNodeCategory;               virtual;
    function GetDescription: string;                      virtual;
    function GetIconText   : string;                      virtual;
    function GetMinInputs  : Integer;                     virtual;
    function GetMaxOutputs : Integer;                     virtual;

    // ── Propriedades configuráveis ────────────────────────────────────────
    function  GetPropertyDefs: TArray<TNodePropertyDef>;  virtual;
    function  GetProp(const AName: string): string;

    // ── Execução ──────────────────────────────────────────────────────────
    function Execute(const AInput: TNodeData): TNodeData; virtual; abstract;

    // ── Ciclo de vida ─────────────────────────────────────────────────────
    procedure Initialize; virtual;
    procedure Finalize;   virtual;

    // ── Props: TStringList com pares Nome=Valor ────────────────────────────
    property  Props: TStringList read FProps;

    constructor Create; virtual;
    destructor  Destroy; override;
  end;

// =============================================================================
//  TNodePlugin — alias conveniente com defaults úteis
// =============================================================================
  TNodePlugin = class(TNodePluginBase)
  public
    function GetCategory   : TNodeCategory; override;
    function GetDescription: string;        override;
    function GetIconText   : string;        override;
    function GetMinInputs  : Integer;       override;
    function GetMaxOutputs : Integer;       override;
  end;

// =============================================================================
//  TNodePluginRegistry — registo global de plugins
// =============================================================================
  TNodePluginRegistry = class
  private
    FClasses: TList<TNodePluginClass>;
    constructor CreateInternal;
  public
    destructor Destroy; override;

    // Regista uma classe de plugin
    procedure Register(AClass: TNodePluginClass);

    // Devolve a lista de classes registadas (só leitura)
    function Classes: TArray<TNodePluginClass>;

    // Cria instância por índice ou por título
    function CreateInstance(AIdx: Integer): TNodePluginBase; overload;
    function CreateInstance(const ATitle: string): TNodePluginBase; overload;

    // Número de plugins registados
    function Count: Integer;

    // Info rápida (sem criar instância)
    function TitleOf(AIdx: Integer): string;
    function ColorOf(AIdx: Integer): TAlphaColor;
    function CategoryOf(AIdx: Integer): TNodeCategory;
  end;

// =============================================================================
//  Instância global — aceder como NodePluginRegistry.Register(...)
// =============================================================================
function NodePluginRegistry: TNodePluginRegistry;

// =============================================================================
//  Helper para construir TNodePropertyDef de forma fluente
// =============================================================================
function NodeProp(const AName, ALabel: string;
                  AKind: TNodePropKind = npkString;
                  const ADefault: string = '';
                  const AHint: string = '';
                  ARequired: Boolean = False): TNodePropertyDef;

function NodePropSelect(const AName, ALabel, AOptions: string;
                        const ADefault: string = ''): TNodePropertyDef;

implementation

// =============================================================================
//  Singleton registry
// =============================================================================
var
  GRegistry: TNodePluginRegistry = nil;

function NodePluginRegistry: TNodePluginRegistry;
begin
  if GRegistry = nil then
    GRegistry := TNodePluginRegistry.CreateInternal;
  Result := GRegistry;
end;

// =============================================================================
//  TNodeData helpers
// =============================================================================
function NewNodeData: TNodeData;
begin
  Result.Values  := TStringList.Create;
  Result.Values.NameValueSeparator := '=';
  Result.Error   := '';
  Result.Success := True;
end;

procedure FreeNodeData(var D: TNodeData);
begin
  if Assigned(D.Values) then
    FreeAndNil(D.Values);
end;

// =============================================================================
//  TNodePluginBase
// =============================================================================
constructor TNodePluginBase.Create;
begin
  inherited;
  FProps := TStringList.Create;
  FProps.NameValueSeparator := '=';
end;

destructor TNodePluginBase.Destroy;
begin
  FProps.Free;
  inherited;
end;


function TNodePluginBase.GetProp(const AName: string): string;
var
  Defs: TArray<TNodePropertyDef>;
  D   : TNodePropertyDef;
begin
  Result := FProps.Values[AName];
  // Se não tiver valor, devolve o default da definição
  if Result = '' then
  begin
    Defs := GetPropertyDefs;
    for D in Defs do
      if D.Name = AName then
        Exit(D.Default);
  end;
end;

function TNodePluginBase.GetCategory   : TNodeCategory;  begin Result := ncCustom;  end;
function TNodePluginBase.GetDescription: string;          begin Result := '';         end;
function TNodePluginBase.GetIconText   : string;          begin Result := '⚙';       end;
function TNodePluginBase.GetMinInputs  : Integer;         begin Result := 1;          end;
function TNodePluginBase.GetMaxOutputs : Integer;         begin Result := 1;          end;

function TNodePluginBase.GetPropertyDefs: TArray<TNodePropertyDef>;
begin
  SetLength(Result, 0);
end;

procedure TNodePluginBase.Initialize; begin end;
procedure TNodePluginBase.Finalize;   begin end;

// =============================================================================
//  TNodePlugin defaults
// =============================================================================
function TNodePlugin.GetCategory   : TNodeCategory; begin Result := ncCustom;  end;
function TNodePlugin.GetDescription: string;         begin Result := '';         end;
function TNodePlugin.GetIconText   : string;         begin Result := '⚙';       end;
function TNodePlugin.GetMinInputs  : Integer;        begin Result := 1;          end;
function TNodePlugin.GetMaxOutputs : Integer;        begin Result := 1;          end;

// =============================================================================
//  TNodePluginRegistry
// =============================================================================
constructor TNodePluginRegistry.CreateInternal;
begin
  inherited Create;
  FClasses := TList<TNodePluginClass>.Create;
end;

destructor TNodePluginRegistry.Destroy;
begin
  FClasses.Free;
  inherited;
end;

procedure TNodePluginRegistry.Register(AClass: TNodePluginClass);
begin
  if not FClasses.Contains(AClass) then
    FClasses.Add(AClass);
end;

function TNodePluginRegistry.Classes: TArray<TNodePluginClass>;
begin
  Result := FClasses.ToArray;
end;

function TNodePluginRegistry.Count: Integer;
begin
  Result := FClasses.Count;
end;

function TNodePluginRegistry.CreateInstance(AIdx: Integer): TNodePluginBase;
begin
  if (AIdx < 0) or (AIdx >= FClasses.Count) then
    raise ERangeError.CreateFmt('Plugin index %d fora de range', [AIdx]);
  Result := FClasses[AIdx].Create;
  Result.Initialize;
end;

function TNodePluginRegistry.CreateInstance(const ATitle: string): TNodePluginBase;
var I: Integer;
    Inst: TNodePluginBase;
begin
  for I := 0 to FClasses.Count - 1 do
  begin
    Inst := FClasses[I].Create;
    if Inst.GetTitle = ATitle then
    begin
      Inst.Initialize;
      Result := Inst;
      Exit;
    end;
    Inst.Free;
  end;
  raise EArgumentException.CreateFmt('Plugin "%s" não encontrado', [ATitle]);
end;

function TNodePluginRegistry.TitleOf(AIdx: Integer): string;
var Inst: TNodePluginBase;
begin
  Inst := FClasses[AIdx].Create;
  try Result := Inst.GetTitle; finally Inst.Free; end;
end;

function TNodePluginRegistry.ColorOf(AIdx: Integer): TAlphaColor;
var Inst: TNodePluginBase;
begin
  Inst := FClasses[AIdx].Create;
  try Result := Inst.GetColor; finally Inst.Free; end;
end;

function TNodePluginRegistry.CategoryOf(AIdx: Integer): TNodeCategory;
var Inst: TNodePluginBase;
begin
  Inst := FClasses[AIdx].Create;
  try Result := Inst.GetCategory; finally Inst.Free; end;
end;

// =============================================================================
//  Helpers de construção de propriedades
// =============================================================================
function NodeProp(const AName, ALabel: string;
                  AKind: TNodePropKind;
                  const ADefault: string;
                  const AHint: string;
                  ARequired: Boolean): TNodePropertyDef;
begin
  Result.Name     := AName;
  Result.Label_   := ALabel;
  Result.Kind     := AKind;
  Result.Default  := ADefault;
  Result.Options  := '';
  Result.Hint     := AHint;
  Result.Required := ARequired;
end;

function NodePropSelect(const AName, ALabel, AOptions: string;
                        const ADefault: string): TNodePropertyDef;
begin
  Result         := NodeProp(AName, ALabel, npkSelect, ADefault);
  Result.Options := AOptions;
end;

initialization

finalization
  FreeAndNil(GRegistry);

end.
