// =============================================================================
//  MeuPlugin.pas  —  Template para criar um plugin personalizado
//
//  INSTRUÇÕES:
//  ──────────────────────────────────────────────────────────────────────────
//  1. Copie este ficheiro para o seu projecto
//  2. Renomeie:
//       - A unit:   "MeuPlugin" → o nome que quiser (ex: "uSlackPlugin")
//       - A classe: "TMyCustomNode" → o nome do seu nó (ex: "TSlackNode")
//  3. Implemente os métodos marcados com { TODO }
//  4. Adicione a unit ao .dpr:
//       uses
//         uNodePlugin,
//         uNodeEditor,
//         uSlackPlugin;   ← aqui
//  5. Compile — o nó aparece automaticamente na paleta!
//
//  DICAS:
//  • Para nós "trigger" (sem entrada): GetMinInputs → 0
//  • Para múltiplas saídas:           GetMaxOutputs → N
//  • Para erros:  Result.Success := False; Result.Error := 'mensagem';
//  • Para ler configuração: GetProp('NomeDoCampo')
//  • Para ler dados de entrada: AInput.Values.Values['chave']
//  • Para escrever dados de saída: Result.Values.Values['chave'] := valor;
// =============================================================================

unit MeuPlugin;

interface

uses
  uNodePlugin;

type
  TMyCustomNode = class(TNodePlugin)

    // ── Obrigatório ────────────────────────────────────────────────────────
    function GetTitle: string; override;
    function GetColor: TAlphaColor; override;

    // ── Recomendado ────────────────────────────────────────────────────────
    function GetCategory   : TNodeCategory; override;
    function GetDescription: string; override;
    function GetIconText   : string; override;

    // ── Propriedades do nó (campos configuráveis) ─────────────────────────
    function GetPropertyDefs: TArray<TNodePropertyDef>; override;

    // ── Lógica principal ───────────────────────────────────────────────────
    function Execute(const AInput: TNodeData): TNodeData; override;

    // ── Opcional: ciclo de vida ────────────────────────────────────────────
    // procedure Initialize; override;  // Chamado quando o nó é criado
    // procedure Finalize;   override;  // Chamado quando o nó é apagado
  end;

implementation

uses
  System.SysUtils,
  System.Classes;

// =============================================================================
//  Metadados
// =============================================================================

function TMyCustomNode.GetTitle: string;
begin
  Result := 'Meu Nó';  { TODO: Nome que aparece no nó e na paleta }
end;

function TMyCustomNode.GetColor: TAlphaColor;
begin
  Result := $FF00BCD4;  { TODO: Cor do cabeçalho (ARGB hex) }
  // Sugestões de cores:
  //   Vermelho:    $FFDC143C    Azul:        $FF2979FF
  //   Verde:       $FF2E7D32    Roxo:        $FF7B1FA2
  //   Laranja:     $FFFF6F00    Cyan:        $FF00BFA5
  //   Castanho:    $FF5D4037    Cinzento:    $FF546E7A
end;

function TMyCustomNode.GetCategory: TNodeCategory;
begin
  Result := ncCustom;   { TODO: ncTrigger / ncHTTP / ncTransform /
                                 ncLogic / ncIO / ncUtility / ncCustom }
end;

function TMyCustomNode.GetDescription: string;
begin
  Result := 'Breve descrição do que este nó faz';  { TODO }
end;

function TMyCustomNode.GetIconText: string;
begin
  Result := '⚙';  { TODO: emoji ou símbolo (ex: '📨', '🔄', '🗄', '⚡') }
end;

// =============================================================================
//  Propriedades configuráveis
//
//  Cada NodeProp define um campo no painel lateral:
//    NodeProp(Nome, Label, Tipo, Default, Dica, Obrigatório)
//
//  Tipos disponíveis:
//    npkString   — texto livre
//    npkInteger  — número inteiro
//    npkFloat    — número decimal
//    npkBoolean  — true / false
//    npkPassword — texto mascarado
//    npkSelect   — lista de opções (preencher .Options com 'A|B|C')
//    npkCode     — bloco de código/script
// =============================================================================
function TMyCustomNode.GetPropertyDefs: TArray<TNodePropertyDef>;
begin
  Result := [
    // Exemplo: campo de texto obrigatório
    NodeProp('Endpoint', 'URL do serviço', npkString, 'https://api.exemplo.com',
             'Endereço completo do serviço', {Required=} True),

    // Exemplo: lista de opções
    NodePropSelect('Method', 'Método HTTP', 'GET|POST|PUT|DELETE', 'POST'),

    // Exemplo: campo password
    NodeProp('Token', 'Token de acesso', npkPassword, '', 'Bearer token ou API key'),

    // Exemplo: inteiro
    NodeProp('Timeout', 'Timeout (ms)', npkInteger, '5000'),

    // Exemplo: booleano
    NodeProp('VerifySSL', 'Verificar SSL', npkBoolean, 'True')

    { TODO: adicione os campos que o seu nó precisa }
  ];
end;

// =============================================================================
//  Execute — lógica principal do nó
//
//  AInput.Values  — TStringList com pares chave=valor vindos do nó anterior
//  AInput.Success — True se o nó anterior foi bem-sucedido
//
//  Deve devolver um TNodeData criado com NewNodeData.
//  Em caso de erro: Result.Success := False; Result.Error := 'mensagem';
// =============================================================================
function TMyCustomNode.Execute(const AInput: TNodeData): TNodeData;
var
  Endpoint : string;
  Method   : string;
  // ... outras variáveis
begin
  // Cria o resultado (obrigatório!)
  Result := NewNodeData;

  // Lê as configurações do nó
  Endpoint := GetProp('Endpoint');
  Method   := GetProp('Method');

  // Lê dados vindos do nó anterior (se aplicável)
  // var ValorAnterior := AInput.Values.Values['alguma_chave'];

  // ── Valida parâmetros obrigatórios ──────────────────────────────────────
  if Endpoint = '' then
  begin
    Result.Success := False;
    Result.Error   := 'URL do serviço não pode estar vazia';
    Exit;
  end;

  // ── Copia os dados de entrada para a saída (passthrough) ────────────────
  if Assigned(AInput.Values) then
    Result.Values.AddStrings(AInput.Values);

  // ── TODO: Implemente aqui a lógica do seu nó ────────────────────────────
  //
  // Exemplos:
  //   Chamar API HTTP (com Indy):
  //     var Http := TIdHTTP.Create; ...
  //
  //   Manipular dados:
  //     var Valor := AInput.Values.Values['campo'];
  //     Result.Values.Values['resultado'] := ...;
  //
  //   Registar no log (debug):
  //     if IsConsole then WriteLn('[MeuNó] executado');
  // ────────────────────────────────────────────────────────────────────────

  // Escreve valores de saída
  Result.Values.Values['status']   := 'ok';
  Result.Values.Values['endpoint'] := Endpoint;
  Result.Values.Values['method']   := Method;

  // Result.Success é True por omissão — só mude se houve erro
end;

// =============================================================================
//  Registo automático — não é necessário modificar
// =============================================================================
initialization
  NodePluginRegistry.Register(TMyCustomNode);

end.
