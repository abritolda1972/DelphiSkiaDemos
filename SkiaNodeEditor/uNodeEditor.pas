// =============================================================================
//  uNodeEditor.pas  —  v4.0  (Plugin Edition)
//  Editor de Nós estilo n8n / Node-RED — com sistema de plugins
//
//  © 2024-2026 Alberto Brito. Todos os direitos reservados.
//
//  Contacto: [abritolda@gmail.com]
// =============================================================================
//
//  NOVIDADES v4.0:
//    ✦ Toolbar FMX nativa (TRectangle + TLabel) com tema escuro consistente
//    ✦ Botões de zoom (−, %, +) na toolbar — sem sobreposição com o log
//    ✦ Botão "Recentrar" — ajusta zoom e pan para mostrar todos os nós
//    ✦ Botão 100% — repõe zoom=1 e centra os nós no canvas
//    ✦ Botão "Novo", "Abrir", "Guardar" — gestão de projectos
//    ✦ Serialização JSON para ficheiro .nep (Node Editor Project)
//    ✦ Atalhos: Ctrl+S (guardar), Ctrl+Shift+S (guardar como),
//               Ctrl+O (abrir), Ctrl+N (novo)
//    ✦ Ícones SVG vectoriais por nó (Feather Icons, branco monocromático)
//    ✦ Tipografia melhorada: Segoe UI / SF Pro com SubpixelAntiAlias
//               e LinearMetrics — texto nítido a qualquer zoom
//    ✦ Constantes tipográficas centralizadas (FONT_*, COL_TEXT_*)
//    ✦ Constantes de espessura centralizadas (STROKE_W, WIRE_W e derivados)
//    ✦ Canvas sem limites — nós podem ser arrastados livremente após pan/zoom
//    ✦ ClipChildren no PaintBox — nós não ultrapassam a toolbar durante zoom
//    ✦ FitToNodes com zoom fixo opcional (usado pelo botão 100%)

//  NOVIDADES v3.0:
//    ✦ Sistema de plugins: crie os seus próprios nós sem tocar neste ficheiro
//    ✦ Paleta organizada por categorias (colapsáveis)
//    ✦ Painel de propriedades gerado automaticamente a partir do plugin
//    ✦ Ícones/emoji por tipo de nó
//    ✦ Execução de nós com TNodeData
//    ✦ Botão "Executar fluxo" (Ctrl+Enter)
//    ✦ Painel de log/output de execução
//
//  Controlos:
//    Arrastar nó         : clique + drag no corpo do nó
//    Duplo-clique        : abre painel de propriedades
//    Nova ligação        : arrastar da porta de SAÍDA (círculo azul)
//    Selecionar ligação  : clicar no cabo (fica amarelo)
//    Delete              : apaga nó ou ligação seleccionados
//    Ctrl+Enter          : executa o fluxo a partir de nós Trigger
//    Ctrl+S / Ctrl+O / N : guardar / abrir / novo projecto
//    Paleta              : clique num item para inserir no canvas
//    Multiseleção        : rubber-band — arraste em área vazia para seleccionar
//    Zoom                : roda do rato; botões −/+/100%/Recentrar na toolbar
//    Pan                 : botão do meio do rato + arrastar
// =============================================================================


unit uNodeEditor;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Math,
  System.Generics.Collections,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Objects,
  FMX.Dialogs,
  Skia,
  FMX.Skia,
  uNodePlugin, FMX.Controls.Presentation;

// =============================================================================
//  Modos de interação
// =============================================================================
type
  TEditorMode = (
    emIdle,
    emDraggingNode,     // Arrastar nó(s) selecionados
    emDraggingWire,     // Desenhar nova ligação
    emRubberBand,       // Seleção por retângulo fantasma
    emPanning           // Pan com botão do meio / espaço+drag
  );

// =============================================================================
//  TNode — bloco no canvas (agora associado a um plugin)
// =============================================================================
  TNode = class
  private
    FID         : Integer;
    FBounds     : TRectF;
    FSelected   : Boolean;
    FPlugin     : TNodePluginBase;  // Instância do plugin (dono: TNode)
    FPluginClass: TNodePluginClass; // Para recriar se necessário
    FRunResult  : string;           // Resultado da última execução
    FRunError   : string;
    FHasRun     : Boolean;
  public
    constructor Create(AID: Integer; AX, AY: Single;
                       APluginClass: TNodePluginClass);
    destructor Destroy; override;

    function HitTest(const P: TPointF): Boolean;
    function HitOutputPort(const P: TPointF): Boolean;
    function HitInputPort (const P: TPointF): Boolean;
    procedure MoveBy(DX, DY: Single);
    function OutputPort: TPointF;
    function InputPort : TPointF;

    // Executa o plugin com os dados fornecidos
    function Execute(const AInput: TNodeData): TNodeData;

    property ID         : Integer          read FID;
    property Bounds     : TRectF           read FBounds     write FBounds;
    property Selected   : Boolean          read FSelected   write FSelected;
    property Plugin     : TNodePluginBase  read FPlugin;
    property PluginClass: TNodePluginClass read FPluginClass;
    property RunResult  : string           read FRunResult  write FRunResult;
    property RunError   : string           read FRunError   write FRunError;
    property HasRun     : Boolean          read FHasRun     write FHasRun;
  end;

// =============================================================================
//  TConnection
// =============================================================================
  TConnection = class
  private
    FID      : Integer;
    FSourceID: Integer;
    FTargetID: Integer;
    FSelected: Boolean;
  public
    constructor Create(AID, ASourceID, ATargetID: Integer);
    property ID      : Integer read FID;
    property SourceID: Integer read FSourceID write FSourceID;
    property TargetID: Integer read FTargetID write FTargetID;
    property Selected: Boolean read FSelected write FSelected;
  end;

// =============================================================================
//  Paleta — categoria colapsável
// =============================================================================
  TPaletteCategory = record
    Category  : TNodeCategory;
    Collapsed : Boolean;
    YStart    : Single;  // Calculado em DrawPalette
    YEnd      : Single;
  end;

// =============================================================================
//  Form principal
// =============================================================================
  TFormNodeEditor = class(TForm)
    PaintBox    : TSkPaintBox;
    ToolBar     : TRectangle;
    BtnZoomOut  : TRectangle;
    BtnZoomReset: TRectangle;
    BtnZoomIn   : TRectangle;
    BtnRun      : TRectangle;
    BtnFit      : TRectangle;
    BtnNew      : TRectangle;
    BtnSave     : TRectangle;
    BtnLoad     : TRectangle;
    LblZoom     : TLabel;
    LblProjectName: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
                          var KeyChar: WideChar; Shift: TShiftState);
    procedure PaintBoxDraw(ASender: TObject; const ACanvas: ISkCanvas;
                           const ADest: TRectF; const AOpacity: Single);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
                                Shift: TShiftState; X, Y: Single);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
                                X, Y: Single);
    procedure PaintBoxMouseUp  (Sender: TObject; Button: TMouseButton;
                                Shift: TShiftState; X, Y: Single);
    procedure PaintBoxDblClick (Sender: TObject);
    procedure PaintBoxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    // Toolbar handlers
    procedure BtnZoomOutClick  (Sender: TObject);
    procedure BtnZoomResetClick(Sender: TObject);
    procedure BtnZoomInClick   (Sender: TObject);
    procedure BtnRunClick      (Sender: TObject);
    procedure BtnFitClick      (Sender: TObject);
    procedure BtnNewClick      (Sender: TObject);
    procedure BtnSaveClick     (Sender: TObject);
    procedure BtnLoadClick     (Sender: TObject);
  private
    // --- Dados ---
    FNodes      : TObjectList<TNode>;
    FConnections: TObjectList<TConnection>;
    FNextID     : Integer;
    FProjectFile: string;   // Caminho do ficheiro actual ('' = novo projecto)

    // --- Interação ---
    FMode       : TEditorMode;
    FDragNode   : TNode;
    FDragOffX   : Single;
    FDragOffY   : Single;
    FWireSrc    : TNode;
    FWirePos    : TPointF;
    FWireTarget : TNode;
    FHoverNode  : TNode;

    // --- Seleção múltipla (rubber-band) ---
    FRubberStart: TPointF;   // Ponto inicial do retângulo
    FRubberEnd  : TPointF;   // Ponto atual (durante drag)
    FMultiDragOffsets: array of TPointF;  // Offset de cada nó selecionado

    // --- Zoom & Pan ---
    FZoom       : Single;    // 0.25 .. 4.0  (1.0 = 100%)
    FPanX       : Single;    // Deslocamento X do canvas
    FPanY       : Single;    // Deslocamento Y do canvas
    FPanStart   : TPointF;   // Posição do rato ao iniciar pan
    FPanOrigin  : TPointF;   // FPanX/Y ao iniciar pan

    // --- Paleta ---
    FPaletteW     : Single;
    FPaletteHover : Integer;    // Índice global de item com hover
    FPalCats      : array of TPaletteCategory;
    FPalItems     : array of record
                      PluginIdx: Integer;
                      Rect     : TRectF;
                    end;
    FCatHeaderH   : Single;

    // --- Painel de propriedades ---
    FPropNode     : TNode;
    FPropW        : Single;
    FPropHover    : Integer;
    FPropScrollY  : Single;   // Scroll dentro do painel

    // --- Double-click ---
    FLastClickTime: TDateTime;
    FLastClickNode: TNode;

    // --- Log de execução ---
    FLogLines   : TStringList;
    FLogVisible : Boolean;
    FLogH       : Single;

    // --- Helpers gerais ---
    function  GenID: Integer;
    function  NodeByID(AID: Integer): TNode;
    function  NodeAt(AX, AY: Single): TNode;
    function  ConnAt(AX, AY: Single): TConnection;

    // --- Paleta ---
    function  InPalette(AX: Single): Boolean;
    procedure BuildPaletteLayout;
    function  PaletteItemAt(AX, AY: Single): Integer;  // Índice em FPalItems
    function  PaletteCatHeaderAt(AX, AY: Single): Integer;
    procedure AddNodeFromPaletteIdx(AItemIdx: Integer);

    // --- Propriedades ---
    function  InPropPanel(AX: Single): Boolean;
    function  PropPanelW: Single;
    function  PropRowH: Single;
    function  PropRowRect(AIdx: Integer): TRectF;
    function  PropRowAt(AX, AY: Single): Integer;
    function  PropAddBtnRect: TRectF;
    function  PropCloseBtnRect: TRectF;
    procedure OpenPropPanel(ANode: TNode);
    procedure ClosePropPanel;
    procedure EditPropAt(AIdx: Integer);
    procedure AddNewProp;

    // --- Acções de teclado ---
    procedure BringToFront(ANode: TNode);
    procedure ClearSel;
    procedure DeleteSelConn;
    procedure DeleteSelNode;

    // --- Zoom & Pan ---
    procedure ZoomAt(ADelta: Integer; AScreenX, AScreenY: Single);
    procedure ResetZoom;
    procedure FitToNodes(AFixedZoom: Single = 0);
    procedure UpdateZoomLabel;
    function  CanvasToScreen(const P: TPointF): TPointF;
    function  ScreenToCanvas(const P: TPointF): TPointF;
    function  ZoomRect: TRectF;

    // --- Execução de fluxo ---
    procedure RunFlow;
    function  ExecuteFrom(ANode: TNode; const AInput: TNodeData): TNodeData;

    // --- Projecto: Guardar / Carregar ---
    procedure NewProject;
    procedure SaveProject(const APath: string);
    procedure LoadProject(const APath: string);
    procedure UpdateTitleBar;

    // --- Bézier ---
    procedure BezierCtrl(const P0, P3: TPointF; out P1, P2: TPointF);
    function  NearBezier(const P, P0, P1, P2, P3: TPointF; Thresh: Single): Boolean;

    // --- Desenho ---
    procedure DrawBG          (const C: ISkCanvas; const R: TRectF);
    procedure DrawConn        (const C: ISkCanvas; Conn: TConnection);
    procedure DrawWireGhost   (const C: ISkCanvas);
    procedure DrawNode        (const C: ISkCanvas; N: TNode);
    procedure DrawPort        (const C: ISkCanvas; const Pt: TPointF;
                               IsOutput, Highlight: Boolean);
    procedure DrawPalette     (const C: ISkCanvas; const R: TRectF);
    procedure DrawPropPanel   (const C: ISkCanvas; const R: TRectF);
    procedure DrawLogPanel    (const C: ISkCanvas; const R: TRectF);
    procedure DrawRubberBand  (const C: ISkCanvas);

  public

  end;

var
  FormNodeEditor: TFormNodeEditor;

implementation

{$R *.fmx}

uses
  uBuiltinNodes,      // Regista os nós integrados
  uExamplePlugins,    // Regista os plugins de exemplo
  System.JSON,
  System.IOUtils;
  // Adicione aqui as suas próprias units de plugin:
  // uMeusPlugins;

// =============================================================================
//  Constantes
// =============================================================================
const
  PORT_R      = 7;
  PORT_HIT    = 12;
  CORNER_R    = 10;
  HDR_H       = 30;
  NODE_W      = 190;
  NODE_H      = 82;
  PAL_W       = 200;   // Paleta mais larga para texto legível
  PAL_ITEM_H  = 48;
  PAL_PAD     = 8;
  CAT_HDR_H   = 28;
  WIRE_THRESH = 8;
  LOG_H_DEF   = 140;
  STROKE_W    = 1.0;
  WIRE_W      = 2.5;
  WIRE_GLOW     = WIRE_W * 2.4;
  WIRE_GLOW_SEL = WIRE_W * 4.8;
  WIRE_GHOST    = WIRE_W * 3.2;

  // Tamanhos de fonte — altere aqui para ajustar toda a tipografia do editor
  FONT_NODE_TITLE  = 11.0;   // Título do nó no cabeçalho
  FONT_NODE_PROP   = 10.0;   // Propriedades visíveis no corpo do nó
  FONT_NODE_STATUS = 10.0;   // Status de execução (✓ OK / ❌ erro)
  FONT_PAL_HEADER  = 10.0;   // Cabeçalho "COMPONENTES"
  FONT_PAL_CAT     = 10.5;   // Nome da categoria (Trigger, HTTP…)
  FONT_PAL_TITLE   = 11.0;   // Nome do plugin na paleta
  FONT_PAL_DESC    = 10.0;   // Descrição curta do plugin
  FONT_PROP_TITLE  = 14.0;   // Título grande do nó no painel de props
  FONT_PROP_LABEL  = 10.5;   // Label de cada campo
  FONT_PROP_VALUE  = 11.0;   // Valor de cada campo
  FONT_PROP_HINT   = 10.0;   // Texto de ajuda / dica
  FONT_LOG         = 10.0;   // Linhas do log de execução
  FONT_ICON_CLOSE  = 14.0;   // Ícone × do botão fechar painel
  FONT_ICON_EDIT   = 12.0;   // Ícone ✎ de edição de campo

  // Opacidades de texto — $FF = totalmente opaco
  COL_TEXT_PRIMARY   = $FFFFFFFF;   // Texto principal — branco puro
  COL_TEXT_SECONDARY = $CCAABBCC;   // Labels / subtítulos — azul claro
  COL_TEXT_MUTED     = $99AABBCC;   // Texto de dica / secundário
  COL_TEXT_DISABLED  = $66AABBCC;   // Texto desactivado / label de secção

// Paleta de cores para categorias
const
  CAT_COLORS: array[TNodeCategory] of Cardinal = (
    $FFDC143C,  // Trigger   - vermelho
    $FF00897B,  // HTTP      - verde-azulado
    $FF2979FF,  // Transform - azul
    $FF00BFA5,  // Logic     - ciano
    $FFFF6F00,  // IO        - laranja
    $FF5D4037,  // Utility   - castanho
    $FF7B1FA2   // Custom    - roxo
  );

// =============================================================================
//  Helper — IfThen para TAlphaColor (Cardinal)
//  (System.Math.IfThen não tem sobrecarga para Cardinal)
// =============================================================================
function ColIf(ACond: Boolean; ATrue, AFalse: TAlphaColor): TAlphaColor; inline;
begin
  if ACond then Result := ATrue else Result := AFalse;
end;

// =============================================================================
//  Helper — cria TSkFont com antialiasing subpixel sempre activo
//  Usa 'Segoe UI' no Windows, 'SF Pro' no macOS, fallback para a fonte padrão.
// =============================================================================
var
  GUITypeface: ISkTypeface = nil;

function UITypeface: ISkTypeface;
begin
  if not Assigned(GUITypeface) then
  begin
    {$IFDEF MSWINDOWS}
    GUITypeface := TSkTypeface.MakeFromName('Segoe UI', TSkFontStyle.Normal);
    {$ELSEIF DEFINED(MACOS)}
    GUITypeface := TSkTypeface.MakeFromName('SF Pro Display', TSkFontStyle.Normal);
    {$ELSE}
    GUITypeface := TSkTypeface.MakeFromName('Roboto', TSkFontStyle.Normal);
    {$ENDIF}
    if not Assigned(GUITypeface) then
      GUITypeface := TSkTypeface.MakeDefault;
  end;
  Result := GUITypeface;
end;

function MakeFont(ASize: Single; ABold: Boolean = False): ISkFont;
begin
  if ABold then
    Result := TSkFont.Create(
      TSkTypeface.MakeFromName(UITypeface.FamilyName, TSkFontStyle.Bold), ASize)
  else
    Result := TSkFont.Create(UITypeface, ASize);
  Result.Edging        := TSkFontEdging.SubpixelAntiAlias;
  Result.Subpixel      := True;
  Result.LinearMetrics := True;
end;

// =============================================================================
//  TNode
// =============================================================================
constructor TNode.Create(AID: Integer; AX, AY: Single;
                          APluginClass: TNodePluginClass);
var
  Defs: TArray<TNodePropertyDef>;
  D   : TNodePropertyDef;
begin
  inherited Create;
  FID          := AID;
  FBounds      := TRectF.Create(AX, AY, AX + NODE_W, AY + NODE_H);
  FSelected    := False;
  FPluginClass := APluginClass;
  FPlugin      := APluginClass.Create;
  FPlugin.Initialize;
  FHasRun      := False;
  FRunResult   := '';
  FRunError    := '';

  // Preenche as propriedades com os valores default do plugin
  Defs := FPlugin.GetPropertyDefs;
  for D in Defs do
    if D.Default <> '' then
      FPlugin.Props.Values[D.Name] := D.Default;
end;

destructor TNode.Destroy;
begin
  if Assigned(FPlugin) then
  begin
    FPlugin.Finalize;
    FPlugin.Free;
  end;
  inherited;
end;

function TNode.HitTest(const P: TPointF): Boolean;
begin Result := FBounds.Contains(P); end;

function TNode.HitOutputPort(const P: TPointF): Boolean;
var C: TPointF;
begin C := OutputPort; Result := Sqrt(Sqr(P.X-C.X)+Sqr(P.Y-C.Y)) <= PORT_HIT; end;

function TNode.HitInputPort(const P: TPointF): Boolean;
var C: TPointF;
begin C := InputPort;  Result := Sqrt(Sqr(P.X-C.X)+Sqr(P.Y-C.Y)) <= PORT_HIT; end;

procedure TNode.MoveBy(DX, DY: Single);
begin FBounds.Offset(DX, DY); end;

function TNode.OutputPort: TPointF;
begin Result := TPointF.Create(FBounds.Right, FBounds.CenterPoint.Y); end;

function TNode.InputPort: TPointF;
begin Result := TPointF.Create(FBounds.Left,  FBounds.CenterPoint.Y); end;

function TNode.Execute(const AInput: TNodeData): TNodeData;
begin
  Result   := FPlugin.Execute(AInput);
  FHasRun  := True;
  if Result.Success then
    FRunResult := Result.Values.Text
  else
  begin
    FRunResult := '';
    FRunError  := Result.Error;
  end;
end;

// =============================================================================
//  TConnection
// =============================================================================
constructor TConnection.Create(AID, ASourceID, ATargetID: Integer);
begin
  inherited Create;
  FID       := AID;
  FSourceID := ASourceID;
  FTargetID := ATargetID;
  FSelected := False;
end;

// =============================================================================
//  FormCreate
// =============================================================================
procedure TFormNodeEditor.FormCreate(Sender: TObject);

  procedure AddNode(AClass: TNodePluginClass; AX, AY: Single);
  var N: TNode;
  begin
    N := TNode.Create(GenID, AX, AY, AClass);
    FNodes.Add(N);
  end;

var N1, N2, N3, N4, N5: TNode;
begin
  FNodes         := TObjectList<TNode>.Create(True);
  FConnections   := TObjectList<TConnection>.Create(True);
  FLogLines      := TStringList.Create;
  FNextID        := 100;
  FProjectFile   := '';
  FMode          := emIdle;
  FDragNode      := nil;
  FWireSrc       := nil;
  FWireTarget    := nil;
  FHoverNode     := nil;
  FPaletteW      := PAL_W;
  FPaletteHover  := -1;
  FCatHeaderH    := CAT_HDR_H;
  FPropNode      := nil;
  FPropW         := 230;
  FPropHover     := -1;
  FPropScrollY   := 0;
  FLastClickTime := 0;
  FLastClickNode := nil;
  FLogVisible    := True;
  FLogH          := LOG_H_DEF;

  // Zoom & Pan
  FZoom  := 1.0;
  FPanX  := 0;
  FPanY  := 0;

  // Constrói layout da paleta (precisa dos plugins registados)
  BuildPaletteLayout;

  // Nós iniciais de demonstração
  N1 := TNode.Create(1, 60,  80,  TWebhookNode);
  N1.Plugin.Props.Values['Method'] := 'POST';
  N1.Plugin.Props.Values['Path']   := '/evento';
  FNodes.Add(N1);

  N2 := TNode.Create(2, 300, 50,  TTransformNode);
  N2.Plugin.Props.Values['Field']  := 'body.data';
  N2.Plugin.Props.Values['Format'] := 'objeto';
  FNodes.Add(N2);

  N3 := TNode.Create(3, 300, 200, TConditionNode);
  N3.Plugin.Props.Values['LeftField']  := 'valor';
  N3.Plugin.Props.Values['Operator']   := '>';
  N3.Plugin.Props.Values['RightValue'] := '100';
  FNodes.Add(N3);

  N4 := TNode.Create(4, 550, 50,  TSendEmailNode);
  N4.Plugin.Props.Values['To'] := 'user@exemplo.com';
  FNodes.Add(N4);

  N5 := TNode.Create(5, 550, 200, TSaveToDatabaseNode);
  N5.Plugin.Props.Values['Table'] := 'eventos';
  FNodes.Add(N5);

  FConnections.Add(TConnection.Create(GenID, 1, 2));
  FConnections.Add(TConnection.Create(GenID, 1, 3));
  FConnections.Add(TConnection.Create(GenID, 2, 4));
  FConnections.Add(TConnection.Create(GenID, 3, 5));

  FLogLines.Add('▶  Editor iniciado — ' + FormatDateTime('hh:nn:ss', Now));
  FLogLines.Add('   ' + IntToStr(NodePluginRegistry.Count) + ' plugins registados.');

  UpdateTitleBar;

  // Centra os nós iniciais no canvas ao arrancar
  TThread.ForceQueue(nil, procedure begin FitToNodes; end);
end;

procedure TFormNodeEditor.FormDestroy(Sender: TObject);
begin
  FConnections.Free;
  FNodes.Free;
  FLogLines.Free;
end;

// =============================================================================
//  BuildPaletteLayout — constrói lista de itens e categorias para a paleta
// =============================================================================
procedure TFormNodeEditor.BuildPaletteLayout;
var
  CatExists : array[TNodeCategory] of Boolean;
  Cat       : TNodeCategory;
  I, NPlug  : Integer;
  Inst      : TNodePluginBase;
begin
  NPlug := NodePluginRegistry.Count;

  // Determina quais categorias têm plugins
  FillChar(CatExists, SizeOf(CatExists), 0);
  for I := 0 to NPlug - 1 do
    CatExists[NodePluginRegistry.CategoryOf(I)] := True;

  // Cria as categorias
  SetLength(FPalCats, 0);
  for Cat := Low(TNodeCategory) to High(TNodeCategory) do
    if CatExists[Cat] then
    begin
      SetLength(FPalCats, Length(FPalCats) + 1);
      FPalCats[High(FPalCats)].Category  := Cat;
      FPalCats[High(FPalCats)].Collapsed := False;
    end;

  // Cria lista plana de itens (com referência ao índice do plugin)
  SetLength(FPalItems, NPlug);
  for I := 0 to NPlug - 1 do
  begin
    FPalItems[I].PluginIdx := I;
    FPalItems[I].Rect      := TRectF.Empty;  // calculado em DrawPalette
  end;
end;

// =============================================================================
//  Helpers
// =============================================================================
function TFormNodeEditor.GenID: Integer;
begin Inc(FNextID); Result := FNextID; end;

function TFormNodeEditor.NodeByID(AID: Integer): TNode;
var I: Integer;
begin
  Result := nil;
  for I := 0 to FNodes.Count-1 do
    if FNodes[I].ID = AID then Exit(FNodes[I]);
end;

function TFormNodeEditor.NodeAt(AX, AY: Single): TNode;
var I: Integer;
begin
  Result := nil;
  for I := FNodes.Count-1 downto 0 do
    if FNodes[I].HitTest(TPointF.Create(AX,AY)) then Exit(FNodes[I]);
end;

procedure TFormNodeEditor.BezierCtrl(const P0, P3: TPointF; out P1, P2: TPointF);
var DX: Single;
begin
  DX := Max(Abs(P3.X-P0.X)*0.5, 60);
  P1 := TPointF.Create(P0.X+DX, P0.Y);
  P2 := TPointF.Create(P3.X-DX, P3.Y);
end;

function TFormNodeEditor.NearBezier(const P, P0, P1, P2, P3: TPointF; Thresh: Single): Boolean;
var I: Integer; T,U,BX,BY: Single;
begin
  Result := False;
  for I := 0 to 32 do begin
    T := I/32; U := 1-T;
    BX := U*U*U*P0.X + 3*U*U*T*P1.X + 3*U*T*T*P2.X + T*T*T*P3.X;
    BY := U*U*U*P0.Y + 3*U*U*T*P1.Y + 3*U*T*T*P2.Y + T*T*T*P3.Y;
    if Sqrt(Sqr(P.X-BX)+Sqr(P.Y-BY)) <= Thresh then Exit(True);
  end;
end;

function TFormNodeEditor.ConnAt(AX, AY: Single): TConnection;
var I: Integer; Conn: TConnection; Src,Dst: TNode; P0,P1,P2,P3: TPointF;
begin
  Result := nil;
  for I := FConnections.Count-1 downto 0 do begin
    Conn := FConnections[I];
    Src  := NodeByID(Conn.SourceID);
    Dst  := NodeByID(Conn.TargetID);
    if (Src=nil) or (Dst=nil) then Continue;
    P0 := Src.OutputPort; P3 := Dst.InputPort;
    BezierCtrl(P0,P3,P1,P2);
    if NearBezier(TPointF.Create(AX,AY),P0,P1,P2,P3,WIRE_THRESH) then Exit(Conn);
  end;
end;

function TFormNodeEditor.InPalette(AX: Single): Boolean;
begin Result := AX >= (PaintBox.Width - FPaletteW); end;

function TFormNodeEditor.InPropPanel(AX: Single): Boolean;
begin Result := Assigned(FPropNode) and (AX < PropPanelW); end;

function TFormNodeEditor.PropPanelW: Single; begin Result := FPropW; end;
function TFormNodeEditor.PropRowH  : Single; begin Result := 56; end;

function TFormNodeEditor.PropRowRect(AIdx: Integer): TRectF;
const HDR = 120; PAD = 8;
var PY: Single;
begin
  PY := HDR + AIdx * (PropRowH + 4) - FPropScrollY;
  Result := TRectF.Create(PAD, PY, PropPanelW-PAD, PY+PropRowH);
end;

function TFormNodeEditor.PropAddBtnRect: TRectF;
const PAD = 8;
var PY: Single;
begin
  if not Assigned(FPropNode) then Exit(TRectF.Empty);
  PY := PropRowRect(FPropNode.Plugin.Props.Count).Top + 6;
  Result := TRectF.Create(PAD, PY, PropPanelW-PAD, PY+30);
end;

function TFormNodeEditor.PropCloseBtnRect: TRectF;
begin Result := TRectF.Create(PropPanelW-32, 6, PropPanelW-6, 30); end;

function TFormNodeEditor.PropRowAt(AX, AY: Single): Integer;
var I: Integer;
begin
  Result := -1;
  if not Assigned(FPropNode) then Exit;
  for I := 0 to FPropNode.Plugin.Props.Count-1 do
    if PropRowRect(I).Contains(TPointF.Create(AX,AY)) then Exit(I);
end;

procedure TFormNodeEditor.OpenPropPanel(ANode: TNode);
begin
  FPropNode    := ANode;
  FPropHover   := -1;
  FPropScrollY := 0;
end;

procedure TFormNodeEditor.ClosePropPanel;
begin FPropNode := nil; FPropHover := -1; end;

procedure TFormNodeEditor.EditPropAt(AIdx: Integer);
var
  Defs        : TArray<TNodePropertyDef>;
  Key, Val    : string;
  NewVal, Lbl : string;
  Chosen      : string;
  Def         : TNodePropertyDef;
  HasDef      : Boolean;
  I, J        : Integer;
  Opts        : TArray<string>;
  Valid       : Boolean;
begin
  if not Assigned(FPropNode) then Exit;
  if (AIdx < 0) or (AIdx >= FPropNode.Plugin.Props.Count) then Exit;

  Key := FPropNode.Plugin.Props.Names[AIdx];
  Val := FPropNode.Plugin.Props.ValueFromIndex[AIdx];

  // Procura a definição para este campo
  Defs   := FPropNode.Plugin.GetPropertyDefs;
  HasDef := False;
  for I := 0 to High(Defs) do
    if Defs[I].Name = Key then
    begin
      Def    := Defs[I];
      HasDef := True;
      Break;
    end;

  // Para campos Select: mostra as opções
  if HasDef and (Def.Kind = npkSelect) and (Def.Options <> '') then
  begin
    Opts   := Def.Options.Split(['|']);
    Chosen := Val;
    if InputQuery(Def.Label_, 'Opções: ' + Def.Options, Chosen) then
    begin
      Valid := False;
      for J := 0 to High(Opts) do
        if Opts[J] = Chosen then Valid := True;
      if Valid or (Chosen <> '') then
      begin
        FPropNode.Plugin.Props.ValueFromIndex[AIdx] := Chosen;
        PaintBox.Redraw;
      end;
    end;
    Exit;
  end;

  // Caso geral
  NewVal := Val;
  Lbl    := Key;
  if HasDef then Lbl := Def.Label_;
  if InputQuery('Editar — ' + Lbl, Lbl + ':', NewVal) then
  begin
    FPropNode.Plugin.Props.ValueFromIndex[AIdx] := NewVal;
    PaintBox.Redraw;
  end;
end;

procedure TFormNodeEditor.AddNewProp;
var Key, Val: string;
begin
  if not Assigned(FPropNode) then Exit;
  Key := ''; Val := '';
  if InputQuery('Novo campo', 'Nome do campo:', Key) and (Key <> '') then
  begin
    if InputQuery('Novo campo', 'Valor inicial:', Val) then
    begin
      FPropNode.Plugin.Props.Values[Key] := Val;
      PaintBox.Redraw;
    end;
  end;
end;

procedure TFormNodeEditor.BringToFront(ANode: TNode);
begin
  if FNodes.IndexOf(ANode) >= 0 then
  begin FNodes.Extract(ANode); FNodes.Add(ANode); end;
end;

procedure TFormNodeEditor.ClearSel;
var I: Integer;
begin
  for I := 0 to FNodes.Count-1       do FNodes[I].Selected       := False;
  for I := 0 to FConnections.Count-1 do FConnections[I].Selected := False;
end;

procedure TFormNodeEditor.DeleteSelConn;
var I: Integer;
begin
  for I := FConnections.Count-1 downto 0 do
    if FConnections[I].Selected then begin FConnections.Delete(I); Break; end;
  PaintBox.Redraw;
end;

procedure TFormNodeEditor.DeleteSelNode;
var I: Integer; ToRem: TNode;
begin
  ToRem := nil;
  for I := 0 to FNodes.Count-1 do
    if FNodes[I].Selected then begin ToRem := FNodes[I]; Break; end;
  if ToRem = nil then Exit;
  if FPropNode = ToRem then ClosePropPanel;
  for I := FConnections.Count-1 downto 0 do
    if (FConnections[I].SourceID = ToRem.ID) or
       (FConnections[I].TargetID = ToRem.ID) then
      FConnections.Delete(I);
  FNodes.Remove(ToRem);
  PaintBox.Redraw;
end;

// =============================================================================
//  Paleta
// =============================================================================
function TFormNodeEditor.PaletteItemAt(AX, AY: Single): Integer;
var I: Integer;
begin
  Result := -1;
  if not InPalette(AX) then Exit;
  for I := 0 to High(FPalItems) do
    if not FPalItems[I].Rect.IsEmpty and
       FPalItems[I].Rect.Contains(TPointF.Create(AX,AY)) then
      Exit(I);
end;

function TFormNodeEditor.PaletteCatHeaderAt(AX, AY: Single): Integer;
var I: Integer;
begin
  Result := -1;
  if not InPalette(AX) then Exit;
  for I := 0 to High(FPalCats) do
    if (AY >= FPalCats[I].YStart) and (AY < FPalCats[I].YStart + CAT_HDR_H) then
      Exit(I);
end;

procedure TFormNodeEditor.AddNodeFromPaletteIdx(AItemIdx: Integer);
var
  N   : TNode;
  PC  : TNodePluginClass;
  CX, CY: Single;
begin
  if (AItemIdx < 0) or (AItemIdx > High(FPalItems)) then Exit;
  PC := NodePluginRegistry.Classes[FPalItems[AItemIdx].PluginIdx];

  CX := (PaintBox.Width - FPaletteW) / 2 - NODE_W/2 + (Random(100)-50);
  CY :=  PaintBox.Height / 2              - NODE_H/2 + (Random(100)-50);

  N := TNode.Create(GenID, CX, CY, PC);
  ClearSel;
  N.Selected := True;
  FNodes.Add(N);
  BringToFront(N);
  PaintBox.Redraw;
end;

// =============================================================================
//  Zoom & Pan
// =============================================================================

// Converte coordenada do canvas (mundo) para ecrã
function TFormNodeEditor.CanvasToScreen(const P: TPointF): TPointF;
begin
  Result.X := P.X * FZoom + FPanX;
  Result.Y := P.Y * FZoom + FPanY;
end;

// Converte coordenada do ecrã para canvas (mundo)
function TFormNodeEditor.ScreenToCanvas(const P: TPointF): TPointF;
begin
  Result.X := (P.X - FPanX) / FZoom;
  Result.Y := (P.Y - FPanY) / FZoom;
end;

// Retorna o rect do viewport no espaço do canvas (para hit-tests etc.)
function TFormNodeEditor.ZoomRect: TRectF;
begin
  Result := TRectF.Create(
    ScreenToCanvas(TPointF.Create(0, 0)),
    ScreenToCanvas(TPointF.Create(PaintBox.Width - FPaletteW, PaintBox.Height))
  );
end;

// Zoom centrado num ponto do ecrã
procedure TFormNodeEditor.ZoomAt(ADelta: Integer; AScreenX, AScreenY: Single);
const
  STEP     = 0.12;
  ZOOM_MIN = 0.2;
  ZOOM_MAX = 4.0;
var
  OldZoom, NewZoom: Single;
begin
  OldZoom := FZoom;
  if ADelta > 0 then
    NewZoom := Min(FZoom * (1 + STEP), ZOOM_MAX)
  else
    NewZoom := Max(FZoom * (1 - STEP), ZOOM_MIN);

  // Ajusta pan para que o ponto sob o cursor fique fixo
  FPanX := AScreenX - (AScreenX - FPanX) * (NewZoom / OldZoom);
  FPanY := AScreenY - (AScreenY - FPanY) * (NewZoom / OldZoom);
  FZoom := NewZoom;
  UpdateZoomLabel;
  PaintBox.Redraw;
end;

procedure TFormNodeEditor.ResetZoom;
begin
  FZoom := 1.0;
  FPanX := 0;
  FPanY := 0;
  UpdateZoomLabel;
  PaintBox.Redraw;
end;

// Calcula o bounding box de todos os nós e ajusta zoom+pan
// para que fiquem todos visíveis com margem, centrados no canvas.
// Se AFixedZoom > 0, usa esse zoom em vez de calcular o melhor.
procedure TFormNodeEditor.FitToNodes(AFixedZoom: Single = 0);
const
  MARGIN = 60;
var
  I          : Integer;
  BB         : TRectF;
  N          : TNode;
  CanvasW    : Single;
  CanvasH    : Single;
  ScaleX     : Single;
  ScaleY     : Single;
  NewZoom    : Single;
begin
  if FNodes.Count = 0 then
  begin
    FZoom := AFixedZoom;
    if FZoom <= 0 then FZoom := 1.0;
    FPanX := 0; FPanY := 0;
    UpdateZoomLabel;
    PaintBox.Redraw;
    Exit;
  end;

  // Bounding box de todos os nós (coordenadas do mundo)
  BB := FNodes[0].Bounds;
  for I := 1 to FNodes.Count - 1 do
  begin
    N := FNodes[I];
    if N.Bounds.Left   < BB.Left   then BB.Left   := N.Bounds.Left;
    if N.Bounds.Top    < BB.Top    then BB.Top     := N.Bounds.Top;
    if N.Bounds.Right  > BB.Right  then BB.Right   := N.Bounds.Right;
    if N.Bounds.Bottom > BB.Bottom then BB.Bottom  := N.Bounds.Bottom;
  end;

  // Área visível do canvas (sem a paleta)
  CanvasW := PaintBox.Width  - FPaletteW;
  CanvasH := PaintBox.Height;

  if AFixedZoom > 0 then
    NewZoom := AFixedZoom
  else
  begin
    // Zoom para caber com margem
    ScaleX  := (CanvasW - MARGIN * 2) / Max(BB.Width,  1);
    ScaleY  := (CanvasH - MARGIN * 2) / Max(BB.Height, 1);
    NewZoom := Min(ScaleX, ScaleY);
    NewZoom := Max(0.2, Min(NewZoom, 4.0));
  end;

  // Pan para centrar
  FZoom := NewZoom;
  FPanX := (CanvasW - BB.Width  * FZoom) / 2 - BB.Left  * FZoom;
  FPanY := (CanvasH - BB.Height * FZoom) / 2 - BB.Top   * FZoom;

  UpdateZoomLabel;
  PaintBox.Redraw;
end;

// Actualiza o label de zoom na toolbar
procedure TFormNodeEditor.UpdateZoomLabel;
var S: string;
begin
  S := IntToStr(Round(FZoom * 100)) + '%';
  LblZoom.Text := S;
end;

// --- Handlers dos botões da toolbar ---
procedure TFormNodeEditor.BtnZoomOutClick(Sender: TObject);
begin
  ZoomAt(-1, PaintBox.Width / 2, PaintBox.Height / 2);
end;

procedure TFormNodeEditor.BtnZoomResetClick(Sender: TObject);
begin
  FitToNodes(1.0);
end;

procedure TFormNodeEditor.BtnZoomInClick(Sender: TObject);
begin
  ZoomAt(1, PaintBox.Width / 2, PaintBox.Height / 2);
end;

procedure TFormNodeEditor.BtnRunClick(Sender: TObject);
begin
  RunFlow;
end;

procedure TFormNodeEditor.BtnFitClick(Sender: TObject);
begin
  FitToNodes;
end;


// =============================================================================
//  Projecto — UpdateTitleBar
// =============================================================================
procedure TFormNodeEditor.UpdateTitleBar;
var Name: string;
begin
  if FProjectFile = '' then
    Name := 'Novo Projecto'
  else
    Name := TPath.GetFileNameWithoutExtension(FProjectFile);
  Caption := 'Node Editor — ' + Name;
  if Assigned(LblProjectName) then
    LblProjectName.Text := Name;
end;

// =============================================================================
//  Projecto — NewProject
// =============================================================================
procedure TFormNodeEditor.NewProject;
begin
  ClosePropPanel;
  FNodes.Clear;
  FConnections.Clear;
  FLogLines.Clear;
  FNextID      := 100;
  FProjectFile := '';
  FZoom        := 1.0;
  FPanX        := 0;
  FPanY        := 0;
  FLogLines.Add('▶  Novo projecto — ' + FormatDateTime('hh:nn:ss', Now));
  UpdateTitleBar;
  UpdateZoomLabel;
  PaintBox.Redraw;
end;

// =============================================================================
//  Projecto — SaveProject
// =============================================================================
procedure TFormNodeEditor.SaveProject(const APath: string);
var
  Root, JMeta, JView : TJSONObject;
  JNodes, JConns     : TJSONArray;
  JNode, JConn       : TJSONObject;
  JProps             : TJSONObject;
  N                  : TNode;
  Conn               : TConnection;
  I                  : Integer;
  JSON               : string;
begin
  Root  := TJSONObject.Create;
  try
    // Metadados
    JMeta := TJSONObject.Create;
    JMeta.AddPair('version',  '1.0');
    JMeta.AddPair('saved',    FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
    JMeta.AddPair('generator','NodeEditor v3');
    Root.AddPair('meta', JMeta);

    // Vista (zoom + pan)
    JView := TJSONObject.Create;
    JView.AddPair('zoom', TJSONNumber.Create(FZoom));
    JView.AddPair('panX', TJSONNumber.Create(FPanX));
    JView.AddPair('panY', TJSONNumber.Create(FPanY));
    Root.AddPair('view', JView);

    // Nós
    JNodes := TJSONArray.Create;
    for I := 0 to FNodes.Count - 1 do
    begin
      N     := FNodes[I];
      JNode := TJSONObject.Create;
      JNode.AddPair('id',      TJSONNumber.Create(N.ID));
      JNode.AddPair('plugin',  N.Plugin.GetTitle);
      JNode.AddPair('x',       TJSONNumber.Create(N.Bounds.Left));
      JNode.AddPair('y',       TJSONNumber.Create(N.Bounds.Top));
      // Propriedades configuradas
      JProps := TJSONObject.Create;
      for var P := 0 to N.Plugin.Props.Count - 1 do
        JProps.AddPair(N.Plugin.Props.Names[P],
                       N.Plugin.Props.ValueFromIndex[P]);
      JNode.AddPair('props', JProps);
      JNodes.Add(JNode);
    end;
    Root.AddPair('nodes', JNodes);

    // Ligações
    JConns := TJSONArray.Create;
    for I := 0 to FConnections.Count - 1 do
    begin
      Conn  := FConnections[I];
      JConn := TJSONObject.Create;
      JConn.AddPair('id',     TJSONNumber.Create(Conn.ID));
      JConn.AddPair('source', TJSONNumber.Create(Conn.SourceID));
      JConn.AddPair('target', TJSONNumber.Create(Conn.TargetID));
      JConns.Add(JConn);
    end;
    Root.AddPair('connections', JConns);

    JSON := Root.Format(2);
  finally
    Root.Free;
  end;

  TFile.WriteAllText(APath, JSON, TEncoding.UTF8);
  FProjectFile := APath;
  UpdateTitleBar;
  FLogLines.Add('💾  Guardado: ' + TPath.GetFileName(APath));
  PaintBox.Redraw;
end;

// =============================================================================
//  Projecto — LoadProject
// =============================================================================
procedure TFormNodeEditor.LoadProject(const APath: string);
var
  JSON               : string;
  Root               : TJSONObject;
  JView              : TJSONObject;
  JNodes, JConns     : TJSONArray;
  JNode, JConn       : TJSONObject;
  JProps             : TJSONObject;
  JPair              : TJSONPair;
  PlugTitle          : string;
  PC                 : TNodePluginClass;
  N                  : TNode;
  NID, SrcID, TgtID  : Integer;
  NX, NY             : Single;
  I, J               : Integer;
  MaxID              : Integer;
begin
  if not TFile.Exists(APath) then
  begin
    FLogLines.Add('❌  Ficheiro não encontrado: ' + APath);
    Exit;
  end;

  JSON := TFile.ReadAllText(APath, TEncoding.UTF8);
  Root := TJSONObject.ParseJSONValue(JSON) as TJSONObject;
  if Root = nil then
  begin
    FLogLines.Add('❌  Ficheiro inválido (JSON mal formado)');
    Exit;
  end;

  try
    ClosePropPanel;
    FNodes.Clear;
    FConnections.Clear;
    MaxID := 100;

    // Vista
    JView := Root.GetValue<TJSONObject>('view', nil);
    if Assigned(JView) then
    begin
      FZoom := JView.GetValue<TJSONNumber>('zoom', TJSONNumber.Create(1)).AsDouble;
      FPanX := JView.GetValue<TJSONNumber>('panX', TJSONNumber.Create(0)).AsDouble;
      FPanY := JView.GetValue<TJSONNumber>('panY', TJSONNumber.Create(0)).AsDouble;
    end;

    // Nós
    JNodes := Root.GetValue<TJSONArray>('nodes', nil);
    if Assigned(JNodes) then
      for I := 0 to JNodes.Count - 1 do
      begin
        JNode     := JNodes.Items[I] as TJSONObject;
        NID       := JNode.GetValue<TJSONNumber>('id', TJSONNumber.Create(0)).AsInt;
        PlugTitle := JNode.GetValue<TJSONString>('plugin', TJSONString.Create('')).Value;
        NX        := JNode.GetValue<TJSONNumber>('x', TJSONNumber.Create(0)).AsDouble;
        NY        := JNode.GetValue<TJSONNumber>('y', TJSONNumber.Create(0)).AsDouble;

        // Encontra a classe do plugin pelo título
        PC := nil;
        for J := 0 to NodePluginRegistry.Count - 1 do
          if NodePluginRegistry.TitleOf(J) = PlugTitle then
          begin
            PC := NodePluginRegistry.Classes[J];
            Break;
          end;

        if PC = nil then
        begin
          FLogLines.Add('⚠  Plugin não encontrado: ' + PlugTitle + ' — nó ignorado');
          Continue;
        end;

        N := TNode.Create(NID, NX, NY, PC);

        // Restaura propriedades
        JProps := JNode.GetValue<TJSONObject>('props', nil);
        if Assigned(JProps) then
          for JPair in JProps do
            N.Plugin.Props.Values[JPair.JsonString.Value] :=
              (JPair.JsonValue as TJSONString).Value;

        FNodes.Add(N);
        if NID > MaxID then MaxID := NID;
      end;

    // Ligações
    JConns := Root.GetValue<TJSONArray>('connections', nil);
    if Assigned(JConns) then
      for I := 0 to JConns.Count - 1 do
      begin
        JConn := JConns.Items[I] as TJSONObject;
        NID   := JConn.GetValue<TJSONNumber>('id',     TJSONNumber.Create(0)).AsInt;
        SrcID := JConn.GetValue<TJSONNumber>('source', TJSONNumber.Create(0)).AsInt;
        TgtID := JConn.GetValue<TJSONNumber>('target', TJSONNumber.Create(0)).AsInt;
        FConnections.Add(TConnection.Create(NID, SrcID, TgtID));
        if NID > MaxID then MaxID := NID;
      end;

    FNextID      := MaxID + 1;
    FProjectFile := APath;
    UpdateTitleBar;
    UpdateZoomLabel;
    FLogLines.Add('📂  Carregado: ' + TPath.GetFileName(APath) +
      ' (' + IntToStr(FNodes.Count) + ' nós, ' +
      IntToStr(FConnections.Count) + ' ligações)');
    FitToNodes;
  finally
    Root.Free;
  end;
end;

// --- Handlers dos botões New / Save / Load ---
procedure TFormNodeEditor.BtnNewClick(Sender: TObject);
begin
  // Confirmação simples via log — pode ser substituída por TDialogService
  NewProject;
end;

procedure TFormNodeEditor.BtnSaveClick(Sender: TObject);
var
  SaveDlg: TSaveDialog;
begin
  if FProjectFile <> '' then
  begin
    SaveProject(FProjectFile);
    Exit;
  end;
  SaveDlg := TSaveDialog.Create(nil);
  try
    SaveDlg.Title      := 'Guardar Projecto';
    SaveDlg.Filter     := 'Node Editor Project (*.nep)|*.nep|JSON (*.json)|*.json';
    SaveDlg.DefaultExt := 'nep';
    if FProjectFile <> '' then
      SaveDlg.FileName := FProjectFile;
    if SaveDlg.Execute then
      SaveProject(SaveDlg.FileName);
  finally
    SaveDlg.Free;
  end;
end;

procedure TFormNodeEditor.BtnLoadClick(Sender: TObject);
var
  OpenDlg: TOpenDialog;
begin
  OpenDlg := TOpenDialog.Create(nil);
  try
    OpenDlg.Title  := 'Abrir Projecto';
    OpenDlg.Filter := 'Node Editor Project (*.nep)|*.nep|JSON (*.json)|*.json|Todos (*.*)|*.*';
    if OpenDlg.Execute then
      LoadProject(OpenDlg.FileName);
  finally
    OpenDlg.Free;
  end;
end;

// =============================================================================
//  RunFlow
// =============================================================================
procedure TFormNodeEditor.RunFlow;
var
  I    : Integer;
  N    : TNode;
  Input: TNodeData;
begin
  FLogLines.Add('');
  FLogLines.Add('▶  Executar fluxo — ' + FormatDateTime('hh:nn:ss', Now));

  // Zera estado de execução
  for I := 0 to FNodes.Count-1 do
  begin
    FNodes[I].HasRun   := False;
    FNodes[I].RunResult:= '';
    FNodes[I].RunError := '';
  end;

  // Executa a partir de nós Trigger (sem entradas)
  for I := 0 to FNodes.Count-1 do
  begin
    N := FNodes[I];
    if N.Plugin.GetMinInputs = 0 then
    begin
      FLogLines.Add('  ⚡ ' + N.Plugin.GetTitle);
      Input := NewNodeData;
      try
        ExecuteFrom(N, Input);
      finally
        FreeNodeData(Input);
      end;
    end;
  end;

  FLogLines.Add('✅  Concluído');
  FLogVisible := True;
  PaintBox.Redraw;
end;

function TFormNodeEditor.ExecuteFrom(ANode: TNode; const AInput: TNodeData): TNodeData;
var
  I      : Integer;
  Conn   : TConnection;
  Tgt    : TNode;
  NextOut: TNodeData;
begin
  Result := ANode.Execute(AInput);

  if not Result.Success then
  begin
    FLogLines.Add('    ❌ ' + ANode.Plugin.GetTitle + ': ' + Result.Error);
    Exit;
  end;

  FLogLines.Add('    ✓ ' + ANode.Plugin.GetTitle);

  // Propaga para nós ligados
  for I := 0 to FConnections.Count-1 do
  begin
    Conn := FConnections[I];
    if Conn.SourceID = ANode.ID then
    begin
      Tgt := NodeByID(Conn.TargetID);
      if Assigned(Tgt) and not Tgt.HasRun then
      begin
        NextOut := ExecuteFrom(Tgt, Result);
        FreeNodeData(NextOut);
      end;
    end;
  end;
end;

// =============================================================================
//  Teclado
// =============================================================================
procedure TFormNodeEditor.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
begin
  if Key = vkDelete then
  begin
    DeleteSelConn;
    DeleteSelNode;
  end;
  // Ctrl+Enter = Executar fluxo
  if (Key = vkReturn) and (ssCtrl in Shift) then
    RunFlow;
  // Ctrl+S = Guardar (Ctrl+Shift+S = Guardar Como)
  if (Key = Ord('S')) and (ssCtrl in Shift) then
  begin
    if (ssShift in Shift) or (FProjectFile = '') then
      BtnSaveClick(nil)
    else
      SaveProject(FProjectFile);
  end;
  // Ctrl+O = Abrir
  if (Key = Ord('O')) and (ssCtrl in Shift) then
    BtnLoadClick(nil);
  // Ctrl+N = Novo
  if (Key = Ord('N')) and (ssCtrl in Shift) then
    BtnNewClick(nil);
end;

// =============================================================================
//  PaintBoxDblClick
// =============================================================================
procedure TFormNodeEditor.PaintBoxDblClick(Sender: TObject);
begin
  // Tratado via detecção manual no MouseDown (mais fiável cross-platform)
end;

// =============================================================================
//  MouseDown
// =============================================================================
procedure TFormNodeEditor.PaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  HitNode  : TNode;
  HitConn  : TConnection;
  I, CatI  : Integer;
  Pt, CPt  : TPointF;   // Pt = ecrã, CPt = canvas
  IsDouble : Boolean;
  RowIdx   : Integer;
  SelCount : Integer;
begin
  Pt  := TPointF.Create(X, Y);

  // --- Botão do meio → iniciar pan ---
  if Button = TMouseButton.mbMiddle then
  begin
    FMode      := emPanning;
    FPanStart  := Pt;
    FPanOrigin := TPointF.Create(FPanX, FPanY);
    Exit;
  end;

  if Button <> TMouseButton.mbLeft then Exit;

  // --- Painel de propriedades ---
  if Assigned(FPropNode) and InPropPanel(X) then
  begin
    if PropCloseBtnRect.Contains(Pt) then begin ClosePropPanel; PaintBox.Redraw; Exit; end;
    if PropAddBtnRect.Contains(Pt)   then begin AddNewProp; Exit; end;
    RowIdx := PropRowAt(X, Y);
    if RowIdx >= 0 then EditPropAt(RowIdx) else ClosePropPanel;
    PaintBox.Redraw; Exit;
  end;

  // --- Paleta: cabeçalho de categoria ---
  CatI := PaletteCatHeaderAt(X, Y);
  if CatI >= 0 then
  begin
    FPalCats[CatI].Collapsed := not FPalCats[CatI].Collapsed;
    PaintBox.Redraw; Exit;
  end;

  // --- Paleta: item ---
  if InPalette(X) then
  begin
    I := PaletteItemAt(X, Y);
    if I >= 0 then AddNodeFromPaletteIdx(I);
    Exit;
  end;

  // Converte para coordenadas do canvas (considera zoom/pan)
  CPt := ScreenToCanvas(Pt);

  // --- Porta de saída → novo fio ---
  for I := FNodes.Count-1 downto 0 do
    if FNodes[I].HitOutputPort(CPt) then
    begin
      ClearSel; FMode := emDraggingWire;
      FWireSrc := FNodes[I]; FWirePos := CPt;
      PaintBox.Redraw; Exit;
    end;

  // --- Nó ---
  HitNode := NodeAt(CPt.X, CPt.Y);
  if Assigned(HitNode) then
  begin
    IsDouble := (FLastClickNode = HitNode) and
                ((System.SysUtils.Now - FLastClickTime) * 86400 < 0.4);
    FLastClickTime := System.SysUtils.Now;
    FLastClickNode := HitNode;

    if IsDouble then
    begin
      OpenPropPanel(HitNode); PaintBox.Redraw; Exit;
    end;

    // Shift+clique → adiciona/remove da seleção sem limpar
    if ssShift in Shift then
      HitNode.Selected := not HitNode.Selected
    else
    begin
      // Se o nó já estava selecionado, não limpa os outros (permite arrastar grupo)
      if not HitNode.Selected then
      begin
        ClearSel;
        HitNode.Selected := True;
      end;
    end;

    BringToFront(HitNode);

    // Prepara multi-drag: guarda offset de TODOS os nós selecionados
    SelCount := 0;
    for I := 0 to FNodes.Count-1 do
      if FNodes[I].Selected then Inc(SelCount);

    SetLength(FMultiDragOffsets, SelCount);
    SelCount := 0;
    for I := 0 to FNodes.Count-1 do
      if FNodes[I].Selected then
      begin
        FMultiDragOffsets[SelCount] :=
          TPointF.Create(CPt.X - FNodes[I].Bounds.Left,
                         CPt.Y - FNodes[I].Bounds.Top);
        Inc(SelCount);
      end;

    FMode     := emDraggingNode;
    FDragNode := HitNode;
    FDragOffX := CPt.X - HitNode.Bounds.Left;
    FDragOffY := CPt.Y - HitNode.Bounds.Top;
    PaintBox.Redraw; Exit;
  end;

  // --- Conexão ---
  HitConn := ConnAt(CPt.X, CPt.Y);
  if Assigned(HitConn) then
  begin
    ClearSel; HitConn.Selected := True; PaintBox.Redraw; Exit;
  end;

  // --- Vazio → iniciar rubber-band ---
  if not (ssShift in Shift) then
  begin
    ClosePropPanel;
    ClearSel;
  end;
  FMode        := emRubberBand;
  FRubberStart := CPt;
  FRubberEnd   := CPt;
  PaintBox.Redraw;
end;

// =============================================================================
//  MouseMove
// =============================================================================
procedure TFormNodeEditor.PaintBoxMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Single);
var
  Pt, CPt    : TPointF;
  I, SI      : Integer;
  NewX, NewY : Single;
  SelNode    : TNode;
  RubberRect : TRectF;
begin
  Pt  := TPointF.Create(X, Y);
  CPt := ScreenToCanvas(Pt);

  // Guarda posição do ecrã para o zoom com scroll
  FWirePos := Pt;

  FPaletteHover := PaletteItemAt(X, Y);
  if Assigned(FPropNode) and InPropPanel(X) then
    FPropHover := PropRowAt(X, Y)
  else
    FPropHover := -1;

  FHoverNode := nil;
  for I := FNodes.Count-1 downto 0 do
    if FNodes[I].HitInputPort(CPt) or FNodes[I].HitOutputPort(CPt) then
    begin FHoverNode := FNodes[I]; Break; end;

  case FMode of

    emPanning:
    begin
      FPanX := FPanOrigin.X + (X - FPanStart.X);
      FPanY := FPanOrigin.Y + (Y - FPanStart.Y);
      PaintBox.Redraw;
    end;

    emDraggingNode:
    begin
      // Move todos os nós selecionados mantendo offsets relativos
      SI := 0;
      for I := 0 to FNodes.Count-1 do
        if FNodes[I].Selected then
        begin
          if SI > High(FMultiDragOffsets) then Break;
          NewX := CPt.X - FMultiDragOffsets[SI].X;
          NewY := CPt.Y - FMultiDragOffsets[SI].Y;
          FNodes[I].Bounds := TRectF.Create(NewX, NewY, NewX+NODE_W, NewY+NODE_H);
          Inc(SI);
        end;
      PaintBox.Redraw;
    end;

    emDraggingWire:
    begin
      FWirePos    := CPt;
      FWireTarget := nil;
      for I := FNodes.Count-1 downto 0 do
        if FNodes[I].HitInputPort(CPt) and (FNodes[I] <> FWireSrc) then
        begin FWireTarget := FNodes[I]; Break; end;
      PaintBox.Redraw;
    end;

    emRubberBand:
    begin
      FRubberEnd := CPt;
      // Seleciona todos os nós dentro do retângulo (acumula com Shift)
      RubberRect := TRectF.Create(
        Min(FRubberStart.X, FRubberEnd.X), Min(FRubberStart.Y, FRubberEnd.Y),
        Max(FRubberStart.X, FRubberEnd.X), Max(FRubberStart.Y, FRubberEnd.Y));
      for I := 0 to FNodes.Count-1 do
        FNodes[I].Selected := RubberRect.IntersectsWith(FNodes[I].Bounds);
      PaintBox.Redraw;
    end;

  else
    PaintBox.Redraw;
  end;
end;

// =============================================================================
//  MouseUp
// =============================================================================
procedure TFormNodeEditor.PaintBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  Pt, CPt : TPointF;
  Tgt     : TNode;
  I       : Integer;
  Dup     : Boolean;
begin
  Pt  := TPointF.Create(X, Y);
  CPt := ScreenToCanvas(Pt);

  if Button = TMouseButton.mbMiddle then
  begin
    if FMode = emPanning then FMode := emIdle;
    Exit;
  end;

  if Button <> TMouseButton.mbLeft then Exit;

  case FMode of

    emDraggingWire:
    begin
      Tgt := nil;
      for I := FNodes.Count-1 downto 0 do
        if FNodes[I].HitInputPort(CPt) and (FNodes[I] <> FWireSrc) then
        begin Tgt := FNodes[I]; Break; end;
      if Assigned(Tgt) then
      begin
        Dup := False;
        for I := 0 to FConnections.Count-1 do
          if (FConnections[I].SourceID = FWireSrc.ID) and
             (FConnections[I].TargetID = Tgt.ID) then begin Dup := True; Break; end;
        if not Dup then
          FConnections.Add(TConnection.Create(GenID, FWireSrc.ID, Tgt.ID));
      end;
      FMode := emIdle; FWireSrc := nil; FWireTarget := nil;
      PaintBox.Redraw;
    end;

    emRubberBand:
    begin
      // Seleção já foi feita no MouseMove; apenas termina o modo
      FMode := emIdle;
      SetLength(FMultiDragOffsets, 0);
      PaintBox.Redraw;
    end;

  else
    FMode     := emIdle;
    FDragNode := nil;
    SetLength(FMultiDragOffsets, 0);
  end;
end;

// Scroll do rato → zoom
procedure TFormNodeEditor.PaintBoxMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
      if InPalette(0) then Exit;  // não faz nada se cursor na paleta
  ZoomAt(WheelDelta, FWirePos.X, FWirePos.Y);
  Handled := True;
end;

// =============================================================================
//  PaintBoxDraw — pipeline de renderização
// =============================================================================
procedure TFormNodeEditor.PaintBoxDraw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
var I: Integer;
begin
  ACanvas.Save;
  try
    DrawBG(ACanvas, ADest);

    // Aplica transformação zoom + pan ao canvas (tudo dentro deste bloco
    // fica em coordenadas do "mundo")
    ACanvas.Save;
    ACanvas.Translate(FPanX, FPanY);
    ACanvas.Scale(FZoom, FZoom);

    for I := 0 to FConnections.Count-1 do DrawConn(ACanvas, FConnections[I]);
    if FMode = emDraggingWire then DrawWireGhost(ACanvas);
    for I := 0 to FNodes.Count-1 do DrawNode(ACanvas, FNodes[I]);
    if FMode = emRubberBand then DrawRubberBand(ACanvas);

    ACanvas.Restore;  // fim da transformação zoom/pan

    // Elementos de UI fixos (não são afectados pelo zoom)
    DrawPalette(ACanvas, ADest);
    if Assigned(FPropNode) then DrawPropPanel(ACanvas, ADest);
    if FLogVisible then DrawLogPanel(ACanvas, ADest);
  finally
    ACanvas.Restore;
  end;
end;

// =============================================================================
//  DrawBG
// =============================================================================
procedure TFormNodeEditor.DrawBG(const C: ISkCanvas; const R: TRectF);
const GS=28; DR=1.2;
var Paint: ISkPaint; X, Y: Single; Area: TRectF;
begin
  Area := TRectF.Create(R.Left, R.Top, R.Right-FPaletteW, R.Bottom);
  Paint := TSkPaint.Create; Paint.Color := $FF1A1A2E;
  C.DrawRect(Area, Paint);
  Paint := TSkPaint.Create; Paint.AntiAlias := True; Paint.Color := $28FFFFFF;
  X := Area.Left+GS;
  while X < Area.Right do begin
    Y := Area.Top+GS;
    while Y < Area.Bottom do begin C.DrawCircle(X,Y,DR,Paint); Y:=Y+GS; end;
    X := X+GS;
  end;
end;

// =============================================================================
//  DrawConn
// =============================================================================
procedure TFormNodeEditor.DrawConn(const C: ISkCanvas; Conn: TConnection);
var Src,Dst: TNode; P0,P1,P2,P3: TPointF; PB: ISkPathBuilder;
    Path: ISkPath; Paint: ISkPaint; Shader: ISkShader;
begin
  Src := NodeByID(Conn.SourceID); Dst := NodeByID(Conn.TargetID);
  if (Src=nil) or (Dst=nil) then Exit;
  P0 := Src.OutputPort; P3 := Dst.InputPort;
  BezierCtrl(P0,P3,P1,P2);
  PB := TSkPathBuilder.Create;
  PB.MoveTo(P0.X,P0.Y); PB.CubicTo(P1.X,P1.Y,P2.X,P2.Y,P3.X,P3.Y);
  Path := PB.Detach;

  Paint := TSkPaint.Create; Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Stroke; Paint.StrokeCap := TSkStrokeCap.Round;
  if Conn.Selected then begin Paint.StrokeWidth := WIRE_GLOW_SEL; Paint.Color := $80FFD740; end
  else begin Paint.StrokeWidth := WIRE_GLOW; Paint.Color := $15FFFFFF; end;
  C.DrawPath(Path, Paint);

  Shader := TSkShader.MakeGradientLinear(P0, P3,
    [Src.Plugin.GetColor, Dst.Plugin.GetColor], nil, TSkTileMode.Clamp);
  Paint := TSkPaint.Create; Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Stroke; Paint.StrokeWidth := WIRE_W;
  Paint.StrokeCap := TSkStrokeCap.Round; Paint.Shader := Shader;
  C.DrawPath(Path, Paint);
end;

// =============================================================================
//  DrawWireGhost
// =============================================================================
procedure TFormNodeEditor.DrawWireGhost(const C: ISkCanvas);
var P0,P1,P2,P3: TPointF; PB: ISkPathBuilder; Path: ISkPath;
    Paint: ISkPaint; Shader: ISkShader;
begin
  if FWireSrc = nil then Exit;
  P0 := FWireSrc.OutputPort; P3 := FWirePos;
  BezierCtrl(P0,P3,P1,P2);
  PB := TSkPathBuilder.Create;
  PB.MoveTo(P0.X,P0.Y); PB.CubicTo(P1.X,P1.Y,P2.X,P2.Y,P3.X,P3.Y);
  Path := PB.Detach;

  Paint := TSkPaint.Create; Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Stroke; Paint.StrokeWidth := WIRE_GHOST;
  Paint.StrokeCap := TSkStrokeCap.Round; Paint.Color := $30FFFFFF;
  C.DrawPath(Path, Paint);

  Shader := TSkShader.MakeGradientLinear(P0, P3,
    [FWireSrc.Plugin.GetColor, $CCFFFFFF], nil, TSkTileMode.Clamp);
  Paint := TSkPaint.Create; Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Stroke; Paint.StrokeWidth := WIRE_W;
  Paint.StrokeCap := TSkStrokeCap.Round;
  Paint.PathEffect := TSkPathEffect.MakeDash([8,5], 0);
  Paint.Shader := Shader;
  C.DrawPath(Path, Paint);
end;

// =============================================================================
//  DrawNode
// =============================================================================
procedure TFormNodeEditor.DrawNode(const C: ISkCanvas; N: TNode);
const
  ICON_SIZE     = 18;
  ICON_PAD      = 8;
  ICON_TEXT_GAP = 6;
var
  Paint     : ISkPaint;
  Font      : ISkFont;
  Body, Hdr : TRectF;
  PropY     : Single;
  I         : Integer;
  HlIn, HlOut: Boolean;
  ColBg     : TAlphaColor;
  StatusText: string;
  SVGStr    : string;
  SVGBrush  : TSkSVGBrush;
  IconRect  : TRectF;
  TitleX    : Single;
begin
  Body := N.Bounds;
  Hdr  := TRectF.Create(Body.Left, Body.Top, Body.Right, Body.Top+HDR_H);

  HlIn  := (FMode = emDraggingWire) and (FWireTarget = N);
  HlOut := (FMode = emIdle) and (FHoverNode = N);

  // Sombra
  Paint := TSkPaint.Create; Paint.AntiAlias := True;
  Paint.Color := $60000000;
  Paint.ImageFilter := TSkImageFilter.MakeDropShadow(0,6,10,10,$80000000);
  C.DrawRoundRect(Body, CORNER_R, CORNER_R, Paint);

  // Corpo
  ColBg := $FF16213E;
  if N.HasRun then
    if N.RunError <> '' then ColBg := $FF2A0A0A
    else ColBg := $FF0A1A0A;
  Paint := TSkPaint.Create; Paint.AntiAlias := True; Paint.Color := ColBg;
  C.DrawRoundRect(Body, CORNER_R, CORNER_R, Paint);

  // Cabeçalho colorido
  Paint := TSkPaint.Create; Paint.AntiAlias := True; Paint.Color := N.Plugin.GetColor;
  C.Save;
  C.ClipRoundRect(TSkRoundRect.Create(Body, CORNER_R, CORNER_R));
  C.DrawRect(Hdr, Paint);
  C.Restore;

  // Ícone SVG + Título
  IconRect := TRectF.Create(
    Hdr.Left + ICON_PAD,
    Hdr.Top  + (HDR_H - ICON_SIZE) / 2,
    Hdr.Left + ICON_PAD + ICON_SIZE,
    Hdr.Top  + (HDR_H - ICON_SIZE) / 2 + ICON_SIZE);
  TitleX := IconRect.Right + ICON_TEXT_GAP;

  SVGStr := N.Plugin.GetIconSVG;
  if SVGStr <> '' then
  begin
    SVGBrush := TSkSVGBrush.Create;
    try
      SVGBrush.Source := SVGStr;
      C.Save;
      C.ClipRect(IconRect);
      SVGBrush.Render(C, IconRect, 1.0);
      C.Restore;
    finally
      SVGBrush.Free;
    end;
  end
  else
  begin
    // Fallback: emoji se não houver SVG
    Paint := TSkPaint.Create; Paint.AntiAlias := True; Paint.Color := COL_TEXT_PRIMARY;
    C.DrawSimpleText(N.Plugin.GetIconText, Hdr.Left + ICON_PAD,
      Hdr.Top + HDR_H/2 + 4, MakeFont(14), Paint);
  end;

  // Título
  Paint := TSkPaint.Create; Paint.AntiAlias := True; Paint.Color := COL_TEXT_PRIMARY;
  Font  := MakeFont(FONT_NODE_TITLE);
  C.DrawSimpleText(N.Plugin.GetTitle, TitleX, Hdr.Top + HDR_H/2 + 4, Font, Paint);

  // Propriedades (até 2 linhas visíveis)
  Paint := TSkPaint.Create; Paint.AntiAlias := True; Paint.Color := COL_TEXT_SECONDARY;
  Font  := MakeFont(FONT_NODE_PROP);
  PropY := Hdr.Bottom+10;
  for I := 0 to Min(N.Plugin.Props.Count-1, 1) do
  begin
    C.DrawSimpleText(N.Plugin.Props[I], Body.Left+10, PropY, Font, Paint);
    PropY := PropY + 13;
  end;

  // Status de execução
  if N.HasRun then
  begin
    Paint := TSkPaint.Create; Paint.AntiAlias := True;
    if N.RunError <> '' then
    begin
      Paint.Color := $FFFF5555;
      StatusText  := '❌ ' + N.RunError;
    end
    else
    begin
      Paint.Color := $FF55FF55;
      StatusText  := '✓ OK';
    end;
    Font := MakeFont(FONT_NODE_STATUS);
    C.DrawSimpleText(StatusText, Body.Left+8, Body.Bottom-6, Font, Paint);
  end;

  // Portas
  DrawPort(C, N.OutputPort, True,  HlOut);
  DrawPort(C, N.InputPort,  False, HlIn);

  // Borda de seleção
  Paint := TSkPaint.Create; Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Stroke; Paint.StrokeWidth := STROKE_W * 1.5;
  Paint.Color := ColIf(N.Selected, $FFFFD740, $30FFFFFF);
  C.DrawRoundRect(Body, CORNER_R, CORNER_R, Paint);
end;

// =============================================================================
//  DrawPort
// =============================================================================
procedure TFormNodeEditor.DrawPort(const C: ISkCanvas; const Pt: TPointF;
  IsOutput, Highlight: Boolean);
var Paint: ISkPaint; R: Single;
begin
  R := PORT_R; if Highlight then R := PORT_R+3;
  Paint := TSkPaint.Create; Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Stroke; Paint.StrokeWidth := STROKE_W * 2;
  Paint.Color := ColIf(Highlight, $FFFFD740, $DDFFFFFF);
  C.DrawCircle(Pt.X, Pt.Y, R, Paint);
  Paint := TSkPaint.Create; Paint.AntiAlias := True;
  Paint.Color := ColIf(IsOutput, $FF4FC3F7, $FF263859);
  C.DrawCircle(Pt.X, Pt.Y, R-2, Paint);
end;

// =============================================================================
//  DrawPalette — barra lateral direita com categorias colapsáveis
// =============================================================================
procedure TFormNodeEditor.DrawPalette(const C: ISkCanvas; const R: TRectF);
var
  PX         : Single;
  BgRect     : TRectF;
  Paint      : ISkPaint;
  TitleFont  : ISkFont;
  SubFont    : ISkFont;
  CatFont    : ISkFont;
  CatF       : ISkFont;
  LabelPn    : ISkPaint;
  SubPn      : ISkPaint;
  CurY       : Single;
  CatI       : Integer;
  Cat        : TNodeCategory;
  CatRect    : TRectF;
  ItemRect   : TRectF;
  Accent     : TRectF;
  NPlug      : Integer;
  Inst       : TNodePluginBase;
  II, J      : Integer;
  IsPH       : Boolean;
  PalIdx     : Integer;
  Arrow      : string;
  LY         : Single;
begin
  NPlug := NodePluginRegistry.Count;
  PX    := R.Right - FPaletteW;

  // Fundo
  BgRect := TRectF.Create(PX, R.Top, R.Right, R.Bottom);
  Paint  := TSkPaint.Create; Paint.Color := $FF0F0F23;
  C.DrawRect(BgRect, Paint);

  // Separador
  Paint := TSkPaint.Create; Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Stroke; Paint.StrokeWidth := STROKE_W;
  Paint.Color := $40FFFFFF;
  C.DrawLine(TPointF.Create(PX, R.Top), TPointF.Create(PX, R.Bottom), Paint);

  // "COMPONENTES"
  Paint := TSkPaint.Create; Paint.AntiAlias := True; Paint.Color := COL_TEXT_DISABLED;
  CatFont := MakeFont(FONT_PAL_HEADER);
  C.DrawSimpleText('COMPONENTES', PX+PAL_PAD, R.Top+22, CatFont, Paint);

  TitleFont := MakeFont(FONT_PAL_TITLE);
  SubFont   := MakeFont(FONT_PAL_DESC);
  LabelPn := TSkPaint.Create; LabelPn.AntiAlias := True; LabelPn.Color := COL_TEXT_PRIMARY;
  SubPn   := TSkPaint.Create; SubPn.AntiAlias   := True; SubPn.Color   := COL_TEXT_MUTED;

  CurY  := R.Top + 34;
  CatF  := MakeFont(FONT_PAL_CAT);

  for CatI := 0 to High(FPalCats) do
  begin
    Cat := FPalCats[CatI].Category;

    // Cabeçalho de categoria
    CatRect := TRectF.Create(PX+4, CurY, R.Right-4, CurY+CAT_HDR_H);
    FPalCats[CatI].YStart := CurY;

    Paint := TSkPaint.Create; Paint.AntiAlias := True; Paint.Color := $18FFFFFF;
    C.DrawRoundRect(CatRect, 4, 4, Paint);

    // Barra colorida da categoria
    Paint := TSkPaint.Create; Paint.AntiAlias := True; Paint.Color := CAT_COLORS[Cat];
    C.DrawRoundRect(TRectF.Create(PX+4, CurY, PX+8, CurY+CAT_HDR_H), 2, 2, Paint);

    // Label categoria + ícone colapsar
    Paint := TSkPaint.Create; Paint.AntiAlias := True; Paint.Color := COL_TEXT_PRIMARY;
    if FPalCats[CatI].Collapsed then Arrow := '▶' else Arrow := '▼';
    C.DrawSimpleText(NodeCategoryNames[Cat], PX+14, CurY+17, CatF, Paint);
    C.DrawSimpleText(Arrow, R.Right-18, CurY+17, CatF, Paint);

    CurY := CurY + CAT_HDR_H + 2;
    FPalCats[CatI].YEnd := CurY;

    if not FPalCats[CatI].Collapsed then
    begin
      // Itens desta categoria
      for II := 0 to NPlug-1 do
      begin
        if NodePluginRegistry.CategoryOf(II) <> Cat then Continue;

        // Encontra o FPalItems correspondente
        PalIdx := -1;
        for J := 0 to High(FPalItems) do
          if FPalItems[J].PluginIdx = II then begin PalIdx := J; Break; end;
        if PalIdx < 0 then Continue;

        ItemRect := TRectF.Create(PX+PAL_PAD, CurY, R.Right-PAL_PAD, CurY+PAL_ITEM_H);
        FPalItems[PalIdx].Rect := ItemRect;

        IsPH := (FPaletteHover = PalIdx);

        Paint := TSkPaint.Create; Paint.AntiAlias := True;
        Paint.Color := ColIf(IsPH, $25FFFFFF, $00000000);
        C.DrawRoundRect(ItemRect, 5, 5, Paint);

        // Acento
        Accent := TRectF.Create(ItemRect.Left, ItemRect.Top+8, ItemRect.Left+3, ItemRect.Bottom-8);
        Paint  := TSkPaint.Create; Paint.AntiAlias := True;
        Paint.Color := NodePluginRegistry.ColorOf(II);
        C.DrawRoundRect(Accent, 2, 2, Paint);

        // Conteúdo do item
        Inst := NodePluginRegistry.CreateInstance(II);
        try
          LY := ItemRect.Top + 16;
          C.DrawSimpleText(Inst.GetIconText + '  ' + Inst.GetTitle,
            ItemRect.Left+8, LY, TitleFont, LabelPn);
          if Inst.GetDescription <> '' then
            C.DrawSimpleText(Inst.GetDescription, ItemRect.Left+8, LY+14, SubFont, SubPn);
          C.DrawSimpleText('↑ inserir', ItemRect.Left+8, ItemRect.Bottom-5, SubFont, SubPn);
        finally
          Inst.Free;
        end;

        CurY := CurY + PAL_ITEM_H + 3;
      end;
    end
    else
    begin
      // Quando colapsado, anula os rects dos itens desta categoria
      for II := 0 to NPlug-1 do
        if NodePluginRegistry.CategoryOf(II) = Cat then
          for J := 0 to High(FPalItems) do
            if FPalItems[J].PluginIdx = II then
              FPalItems[J].Rect := TRectF.Empty;
    end;

    CurY := CurY + 4;
  end;
end;

// =============================================================================
//  DrawPropPanel
// =============================================================================
procedure TFormNodeEditor.DrawPropPanel(const C: ISkCanvas; const R: TRectF);
const ROW_PAD = 8;
var
  PW         : Single;
  BgRect     : TRectF;
  Paint      : ISkPaint;
  TitleFont  : ISkFont;
  LabelFont  : ISkFont;
  ValueFont  : ISkFont;
  HintFont   : ISkFont;
  TitlePn    : ISkPaint;
  LabelPn    : ISkPaint;
  ValuePn    : ISkPaint;
  HintPn     : ISkPaint;
  RowRect    : TRectF;
  AccentRect : TRectF;
  CloseRect  : TRectF;
  AddRect    : TRectF;
  I          : Integer;
  Key, Val   : string;
  KeyLabel   : string;
  KindShow   : string;
  Desc       : string;
  LY         : Single;
  Defs       : TArray<TNodePropertyDef>;
  Def        : TNodePropertyDef;
  HasDef     : Boolean;
  J          : Integer;
  KindStr    : string;
begin
  if not Assigned(FPropNode) then Exit;

  PW     := PropPanelW;
  BgRect := TRectF.Create(0, 0, PW, R.Bottom);

  // Sombra
  Paint := TSkPaint.Create; Paint.AntiAlias := True; Paint.Color := $00000000;
  Paint.ImageFilter := TSkImageFilter.MakeDropShadow(4,0,12,12,$60000000);
  C.DrawRect(BgRect, Paint);

  // Fundo
  Paint := TSkPaint.Create; Paint.Color := $F20D0D1F;
  C.DrawRect(BgRect, Paint);

  // Borda direita
  Paint := TSkPaint.Create; Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Stroke; Paint.StrokeWidth := STROKE_W;
  Paint.Color := $50FFFFFF;
  C.DrawLine(TPointF.Create(PW,0), TPointF.Create(PW,R.Bottom), Paint);

  // Fontes
  TitleFont := MakeFont(FONT_PROP_TITLE);
  LabelFont := MakeFont(FONT_PROP_LABEL);
  ValueFont := MakeFont(FONT_PROP_VALUE);
  HintFont  := MakeFont(FONT_PROP_HINT);

  TitlePn := TSkPaint.Create; TitlePn.AntiAlias := True; TitlePn.Color := COL_TEXT_PRIMARY;
  LabelPn := TSkPaint.Create; LabelPn.AntiAlias := True; LabelPn.Color := COL_TEXT_SECONDARY;
  ValuePn := TSkPaint.Create; ValuePn.AntiAlias := True; ValuePn.Color := COL_TEXT_PRIMARY;
  HintPn  := TSkPaint.Create; HintPn.AntiAlias  := True; HintPn.Color  := COL_TEXT_MUTED;

  // Barra de cor do nó
  Paint := TSkPaint.Create; Paint.Color := FPropNode.Plugin.GetColor;
  C.DrawRect(TRectF.Create(0,0,PW,4), Paint);

  // Ícone + tipo
  C.DrawSimpleText(FPropNode.Plugin.GetIconText + '  PROPRIEDADES', ROW_PAD, 24, LabelFont, LabelPn);

  // Título
  C.DrawSimpleText(FPropNode.Plugin.GetTitle, ROW_PAD, 46, TitleFont, TitlePn);

  // Info
  C.DrawSimpleText(IntToStr(FPropNode.Plugin.Props.Count) +
    ' campo(s)  •  clique para editar', ROW_PAD, 62, HintFont, HintPn);

  // Separador
  Paint := TSkPaint.Create; Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Stroke; Paint.StrokeWidth := STROKE_W; Paint.Color := $30FFFFFF;
  C.DrawLine(TPointF.Create(ROW_PAD,74), TPointF.Create(PW-ROW_PAD,74), Paint);

  // Botão fechar
  CloseRect := PropCloseBtnRect;
  Paint := TSkPaint.Create; Paint.AntiAlias := True; Paint.Color := $20FFFFFF;
  C.DrawRoundRect(CloseRect, 4, 4, Paint);
  C.DrawSimpleText('×', CloseRect.Left+7, CloseRect.Bottom-6,
    MakeFont(FONT_ICON_CLOSE), TitlePn);

  // Obtém definições para tipo amigável
  Defs := FPropNode.Plugin.GetPropertyDefs;

  // Linhas de propriedades
  for I := 0 to FPropNode.Plugin.Props.Count-1 do
  begin
    Key     := FPropNode.Plugin.Props.Names[I];
    Val     := FPropNode.Plugin.Props.ValueFromIndex[I];
    RowRect := PropRowRect(I);
    if RowRect.Bottom < 0 then Continue;
    if RowRect.Top > R.Bottom then Break;

    // Procura definição
    HasDef := False; KindStr := '';
    for J := 0 to High(Defs) do
      if Defs[J].Name = Key then
      begin
        Def := Defs[J]; HasDef := True;
        case Def.Kind of
          npkPassword: Val := StringOfChar('•', Length(Val));
          npkCode    : Val := '{ código }';
          npkBoolean : KindStr := 'bool';
          npkInteger : KindStr := 'int';
          npkFloat   : KindStr := 'float';
          npkSelect  : KindStr := 'select';
        end;
        Break;
      end;

    // Fundo linha
    Paint := TSkPaint.Create; Paint.AntiAlias := True;
    Paint.Color := ColIf(I = FPropHover, $28FFFFFF, $12FFFFFF);
    C.DrawRoundRect(RowRect, 5, 5, Paint);

    // Acento colorido
    AccentRect := TRectF.Create(RowRect.Left, RowRect.Top+6, RowRect.Left+3, RowRect.Bottom-6);
    Paint := TSkPaint.Create; Paint.AntiAlias := True; Paint.Color := FPropNode.Plugin.GetColor;
    C.DrawRoundRect(AccentRect, 2, 2, Paint);

    LY := RowRect.Top + 16;

    // Chave + tipo
    KeyLabel := Key;
    if HasDef then KeyLabel := Def.Label_;

    if KindStr <> '' then
      KindShow := ' [' + KindStr + ']'
    else
      KindShow := '';

   // KindShow := IfThen(KindStr <> '', ' [' + KindStr + ']', '');
    C.DrawSimpleText(KeyLabel + KindShow, RowRect.Left+8, LY, LabelFont, LabelPn);

    // Valor
    C.DrawSimpleText(Val, RowRect.Left+8, LY+18, ValueFont, ValuePn);

    // Ícone de edição
    if I = FPropHover then
      C.DrawSimpleText('✎', RowRect.Right-18, LY+10,
        MakeFont(FONT_ICON_EDIT), HintPn);
  end;

  // Botão "+ Adicionar campo"
  AddRect := PropAddBtnRect;
  if not AddRect.IsEmpty then
  begin
    Paint := TSkPaint.Create; Paint.AntiAlias := True; Paint.Color := $15FFFFFF;
    C.DrawRoundRect(AddRect, 5, 5, Paint);
    C.DrawSimpleText('+ Adicionar campo', AddRect.Left+8, AddRect.Top+20, LabelFont, HintPn);
  end;

  // Descrição do plugin (fundo do painel)
  Desc := FPropNode.Plugin.GetDescription;
  if Desc <> '' then
  begin
    LY := AddRect.Bottom + 12;
    Paint := TSkPaint.Create; Paint.AntiAlias := True; Paint.Color := $30FFFFFF;
    C.DrawLine(TPointF.Create(ROW_PAD, LY-6), TPointF.Create(PW-ROW_PAD, LY-6), Paint);
    C.DrawSimpleText(Desc, ROW_PAD, LY+8, HintFont, HintPn);
  end;
end;

// =============================================================================
//  DrawLogPanel
// =============================================================================
procedure TFormNodeEditor.DrawLogPanel(const C: ISkCanvas; const R: TRectF);
var
  LY, PX, LH: Single;
  Paint      : ISkPaint;
  Font       : ISkFont;
  FP         : ISkPaint;
  I          : Integer;
  BgRect     : TRectF;
  Line       : string;
  Col        : TAlphaColor;
begin
  LH    := FLogH;
  if Assigned(FPropNode) then PX := PropPanelW else PX := 0;
  BgRect := TRectF.Create(PX, R.Bottom-LH, R.Right-FPaletteW, R.Bottom);

  Paint := TSkPaint.Create; Paint.Color := $E0080814;
  C.DrawRect(BgRect, Paint);

  Paint := TSkPaint.Create; Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Stroke; Paint.StrokeWidth := STROKE_W;
  Paint.Color := $40FFFFFF;
  C.DrawLine(TPointF.Create(BgRect.Left, BgRect.Top),
             TPointF.Create(BgRect.Right, BgRect.Top), Paint);

  // Header
  FP := TSkPaint.Create; FP.AntiAlias := True; FP.Color := COL_TEXT_DISABLED;
  Font := MakeFont(FONT_LOG);
  C.DrawSimpleText('LOG DE EXECUÇÃO', BgRect.Left+10, BgRect.Top+16, Font, FP);

  // Linhas do log (as últimas que cabem)
  Font := MakeFont(FONT_LOG);
  LY := BgRect.Bottom - 10;
  for I := FLogLines.Count-1 downto 0 do
  begin
    if LY < BgRect.Top + 24 then Break;
    Line := FLogLines[I];
    if Line.Contains('❌') then Col := $FFFF6666
    else if Line.Contains('✅') or Line.Contains('✓') then Col := $FF66FF66
    else if Line.Contains('▶') then Col := $FF88AAFF
    else Col := $CCFFFFFF;
    FP := TSkPaint.Create; FP.AntiAlias := True; FP.Color := Col;
    C.DrawSimpleText(Line, BgRect.Left+10, LY, Font, FP);
    LY := LY - 16;
  end;
end;

// =============================================================================
//  DrawRubberBand — retângulo de seleção fantasma
// =============================================================================
procedure TFormNodeEditor.DrawRubberBand(const C: ISkCanvas);
var
  R     : TRectF;
  Paint : ISkPaint;
begin
  R := TRectF.Create(
    Min(FRubberStart.X, FRubberEnd.X), Min(FRubberStart.Y, FRubberEnd.Y),
    Max(FRubberStart.X, FRubberEnd.X), Max(FRubberStart.Y, FRubberEnd.Y));

  if (R.Width < 2) and (R.Height < 2) then Exit;

  // Preenchimento semi-transparente
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Color     := $202979FF;  // Azul muito transparente
  C.DrawRect(R, Paint);

  // Borda tracejada
  Paint := TSkPaint.Create;
  Paint.AntiAlias   := True;
  Paint.Style       := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := STROKE_W * 1.5 / FZoom;  // Mantém espessura visual constante
  Paint.Color       := $CC4FC3F7;
  Paint.PathEffect  := TSkPathEffect.MakeDash([6/FZoom, 3/FZoom], 0);
  C.DrawRect(R, Paint);
end;

end.
