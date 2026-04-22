unit QRVclUtils;

(* ===========================================================================
  QRVclUtils — Helpers específicos do ecossistema VCL (Windows only).

  Esta unit serve como ponte entre o mundo VCL (TColor, TColorDialog,
  TPanel.Color, etc.) e o core do renderer (QRRenderer) que só conhece
  TAlphaColor.

  · O ficheiro inteiro é envolvido por {$IFDEF MSWINDOWS} para que
    possa aparecer no source tree de projectos multi-plataforma sem
    quebrar a compilação em Linux/macOS/iOS/Android.

  · Não precisa ser incluído em builds FMX/consola — nesses casos os
    callers passam TAlphaColor directamente para TQRCodeRequest /
    TQROptions.
  =========================================================================== *)

interface

{$IFDEF MSWINDOWS}

uses
  System.UITypes,
  Vcl.Graphics,
  Winapi.Windows;

{ Converte um TColor (VCL, formato COLORREF) para TAlphaColor (RGBA com
  alpha totalmente opaco). Resolve automaticamente cores do sistema
  (clBtnFace, clWindow, etc.) via ColorToRGB. }
function VclColorToAlpha(C: TColor): TAlphaColor;

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

function VclColorToAlpha(C: TColor): TAlphaColor;
var
  CRGB: COLORREF;
begin
  CRGB   := ColorToRGB(C);
  Result := $FF000000
           or (TAlphaColor(GetRValue(CRGB)) shl 16)
           or (TAlphaColor(GetGValue(CRGB)) shl  8)
           or  TAlphaColor(GetBValue(CRGB));
end;

{$ENDIF}

end.
