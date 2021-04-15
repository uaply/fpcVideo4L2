{$mode objfpc}{$h+}
unit RGB24writer;

interface

uses FPImage, classes, sysutils, BMPComn;

type
  PFastBitmapPixelComponents24Bit = ^TFastBitmapPixelComponents24Bit;
  TFastBitmapPixelComponents24Bit = packed record
    Blue: 0..255; // 8 bits
    Green: 0..255; // 8 bits
    Red: 0..255; // 8 bits
  end;

  TFPWriterRGB24 = class (TFPCustomImageWriter)
  private
    FBpp:byte;
  protected
    procedure InternalWrite (Stream:TStream; Img: TFPCustomImage); override;
  public
    constructor Create; override;
    property Bpp:byte read FBpp;
  end;


implementation

Function FPColorToRGB(Const Color : TFPColor) : TColorRGB;
begin
  With Result,Color do
  begin
    R:=(Red   and $FF00) shr 8;
    G:=(Green and $FF00) shr 8;
    B:=(Blue  and $FF00) shr 8;
  end;
end;

constructor TFPWriterRGB24.create;
begin
  inherited Create;
  FBpp:=3;
end;

procedure TFPWriterRGB24.InternalWrite (Stream:TStream; Img:TFPCustomImage);
var
  Row,Col,RowSize, i:Integer;
  aLine : PByte;
  PadCount : byte;
begin
  RowSize:=Img.Width*FBpp;
  PadCount:=(4-(RowSize mod 4)) mod 4; { every row must end on 4 byte boundary }
  inc(RowSize,PadCount);

  GetMem(aLine,RowSize);
  try
    for Row:=0 to Img.Height-1 do
    begin
      for Col:=0 to img.Width-1 do
        PColorRGB(aLine)[Col]:=FPColorToRGB(Img.colors[Col,Row]);
      { pad the scanline with zeros }
      for i:=RowSize-PadCount to RowSize-1 do Pbyte(aline)[i]:=0;
      Stream.Write(aLine[0],RowSize);
     end;
  finally
    FreeMem(aLine);
  end;
end;

end.
