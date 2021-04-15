{$mode objfpc}{$h+}
unit RGB32writer;

interface

uses FPImage, classes, sysutils, BMPComn;

type
  PFastBitmapPixelComponents32Bit = ^TFastBitmapPixelComponents32Bit;
  TFastBitmapPixelComponents32Bit = packed record
    Blue: 0..255; // 8 bits
    Green: 0..255; // 8 bits
    Red: 0..255; // 8 bits
    Alfa: 0..255; // 8 bits
  end;

  TFPWriterRGB32 = class (TFPCustomImageWriter)
  private
    FBpp:byte;
  protected
    procedure InternalWrite (Stream:TStream; Img: TFPCustomImage); override;
  public
    constructor Create; override;
    property Bpp:byte read FBpp;
  end;


implementation

constructor TFPWriterRGB32.create;
begin
  inherited Create;
  FBpp:=4;
end;

procedure TFPWriterRGB32.InternalWrite (Stream:TStream; Img:TFPCustomImage);
var
  Row,Col,RowSize, i:Integer;
  aLine : PByte;
  tmpcol : TColorRGBA;
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
      begin
        with tmpcol,img.colors[Col,Row] do
        begin
          R:=(Red   and $FF00) shr 8;
          G:=(Green and $FF00) shr 8;
          B:=(Blue  and $FF00) shr 8;
        end;
        PColorRGBA(aline)[Col]:=TColorRGBA( (tmpcol.R shl 16) or (tmpcol.G shl 8) or (tmpcol.B shl 0) );
      end;
      { pad the scanline with zeros }
      for i:=RowSize-PadCount to RowSize-1 do Pbyte(aline)[i]:=0;
      Stream.Write(aLine[0],RowSize);
     end;
  finally
    FreeMem(aLine);
  end;
end;

end.
