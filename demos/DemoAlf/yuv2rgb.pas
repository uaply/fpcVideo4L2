{
  YUYV to BGRA conversion routines
  
  Copyright (C) 2013 Yuriy Pilgun

  This program is provided under the terms of the MIT License.
}
unit YUV2RGB;

{$mode objfpc}{$H+}

interface

{ 
  Conversion coefficients kRV, kGU, kGV, kBU are guessed without deep investigation.
  Values of YUV components assumed to extend to full range 0..255 / -128..127.
  
  If you want faster conversion, see:
    Etienne Dupuis. "Optimizing YUV-RGB Color Space Conversion Using Intel's SIMD Technology," August 2003.
  Or adapt MMX code from FFmpeg/libswscale library.
}
procedure YUYV_to_BGRA(src: PLongWord; dest: PLongWord; size: integer);
procedure YUYV_to_BGRA_Slow(src: PLongWord; dest: PLongWord; size: integer);
procedure YUYV_to_BGRA16(src: PLongWord; dest: PWord; size: integer);
procedure YUYV_to_BGRA_Slow16(src: PLongWord; dest: PWord; size: integer);
procedure YUYV_to_Gray(src: PLongWord; dest: PLongWord; size: integer);
procedure YUYV_to_Gray16(src: PLongWord; dest: PWord; size: integer);
procedure BGR32_to_BGR16(src: PLongWord; dest: PWord; size: integer);

implementation

const
  Alpha=$FF000000;

const
  kRV=1.402;
  kGU=-0.34414;
  kGV=-0.71414;
  kBU=1.772;

var
  coeffRV: array [0..255] of SmallInt;
  coeffGU: array [0..255] of SmallInt;
  coeffGV: array [0..255] of SmallInt;
  coeffBU: array [0..255] of SmallInt;

const
  offsetC = 320;

var
  coeffR: array[0..1023] of LongWord;
  coeffG: array[0..1023] of LongWord;
  coeffB: array[0..1023] of LongWord;

  coeffR16: array[0..1023] of Word;
  coeffG16: array[0..1023] of Word;
  coeffB16: array[0..1023] of Word;

const
  rv=round(kRV*65536);
  gu=round(kGU*65536);
  gv=round(kGV*65536);
  bu=round(kBU*65536);

procedure InitializeTables;
var
  i:integer;
begin
  for i:=0 to 255 do begin
    coeffRV[i]:=integer(rv)*(i-128) shr 16;
    coeffGU[i]:=integer(gu)*(i-128) shr 16;
    coeffGV[i]:=integer(gv)*(i-128) shr 16;
    coeffBU[i]:=integer(bu)*(i-128) shr 16;
  end;
  for i:=0 to 1023 do begin
    if i<offsetC then begin
      coeffR[i]:=0 or Alpha;
      coeffR16[i]:=0;
      coeffG[i]:=0 or Alpha;
      coeffG16[i]:=0;
      coeffB[i]:=0 or Alpha;
      coeffB16[i]:=0;
    end else
    if i>offsetC+255 then begin
      coeffR[i]:=$FF0000 or Alpha;
      coeffR16[i]:=$F800;
      coeffG[i]:=$00FF00 or Alpha;
      coeffG16[i]:=$07E0;
      coeffB[i]:=$0000FF or Alpha;
      coeffB16[i]:=$001F;
    end else begin
      coeffR[i]:=(i-offsetC) shl 16 or Integer(Alpha);
      coeffR16[i]:=((i-offsetC) >> 3) shl 11;
      coeffG[i]:=(i-offsetC) shl 8 or Integer(Alpha);
      coeffG16[i]:=((i-offsetC) >> 2) shl 5;
      coeffB[i]:=(i-offsetC) or Integer(Alpha);
      coeffB16[i]:=((i-offsetC) >> 3);
    end;
  end;
end;


procedure YUYV_to_BGRA(src: PLongWord; dest: PLongWord; size: integer);
var
  i:integer;
  a:integer;
  Y1,U,Y2,V: integer;
  r0,g0,b0: integer;
begin
  for i := 0 to (size div 2) - 1 do begin
    a:=src^;

    Y1:=(a) and $FF;
    U:=(a shr 8) and $FF;
    Y2:=(a shr 16) and $FF;
    V:=(a shr 24) and $FF;

    r0:=coeffRV[V];
    g0:=coeffGU[U]+coeffGV[V];
    b0:=coeffBU[U];

    dest^:=coeffR[Y1+r0+offsetC] or coeffG[Y1+g0+offsetC] or coeffB[Y1+b0+offsetC];
    Inc(dest);

    dest^:=coeffR[Y2+r0+offsetC] or coeffG[Y2+g0+offsetC] or coeffB[Y2+b0+offsetC];
    Inc(dest);

    Inc(src);
  end;
end;

procedure YUYV_to_BGRA16(src: PLongWord; dest: PWord; size: integer);
var
  i:integer;
  a:integer;
  Y1,U,Y2,V: integer;
  r0,g0,b0: integer;
begin
  for i := 0 to (size div 2) - 1 do begin
    a:=src^;

    Y1:=(a) and $FF;
    U:=(a shr 8) and $FF;
    Y2:=(a shr 16) and $FF;
    V:=(a shr 24) and $FF;

    r0:=coeffRV[V];
    g0:=coeffGU[U]+coeffGV[V];
    b0:=coeffBU[U];

    dest^:=coeffR16[Y1+r0+offsetC] or coeffG16[Y1+g0+offsetC] or coeffB16[Y1+b0+offsetC];
    Inc(dest);

    dest^:=coeffR16[Y2+r0+offsetC] or coeffG16[Y2+g0+offsetC] or coeffB16[Y2+b0+offsetC];
    Inc(dest);

    Inc(src);
  end;
end;

procedure YUYV_to_BGRA_Slow(src: PLongWord; dest: PLongWord; size: integer);
var
  i:integer;
  a:integer;
  Y1,U,Y2,V: integer;
  r0,g0,b0: integer;
  R,G,B: integer;
begin
  for i := 0 to (size div 2) - 1 do begin
    a:=src^;
    Y1:=(a) and $FF;
    U:=(a shr 8) and $FF  - 128;
    Y2:=(a shr 16) and $FF;
    V:=(a shr 24) and $FF  - 128;
    r0:=(rv*V shr 16);
    g0:=(gu*U shr 16)+(gv*V shr 16);
    b0:=(bu*U shr 16);
    R:=Y1+r0;
    G:=Y1+g0;
    B:=Y1+b0;
    if R<0 then R:=0 else if R>255 then R:=255;
    if G<0 then G:=0 else if G>255 then G:=255;
    if B<0 then B:=0 else if B>255 then B:=255;
    dest^:=(R shl 16) or (G shl 8) or (B) or Integer(Alpha);

    Inc(dest);
    R:=Y2+r0;
    G:=Y2+g0;
    B:=Y2+b0;
    if R<0 then R:=0 else if R>255 then R:=255;
    if G<0 then G:=0 else if G>255 then G:=255;
    if B<0 then B:=0 else if B>255 then B:=255;
    dest^:=(R shl 16) or (G shl 8) or (B) or Integer(Alpha);

    Inc(dest);
    Inc(src);
  end;
end;

procedure YUYV_to_BGRA_Slow16(src: PLongWord; dest: PWord; size: integer);
var
  i:integer;
  a:integer;
  Y1,U,Y2,V: integer;
  r0,g0,b0: integer;
  R,G,B: integer;
begin
  for i := 0 to (size div 2) - 1 do begin
    a:=src^;
    Y1:=(a) and $FF;
    U:=(a shr 8) and $FF  - 128;
    Y2:=(a shr 16) and $FF;
    V:=(a shr 24) and $FF  - 128;
    r0:=(rv*V shr 16);
    g0:=(gu*U shr 16)+(gv*V shr 16);
    b0:=(bu*U shr 16);
    R:=Y1+r0;
    G:=Y1+g0;
    B:=Y1+b0;
    if R<0 then R:=0 else if R>255 then R:=255;
    if G<0 then G:=0 else if G>255 then G:=255;
    if B<0 then B:=0 else if B>255 then B:=255;
    R := R >> 3;
    G := G >> 2;
    B := B >> 3;
    dest^:=(R shl 11) or (G shl 5) or (B);

    Inc(dest);
    R:=Y2+r0;
    G:=Y2+g0;
    B:=Y2+b0;
    if R<0 then R:=0 else if R>255 then R:=255;
    if G<0 then G:=0 else if G>255 then G:=255;
    if B<0 then B:=0 else if B>255 then B:=255;
    R := R >> 3;
    G := G >> 2;
    B := B >> 3;
    dest^:=(R shl 11) or (G shl 5) or (B);
    Inc(dest);
    Inc(src);
  end;
end;


procedure YUYV_to_Gray(src: PLongWord; dest: PLongWord; size: integer);
var i, b: integer;
begin
  for i := 0 to (size div 2) - 1 do begin
    b := src^ and $FF;
    dest^ := (b shl 16) or (b shl 8) or (b) or Integer(Alpha);
    Inc(dest);
    b := (src^ shr 16) and $FF;
    dest^ := (b shl 16) or (b shl 8) or (b) or Integer(Alpha);
    Inc(dest);
    Inc(src);
  end;
end;

procedure YUYV_to_Gray16(src: PLongWord; dest: PWord; size: integer);
var i, b, g: integer;
begin
  for i := 0 to (size div 2) - 1 do begin
    g := ((src^ and $FF) >> 2);
    b := g >> 1;
    dest^ := (b shl 11) or (g shl 5) or (b);
    Inc(dest);
    g := (((src^ shr 16) and $FF) >> 2);
    b := g >> 1;
    dest^ := (b shl 11) or (g shl 5) or (b);
    Inc(dest);
    Inc(src);
  end;
end;

procedure BGR32_to_BGR16(src: PLongWord; dest: PWord; size: integer);
var
  i:integer;
  a:integer;
  R,G,B:integer;
begin
  for i := 0 to (size - 1) do begin
    a:=src^;
    R:=(a shr 16) and $FF;
    G:=(a shr 8) and $FF;
    B:=(a shr 0) and $FF;
    R := R >> 3;
    G := G >> 2;
    B := B >> 3;
    dest^:=(R shl 11) or (G shl 5) or (B);
    Inc(dest);
    Inc(src);
  end;
end;



initialization
  InitializeTables;

end.

