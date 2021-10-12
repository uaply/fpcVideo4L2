unit SRGGB82RGB;
//https://forum.lazarus.freepascal.org/index.php?topic=48598.0

{$mode objfpc}{$H+}
{$O+}

interface

uses
  Classes, SysUtils;

// Conversation from Bayer 8 Bit see http://www.siliconimaging.com/RGB%20Bayer.htm
//
// Attention only BGR not BGRA !!!! This is not compatible  with Linux TBitmap.RawImage
procedure SRGGB8_to_BGR(src: PByte; dest: PByte; w, h: integer); {  8  RGRG.. GBGB.. *}
// Attention only BGR not BGRA !!!! This is not compatible  with Linux TBitmap.RawImage
procedure SGBRG8_to_BGR(src: PByte; dest: PByte; w, h: integer); {  8  GBGB.. RGRG.. *}


implementation

uses
  ctypes;

type
  cvsize = packed record
    width : cint;
    height: cint;
  end;
  cvstatus=cint;
  pbbyte= pbyte;

function icvBayer2BGR_8u_C1C3R(
     bayer0:pbbyte;
     bayer_step:cint;
     dst0:pbbyte;
     dst_step:cint;
     size:cvsize;
     blue,
     start_with_green:cint):cvstatus;
var
    t0,t1 :cint;
    bayer_end,
    bayer,dst : pbbyte;
    start, cnt: LongInt;
label 123;
begin
  Result:= 0;
  // Destination clear
  // wide of picture * RGB * size of byte
  cnt:= size.width*3*sizeof(dst0[0]);
  // heighth of picture minus one * size of one line
  start:= (size.height - 1)*dst_step;
  //
  fillchar( dst0^, cnt, #0);
  fillchar( (dst0 + start)^, cnt ,#0 );
//    memset( dst0, 0, size.width*3*sizeof(dst0[0]) );
//    memset( dst0 + (size.height - 1)*dst_step, 0, size.width*3*sizeof(dst0[0]) );
  inc(dst0 ,dst_step + 3 + 1);
  dec(size.height , 2);
  dec(size.width , 2);
  repeat
//    for( ; size.height-- > 0; bayer0 += bayer_step, dst0 += dst_step )
    bayer:=bayer0;
    dst:=dst0;
    bayer_end:=bayer+size.width;
{       const uchar* bayer = bayer0;
      uchar* dst = dst0;
      const uchar* bayer_end = bayer + size.width;}
    dst[-4] :=0;
    dst[-3] :=0;
    dst[-2] :=0;
    dst[size.width*3-1] :=0;
    dst[size.width*3]   :=0;
    dst[size.width*3+1] :=0;
    if( size.width <= 0 ) then
        goto 123; // continue; // can't use contineu because of decrement operator abuse + for
    if( start_with_green )<>0 then
     begin
        t0 := (ord(bayer[1]) + ord(bayer[bayer_step*2+1]) + 1) shr {>>} 1;
        t1 := (ord(bayer[bayer_step]) + ord(bayer[bayer_step+2]) + 1) shr {>>} 1;
        dst[-blue] := t0;
        dst[0] := bayer[bayer_step+1];
        dst[blue] := (t1);
        inc(bayer);
        inc(dst, 3);
     end;

    if( blue > 0 ) then
     begin
        repeat
//           for( ; bayer <= bayer_end - 2; bayer += 2, dst += 6 )

            t0 := (ord(bayer[0]) + ord(bayer[2]) + ord(bayer[bayer_step*2]) +
                  ord(bayer[bayer_step*2+2]) + 2) shr {>>} 2;
            t1 := (ord(bayer[1]) + ord(bayer[bayer_step]) +
                  ord(bayer[bayer_step+2]) + ord(bayer[bayer_step*2+1])+2) shr {>>} 2;
            dst[-1] := (t0);
            dst[0] := (t1);
            dst[1] := bayer[bayer_step+1];

            t0 := (ord(bayer[2]) + ord(bayer[bayer_step*2+2]) + 1) shr {>>} 1;
            t1 := (ord(bayer[bayer_step+1]) + ord(bayer[bayer_step+3]) + 1) shr {>>} 1;
            dst[2] := (t0);
            dst[3] := bayer[bayer_step+2];
            dst[4] := (t1);

          inc(bayer,2);
          inc(dst,6);
        until bayer>(bayer_end-2);
     end
    else
     begin
//            for( ; bayer <= bayer_end - 2; bayer += 2, dst += 6 )            {\
       repeat
            t0 := (ord(bayer[0]) + ord(bayer[2]) + ord(bayer[bayer_step*2]) +
                  ord(bayer[bayer_step*2+2]) + 2) {>>} shr 2;
            t1 := (ord(bayer[1]) + ord(bayer[bayer_step]) +
                  ord(bayer[bayer_step+2]) + ord(bayer[bayer_step*2+1])+2) {>>} shr 2;
            dst[1] := (t0);
            dst[0] := (t1);
            dst[-1] := bayer[bayer_step+1];

            t0 := (ord(bayer[2]) + ord(bayer[bayer_step*2+2]) + 1) {>>} shr 1;
            t1 := (ord(bayer[bayer_step+1]) + ord(bayer[bayer_step+3]) + 1) {>>} shr 1;
            dst[4] := (t0);
            dst[3] := bayer[bayer_step+2];
            dst[2] := (t1);
            inc(bayer,2);
          inc(dst,6);
        until bayer>(bayer_end-2);
     end;

    if( bayer < bayer_end ) then
    begin
        t0 := (ord(bayer[0]) + ord(bayer[2]) + ord(bayer[bayer_step*2]) +
              ord(bayer[bayer_step*2+2]) + 2) {>>} shr 2;
        t1 := (ord(bayer[1]) + ord(bayer[bayer_step]) +
              ord(bayer[bayer_step+2]) + ord(bayer[bayer_step*2+1])+2) shr { >>} 2;
        dst[-blue] := (t0);
        dst[0] := (t1);
        dst[blue] := bayer[bayer_step+1];
        inc(bayer);
        inc(dst,3);
    end;
    blue := -blue;
    start_with_green := not start_with_green;
//( ; size.height-- > 0; bayer0 += bayer_step, dst0 += dst_step )
123:
    inc(bayer0,bayer_step);
    inc(dst0,dst_step);
    dec(size.height);
  until (size.height<=0);
  result:=1;
end;


//if (code= CV_BayerBG2BGR ) or (code= CV_BayerGB2BGR) then
//  blue:=-1
//else
//  blue:=1;
//if (code = CV_BayerGB2BGR) or (code =CV_BayerGR2BGR) then
// start_with_green:=1
//else
// start_with_green:=0;
////    int blue = code == CV_BayerBG2BGR || code == CV_BayerGB2BGR ? -1 : 1;
////    int start_with_green = code == CV_BayerGB2BGR || code == CV_BayerGR2BGR;


// Attention only BGR not BGRA !!!! This is not compatible  with Linux TBitmap.RawImage
procedure SRGGB8_to_BGR(src: PByte; dest: PByte; w, h:  integer);
var
  //i: Integer;
  //blue,
  //start_with_green:cint;
  res : integer;
  x: cvsize;
  wx: cint;
begin
  x.width:=w;
  x.height:=h;
  wx:= w * 3;  // 3 * for RGB
  res := icvBayer2BGR_8u_C1C3R(src,w ,dest, wx ,x,1,0);
end;


// Attention only BGR not BGRA !!!! This is not compatible  with Linux TBitmap.RawImage
procedure SGBRG8_to_BGR(src: PByte; dest: PByte; w, h: integer);
var
  //blue,
  //start_with_green:cint;
  res : integer;
  x: cvsize;
  wx: cint;
begin
  x.width:=w;
  x.height:=h;
  wx:= w * 3;  // 3 * for RGB
  res := icvBayer2BGR_8u_C1C3R(src,w ,dest, wx ,x,1,-1);
end;



//icvBayer2BGR_8u_C1C3R((b.getimagepointer(0,0)),b.RowPitch,pbbyte(b2.getimagepointer(0,0)),b2.RowPitch,x,  CV_BayerBG2BGR);
//  where getimagepointer just gets a pointer to the first pixel of a raw image
//  and "rowpitch" is the difference (in bytes) between the first pixels of two rows that follow eachother (@scanline[1]-@scanline[0]).
// x is a TSize like record with dimensions


end.

//I have image data that gets acquired like so:
//
//unsigned char* imageData = NULL;
//GetImage(imageData);
//The imageData returns as raw BayerGR8 format: That is to say:
//
//G R G R G R G R ...
//B G B G B G B G ...
//G R G R G R G R ...
//      ...
//where each of those pixels occupies 8 bits.
//
//The images that are being grabbed are 2752x2200 (pixels).
//
//Whenever I set up a bitmap and then create a bitmap using this image data, the bitmap always comes out blank. Here is my bitmap setup:
//
//    BITMAPFILEHEADER* bf = new BITMAPFILEHEADER;
//    bf->bfType = 0x4d42;
//    bf->bfSize = 6054400 + 54 + sizeof(BITMAPINFO);
//    bf->bfOffBits = 54;
//
//    BITMAPINFOHEADER* bih = new BITMAPINFOHEADER;
//    bih->biSize = 40;
//    bih->biWidth = 2752;
//    bih->biHeight = 2200;
//    bih->biPlanes = 1;
//    bih->biBitCount = 8;
//    bih->biCompression = 0;
//    bih->biXPelsPerMeter = 2835;
//    bih->biYPelsPerMeter = 2835;
//    bih->biClrUsed = 0;
//    bih->biClrImportant = 0;
//So far I have tried the following:
//
//
//Got it. Here's the function I created that got it working (interpolation method):
//
///////////////////////////////////////////////////////////////
//// ConvertBayer8ToBgr()
//// Converts raw BayerGR8 pixels into
////     BGR pixels.
////
////  G | R | G | R              B G R | B G R | B G R | B G R
//// --- --- --- ---            ------- ------- ------- -------
////  B | G | B | G        |\    B G R | B G R | B G R | B G R
//// --- --- --- ---  -----  \  ------- ------- ------- -------
////  G | R | G | R   -----  /   B G R | B G R | B G R | B G R
//// --- --- --- ---       |/   ------- ------- ------- -------
////  B | G | B | G              B G R | B G R | B G R | B G R
////
///////////////////////////////////////////////////////////////
//void ConvertBayer8ToBGR(VmbUchar_t* bayerImgDat, VmbUchar_t* bgrOutputDat)
//{
//    VmbUchar_t* newimagedata_start = bgrOutputDat;
//
//    int currentTempIndex = 0;
//    int nearestBluesAvg = 0;
//    int nearestRedsAvg = 0;
//    int nearestGreensAvg = 0;
//
//    for(int j = 0; j < 1100; j++)
//    {
//        for(int i = 0; i < 2752; i++) //G R G R G...
//        {
//            if(currentTempIndex % 2 == 0 /* even, green */)
//            {
//                //avg blue
//                if(j == 0) //if in the first row, only take next blue
//                {
//                    nearestBluesAvg = *(bayerImgDat+currentTempIndex+2752);
//                }
//                else
//                {
//                    nearestBluesAvg = (*(bayerImgDat + currentTempIndex + 2752) + *(bayerImgDat+currentTempIndex-2752)) / 2;
//                }
//                *bgrOutputDat = nearestBluesAvg; //b
//                bgrOutputDat++;
//                *bgrOutputDat = *(bayerImgDat + currentTempIndex); //g
//                bgrOutputDat++;
//                //avg red
//                if(i == 0) //if in first column, only take next red
//                {
//                    nearestRedsAvg = *(bayerImgDat+currentTempIndex+1);
//                }
//                else
//                {
//                    nearestRedsAvg = ( (*(bayerImgDat+currentTempIndex+1)) + (*(bayerImgDat+currentTempIndex-1)) ) / 2;
//                }
//                *bgrOutputDat = nearestRedsAvg; //r
//                bgrOutputDat++;
//
//                currentTempIndex++;
//            }
//            else /* odd, red*/
//            {
//                //avg blue
//                if(i == 1099) //if in last column, take just left-down blue pixel
//                {
//                    nearestBluesAvg = *(bayerImgDat+currentTempIndex-1+2752);
//                }
//                else // else take both left-down and right-down
//                {
//                    nearestBluesAvg = (*(bayerImgDat+currentTempIndex+1+2752) + *(bayerImgDat+currentTempIndex-1+2752)) / 2;
//                }
//                *bgrOutputDat = nearestBluesAvg; //b
//                bgrOutputDat++;
//                //avg green
//                nearestGreensAvg = (*(bayerImgDat+currentTempIndex-1) + *(bayerImgDat+currentTempIndex+2752)) / 2;
//                *bgrOutputDat = nearestGreensAvg;  //g
//                bgrOutputDat++;
//                *bgrOutputDat = *(bayerImgDat + currentTempIndex); //r
//                bgrOutputDat++;
//
//                currentTempIndex++;
//            }
//        }
//        for(int i = 0; i < 2752; i++)//B G B G B G B....
//        {
//            if(currentTempIndex % 2 == 0 /* even, blue */)
//            {
//
//                *bgrOutputDat = *(bayerImgDat + currentTempIndex); //b
//                bgrOutputDat++;
//                //avg green
//                nearestGreensAvg = (*(bayerImgDat + currentTempIndex + 1) + *(bayerImgDat + currentTempIndex -2752)) / 2;
//                *bgrOutputDat = nearestGreensAvg; //g
//                bgrOutputDat++;
//                //avg red
//                if(i == 0) //if first column, take only right-up pixel
//                {
//                    nearestRedsAvg = *(bayerImgDat+currentTempIndex+1-2752);
//                }
//                else //else take both left-up and right-up pixels
//                {
//                    nearestRedsAvg = (*(bayerImgDat+currentTempIndex-1-2752) + *(bayerImgDat+currentTempIndex+1-2752)) / 2;
//                }
//                *bgrOutputDat = nearestRedsAvg; //r
//                bgrOutputDat++;
//
//                currentTempIndex++;
//
//            }
//            else /* odd, green*/
//            {
//                //avg blue
//                if(i == 2751) //if in last column, only take previous blue (next blue doesnt exist)
//                {
//                    nearestBluesAvg = *(bayerImgDat + currentTempIndex - 1);
//                }
//                else //else take both next and previous
//                {
//                    nearestBluesAvg = (*(bayerImgDat+currentTempIndex+1) + *(bayerImgDat+currentTempIndex-1)) / 2;
//                }
//                *bgrOutputDat = nearestBluesAvg; //b
//                bgrOutputDat++;
//                *bgrOutputDat = *(bayerImgDat + currentTempIndex); //g
//                bgrOutputDat++;
//                //avg red
//                if(j == 1099) //if in last row, only take previous red (next red doesn't exist)
//                {
//                    nearestRedsAvg = *(bayerImgDat+currentTempIndex-2752);
//                }
//                else //else take both
//                {
//                    nearestRedsAvg = (*(bayerImgDat+currentTempIndex+2752) + *(bayerImgDat+currentTempIndex-2752)) / 2;
//                }
//                *bgrOutputDat = nearestRedsAvg; //r
//                bgrOutputDat++;
//
//                currentTempIndex++;
//            }
//        }
//    }

